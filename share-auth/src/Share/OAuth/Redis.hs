{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Share.OAuth.Redis
  ( newPendingSession,
    getPendingSession,
    PendingSession (..),
    failPendingSession,
    createOAuth2Code,
    exchangeOAuth2Code,
    R.liftRedis,
  )
where

import Control.Monad (void)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Binary
import Data.Binary qualified as Binary
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Typeable (TypeRep, typeRep)
import Database.Redis qualified as R
import Share.OAuth.Errors qualified as OAuthError
import Share.OAuth.PKCE (verifyPkce)
import Share.OAuth.Session
import Share.OAuth.Session qualified as Session
import Share.OAuth.Types
import Share.OAuth.Types qualified as OAuth2
import Share.Utils.Binary (JSONBinary (..))
import Share.Utils.IDs qualified as IDs
import Share.Utils.SecureTokens (newSecureToken)
import Share.Utils.URI (URIParam)
import Network.URI (URI)
import UnliftIO

data RedisErr
  = RedisErr R.Reply
  | -- We failed to decode the value at a key, likely because the binary format for that
    -- entity was changed in a backwards-incompatible way.
    --          key        errMsg type
    DecodingErr ByteString String TypeRep
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Pending sessions expire in 5 minutes.
pendingSessionExpiry :: NominalDiffTime
pendingSessionExpiry = (5 * 60)

-- | Code exchanges expire in 5 minutes.
codeTTL :: NominalDiffTime
codeTTL = (5 * 60)

class RedisKey a where
  redisKey :: a -> ByteString

pendingSessionKey :: PendingSessionId -> ByteString
pendingSessionKey sid = Text.encodeUtf8 $ "sessions:pending:" <> IDs.fromId sid

codeExchangeKey :: OAuth2.Code -> ByteString
codeExchangeKey code = BS.intercalate ":" ["oauthcode", Text.encodeUtf8 $ OAuth2.codeText code]

redisPut :: (RedisKey a, Binary a) => Maybe NominalDiffTime -> a -> R.Redis ()
redisPut mayExpiry a = do
  let key = redisKey a
  let value = toStrict $ encode a
  void . handleErrReply $ case mayExpiry of
    Just expirySeconds -> do
      R.setex key (floor expirySeconds) value
    Nothing -> R.set key value

handleErrReply :: R.Redis (Either R.Reply a) -> R.Redis a
handleErrReply m = m >>= either (throwIO . RedisErr) pure

redisGet :: forall a. (Typeable a, Binary a) => ByteString -> R.Redis (Maybe a)
redisGet key = do
  a <-
    handleErrReply (R.get key) >>= \case
      Nothing -> pure Nothing
      (Just bs) ->
        case Binary.decodeOrFail (fromStrict bs) of
          Left (_, _, errMsg) -> throwIO $ DecodingErr key errMsg (typeRep (Proxy @a))
          Right (_, _, a) -> pure (Just a)
  pure a

redisDelete :: ByteString -> R.Redis ()
redisDelete key = do
  void $ handleErrReply (R.del [key])

getPendingSession :: PendingSessionId -> R.Redis (Maybe PendingSession)
getPendingSession sid = do
  redisGet (pendingSessionKey sid)

-- The passed UUID is a clientID, it will be part of the URL that the
-- oauth flow redirects to, which is how we will figure out which
-- client just authenticated
createPendingSession :: LoginRequest -> PKCEVerifier -> Maybe URI -> Maybe Aeson.Value -> R.Redis PendingSession
createPendingSession loginRequest pkceVerifier returnToURI additionalData = do
  psid <- generatePendingSessionId
  return $
    PendingSession
      { pendingId = psid,
        loginRequest,
        pkceVerifier,
        returnToURI,
        additionalData
      }

-- | Generate a cryptographically secure pending-session identifier
generatePendingSessionId :: MonadIO m => m PendingSessionId
generatePendingSessionId =
  PendingSessionId <$> newSecureToken

newPendingSession :: LoginRequest -> PKCEVerifier -> Maybe URI -> Maybe Aeson.Value -> R.Redis PendingSession
newPendingSession loginRequest pkceVerifier returnToURI additionalData = do
  sesh <- createPendingSession loginRequest pkceVerifier returnToURI additionalData
  redisPut (Just pendingSessionExpiry) sesh
  pure sesh

failPendingSession :: PendingSessionId -> R.Redis ()
failPendingSession sid = do
  _ <- R.del [pendingSessionKey sid]
  return ()

instance RedisKey PendingSession where
  redisKey (PendingSession {pendingId = sid}) = pendingSessionKey sid

data CodeExchange = CodeExchange
  { code :: OAuth2.Code,
    authRequest :: OAuth2.AuthenticationRequest,
    userId :: UserId,
    sessionId :: SessionId
  }
  deriving (Binary) via JSONBinary CodeExchange
  deriving stock (Show)

instance Aeson.ToJSON CodeExchange where
  toJSON (CodeExchange code authRequest userId sessionId) =
    Aeson.object
      [ "code" .= code,
        "auth_request" .= authRequest,
        "user_id" .= userId,
        "session_id" .= sessionId
      ]

instance Aeson.FromJSON CodeExchange where
  parseJSON = do
    Aeson.withObject "CodeExchange" $ \obj -> do
      code <- obj .: "code"
      authRequest <- obj .: "auth_request"
      userId <- obj .: "user_id"
      sessionId <- obj .: "session_id"
      pure $ CodeExchange {..}

instance RedisKey CodeExchange where
  redisKey CodeExchange {code} = codeExchangeKey code

createOAuth2Code :: URI -> Set URI -> UserId -> OAuth2.AuthenticationRequest -> R.Redis (Session, OAuth2.Code)
createOAuth2Code issuer aud userId authRequest = do
  code <- OAuth2.Code <$> newSecureToken
  session@(Session {sessionId}) <- liftIO $ Session.createSession issuer aud userId
  redisPut (Just codeTTL) $ CodeExchange code authRequest userId sessionId
  pure (session, code)

exchangeOAuth2Code ::
  OAuth2.Code ->
  OAuth2.OAuthClientId ->
  URIParam ->
  OAuth2.PKCEVerifier ->
  (R.Redis (SessionId, UserId, OAuth2.AuthenticationRequest))
exchangeOAuth2Code code callerClientId callerRedirectURI pkceVerifier = do
  let key = codeExchangeKey code
  mayCodeExchange <- redisGet key
  case mayCodeExchange of
    Nothing -> throwIO (OAuthError.CodeMissingOrExpired code)
    Just (CodeExchange {userId, sessionId, authRequest = authRequest@(OAuth2.AuthenticationRequest {clientId, redirectURI, pkceChallenge, pkceChallengeMethod})})
      | clientId /= callerClientId -> throwIO (OAuthError.MismatchedClientId callerClientId clientId)
      | redirectURI /= callerRedirectURI -> throwIO (OAuthError.MismatchedRedirectURI callerRedirectURI redirectURI)
      | otherwise -> do
          validatePKCE pkceVerifier pkceChallenge pkceChallengeMethod
          redisDelete key
          pure (sessionId, userId, authRequest)

validatePKCE :: OAuth2.PKCEVerifier -> OAuth2.PKCEChallenge -> OAuth2.PKCEChallengeMethod -> R.Redis ()
validatePKCE verifier challenge method =
  if verifyPkce verifier challenge method
    then pure ()
    else throwIO OAuthError.PKCEChallengeFailure
