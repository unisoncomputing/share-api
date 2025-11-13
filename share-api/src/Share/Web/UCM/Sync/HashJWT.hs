-- | Hash JWT signing and verifying
module Share.Web.UCM.Sync.HashJWT
  ( signHashForUser,
    verifyHashJWT,
    signHashesForUser,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Either.Combinators qualified as Either
import Share.IDs
import Share.IDs qualified as IDs
import Share.JWT qualified as JWT
import Share.Prelude
import Share.Web.App
import Share.Web.Authentication.HashJWT qualified as HashJWT
import Share.Web.Authentication.JWT qualified as AuthJWT
import Share.Web.Authentication.Types qualified as AuthN
import System.IO.Unsafe (unsafePerformIO)
import Unison.Hash32 (Hash32)
import Unison.Share.API.Hash (HashJWT (HashJWT), HashJWTClaims)
import Unison.Share.API.Hash qualified as Share
import Unison.Util.Cache (Cache)
import Unison.Util.Cache qualified as Cache

-- | Hash32s are ~100 bytes, HashJWTs are generally ~300 bytes,
-- so we can afford to cache a lot of these.
--
-- As of March 2024 base/main 24,000 hashes total,
--
-- 2 * (100 + 300) = 800 bytes per hashjwt.
--
-- So, back of napkin math, it's about 800 bytes * 24,000 entities = 19.2 MB per entire copy
-- of base.
--
-- We have lots of memory to spare right now so let's go with 500 MB = 26 full copies of base
-- = 24,000 * 26 = 624,000 entities
-- could fit at once.
publicHashJWTCacheSize :: Word
publicHashJWTCacheSize = 624000

-- | Size of the cache we use for hashJWTs that are keyes to their user.
--
-- This doesn't need to be nearly as large as the public cache, and should just be enough to
-- hold a couple releases at once.
userSpecificHashJWTCacheSize :: Word
userSpecificHashJWTCacheSize = 100000

-- | Cached HashJWTs which are keyed to a specific user.
userSpecificHashJWTCache :: Cache (UserId, Hash32) Share.HashJWT
userSpecificHashJWTCache = unsafePerformIO $ Cache.semispaceCache userSpecificHashJWTCacheSize
{-# NOINLINE userSpecificHashJWTCache #-}

-- | Cached HashJWTs which are publicly available.
publicHashJWTCache :: Cache Hash32 Share.HashJWT
publicHashJWTCache = unsafePerformIO $ Cache.semispaceCache publicHashJWTCacheSize
{-# NOINLINE publicHashJWTCache #-}

-- | Sign a hash for a particular user, which allows them to download the associated entity later.
-- A 'Nothing' user ID indicates that this hash is publicly available.
signHashesForUser :: (Traversable t) => Maybe UserId -> t Hash32 -> WebApp (t Share.HashJWT)
signHashesForUser mayUserId hashes = do
  case mayUserId of
    Nothing ->
      for hashes $ Cache.apply publicHashJWTCache signJWT
    Just userId ->
      for hashes \hash -> Cache.apply userSpecificHashJWTCache (\(_, h) -> signJWT h) (userId, hash)
  where
    signJWT :: Hash32 -> WebApp Share.HashJWT
    signJWT hash = do
      Share.HashJWT . JWT.signedJWTToText <$> HashJWT.signHashJWT (Share.HashJWTClaims {Share.hash = hash, Share.userId = IDs.toText <$> mayUserId})

signHashForUser :: Maybe UserId -> Hash32 -> WebApp HashJWT
signHashForUser mayUserId hash = fmap runIdentity $ signHashesForUser mayUserId (Identity hash)

-- | Validate that the given hash jwt matches the currently authenticated user.
-- If a 'Nothing' is provided as the user, the HashJWT must have a 'null' user, which
-- indicates that the requested hash is publicly available.
verifyHashJWT :: Maybe UserId -> HashJWT -> WebApp (Either AuthN.AuthenticationErr HashJWTClaims)
verifyHashJWT mayUserId (HashJWT jwtText) = runExceptT $ do
  signedJWT <- except . Either.mapLeft AuthN.JWTErr $ JWT.textToSignedJWT jwtText
  JWT.JSONJWTClaims hashJWTValue <- ExceptT $ AuthJWT.verifyJWT signedJWT userIdCheck
  pure hashJWTValue
  where
    -- user id on hash jwt must match the current user,
    -- if user id on hash jwt is 'Nothing' then anyone, including unauthenticated callers,
    -- may use it; it's a public definition.
    userIdCheck (JWT.JSONJWTClaims (Share.HashJWTClaims {Share.userId = hashJWTUserId})) =
      case hashJWTUserId of
        Nothing -> Nothing
        hjUser
          | (IDs.toText <$> mayUserId) == hjUser -> Nothing
          | otherwise -> Just $ AuthN.CustomError "This HashJWT is for a different user."
