{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Share.Postgres.Orphans () where

import Data.Aeson qualified as Aeson
import Data.Bytes.Put (runPutS)
import Data.Either.Extra qualified as Either
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Hasql.Session qualified as Hasql
import Servant (err500)
import Servant.API
import Share.Postgres.Composites (DecodeComposite (..))
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Utils.Postgres (RawLazyBytes (..))
import Share.Web.Errors (ErrorID (..), ToServerError (..))
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), ComponentHash (..), PatchHash (..))
import U.Codebase.Reference (Id' (Id), Reference' (..))
import U.Codebase.Referent (ConstructorType (..), Referent' (..))
import U.Codebase.Sqlite.Patch.TermEdit qualified as SqliteTermEdit
import U.Codebase.Sqlite.Serialization qualified as Serialization
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.TempEntityType (TempEntityType (..))
import U.Codebase.TermEdit qualified as TermEdit
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Name (Name)
import Unison.NameSegment.Internal (NameSegment (..))
import Unison.SyncV2.Types (CBORBytes (..))
import Unison.Syntax.Name qualified as Name
import UnliftIO (MonadUnliftIO (..))

-- Orphans for 'Hash'
instance Hasql.EncodeValue Hash where
  encodeValue =
    Hasql.encodeValue
      & contramap Hash.toBase32HexText

instance Hasql.DecodeValue Hash where
  decodeValue =
    Hasql.decodeValue
      -- We can trust that encoded values are valid,
      -- and skipping validation is a significant performance improvement
      <&> Hash.unsafeFromBase32HexText

instance Hasql.EncodeValue Hash32 where
  encodeValue =
    Hasql.encodeValue
      & contramap Hash32.toText

instance Hasql.DecodeValue Hash32 where
  decodeValue =
    Hasql.decodeValue
      -- We can trust that encoded values are valid,
      -- and skipping validation is a significant performance improvement
      <&> Hash32.unsafeFromBase32Hex
      . Base32Hex.UnsafeFromText

instance FromHttpApiData Hash where
  parseUrlPiece txt =
    Either.maybeToEither "Invalid Hash" $ Hash.fromBase32HexText txt

instance ToHttpApiData Hash where
  toUrlPiece =
    Hash.toBase32HexText

deriving via Hash instance Aeson.ToJSON BranchHash

deriving via Hash instance Hasql.DecodeValue BranchHash

deriving via Hash instance Hasql.EncodeValue BranchHash

deriving via Hash instance Hasql.DecodeValue CausalHash

deriving via Hash instance Hasql.EncodeValue CausalHash

deriving via Hash instance FromHttpApiData CausalHash

deriving via Hash instance ToHttpApiData CausalHash

deriving via Hash instance Aeson.ToJSON PatchHash

deriving via Hash instance Hasql.DecodeValue PatchHash

deriving via Hash instance Hasql.EncodeValue PatchHash

deriving via Hash instance Hasql.DecodeValue ComponentHash

deriving via Hash instance Hasql.EncodeValue ComponentHash

deriving via Hash instance FromHttpApiData ComponentHash

deriving via Hash instance ToHttpApiData ComponentHash

deriving via Text instance Hasql.DecodeValue NameSegment

deriving via Text instance Hasql.EncodeValue NameSegment

instance Hasql.DecodeValue Name where
  decodeValue =
    Hasql.decodeValue @Text
      & Decoders.refine Name.parseTextEither

instance Hasql.EncodeValue Name where
  encodeValue =
    Hasql.encodeValue @Text
      & contramap Name.toText

instance (Hasql.DecodeValue t, Hasql.DecodeValue h, Show t, Show h) => Hasql.DecodeRow (Reference' t h) where
  decodeRow = do
    decodeReference (decodeField @(Maybe t)) (decodeField @(Maybe h)) (decodeField @(Maybe Int64))

instance (Hasql.DecodeValue t, Hasql.DecodeValue h, Show t, Show h) => DecodeComposite (Reference' t h) where
  decodeComposite = decodeReference (Decoders.field (Hasql.decodeField @(Maybe t))) (Decoders.field $ Hasql.decodeField @(Maybe h)) (Decoders.field $ Hasql.decodeField @(Maybe Int64))

decodeReference :: forall t h m. (Monad m, Show t, Show h) => m (Maybe t) -> m (Maybe h) -> m (Maybe Int64) -> m (Reference' t h)
decodeReference getT getH getI = do
  t <- getT
  h <- getH
  i <- getI
  let wordI = either (error . show) id . tryInto @Word64 <$> i
  pure $ mkRef t h wordI
  where
    mkRef (Just t) Nothing Nothing =
      ReferenceBuiltin t
    mkRef Nothing (Just h) (Just componentIdx) =
      ReferenceDerived (Id h componentIdx)
    mkRef t h i =
      error $ "invalid find_type_index type reference: " ++ str
      where
        str = "(" ++ show t ++ ", " ++ show h ++ ", " ++ show i ++ ")"

instance (Hasql.DecodeRow (Reference' t h)) => Hasql.DecodeRow (Referent' (Reference' t h) (Reference' t h)) where
  decodeRow = decodeReferent Hasql.decodeRow (decodeField @(Maybe Int64))

instance (DecodeComposite (Reference' t h)) => DecodeComposite (Referent' (Reference' t h) (Reference' t h)) where
  decodeComposite = decodeReferent decodeComposite (Decoders.field $ Hasql.decodeField @(Maybe Int64))

decodeReferent :: (Monad m) => m (Reference' t h) -> m (Maybe Int64) -> m (Referent' (Reference' t h) (Reference' t h))
decodeReferent getRef getCid = do
  ref <- getRef
  mayCid <- getCid
  let wordCid = either (error . show) id . tryInto @Word64 <$> mayCid
  case wordCid of
    Nothing -> pure $ Ref ref
    Just cid -> pure $ Con ref cid

instance Hasql.DecodeValue ConstructorType where
  decodeValue =
    Hasql.decodeValue @Int64 & Decoders.refine \case
      0 -> Right DataConstructor
      1 -> Right EffectConstructor
      n -> Left $ "Invalid ConstructorType: " <> tShow n

instance Hasql.EncodeValue ConstructorType where
  encodeValue =
    Hasql.encodeValue @Int64
      & contramap \case
        DataConstructor -> 0
        EffectConstructor -> 1

-- | Decode a single field as part of a Row
decodeField :: (Hasql.DecodeField a) => Decoders.Row a
decodeField = Decoders.column Hasql.decodeField

instance Hasql.EncodeValue TempEntity where
  encodeValue =
    Encoders.bytea
      & contramap (runPutS . Serialization.putTempEntity)

instance Hasql.EncodeValue TempEntityType where
  encodeValue =
    Encoders.enum \case
      TermComponentType -> "term_component"
      DeclComponentType -> "decl_component"
      NamespaceType -> "namespace"
      PatchType -> "patch"
      CausalType -> "causal"

instance Hasql.DecodeValue TempEntityType where
  decodeValue =
    Decoders.enum
      \case
        "term_component" -> Just TermComponentType
        "decl_component" -> Just DeclComponentType
        "namespace" -> Just NamespaceType
        "patch" -> Just PatchType
        "causal" -> Just CausalType
        _ -> Nothing

instance Hasql.EncodeValue TermEdit.Typing where
  encodeValue =
    Encoders.enum \case
      TermEdit.Same -> "same"
      TermEdit.Subtype -> "subtype"
      TermEdit.Different -> "different"

instance Hasql.DecodeValue TermEdit.Typing where
  decodeValue =
    Decoders.enum
      ( \case
          "same" -> Just TermEdit.Same
          "subtype" -> Just TermEdit.Subtype
          "different" -> Just TermEdit.Different
          _ -> Nothing
      )

instance Hasql.EncodeValue SqliteTermEdit.Typing where
  encodeValue =
    Encoders.enum \case
      SqliteTermEdit.Same -> "same"
      SqliteTermEdit.Subtype -> "subtype"
      SqliteTermEdit.Different -> "different"

instance Hasql.DecodeValue SqliteTermEdit.Typing where
  decodeValue =
    Decoders.enum
      ( \case
          "same" -> Just SqliteTermEdit.Same
          "subtype" -> Just SqliteTermEdit.Subtype
          "different" -> Just SqliteTermEdit.Different
          _ -> Nothing
      )

deriving via RawLazyBytes instance Hasql.DecodeValue (CBORBytes t)

instance ToServerError Hasql.SessionError where
  toServerError _ = (ErrorID "query-error", err500)

instance Logging.Loggable Hasql.SessionError where
  toLog = \case
    (Hasql.QueryError template params err) ->
      Logging.withSeverity Logging.Error . Logging.textLog $
        Text.unlines
          [ "QueryError:",
            indent (tShow err),
            "TEMPLATE:",
            indent (Text.decodeUtf8 template),
            "PARAMS:",
            indent (tShow params)
          ]
    (Hasql.PipelineError cmdErr) ->
      Logging.withSeverity Logging.Error . Logging.textLog $
        Text.unlines
          [ "PipelineError:",
            indent (tShow cmdErr)
          ]
    where
      indent :: Text -> Text
      indent = Text.unlines . fmap ("    " <>) . Text.lines

-- | See https://github.com/nikita-volkov/hasql/issues/144
-- This instance won't be added upstream.
instance MonadUnliftIO Hasql.Session where
  withRunInIO inner = do
    conn <- ask
    res <- liftIO $ try $ inner $ \sess -> do
      Hasql.run sess conn >>= either throwIO pure
    case res of
      Left e -> throwError e
      Right a -> pure a
