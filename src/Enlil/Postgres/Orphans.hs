{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Enlil.Postgres.Orphans () where

import Data.Aeson qualified as Aeson
import Data.Bytes.Put (runPutS)
import Data.Either.Extra qualified as Either
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Enlil.Prelude
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.Errors (ErrorID (..), ToServerError (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate qualified as Hasql
import Hasql.Session qualified as Hasql
import Servant (err500)
import Servant.API
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
import Unison.NameSegment (NameSegment (..))

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
      <&> Hash32.unsafeFromBase32Hex . Base32Hex.UnsafeFromText

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

instance (Hasql.DecodeValue t, Hasql.DecodeValue h, Show t, Show h) => Hasql.DecodeRow (Reference' t h) where
  decodeRow = do
    t <- decodeField @(Maybe t)
    h <- decodeField @(Maybe h)
    i <- decodeField @(Maybe Int64)
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
  decodeRow = do
    ref <- Hasql.decodeRow
    mayCid <- decodeField @(Maybe Int64)
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
decodeField :: Hasql.DecodeField a => Decoders.Row a
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

instance ToServerError Hasql.QueryError where
  toServerError _ = (ErrorID "query-error", err500)

instance Logging.Loggable Hasql.QueryError where
  toLog (Hasql.QueryError template params err) =
    Logging.withSeverity Logging.Error . Logging.textLog $
      Text.unlines
        [ "QueryError:",
          indent (tShow err),
          "TEMPLATE:",
          indent (Text.decodeUtf8 template),
          "PARAMS:",
          indent (tShow params)
        ]
    where
      indent :: Text -> Text
      indent = Text.unlines . fmap ("    " <>) . Text.lines
