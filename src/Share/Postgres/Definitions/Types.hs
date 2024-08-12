module Share.Postgres.Definitions.Types
  ( TermComponentElementBytes (..),
    TermComponentElement (..),
    termComponentElementToByteString,
    TypeComponentElementBytes (..),
    TypeComponentElement (..),
    typeComponentElementToByteString,
    EvalResultTerm (..),
    evalResultTermToByteString,
    DeclKindEnum (..),
    declTypeToDeclKindEnum,
    declKindEnumToDeclType,
    declKindEnumToConstructorType,
    constructorTypeToDeclKindEnum,
    ModifierEnum (..),
    LocalTermBytes (..),
    LocalTypeBytes (..),
    PgComponentIndex (..),
    pgComponentIndex,
    unPgComponentIndex,
    PgConstructorIndex (..),
    pgConstructorIndex,
    unPgConstructorIndex,
  )
where

import Hasql.Decoders qualified as Decoders
import Hasql.Decoders qualified as Hasql
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Share.Postgres.Serialization qualified as S
import Share.Prelude
import Share.Utils.Postgres qualified as PG
import U.Codebase.Decl qualified as DD
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Symbol qualified as Sqlite
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import Unison.ConstructorType qualified as CT

-- | Type wrappers for keeping indexes straight
newtype PgComponentIndex = PgComponentIndex Int64
  deriving newtype (Show, Eq, Ord, EncodeValue, DecodeValue)

pgComponentIndex :: Reference.Pos -> PgComponentIndex
pgComponentIndex = PgComponentIndex . fromIntegral

unPgComponentIndex :: PgComponentIndex -> Reference.Pos
unPgComponentIndex (PgComponentIndex i) = fromIntegral i

newtype PgConstructorIndex = PgConstructorIndex Int64
  deriving newtype (Show, Eq, Ord, EncodeValue, DecodeValue)

pgConstructorIndex :: DD.ConstructorId -> PgConstructorIndex
pgConstructorIndex = PgConstructorIndex . fromIntegral

unPgConstructorIndex :: PgConstructorIndex -> DD.ConstructorId
unPgConstructorIndex (PgConstructorIndex i) = fromIntegral i

evalResultTermToByteString :: EvalResultTerm -> ByteString
evalResultTermToByteString (EvalResultTerm trm) =
  S.encodeEvalResult trm

-- | The result of evaluating a term, we store this as an independent term without a type.
data EvalResultTerm
  = EvalResultTerm TermFormat.Term
  deriving stock (Show)

instance EncodeValue EvalResultTerm where
  encodeValue =
    Encoders.bytea
      & contramap evalResultTermToByteString

instance DecodeValue EvalResultTerm where
  decodeValue =
    Decoders.bytea
      & Hasql.refine \bytes ->
        S.decodeEvalResult bytes
          & mapLeft tShow
          & fmap EvalResultTerm

termComponentElementToByteString :: TermComponentElement -> ByteString
termComponentElementToByteString (TermComponentElement trm typ) =
  S.encodeTermAndType trm typ

data TermComponentElement
  = TermComponentElement TermFormat.Term TermFormat.Type
  deriving stock (Show)

instance EncodeValue TermComponentElement where
  encodeValue =
    Encoders.bytea
      & contramap termComponentElementToByteString

instance DecodeValue TermComponentElement where
  decodeValue =
    Decoders.bytea
      & Hasql.refine \bytes ->
        S.decodeTermAndType bytes
          & mapLeft tShow
          & fmap (uncurry TermComponentElement)

-- | The term and type bytes of a term component.
newtype TermComponentElementBytes = TermComponentElementBytes {termComponentElementBytes :: ByteString}
  deriving stock (Show)

instance EncodeValue TermComponentElementBytes where
  encodeValue =
    Encoders.bytea
      & contramap termComponentElementBytes

instance DecodeValue TermComponentElementBytes where
  decodeValue =
    Decoders.bytea
      & Hasql.refine (Right . TermComponentElementBytes)

typeComponentElementToByteString :: TypeComponentElement -> ByteString
typeComponentElementToByteString (TypeComponentElement decl) =
  S.encodeDecl decl

data TypeComponentElement
  = TypeComponentElement (DeclFormat.Decl Sqlite.Symbol)
  deriving stock (Show)

instance EncodeValue TypeComponentElement where
  encodeValue =
    Encoders.bytea
      & contramap typeComponentElementToByteString

instance DecodeValue TypeComponentElement where
  decodeValue =
    Decoders.bytea
      & Hasql.refine \bytes ->
        S.decodeDecl bytes
          & bimap tShow TypeComponentElement

newtype TypeComponentElementBytes = TypeComponentElementBytes {typeComponentElementBytes :: ByteString}
  deriving stock (Show)

instance EncodeValue TypeComponentElementBytes where
  encodeValue =
    Encoders.bytea
      & contramap typeComponentElementBytes

instance DecodeValue TypeComponentElementBytes where
  decodeValue =
    Decoders.bytea
      & Hasql.refine (Right . TypeComponentElementBytes)

-- | Custom type for serializing/deserializing the decl_kind postgres enum
data DeclKindEnum = Ability | Data
  deriving stock (Show)

instance EncodeValue DeclKindEnum where
  encodeValue =
    Encoders.enum
      ( \case
          Ability -> "ability"
          Data -> "data"
      )

instance DecodeValue DeclKindEnum where
  decodeValue =
    Decoders.enum
      ( \case
          "ability" -> Just Ability
          "data" -> Just Data
          _ -> Nothing
      )

declTypeToDeclKindEnum :: Decl.DeclType -> DeclKindEnum
declTypeToDeclKindEnum Decl.Data = Data
declTypeToDeclKindEnum Decl.Effect = Ability

declKindEnumToDeclType :: DeclKindEnum -> Decl.DeclType
declKindEnumToDeclType Data = Decl.Data
declKindEnumToDeclType Ability = Decl.Effect

declKindEnumToConstructorType :: DeclKindEnum -> CT.ConstructorType
declKindEnumToConstructorType Data = CT.Data
declKindEnumToConstructorType Ability = CT.Effect

constructorTypeToDeclKindEnum :: CT.ConstructorType -> DeclKindEnum
constructorTypeToDeclKindEnum CT.Data = Data
constructorTypeToDeclKindEnum CT.Effect = Ability

data ModifierEnum = Structural | Unique
  deriving stock (Show)

instance EncodeValue ModifierEnum where
  encodeValue =
    Encoders.enum
      ( \case
          Structural -> "structural"
          Unique -> "unique"
      )

instance DecodeValue ModifierEnum where
  decodeValue =
    Decoders.enum
      ( \case
          "structural" -> Just Structural
          "unique" -> Just Unique
          _ -> Nothing
      )

newtype LocalTermBytes = LocalTermBytes ByteString
  deriving (EncodeValue, DecodeValue) via PG.RawBytes

newtype LocalTypeBytes = LocalTypeBytes ByteString
  deriving (EncodeValue, DecodeValue) via PG.RawBytes
