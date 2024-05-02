{-# LANGUAGE DeriveAnyClass #-}

module Enlil.Postgres.Serialization
  ( decodeTermAndType,
    encodeTermAndType,
    decodeTermComponentElementType,
    decodeDecl,
    encodeDecl,
    encodeEvalResult,
    decodeEvalResult,
    encodeNamespace,
    decodeNamespace,
    encodePatch,
    decodePatch,
    decodeTypedTempEntity,
  )
where

import Data.Bytes.Get (runGetS)
import Data.Text qualified as Text
import Enlil.Prelude
import Enlil.Utils.Logging
import Enlil.Utils.Logging qualified as Logging
import Enlil.Web.Errors (ErrorID (..), ToServerError (..))
import Servant (err500)
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Decode qualified as SqliteDecoders
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull
import U.Codebase.Sqlite.Serialization qualified as SS
import U.Codebase.Sqlite.Symbol qualified as Sqlite
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.TempEntityType (TempEntityType)
import U.Codebase.Sqlite.TempEntityType qualified as TempEntityType
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Util.Serialization (Get, putBytes)

data DecodeError = DecodeError
  { decoder :: Text, -- the name of the decoder
    err :: Text -- the error message
  }
  deriving stock (Show)
  deriving anyclass (Exception)

instance From SqliteDecoders.DecodeError DecodeError where
  from (SqliteDecoders.DecodeError decoder err) = DecodeError decoder (Text.pack err)

instance From DecodeError SqliteDecoders.DecodeError where
  from (DecodeError decoder err) = SqliteDecoders.DecodeError decoder (Text.unpack err)

instance ToServerError DecodeError where
  toServerError (DecodeError decoder _err) = (ErrorID $ "decode-error:" <> decoder, err500)

instance Logging.Loggable DecodeError where
  toLog (DecodeError decoder err) =
    Logging.textLog ("Error in decoder: " <> decoder <> ": " <> err)
      & Logging.withSeverity Logging.Error

getFromBytesOr :: Text -> Get a -> ByteString -> Either DecodeError a
getFromBytesOr decoder get bs = case runGetS get bs of
  Left err -> Left (DecodeError decoder $ Text.pack err)
  Right a -> Right a

-- | Decode a term and its type from a bytestring.
decodeTermAndType :: ByteString -> Either DecodeError (TermFormat.Term, TermFormat.Type)
decodeTermAndType = getFromBytesOr "getTerm" SS.getTermAndType

-- | Encode a term and its type to a bytestring.
encodeTermAndType :: TermFormat.Term -> TermFormat.Type -> ByteString
encodeTermAndType trm typ = putBytes SS.putTermAndType (trm, typ)

-- | Given the bytes for a term component element, decode ONLY the type of the component element.
-- This is useful during sync when we need the type for some indexes, but want to avoid the
-- work of decoding the entire term component element.
decodeTermComponentElementType :: ByteString -> Either DecodeError TermFormat.Type
decodeTermComponentElementType = getFromBytesOr "getTermComponentElementType" SS.getTypeFromTermAndType

encodeEvalResult :: TermFormat.Term -> ByteString
encodeEvalResult trm = putBytes SS.putSingleTerm trm

decodeEvalResult :: ByteString -> Either DecodeError TermFormat.Term
decodeEvalResult = getFromBytesOr "getEvalResult" SS.getSingleTerm

-- | Decode a data or ability declaration from a bytestring.
decodeDecl :: ByteString -> Either DecodeError (DeclFormat.Decl Sqlite.Symbol)
decodeDecl = getFromBytesOr "getDecl" SS.getDeclElement

-- | Encode a data or ability declaration to a bytestring.
encodeDecl :: DeclFormat.Decl Sqlite.Symbol -> ByteString
encodeDecl decl = putBytes SS.putDeclElement decl

decodeNamespace :: ByteString -> Either DecodeError BranchFull.LocalBranch
decodeNamespace = getFromBytesOr "getLocalBranch" SS.getLocalBranch

encodeNamespace :: BranchFull.LocalBranch -> ByteString
encodeNamespace = putBytes SS.putLocalBranch

decodePatch :: ByteString -> Either DecodeError PatchFull.LocalPatch
decodePatch = getFromBytesOr "getLocalBranch" SS.getLocalPatch

encodePatch :: PatchFull.LocalPatch -> ByteString
encodePatch = putBytes SS.putLocalPatch

-- | Decode a temp entity to the appropriate type.
decodeTypedTempEntity :: TempEntityType -> ByteString -> Either DecodeError TempEntity
decodeTypedTempEntity typeId blob = first convertDecodeError $
  case typeId of
    TempEntityType.TermComponentType -> Entity.TC <$> SqliteDecoders.decodeTempTermFormat blob
    TempEntityType.DeclComponentType -> Entity.DC <$> SqliteDecoders.decodeTempDeclFormat blob
    TempEntityType.NamespaceType -> Entity.N <$> SqliteDecoders.decodeTempNamespaceFormat blob
    TempEntityType.PatchType -> Entity.P <$> SqliteDecoders.decodeTempPatchFormat blob
    TempEntityType.CausalType -> Entity.C <$> SqliteDecoders.decodeTempCausalFormat blob
  where
    convertDecodeError :: SqliteDecoders.DecodeError -> DecodeError
    convertDecodeError (SqliteDecoders.DecodeError {decoder, err}) =
      DecodeError {decoder, err = Text.pack err}
