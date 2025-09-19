-- | Utils for working with `unison` packages and types
module Share.Utils.Unison where

import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding qualified as Text
import Servant
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors qualified as Errors
import Unison.Hash qualified as Hash
import Unison.Hash32 qualified as Hash32

newtype InvalidCausalHash = InvalidCausalHash CausalHash
  deriving stock (Show, Eq)

instance Errors.ToServerError InvalidCausalHash where
  toServerError (InvalidCausalHash (CausalHash ch)) =
    (Errors.ErrorID "invalid-causal-hash", err400 {errBody = "Invalid causal hash: " <> (BL.fromStrict . Text.encodeUtf8 . Hash.toBase32HexText $ ch)})

instance Logging.Loggable InvalidCausalHash where
  toLog (InvalidCausalHash ch) = Logging.withSeverity Logging.UserFault . Logging.textLog $ "Invalid causal hash: " <> (tShow ch)

causalHashToHash32 :: CausalHash -> Hash32.Hash32
causalHashToHash32 (CausalHash ch) = Hash32.fromHash ch

hash32ToCausalHash :: Hash32.Hash32 -> CausalHash
hash32ToCausalHash = CausalHash . Hash32.toHash

-- |  Helper types for handling unstructured query params from unison request types
type ProjectShortHandParam = Text

type ProjectIdParam = Text

type BranchIdParam = Text

type BranchNameParam = Text
