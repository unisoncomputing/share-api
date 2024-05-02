-- | Contains DB types for References and Referents
module Enlil.Postgres.Refs.Types
  ( PGReference,
    PGReferent,
    referenceFields,
    referentFields,
  )
where

import Enlil.Postgres.IDs
import Enlil.Prelude
import U.Codebase.Reference (Id' (..), Reference' (..))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent')
import U.Codebase.Referent qualified as Referent

-- | A V2 Reference with a Postgres ComponentHashId instead of a ComponentHash
type PGReference = Reference' Text ComponentHashId

-- | A V2 Referent with a Postgres ComponentHashId instead of a ComponentHash
type PGReferent = Referent' PGReference PGReference

-- | Useful for deconstructing a Reference into its fields for query interpolation.
referenceFields :: Reference' text hash -> (Maybe text, Maybe hash, Maybe Int64)
referenceFields = \case
  Reference.ReferenceBuiltin txt -> (Just txt, Nothing, Nothing)
  Reference.ReferenceDerived (Id h p) -> (Nothing, Just h, Just (either (error . show) id $ tryInto @Int64 p))

-- | Useful for deconstructing a Referent into its fields for query interpolation.
referentFields :: Referent' (Reference' text hash) (Reference' text hash) -> (Maybe text, Maybe hash, Maybe Int64, Maybe Int64)
referentFields ref = case (bimap referenceFields referenceFields ref) of
  Referent.Ref (t, h, i) -> (t, h, i, Nothing)
  Referent.Con (t, h, i) conId -> (t, h, i, Just (either (error . show) id $ tryInto @Int64 conId))
