{-# LANGUAGE RecordWildCards #-}

module Share.Postgres.NameLookups.Types
  ( NamesPerspective (..),
    ReversedName (..),
    ReversedPath (..),
    PathSegments (..),
    NamedRef (..),
    NamespaceText,
    NameLookupReceipt (..),
    pathSegmentsToText,
    textToPathSegments,
    reversedNameToNamespaceText,
    reversedNameToPathSegments,
    prefixNamedRef,
    prefixReversedName,
    stripPrefixFromNamedRef,
    pgNamedReferenceFields,
    pgNamedReferentFields,
    ref_,
    reversedNameToName,
  )
where

import Control.Lens hiding (from)
import Data.List.Extra qualified as List
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Hasql.Decoders qualified as Decoders
import Share.Postgres qualified as PG
import Share.Postgres.IDs (BranchHashId, ComponentHashId)
import Share.Postgres.Refs.Types
import Share.Prelude
import U.Codebase.Referent (ConstructorType)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))

-- | Proof that we've checked that a given name lookup exists before we try to use it.
data NameLookupReceipt = UnsafeNameLookupReceipt
  deriving (Eq, Show)

-- | Any time we need to lookup or search names we need to know what the scope of that search
-- should be. This can be complicated to keep track of, so this is a helper type to make it
-- easy to pass around.
--
-- You should use 'namesPerspectiveForRootAndPath' to construct this type.
--
-- E.g. if we're in loose code, we need to search the correct name lookup for the
-- user's perspective. If their perspective is "myprojects.json.latest.lib.base.data.List",
-- we need to search names using the name index mounted at "myprojects.json.latest.lib.base".
--
-- The NamesPerspective representing this viewpoint would be:
--
-- @@
-- NamesPerspective
--  { nameLookupBranchHashId = #libbasehash
--  , pathToMountedNameLookup = ["myprojects.json", "latest", "lib", "base"]
--  , relativePerspective = ["data", "List"]
--  }
-- @@
data NamesPerspective = NamesPerspective
  { -- | The branch hash of the name lookup we'll use for queries
    nameLookupBranchHashId :: BranchHashId,
    -- | Where the name lookup is mounted relative to the root branch
    pathToMountedNameLookup :: PathSegments,
    -- | The path to the perspective relative to the current name lookup
    relativePerspective :: PathSegments,
    nameLookupReceipt :: NameLookupReceipt
  }
  deriving (Eq, Show)

newtype ReversedName = ReversedName (NonEmpty Text)
  deriving stock (Eq, Ord, Show)

reversedNameToName :: ReversedName -> Name
reversedNameToName (ReversedName revName) =
  Name.fromReverseSegments (coerce @(NonEmpty Text) @(NonEmpty NameSegment) revName)

instance From ReversedName Name where
  from = reversedNameToName

instance PG.EncodeValue ReversedName where
  encodeValue =
    PG.encodeValue
      & contramap (\revSegs -> Text.intercalate "." (into @[Text] revSegs) <> ".")

instance PG.DecodeValue ReversedName where
  decodeValue =
    PG.decodeValue
      & Decoders.refine reversedNameFromText

instance From ReversedName (NonEmpty Text)

instance From (NonEmpty Text) ReversedName

instance From ReversedName [Text] where
  from (ReversedName n) = toList n

newtype ReversedPath = ReversedPath [Text]
  deriving (Eq, Ord, Show)

instance From ReversedPath [Text]

instance From [Text] ReversedPath

-- | TODO: Can probably remove this and just use Path since share is high enough in the
-- package hierarchy
newtype PathSegments = PathSegments [Text]
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance From PathSegments [Text]

instance From [Text] PathSegments

-- | A namespace rendered as a path, no leading '.'
-- E.g. "base.data"
type NamespaceText = Text

-- |
-- >>> pathSegmentsToText (PathSegments ["base", "data", "List"])
-- "base.data.List"
pathSegmentsToText :: PathSegments -> Text
pathSegmentsToText (PathSegments txt) = Text.intercalate "." txt

-- |
-- >>> textToPathSegments "base.data.List"
-- PathSegments ["base","data","List"]
textToPathSegments :: Text -> PathSegments
textToPathSegments txt = PathSegments $ Text.splitOn "." txt

-- |
-- >>> reversedSegmentsToNamespaceText (["List", "data", "base"])
-- "base.data.List"
reversedNameToNamespaceText :: ReversedName -> NamespaceText
reversedNameToNamespaceText (ReversedName txt) = Text.intercalate "." . reverse . toList $ txt

reversedNameToPathSegments :: ReversedName -> PathSegments
reversedNameToPathSegments (ReversedName revName) = PathSegments . reverse . toList $ revName

-- | Convert a reversed name into reversed segments.
--
-- >>> reversedNameFromText "foo.bar."
-- Right ("foo" :| ["bar"])
reversedNameFromText :: (HasCallStack) => Text -> Either Text ReversedName
reversedNameFromText txt =
  txt
    & Text.splitOn "."
    -- Names have a trailing dot, so we need to drop the last empty segment
    & List.dropEnd1
    & NonEmpty.nonEmpty
    & maybe (Left "Empty Name found when deserializing ReversedName") (Right . ReversedName)

data NamedRef ref = NamedRef {reversedSegments :: ReversedName, ref :: ref}
  deriving stock (Show, Functor, Foldable, Traversable)

ref_ :: Lens (NamedRef ref) (NamedRef ref') ref ref'
ref_ = lens ref (\namedRef ref -> namedRef {ref = ref})

instance PG.DecodeRow ref => PG.DecodeRow (NamedRef ref) where
  decodeRow = do
    reversedSegments <- PG.decodeField
    ref <- PG.decodeRow
    pure $ NamedRef {..}

pgNamedReferenceFields :: NamedRef PGReference -> (ReversedName, Maybe Text, Maybe ComponentHashId, Maybe Int64)
pgNamedReferenceFields NamedRef {ref, reversedSegments} =
  let (builtin, componentHash, componentIndex) = referenceFields ref
   in (reversedSegments, builtin, componentHash, componentIndex)

pgNamedReferentFields :: NamedRef (PGReferent, Maybe ConstructorType) -> (ReversedName, Maybe Text, Maybe ComponentHashId, Maybe Int64, Maybe Int64, Maybe ConstructorType)
pgNamedReferentFields NamedRef {ref = (referent, constructorType), reversedSegments} =
  let (builtin, componentHash, componentIndex, constructorIndex) = referentFields referent
   in (reversedSegments, builtin, componentHash, componentIndex, constructorIndex, constructorType)

-- | Requalifies a NamedRef to some namespace prefix.
prefixNamedRef :: PathSegments -> NamedRef ref -> NamedRef ref
prefixNamedRef prefix NamedRef {reversedSegments, ref} =
  NamedRef {reversedSegments = prefixReversedName prefix reversedSegments, ref}

-- | Requalifies a ReversedName to some namespace prefix.
prefixReversedName :: PathSegments -> ReversedName -> ReversedName
prefixReversedName (PathSegments prefix) (ReversedName reversedSegments) =
  ReversedName $ NonEmpty.appendl reversedSegments (reverse prefix)

-- | Strips a prefix path from a named ref. No-op if the prefix doesn't match.
--
-- >>> stripPrefixFromNamedRef (PathSegments ["foo", "bar"]) (NamedRef (ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| []), ref = ()}
--
-- >>> stripPrefixFromNamedRef (PathSegments ["no", "match"]) (NamedRef (ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| ["bar","foo"]), ref = ()}
stripPrefixFromNamedRef :: PathSegments -> NamedRef r -> NamedRef r
stripPrefixFromNamedRef (PathSegments prefix) namedRef =
  let newReversedName =
        reversedSegments namedRef
          & \case
            reversedName@(ReversedName (name NonEmpty.:| reversedPath)) ->
              case List.stripSuffix (reverse prefix) reversedPath of
                Nothing -> reversedName
                Just strippedReversedPath -> ReversedName (name NonEmpty.:| strippedReversedPath)
   in namedRef {reversedSegments = newReversedName}
