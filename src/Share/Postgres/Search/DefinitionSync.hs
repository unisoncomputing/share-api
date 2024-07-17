{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Search.DefinitionSync
  ( submitReleaseToBeSynced,
    claimUnsyncedRelease,
    insertDefinitionDocuments,
  )
where

import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hasql.Interpolate qualified as Hasql
import Share.BackgroundJobs.Search.DefinitionSync.Types
import Share.IDs (ProjectId, ReleaseId)
import Share.Postgres
import Share.Postgres qualified as PG
import Share.Prelude
import Unison.NameSegment (NameSegment)
import Unison.Server.Types (TermTag (..), TypeTag (..))
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment

submitReleaseToBeSynced :: ReleaseId -> Transaction e ()
submitReleaseToBeSynced releaseId = do
  execute_
    [sql|
    INSERT INTO global_definition_search_release_queue (id)
    VALUES (#{releaseId})
    |]

-- | Claim the oldest unsynced release to be indexed.
claimUnsyncedRelease :: Transaction e (Maybe ReleaseId)
claimUnsyncedRelease = do
  query1Col
    [sql|
    WITH chosen_release AS (
      SELECT q.id
      FROM global_definition_search_release_queue q
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM global_definition_search_release_queue
      USING chosen_release
      WHERE global_definition_search_release_queue.id = chosen_release.id
    RETURNING chosen_release.id
    |]

-- | Save definition documents to be indexed for search.
insertDefinitionDocuments :: [DefinitionDocument ProjectId ReleaseId] -> Transaction e ()
insertDefinitionDocuments docs = do
  let docsTable = docRow <$> docs
  execute_ $
    [sql|
    INSERT INTO global_definition_search_docs (project_id, release_id, name, search_tokens, metadata)
      SELECT * FROM ^{PG.toTable docsTable}
      ON CONFLICT DO NOTHING
    |]
  where
    docRow :: DefinitionDocument ProjectId ReleaseId -> (ProjectId, ReleaseId, Text, [Text], Hasql.Jsonb)
    docRow DefinitionDocument {project, release, fqn, tokens, metadata} =
      ( project,
        release,
        Name.toText fqn,
        searchTokensToTSVectorArray $ Set.toList tokens,
        Hasql.Jsonb $ Aeson.toJSON metadata
      )

-- | Prepare search tokens in a standard format for indexing.
searchTokensToTSVectorArray :: [DefnSearchToken NameSegment] -> [Text]
searchTokensToTSVectorArray tokens = do
  searchTokensToTSVector <$> tokens

-- | Convert a search token to a TSVector.
--
-- >>> import Unison.Syntax.Name qualified as Name
-- >>> searchTokensToTSVector (NameToken (Name.unsafeParseText "my.cool.name"))
-- "n:my.cool.name"
--
-- >>> searchTokensToTSVector (TypeMentionToken (NameSegment.unsafeParseText "Nat") (Occurrence 1))
-- "m:Nat:1"
--
-- >>> searchTokensToTSVector (TypeVarToken (VarId 1) (Occurrence 1))
-- "v:1:1"
--
-- >>> searchTokensToTSVector (TermTagToken Doc)
-- "t:doc"
--
-- >>> searchTokensToTSVector (TermTagToken (Constructor Data))
-- "t:data-con"
--
-- >>> searchTokensToTSVector (TypeTagToken Data)
-- "t:data"
searchTokensToTSVector :: DefnSearchToken NameSegment -> Text
searchTokensToTSVector = \case
  NameToken name -> makeSearchToken nameType (Name.toText name) Nothing
  TypeMentionToken t occ -> makeSearchToken typeMentionType (NameSegment.toEscapedText t) (Just occ)
  TypeVarToken varId occ -> makeSearchToken typeVarType (varIdText varId) (Just occ)
  HashToken sh -> makeSearchToken hashType (into @Text sh) Nothing
  TermTagToken termTag -> makeSearchToken tagType (termTagText termTag) Nothing
  TypeTagToken typTag -> makeSearchToken tagType (typeTagText typTag) Nothing
  where
    varIdText :: VarId -> Text
    varIdText (VarId n) = tShow n
    termTagText :: TermTag -> Text
    termTagText = \case
      Doc -> "doc"
      Test -> "test"
      Plain -> "plain"
      Constructor typeTag -> typeTagText typeTag <> "-con"
    typeTagText :: TypeTag -> Text
    typeTagText = \case
      Data -> "data"
      Ability -> "ability"
    nameType :: Text
    nameType = "n"
    typeMentionType :: Text
    typeMentionType = "m"
    typeVarType :: Text
    typeVarType = "v"
    hashType :: Text
    hashType = "h"
    tagType :: Text
    tagType = "t"
    makeSearchToken :: Text -> Text -> Maybe Occurrence -> Text
    makeSearchToken kind txt occ = do
      Text.intercalate ":" $
        [kind, Text.replace ":" "" txt]
          <> case occ of
            Just (Occurrence n) -> [tShow n]
            Nothing -> []
