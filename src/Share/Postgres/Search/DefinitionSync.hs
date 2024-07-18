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
import Unison.DataDeclaration qualified as DD
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Server.Types (TermTag (..), TypeTag (..))
import Unison.ShortHash (ShortHash)
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
    WITH chosen_release(release_id) AS (
      SELECT q.release_id
      FROM global_definition_search_release_queue q
      ORDER BY q.created_at ASC
      LIMIT 1
      -- Skip any that are being synced by other workers.
      FOR UPDATE SKIP LOCKED
    )
    DELETE FROM global_definition_search_release_queue
      USING chosen_release
      WHERE global_definition_search_release_queue.release_id = chosen_release.release_id
    RETURNING chosen_release.release_id
    |]

-- | Save definition documents to be indexed for search.
insertDefinitionDocuments :: [DefinitionDocument ProjectId ReleaseId Name (NameSegment, ShortHash)] -> Transaction e ()
insertDefinitionDocuments docs = do
  let docsTable = docRow <$> docs
  execute_ $
    [sql|
    INSERT INTO global_definition_search_docs (project_id, release_id, name, search_tokens, metadata)
      SELECT * FROM ^{PG.toTable docsTable}
      ON CONFLICT DO NOTHING
    |]
  where
    docRow :: DefinitionDocument ProjectId ReleaseId Name (NameSegment, ShortHash) -> (ProjectId, ReleaseId, Text, [Text], Hasql.Jsonb)
    docRow DefinitionDocument {project, release, fqn, tokens, metadata} =
      ( project,
        release,
        Name.toText fqn,
        foldMap searchTokenToText $ Set.toList tokens,
        Hasql.Jsonb $ Aeson.toJSON metadata
      )

-- | Convert a search token to a TSVector.
--
-- >>> import Unison.Syntax.Name qualified as Name
-- >>> searchTokenToText (NameToken (Name.unsafeParseText "my.cool.name"))
-- ["n:my.cool.name"]
--
-- >>> import Unison.ShortHash qualified as SH
-- >>> import Data.Maybe (fromJust)
-- >>> searchTokenToText (TypeMentionToken (NameSegment.unsafeParseText "Thing", fromJust $ SH.fromText "#2tWjVAuc7") (Occurrence 1))
-- ["mn:Thing:1","mh:#2tWjVAuc7:1"]
--
-- >>> searchTokenToText (TypeVarToken (VarId 1) (Occurrence 1))
-- ["v:1:1"]
--
-- >>> searchTokenToText (TermTagToken Doc)
-- ["t:doc"]
-- >>> searchTokenToText (TermTagToken (Constructor Data))
-- WAS WAS WAS "t:data-con"
-- ["t:data-con"]
-- >>> searchTokenToText (TypeTagToken Data)
-- ["t:data"]
searchTokenToText :: DefnSearchToken (NameSegment, ShortHash) -> [Text]
searchTokenToText = \case
  NameToken name -> [makeSearchToken nameType (Name.toText name) Nothing]
  TypeMentionToken (ns, typeRef) occ ->
    [ makeSearchToken typeMentionTypeByNameType (NameSegment.toEscapedText ns) (Just occ),
      makeSearchToken typeMentionTypeByHashType (into @Text @ShortHash typeRef) (Just occ)
    ]
  TypeVarToken varId occ -> [makeSearchToken typeVarType (varIdText varId) (Just occ)]
  HashToken sh -> [makeSearchToken hashType (into @Text sh) Nothing]
  TermTagToken termTag -> [makeSearchToken tagType (termTagText termTag) Nothing]
  TypeTagToken typTag -> [makeSearchToken tagType (typeTagText typTag) Nothing]
  TypeModToken mod -> [makeSearchToken typeModType (typeModText mod) Nothing]
  where
    typeModText = \case
      DD.Structural -> "structural"
      DD.Unique {} -> "unique"
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
    typeMentionTypeByNameType :: Text
    typeMentionTypeByNameType = "mn"
    typeMentionTypeByHashType :: Text
    typeMentionTypeByHashType = "mh"
    typeVarType :: Text
    typeVarType = "v"
    hashType :: Text
    hashType = "h"
    tagType :: Text
    tagType = "t"
    typeModType :: Text
    typeModType = "mod"
    makeSearchToken :: Text -> Text -> Maybe Occurrence -> Text
    makeSearchToken kind txt occ = do
      Text.intercalate ":" $
        [kind, Text.replace ":" "" txt]
          <> case occ of
            Just (Occurrence n) -> [tShow n]
            Nothing -> []
