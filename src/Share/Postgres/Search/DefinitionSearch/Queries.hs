{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Search.DefinitionSearch.Queries
  ( submitReleaseToBeSynced,
    claimUnsyncedRelease,
    insertDefinitionDocuments,
    cleanIndexForRelease,
    defNameSearch,
    DefnNameSearchFilter (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hasql.Interpolate qualified as Hasql
import Share.BackgroundJobs.Search.DefinitionSync.Types
import Share.IDs (ProjectId, ReleaseId, UserId)
import Share.Postgres
import Share.Prelude
import Share.Utils.API (Limit, Query (Query))
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
insertDefinitionDocuments docs = pipelined $ do
  let docsTable = docRow <$> docs
  for_ docsTable \(projectId, releaseId, fqn, tokens, arity, tag, metadata) -> do
    -- Ideally we'd do a bulk insert, but Hasql doesn't provide any method for passing arrays of
    -- arrays as parameters, so instead we insert each record individually so we can use our
    -- only level of array-ness for the tokens.
    execute_ $
      [sql|
      INSERT INTO global_definition_search_docs (project_id, release_id, name, search_tokens, arity, tag, metadata)
        VALUES (#{projectId}, #{releaseId}, #{fqn}, array_to_tsvector(#{tokens}), #{arity}, #{tag}::definition_tag, #{metadata}::jsonb)
        ON CONFLICT DO NOTHING
      |]
  where
    docRow :: DefinitionDocument ProjectId ReleaseId Name (NameSegment, ShortHash) -> (ProjectId, ReleaseId, Text, [Text], Int32, TermOrTypeTag, Hasql.Jsonb)
    docRow DefinitionDocument {project, release, fqn, tokens, arity, tag, metadata} =
      ( project,
        release,
        Name.toText fqn,
        foldMap searchTokenToText $ Set.toList tokens,
        fromIntegral arity,
        tag,
        Hasql.Jsonb $ Aeson.toJSON metadata
      )

-- | Wipe out any rows for the given release, useful when re-indexing.
cleanIndexForRelease :: ReleaseId -> Transaction e ()
cleanIndexForRelease releaseId = do
  execute_
    [sql|
    DELETE FROM global_definition_search_docs
    WHERE release_id = #{releaseId}
    |]

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
    makeSearchToken :: Text -> Text -> Maybe (Maybe Occurrence) -> Text
    makeSearchToken kind txt occ = do
      Text.intercalate ":" $
        [kind, Text.replace ":" "" txt]
          <> case occ of
            Just (Just (Occurrence n)) -> [tShow n]
            Just Nothing -> ["r"]
            Nothing -> []

data DefnNameSearchFilter
  = ProjectFilter ProjectId
  | ReleaseFilter ReleaseId
  | UserFilter UserId

defNameSearch :: Maybe UserId -> Maybe DefnNameSearchFilter -> Query -> Limit -> Transaction e [(ProjectId, ReleaseId, Name)]
defNameSearch mayCaller mayFilter (Query query) limit = do
  let filters = case mayFilter of
        Just (ProjectFilter projId) -> [sql| AND doc.project_id = #{projId} |]
        Just (ReleaseFilter relId) -> [sql| AND doc.release_id = #{relId} |]
        Just (UserFilter userId) -> [sql| AND p.owner_id = #{userId} |]
        Nothing -> mempty
  queryListRows @(ProjectId, ReleaseId, Name)
    [sql|
    WITH matches_deduped_by_project(project_id, release_id, name, tag) AS (
      SELECT DISTINCT ON (doc.project_id, doc.name) doc.project_id, doc.release_id, doc.name, doc.tag FROM global_definition_search_docs doc
        JOIN projects p ON p.id = doc.project_id
        JOIN project_releases r ON r.id = doc.release_id
        WHERE
          -- Search name by a trigram 'word similarity'
          -- which will match if the query is similar to any 'word' (e.g. name segment)
          -- in the name.
          #{query} <% doc.name
        AND (NOT p.private OR (#{mayCaller} IS NOT NULL AND EXISTS (SELECT FROM accessible_private_projects pp WHERE pp.user_id = #{mayCaller} AND pp.project_id = p.id)))
          ^{filters}
        ORDER BY doc.project_id, doc.name, r.major_version, r.minor_version, r.patch_version
    ),
    -- Find the <limit> best matches
    best_results(project_id, release_id, name, tag)  AS (
      SELECT m.project_id, m.release_id, m.name, m.tag
        FROM matches_deduped_by_project m
        ORDER BY similarity(#{query}, m.name) DESC
        LIMIT #{limit}
    )
    -- THEN sort docs to the bottom.
    SELECT br.project_id, br.release_id, br.name
        FROM best_results br
        -- docs and tests to the bottom, but otherwise sort by quality of the match.
        ORDER BY (br.tag <> 'doc'::definition_tag, br.tag <> 'test'::definition_tag, br.name LIKE ('%' || like_escape(#{query})), similarity(#{query}, br.name)) DESC
  |]
