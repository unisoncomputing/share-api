{-# LANGUAGE TypeOperators #-}

-- | This module contains queries for the definition search index.
module Share.Postgres.Search.DefinitionSearch.Queries
  ( submitReleaseToBeSynced,
    claimUnsyncedRelease,
    insertDefinitionDocuments,
    cleanIndexForRelease,
    defNameCompletionSearch,
    definitionTokenSearch,
    definitionNameSearch,
    DefnNameSearchFilter (..),
    -- Exported for logging/debugging
    searchTokensToTsQuery,
  )
where

import Control.Lens
import Data.Aeson (fromJSON)
import Data.Aeson qualified as Aeson
import Data.Foldable qualified as Foldable
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hasql.Interpolate qualified as Hasql
import Servant (ServerError (..))
import Servant.Server (err500)
import Share.BackgroundJobs.Search.DefinitionSync.Types
import Share.IDs (ProjectId, ReleaseId, UserId)
import Share.Postgres
import Share.Prelude
import Share.Utils.API (Limit, Query (Query))
import Share.Utils.Logging qualified as Logging
import Share.Web.Errors qualified as Errors
import Unison.DataDeclaration qualified as DD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Server.Types (TermTag (..), TypeTag (..))
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment

data DefinitionSearchError = FailedToDecodeMetadata Aeson.Value Text
  deriving stock (Show, Eq, Ord)

instance Errors.ToServerError DefinitionSearchError where
  toServerError = \case
    FailedToDecodeMetadata _v _err -> (Errors.ErrorID "invalid-definition-search-metadata", err500 {errBody = "Internal Server Error"})

instance Logging.Loggable DefinitionSearchError where
  toLog = \case
    FailedToDecodeMetadata v err ->
      Logging.textLog ("Failed to decode metadata: " <> tShow v <> " " <> err)
        & Logging.withSeverity Logging.Error

submitReleaseToBeSynced :: (QueryM m) => ReleaseId -> m ()
submitReleaseToBeSynced releaseId = do
  execute_
    [sql|
    INSERT INTO global_definition_search_release_queue (release_id)
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
insertDefinitionDocuments :: [DefinitionDocument ProjectId ReleaseId Name (Name, ShortHash)] -> Transaction e ()
insertDefinitionDocuments docs = pipelined $ do
  let docsTable = docRow <$> docs
  execute_ $
    [sql|
      WITH docs(project_id, release_id, name, token_text, arity, tag, metadata) AS (
        SELECT * FROM ^{toTable docsTable}
      ) INSERT INTO global_definition_search_docs (project_id, release_id, name, search_tokens, arity, tag, metadata)
        SELECT d.project_id, d.release_id, d.name, tsvector(d.token_text::text), d.arity, d.tag::definition_tag, d.metadata
          FROM docs d
        ON CONFLICT DO NOTHING
      |]
  where
    docRow :: DefinitionDocument ProjectId ReleaseId Name (Name, ShortHash) -> (ProjectId, ReleaseId, Text, Text, Arity, TermOrTypeTag, Hasql.Jsonb)
    docRow DefinitionDocument {project, release, fqn, tokens, arity, tag, metadata} =
      let expandedTokens :: [DefnSearchToken (Either Name ShortHash)]
          expandedTokens =
            tokens & foldMap \case
              NameToken name -> [NameToken name]
              TypeMentionToken (name, ref) occ -> [TypeMentionToken (Left name) occ, TypeMentionToken (Right ref) occ]
              TypeVarToken v occ -> [TypeVarToken v occ]
              HashToken sh -> [HashToken sh]
              TermTagToken tag -> [TermTagToken tag]
              TypeTagToken tag -> [TypeTagToken tag]
              TypeModToken mod -> [TypeModToken mod]
       in ( project,
            release,
            -- Prefix all names with '.' in the index
            -- It helps with prefix matching by segment.
            Name.toText . Name.makeAbsolute $ fqn,
            Text.unwords (searchTokenToText False <$> expandedTokens),
            arity,
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
--
-- Note: Names in tokens have their segments reversed, this is because PG Gin indexes only support
-- prefix-matching on lexemes, and this lets us match on any valid name suffix.
-- This is also why Hashes and Names come LAST in the token, so we can do, e.g. `mn,1,map.List:*`
--
-- >>> import Unison.Syntax.Name qualified as Name
-- >>> searchTokenToText False (NameToken (Name.unsafeParseText "my.cool.name"))
-- "n,name.cool.my"
--
-- >>> searchTokenToText False (TypeMentionToken (Left $ Name.unsafeParseText "Thing") (Just $ Occurrence 1))
-- "mn,1,Thing"
--
-- >>> import Unison.ShortHash qualified as SH
-- >>> import Data.Maybe (fromJust)
-- >>> searchTokenToText False (TypeMentionToken (Right . fromJust $ SH.fromText "#2tWjVAuc7") (Just $ Occurrence 1))
-- "mh,1,#2tWjVAuc7"
--
-- >>> searchTokenToText False (TypeMentionToken (Left $ Name.unsafeParseText "Thing") Nothing)
-- "mn,r,Thing"
--
-- >>> searchTokenToText False (TypeMentionToken (Right $ fromJust $ SH.fromText "#2tWjVAuc7") Nothing)
-- "mh,r,#2tWjVAuc7"
--
-- >>> searchTokenToText False (TypeVarToken (VarId 1) (Just $ Occurrence 2))
-- "v,2,1"
--
-- >>> searchTokenToText False (TermTagToken Doc)
-- "t,doc"
-- >>> searchTokenToText False (TermTagToken (Constructor Data))
-- "t,data-con"
-- >>> searchTokenToText False (TypeTagToken Data)
-- "t,data"
--
-- Should backslash escape special symbols.
-- >>> searchTokenToText False (NameToken (Name.unsafeParseText "my.name!"))
-- "n,name\\!.my"
--
-- >>> searchTokenToText False (NameToken (Name.unsafeParseText "operators.\\&:|!|"))
-- "n,\\\\\\&\\:\\|\\!\\|.operators"
--
-- Should add wildcards to the end of name, hash, and type mention tokens, but not others.
-- >>> searchTokenToText True (NameToken (Name.unsafeParseText "my.name"))
-- "n,name.my:*"
--
-- >>> searchTokenToText True (TypeMentionToken (Left $ Name.unsafeParseText "Thing") (Just $ Occurrence 1))
-- "mn,1,Thing:*"
--
-- >>> searchTokenToText True (TypeMentionToken (Right . fromJust $ SH.fromText "#2tWjVAuc7") (Just $ Occurrence 1))
-- "mh,1,#2tWjVAuc7:*"
--
-- >>> searchTokenToText True (TypeVarToken (VarId 1) (Just $ Occurrence 1))
-- "v,1,1"
searchTokenToText :: Bool -> DefnSearchToken (Either Name ShortHash) -> Text
searchTokenToText shouldAddWildcards = \case
  NameToken name ->
    makeSearchToken nameType (reversedNameText name) Nothing
      & addWildCard
  TypeMentionToken (Left name) occ ->
    -- We normalize the name to lowercase for case-insensitive search, but only for type name
    -- mentions.
    makeSearchToken typeMentionTypeByNameType (Text.toLower (reversedNameText name)) (Just occ)
      & addWildCard
  TypeMentionToken (Right sh) occ ->
    makeSearchToken typeMentionTypeByHashType (into @Text @ShortHash sh) (Just occ)
      & addWildCard
  TypeVarToken varId occ -> makeSearchToken typeVarType (varIdText varId) (Just occ)
  HashToken sh ->
    makeSearchToken hashType (into @Text sh) Nothing
      & addWildCard
  TermTagToken termTag -> makeSearchToken tagType (termTagText termTag) Nothing
  TypeTagToken typTag -> makeSearchToken tagType (typeTagText typTag) Nothing
  TypeModToken mod -> makeSearchToken typeModType (typeModText mod) Nothing
  where
    addWildCard token = if shouldAddWildcards then (token <> ":*") else token
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
    escapeToken :: Text -> Text
    escapeToken txt =
      txt
        -- FIRST we escape all existing backslashes
        & Text.replace "\\" "\\\\"
        -- Then fold over the provided characters, escaping them with a preceding backslash
        & \t -> foldr (\c acc -> Text.replace (Text.singleton c) (Text.pack ['\\', c]) acc) t ("()|& :*!," :: String)
    makeSearchToken :: Text -> Text -> Maybe OccurrenceKind -> Text
    makeSearchToken kind txt occTxt = do
      let occ = case occTxt of
            Just (Count (Occurrence n)) -> [tShow n]
            Just ReturnPosition -> ["r"]
            Nothing -> []
       in Text.intercalate "," $
            [kind] <> occ <> [escapeToken txt]

reversedNameText :: Name -> Text
reversedNameText n =
  n
    & Name.reverseSegments
    & fmap NameSegment.toEscapedText
    & Foldable.toList
    & Text.intercalate "."
    -- We add a trailing dot so we can match on "Name.:*"to find versions of Name in different namespaces,
    -- but don't match 'Interval' for 'Int', etc.
    & (<> ".")

searchTokensToTsQuery :: Set (DefnSearchToken (Either Name ShortHash)) -> Text
searchTokensToTsQuery tokens =
  tokens
    & Set.toList
    & fmap (searchTokenToText True)
    & Text.intercalate " & "

data DefnNameSearchFilter
  = ProjectFilter ProjectId
  | ReleaseFilter ReleaseId
  | UserFilter UserId

-- | Find names which would be valid completions for the given query.
defNameCompletionSearch :: Maybe UserId -> Maybe DefnNameSearchFilter -> Query -> Limit -> Transaction e [(ProjectId, ReleaseId, Name, TermOrTypeTag)]
defNameCompletionSearch mayCaller mayFilter (Query query) limit = do
  let filters = case mayFilter of
        Just (ProjectFilter projId) -> [sql| AND doc.project_id = #{projId} |]
        Just (ReleaseFilter relId) -> [sql| AND doc.release_id = #{relId} |]
        Just (UserFilter userId) -> [sql| AND p.owner_user_id = #{userId} |]
        Nothing -> mempty
  queryListRows @(ProjectId, ReleaseId, Name, TermOrTypeTag)
    [sql|
    WITH matches_deduped_by_project(project_id, release_id, name, tag) AS (
      SELECT DISTINCT ON (doc.project_id, doc.name) doc.project_id, doc.release_id, doc.name, doc.tag FROM global_definition_search_docs doc
        JOIN projects p ON p.id = doc.project_id
        JOIN project_releases r ON r.id = doc.release_id
        WHERE
          -- Find names which contain the query
          doc.name ILIKE ('%.' || like_escape(#{query}) || '%')
        AND (NOT p.private OR (#{mayCaller} IS NOT NULL AND EXISTS (SELECT FROM accessible_private_projects pp WHERE pp.user_id = #{mayCaller} AND pp.project_id = p.id)))
          ^{filters}
        ORDER BY doc.project_id, doc.name, r.major_version, r.minor_version, r.patch_version
    ),
    -- Find the <limit> best matches
    best_results(project_id, release_id, name, tag)  AS (
      SELECT m.project_id, m.release_id, m.name, m.tag
        FROM matches_deduped_by_project m
        -- Prefer matches where the query is near the end of the name,
        -- e.g. for query 'List', 'data.List' should come before 'data.List.map'
        ORDER BY length(m.name) - position(LOWER(#{query}) in LOWER(m.name)) ASC
        LIMIT #{limit}
    )
    -- THEN sort docs to the bottom.
    SELECT br.project_id, br.release_id, br.name, br.tag
        FROM best_results br
        -- docs and tests to the bottom, but otherwise sort by the quality of the match.
        -- e.g. for query 'List', 'data.List' should come before 'data.List.map'
        ORDER BY br.tag <> 'doc'::definition_tag DESC, br.tag <> 'test'::definition_tag DESC,
                 length(br.name) - position(LOWER(#{query}) in LOWER(br.name)) ASC
  |]
    -- Names are stored in absolute form, but we usually work with them in relative form.
    <&> over (traversed . _3) Name.makeRelative

-- | Perform a type search for the given tokens.
definitionTokenSearch :: Maybe UserId -> Maybe DefnNameSearchFilter -> Limit -> Set (DefnSearchToken (Either Name ShortHash)) -> Maybe Arity -> Transaction e [(ProjectId, ReleaseId, Name, TermOrTypeSummary)]
definitionTokenSearch mayCaller mayFilter limit searchTokens preferredArity = do
  let filters = case mayFilter of
        Just (ProjectFilter projId) -> [sql| AND doc.project_id = #{projId} |]
        Just (ReleaseFilter relId) -> [sql| AND doc.release_id = #{relId} |]
        Just (UserFilter userId) -> [sql| AND p.owner_user_id = #{userId} |]
        Nothing -> mempty
  let (regularTokens, returnTokens) =
        searchTokens & foldMap \token -> case token of
          TypeMentionToken _ ReturnPosition -> (mempty, Set.singleton token)
          TypeVarToken _ ReturnPosition -> (mempty, Set.singleton token)
          _ -> (Set.singleton token, mempty)
  let tsQueryText = searchTokensToTsQuery regularTokens
  let returnTokensText = searchTokensToTsQuery returnTokens
  rows <-
    queryListRows @(ProjectId, ReleaseId, Name, Hasql.Jsonb)
      [sql|
    WITH matches_deduped_by_project(project_id, release_id, name, arity, metadata, search_tokens) AS (
      SELECT DISTINCT ON (doc.project_id, doc.name) doc.project_id, doc.release_id, doc.name, doc.arity, doc.metadata, doc.search_tokens FROM global_definition_search_docs doc
        JOIN projects p ON p.id = doc.project_id
        JOIN project_releases r ON r.id = doc.release_id
        WHERE
          -- match on search tokens using GIN index.
          tsquery(#{tsQueryText}) @@ doc.search_tokens
        AND (NOT p.private OR (#{mayCaller} IS NOT NULL AND EXISTS (SELECT FROM accessible_private_projects pp WHERE pp.user_id = #{mayCaller} AND pp.project_id = p.id)))
          ^{filters}
        ORDER BY doc.project_id, doc.name, r.major_version, r.minor_version, r.patch_version
    ) SELECT m.project_id, m.release_id, m.name, m.metadata
        FROM matches_deduped_by_project m
        -- Score matches by:
        -- - Whether arity is equal or greater than the preferred arity
        -- - Whether the return type of the query matches the type signature
        -- - Prefer shorter arities
        -- - Prefer less complex type signatures (by number of tokens)
        ORDER BY (m.arity >= #{preferredArity}) DESC, tsquery(#{returnTokensText}) @@ m.search_tokens DESC, m.arity ASC, length(m.search_tokens) ASC
        LIMIT #{limit}
  |]
  rows
    & over (traversed . _3) Name.makeRelative
    & traverseOf
      (traversed . _4)
      ( \(Hasql.Jsonb v) -> do
          case fromJSON v of
            Aeson.Error err -> unrecoverableError $ FailedToDecodeMetadata v (Text.pack err)
            Aeson.Success summary -> pure summary
      )

-- | Perform a fuzzy trigram search on definition names
definitionNameSearch :: Maybe UserId -> Maybe DefnNameSearchFilter -> Limit -> Query -> Transaction e [(ProjectId, ReleaseId, Name, TermOrTypeSummary)]
definitionNameSearch mayCaller mayFilter limit (Query query) = do
  let filters = case mayFilter of
        Just (ProjectFilter projId) -> [sql| AND doc.project_id = #{projId} |]
        Just (ReleaseFilter relId) -> [sql| AND doc.release_id = #{relId} |]
        Just (UserFilter userId) -> [sql| AND p.owner_user_id = #{userId} |]
        Nothing -> mempty
  rows <-
    queryListRows @(ProjectId, ReleaseId, Name, Hasql.Jsonb)
      [sql|
    WITH matches_deduped_by_project(project_id, release_id, name, metadata) AS (
      SELECT DISTINCT ON (doc.project_id, doc.name) doc.project_id, doc.release_id, doc.name, doc.metadata FROM global_definition_search_docs doc
        JOIN projects p ON p.id = doc.project_id
        JOIN project_releases r ON r.id = doc.release_id
        WHERE
          -- Adjust the similarity threshold as needed (between 0 and 1)
          word_similarity(#{query}, doc.name) >= 0.5
        AND (NOT p.private OR (#{mayCaller} IS NOT NULL AND EXISTS (SELECT FROM accessible_private_projects pp WHERE pp.user_id = #{mayCaller} AND pp.project_id = p.id)))
          ^{filters}
        ORDER BY doc.project_id, doc.name, r.major_version, r.minor_version, r.patch_version
    ) SELECT m.project_id, m.release_id, m.name, m.metadata
        FROM matches_deduped_by_project m
        -- Score matches by:
        -- - whether it contains the exact provided spelling
        -- - how close the query is to the END of the name (generally we want to match the last segment)
        -- - similarity, just in case the query is a bit off
        ORDER BY m.name ILIKE ('%.' || #{query} || '%') DESC, length(m.name) - position(LOWER(#{query}) in LOWER(m.name)) ASC, word_similarity(#{query}, m.name) DESC
        LIMIT #{limit}
  |]
  rows
    & over (traversed . _3) Name.makeRelative
    & traverseOf
      (traversed . _4)
      ( \(Hasql.Jsonb v) -> do
          case fromJSON v of
            Aeson.Error err -> unrecoverableError $ FailedToDecodeMetadata v (Text.pack err)
            Aeson.Success summary -> pure summary
      )
