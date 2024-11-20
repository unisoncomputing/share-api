{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.Contributions.Queries
  ( createContribution,
    contributionByProjectIdAndNumber,
    shareContributionByProjectIdAndNumber,
    listContributionsByProjectId,
    contributionById,
    updateContribution,
    insertContributionStatusChangeEvent,
    contributionStatusChangeEventsByContributionId,
    listContributionsByUserId,
    shareContributionsByBranchOf,
    getPagedShareContributionTimelineByProjectIdAndNumber,
    performMergesAndBCAUpdatesFromBranchPush,
    rebaseContributionsFromMergedBranches,
    contributionStateTokenById,
    getPrecomputedNamespaceDiff,
    savePrecomputedNamespaceDiff,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Safe (lastMay)
import Share.Codebase.Types (CodebaseEnv (..))
import Share.Contribution (Contribution (..), ContributionStatus (..))
import Share.IDs
import Share.Postgres qualified as PG
import Share.Postgres.Comments.Queries (commentsByTicketOrContribution)
import Share.Postgres.IDs
import Share.Prelude
import Share.Utils.API
import Share.Web.Errors
import Share.Web.Share.Contributions.API (ContributionTimelineCursor, ListContributionsCursor)
import Share.Web.Share.Contributions.Types

createContribution ::
  -- | Author
  UserId ->
  ProjectId ->
  -- | Title
  Text ->
  -- | Description
  Maybe Text ->
  ContributionStatus ->
  -- | Source Branch
  BranchId ->
  -- | Target Branch
  BranchId ->
  PG.Transaction e (ContributionId, ContributionNumber)
createContribution authorId projectId title description status sourceBranchId targetBranchId = do
  (contributionId, number) <-
    PG.queryExpect1Row
      [PG.sql|
        WITH contrib_number AS (
            SELECT (COALESCE(MAX(contribution_number), 0) + 1) AS new
            FROM contributions contribution
            WHERE contribution.project_id = #{projectId}
        )
        INSERT INTO contributions(
          author_id,
          project_id,
          title,
          description,
          status,
          source_branch,
          target_branch,
          contribution_number,
          best_common_ancestor_causal_id
        )
        SELECT #{authorId}, #{projectId}, #{title}, #{description}, #{status}, #{sourceBranchId}, #{targetBranchId}, contrib_number.new, best_common_causal_ancestor(source_branch.causal_id, target_branch.causal_id)
          FROM contrib_number
          JOIN project_branches AS source_branch ON source_branch.id = #{sourceBranchId}
          JOIN project_branches AS target_branch ON target_branch.id = #{targetBranchId}
        RETURNING contributions.id, contributions.contribution_number
      |]
  insertContributionStatusChangeEvent contributionId authorId Nothing status
  pure (contributionId, number)

contributionByProjectIdAndNumber ::
  ProjectId ->
  ContributionNumber ->
  PG.Transaction e (Maybe Contribution)
contributionByProjectIdAndNumber projectId contributionNumber = do
  PG.query1Row @Contribution
    [PG.sql|
        SELECT
          contribution.id,
          contribution.project_id,
          contribution.contribution_number,
          contribution.title,
          contribution.description,
          contribution.status,
          contribution.source_branch,
          contribution.target_branch,
          contribution.best_common_ancestor_causal_id,
          contribution.created_at,
          contribution.updated_at,
          contribution.author_id
        FROM contributions contribution
        WHERE contribution.project_id = #{projectId}
              AND contribution.contribution_number = #{contributionNumber}
      |]

shareContributionByProjectIdAndNumber ::
  ProjectId ->
  ContributionNumber ->
  PG.Transaction e (Maybe (ShareContribution UserId))
shareContributionByProjectIdAndNumber projectId contributionNumber = do
  PG.query1Row
    [PG.sql|
        SELECT
          contribution.id,
          contribution.contribution_number,
          project_owner.handle,
          project.slug,
          contribution.title,
          contribution.description,
          contribution.status,
          source_branch.name,
          source_branch_contributor.handle,
          target_branch.name,
          target_branch_contributor.handle,
          contribution.created_at,
          contribution.updated_at,
          contribution.author_id,
          (SELECT COUNT(*) FROM comments comment WHERE comment.contribution_id = contribution.id AND comment.deleted_at IS NULL) as num_comments
        FROM contributions AS contribution
             JOIN projects AS project ON project.id = contribution.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
             JOIN project_branches AS source_branch ON source_branch.id = contribution.source_branch
             LEFT JOIN users AS source_branch_contributor ON source_branch_contributor.id = source_branch.contributor_id
             JOIN project_branches AS target_branch ON target_branch.id = contribution.target_branch
             LEFT JOIN users AS target_branch_contributor ON target_branch_contributor.id = target_branch.contributor_id
        WHERE contribution.project_id = #{projectId}
              AND contribution_number = #{contributionNumber}
      |]

-- | Lists all contributions for a project which match the provided filters.
--   Most recently updated first.
listContributionsByProjectId ::
  ProjectId ->
  Limit ->
  Maybe (Cursor ListContributionsCursor) ->
  Maybe UserId ->
  Maybe ContributionStatus ->
  Maybe ContributionKindFilter ->
  PG.Transaction e (Maybe (Cursor ListContributionsCursor), [ShareContribution UserId])
listContributionsByProjectId projectId limit mayCursor mayUserFilter mayStatusFilter mayKindFilter = do
  let kindFilter = case mayKindFilter of
        Nothing -> "true"
        Just kind -> case kind of
          AllContributionKinds -> mempty
          OnlyCoreContributions -> [PG.sql| source_branch.contributor_id IS NULL |]
          OnlyContributorContributions -> [PG.sql| source_branch.contributor_id IS NOT NULL |]
  let cursorFilter = case mayCursor of
        Nothing -> "true"
        Just (Cursor (beforeTime, contributionId)) ->
          [PG.sql|
          (contribution.updated_at, contribution.id) < (#{beforeTime}, #{contributionId})
          |]
  addCursor
    <$> PG.queryListRows @(ShareContribution UserId)
      [PG.sql|
        SELECT
          contribution.id,
          contribution.contribution_number,
          project_owner.handle,
          project.slug,
          contribution.title,
          contribution.description,
          contribution.status,
          source_branch.name,
          source_branch_contributor.handle,
          target_branch.name,
          target_branch_contributor.handle,
          contribution.created_at,
          contribution.updated_at,
          contribution.author_id,
          (SELECT COUNT(*) FROM comments comment WHERE comment.contribution_id = contribution.id AND comment.deleted_at IS NULL) as num_comments
        FROM contributions AS contribution
             JOIN projects AS project ON project.id = contribution.project_id
             JOIN users AS project_owner ON project_owner.id = project.owner_user_id
             JOIN project_branches AS source_branch ON source_branch.id = contribution.source_branch
             LEFT JOIN users AS source_branch_contributor ON source_branch_contributor.id = source_branch.contributor_id
             JOIN project_branches AS target_branch ON target_branch.id = contribution.target_branch
             LEFT JOIN users AS target_branch_contributor ON target_branch_contributor.id = target_branch.contributor_id
        WHERE
          ^{kindFilter}
          AND ^{cursorFilter}
          AND project.id = #{projectId}
          AND (#{mayUserFilter} IS NULL OR contribution.author_id = #{mayUserFilter})
          AND (#{mayStatusFilter} IS NULL OR contribution.status = #{mayStatusFilter})
        ORDER BY contribution.updated_at DESC, contribution.id DESC
        LIMIT #{limit}
      |]
  where
    addCursor :: [ShareContribution UserId] -> (Maybe (Cursor ListContributionsCursor), [ShareContribution UserId])
    addCursor xs =
      ( lastMay xs <&> \(ShareContribution {updatedAt, contributionId}) ->
          Cursor (updatedAt, contributionId),
        xs
      )

contributionById :: ContributionId -> PG.Transaction e (Maybe Contribution)
contributionById contributionId = do
  PG.query1Row @Contribution
    [PG.sql|
        SELECT
          contribution.id,
          contribution.project_id,
          contribution.contribution_number,
          contribution.title,
          contribution.description,
          contribution.status,
          contribution.source_branch,
          contribution.target_branch,
          contribution.best_common_ancestor_causal_id,
          contribution.created_at,
          contribution.updated_at,
          contribution.author_id
        FROM contributions AS contribution
        WHERE contribution.id = #{contributionId}
      |]

updateContribution :: UserId -> ContributionId -> Maybe Text -> NullableUpdate Text -> Maybe ContributionStatus -> Maybe BranchId -> Maybe BranchId -> PG.Transaction e Bool
updateContribution callerUserId contributionId newTitle newDescription newStatus newSourceBranchId newTargetBranchId = do
  isJust <$> runMaybeT do
    Contribution {..} <- MaybeT $ contributionById contributionId
    let updatedTitle = fromMaybe title newTitle
    let updatedDescription = fromNullableUpdate description newDescription
    let updatedStatus = fromMaybe status newStatus
    let updatedSourceBranchId = fromMaybe sourceBranchId newSourceBranchId
    let updatedTargetBranchId = fromMaybe targetBranchId newTargetBranchId
    -- Add a status change event
    when (isJust newStatus && newStatus /= Just status) do
      lift $ insertContributionStatusChangeEvent contributionId callerUserId (Just status) updatedStatus
    lift $
      PG.execute_
        [PG.sql|
        UPDATE contributions
        SET
          title = #{updatedTitle},
          description = #{updatedDescription},
          status = #{updatedStatus},
          source_branch = #{updatedSourceBranchId},
          target_branch = #{updatedTargetBranchId}
        WHERE id = #{contributionId}
        |]

insertContributionStatusChangeEvent :: ContributionId -> UserId -> Maybe ContributionStatus -> ContributionStatus -> PG.Transaction e ()
insertContributionStatusChangeEvent contributionId actorUserId oldStatus newStatus = do
  PG.execute_
    [PG.sql|
        INSERT INTO contribution_status_events
          (contribution_id, actor, old_status, new_status)
          VALUES (#{contributionId}, #{actorUserId}, #{oldStatus}, #{newStatus})
      |]

contributionStatusChangeEventsByContributionId :: ContributionId -> Maybe UTCTime -> Maybe UTCTime -> PG.Transaction e [StatusChangeEvent UserId]
contributionStatusChangeEventsByContributionId contributionId mayFromExclusive untilInclusive = do
  PG.queryListRows
    [PG.sql|
        SELECT
          event.old_status,
          event.new_status,
          event.actor,
          event.created_at
        FROM contribution_status_events AS event
        WHERE contribution_id = #{contributionId}
              AND (#{mayFromExclusive} IS NULL OR event.created_at > #{mayFromExclusive})
              AND (#{untilInclusive} IS NULL OR event.created_at <= #{untilInclusive})
        ORDER BY event.created_at ASC
      |]

listContributionsByUserId ::
  Maybe UserId ->
  UserId ->
  Limit ->
  Maybe (Cursor (UTCTime, ContributionId)) ->
  Maybe ContributionStatus ->
  Maybe ContributionKindFilter ->
  PG.Transaction e (Maybe (Cursor (UTCTime, ContributionId)), [ShareContribution UserId])
listContributionsByUserId callerUserId userId limit mayCursor mayStatusFilter mayKindFilter = do
  let kindFilter = case mayKindFilter of
        Nothing -> "true"
        Just kind -> case kind of
          AllContributionKinds -> mempty
          OnlyCoreContributions -> [PG.sql| source_branch.contributor_id IS NULL |]
          OnlyContributorContributions -> [PG.sql| source_branch.contributor_id IS NOT NULL |]
  let cursorFilter = case mayCursor of
        Nothing -> "true"
        Just (Cursor (beforeTime, contributionId)) ->
          [PG.sql|
          (contribution.updated_at, contribution.id) < (#{beforeTime}, #{contributionId})
          |]
  addCursor
    <$> PG.queryListRows @(ShareContribution UserId)
      [PG.sql|
      SELECT
        contribution.id,
        contribution.contribution_number,
        project_owner.handle,
        project.slug,
        contribution.title,
        contribution.description,
        contribution.status,
        source_branch.name,
        source_branch_contributor.handle,
        target_branch.name,
        target_branch_contributor.handle,
        contribution.created_at,
        contribution.updated_at,
        contribution.author_id,
        (SELECT COUNT(*) FROM comments comment WHERE comment.contribution_id = contribution.id AND comment.deleted_at IS NULL) as num_comments
      FROM contributions AS contribution
        JOIN projects AS project ON project.id = contribution.project_id
      WHERE
        contribution.author_id = #{userId}
        AND NOT project.private
          OR EXISTS (
            SELECT FROM accessible_private_projects ap
            WHERE
              ap.user_id = #{callerUserId}
              AND ap.project_id = project.id
          )
        AND (#{mayStatusFilter} IS NULL OR contribution.status = #{mayStatusFilter})
        AND ^{cursorFilter}
        AND ^{kindFilter}
      ORDER BY contribution.updated_at DESC, contribution.id DESC
      LIMIT #{limit}
      |]
  where
    addCursor :: [ShareContribution UserId] -> (Maybe (Cursor ListContributionsCursor), [ShareContribution UserId])
    addCursor xs =
      ( lastMay xs <&> \(ShareContribution {updatedAt, contributionId}) ->
          Cursor (updatedAt, contributionId),
        xs
      )

-- | Note: Doesn't perform auth checks, the assumption is that if you already have access to
-- the branchId you have access to all associated contributions.
shareContributionsByBranchOf :: Traversal s t BranchId [ShareContribution UserId] -> s -> PG.Transaction e t
shareContributionsByBranchOf trav s =
  s
    & unsafePartsOf trav %%~ \branchIds -> do
      contributionsByBranch <-
        ( PG.queryListRows @(PG.Only BranchId PG.:. ShareContribution UserId)
            [PG.sql|
          WITH source_branches(branch_id) AS (
            SELECT * FROM ^{PG.singleColumnTable branchIds}
          )
          SELECT
            source_branches.branch_id,
            contribution.id,
            contribution.contribution_number,
            project_owner.handle,
            project.slug,
            contribution.title,
            contribution.description,
            contribution.status,
            source_branch.name,
            source_branch_contributor.handle,
            target_branch.name,
            target_branch_contributor.handle,
            contribution.created_at,
            contribution.updated_at,
            contribution.author_id,
            (SELECT COUNT(*) FROM comments comment WHERE comment.contribution_id = contribution.id AND comment.deleted_at IS NULL) as num_comments
          FROM source_branches
            JOIN project_branches AS source_branch ON source_branch.id = source_branches.branch_id
            JOIN contributions AS contribution ON contribution.source_branch = source_branch.id
            JOIN projects AS project ON project.id = contribution.project_id
            JOIN users AS project_owner ON project_owner.id = project.owner_user_id
            LEFT JOIN users AS source_branch_contributor ON source_branch_contributor.id = source_branch.contributor_id
            JOIN project_branches AS target_branch ON target_branch.id = contribution.target_branch
            LEFT JOIN users AS target_branch_contributor ON target_branch_contributor.id = target_branch.contributor_id
      |]
            <&> ( \(results :: [PG.Only BranchId PG.:. ShareContribution UserId]) ->
                    -- Group by the source branch Id.
                    results
                      & fmap
                        ( \(PG.Only branchId PG.:. shareContribution) ->
                            (branchId, [shareContribution])
                        )
                      & Map.fromListWith (<>)
                )
          )
      pure $ branchIds <&> \branchId -> Map.findWithDefault [] branchId contributionsByBranch

getPagedShareContributionTimelineByProjectIdAndNumber ::
  ProjectId ->
  ContributionNumber ->
  Maybe ContributionTimelineCursor ->
  Limit ->
  PG.Transaction SomeServerError (Maybe ContributionTimelineCursor, [ContributionTimelineEvent UserId])
getPagedShareContributionTimelineByProjectIdAndNumber projectId contributionNumber mayFromExclusive limit = do
  Contribution {contributionId} <- contributionByProjectIdAndNumber projectId contributionNumber `whenNothingM` throwSomeServerError (EntityMissing (ErrorID "contribution:missing") "Contribution not found")
  mayTillInclusiveTimestamp <- determineUpperDateBound contributionId
  statusChangeEvents <- fmap ContributionTimelineStatusChange <$> contributionStatusChangeEventsByContributionId contributionId mayFromExclusive mayTillInclusiveTimestamp
  comments <- fmap ContributionTimelineComment <$> commentsByTicketOrContribution (Left contributionId) mayFromExclusive mayTillInclusiveTimestamp
  let timeline = List.sortOn eventTimestamp (statusChangeEvents <> comments)
  pure (mayTillInclusiveTimestamp, timeline)
  where
    -- We want to page a set number of events, but are joining across many event sources.
    -- This converts a page size into a timestamp range which is easily applicable across all
    -- event sources.
    --
    -- Effectively we join all the events from all sources starting at the 'from' time, then
    -- take the first <page-size> events from that set, which allows us to determine the
    -- 'till' time to use when ACTUALLY fetching entities from each individual table.
    --
    -- We can't just grab 'at most N' elements from each table because it may result in some
    -- events appearing to be missing from the timeline.
    determineUpperDateBound :: ContributionId -> PG.Transaction e (Maybe UTCTime)
    determineUpperDateBound contributionId = do
      PG.query1Col @(UTCTime)
        [PG.sql|
            WITH events(timestamp, contribution_id) AS (
              SELECT status_event.created_at, status_event.contribution_id FROM contribution_status_events status_event
              UNION ALL
              SELECT comment.created_at, comment.contribution_id FROM comments comment
            ), events_in_window (timestamp, contribution_id) AS (
              SELECT timestamp, contribution_id FROM events
                WHERE events.contribution_id = #{contributionId}
                  AND (#{mayFromExclusive} IS NULL OR timestamp > #{mayFromExclusive})
                ORDER BY timestamp ASC
                LIMIT #{limit}
            )
            SELECT MAX(events_in_window.timestamp) FROM events_in_window
        |]

data NewBCAs = NewBCAs
  { sourceBranchCausal :: CausalId,
    targetBranchCausal :: CausalId,
    bca :: CausalId
  }
  deriving (Show)

-- | Recompute the best common ancestors for all contributions related to the branch, then
-- return the set of contribution IDs which have been marked as merged.
performMergesAndBCAUpdatesFromBranchPush :: UserId -> BranchId -> PG.Transaction e (Set ContributionId)
performMergesAndBCAUpdatesFromBranchPush callerUserId branchId = do
  -- Get the new BCAs for all contributions related to the branch
  contributionsToMarkAsMerged <-
    PG.queryListCol @(ContributionId)
      [PG.sql|
    WITH new_bcas(contribution_id, source_causal_id, target_causal_id, bca_id) AS (
      SELECT contr.id, source_branch.causal_id, target_branch.causal_id, best_common_causal_ancestor(source_branch.causal_id, target_branch.causal_id) FROM contributions contr
        JOIN project_branches AS source_branch ON source_branch.id = contr.source_branch
        JOIN project_branches AS target_branch ON target_branch.id = contr.target_branch
        WHERE contr.source_branch = #{branchId} OR contr.target_branch = #{branchId}
          AND status IN (#{Draft}, #{InReview})
    ), contributions_to_mark_as_merged(contribution_id) AS (
      SELECT contribution_id FROM new_bcas
        WHERE new_bcas.bca_id IS NOT NULL
          AND new_bcas.bca_id = new_bcas.source_causal_id
    ), non_merged_bca_updates AS MATERIALIZED (
      UPDATE contributions contr
        SET best_common_ancestor_causal_id = new_bcas.bca_id
        FROM new_bcas
        WHERE
          contr.id = new_bcas.contribution_id
          AND contr.id NOT IN (SELECT contribution_id FROM contributions_to_mark_as_merged)
    ) SELECT contribution_id FROM contributions_to_mark_as_merged
      |]
  for_ contributionsToMarkAsMerged \contributionId -> do
    _success <- updateContribution callerUserId contributionId Nothing Unchanged (Just Merged) Nothing Nothing
    pure ()
  pure $ Set.fromList contributionsToMarkAsMerged

rebaseContributionsFromMergedBranches :: Set ContributionId -> PG.Transaction e (Set ContributionId)
rebaseContributionsFromMergedBranches mergedContributions = do
  PG.queryListCol @ContributionId
    [PG.sql|
      WITH merged_contribution_ids(contribution_id) AS (
        SELECT * FROM ^{PG.singleColumnTable $ Set.toList mergedContributions}
      )
      UPDATE contributions contr
        SET target_branch = merged_contribution.target_branch
        -- Update all open contributions which are merging into a branch that was just merged
        FROM merged_contribution_ids
          JOIN contributions merged_contribution ON merged_contribution.id = merged_contribution_ids.contribution_id
        WHERE merged_contribution.source_branch = contr.target_branch
          AND contr.status IN (#{Draft}, #{InReview})
        RETURNING contr.id
    |]
    <&> Set.fromList

contributionStateTokenById :: ContributionId -> PG.Transaction e ContributionStateToken
contributionStateTokenById contributionId = do
  PG.queryExpect1Row @ContributionStateToken
    [PG.sql|
        SELECT
          contribution.id,
          contribution.source_branch,
          contribution.target_branch,
          source_causal.hash,
          target_causal.hash,
          contribution.status
        FROM contributions contribution
          JOIN project_branches AS source_branch ON source_branch.id = contribution.source_branch
          JOIN project_branches AS target_branch ON target_branch.id = contribution.target_branch
          JOIN causals source_causal ON source_causal.id = source_branch.causal_id
          JOIN causals target_causal ON target_causal.id = target_branch.causal_id
        WHERE contribution.id = #{contributionId}
      |]

getPrecomputedNamespaceDiff ::
  (CodebaseEnv, BranchHashId) ->
  (CodebaseEnv, BranchHashId) ->
  PG.Transaction e (Maybe Text)
getPrecomputedNamespaceDiff
  (CodebaseEnv {codebaseOwner = leftCodebaseUser}, leftBHId)
  (CodebaseEnv {codebaseOwner = rightCodebaseUser}, rightBHId) = do
    PG.query1Col @Text
      [PG.sql|
          SELECT (diff :: text)
          FROM namespace_diffs nd
          WHERE nd.left_namespace_id = #{leftBHId}
            AND nd.right_namespace_id = #{rightBHId}
            AND nd.left_codebase_owner_user_id = #{leftCodebaseUser}
            AND nd.right_codebase_owner_user_id = #{rightCodebaseUser}
        |]

savePrecomputedNamespaceDiff ::
  (CodebaseEnv, BranchHashId) ->
  (CodebaseEnv, BranchHashId) ->
  Text ->
  PG.Transaction e ()
savePrecomputedNamespaceDiff (CodebaseEnv {codebaseOwner = leftCodebaseUser}, leftBHId) (CodebaseEnv {codebaseOwner = rightCodebaseUser}, rightBHId) diff = do
  PG.execute_
    [PG.sql|
        INSERT INTO namespace_diffs (left_namespace_id, right_namespace_id, left_codebase_owner_user_id, right_codebase_owner_user_id, diff)
        VALUES (#{leftBHId}, #{rightBHId}, #{leftCodebaseUser}, #{rightCodebaseUser}, #{diff}::jsonb)
        ON CONFLICT DO NOTHING
      |]
