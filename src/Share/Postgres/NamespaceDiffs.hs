{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeOperators #-}

module Share.Postgres.NamespaceDiffs
  ( getRelevantTermsForDiff,
    getRelevantTypesForDiff,
  )
where

import Data.Either qualified as Either
import Share.Postgres qualified as PG
import Share.Postgres.IDs (BranchHashId)
import Share.Postgres.NameLookups.Types (NameLookupReceipt, NamedRef (..), ReversedName)
import Share.Postgres.Refs.Types (PGReference, PGReferent)
import Share.Prelude
import Unison.Name (Name)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Rel

-- | Get's the terms relevant for computing the diff between two branches.
-- Where 'relevant' is defined as:
--
-- 1. Terms that are in the old namespace but not the new namespace
-- 2. Terms that are in the new namespace but not the old namespace
-- 3. Names that are in both namespaces, but have different refs
-- 4. Refs that are in both namespaces, but have different names
getRelevantTermsForDiff ::
  (PG.QueryA m) =>
  NameLookupReceipt ->
  BranchHashId ->
  BranchHashId ->
  m
    ( Relation Name PGReferent {- relevant terms in old namespace -},
      Relation Name PGReferent {- relevant terms only in new namespace -}
    )
getRelevantTermsForDiff !_nameLookupReceipt oldBranchHashId newBranchHashId = do
  -- This SQL query does the following:
  --
  -- 1. Find (name, ref) pairs that are in the old namespace but not the new namespace
  -- 2. Find (name, ref) pairs that are in the new namespace but not the old namespace
  -- 3. Find (name, ref) pairs that are in both namespaces, but have different refs
  -- 4. Find (name, ref) pairs that are in both namespaces, but have different names
  -- 5. Return the results as a list of (ref, name, isNew) tuples. It's possible for the same
  --    (name, ref) pair to appear with both (isNew = true) and (isNew = false) in the result.
  PG.queryListRows @(NamedRef PGReferent PG.:. PG.Only Bool)
    [PG.sql|
            WITH only_in_old AS (
              ( SELECT old.reversed_name, old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index
                FROM scoped_term_name_lookup old
                WHERE old.root_branch_hash_id = #{oldBranchHashId}
                  AND NOT EXISTS (
                    SELECT FROM scoped_term_name_lookup new
                      WHERE new.root_branch_hash_id = #{newBranchHashId}
                        AND new.reversed_name = old.reversed_name
                        AND (old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index)
                          IS NOT DISTINCT FROM (new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index)
                  )
              )
            ), only_in_new AS (
              ( SELECT new.reversed_name, new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index
                FROM scoped_term_name_lookup new
                WHERE new.root_branch_hash_id = #{newBranchHashId}
                  AND NOT EXISTS (
                    SELECT FROM scoped_term_name_lookup old
                      WHERE old.root_branch_hash_id = #{oldBranchHashId}
                        AND old.reversed_name = new.reversed_name
                        AND (old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index)
                          IS NOT DISTINCT FROM (new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index)
                  )
              )
            ), relevant_terms_in_old AS (
              SELECT old.reversed_name, old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index
              FROM scoped_term_name_lookup old
              WHERE old.root_branch_hash_id = #{oldBranchHashId}
                AND
                  (  EXISTS (SELECT FROM only_in_old
                              WHERE only_in_old.reversed_name = old.reversed_name
                                 OR (old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index)
                                      IS NOT DISTINCT FROM (only_in_old.referent_builtin, only_in_old.referent_component_hash_id, only_in_old.referent_component_index, only_in_old.referent_constructor_index)
                            )
                  OR EXISTS (SELECT FROM only_in_new
                               WHERE only_in_new.reversed_name = old.reversed_name
                                 OR (old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index)
                                      IS NOT DISTINCT FROM (only_in_new.referent_builtin, only_in_new.referent_component_hash_id, only_in_new.referent_component_index, only_in_new.referent_constructor_index)
                            )
                  )
            ), relevant_terms_in_new AS (
              SELECT new.reversed_name, new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index
              FROM scoped_term_name_lookup new
              WHERE new.root_branch_hash_id = #{newBranchHashId}
                AND
                  (  EXISTS (SELECT FROM only_in_old
                              WHERE only_in_old.reversed_name = new.reversed_name
                                 OR (new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index)
                                      IS NOT DISTINCT FROM (only_in_old.referent_builtin, only_in_old.referent_component_hash_id, only_in_old.referent_component_index, only_in_old.referent_constructor_index)
                            )
                  OR EXISTS (SELECT FROM only_in_new
                               WHERE only_in_new.reversed_name = new.reversed_name
                                 OR (new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index)
                                      IS NOT DISTINCT FROM (only_in_new.referent_builtin, only_in_new.referent_component_hash_id, only_in_new.referent_component_index, only_in_new.referent_constructor_index)
                            )
                  )
            ) SELECT old.reversed_name, old.referent_builtin, old.referent_component_hash_id, old.referent_component_index, old.referent_constructor_index, false
                FROM relevant_terms_in_old old
              UNION
              SELECT new.reversed_name, new.referent_builtin, new.referent_component_hash_id, new.referent_component_index, new.referent_constructor_index, true
                FROM relevant_terms_in_new new
          |]
    <&> ( fmap \(NamedRef {reversedSegments, ref} PG.:. PG.Only inNew) ->
            if inNew
              then Right (from @ReversedName @Name reversedSegments, ref)
              else
                Left (from @ReversedName @Name reversedSegments, ref)
        )
    <&> \rows ->
      let (old, new) = Either.partitionEithers rows
       in (Rel.fromList old, Rel.fromList new)

-- | Gets the types relevant for computing the diff between two branches.
-- Where 'relevant' is defined as:
--
-- 1. Types that are in the old namespace but not the new namespace
-- 2. Types that are in the new namespace but not the old namespace
-- 3. Names that are in both namespaces, but have different refs
-- 4. Refs that are in both namespaces, but have different names
getRelevantTypesForDiff :: (PG.QueryA m) => NameLookupReceipt -> BranchHashId -> BranchHashId -> m (Relation Name PGReference, Relation Name PGReference)
getRelevantTypesForDiff !_nameLookupReceipt oldBranchHashId newBranchHashId = do
  -- This SQL query does the following:
  --
  -- 1. Find (name, ref) pairs that are in the old namespace but not the new namespace
  -- 2. Find (name, ref) pairs that are in the new namespace but not the old namespace
  -- 3. Find (name, ref) pairs that are in both namespaces, but have different refs
  -- 4. Find (name, ref) pairs that are in both namespaces, but have different names
  -- 5. Return the results as a list of (ref, name, isNew) tuples. It's possible for the same
  --    (name, ref) pair to appear with both (isNew = true) and (isNew = false) in the result.
  PG.queryListRows @(NamedRef PGReference PG.:. PG.Only Bool)
    [PG.sql|
            WITH only_in_old AS (
              ( SELECT old.reversed_name, old.reference_builtin, old.reference_component_hash_id, old.reference_component_index
                FROM scoped_type_name_lookup old
                WHERE old.root_branch_hash_id = #{oldBranchHashId}
              )
            EXCEPT
              ( SELECT new.reversed_name, new.reference_builtin, new.reference_component_hash_id, new.reference_component_index
                FROM scoped_type_name_lookup new
                WHERE new.root_branch_hash_id = #{newBranchHashId}
              )
            ), only_in_new AS (
              ( SELECT new.reversed_name, new.reference_builtin, new.reference_component_hash_id, new.reference_component_index
                FROM scoped_type_name_lookup new
                WHERE new.root_branch_hash_id = #{newBranchHashId}
              )
            EXCEPT
              ( SELECT old.reversed_name, old.reference_builtin, old.reference_component_hash_id, old.reference_component_index
                FROM scoped_type_name_lookup old
                WHERE old.root_branch_hash_id = #{oldBranchHashId}
              )
            ), relevant_types_in_old AS (
              SELECT old.reversed_name, old.reference_builtin, old.reference_component_hash_id, old.reference_component_index
              FROM scoped_type_name_lookup old
              WHERE old.root_branch_hash_id = #{oldBranchHashId}
                AND
                  (  EXISTS (SELECT FROM only_in_old
                              WHERE only_in_old.reversed_name = old.reversed_name
                                 OR (old.reference_builtin, old.reference_component_hash_id, old.reference_component_index)
                                      IS NOT DISTINCT FROM (only_in_old.reference_builtin, only_in_old.reference_component_hash_id, only_in_old.reference_component_index)
                            )
                  OR EXISTS (SELECT FROM only_in_new
                               WHERE only_in_new.reversed_name = old.reversed_name
                                 OR (old.reference_builtin , old.reference_component_hash_id, old.reference_component_index)
                                      IS NOT DISTINCT FROM (only_in_new.reference_builtin, only_in_new.reference_component_hash_id, only_in_new.reference_component_index)
                            )
                  )
            ), relevant_types_in_new AS (
              SELECT new.reversed_name, new.reference_builtin, new.reference_component_hash_id, new.reference_component_index
              FROM scoped_type_name_lookup new
              WHERE new.root_branch_hash_id = #{newBranchHashId}
                AND
                  (  EXISTS (SELECT FROM only_in_old
                              WHERE only_in_old.reversed_name = new.reversed_name
                                 OR (new.reference_builtin, new.reference_component_hash_id, new.reference_component_index)
                                      IS NOT DISTINCT FROM (only_in_old.reference_builtin, only_in_old.reference_component_hash_id, only_in_old.reference_component_index)
                            )
                  OR EXISTS (SELECT FROM only_in_new
                               WHERE only_in_new.reversed_name = new.reversed_name
                                 OR (new.reference_builtin, new.reference_component_hash_id, new.reference_component_index)
                                      IS NOT DISTINCT FROM (only_in_new.reference_builtin, only_in_new.reference_component_hash_id, only_in_new.reference_component_index)
                            )
                  )
            ) SELECT old.reversed_name, old.reference_builtin, old.reference_component_hash_id, old.reference_component_index, false
                FROM relevant_types_in_old old
              UNION
              SELECT new.reversed_name, new.reference_builtin, new.reference_component_hash_id, new.reference_component_index, true
                FROM relevant_types_in_new new
          |]
    <&> ( fmap \(NamedRef {reversedSegments, ref} PG.:. PG.Only inNew) ->
            if inNew
              then Right (from @ReversedName @Name reversedSegments, ref)
              else Left (from @ReversedName @Name reversedSegments, ref)
        )
    <&> \rows ->
      let (old, new) = Either.partitionEithers rows
       in (Rel.fromList old, Rel.fromList new)
