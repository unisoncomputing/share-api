\set ON_ERROR_STOP true

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.tables 
    WHERE table_name = 'is_transcript_database'
  ) THEN
    RAISE EXCEPTION 'Refusing to clean non-transcript database.';
  END IF;
END $$;

-- Resets most relevant tables, useful to run between tests.
-- Doesn't clean codebase tables since that just slows things down, but does clean out codebase ownership.
SET client_min_messages TO WARNING;
TRUNCATE TABLE users CASCADE;
TRUNCATE TABLE superadmins CASCADE;
TRUNCATE TABLE role_memberships CASCADE;
TRUNCATE TABLE loose_code_roots CASCADE;
TRUNCATE TABLE org_members CASCADE;
TRUNCATE TABLE tours CASCADE;
TRUNCATE TABLE projects CASCADE;
TRUNCATE TABLE project_branches CASCADE;
TRUNCATE TABLE project_releases CASCADE;
TRUNCATE TABLE project_favorites CASCADE;
TRUNCATE TABLE catalog_categories CASCADE;
TRUNCATE TABLE project_categories CASCADE;
TRUNCATE TABLE oauth_clients CASCADE;
TRUNCATE TABLE oauth_clients CASCADE;
TRUNCATE TABLE contributions CASCADE;
TRUNCATE TABLE contribution_status_events CASCADE;
TRUNCATE TABLE comments CASCADE;
TRUNCATE TABLE comment_revisions CASCADE;
TRUNCATE TABLE contributions CASCADE;
TRUNCATE TABLE contribution_status_events CASCADE;
TRUNCATE TABLE tickets CASCADE;
TRUNCATE TABLE comments CASCADE;
TRUNCATE TABLE comment_revisions CASCADE;
TRUNCATE TABLE teams CASCADE;
TRUNCATE TABLE team_members CASCADE;
TRUNCATE TABLE orgs CASCADE;
TRUNCATE TABLE subjects CASCADE;
TRUNCATE TABLE resources CASCADE;
TRUNCATE TABLE causal_diff_queue CASCADE;
TRUNCATE TABLE namespace_diffs CASCADE;
TRUNCATE TABLE personal_keys CASCADE;
TRUNCATE TABLE history_comments CASCADE;
TRUNCATE TABLE history_comment_revisions CASCADE;
TRUNCATE TABLE history_comment_revisions_project_discovery CASCADE;

TRUNCATE TABLE namespace_ownership CASCADE;
TRUNCATE TABLE causal_ownership CASCADE;
-- Transcripts should generally behave the same with or without truncating these, and leaving them in place saves
-- time when rerunning tests.
-- TRUNCATE TABLE branch_hashes CASCADE;
-- TRUNCATE TABLE namespaces CASCADE;
-- TRUNCATE TABLE causals CASCADE;

TRUNCATE TABLE public.cloud_subscribers CASCADE;
