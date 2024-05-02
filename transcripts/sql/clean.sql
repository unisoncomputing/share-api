-- Resets most relevant tables, useful to run between tests.
-- Doesn't clean codebase tables since that just slows things down, but does clean out codebase ownership.
SET client_min_messages TO WARNING;
TRUNCATE TABLE users CASCADE;
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

TRUNCATE TABLE namespace_ownership CASCADE;
TRUNCATE TABLE causal_ownership CASCADE;
-- TRUNCATE TABLE branch_hashes CASCADE;
-- TRUNCATE TABLE namespaces CASCADE;
-- TRUNCATE TABLE causals CASCADE;

TRUNCATE TABLE public.cloud_subscribers CASCADE;
