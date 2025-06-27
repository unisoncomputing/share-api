SET client_min_messages TO WARNING;
-- Useful for local performance testing.
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

-- This file includes various fixtures that are useful when testing and running
-- a local setup of our applications
-- Users
INSERT INTO users (
  id,
  primary_email,
  email_verified,
  avatar_url,
  name,
  handle
  )
VALUES (
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'test@example.com',
  TRUE,
  NULL, -- Test a user with a null avater.
  NULL, -- Test a user with a null name.
  'test'
  ),
(
  'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9',
  'unison@example.com',
  TRUE,
  'https://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50?f=y&d=retro',
  'Unison Org',
  'unison'
  ),
(
  '43efd5e7-139a-40b2-8a35-3f99b054dc84',
  'transcripts@example.com',
  TRUE,
  'https://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50?f=y&d=retro',
  'The Transcript User',
  'transcripts'
  ),
(
  '3dd1a929-28dd-4585-88aa-96b4dae8606d',
  'unauthorized@example.com',
  TRUE,
  'https://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50?f=y&d=retro',
  'Unauthorized User',
  'unauthorized'
  ),
(
  'fe8921ca-aee7-40a2-8020-241ca78f2a5c',
  'admin@example.com',
  TRUE,
  'https://www.gravatar.com/avatar/205e460b479e2e5b48aec07710c08d50?f=y&d=retro',
  'Admin User',
  'admin'
  );

INSERT INTO orgs (
  id,
  user_id) 
VALUES (
  '7ab35ad5-6755-4dd1-9753-bc3ba6b88039',
  'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9'
);

INSERT INTO org_members (
  org_id,
  organization_user_id,
  member_user_id)
VALUES (
  '7ab35ad5-6755-4dd1-9753-bc3ba6b88039',
  'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9',
  'd32f4ddf-2423-4f10-a4de-465939951354'),
(
  '7ab35ad5-6755-4dd1-9753-bc3ba6b88039',
  'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9',
  '43efd5e7-139a-40b2-8a35-3f99b054dc84'),
(
  '7ab35ad5-6755-4dd1-9753-bc3ba6b88039',
  'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9',
  'fe8921ca-aee7-40a2-8020-241ca78f2a5c');

-- Make 'test' the owner of the unison org
INSERT INTO role_memberships(subject_id, resource_id, role_id)
  SELECT (SELECT u.subject_id FROM users u WHERE u.handle = 'test'),
         (SELECT org.resource_id FROM orgs org JOIN users orgu ON org.user_id = orgu.id WHERE orgu.handle = 'unison'), 
         (SELECT r.id FROM roles r WHERE r.ref = 'org_owner');

INSERT INTO tours (
  user_id,
  tour_id)
VALUES (
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'welcome-terms');

-- User Profiles
UPDATE
  users
SET
  bio = 'A test user',
  website = 'https://unison-lang.org',
  location = 'The testverse',
  pronouns = 'they/them',
  twitterHandle = '@unisonTestUser'
WHERE
  id = 'd32f4ddf-2423-4f10-a4de-465939951354';

UPDATE
  users
SET
  bio = 'A transcript!',
  website = 'https://unison-lang.org',
  location = 'Unison Share',
  pronouns = 'they/them',
  twitterHandle = '@unisonTranscript'
WHERE
  id = '43efd5e7-139a-40b2-8a35-3f99b054dc84';

-- Initialize the empty namespace and hash
INSERT INTO branch_hashes(id, base32)
  VALUES (0, 'n6ulbn64d1i6o7d2pnit3i1gtfbnmhgkci1cljvi5qfos0e265mpo7ml8t26fdcooveci943n1ba972aa6fqlb4thcei6qihim88qlo')
  ON CONFLICT DO NOTHING;

-- Initialize the empty namespace
INSERT INTO namespaces(namespace_hash_id, contained_terms, deep_contained_terms, contained_types, deep_contained_types, contained_constructors, deep_contained_constructors)
  VALUES (0, 0, 0, 0, 0, 0, 0)
  ON CONFLICT DO NOTHING;

INSERT INTO namespace_depth(namespace_hash_id, depth)
  VALUES (0, 0)
  ON CONFLICT DO NOTHING;

-- Initialize the empty causal
INSERT INTO causals(id, hash, namespace_hash_id)
  VALUES (0, 'sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg', 0)
  ON CONFLICT DO NOTHING;

INSERT INTO causal_depth(causal_id, depth)
  VALUES (0, 0)
  ON CONFLICT DO NOTHING;

-- Projects
INSERT INTO projects (
  id,
  owner_user_id,
  slug,
  summary,
  private)
VALUES
  -- Public project owned by test user
  (
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    'd32f4ddf-2423-4f10-a4de-465939951354',
    'publictestproject',
    'test project summary',
    FALSE),
  -- Private project owned by test user
  (
    '91fdca65-0eed-4f44-bdfa-06050fd69dba', 'd32f4ddf-2423-4f10-a4de-465939951354', 'privatetestproject', 'private summary', TRUE),
  -- Private project owned by unison org
  (
    '13e2392d-c094-4383-bbd0-dc203705c9a6', 'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9', 'privateorgproject', 'Private Unison Project', TRUE);

INSERT INTO project_branches (
  id,
  project_id,
  name,
  creator_id,
  contributor_id,
  causal_id,
  merge_target_branch_id,
  updated_at)
-- @test/publictestproject/main
VALUES (
  'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d',
  'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
  'main',
  'd32f4ddf-2423-4f10-a4de-465939951354',
  NULL,
  0,
  NULL,
  '2023-03-01T00:00:00.0Z'),
-- @test/publictestproject/@transcripts/contribution
(
  'bb343b05-2da8-47a4-b008-d45b8b367116', 'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd', 'contribution', '43efd5e7-139a-40b2-8a35-3f99b054dc84', '43efd5e7-139a-40b2-8a35-3f99b054dc84', 0, 'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d', '2023-03-02T00:00:00.0Z'),
-- @test/publictestproject/feature
(
  '0e5421d1-3443-4101-a39f-4cb20c59368f', 'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd', 'feature', 'd32f4ddf-2423-4f10-a4de-465939951354', NULL, 0, 'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d', '2023-03-02T00:00:00.0Z'),
-- @test/privatetestproject/privatebranch
(
  '4b0201f8-82a7-4710-a892-a2829f2f044b', '91fdca65-0eed-4f44-bdfa-06050fd69dba', 'privatebranch', 'd32f4ddf-2423-4f10-a4de-465939951354', NULL, 0, NULL, '2023-03-02T00:00:00.0Z'),
-- @test/privatetestproject/main
(
  'f0d9aee9-0e4c-4f13-88d9-9b44c8c0601f', '91fdca65-0eed-4f44-bdfa-06050fd69dba', 'main', 'd32f4ddf-2423-4f10-a4de-465939951354', NULL, 0, NULL, '2023-03-02T00:00:00.0Z'),
-- @unison/privateorgproject/main
(
  '8d599975-8738-4f38-9410-3a73efb4a14a', '13e2392d-c094-4383-bbd0-dc203705c9a6', 'main', 'e5e7635c-8db2-4b7f-9fee-86ee8d120ef9', NULL, 0, NULL, '2023-03-02T00:00:00.0Z'),
-- @unison/privateorgproject/@transcripts/privatecontribution
(
  '9c6cab2e-56e8-4d49-a200-95163ee870b5', '13e2392d-c094-4383-bbd0-dc203705c9a6', 'privatecontribution', '43efd5e7-139a-40b2-8a35-3f99b054dc84', '43efd5e7-139a-40b2-8a35-3f99b054dc84', 0, NULL, '2023-03-02T00:00:00.0Z');

INSERT INTO project_releases (
  id,
  project_id,
  major_version,
  minor_version,
  patch_version,
  created_by,
  unsquashed_causal_id,
  squashed_causal_id)
VALUES
  -- A couple published releases on the test project
  -- @test/publictestproject/releases/1.2.3
  (
    '551aff6a-c1ea-456d-9256-25ffeea414f7',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    1,
    2,
    3,
    'd32f4ddf-2423-4f10-a4de-465939951354',
    0,
    0),
  -- @test/publictestproject/releases/1.0.0
  (
    'c1480162-660c-4529-a39b-104e942f6ca1',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    1,
    0,
    0,
    'd32f4ddf-2423-4f10-a4de-465939951354',
    0,
    0);

INSERT INTO project_favorites (
  project_id,
  user_id)
VALUES (
  'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
  'd32f4ddf-2423-4f10-a4de-465939951354');

INSERT INTO catalog_categories (
  id,
  name)
VALUES (
  '9dbf16aa-9aeb-444a-bd66-1dbddfbd36db',
  'network');

INSERT INTO project_categories (
  project_id,
  category_id)
VALUES (
  'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
  '9dbf16aa-9aeb-444a-bd66-1dbddfbd36db');

INSERT INTO oauth_clients (
    client_id,
    client_secret,
    redirect_host,
    approved_scopes,
    audience)
VALUES (
    'ucm',
    NULL,
    'http://localhost',
    'openid cloud sync',
    'http://localhost:5424');


INSERT INTO oauth_clients (
    client_id,
    client_secret,
    redirect_host,
    approved_scopes,
    audience)
VALUES (
    'cloud',
    'cloud-secret',
    'http://cloud',
    'openid cloud sync',
    'http://cloud:3030');

INSERT INTO contributions (
    id,
    project_id,
    contribution_number,
    title,
    description,
    status,
    source_branch,
    target_branch,
    source_causal_id,
    target_causal_id,
    author_id)
VALUES (
    '511fbf1c-5353-4e01-ba23-b94e6092ede5',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    1,
    'Fix issue with user authentication',
    'This contribution addresses an issue where users were unable to log in due to a validation error in the authentication process.

## Changes made:

* Modified the validation logic for the Auth type to properly authenticate users.
* Added unit tests to ensure the authentication process works as expected.

## Testing:

I tested this change locally on my development environment and confirmed that users can now log in without any issues. All unit tests are passing.',

    'in_review',
    'bb343b05-2da8-47a4-b008-d45b8b367116',
    'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d',
    (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = 'bb343b05-2da8-47a4-b008-d45b8b367116' LIMIT 1),
    (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = 'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d' LIMIT 1),
    'd32f4ddf-2423-4f10-a4de-465939951354');

INSERT INTO contribution_status_events (
    contribution_id,
    actor,
    new_status,
    old_status)
VALUES (
  '511fbf1c-5353-4e01-ba23-b94e6092ede5',
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'in_review',
  NULL
);

INSERT INTO comments (id, contribution_id, author_id)
VALUES (
  'b9dce6b4-f5b2-4b28-a485-988c0c7a56be',
  '511fbf1c-5353-4e01-ba23-b94e6092ede5',
  'd32f4ddf-2423-4f10-a4de-465939951354'
), (
  '82c21bb5-6331-497f-b2e6-15414ebc63c4',
  '511fbf1c-5353-4e01-ba23-b94e6092ede5',
  'd32f4ddf-2423-4f10-a4de-465939951354'
);

INSERT INTO comment_revisions(comment_id, revision_number, author_id, content)
VALUES (
  'b9dce6b4-f5b2-4b28-a485-988c0c7a56be',
  0,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is a comment'
), (
  'b9dce6b4-f5b2-4b28-a485-988c0c7a56be',
  1,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is a revised comment'
), (
  '82c21bb5-6331-497f-b2e6-15414ebc63c4',
  0,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is another comment'
);

INSERT INTO contributions (
    id,
    project_id,
    contribution_number,
    title,
    description,
    status,
    source_branch,
    target_branch,
    source_causal_id,
    target_causal_id,
    author_id)
VALUES (
    '3560fa5d-d29d-49b8-bf4f-1045fef31c81',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    2,
    'Add pagination to user list',
    'Adds pagination functionality to the user list page in the admin panel to improve performance and user experience.
## Changes Made:

* Implemented pagination using the Paginator type.
* Updated the user list view to display a paginated list.
* Added a new route for paginated user list.

## Testing:

I tested the pagination feature with a large dataset to ensure it functions correctly. I also tested edge cases and confirmed that the user list is now paginated as expected.
',
    'in_review',
    'bb343b05-2da8-47a4-b008-d45b8b367116',
    'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d',
    (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = 'bb343b05-2da8-47a4-b008-d45b8b367116' LIMIT 1),
    (SELECT pb.causal_id FROM project_branches pb WHERE pb.id = 'a1b2c3d4-5e6f-7a8b-9c0d-1e2f3a4b5c6d' LIMIT 1),
    'd32f4ddf-2423-4f10-a4de-465939951354');

INSERT INTO contribution_status_events (
    contribution_id,
    actor,
    new_status,
    old_status)
VALUES (
  '3560fa5d-d29d-49b8-bf4f-1045fef31c81',
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'in_review',
  NULL
);

INSERT INTO namespace_ownership(namespace_hash_id, user_id)
  SELECT 0, id FROM users;

INSERT INTO causal_ownership(causal_id, user_id)
  SELECT 0, id FROM users;

-- Set all users loose code to the empty causal to start.
INSERT INTO loose_code_roots (user_id, causal_id)
  SELECT users.id, 0
    FROM users;

INSERT INTO tickets (
    id,
    project_id,
    ticket_number,
    title,
    description,
    status,
    author_id)
VALUES (
    'a0530678-db50-4247-a6f0-4399715197aa',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    1,
    'Bug Report',
    'I want the code to solve all my problems but it does not. Please fix.

## Things I need:

* It should tie my *shoes*
* It should make me _coffee_
* It should do my taxes
',
    'open',
    'd32f4ddf-2423-4f10-a4de-465939951354'
), (
    'bb99db0a-ca27-4260-b066-c6bafe0c3fd9',
    'cdad39a9-9ed2-4a5e-b2d7-62bbe81446dd',
    2,
    'Completed Request',
    'This ticket is closed',
    'closed',
    'd32f4ddf-2423-4f10-a4de-465939951354'
);

INSERT INTO comments (id, ticket_id, author_id)
VALUES (
  '25118f35-87f6-4429-bb43-69b0e5bb717a',
  'a0530678-db50-4247-a6f0-4399715197aa',
  'd32f4ddf-2423-4f10-a4de-465939951354'
), (
  'd9164a6f-706b-402d-a2b0-b55a7c19a4f5',
  'a0530678-db50-4247-a6f0-4399715197aa',
  'd32f4ddf-2423-4f10-a4de-465939951354'
);

INSERT INTO comment_revisions(comment_id, revision_number, author_id, content)
VALUES (
  '25118f35-87f6-4429-bb43-69b0e5bb717a',
  0,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is a ticket comment'
), (
  '25118f35-87f6-4429-bb43-69b0e5bb717a',
  1,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is a revised ticket comment'
),
(
  'd9164a6f-706b-402d-a2b0-b55a7c19a4f5',
  0,
  'd32f4ddf-2423-4f10-a4de-465939951354',
  'This is another ticket comment'
);

--- known hash mismatches
COPY known_component_hash_mismatches(provided_component_hash,actual_component_hash)
FROM '/docker-entrypoint-initdb.d/hash_mismatches/term_component_hash_mismatches.csv'
DELIMITER ','
CSV HEADER;

INSERT INTO public.cloud_subscribers(user_id, is_active, tier_name) VALUES
  ('d32f4ddf-2423-4f10-a4de-465939951354', true, 'Starter');

INSERT INTO superadmins(user_id) VALUES
  ('fe8921ca-aee7-40a2-8020-241ca78f2a5c');
