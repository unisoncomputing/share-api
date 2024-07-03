-- Create users for each app
CREATE USER cloud WITH PASSWORD :'cloud_password';
CREATE USER share WITH PASSWORD :'share_password';

-- On AWS the postgres user isn't a superuser, so we need to manually grant it permissions to these users.
-- This also allows us to gracefully migrate to the new owners.
GRANT cloud TO postgres;
GRANT share TO postgres;

-- Create schemas for each user
CREATE SCHEMA cloud AUTHORIZATION cloud;
CREATE SCHEMA share AUTHORIZATION share;

-- Set the search path for each user so they default to making tables in their own schema.
-- They also get access to the public schema, since that's where all the extensions
-- and shared tables/views live.
ALTER USER cloud SET search_path TO "$user", public;
ALTER USER share SET search_path TO "$user", public;

-- Update the readonly and admin user to see all the schemas.
ALTER USER scripts_readonly SET search_path TO "$user", public, share, cloud;
ALTER USER readonly SET search_path TO "$user", public, share, cloud;
ALTER USER postgres SET search_path TO "$user", public, share, cloud;

-- App users can create and read stuff in the public schema.
GRANT ALL ON SCHEMA public TO cloud;
GRANT ALL ON SCHEMA public TO share;

-- Set the default privileges for each user so any public tables are actually 
-- made public.
ALTER DEFAULT PRIVILEGES 
    FOR USER cloud
    IN SCHEMA public
    GRANT SELECT ON TABLES TO PUBLIC;
ALTER DEFAULT PRIVILEGES 
    FOR USER share
    IN SCHEMA public
    GRANT SELECT ON TABLES TO PUBLIC;

-- 🚨 Restart the apps here to ensure their postgres user sessions have the new search path which includes the new
-- schemas.

-- Now we can move all the tables into the correct schemas for their apps.
-- Generate all the required ALTER TABLE statements to move each table into the correct schema.
SELECT format(
  'ALTER TABLE %I.%I.%I SET SCHEMA %I;',
  table_catalog,
  table_schema,
  table_name,
  'cloud'
)
FROM information_schema.tables
WHERE table_schema = 'public' 
AND (table_name LIKE 'cloud_%'
      AND table_name NOT IN ('cloud_subscribers', 'cloud_users')
    ) OR table_name IN ('stripe_customers', 'stripe_events', 'environments', 'service_ids', 'services', 'service_id_assignments', 'web_services')
;

-- Any remaining tables go to the share schema 
SELECT format(
  'ALTER TABLE %I.%I.%I SET SCHEMA %I;',
  table_catalog,
  table_schema,
  table_name,
  'share'
)
FROM information_schema.tables
WHERE table_schema = 'public' 
AND table_name NOT LIKE 'cloud_%'
AND table_name NOT IN ('users', 'cloud_subscribers', 'tours', 'org_members', 'projects')
AND table_name NOT LIKE 'pg_%'
AND table_name NOT LIKE 'debug_%'
;

-- Ensure share user can access the public view.
GRANT SELECT on public.cloud_subscribers TO PUBLIC;
GRANT SELECT on public.users TO PUBLIC;
GRANT REFERENCES on public.users TO PUBLIC;
GRANT SELECT on public.tours TO PUBLIC;
GRANT SELECT on public.cloud_users TO PUBLIC;
GRANT SELECT on public.org_members TO PUBLIC;
GRANT SELECT on public.projects TO PUBLIC;
-- cloud current can add/remove tours
GRANT ALL on public.tours TO cloud;

-- Generate all the required ALTER TABLE statements to change the owner of each table to the correct user.
SELECT format(
  'ALTER TABLE %I.%I.%I OWNER TO %I;',
  table_catalog,
  table_schema,
  table_name,
  'cloud'
)
FROM information_schema.tables
WHERE (table_schema = 'cloud') OR (table_schema = 'public' AND table_name IN ('cloud_subscribers', 'cloud_users'))
;

-- Generate all the required ALTER TABLE statements to change the owner of each table to the correct user.
SELECT format(
  'ALTER TABLE %I.%I.%I OWNER TO %I;',
  table_catalog,
  table_schema,
  table_name,
  'share'
)
FROM information_schema.tables
WHERE (table_schema = 'share') OR (table_schema = 'public' AND table_name IN ('users', 'tours', 'org_members', 'projects'))
;


-- Run the generated ALTER TABLE statements to change the table owners, then do the same but to move them into the
-- schema.

-- Now test the user permissions and then change each app's DB credentials to the new users and re-deploy.
