ALTER TABLE service_id_assignments DROP CONSTRAINT service_id_assignments_service_hash_fkey ;
ALTER TABLE named_services DROP CONSTRAINT named_services_service_hash_fkey;

ALTER TABLE services ALTER COLUMN service_hash TYPE text;
ALTER TABLE named_services ALTER COLUMN service_hash TYPE text;
ALTER TABLE named_services ADD CONSTRAINT service_id_assignments_service_hash_fkey FOREIGN KEY (service_hash) REFERENCES services(service_hash);
ALTER TABLE service_id_assignments ALTER COLUMN service_hash TYPE text;
ALTER TABLE service_id_assignments ADD CONSTRAINT service_id_assignments_service_hash_fkey FOREIGN KEY (service_hash) REFERENCES services(service_hash);

ALTER TABLE web_services ALTER COLUMN service_hash TYPE text;
ALTER TABLE web_services ADD CONSTRAINT web_services_service_hash_fkey FOREIGN KEY (service_hash) REFERENCES services(service_hash);

ALTER TABLE web_services DROP COLUMN service_name;
ALTER TABLE web_services ADD COLUMN services_version smallint;

