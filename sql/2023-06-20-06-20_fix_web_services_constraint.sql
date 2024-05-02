ALTER TABLE web_services
      DROP CONSTRAINT web_services_pkey;

ALTER TABLE web_services
      ADD CONSTRAINT web_services_pkey
      PRIMARY KEY (service_hash);


