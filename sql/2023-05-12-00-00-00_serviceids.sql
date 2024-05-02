
CREATE TABLE service_ids (
    service_id text NOT NULL PRIMARY KEY,
    user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    UNIQUE (service_id)
);

CREATE TABLE service_id_assignments (
    user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    service_id Text NOT NULL,
    service_hash bytea NOT NULL REFERENCES services(service_hash) ON DELETE CASCADE,
    assignment_time timestamp NOT NULL DEFAULT NOW()
);

CREATE TABLE named_services (
    service_hash bytea NOT NULL PRIMARY KEY  REFERENCES services(service_hash) ON DELETE CASCADE,
    user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    service_name text NOT NULL ,  
    UNIQUE (user_id, service_name)
);


