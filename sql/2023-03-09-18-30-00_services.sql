CREATE TABLE environments (
    id uuid NOT NULL DEFAULT uuid_generate_v4() PRIMARY KEY,
    user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name text NOT NULL,
    created_at timestamp NOT NULL DEFAULT NOW(),
    UNIQUE (user_id, name)
);

CREATE TABLE services (
    user_id uuid NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    service_hash bytea NOT NULL PRIMARY KEY,
    deployed_at timestamp NOT NULL DEFAULT NOW(),
    undeployed_at timestamp NULL,
    environment uuid NOT NULL REFERENCES environments(id) ON DELETE CASCADE,
    UNIQUE (user_id, service_hash)
);

CREATE INDEX services_by_user_id ON services(user_id);
CREATE INDEX services_by_service_hash ON services(service_hash);

CREATE TABLE web_services (
    user_id uuid NOT NULL REFERENCES users(id) PRIMARY KEY,
    service_name text,
    service_hash bytea NOT NULL,
    deployed_at timestamp NOT NULL DEFAULT NOW(),
    undeployed_at timestamp NULL,
    UNIQUE (user_id, service_name)
);

