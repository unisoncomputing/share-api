CREATE TABLE oauth_clients (
  client_id text PRIMARY KEY NOT NULL CHECK (client_id <> ''),
  -- A null secret means the client is public (e.g. ucm)
  -- and that we need to use a PKCE flow
  client_secret text NULL CHECK (client_secret <> ''),
  redirect_host text NOT NULL CHECK (redirect_host <> ''),
  approved_scopes text NOT NULL CHECK (approved_scopes <> ''),
  audience text NOT NULL CHECK (audience <> '')
);
