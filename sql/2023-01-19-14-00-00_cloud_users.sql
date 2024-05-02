-- Users in this table have access to unison cloud.
CREATE TABLE cloud_users (
    user_id uuid PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE
)
