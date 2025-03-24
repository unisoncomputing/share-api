-- Table for users who have enhanced privileges for administrating Share.
CREATE TABLE superadmins (
    user_id UUID PRIMARY KEY REFERENCES users(id)
);
