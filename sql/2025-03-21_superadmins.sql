-- Table for users who have enhanced privileges for administrating Share.
CREATE TABLE superadmins (
    user_id PRIMARY KEY REFERENCES users(id)
);
