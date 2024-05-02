ALTER TABLE users
ADD COLUMN bio text NULL CHECK (length(bio) > 0),
ADD COLUMN website text NULL CHECK (length(website) > 0),
ADD COLUMN location text NULL CHECK (length(location) > 0),
ADD COLUMN twitterHandle text NULL CHECK (length(twitterHandle) > 0),
ADD COLUMN pronouns text NULL CHECK (length(pronouns) > 0);
