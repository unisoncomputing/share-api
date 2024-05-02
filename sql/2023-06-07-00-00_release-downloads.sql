-- Tracks the number of downloads per release per day
CREATE TABLE project_release_daily_downloads (

    -- The release that was downloaded
    release_id UUID NOT NULL REFERENCES project_releases(id) ON DELETE CASCADE,

    -- The day of the downloads
    day DATE NOT NULL,

    -- The number of downloads on that day
    downloads INTEGER NOT NULL,

    PRIMARY KEY (release_id, day)
);
