-- Tracks the number of downloads per branch per day
CREATE TABLE project_branch_daily_downloads (

    -- The release that was downloaded
    branch_id UUID NOT NULL REFERENCES project_branches(id) ON DELETE CASCADE,

    -- The day of the downloads
    day DATE NOT NULL,

    -- The number of downloads on that day
    downloads INTEGER NOT NULL,

    PRIMARY KEY (branch_id, day)
);

-- Add these indexes since these queries are used in the metrics dashboards
CREATE INDEX project_branch_daily_downloads_by_day ON project_branch_daily_downloads(day DESC);
CREATE INDEX project_release_daily_downloads_by_day ON project_release_daily_downloads(day DESC);
