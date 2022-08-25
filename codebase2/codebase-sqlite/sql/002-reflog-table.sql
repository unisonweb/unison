CREATE TABLE reflog (
    -- Reminder that SQLITE doesn't have any actual 'time' type,
    -- This column contains TEXT values formatted as ISO8601 strings
    -- ("YYYY-MM-DD HH:MM:SS.SSS")
    time TEXT NOT NULL,
    root_causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
    reason TEXT NOT NULL
);

CREATE INDEX reflog_time_desc ON reflog (
  time DESC
)

-- Semicolon intentionally omitted, for the same reason
-- semicolons in comments will blow up codebase initialization.
-- (oops, almost used a semicolon at the end of that last phrase!)
-- Sqlite doesn't let us submit multiple statements in the same
-- command, so we are using Haskell code to segment the statements
-- by splitting on semicolons.  It doesn't know to ignore comments,
-- though I guess that wouldn't be hard to implement.  Should have
-- done it from the start.
