CREATE TABLE reflog (
    -- Reminder that SQLITE doesn't have any actual 'time' type,
    -- This column contains TEXT values formatted as ISO8601 strings
    -- ("YYYY-MM-DD HH:MM:SS.SSS")
    time TEXT NOT NULL,
    from_root_causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
    to_root_causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
    reason TEXT NOT NULL
);

CREATE INDEX reflog_time_desc ON reflog (
  time DESC
);
