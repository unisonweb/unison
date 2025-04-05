-- Add a new column to the project_branch table to store the causal_hash_id
ALTER TABLE project_branch ADD COLUMN causal_hash_id INTEGER NOT NULL;
