-- Migration: add_source_status_column
-- Created at: 1764991593

ALTER TABLE data_refresh_log ADD COLUMN IF NOT EXISTS source_status TEXT;;