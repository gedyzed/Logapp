-- Finance lookup indexes. Statements are separated by a line that contains only -- >>>.
CREATE INDEX IF NOT EXISTS idx_payment_events_budget ON payment_events (project_budget_id, budget_line_id);
-- >>>
CREATE INDEX IF NOT EXISTS idx_batch_items_batch ON batch_items (batch_id);
-- >>>
CREATE INDEX IF NOT EXISTS idx_batch_projects_batch ON batch_projects (batch_id);
-- >>>
CREATE INDEX IF NOT EXISTS idx_financial_batches_status ON financial_batches (status, prepared_at);
