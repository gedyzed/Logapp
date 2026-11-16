-- Notification lookup indexes. Statements are separated by a line that contains only -- >>>.
CREATE INDEX IF NOT EXISTS idx_email_deliveries_status ON email_deliveries (status, run_id);
-- >>>
CREATE INDEX IF NOT EXISTS idx_email_recipients_active ON email_recipients (active);
