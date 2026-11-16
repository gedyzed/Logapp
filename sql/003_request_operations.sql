-- Request lookup indexes. Statements are separated by a line that contains only -- >>>.
CREATE INDEX IF NOT EXISTS idx_requests_status ON requests (status);
-- >>>
CREATE INDEX IF NOT EXISTS idx_requests_order_date ON requests (order_date);
-- >>>
CREATE INDEX IF NOT EXISTS idx_requests_registered ON requests (registered_at);
-- >>>
CREATE INDEX IF NOT EXISTS idx_state_events_request ON request_state_events (request_id, event_number);
