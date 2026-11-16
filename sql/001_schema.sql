-- Logapp schema. Statements are separated by a line that contains only -- >>>.
CREATE TABLE schema_migrations (
  version INTEGER PRIMARY KEY,
  filename TEXT NOT NULL,
  applied_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
-- >>>
CREATE TABLE departments (
  code VARCHAR(20) PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  CONSTRAINT departments_name_nonblank CHECK (btrim(name) <> '')
);
-- >>>
CREATE TABLE currencies (
  code CHAR(3) PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  CONSTRAINT currencies_name_nonblank CHECK (btrim(name) <> '')
);
-- >>>
CREATE TABLE fx_rate_months (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  rate_month VARCHAR(7) NOT NULL UNIQUE,
  etb_per_eur NUMERIC(24,10) NOT NULL,
  usd_per_eur NUMERIC(24,10) NOT NULL,
  gbp_per_eur NUMERIC(24,10) NOT NULL,
  source VARCHAR(50) NOT NULL,
  fetched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT fx_month_shape CHECK (rate_month ~ '^[0-9]{4}-[0-9]{2}$'),
  CONSTRAINT fx_rates_positive CHECK (etb_per_eur > 0 AND usd_per_eur > 0 AND gbp_per_eur > 0)
);
-- >>>
CREATE TABLE project_budgets (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  code VARCHAR(40) NOT NULL UNIQUE,
  name VARCHAR(200) NOT NULL,
  department_code VARCHAR(20) NOT NULL REFERENCES departments(code),
  last_checkpoint_at TIMESTAMPTZ,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT project_code_nonblank CHECK (btrim(code) <> ''),
  CONSTRAINT project_name_nonblank CHECK (btrim(name) <> '')
);
-- >>>
CREATE TABLE budget_lines (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_budget_id UUID NOT NULL REFERENCES project_budgets(id),
  code VARCHAR(40) NOT NULL,
  description VARCHAR(300) NOT NULL DEFAULT '',
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (project_budget_id, code),
  CONSTRAINT budget_line_code_nonblank CHECK (btrim(code) <> '')
);
-- >>>
CREATE TABLE areas (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  code VARCHAR(40) NOT NULL UNIQUE,
  name VARCHAR(200) NOT NULL,
  country_code CHAR(3) NOT NULL DEFAULT 'ETH',
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT area_code_nonblank CHECK (btrim(code) <> ''),
  CONSTRAINT area_name_nonblank CHECK (btrim(name) <> '')
);
-- >>>
CREATE TABLE people (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  full_name VARCHAR(200) NOT NULL,
  email VARCHAR(200),
  department_code VARCHAR(20) REFERENCES departments(code),
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT people_name_nonblank CHECK (btrim(full_name) <> '')
);
-- >>>
CREATE UNIQUE INDEX people_email_unique ON people (email) WHERE email IS NOT NULL;
-- >>>
CREATE TABLE suppliers (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  supplier_name VARCHAR(200) NOT NULL,
  email VARCHAR(200),
  phone VARCHAR(50),
  notes TEXT,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT supplier_name_nonblank CHECK (btrim(supplier_name) <> '')
);
-- >>>
CREATE UNIQUE INDEX suppliers_name_lower ON suppliers (lower(supplier_name));
-- >>>
CREATE TABLE requests (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  submission_key UUID NOT NULL UNIQUE,
  request_reference_base TEXT NOT NULL,
  request_reference_sequence INTEGER NOT NULL,
  request_reference TEXT NOT NULL UNIQUE,
  po_reference_base TEXT,
  po_reference_sequence INTEGER,
  po_reference TEXT UNIQUE,
  budget_line_id UUID NOT NULL REFERENCES budget_lines(id),
  area_id UUID NOT NULL REFERENCES areas(id),
  department_code VARCHAR(20) NOT NULL REFERENCES departments(code),
  requester_id UUID NOT NULL REFERENCES people(id),
  submitted_by_id UUID NOT NULL REFERENCES people(id),
  supplier_id UUID REFERENCES suppliers(id),
  description TEXT NOT NULL,
  quantity NUMERIC(14,3),
  unit VARCHAR(80),
  amount NUMERIC(20,8) NOT NULL,
  currency CHAR(3) NOT NULL REFERENCES currencies(code),
  scenario CHAR(1) NOT NULL,
  expected_days INTEGER NOT NULL,
  waiver_memo TEXT,
  remarks TEXT,
  order_date DATE NOT NULL,
  po_reason VARCHAR(160),
  po_date DATE,
  due_date DATE GENERATED ALWAYS AS (order_date + expected_days) STORED,
  status VARCHAR(20) NOT NULL,
  registered_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  status_changed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  pending_entered_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  cancellation_reason TEXT,
  edited_at TIMESTAMPTZ,
  edit_count INTEGER NOT NULL DEFAULT 0,
  deleted_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version INTEGER NOT NULL DEFAULT 1,
  UNIQUE (request_reference_base, request_reference_sequence),
  CONSTRAINT requests_status CHECK (status IN ('under_review', 'pending', 'paid', 'cancelled')),
  CONSTRAINT requests_scenario CHECK (scenario IN ('A', 'B', 'C', 'D', 'E', 'G')),
  CONSTRAINT requests_amount_positive CHECK (amount > 0),
  CONSTRAINT requests_description_nonblank CHECK (btrim(description) <> '')
);
-- >>>
CREATE TABLE request_state_events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  request_id UUID NOT NULL REFERENCES requests(id),
  event_number INTEGER NOT NULL,
  operation_key UUID NOT NULL UNIQUE,
  from_status VARCHAR(20),
  to_status VARCHAR(20) NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  actual_days INTEGER,
  delay_days INTEGER,
  is_delayed BOOLEAN GENERATED ALWAYS AS (CASE WHEN delay_days IS NULL THEN NULL WHEN delay_days > 0 THEN TRUE ELSE FALSE END) STORED,
  remarks TEXT,
  UNIQUE (request_id, event_number)
);
-- >>>
CREATE TABLE request_revisions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  request_id UUID NOT NULL REFERENCES requests(id),
  revision_no INTEGER NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  status_at_edit VARCHAR(20) NOT NULL,
  changed_fields JSONB NOT NULL,
  UNIQUE (request_id, revision_no),
  CONSTRAINT revision_status CHECK (status_at_edit IN ('under_review', 'pending'))
);
-- >>>
CREATE TABLE payment_events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  request_id UUID NOT NULL UNIQUE REFERENCES requests(id),
  project_budget_id UUID NOT NULL REFERENCES project_budgets(id),
  budget_line_id UUID NOT NULL REFERENCES budget_lines(id),
  original_amount NUMERIC(20,8) NOT NULL,
  original_currency CHAR(3) NOT NULL REFERENCES currencies(code),
  payment_date DATE NOT NULL,
  payment_method VARCHAR(50) NOT NULL DEFAULT 'recorded',
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  fx_rate_month_id UUID NOT NULL REFERENCES fx_rate_months(id),
  fx_is_fallback BOOLEAN NOT NULL DEFAULT FALSE,
  etb_value NUMERIC(28,8) NOT NULL,
  usd_value NUMERIC(28,8) NOT NULL,
  eur_value NUMERIC(28,8) NOT NULL,
  gbp_value NUMERIC(28,8) NOT NULL,
  remarks TEXT,
  CONSTRAINT payment_amount_positive CHECK (original_amount > 0),
  CONSTRAINT payment_values_nonnegative CHECK (
    etb_value >= 0 AND usd_value >= 0 AND eur_value >= 0 AND gbp_value >= 0
  )
);
-- >>>
CREATE TABLE financial_batches (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  batch_code VARCHAR(40) NOT NULL UNIQUE,
  batch_name VARCHAR(80) NOT NULL,
  status VARCHAR(20) NOT NULL,
  prepared_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  applied_at TIMESTAMPTZ,
  rejected_at TIMESTAMPTZ,
  total_gbp NUMERIC(20,2) NOT NULL,
  remarks TEXT,
  CONSTRAINT batch_status CHECK (status IN ('active', 'applied', 'rejected')),
  CONSTRAINT batch_name_nonblank CHECK (btrim(batch_name) <> ''),
  CONSTRAINT batch_code_shape CHECK (batch_code ~ '^FB-[0-9]{4}-[0-9]{3}$')
);
-- >>>
CREATE TABLE batch_projects (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  batch_id UUID NOT NULL REFERENCES financial_batches(id) ON DELETE CASCADE,
  project_budget_id UUID NOT NULL REFERENCES project_budgets(id),
  draft_lock BOOLEAN NOT NULL DEFAULT TRUE,
  UNIQUE (batch_id, project_budget_id)
);
-- >>>
CREATE UNIQUE INDEX idx_batch_projects_draft
  ON batch_projects (project_budget_id) WHERE draft_lock;
-- >>>
CREATE TABLE batch_items (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  batch_id UUID NOT NULL REFERENCES financial_batches(id) ON DELETE CASCADE,
  payment_event_id UUID NOT NULL REFERENCES payment_events(id),
  gbp_amount NUMERIC(20,2) NOT NULL,
  released_at TIMESTAMPTZ,
  UNIQUE (batch_id, payment_event_id)
);
-- >>>
CREATE UNIQUE INDEX idx_batch_items_open
  ON batch_items (payment_event_id) WHERE released_at IS NULL;
-- >>>
CREATE TABLE email_recipients (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  display_name VARCHAR(200) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deactivated_at TIMESTAMPTZ
);
-- >>>
CREATE TABLE weekly_notification_runs (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  period_key DATE NOT NULL UNIQUE,
  period_start TIMESTAMPTZ NOT NULL,
  period_end TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT weekly_period_order CHECK (period_end > period_start)
);
-- >>>
CREATE TABLE email_deliveries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id UUID NOT NULL REFERENCES weekly_notification_runs(id),
  recipient_id UUID NOT NULL REFERENCES email_recipients(id),
  status VARCHAR(20) NOT NULL,
  attempt_count INTEGER NOT NULL DEFAULT 0,
  last_attempt_at TIMESTAMPTZ,
  accepted_at TIMESTAMPTZ,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (run_id, recipient_id),
  CONSTRAINT delivery_status CHECK (status IN ('queued', 'sending', 'sent', 'failed'))
);
-- >>>
CREATE TABLE settings (
  id SMALLINT PRIMARY KEY,
  business_timezone VARCHAR(80) NOT NULL,
  weekly_cutoff_local TIME NOT NULL,
  default_currency CHAR(3) NOT NULL REFERENCES currencies(code),
  default_country CHAR(3) NOT NULL,
  default_submitted_by VARCHAR(200) NOT NULL,
  notification_catchup_hours INTEGER NOT NULL DEFAULT 72,
  application_base_url TEXT,
  data_revision BIGINT NOT NULL DEFAULT 0,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT settings_singleton CHECK (id = 1)
);
