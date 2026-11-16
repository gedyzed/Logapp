CREATE TABLE IF NOT EXISTS schema_migrations (
  version INTEGER PRIMARY KEY,
  filename TEXT NOT NULL,
  applied_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS departments (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  active INTEGER NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS currencies (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  active INTEGER NOT NULL DEFAULT 1
);
CREATE TABLE IF NOT EXISTS fx_rate_months (
  id TEXT PRIMARY KEY,
  rate_month TEXT NOT NULL UNIQUE,
  etb_per_eur REAL NOT NULL,
  usd_per_eur REAL NOT NULL,
  gbp_per_eur REAL NOT NULL,
  source TEXT NOT NULL,
  fetched_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS project_budgets (
  id TEXT PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  department_code TEXT NOT NULL REFERENCES departments(code),
  last_checkpoint_at TEXT,
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS budget_lines (
  id TEXT PRIMARY KEY,
  project_budget_id TEXT NOT NULL REFERENCES project_budgets(id),
  code TEXT NOT NULL,
  description TEXT NOT NULL DEFAULT '',
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE (project_budget_id, code)
);
CREATE TABLE IF NOT EXISTS areas (
  id TEXT PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  country_code TEXT NOT NULL DEFAULT 'ETH',
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS people (
  id TEXT PRIMARY KEY,
  full_name TEXT NOT NULL,
  email TEXT,
  department_code TEXT REFERENCES departments(code),
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE UNIQUE INDEX IF NOT EXISTS people_email_unique ON people (email) WHERE email IS NOT NULL;
CREATE TABLE IF NOT EXISTS suppliers (
  id TEXT PRIMARY KEY,
  supplier_name TEXT NOT NULL,
  email TEXT,
  phone TEXT,
  notes TEXT,
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE UNIQUE INDEX IF NOT EXISTS suppliers_name_lower ON suppliers (lower(supplier_name));
CREATE TABLE IF NOT EXISTS requests (
  id TEXT PRIMARY KEY,
  submission_key TEXT NOT NULL UNIQUE,
  request_reference_base TEXT NOT NULL,
  request_reference_sequence INTEGER NOT NULL,
  request_reference TEXT NOT NULL UNIQUE,
  po_reference_base TEXT,
  po_reference_sequence INTEGER,
  po_reference TEXT UNIQUE,
  budget_line_id TEXT NOT NULL REFERENCES budget_lines(id),
  area_id TEXT NOT NULL REFERENCES areas(id),
  department_code TEXT NOT NULL REFERENCES departments(code),
  requester_id TEXT NOT NULL REFERENCES people(id),
  submitted_by_id TEXT NOT NULL REFERENCES people(id),
  supplier_id TEXT REFERENCES suppliers(id),
  description TEXT NOT NULL,
  quantity REAL,
  unit TEXT,
  amount REAL NOT NULL,
  currency TEXT NOT NULL REFERENCES currencies(code),
  scenario TEXT NOT NULL,
  expected_days INTEGER NOT NULL,
  waiver_memo TEXT,
  remarks TEXT,
  order_date TEXT NOT NULL,
  po_reason TEXT,
  po_date TEXT,
  due_date TEXT,
  status TEXT NOT NULL,
  registered_at TEXT NOT NULL,
  status_changed_at TEXT NOT NULL,
  pending_entered_at TEXT,
  cancelled_at TEXT,
  cancellation_reason TEXT,
  edited_at TEXT,
  edit_count INTEGER NOT NULL DEFAULT 0,
  deleted_at TEXT,
  updated_at TEXT NOT NULL,
  version INTEGER NOT NULL DEFAULT 1,
  UNIQUE (request_reference_base, request_reference_sequence)
);
CREATE TABLE IF NOT EXISTS request_state_events (
  id TEXT PRIMARY KEY,
  request_id TEXT NOT NULL REFERENCES requests(id),
  event_number INTEGER NOT NULL,
  operation_key TEXT NOT NULL UNIQUE,
  from_status TEXT,
  to_status TEXT NOT NULL,
  occurred_at TEXT NOT NULL,
  actual_days INTEGER,
  delay_days INTEGER,
  is_delayed INTEGER,
  remarks TEXT,
  UNIQUE (request_id, event_number)
);
CREATE TABLE IF NOT EXISTS request_revisions (
  id TEXT PRIMARY KEY,
  request_id TEXT NOT NULL REFERENCES requests(id),
  revision_no INTEGER NOT NULL,
  changed_at TEXT NOT NULL,
  status_at_edit TEXT NOT NULL,
  changed_fields TEXT NOT NULL,
  UNIQUE (request_id, revision_no)
);
CREATE TABLE IF NOT EXISTS payment_events (
  id TEXT PRIMARY KEY,
  request_id TEXT NOT NULL UNIQUE REFERENCES requests(id),
  project_budget_id TEXT NOT NULL REFERENCES project_budgets(id),
  budget_line_id TEXT NOT NULL REFERENCES budget_lines(id),
  original_amount REAL NOT NULL,
  original_currency TEXT NOT NULL REFERENCES currencies(code),
  payment_date TEXT NOT NULL,
  payment_method TEXT NOT NULL DEFAULT 'recorded',
  recorded_at TEXT NOT NULL,
  fx_rate_month_id TEXT NOT NULL REFERENCES fx_rate_months(id),
  fx_is_fallback INTEGER NOT NULL DEFAULT 0,
  etb_value REAL NOT NULL,
  usd_value REAL NOT NULL,
  eur_value REAL NOT NULL,
  gbp_value REAL NOT NULL,
  remarks TEXT
);
CREATE TABLE IF NOT EXISTS financial_batches (
  id TEXT PRIMARY KEY,
  batch_code TEXT NOT NULL UNIQUE,
  batch_name TEXT NOT NULL,
  status TEXT NOT NULL,
  prepared_at TEXT NOT NULL,
  applied_at TEXT,
  rejected_at TEXT,
  total_gbp REAL NOT NULL,
  remarks TEXT
);
CREATE TABLE IF NOT EXISTS batch_projects (
  id TEXT PRIMARY KEY,
  batch_id TEXT NOT NULL REFERENCES financial_batches(id) ON DELETE CASCADE,
  project_budget_id TEXT NOT NULL REFERENCES project_budgets(id),
  draft_lock INTEGER NOT NULL DEFAULT 1,
  UNIQUE (batch_id, project_budget_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS idx_batch_projects_draft
  ON batch_projects (project_budget_id) WHERE draft_lock = 1;
CREATE TABLE IF NOT EXISTS batch_items (
  id TEXT PRIMARY KEY,
  batch_id TEXT NOT NULL REFERENCES financial_batches(id) ON DELETE CASCADE,
  payment_event_id TEXT NOT NULL REFERENCES payment_events(id),
  gbp_amount REAL NOT NULL,
  released_at TEXT,
  UNIQUE (batch_id, payment_event_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS idx_batch_items_open
  ON batch_items (payment_event_id) WHERE released_at IS NULL;
CREATE TABLE IF NOT EXISTS email_recipients (
  id TEXT PRIMARY KEY,
  display_name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  active INTEGER NOT NULL DEFAULT 1,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  deactivated_at TEXT
);
CREATE TABLE IF NOT EXISTS weekly_notification_runs (
  id TEXT PRIMARY KEY,
  period_key TEXT NOT NULL UNIQUE,
  period_start TEXT NOT NULL,
  period_end TEXT NOT NULL,
  created_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS email_deliveries (
  id TEXT PRIMARY KEY,
  run_id TEXT NOT NULL REFERENCES weekly_notification_runs(id),
  recipient_id TEXT NOT NULL REFERENCES email_recipients(id),
  status TEXT NOT NULL,
  attempt_count INTEGER NOT NULL DEFAULT 0,
  last_attempt_at TEXT,
  accepted_at TEXT,
  error_message TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  UNIQUE (run_id, recipient_id)
);
CREATE TABLE IF NOT EXISTS settings (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  business_timezone TEXT NOT NULL,
  weekly_cutoff_local TEXT NOT NULL,
  default_currency TEXT NOT NULL REFERENCES currencies(code),
  default_country TEXT NOT NULL,
  default_submitted_by TEXT NOT NULL,
  notification_catchup_hours INTEGER NOT NULL DEFAULT 72,
  application_base_url TEXT,
  data_revision INTEGER NOT NULL DEFAULT 0,
  updated_at TEXT NOT NULL
);
