# Logistics Procurement Tracker: PostgreSQL database design

**Version:** 1.1 review candidate  
**Prepared:** November 2026  
**Companion contract:** `PRODUCT_SPECIFICATION.md`  
**Database:** PostgreSQL, one shared application database  
**Deliverable:** logical/physical design and integrity contract, ready for clean PostgreSQL migration

---

## 1. Design decisions that matter most

1. One request belongs to exactly one budget line; its parent budget is derived through `budget_lines.project_budget_id`.
2. A request can create at most one immutable payment event.
3. Checkpoints belong to project budgets. `last_checkpoint_at` is stored directly on `project_budgets` and updated atomically whenever a batch is confirmed.
4. A project budget can belong to at most one draft batch at a time.
5. Draft membership and line totals are frozen; new payments stay outside the active draft.
6. Applied payment membership remains permanently acknowledged; abandoning a draft releases membership without deleting history.
7. Normalized financial batches: `financial_batches` links to included projects via `batch_projects` and included requests via `batch_items` (no comma-separated text strings).
8. Original request currency/amount and frozen multi-currency valuations (`usd_value`, `eur_value`, `gbp_value`, `etb_value`) are snapshotted permanently in `payment_events`.
9. Recipients and delivery logs live in PostgreSQL (`email_recipients`, `weekly_notification_runs`, `email_deliveries`), ensuring an audit-proof notification record.
10. Pending entry/exit timestamps and SLA target tracking (`due_date`, `actual_days`, `delay_days`) support Pending cycle time and Overdue metrics.
11. Exact decimal calculations, constraints and transactions are required; UI checks alone are insufficient [D1-D3].

The design intentionally does not contain arbitrary users/roles as product entities, allocation rows, partial payments, or a separate demo/live database switch.

## 2. Diagram files

- `DATABASE_ERD.png`: rendered overview for discussion.
- `DATABASE_ERD.mmd`: editable Mermaid source including direct foreign-key relationships.

![PostgreSQL relationship overview](DATABASE_ERD.png)

## 3. Conventions

### 3.1 Keys and Idempotency

- Business table primary keys: UUID, generated server-side with `gen_random_uuid()` or an equivalent approved UUID generator [D6].
- Foreign keys use UUID, never human reference strings.
- `submission_key`: A unique UUID generated per request creation modal session to guarantee idempotency and prevent duplicate inserts from double-clicks or network replays.
- `app_defaults`: Key-value configuration table (`setting_key VARCHAR(50) PK`, `setting_value TEXT`).
- `schema_migrations.version`: Integer primary key tracking applied migration revisions.
- IDs are opaque to R/UI; do not coerce them into 32-bit integers.
- Human references (`request_reference`, `po_reference`, `batch_code`) are separate unique fields.

### 3.2 Dates and times

- `TIMESTAMPTZ` for exact instants: registration, state changes, batch preparation/application, email delivery attempts. Stored in UTC, formatted and displayed in `Africa/Addis_Ababa` (UTC+3) by default [D5].
- `DATE` for pure calendar business dates: `order_date`, `po_date`, `payment_date`. Stored as calendar day (`YYYY-MM-DD`) without timezone shifts.
- Capture one server timestamp per business transition and reuse it consistently for request, state, and payment records.
- `registered_at` is an immutable server-assigned clock timestamp, not a user-entered date.
- Calendar filter week: Monday-Sunday.
- Reporting week: Friday cutoff (14:00 Africa/Addis_Ababa) to next Friday cutoff, half-open interval.

### 3.3 Numeric values

- Original request amount: NUMERIC(20,10), positive, finite.
- Quantity: NUMERIC(14,3), optional positive.
- InforEuro / monthly exchange rates: NUMERIC(24,10), positive finite.
- Frozen converted payment values (`etb_value`, `usd_value`, `eur_value`, `gbp_value`): NUMERIC(28,8), nonnegative finite.
- Batch line displayed totals and financial export deductions: NUMERIC(20,2), rounded half-away-from-zero using PostgreSQL decimal arithmetic [D3].
- Store positive payment values. The UI/export negates once to display deductions.
- Reject NaN and infinity explicitly; a positive-value check alone is not a complete finite-value check for PostgreSQL numeric.

### 3.4 Nullability and deletion

Every field is NOT NULL unless explicitly marked nullable. Optional text uses NULL for absence. Required text must pass a nonblank check.

All historical and business FKs use `ON DELETE RESTRICT` / `NO ACTION` (with `ON DELETE CASCADE` reserved strictly for internal batch junction rows like `batch_projects` and `batch_items` when a draft is discarded). No cascade deletion of requests, payments, batches, recipients with history, or rates in use. The application UI never hard-deletes financial or history records.

### 3.5 Snapshots and deliberate duplication

- **Reference Snapshots:** Issued document references (`OF/...` and `PO/...`) freeze component values so subsequent master data edits never alter issued legal numbers.
- **Payment Valuation Snapshots:** `payment_events` captures exact converted values across all four currencies on payment day. Future exchange rate publications never recalculate past paid events.
- **Checkpoint Batch Snapshots:** `batch_items` captures exact frozen line amounts at the moment of batch creation.

---

## 4. Entity inventory

Thirteen core tables, grouped by purpose:

**Reference data:** `project_budgets`, `budget_lines`, `areas`, `people`, `suppliers`.  
**Procurement:** `requests`, `request_state_events`.  
**Rates & payments:** `currencies` (InforEuro monthly rates & cross-rates), `payment_events`.  
**Finance:** `financial_batches`, `batch_projects`, `batch_items`.  
**Notifications:** `email_recipients`, `weekly_notification_runs`, `email_deliveries`.  
**Support:** `app_defaults`, `schema_migrations`.

---

## 5. Reference-data tables

### 5.1 project_budgets

Purpose: unified project budget/grant entity and financial checkpoint anchor; 
Fields:
- `id UUID`: PK.
- `code VARCHAR(40)`: unique, uppercase normalized business code (e.g. `29FZN`).
- `name VARCHAR(200)`: required readable name.
- `department VARCHAR(40)`: primary managing department (e.g. `WASH`, `LOG`).
- `last_checkpoint_at TIMESTAMPTZ`: nullable; timestamp of the latest applied batch deduction. Updated atomically whenever a batch containing this project is confirmed.
- `active BOOLEAN`: default true.
- `created_at TIMESTAMPTZ`, `updated_at TIMESTAMPTZ`.
- `version INTEGER`: positive optimistic concurrency version, default 1.

Constraints/indexes:
- Unique(code).
- Nonblank code/name.
- Code immutable once referenced in an issued request or financial batch.
- Deactivation rejected if open requests, unapplied paid events, or an active draft batch exist.

### 5.2 budget_lines

Purpose: line item belonging to a project budget.

Fields:
- `id UUID`: PK.
- `project_budget_id UUID`: FK project_budgets.
- `code VARCHAR(40)`: business code (e.g. `SF9AVR`).
- `description VARCHAR(300)`.
- `active BOOLEAN`: default true.
- `created_at TIMESTAMPTZ`, `updated_at TIMESTAMPTZ`.
- `version INTEGER`: default 1.

Constraints/indexes:
- Unique(project_budget_id, code).
- Parent project budget and code immutable once used in a request.
- New requests require both the line and parent project budget to be active.

### 5.3 areas

Purpose: standardized field bases and operational missions.

Fields:
- `id UUID`: PK.
- `code VARCHAR(40)`: unique stable short code (e.g. `MEK`, `ADD`).
- `name VARCHAR(200)`: readable name (e.g. `Mekelle`, `Addis Ababa`).
- `country_code VARCHAR(10)`: default `ETH`.
- `active BOOLEAN`: default true.
- `created_at TIMESTAMPTZ`, `updated_at TIMESTAMPTZ`.
- `version INTEGER`.

Constraints/indexes:
- Unique(code).
- Short code and country code are used in request references and are immutable once issued.

### 5.4 people

Purpose: registered staff contacts for requesters and logistics focal points.

Fields:
- `id UUID`: PK.
- `full_name VARCHAR(200)`: required display name.
- `email VARCHAR(200)`: unique, normalized email address.
- `department VARCHAR(40)`: optional department affiliation.
- `active BOOLEAN`: default true.
- `created_at TIMESTAMPTZ`, `updated_at TIMESTAMPTZ`.
- `version INTEGER`.

Constraints/indexes:
- Unique(email).
- Inactive staff cannot be selected for new requests but remain intact in historical records.

### 5.5 suppliers

Purpose: deduplicated vendor register.

Fields:
- `id UUID`: PK.
- `supplier_name VARCHAR(200)`: unique, normalized supplier name.
- `email VARCHAR(200)`: nullable contact email.
- `phone VARCHAR(50)`: nullable contact phone.
- `notes TEXT`: optional background notes.
- `active BOOLEAN`: default true.
- `created_at TIMESTAMPTZ`, `updated_at TIMESTAMPTZ`.
- `version INTEGER`.

Constraints/indexes:
- Unique(supplier_name).
- Assigned at PO issuance/approval. Deactivation does not alter issued POs.

---

## 6. Requests and state history

### 6.1 requests

Purpose: operational procurement requests, approvals, and reference generation.

Identity & Idempotency:
- `id UUID`: PK.
- `submission_key UUID`: unique creation-operation token to guarantee idempotency under concurrent retries.
- `request_reference TEXT`: unique issued OF reference (`OF/{budget}/{line}/{country}/{area}/{department}/{dd-mm-yyyy}`).
- `request_reference_base TEXT`: base reference string before collision suffix.
- `request_reference_sequence INTEGER`: positive integer; 1 for first occurrence, 2+ for collision suffixes (`/002`, `/003`).
- `po_reference TEXT`: nullable unique issued PO reference (`PO/OF/...`).
- `po_reference_base TEXT`: nullable base string.
- `po_reference_sequence INTEGER`: nullable positive integer.

Classification:
- `budget_line_id UUID`: FK budget_lines. Parent budget derived through this line.
- `area_id UUID`: FK areas. Supplies area code and country code for reference generation.
- `department_code VARCHAR(20)`: requesting department code.
- `requester_id UUID`: FK people.
- `focal_id UUID`: nullable FK people (logistics focal point).
- `supplier_id UUID`: nullable FK suppliers; required from Pending onward and retained if cancelled.

Content & Pricing:
- `description TEXT`: required item/service description.
- `quantity NUMERIC(14,3)`: optional positive quantity.
- `unit VARCHAR(80)`: optional unit of measure; required if quantity is set.
- `amount NUMERIC(20,10)`: required positive original total amount.
- `currency CHAR(3)`: original currency (`ETB`, `USD`, `EUR`, `GBP`).
- `procurement_scenario VARCHAR(10)`: assigned scenario (`A`, `B`, `C`, `D`, `E`, `G`).
- `expected_days INTEGER`: SLA completion threshold (e.g. 7, 14, 21, 45, 60 days).
- `due_date DATE`: target completion date computed upon approval (`po_date + expected_days`).
- `waiver_memo TEXT`: optional waiver or framework justification.
- `remarks TEXT`: consolidated remarks (logistics, finance, and operational notes).

Business dates:
- `order_date DATE`: single business date of submission and reference date (`DD-MM-YYYY`).
- `po_reason VARCHAR(160)`: required at approval.
- `po_date DATE`: date PO issued; required at approval.

State & Lifecycle Timestamps:
- `status VARCHAR(20)`: `under_review`, `pending`, `paid`, `cancelled`.
- `registered_at TIMESTAMPTZ`: immutable server clock timestamp set at registration.
- `status_changed_at TIMESTAMPTZ`: timestamp of most recent transition.
- `pending_entered_at TIMESTAMPTZ`: nullable; stamped once upon entering Pending. Used for real-time overdue age calculation.
- `cancelled_at TIMESTAMPTZ`: nullable.
- `cancellation_reason TEXT`: required if cancelled.
- `updated_at TIMESTAMPTZ`.
- `version INTEGER`: optimistic concurrency version incremented on each transition.

Constraints/indexes:
- Unique(submission_key).
- Unique(request_reference).
- Unique(po_reference) WHERE po_reference IS NOT NULL.
- Unique(request_reference_base, request_reference_sequence).
- Unique(po_reference_base, po_reference_sequence) WHERE po_reference_base IS NOT NULL.
- Index(status, order_date DESC).
- Index(registered_at DESC, id DESC).
- Index(budget_line_id, status).
- Index(area_id), Index(requester_id), Index(supplier_id).
- Partial Index(due_date) WHERE status = 'pending'.

State Integrity Checks:
- `under_review`: `supplier_id`, `po_reference`, `po_date`, and `pending_entered_at` must be NULL.
- `pending`: `supplier_id`, `po_reference`, `po_date`, and `pending_entered_at` must be present.
- `paid`: approval fields and `pending_entered_at` must be present; exactly one corresponding row in `payment_events`; exit metrics recorded in `request_state_events`.
- `cancelled`: `cancellation_reason` and `cancelled_at` must be present. If cancelled after approval, PO fields and pending timestamp are preserved for historical audit.
- Immutability: `registered_at` cannot be updated or backdated; a paid or cancelled request cannot be updated via standard application workflows.

### 6.2 request_state_events

Purpose: Immutable lifecycle transition audit log and waiting time calculation store.

Fields:
- `id UUID`: PK.
- `request_id UUID`: FK requests.
- `event_number INTEGER`: Positive, sequential within request (1, 2, ...).
- `operation_key UUID`: Unique idempotency key for this transition.
- `from_status VARCHAR(20)`: Nullable only for initial submission (`NULL` -> `under_review`).
- `to_status VARCHAR(20)`: `under_review`, `pending`, `paid`, or `cancelled`.
- `occurred_at TIMESTAMPTZ`: Transition timestamp.
- `actual_days REAL`: Actual duration in days spent in `from_status` prior to this transition. Specifically populated upon exiting `pending` to measure real turnaround time.
- `delay_days REAL`: Populated upon exiting `pending` as `actual_days - expected_days`. Positive values indicate delays; zero or negative indicates on-time completion.
- `is_delayed INTEGER`: Populated upon exiting `pending`: `1` if `actual_days > expected_days`, else `0`.
- `remarks TEXT`: Sanitized explanation, rejection reason, or cancellation justification.

Constraints/indexes:
- `Unique(request_id, event_number)`.
- `Unique(operation_key)`.
- Index(`request_id`, `occurred_at DESC`).
- Allowed transitions only:
  - `NULL` -> `under_review`
  - `under_review` -> `pending` (PO issued)
  - `under_review` -> `cancelled`
  - `pending` -> `paid` (payment processed)
  - `pending` -> `cancelled`
- Append-only; strictly no UPDATE or DELETE in runtime operations.

Note on Overdue vs Delay:
- Active Overdue is calculated dynamically on `requests` where `status = 'pending' AND CURRENT_DATE > due_date`.
- Historical Delay is locked into `request_state_events` upon exit from Pending (`delay_days`, `is_delayed`), powering the Insights tab "Completion time by request" without recalculating or drifting.

---

## 7. Currencies and payment events

### 7.1 currencies

Purpose: Single authoritative master table for ISO currencies and official European Commission InforEuro monthly exchange rates.

Fields:
- `id UUID`: PK.
- `rate_month VARCHAR(7)`: Target accounting month formatted as `YYYY-MM` (e.g. `2026-11`).
- `code CHAR(3)`: ISO 4217 currency code (`EUR`, `USD`, `GBP`, `ETB`).
- `rate_to_eur NUMERIC(24,10)`: Official monthly InforEuro conversion rate relative to EUR base (EUR = 1.0000000000).
- `eur_per_unit NUMERIC(24,10)`: Direct multiplier to EUR (`1.0 / rate_to_eur`).
- `usd_per_unit NUMERIC(24,10)`: Cross-rate multiplier to USD.
- `gbp_per_unit NUMERIC(24,10)`: Cross-rate multiplier to GBP.
- `etb_per_unit NUMERIC(24,10)`: Cross-rate multiplier to ETB.
- `active BOOLEAN`: Default `TRUE`. Inactive currencies are hidden from creation selectors.
- `source VARCHAR(50)`: `InforEuro` or `SYSTEM_FIXTURE`.
- `created_at TIMESTAMPTZ`: Ingestion timestamp.
- `updated_at TIMESTAMPTZ`.

Constraints/indexes:
- `Unique(rate_month, code)`.
- `Index(code, rate_month DESC)`.
- Rates must be positive and finite.
- Base rate `EUR` is always 1.0. ETB and USD cross-rates are derived deterministically.
- Replaces legacy multi-table CBE rate structures with a clean, grant-auditable monthly standard.

### 7.2 payment_events

Purpose: Immutable financial realization record created once when a request transitions to `paid`.

Fields:
- `id UUID`: PK.
- `request_id UUID`: Unique FK requests (1:1 relationship with paid request).
- `project_budget_id UUID`: FK project_budgets (immutable historical snapshot of project assignment).
- `budget_line_id UUID`: FK budget_lines (immutable historical snapshot of line assignment).
- `original_amount NUMERIC(18,2)`: Exact invoice/PO amount paid.
- `original_currency CHAR(3)`: Currency of payment.
- `payment_date DATE`: User-entered business payment date.
- `payment_method VARCHAR(50)`: Payment method (e.g., `Bank Transfer`, `Cheque`, `Cash`).
- `recorded_at TIMESTAMPTZ`: Exact system timestamp of payment processing.
- `etb_value NUMERIC(28,8)`: Converted ledger value in ETB using applicable monthly rate.
- `usd_value NUMERIC(28,8)`: Converted ledger value in USD (primary finance deduction currency).
- `eur_value NUMERIC(28,8)`: Converted ledger value in EUR.
- `gbp_value NUMERIC(28,8)`: Converted ledger value in GBP.
- `remarks TEXT`: Optional finance notes or transaction reference.

Constraints/indexes:
- `Unique(request_id)`: Guarantees exactly one payment per request.
- `Index(project_budget_id, budget_line_id, recorded_at)`.
- `Index(payment_date, project_budget_id)`.
- Converted values are frozen snapshots computed in NUMERIC precision at payment time; subsequent rate updates never alter historical payment records.
- Append-only; no UPDATE or DELETE in runtime operations.

---

## 8. Financial batches (checkpoints)

Financial batches group paid payment events by project budget to reconcile spending, export donor deduction summaries, and checkpoint project balances.

### 8.1 financial_batches

Purpose: Top-level batch header tracking batch status and lifecycle.

Fields:
- `id UUID`: PK.
- `batch_code VARCHAR(40)`: Unique standardized batch reference: `FB-YYYY-NNN` (e.g., `FB-2026-001`).
- `status VARCHAR(20)`: Batch state (`draft`, `applied`, `abandoned`).
- `prepared_at TIMESTAMPTZ`: Timestamp when batch draft was assembled.
- `applied_at TIMESTAMPTZ`: Timestamp when batch was confirmed and deducted from project balances. Nullable.
- `abandoned_at TIMESTAMPTZ`: Timestamp when draft batch was discarded. Nullable.
- `total_usd NUMERIC(20,2)`: Total rounded USD deduction amount across all included items.
- `remarks TEXT`: Optional notes or accounting reference.

State transition checks:
- `draft`: `applied_at` and `abandoned_at` must both be NULL.
- `applied`: `applied_at` is present; `abandoned_at` is NULL.
- `abandoned`: `abandoned_at` is present; `applied_at` is NULL.
- Terminal statuses (`applied`, `abandoned`) cannot be re-opened or modified.

### 8.2 batch_projects

Purpose: Normalized junction table linking financial batches to the included project budgets.

Fields:
- `id UUID`: PK.
- `batch_id UUID`: FK financial_batches.
- `project_budget_id UUID`: FK project_budgets.
- `is_active BOOLEAN`: Default `TRUE`. Controls draft reservation locking.

Constraints/indexes:
- `Unique(batch_id, project_budget_id)`.
- `Index(batch_id)`.
- `Index(project_budget_id)`.
- **Partial Unique Index (Draft Locking):**
  ```sql
  CREATE UNIQUE INDEX idx_batch_projects_active_draft
  ON batch_projects (project_budget_id)
  WHERE is_active = TRUE;
  ```
  *Locking Mechanics:* While a batch is in `draft`, `is_active` remains `TRUE`, preventing the project budget from being included in any other concurrent draft batch. When the batch is `applied` or `abandoned`, `is_active` is updated to `FALSE`, instantly freeing the project budget for subsequent batching while preserving full audit history.

### 8.3 batch_items

Purpose: Normalized junction table linking financial batches to individual payment events included in the deduction.

Fields:
- `id UUID`: PK.
- `batch_id UUID`: FK financial_batches.
- `payment_event_id UUID`: FK payment_events.
- `usd_amount NUMERIC(20,2)`: Frozen USD snapshot value for export and deduction.
- `is_active BOOLEAN`: Default `TRUE`. Controls item membership exclusivity.

Constraints/indexes:
- `Unique(batch_id, payment_event_id)`.
- `Index(batch_id)`.
- `Index(payment_event_id)`.
- **Partial Unique Index (Item Exclusivity):**
  ```sql
  CREATE UNIQUE INDEX idx_batch_items_active
  ON batch_items (payment_event_id)
  WHERE is_active = TRUE;
  ```
  *Item Mechanics:* An unbatched payment event is eligible for inclusion in a new draft. When placed into a draft batch, `is_active = TRUE` reserves it.
  - If the batch is **applied**: `is_active` remains `TRUE` permanently (or items are flagged permanently acknowledged), ensuring this payment event is never deducted twice.
  - If the batch is **abandoned**: `is_active` is updated to `FALSE`, releasing the payment event back to available status for future batches.

---

## 9. Notification tables

### 9.1 email_recipients

Purpose: Maintained recipient list for automated weekly procurement digests.

Fields:
- `id UUID`: PK.
- `display_name VARCHAR(200)`: Recipient full name or role label.
- `email VARCHAR(255)`: Normalized (lowercased, trimmed) valid email address.
- `active BOOLEAN`: Default `TRUE`.
- `created_at TIMESTAMPTZ`.
- `updated_at TIMESTAMPTZ`.
- `deactivated_at TIMESTAMPTZ`: Nullable deactivation timestamp.

Constraints/indexes:
- `Unique(email)`.
- Single clean `email` column replaces redundant duplicate fields (`normalized_email` and `version` dropped). Deletion is soft (`active = FALSE`).

### 9.2 weekly_notification_runs

Purpose: One campaign record per completed weekly Friday reporting period.

Fields:
- `id UUID`: PK.
- `period_key DATE`: Local Friday ending date, unique per week.
- `period_start TIMESTAMPTZ`: Inclusive start of reporting period.
- `period_end TIMESTAMPTZ`: Exclusive end of reporting period (Friday cutoff).
- `business_timezone VARCHAR(80)`: Configured schedule timezone (e.g. `Africa/Addis_Ababa`).
- `cutoff_local TIME`: Configured cutoff time (e.g. `14:00:00`).
- `created_at TIMESTAMPTZ`.

Constraints/indexes:
- `Unique(period_key)`: Prevents duplicate campaigns for the same week.
- `period_end > period_start`.

### 9.3 email_deliveries

Purpose: Delivery outcome ledger per recipient for each weekly notification run.

Fields:
- `id UUID`: PK.
- `run_id UUID`: FK weekly_notification_runs.
- `recipient_id UUID`: FK email_recipients.
- `status VARCHAR(20)`: `queued`, `sending`, `sent`, `failed`.
- `attempt_count INTEGER`: Default 0.
- `last_attempt_at TIMESTAMPTZ`: Nullable.
- `accepted_at TIMESTAMPTZ`: Timestamp when SMTP/service accepted delivery. Nullable.
- `error_message TEXT`: Sanitized failure message if delivery failed.
- `created_at TIMESTAMPTZ`.
- `updated_at TIMESTAMPTZ`.

Constraints/indexes:
- `Unique(run_id, recipient_id)`.
- `Index(status, run_id)`.
- Over-engineered lease tokens and heavy nested JSON logs are removed in favor of concise, actionable delivery status and timestamp logging.

---

## 10. Settings

### 10.1 settings

Purpose: Singleton application configuration table storing operational defaults and live system parameters.

Fields:
- `id SMALLINT`: PK, constrained by `CHECK (id = 1)`.
- `business_timezone VARCHAR(80)`: Default `'Africa/Addis_Ababa'`.
- `weekly_cutoff_local TIME`: Default `'14:00:00'`.
- `default_currency CHAR(3)`: Single universal default currency across all forms and request entries, default `'ETB'`.
- `finance_currency CHAR(3)`: Accounting deduction and batch currency, fixed `'USD'`.
- `fx_max_age_days INTEGER`: Default `31` days (aligned with monthly InforEuro schedule).
- `pending_overdue_hours INTEGER`: Default `72` hours. Used for pending overdue alerting.
- `notification_catchup_hours INTEGER`: Default `72` hours. Maximum lookback window for background workers to dispatch un-sent weekly digests following server restarts.
- `application_base_url TEXT`: Base URL for email deep-links.
- `data_revision BIGINT`: Monotonic transaction counter incremented on meaningful domain changes to coordinate reactive cache invalidation.
- `updated_at TIMESTAMPTZ`.

Constraints:
- Single row only (`id = 1`).
- Legacy CBE buying type options and redundant settings tables (`app_defaults`) are unified into this single clean table.
- Secrets, passwords, and SMTP credentials reside strictly in environment variables, never in the database.

---

## 11. Exact relationship inventory

- `project_budgets` 1 -> 0..many `budget_lines`.
- `project_budgets` 1 -> 0..many `batch_projects`.
- `budget_lines` 1 -> 0..many `requests`.
- `budget_lines` 1 -> 0..many `payment_events` (historical line snapshot).
- `areas` 1 -> 0..many `requests`.
- `peoples` 1 -> 0..many `requests` (via `requester_id` and `focal_point_id`).
- `suppliers` 1 -> 0..many `requests` (via `supplier_id`).
- `requests` 1 -> 1..many `request_state_events`.
- `requests` 1 -> 0..1 `payment_events`.
- `currencies` 1 -> 0..many `payment_events` (via rate month and currency lookup).
- `financial_batches` 1 -> 1..many `batch_projects`.
- `financial_batches` 1 -> 1..many `batch_items`.
- `payment_events` 1 -> 0..many `batch_items`.
- `weekly_notification_runs` 1 -> 0..many `email_deliveries`.
- `email_recipients` 1 -> 0..many `email_deliveries`.

Integrity rules:
- All foreign keys enforce `ON DELETE RESTRICT`. Cascading deletes are strictly disallowed to prevent accidental data loss in financial and audit chains.

---

## 12. Enforcement matrix

### 12.1 Declarative constraints (PK / FK / UNIQUE / CHECK)

- Primary keys: UUIDs generated via `gen_random_uuid()` (or application-provided UUIDv7).
- Foreign keys: Direct physical constraints with `ON DELETE RESTRICT`.
- Reference formats: Validated by CHECK constraints and partial indexes.
- Batch draft reservation: Enforced declaratively by PostgreSQL partial unique index on `batch_projects(project_budget_id) WHERE is_active = TRUE`.
- Item draft uniqueness: Enforced declaratively by PostgreSQL partial unique index on `batch_items(payment_event_id) WHERE is_active = TRUE`.
- Email uniqueness: Unique lowercased recipient index.
- Precision & Non-negativity: Enforced via `CHECK (amount > 0)` and standardized numeric definitions.

### 12.2 Transactional rules & triggers

- **1:1 Paid Request to Payment Event:** Transition to `paid` occurs in the same transaction that inserts into `payment_events`.
- **Status State Machine:** `request_state_events` records transitions; invalid sequence pairs roll back.
- **Pending Metrics Integrity:** Exit from `pending` computes and writes `actual_days`, `delay_days`, and `is_delayed` directly into `request_state_events`.
- **Batch Application:** Updating a batch to `applied` sets `is_active = FALSE` on `batch_projects`, records `applied_at`, and finalizes the deductions.

---

## 13. Concurrency and locking

1. **Deterministic Lock Order:**
   When transactions require multiple resources, locks are acquired in consistent order:
   1. `financial_batches` (if modifying an existing batch)
   2. `project_budgets` (ordered by ID ascending)
   3. `requests` (ordered by ID ascending)
   4. `settings` (`data_revision` counter incremented last)

2. **Partial Index Draft Reservations:**
   Eliminates heavy table-level locking. If User A starts a draft batch with Project X, PostgreSQL partial index locks Project X from being included in User B's draft. If User A abandons the draft, `is_active` becomes `FALSE` and Project X is immediately available.

---

## 14. Query contracts and metrics

### 14.1 Active vs Completed Overdue Metrics

1. **Active Overdue Requests (Tracker View):**
   ```sql
   SELECT *
   FROM requests
   WHERE status = 'pending'
     AND CURRENT_DATE > due_date;
   ```
   Overdue age is dynamically calculated as `CURRENT_DATE - due_date`.

2. **Historical Completed Cycle Time (Insights View):**
   ```sql
   SELECT
     request_id,
     actual_days,
     delay_days,
     is_delayed,
     occurred_at
   FROM request_state_events
   WHERE from_status = 'pending'
     AND to_status IN ('paid', 'cancelled');
   ```
   Uses immutable recorded metrics; guarantees historical reports remain stable across years.

### 14.2 Financial Updates & Checkpoints

- **Available Balance:** Sum of `budget_lines.allocated_amount` minus `batch_items.usd_amount` from `applied` batches.
- **Unbatched Spend:** Sum of `payment_events.usd_value` where `id NOT IN (SELECT payment_event_id FROM batch_items WHERE is_active = TRUE)`.
- **Draft Reserved Spend:** Sum of `batch_items.usd_amount` where `batch_id` is in `draft` status.

---

## 15. Migrations and deployment

- **Target Database:** PostgreSQL 15+ (production target) while maintaining SQLite development compatibility.
- **Seed Data:** Deterministic UUID fixtures, standard InforEuro monthly rate sets, predefined ACTED Ethiopia areas (`MEK`, `SAM`, `SHR`, `ADD`), and initial `settings` row (`ETB` default currency).
- **Zero Secrets:** All database passwords, SMTP credentials, and tokens are passed strictly via environment variables.

---

## 16. Summary of design refinements

1. **Clean Normalization:**
   - Comma-separated `project_codes` replaced by `batch_projects` and `batch_items`.
   - Distinct `areas` table separates geographical locations from financial projects.
   - Distinct `peoples` table unifies requester and focal point identities.
2. **Simplified Concurrency:**
   - 4-table checkpoint complexity replaced by 3 clean tables with partial unique index draft locking.
3. **Robust Currencies & Precisions:**
   - Single InforEuro `currencies` table replaces old CBE structures.
   - Default currency unified as `ETB` across the application.
   - Request invoice amounts typed as `NUMERIC(18,2)`; rates as `NUMERIC(24,10)`; snapshot conversions as `NUMERIC(28,8)`.
4. **Streamlined Notifications & Settings:**
   - Redundant email normalization and JSON log bloat eliminated.
   - Single consolidated `settings` table with clear catchup and revision semantics.
