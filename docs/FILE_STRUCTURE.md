# Repository baseline and collaboration guide

Version 1.0 | 27 October 2026 | Proposed implementation organization

## 1. Recommendation

Use a small, feature-prefixed R application with only three source subfolders: `R/core`, `R/features`, and `R/integrations`. Keep one UI/server module per main page, one service and one repository per substantial feature, and only split the request modals and exports where ownership/size justify it.

This is not a framework migration, an R package conversion or a folder per database table. It is a pragmatic structure for two developers and their agents under the current deadline.

The 19 database tables do NOT require 19 UI modules or 19 repository files. Requests, finance, reference management and notifications each own a coherent set of tables/operations.

### Alignment and authority

- Product behavior: `docs/PRODUCT_SPECIFICATION.md`.
- Data/integrity: `docs/DATABASE_DESIGN.md`.
- File responsibilities/ownership: this document.
- Approved visual references: `docs/ui/`.

This guide does not approve the unresolved assumptions in product section 23. Do not implement an assumption as confirmed merely because its file exists.

### Baseline status

The supplied scaffold contains comments describing responsibilities, not application implementation. It is NOT runnable, NOT a passing test suite and NOT a deployable release. SQL placeholders are not migrations that may be marked applied. The existing working app must remain recoverable through a Git tag/branch before replacing its layout.

## 2. Complete initial inventory

```text
expense-tracker/
|-- app.R
|-- R/
|   |-- core/
|   |   |-- bootstrap.R
|   |   |-- config.R
|   |   |-- db.R
|   |   |-- contracts.R
|   |   |-- periods.R
|   |   |-- theme.R
|   |   |-- components.R
|   |   `-- workbook.R
|   |-- features/
|   |   |-- requests_page.R
|   |   |-- requests_modals.R
|   |   |-- requests_service.R
|   |   |-- requests_repo.R
|   |   |-- requests_export.R
|   |   |-- weekly_report.R
|   |   |-- finance_page.R
|   |   |-- finance_service.R
|   |   |-- finance_repo.R
|   |   |-- finance_export.R
|   |   |-- manage_page.R
|   |   |-- manage_service.R
|   |   |-- manage_repo.R
|   |   |-- insights_page.R
|   |   |-- insights_service.R
|   |   |-- insights_repo.R
|   |   |-- fx_service.R
|   |   |-- notifications_service.R
|   |   `-- notifications_repo.R
|   `-- integrations/
|       |-- cbe_client.R
|       `-- email_client.R
|-- sql/
|   |-- 001_schema.sql
|   |-- 002_integrity.sql
|   |-- 003_request_operations.sql
|   |-- 004_finance_operations.sql
|   `-- 005_notification_operations.sql
|-- scripts/
|   |-- migrate.R
|   |-- seed_sample.R
|   |-- notify_weekly.R
|   |-- test.R
|   `-- verify_release.R
|-- tests/
|   `-- testthat/
|       |-- helper-context.R
|       |-- test-requests.R
|       |-- test-finance.R
|       |-- test-manage.R
|       |-- test-weekly.R
|       |-- test-notifications.R
|       |-- test-fx.R
|       |-- test-insights.R
|       `-- test-ui.R
|-- www/
|   `-- app.css
|-- README.md
|-- AGENTS.md
|-- .gitignore
|-- .Renviron.example
`-- docs/
    |-- PRODUCT_SPECIFICATION.md
    |-- DATABASE_DESIGN.md
    |-- FILE_STRUCTURE.md
    |-- RELEASE_CHECKLIST.md
    |-- ui/
    |   `-- README.md
    |-- DATABASE_ERD.png
    `-- DATABASE_ERD.mmd
```

### Files deliberately generated later

Do not hand-write a fake `renv.lock`. After the first shared dependency setup, commit the real lockfile and generated `renv/activate.R`, `renv/settings.json` and root `.Rprofile`; do not commit the installed library. Pin the R version in README/CI as well.

Add `.github/workflows/ci.yml` and `.github/workflows/weekly-notification.yml` only when their real commands and secrets are configured. A comment-only workflow is not a valid automation baseline. The notification workflow invokes `scripts/notify_weekly.R`; it does not contain report-generation logic. Set the schedule using the agreed timezone conversion and keep a manual trigger.

Add a `.github/CODEOWNERS` file only after the actual repository usernames are supplied. Do not commit placeholder reviewers and assume review protection exists. Add a `www/` logo asset only if approved branding is available. No empty assets/data/logs/output folders are needed now.

## 3. Naming and file boundaries

- `*_page.R`: page UI plus server module together, using namespaced inputs/outputs.
- `*_modals.R`: feature-owned modal UI and event wiring, not business rules.
- `*_service.R`: validation, workflow orchestration and presentation-ready results.
- `*_repo.R`: parameterized queries and adapters to controlled database operations.
- `*_export.R`: feature-specific workbook columns/metadata, not independent financial queries.
- `*_client.R`: external-provider transport only.
- `core/`: small genuinely shared foundations, not a dumping ground.

Keep page UI and server together so one developer owns the whole interaction. Do not create separate UI/server folders or one file per button/field/table. Split a file later only when two people genuinely need independent ownership or it becomes difficult to reason about.

There are 30 application R files including app.R, plus five command-line entry scripts. This is intentionally more than a four-file prototype but much smaller than a framework-generated enterprise layout. Most files are narrow; the core loader prevents them becoming hidden global side effects.

## 4. Detailed file-by-file contract

Each entry gives the owner, expected code shape, dependencies and prohibited responsibilities. Owners are a proposed allocation, not a claim about either developer's experience.

### `app.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Compose the four-tab Shiny application and session wiring.
- **Expected kind of code:** Thin entry point: call the loader, initialize app dependencies, assemble UI and call page module servers.
- **Connects to:** Calls bootstrap.R; wires requests_page, finance_page, manage_page and insights_page with explicit dependencies and refresh callbacks.
- **Boundary:** No SQL, business transitions, sample data, SMTP, large CSS strings or automatic migrations. Target roughly 100-150 lines, not a hard correctness limit.

### `R/core/bootstrap.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Load definitions explicitly and build dependency bundles for app, worker and command-line scripts.
- **Expected kind of code:** Explicit ordered source manifest and factory functions; definitions load without side effects. Separate app/worker/test dependency builders.
- **Connects to:** Loads core, integrations and feature definitions; returns config, connection provider, repositories, services and clock. Used by app.R and scripts.
- **Boundary:** Never recursively source arbitrary files or run migrations, seeds, network calls or email on source. Worker initialization must not require CBE or Shiny UI initialization.

### `R/core/config.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Read and validate environment configuration and nonsecret app_settings.
- **Expected kind of code:** Configuration parsing, required-variable checks, constants and schema-version expectation.
- **Connects to:** Used by bootstrap, db, periods, FX and notification service. Database settings are read through db.R; credentials come only from environment/secrets.
- **Boundary:** One canonical currency/state list. Separate app/worker/migration/test credentials. Do not require email secrets to launch the app. No hidden demo/live switch.

### `R/core/db.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Manage PostgreSQL connections, transactions, settings reads, health/schema checks and revision reads.
- **Expected kind of code:** DBI/RPostgres connection or bounded pool helpers, with-connection/with-transaction wrappers, schema/settings/revision queries, safe error translation.
- **Connects to:** All feature repositories depend on it; migration/seed/test scripts use appropriate connection roles.
- **Boundary:** No request/finance query collection here. One connection per transaction; do not reacquire another connection halfway through an atomic operation. No migration execution at app startup.

### `R/core/contracts.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Define cross-feature data and error contracts before parallel work.
- **Expected kind of code:** Typed result/error constructors, zero-row prototypes, filter normalization, decimal/UUID serialization conventions and boundary validators.
- **Connects to:** Used by services, repositories, modules and tests. Exact public signatures are recorded in docs/FILE_STRUCTURE.md and implemented here only where genuinely shared.
- **Boundary:** No miscellaneous helper dumping ground, no SQL or UI. Authoritative money arithmetic stays in PostgreSQL NUMERIC; return decimal text/minor-unit-safe representations, not silently rounded R doubles.

### `R/core/periods.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Provide one shared calendar/report-period definition.
- **Expected kind of code:** Pure timezone-aware functions for Monday-Sunday filters, Friday-Friday completed periods, validated report deep links and an injectable clock.
- **Connects to:** Used by requests_service, weekly_report, insights_service and notifications_service; tested independently.
- **Boundary:** No Shiny inputs, database writes or scheduler/provider calls. Friday report cutoff and Monday calendar week are distinct named operations.

### `R/core/theme.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Define the shared bslib theme and semantic design tokens.
- **Expected kind of code:** Theme factory, light/dark token mapping and shared chart palette.
- **Connects to:** Used by app.R, components.R and chart rendering; pairs with www/app.css.
- **Boundary:** No page-specific business rules. One shell-level theme switch; do not add independent switches to every page.

### `R/core/components.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Provide small reusable presentation elements.
- **Expected kind of code:** UI builders for filter panels, state badges, validation summaries, confirmation footers, loading/empty/error states and amount labels.
- **Connects to:** Used by all page/modal files; receives data/labels as arguments and theme tokens from theme.R.
- **Boundary:** No SQL, observers with hidden writes, business transitions or invented fields. Feature-specific tables/forms stay in their feature file.

### `R/core/workbook.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Share minimal Excel-writing mechanics without centralizing feature logic.
- **Expected kind of code:** Workbook/worksheet creation, metadata/header styling, numeric-cell formatting, safe text-cell writing and temporary download-file handling.
- **Connects to:** Called by requests_export and finance_export.
- **Boundary:** No independent queries, filter decisions, currency conversion or checkpoint application. Treat user text as text, not spreadsheet formulas. No exported files committed to Git.

### `R/features/requests_page.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Implement Requests tab layout and reactive behavior.
- **Expected kind of code:** Namespaced UI/server module: state cards, compact filters, newest-first table, pagination, export handler and modal launch.
- **Connects to:** Calls requests_service, requests_modals, weekly_report, requests_export and shared components; returns refresh/action signals to app shell.
- **Boundary:** No direct SQL or lifecycle writes. State-card counts exclude selected-state filter. Table remains immediately below cards.

### `R/features/requests_modals.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Implement create/details/edit/approve/Paid/cancel dialogs.
- **Expected kind of code:** Namespaced modal builders and event handlers, form-state preservation, field-level errors and explicit confirmations.
- **Connects to:** Owned by requests_page; calls requests_service, manage_service choice readers and fx_service rate status; uses components.
- **Boundary:** Do not register duplicate observers every time a modal opens. No supplier in initial form. Use database ID/version, not row number. Paid lock and business rules belong to services/DB too.

### `R/features/requests_service.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Own request validation and lifecycle orchestration.
- **Expected kind of code:** Payload validation, idempotency handling, create/edit/approve/cancel/mark-paid operations, reference-base formatting and scoped list/count/detail view models.
- **Connects to:** Calls requests_repo, manage_service read contracts, fx_service and periods; uses db transaction context and contracts.
- **Boundary:** No Shiny input/output or modal code. Atomic Paid writes are one DB operation. Reference uniqueness/locking and final decimal valuation are enforced in SQL functions, not duplicated in R.

### `R/features/requests_repo.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Own request-related PostgreSQL reads and write-function adapters.
- **Expected kind of code:** Parameterized list/count/detail/weekly queries, typed mapping, and calls to approved request SQL operations.
- **Connects to:** Called by requests_service and weekly-report service path; uses db.R and contracts.
- **Boundary:** No UI, provider HTTP or separate business policy. Do not create payment events through ad hoc inserts outside the controlled Paid operation.

### `R/features/requests_export.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Export request-table and weekly-report results.
- **Expected kind of code:** Feature-specific column selection, scope metadata, original/display currency labels and download preparation.
- **Connects to:** Receives approved view models from requests_service; uses workbook.R.
- **Boundary:** Do not requery with different filters or export only the visible pagination page. Weekly export does not change checkpoint state.

### `R/features/weekly_report.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Present the temporary weekly section and report modal within Requests.
- **Expected kind of code:** Small nested module/helper for completed-period notice, once-per-session prompt, validated deep-link selection, report table and export.
- **Connects to:** Called by requests_page; calls periods, requests_service weekly query and requests_export.
- **Boundary:** Not a fifth tab, no stored report attachment, no email sending. Membership uses the approved reporting timestamp; sidebar filters do not leak into it.

### `R/features/finance_page.R`

- **Owner:** Friend.
- **Purpose / expected output:** Implement Financial Updates UI.
- **Expected kind of code:** Namespaced module: budget cards, nested line rows, selection, preview modal, current batch cards, apply/abandon dialogs and history.
- **Connects to:** Calls finance_service and finance_export; uses components and refresh callback.
- **Boundary:** Budget checkboxes only; no line checkpoint date. USD batch currency. No financial SQL or calculation in render functions.

### `R/features/finance_service.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own budget-level checkpoint workflow and finance view models.
- **Expected kind of code:** Budget/line summaries, eligibility, server-held preview tokens, prepare/apply/abandon orchestration and exact batch/history retrieval.
- **Connects to:** Calls finance_repo and contracts; receives clock/settings; returns frozen export view model.
- **Boundary:** No request transitions or external workbook writes. Preview changed -> reconfirm. One draft per budget. Do not release applied payment membership.

### `R/features/finance_repo.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own financial queries and controlled checkpoint operation adapters.
- **Expected kind of code:** Parameterized available/reserved/applied queries, last budget checkpoint lookup, exact batch membership reads and SQL function calls.
- **Connects to:** Called by finance_service; uses db.R and contracts; consumes immutable payment_events created by Requests.
- **Boundary:** No imports of requests_page/modals/service. Exclude released abandoned memberships from current sums. Avoid history joins that multiply payments.

### `R/features/finance_export.R`

- **Owner:** Friend.
- **Purpose / expected output:** Produce the agreed concise finance workbook.
- **Expected kind of code:** One-sheet export mapping: budget code, line code, negative USD difference; minimal batch/date/status metadata.
- **Connects to:** Receives frozen batch/current-view result from finance_service; uses workbook.R.
- **Boundary:** Nonzero lines only; exact saved totals. No raw rate/payment worksheets by default, no current-rate recalculation, no apply-on-download.

### `R/features/manage_page.R`

- **Owner:** Friend.
- **Purpose / expected output:** Implement reference-list and email-recipient management screens.
- **Expected kind of code:** Namespaced forms/tables for budgets, dependent lines, areas, role-specific contacts and recipients; active/inactive actions; rate status/refresh.
- **Connects to:** Calls manage_service and fx_service; uses shared components.
- **Boundary:** No credential editor. Removing email deactivates it. No hard deletion of referenced business/history rows.

### `R/features/manage_service.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own reference-data and recipient validation and management rules.
- **Expected kind of code:** List-choice readers, create/update/deactivate/reactivate operations, normalization and conflict messages.
- **Connects to:** Calls manage_repo; exposes read contracts to Requests/Insights; FX refresh delegated to fx_service.
- **Boundary:** No SQL in page; do not deactivate budgets with open/unapplied/reserved work. Contact identity is not authentication.

### `R/features/manage_repo.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own reference-data and recipient persistence.
- **Expected kind of code:** Parameterized master-data/recipient reads and writes under transaction/trigger protections; typed choices and version checks.
- **Connects to:** Used by manage_service; uses db.R. Notification worker reads recipients through notifications_repo, not through Manage UI.
- **Boundary:** Do not mutate payment/checkpoint history. Active-state checks and parent-budget lock ordering must match DB design.

### `R/features/insights_page.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Implement chart selector, dynamic filter panel and chart area.
- **Expected kind of code:** Namespaced UI/server module, chart-specific controls, theme-aware plotting and tabular/empty/error presentation.
- **Connects to:** Calls insights_service and components/theme.
- **Boundary:** No hidden filters carried across chart types. No SQL or ad hoc currency revaluation in chart code.

### `R/features/insights_service.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Define chart metrics, cohorts and chart-ready results.
- **Expected kind of code:** Arrival/spending/Pending-age metric definitions, filter validation, as-of time and overdue-threshold handling.
- **Connects to:** Calls insights_repo, periods and contracts; returns values plus date-basis/currency/as-of metadata.
- **Boundary:** Pending waiting time only. No invented SLA when threshold NULL. Separate current Pending ages from completed/cancelled durations.

### `R/features/insights_repo.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Own read-only analytical queries.
- **Expected kind of code:** Parameterized aggregates over registered_at, payment_date, frozen payment values and Pending entry/exit timestamps.
- **Connects to:** Called by insights_service; uses db.R. Reads domain tables directly through approved queries, not other pages.
- **Boundary:** Read-only; no checkpoint updates, current-rate revaluation or duplicate sums through batch history joins.

### `R/features/fx_service.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Own rate validation, caching, publication and rate-set selection.
- **Expected kind of code:** Validate complete four-currency sets, select approved immutable sets, freshness checks, explicit refresh and publish orchestration. Rate-table SQL adapters remain local to this file for the baseline.
- **Connects to:** Calls cbe_client and db.R; used by Requests/Manage and seed/test adapters.
- **Boundary:** No notification-worker dependency. HTTP happens before short publication transaction. Financial conversion is PostgreSQL NUMERIC; do not add an R floating-point financial engine. Split fx_repo only if this file becomes hard to maintain.

### `R/features/notifications_service.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own weekly link-only notification orchestration.
- **Expected kind of code:** Determine completed period, queue active recipients, claim leases, send separately, bounded retry and accepted/failed/uncertain/suppressed outcome handling.
- **Connects to:** Calls periods, notifications_repo and email_client; invoked only by notify_weekly.R worker script.
- **Boundary:** No report generation, attachment, Shiny session dependency, rate fetch or financial writes. Missing auth -> Not configured, never false Sent.

### `R/features/notifications_repo.R`

- **Owner:** Friend.
- **Purpose / expected output:** Own notification campaign/delivery persistence and worker recipient reads.
- **Expected kind of code:** Parameterized recipient reads, unique run/delivery upserts, short claim/finalize transactions and sanitized attempt logs.
- **Connects to:** Called by notifications_service; uses worker-role connection from db.R and controlled notification SQL operations.
- **Boundary:** No long transaction around provider send. Worker role cannot write requests/payments/checkpoints. Address removal rechecked before send.

### `R/integrations/cbe_client.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Adapt the external CBE HTTP API.
- **Expected kind of code:** Request construction, timeout/retry limits, object/array parsing and normalized raw response/error.
- **Connects to:** Called only by fx_service; receives endpoint/date/subtype and HTTP adapter for tests.
- **Boundary:** No database writes, Shiny UI or invented fallback rates. Keep source/date/subtype explicit.

### `R/integrations/email_client.R`

- **Owner:** Friend.
- **Purpose / expected output:** Adapt the selected authenticated email provider.
- **Expected kind of code:** Build/send a simple text/HTML link message, apply configured auth/timeouts and return provider acceptance or classified failure.
- **Connects to:** Called by notifications_service; receives sender/recipient/link and secrets through config, not browser input.
- **Boundary:** No DB, scheduler, recipient-list management or report attachment. Never log credentials; acceptance is not proof of inbox delivery.

### `sql/001_schema.sql`

- **Owner:** Integrator.
- **Purpose / expected output:** Create all 19 tables, primary/foreign/composite keys, ordinary checks and indexes.
- **Expected kind of code:** Versioned DDL for the approved dictionary, including partial unique reservation/membership indexes.
- **Connects to:** Read by migrate.R; must match DATABASE_DESIGN.md.
- **Boundary:** No sample records, credentials, DROP/recreate-on-startup or mutable budget balances. First migration metadata table is bootstrapped safely by migration runner.

### `sql/002_integrity.sql`

- **Owner:** Integrator.
- **Purpose / expected output:** Install cross-row consistency and immutability protections.
- **Expected kind of code:** Trigger functions/deferred constraint triggers for state/payment consistency, complete rates, sealed batches, sums and immutable history.
- **Connects to:** Depends on 001; protects all repositories and operation functions.
- **Boundary:** CHECK constraints cannot replace cross-row enforcement. Validators must not introduce reverse lock order. No committed unsealed batch.

### `sql/003_request_operations.sql`

- **Owner:** Dagim; integrator reviews.
- **Purpose / expected output:** Implement atomic request writes and reference allocation.
- **Expected kind of code:** Allowlisted PostgreSQL functions for create/edit/approve/cancel/mark-paid, idempotency, version checks, NUMERIC valuation, audit and revision.
- **Connects to:** Called by requests_repo; depends on schema/integrity.
- **Boundary:** One transaction per business action. Preserve request->budget lock order. Never execute CBE HTTP in SQL or inside transaction.

### `sql/004_finance_operations.sql`

- **Owner:** Friend; integrator reviews.
- **Purpose / expected output:** Implement exact budget-level prepare/apply/abandon operations.
- **Expected kind of code:** Allowlisted functions with sorted budget locks, frozen membership/line totals, sealing and correct release semantics.
- **Connects to:** Called by finance_repo; depends on 001/002 and immutable payments.
- **Boundary:** No line-level selection/checkpoints, no timestamp sweep, no releasing applied items, no silent partial success across selected budgets.

### `sql/005_notification_operations.sql`

- **Owner:** Friend; integrator reviews.
- **Purpose / expected output:** Implement safe worker queue/claim/finalize operations.
- **Expected kind of code:** Allowlisted functions for run/delivery idempotency, leases and outcome transitions.
- **Connects to:** Called by notifications_repo; no dependence on request/finance operations.
- **Boundary:** No provider calls in SQL. Separate worker grants. Permission provisioning occurs explicitly after function creation, not through committed passwords.

### `scripts/migrate.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Apply pending migrations explicitly and provision approved grants.
- **Expected kind of code:** CLI entry: migration credentials, advisory migration lock, checksums, ordered files, transaction per migration and fail-fast.
- **Connects to:** Loads non-UI bootstrap/db; reads sql/*.sql and schema_migrations.
- **Boundary:** Refuse comment-only/unimplemented migrations; never mark them applied. No app/worker credentials used for schema changes.

### `scripts/seed_sample.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Insert fictional acceptance fixtures idempotently.
- **Expected kind of code:** Explicit CLI seed using stable fixture keys/operation IDs and the same domain write contracts.
- **Connects to:** Calls services/approved DB functions; includes clearly synthetic rate fixture when requested.
- **Boundary:** No automatic startup seed; no trigger bypass; refuse shared reset without explicit safety checks.

### `scripts/notify_weekly.R`

- **Owner:** Friend.
- **Purpose / expected output:** Run one independent notification-worker invocation.
- **Expected kind of code:** Thin CLI entry: worker bootstrap, config check, notifications_service invocation, sanitized summary and exit status.
- **Connects to:** Scheduler invokes it; uses same periods.R as app.
- **Boundary:** No Shiny startup, migrations, seed, rate fetch or generated report file.

### `scripts/test.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Run the agreed automated test suites.
- **Expected kind of code:** CLI entry for pure/service/DB/module/browser tests with explicit suite selection and meaningful exit codes.
- **Connects to:** Loads tests/testthat helpers and feature code.
- **Boundary:** Isolated test DB only; missing integration prerequisites -> clear skip/failure per suite, never fake pass.

### `scripts/verify_release.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Record release checks and deployment smoke-test evidence.
- **Expected kind of code:** Orchestrate configured tests/schema/config checks; write a timestamped report of pass/fail/skip/not-run.
- **Connects to:** Uses test.R, schema health and release checklist; operator supplies hosted test evidence.
- **Boundary:** Does not prove a hosted workflow merely by parsing files; no destructive production test.

### `tests/testthat/helper-context.R`

- **Owner:** Integrator.
- **Purpose / expected output:** Build isolated DB/schema fixtures, injected clock, fake HTTP/email clients and test dependency bundles; no automatic shared DB connection.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-requests.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Request creation/validation/idempotency/references/transitions/Paid locking; specification T03-T15.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-finance.R`

- **Owner:** Friend.
- **Purpose / expected output:** Budget reservations, frozen drafts, apply/abandon, exact membership, concurrency, simple export and rounding; T27-T41/T53-T54.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-manage.R`

- **Owner:** Friend.
- **Purpose / expected output:** Reference hierarchy, normalization, active/inactive restrictions and recipient add/remove/reactivation.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-weekly.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Friday-Friday versus Monday-Sunday, cutoff boundary, deep link, all-state scope and backdated entry; T16-T20/T26.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-notifications.R`

- **Owner:** Friend.
- **Purpose / expected output:** DB recipients, idempotent queue, missing credentials, retry/uncertain/suppressed outcomes and no attachments; T21-T25.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-fx.R`

- **Owner:** Dagim.
- **Purpose / expected output:** CBE response validation, immutable publication, stale/outage policy, exact conversion fixtures and missing entry-rate handling; T42-T44.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-insights.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Arrival/payment date bases, Pending-only timing, nullable overdue threshold and filter isolation; T45-T47.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `tests/testthat/test-ui.R`

- **Owner:** Dagim.
- **Purpose / expected output:** Empty states, module reactivity, theme/form preservation and browser workflow checks; T01/T15/T48/T50.
- **Expected kind of code:** testthat cases, testServer/module checks or explicitly configured browser tests as appropriate.
- **Connects to:** Uses helper-context.R and the feature public contracts.
- **Boundary:** Tests must be independent; use two actual DB connections for concurrency cases. Manual accessibility/hosted/restore checks remain in release checklist.

### `www/app.css`

- **Owner:** Dagim.
- **Purpose / expected output:** Implement shared responsive styling and semantic light/dark tokens.
- **Expected kind of code:** Scoped CSS for shell, sidebar, cards, tables, modals, focus and theme variables.
- **Connects to:** Pairs with theme.R/components and page-specific class names.
- **Boundary:** No business rules, external secrets, hard-coded light-only surfaces or per-page duplicate palettes. Add JavaScript only if a real requirement cannot be met by Shiny/bslib.

## 5. Root/documentation files

### README.md

Practical setup/runbook: prerequisites, pinned R/PostgreSQL/package versions, environment variable names, migration/seed/test commands, local run, worker invocation, deployment allowlist, backup/restore, troubleshooting and known limitations. Do not duplicate the full business specification. Until commands are implemented, label them planned rather than tell users they work.

### AGENTS.md

Short mandatory entry instructions for coding agents: read the three authoritative docs, stay within assigned files/requirement IDs, no schema/API changes without review, no secrets, no new tabs/states, no mode switches, no fabricated test claims. Link to the documents rather than copy their rules into a competing specification.

### .Renviron.example

Variable names and empty/example values only: app DB connection/TLS, worker DB connection, migration/test credentials, sender/provider configuration and optional app URL override. Use separate connection builders by process role. Actual `.Renviron` and credentials are ignored. A sender address alone is not sufficient authentication.

### .gitignore

Ignore local secrets, R session/history files, package libraries/caches, SQLite files from the old prototype, generated exports/logs, test/browser artifacts and local deployment account metadata. Keep approved UI assets and fictional fixture source. Never ignore the entire docs folder or the real dependency lockfile.

### docs/PRODUCT_SPECIFICATION.md and docs/DATABASE_DESIGN.md

Copy the latest reviewed versions from the previous delivery unchanged. Any later behavior/schema change updates these first or in the same reviewed PR. Keep assumptions visible until approved. No silent business-rule changes in this file-structure guide.

### docs/FILE_STRUCTURE.md

This guide. Update it if a file moves, a dependency changes or ownership transfers. It is the location contract, not another product requirements document.

### docs/RELEASE_CHECKLIST.md

Record the 54 acceptance tests from the product spec with actual pass/fail/skip/not-run evidence, tester, date, environment, issue reference and final acceptance. Include manual browser, two-session, hosted persistence and backup/restore checks. A placeholder checklist is not evidence of tests passing.

### docs/ui/README.md

Shared design link, approved frame filenames, theme/token decisions and review status. Later put approved light/dark frame exports here. Mark generated mockups as references, not implemented screenshots. Do not commit dozens of unreviewed alternatives.

### docs/DATABASE_ERD.png and docs/DATABASE_ERD.mmd

Visual overview and editable source from the reviewed database package. Update alongside schema design changes; the detailed dictionary remains authoritative for fields/constraints. SVG/DOT are optional source assets, not needed for app runtime.

## 6. One concrete database-write policy

Choose the following baseline to remove ambiguity:

- Request lifecycle, reference allocation, Paid creation and checkpoint transitions use controlled PostgreSQL functions in migrations 003/004.
- R services validate user payloads, select immutable rate sets, manage previews and translate errors. They do not reimplement SQL reference allocation, decimal valuation or reservation mechanics.
- Repositories call the functions using parameters on the same transaction connection. They do not insert the same financial facts through an alternative path.
- Integrity triggers from 002 protect the data even if an application bug attempts an invalid write.
- Master-data/recipient edits and rate publication may use parameterized repository DML with the required triggers, version checks and transaction/lock rules. These writes do not bypass the integrity contract.
- Notification queue/lease/finalize functions use 005 and the restricted worker role.
- Audit and revision writes occur within the owning business transaction. There is no public generic audit API that lets a page claim an action succeeded independently.

This is an implementation allocation of the existing database design, not a new business feature. Migration owner must review the exact privileges and safe function search paths.

## 7. Dependency direction and load order

### Runtime direction

```text
app.R
  -> bootstrap / config / connection provider
  -> four page modules
       -> feature services
            -> feature repositories
                 -> PostgreSQL queries / controlled operations
       -> feature exports -> workbook helper

fx_service -> cbe_client
notifications_service -> email_client
app and worker -> the SAME periods helper
```

No page imports another page's input/output objects. A service does not read input$. A repository does not call showModal. A provider client does not write PostgreSQL. Finance and Insights read immutable payment facts without calling the Requests page.

### Explicit source order

1. contracts, config, db, periods, theme, components, workbook.
2. provider clients.
3. feature repositories.
4. fx_service and other feature services.
5. export builders.
6. weekly report and request modals.
7. page modules.
8. app composition initializes dependencies and sessions.

Source files define functions only. Referencing a later-loaded function inside a function body is fine; executing it at source time is not. The worker loader can omit UI/theme/workbook modules and never initializes Shiny. The app loader does not require SMTP secrets. Avoid recursive source-everything patterns and global connections created merely by loading a file.

### Settings, clock and refresh

- Pass a dependency bundle explicitly: connection provider, service functions, settings reader and clock.
- Snapshot settings consistently for each action/preview; refresh or invalidate when settings_version changes.
- The app shell owns a session-local refresh signal and polls data_revision for other-session changes.
- Modules receive read-only refresh dependencies and a successful-write callback. They do not overwrite each other's reactive values.
- Do not hold a connection for the entire lifetime of every browser session. Acquire/release around work; one acquired connection stays with one transaction.
- Shared refresh updates lists/counts, not an unsaved form's entered values.

## 8. Public interface contracts to freeze before parallel coding

Agree exact argument/column names in the first baseline PR. Suggested contract names below are names/signatures to implement, not existing functions.

### Requests

- `requests_list(filters, state, page, page_size)` -> rows, total_count, scope metadata.
- `requests_counts(filters)` -> four counts, without selected-state filtering.
- `requests_get(id)` -> detail, version, allowed_actions.
- `requests_create(payload, submission_key)` -> id, reference, version.
- `requests_edit(id, expected_version, payload)`.
- `requests_approve(id, expected_version, payload, operation_key)`.
- `requests_mark_paid(id, expected_version, payload, operation_key)`.
- `requests_cancel(id, expected_version, reason, operation_key)`.
- `requests_weekly(period)` -> all-state rows and explicit period metadata.

### Finance

- `finance_summary()` -> all active budgets/lines, available/reserved/unapplied values, eligibility and budget checkpoint.
- `finance_preview(budget_ids)` -> exact event-set fingerprint, totals, settings version and expiry.
- `finance_prepare(preview, creation_key)` -> sealed draft.
- `finance_get_batch(id)` -> frozen budgets/lines/membership/status/version.
- `finance_apply(id, expected_version, confirmed, note, operation_key)`.
- `finance_abandon(id, expected_version, confirmed, note, operation_key)`.
- `finance_history(filters)` -> applied/abandoned history.

### Manage, FX, Insights, worker

- Manage exposes stable choice readers and versioned create/update/deactivate/reactivate operations.
- FX exposes rate status, refresh, published-set lookup and eligibility, not a second R financial calculator.
- Insights accepts chart key plus validated chart-specific filters and returns data/date basis/as-of/currency/threshold metadata.
- Worker accepts current clock/config and returns counts of queued/sent/failed/uncertain/suppressed, not a report file.

### Result rules

- One success/error shape across services; typed errors match product specification section 16.
- Empty results are typed zero-row tables, not NULL with surprise columns.
- UUIDs are strings. Business dates are Date values. Instants are timezone-aware timestamps.
- Monetary values remain exact decimal representations across DB/service boundaries; format for display at the edge. Never send rounded browser totals back as authoritative amounts.
- Preview fingerprints include exact sorted event IDs and valuation strings; tokens are server-held or signed, not trusted browser hashes.
- Only shared contracts go in contracts.R. Feature-specific validation stays with its service.

## 9. Complete workflow examples

### Register a request

Requests page opens requests_modals -> form calls requests_service -> validates reference choices via manage_service -> requests_repo invokes request-create SQL operation -> database issues reference, inserts request/state/audit and revision atomically -> service returns saved ID -> modal closes -> refresh signal updates cards/table. Requests export uses the same filter scope, not current page rows only.

### Mark Paid

Modal gets a valid immutable rate set through fx_service before the write transaction -> requests_service submits ID/version/payment date/confirmation -> requests_repo calls Paid operation -> SQL locks request then budget, freezes NUMERIC valuations, inserts one payment event and sets Paid/state/audit/revision -> commit -> Financial Updates/Insights refresh through revision. No page directly inserts a payment.

### Prepare/apply a batch

Finance page selects budget IDs -> finance_service preview -> user confirms -> finance_repo calls prepare operation -> SQL locks budgets in order, rechecks event set, creates/seals draft, reserves budgets/items and freezes nonzero line totals -> page shows Current batch. Export reads saved totals. Apply releases budget reservations but retains payment acknowledgment; new payments remain outside. Abandon releases both reservation types but retains history.

### Weekly report and notification

periods.R computes Friday-Friday boundaries -> weekly_report calls requests_service/requests_repo for the bounded all-state table. Independently, scheduler invokes notify_weekly.R -> notifications_service computes the same period -> notifications_repo reads active PostgreSQL recipients/queues claims -> email_client sends the owned-app link -> notifications_repo records acceptance/failure. No attachment, report generation or dependency on an active browser.

### Pending-time chart

Insights page selects Pending waiting time -> insights_service validates Pending-entry cohort/time/budget filters -> insights_repo reads pending_entered_at/exited_at -> service returns current ages/completed durations and nullable threshold -> page renders with shared theme. Under-review time is never added.

## 10. Proposed two-developer ownership

### Dagim: Requests and analysis slice

Own requests_page/modals/service/repo/export, weekly_report, insights_page/service/repo, fx_service, cbe_client, periods, theme/components/CSS, request/weekly/FX/Insights/UI tests, and request SQL operations 003 with integrator review.

### Friend: Finance and shared-data slice

Own finance_page/service/repo/export, manage_page/service/repo, notifications_service/repo, email_client, notification script, finance/Manage/notification tests, and finance/notification SQL operations 004/005.

### One integrator for shared foundations

Nominate one person, provisionally your friend, to own app.R, bootstrap/config/db/contracts/workbook, migrations 001/002, migration/seed/test/release scripts, dependency lockfile, deployment configuration and final merges. Dagim reviews shared UI/period changes. If that workload is too heavy, transfer a whole file explicitly rather than both editing it concurrently.

This is a proposed assignment; swap names if your strengths differ. The rule that matters is one current owner per file and one reviewer for critical shared changes.

### Files that must move together

- Request field change: request modal/service/repo + relevant migration + request tests + product/database docs.
- Finance workflow change: finance page/service/repo/export + operation migration + finance tests + docs.
- Recipient change: manage service/repo + notifications repo/service + both test sets; coordinate before merging.
- Rate policy change: FX service/client + Paid SQL operation + request/finance/FX tests + docs.
- Period change: periods + weekly/notification/Insights tests; never duplicate a new Friday calculation in the worker.
- Theme change: theme/components/CSS + affected page screenshots/tests; no private page palette.

## 11. Merge-conflict rules

1. Preserve current working app with a Git tag/branch before structural changes. Do not delete it without a recovery point.
2. One baseline PR introduces the folder map, current docs, shared contracts and placeholder status. Main is a structural baseline at this point, not a runnable release. If main must always run, keep this PR on an integration branch until the thin shell works.
3. After baseline merge, both developers update from the same commit before branching.
4. Use short feature branches and small PRs. Integrate at least every few focused hours.
5. Do not both edit app.R to register a module. Request one small integration change from its owner.
6. Do not both append SQL to the same migration. Each feature owns its operation file during initial construction; integrator owns schema/integrity.
7. Once a migration is applied to the shared database, never edit it. Reserve the next number centrally and add a new migration. Before the first shared application, draft files may be reviewed/amended together.
8. Agree public function names and returned columns first. Renaming them requires a contract PR before consumers change.
9. Do not reformat whole files, rename folders, update all dependencies or regenerate approved images inside an unrelated feature PR.
10. Never commit a mutable SQLite/PostgreSQL dump, secrets, real records or generated exports to solve collaboration.
11. Assign agents a file allowlist and requirement/test IDs. They may propose changes outside ownership but must not silently make them.
12. Git review cannot guarantee zero conflicts; this structure reduces shared editing and makes remaining conflicts local and reviewable.

## 12. First baseline PR and subsequent milestones

### Structural baseline

- Copy these paths/docs into the repository.
- Keep the current working version recoverable.
- Add per-file responsibility headers, not fake working functions.
- Nominate owners and freeze initial interfaces.
- Review product assumptions; do not mark them approved by merging the scaffold.

### Runnable shell baseline

- Implement loader/config/db health/schema checks and the four page module shells.
- Implement shared theme/components and actual dependency setup.
- Generate/commit the real renv files.
- A missing DB/config produces a clear startup message; no hidden SQLite fallback.
- CI can parse/lint and run pure tests, but must not report unimplemented integration tests as passing.

### First functional vertical slice

- Implement/apply real schema/integrity/request operations.
- Seed fictional reference records explicitly.
- Create -> open -> approve a request and verify persistence.
- Deploy a minimal app-only bundle and test PostgreSQL connectivity/restart persistence.

### Remaining slices

Paid/locking -> budget batches/export -> Manage/recipients -> weekly report/worker -> Insights/theme/browser tests -> release evidence.

## 13. Deployment boundaries

App deployment allowlist: app.R, required R source, www assets, and actual dependency metadata/activation files supported by the deployment workflow. No local secrets, tests, docs, sql migrations, worker scripts, DB dumps or generated exports in the public app bundle unless explicitly needed and reviewed.

The worker runs from the repository/CI environment with worker credentials and loads only its needed definitions. It does not run app.R. Migrations run separately with migration credentials before app rollout. Source files can be shared without initializing every integration.

The exact rsconnect manifest/lockfile handling is implemented and tested when the deployment configuration is added; this guide is not a claim that a generic file list alone deploys successfully.

## 14. What not to add now

- No framework rewrite, R package conversion, Docker/Kubernetes setup or second backend solely for neatness.
- No folder per table, button, chart or modal.
- No central 2,000-line repository.R/service.R/helpers.R.
- No separate report-generation microservice.
- No fifth tab, line checkboxes/checkpoint dates, automatic scenario logic or mode-dependent database routing.
- No fake lockfile, empty workflow YAML, dummy test pass, or SQL placeholder marked applied.

## 15. Review checklist for this structure

- Every file has one owner and a defined boundary.
- Every page has a service path; no page writes SQL.
- Finance and request repositories are separate, not one shared merge hotspot.
- One periods helper is shared by app/worker.
- One decimal/transaction policy protects money.
- Worker has a separate entry point/role and no report generation.
- All schema/operation changes have reviewed migrations.
- Existing specification assumptions remain visible.
- Structural scaffold is labelled non-runnable until implemented.

End of repository baseline guide.
