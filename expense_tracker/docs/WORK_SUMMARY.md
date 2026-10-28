# Session handoff

This file is a session handoff so a later session can continue without the chat. It is not a replacement specification. Product behavior stays in `docs/PRODUCT_SPECIFICATION.md`. Data rules stay in `docs/DATABASE_DESIGN.md`. File ownership stays in `docs/FILE_STRUCTURE.md`. The release checklist stays in `docs/RELEASE_CHECKLIST.md`. Do not treat this handoff as authority to add states, change the Friday membership cutoff, relabel CBE, or mark T01–T54 passed.

Reviewed 28 October 2026. No commit was made. These UI files were left untouched because another agent was editing them: `app.R`, `www/app.css`, `R/core/theme.R`, `R/features/requests_page.R`, `R/features/weekly_report.R`, `R/features/requests_modals.R`, `R/features/finance_page.R`, `R/features/manage_page.R`, `R/features/insights_page.R`, and `R/features/requests_repo.R`.

## What the app is

Logistics Procurement Tracker is one R Shiny application on PostgreSQL. Logistics staff register a procurement request, move it through review and a purchase order, and mark it paid inside the tracker. Marking paid records the original request total. It does not send or execute a payment. Finance prepares frozen USD batches for whole budgets so someone can update an external workbook. A separate worker emails a link to the completed Friday-to-Friday request report. It does not attach the report.

The written product documents describe four tabs and no login: Requests, Financial Updates, Manage, and Insights. The shell currently open in `app.R` is a three-tab screen (Requests, Financial updates, Manage) behind one shared username and password from `APP_SHARED_USERNAME` and `APP_SHARED_PASSWORD`. Insights code exists and is not mounted in that shell. That conflict is unresolved. Do not pick a winner in code until the product owner does.

## File map

```text
app.R                         Shiny shell
www/app.css                   Styles
R/core/bootstrap.R            Load order and dependency builders
R/core/config.R               Environment and database URL parsing
R/core/db.R                   Connections, transactions, settings
R/core/contracts.R            Validation, states, decimal text helpers
R/core/periods.R              Monday calendar week and Friday reporting week
R/core/theme.R                Theme
R/core/components.R           Shared UI pieces
R/core/workbook.R             Excel writer
R/features/*_page.R           Page UI and server modules
R/features/*_service.R        Validation and workflow
R/features/*_repo.R           Parameterized SQL
R/features/requests_modals.R  Request dialogs
R/features/requests_export.R  Request and weekly Excel rows
R/features/finance_export.R   Finance Excel rows
R/features/weekly_report.R    Weekly section inside Requests
R/features/fx_service.R       Rate publication and paid-quote selection
R/integrations/cbe_client.R   Commercial Bank of Ethiopia HTTP adapter
R/integrations/email_client.R SMTP link delivery
sql/001_schema.sql            Tables
sql/002_integrity.sql         Guards, payment valuation, locks
sql/003_request_operations.sql Request create, edit, approve, cancel, mark paid
sql/004_finance_operations.sql Batch preview, prepare, apply, abandon
sql/005_notification_operations.sql Email queue
scripts/migrate.R             Apply sql/001–005; refuses comment-only files
scripts/seed_sample.R         Explicit fictional seed
scripts/notify_weekly.R       Weekly worker entry
scripts/test.R                Parse R files, then testthat
scripts/verify_release.R      Writes an artifact; does not tick the checklist
tests/testthat/               Pure tests; database tests skip without TEST_DB_*
```

`docs/FILE_STRUCTURE.md` still describes an earlier scaffold in its status section. The files above now contain working logic, not comment-only placeholders.

## What is implemented

Request states are only `under_review`, `pending`, `paid`, and `cancelled`. Creation goes to under review. Under review can go to pending or cancelled. Pending can go to paid or cancelled. Paid and cancelled are terminal. The pairs are enforced in `request_state_events` and `requests_guard()` in `sql/002_integrity.sql`, and the writes go through `request_create`, `request_approve`, `request_cancel`, and `request_mark_paid` in `sql/003_request_operations.sql`. There is no generic status-update function.

One request stores one `budget_line_id`. The budget is the parent of that line. `amount` is the whole PO total. Quantity and unit are an optional pair and are not multiplied into the amount.

References are built in `request_create` and `request_approve`. Patterns are `OF/{budget}/{line}/{country}/{area}/{department}/{dd-mm-yyyy}` and `PO/OF/.../{reason}/{dd-mm-yyyy}`. `format_reference` leaves sequence 1 unsuffixed, appends `/002` style suffixes below 1000, and does not truncate 1000 or above. `next_reference_sequence` takes the max existing sequence, including cancelled rows, under an advisory lock, so a cancelled reference is not reused.

`request_mark_paid` copies `requests.amount` into `payment_events.original_amount`. `fill_payment_values()` in `sql/002_integrity.sql` converts that amount with the published rate set. Nothing in that path calls a bank or payment provider. Paid and cancelled rows cannot be updated (`requests_guard`).

Finance batches are USD only (`checkpoint_batches.currency` must be USD). Callers pass budget ids, not line ids. `finance_prepare` freezes budgets, lines, and payment items in one transaction and seals the batch. A partial unique index allows one open reservation per budget. Later payments are not inserted into a sealed batch; abandonment is the only release of item membership. Display and Excel negate the stored positive USD once (`negate_money` in the finance page and `finance_export.R`). The service keeps the stored sign positive.

Reporting-week membership uses `registered_at` with `start <= registered_at < end` in `requests_repo_weekly`. The cutoff comes from `app_settings.weekly_cutoff_local`, default `14:00` in `Africa/Addis_Ababa`, via `reporting_period_for_instant` in `R/core/periods.R`. The ordinary This week filter is Monday–Sunday on the request date and is a different helper.

Exchange rates are stored as CBE or `SYNTHETIC_TEST_FIXTURE`. The fixture label is not CBE. `fx_refresh` publishes source `CBE`. There is no InforEuro client.

Overdue is not a fifth status. `pending_overdue_hours` is nullable. `pending_is_overdue` in `R/features/insights_service.R` returns NA when the threshold is unset, and otherwise compares unrounded elapsed hours strictly greater than the threshold.

`notifications_run` sends one plain-text link per recipient, plus the period that belongs to that delivery. It writes notification runs and deliveries only. It does not write requests, payments, or checkpoints.

Money that crosses SQL is cast to text (`::text`, `to_char`) and checked with the decimal helpers in `R/core/contracts.R`. Authoritative conversion stays in PostgreSQL `NUMERIC`.

## Logic fixes in this session

- `two_decimal_text` in `R/core/contracts.R`, used by `finance_summary` and `finance_write_current`. A line with no payments came back from PostgreSQL as `"0"`. The old check against `"0.00"` treated that line as a nonzero USD difference, so a zero budget could receive a batch checkbox. `"0"` and `"0.0"` are now `"0.00"` before that check. Display still negates the positive total once.
- `issued_reference_unchanged` in `requests_edit` (`R/features/requests_service.R`). Budget line, area, department, and reference date define the issued OF reference. The edit SQL never updated them, so a changed form was saved as success and the old reference remained. The service now rejects that change and tells the user to cancel and register a replacement.
- `pending_is_overdue` and `insights_repo_pending`. Overdue used `age_days * 24` after the age had been rounded to a tenth of a day, so a short pending time could cross an hour threshold it had not actually reached. The flag now uses elapsed hours from `pending_entered_at` and is strict (`hours > threshold`). An unset threshold stays NA. Overdue is still not a status.
- `rate_decimal_text` in `R/integrations/cbe_client.R`. CBE rates were passed through `as.numeric` and `formatC`, which rewrote long decimals (for example `123456789.1234567896` became `123456789.1234567910`). Decimal text is padded or rounded to 10 places without binary floating point. JSON number tokens are quoted before `jsonlite` parses them. The source name remains CBE.
- `notification_claim_delivery` in `sql/005_notification_operations.sql`, read by `notifications_repo_claim` and used by `notifications_run`. The claimed row's period label was always empty, and the email body used the latest completed period even when the link belonged to an older queued delivery. The body now uses the claimed run's own start, end, and timezone.

`sql/005_notification_operations.sql` was edited in place. `scripts/migrate.R` stores a checksum and will refuse to change a database that already applied an older copy of that file. It will not mark the file applied by itself. This session did not run migrate and did not insert a `schema_migrations` row. A database that already has the old checksum needs a new migration file, which was not added because that is a schema-process decision.

## Known defects still open

- Requests UI, in `R/features/requests_page.R` and `R/features/requests_modals.R`, always prints "Deadline not configured" for a pending row and does not read `pending_overdue_hours`. The product rule is: when the threshold is unset, show pending age and "Overdue threshold not configured", and do not invent a countdown or an SLA. Those files were not edited.
- `R/core/workbook.R` turns two-decimal money columns into R doubles before writing Excel. `R/features/insights_page.R` uses `as.numeric` on spending totals for the chart. Neither file was changed. Stored SQL values stay `NUMERIC`.
- `sql/` has no `GRANT` or `REVOKE`. `scripts/migrate.R` says separate app and worker role grants were not executed. The worker code does not write requests, payments, or checkpoints, but a shared login could. Adding least-privilege roles is a database-administration change and was not done.
- Pending edits still silently keep some locked fields (quantity, scenario, and similar) because `request_edit` does not update them. Classification changes are now rejected. A full pending field-by-field rejection was not added.
- `docs/FILE_STRUCTURE.md` still says the tree is a non-runnable scaffold. That sentence is stale. This handoff does not rewrite that document.
- T01–T54 in `docs/RELEASE_CHECKLIST.md` are not passed. Do not tick them from this session.

## Product conflicts to resolve before more behavior changes

Do not silently change the database rule to match the newer UI wording.

| Topic | Product documents | Current UI request / shell |
| --- | --- | --- |
| Tabs | Four: Requests, Financial Updates, Manage, Insights. README matches this. | `app.R` shows three tabs. Insights is implemented in `R/features/insights_page.R` and is not in the shell. |
| Sign-in | No login. No profile menu. A contact name is not an identity. | `app.R` is one shared username and password gate. There is no account directory. |
| Exchange rates | Commercial Bank of Ethiopia, `transactionalBuying`, not a monthly InforEuro period. Synthetic fixtures must not be labelled CBE. | The newer UI request asks for InforEuro monthly periods. The code is still CBE in `R/integrations/cbe_client.R` and `fx_service.R`. It was not relabelled. |
| Friday | Report membership is Friday 14:00 Africa/Addis_Ababa to the next Friday 14:00, half-open, using `registered_at`. Default column `weekly_cutoff_local` is `14:00`. | `weekly_window_open` in `R/features/weekly_report.R` highlights the section from Friday 13:00 until Monday 13:00. That is a visibility window. Membership was left at Friday 14:00. |

The calendar filter week remains Monday–Sunday and must stay separate from the reporting week.

## How to run

Rscript for this machine:

`C:\Users\Staff\AppData\Local\Programs\R\R-4.6.1\bin\Rscript.exe`

From the project folder:

```text
Rscript scripts/migrate.R
Rscript scripts/seed_sample.R
Rscript -e "shiny::runApp('.', host = '127.0.0.1', launch.browser = TRUE)"
Rscript scripts/notify_weekly.R
Rscript scripts/test.R
```

Copy `.env.example` to `.env` or `.Renviron.example` to `.Renviron` and set the database URL there. Do not commit those files and do not print their secrets. Opening the app does not create tables and does not load sample rows. `scripts/seed_sample.R` is the explicit seed. Its rate source is `SYNTHETIC_TEST_FIXTURE`.

PostgreSQL was not installed via winget because the download returned HTTP 403. No local PostgreSQL service was available for migrate, seed, or browser checks in this session.

## Tests that actually ran

On 28 October 2026, after the logic fixes above, this command was run:

`C:\Users\Staff\AppData\Local\Programs\R\R-4.6.1\bin\Rscript.exe scripts/test.R`

Exit code 0. It parsed 44 R files. The pure tests completed. Four database tests skipped because `TEST_DB_*` is not set (finance, manage, notifications, requests). They were not passed.

Not run: migrate, seed, browser use of Requests / Financial updates / Manage / Insights, email delivery, a live CBE fetch, a hosted database, and a worker role that cannot update requests. `scripts/verify_release.R` was not run. T01–T54 stay unchecked.

## Next concrete steps

1. Decide the four conflicts in the table above. Until then, leave Friday membership at 14:00 and leave the rate source named CBE.
2. Install PostgreSQL by a path other than the winget download that returned HTTP 403, or point `.env` at Neon. Run `scripts/migrate.R`, then `scripts/seed_sample.R`. If migrate stops because `sql/005_notification_operations.sql` was already applied with a different checksum, add a new migration for `notification_claim_delivery` instead of editing the applied file or hand-marking it applied.
3. After the UI editor finishes, point the requests table and detail modal at `pending_overdue_hours`. Unset means age plus "Overdue threshold not configured". Set means the `pending_is_overdue` flag, not a new state.
4. Add separate app and worker database roles. The worker role must not be able to write requests, payments, or checkpoints.
5. Run `scripts/test.R` with `TEST_DB_*` pointed at a disposable database, then exercise the shell in a browser. Only then consider checklist items, one id at a time, with the command that was actually run.
