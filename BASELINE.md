# Baseline

Snapshot of the runnable SQLite app at the repo root before the test-suite edits. The nested `expense_tracker/` tree is not the app under test.

Recorded on 9 November 2026 with R 4.6.1 and testthat 3.3.2.

## Current tests

`Rscript -e "testthat::test_dir('tests/testthat')"` fails before any test runs.

`tests/testthat/helper-context.R` and every `test-*.R` file contain unresolved Git conflict markers. Parsing stops at `helper-context.R` line 1 (`unexpected input <<`). `scripts/test.R` is a comment-only placeholder. There is no passing suite.

## Clock and weekly report

`weekly_window_active()` in `R/features/requests_service.R` returns `TRUE` on every call. It does not read the clock. The banner in `output$weekly_report_banner` (`R/features/requests_modals.R`) is therefore visible on every day.

`weekly_data()` keeps expenses whose `created` date falls from Monday of the current week through `Sys.Date()`. The weekly table uses DataTables `pageLength = 10`. A row click opens the request dialog with `read_only = TRUE`, so the footer is only Back.

The tests will still require Africa/Addis_Ababa, Friday 13:00 inclusive through Saturday 17:00 exclusive. That predicate is not what the app does today.

There is no injectable clock. "Now" is `Sys.time()` / `Sys.Date()`. `db_stamp()` and `now()` in `R/core/db.R` stamp inserts with the real wall clock.

## Roles

One shared login in `R/core/config.R`: user `acted`, password `logapp`. There are no per-role status rights. Every signed-in session can run the same workflow.

## Status

Stored values are `under_review`, `pending`, `paid`, and `cancelled`. Legacy text is mapped on read: `waiting` to `under_review`, `under validation` to `pending`, `rejected` to `cancelled`.

- A new request is saved as `under_review`.
- Proceed moves `under_review` to `pending`, stores supplier, PO reason, PO date, and the PO reference.
- Pay moves `pending` to `paid`, stores `payment_date`, and calls `reload_expenses()` so the same session does not wait for the one-second database poll.
- Cancel moves `under_review` or `pending` to `cancelled`. It requires a reason. Paid rows cannot be cancelled.
- Paid and cancelled rows cannot be edited. The edit form has no status input. Save writes the previous status.

Status chips and KPI cards only filter the register through `request_table_status()`. They do not write a request's status. The status filter is applied in `output$table_requests`, not inside the debounced `base()` filter.

## Fields shown at each step

The new-request form collects Description, Requested by, Project code, Budget line, Department, Area, Quantity, Unit, Amount, Currency, and Scenario. Optional fields are Submitted by, Country, Order date, Framework agreement, and Remarks.

The read-only detail shows Amount and its currency only in the header (`ETB 10,000.00`). It does not show Scenario. Remarks appear only when a remark is stored. Pending and paid add PO reason and PO date. Paid also adds Payment date. Order Form and Purchase Order references are always in the dialog.

The edit form includes Amount and Currency. It does not include Scenario.

Proceed asks only for Supplier, PO reason, and PO date. Pay asks only for Payment date and a confirmation checkbox. Cancel asks only for a reason. Those three dialogs do not repeat Amount or the other request fields.

## Filters

Requests sidebar: status chips, project, budget line, request period, and a search box. Advanced filters are week, requested by, and submitted by. Budget-line choices come from the selected project codes. Clearing the project clears the budget-line selection. `request_filters` is debounced by 300ms. The session opens on the current month. Clear (`input$reset`) sets the month to `""` (All months) and clears the other request filters. An empty month does not drop rows by date, including rows dated after today.

Insights uses a separate filter set and `input$insights_clear`. Clearing it sets month, project, department, and scenario to empty.

## Pagination

| Surface | Size today |
| --- | --- |
| Requests register (`output$table_requests`) | 8 |
| Weekly report table | 10 |
| Financial Updates projects | default 10, choices 10 / 25 / 50; previous, next, and a five-page number window |
| Manage projects | 8 |
| Manage suppliers | 8 |
| Manage people | 10 |
| Manage notification emails | 10 |
| Insights Days and budget-line previews | first 5, then Show all |

Financial Updates search resets the page to 1. Changing the finance page size also resets to 1. Select-all and paging are client-side on the project cards.

## Select all

Finance Select all checks every project row that matches the search box, including rows on other pages. Those rows stay in the DOM. Rows hidden by the search are not selected. The button label count is the number of selected project codes. The same click again clears that selection.

## Batch and recorded expenses

`budget_lines` stores `project_id` and `budget_line_code` only. There is no budget amount and no remaining balance.

A paid request with an empty `deducted_at` appears under Financial Updates. Creating a batch asks for a name:

- An empty name shows a warning and inserts nothing.
- A name longer than 80 characters shows a warning and inserts nothing.
- A duplicate name is allowed.

The batch stores the selected projects' undeducted paid request ids. Confirm Applied sets `expenses.deducted_at` and `projects.last_deducted_at` once and moves the batch to Applied. Confirming again is not offered for an applied batch. Reject moves the batch to Rejected and does not set `deducted_at`, so the projects return to the unpaid list.

## Currency

The navbar input is `global_cur`. `active_currency()` defaults to ETB. Choices are ETB, USD, EUR, and GBP.

`RATES` in `R/core/config.R` is units per 1 USD: ETB 155, USD 1, EUR 0.92, GBP 0.79. Each loaded expense gains ETB, USD, EUR, and GBP columns: the original amount when the currencies match, otherwise `usd * RATES[[k]]`.

The requests table prints the original `amount` and `currency`, not the active currency. Insights sums the active-currency column. Finance converts with `fin_to_display_minor()`. That uses this month's EuroInfo rows in `fx` when `currency` and `etb` are present. Otherwise it uses ETB per unit: USD 155, EUR 155/0.92, GBP 155/0.79, ETB 1.

## Days to complete

For paid rows in the selected Insights month, `actual = payment_date - order_date`. Expected days come from `SCENARIO_DAYS`: A 7, B 14, C 21, D 45, E 60, G 60. On time means `actual <= expected`. Average delay is the mean of `actual - expected`. A negative delay is early. The on-time percentage is on-time rows divided by rows that have both dates.

## Manage validation

Adding a project, budget line, or supplier rejects an empty value and a case-insensitive duplicate with `showNotification(..., type = "warning")`. The same empty and duplicate checks on project and supplier edit are warnings. Nothing is inserted when the check fails. A successful add is written to SQLite and is available to the new-request form, the request filters, and Financial Updates after the catalog reloads.

## Modals

`R/core/components.R` blurs the page while a Shiny modal is open. `body.modal-open:has(.modal.show)` sets `filter: blur(4px)` on the requests page, dashboard, finance page, manage layout, navbar, and footer. `.modal-backdrop.show` sets `backdrop-filter: blur(5px)`. The `.modal` element is not in that selector, so the open dialog stays sharp.

Dialogs: new request, request details, edit, cancel, proceed, pay confirmation, see-more, weekly-row details, batch name, confirm applied, reject batch, share, and manage edits for project, supplier, person, and email. Notifications are not dialogs.

## App edits after this snapshot

These are the only application changes made for the suite. They are not part of the behavior above.

- `app_now()` and `app_today()` in `R/core/config.R`. `TRACKER_NOW`, when set, is read as an Africa/Addis_Ababa time. When unset, both functions use the real clock. Date defaults, the weekly date range, Insights "today", and the EuroInfo month use `app_today()`. `now()` and `db_stamp()` still use the real clock so insert stamps stay unique. `weekly_window_active()` still returns `TRUE`.
- Page size 10 on the requests register, Manage projects, Manage suppliers, and the Insights preview lists. Weekly and Financial Updates were already 10.
- Empty and duplicate project, budget line, and supplier checks in Manage, including edit of a project or supplier, use `type = "error"` and still save nothing.

## Database

The app uses one SQLite file. `TRACKER_DB_PATH` must already exist or startup stops. If it is unset, the file is `tracker.db` beside `app.R`. `R/core/db.R` calls `init_db()` when it is sourced. Tests must not open `tracker.db`.
