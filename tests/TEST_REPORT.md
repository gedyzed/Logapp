# Expense Tracker Test Report

## Summary
The test suite for the runnable SQLite expense tracker application (`app.R`, `R/`) has been refactored into two deterministic, high-speed test layers:
1. **Unit Tests (`tests/testthat/unit/`)**: Pure function logic, business calculations, rate conversions, and input validation without browser overhead.
2. **Integration Tests (`tests/testthat/integration/`)**: Headless `shiny::testServer` sessions running against an isolated, temporary SQLite database fixture with pre-seeded test data. Never opens production `tracker.db`.

Browser-dependent requirements (`[2]`, `[3b]`, `[5]`, `[6]`) are verified at the integration layer for programmatic properties (handlers, inputs, modal classes, outputs) and supplemented by manual verification steps in [`MANUAL_CHECKLIST.md`](../MANUAL_CHECKLIST.md).

---

## Requirement Verification Matrix

| Req ID | Description | Status | Layer / Method | Notes |
|---|---|---|---|---|
| **[1]** | Weekly report active window bounds & banner visibility (Fri 13:00 to Sat 17:00 EAT) | **KNOWN BUG** | Unit & Integration | Baseline `weekly_window_active()` returns `TRUE` unconditionally; fails baseline assertion as documented in `BASELINE.md`. |
| **[2]** | Weekly report read-only detail view and absence of edit/submit/delete controls | **PARTIAL (manual)** | Integration + Manual | Integration tests assert absence of edit/delete controls in weekly modal builder; visual verification in `MANUAL_CHECKLIST.md` Step 2. |
| **[3]** | Request lifecycle transitions (`under_review` &rarr; `pending` &rarr; `paid` &rarr; `cancelled`), SQLite status, and KPI sync | **PASS** | Integration (`testServer`) | Verified transitions, SQLite persistence, and KPI counts. |
| **[3b]** | Form field visibility and persistence per step (new, edit, proceed, pay, cancel, read-only) | **PARTIAL (manual)** | Unit & Integration + Manual | Modal DOM structures and inputs tested; visual layout verified via `MANUAL_CHECKLIST.md` Step 3. |
| **[4]** | Non-editable terminal states (`paid` and `cancelled` requests cannot be edited; edit form preserves old status) | **PASS** | Unit & Integration | Guard clauses and SQL updates verify status immutability. |
| **[5]** | Request details display & status chips/KPI card filtering via `request_table_status()` | **PARTIAL (manual)** | Integration + Manual | Filter reactivity and row data verified; physical row click and chip selection verified via `MANUAL_CHECKLIST.md` Step 4. |
| **[6]** | Modal dialog styling and background blur (`filter: blur(4px)`, sharp modal) | **PARTIAL (manual)** | Integration + Manual | Integration asserts modal CSS classes and blur rules in `components.R`; visual verification in `MANUAL_CHECKLIST.md` Step 1. |
| **[7]** | Requests table filtering (month, specific month, all months, project, search, requester) | **PASS** | Integration (`testServer`) | Filter query logic matches expected subsets from test fixture. |
| **[8]** | Project and budget line cascading filter dependency | **PASS** | Integration (`testServer`) | Budget line select choices dynamically update based on selected project code. |
| **[9]** | Clear filters resets inputs to session default | **PASS** | Integration (`testServer`) | Filter reset restores original row count and default month. |
| **[10]** | Standard page size of 10 rows across all tables (Requests, Weekly, Finance, Manage, Insights) | **PASS** | Unit & Integration | Standardized `pageLength = 10` across all DT tables and preview blocks. |
| **[11]** | Finance "Select all" selects all matching search query across pages | **PASS** | Integration (`testServer`) | Selection includes filtered projects across all pages. |
| **[12]** | Finance "Select all" button count equals number of selected project codes | **PASS** | Integration (`testServer`) | Counter dynamically matches number of selected codes. |
| **[13]** | Payment moves request from pending to paid and triggers `reload_expenses()` | **PASS** | Integration (`testServer`) | Request moves to paid, expenses reactive reloads, appears in Financial Updates pool. |
| **[14]** | Financial batches creation validation (empty name warning, >80 chars warning, duplicate allowed) | **PASS** | Integration (`testServer`) | Empty and long names rejected with notification; duplicates permitted. |
| **[15]** | Batch creation associates exact undeducted paid requests | **PASS** | Unit & Integration | Stored `request_ids` in SQLite batch row match candidate IDs. |
| **[16]** | Confirm Applied sets `expenses.deducted_at` & `projects.last_deducted_at`; Reject does not | **PASS** | Integration (`testServer`) | Batch confirmation stamps deduction timestamps; reject moves batch without stamping expenses. |
| **[17]** | Batch Excel export generates valid XLSX with required columns and formatted values | **PASS** | Unit & Integration | Verified via `readxl::read_excel` on generated temporary XLSX files. |
| **[18]** | Insights filters operate independently from requests sidebar filters | **PASS** | Integration (`testServer`) | Changing request sidebar filters does not alter Insights metrics or filters. |
| **[19]** | Insights calculation of Days to complete (actual vs expected) and average delay by scenario | **PASS** | Unit & Integration | SCENARIO_DAYS calculation, on-time rate, and average delay calculations verified against SQLite fixture. |
| **[20]** | Insights KPI aggregation (total spend in active currency, pending counts, completion rate) | **PASS** | Integration (`testServer`) | Aggregations reflect filtered data subsets. |
| **[21]** | Manage validation: Add and edit project, budget line, and supplier reject empty or duplicate with `type = "error"` | **PASS** | Integration (`testServer`) | Error notifications emitted, zero invalid rows inserted into database. |
| **[22]** | Multi-currency conversions and rate calculations (RATES fallback and EuroInfo monthly rates) | **KNOWN BUG** | Unit & Integration | Request rows calculate `usd * RATES[[k]]`; baseline rate discrepancy noted on currency conversions. |

---

## Test Execution Performance
- **Unit Suite Duration**: ~3.6s (61 passed assertions)
- **Integration Suite Duration**: ~76.1s (130 passed assertions across 7 test files, each file completing in `< 20s`)
- **Total Assertions**: 191 passed assertions, 0 unexpected failures, 0 regressions.
- **Runner**: `Rscript run_tests.R` (or `Rscript scripts/test.R`)

