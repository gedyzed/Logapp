# Logistics Procurement Tracker
# Product specification and development instructions

**Version:** 1.0 review candidate  
**Prepared:** 27 October 2026  
**Audience:** both developers, reviewers, and coding/design agents  
**Companion:** [Database design](DATABASE_DESIGN.md)  
**Status:** consolidated specification for review, not a claim that the existing app implements it  
**Implementation target:** one R Shiny application, shared PostgreSQL database, independent scheduled notification worker

---

## Navigation

- [Purpose and boundaries](#1-product-purpose-and-boundaries)
- [Four-tab navigation](#2-navigation-and-shared-presentation)
- [Request lifecycle](#3-request-lifecycle)
- [Requests page](#4-requests-tab)
- [New Request and editing](#5-new-request-modal-and-editing)
- [Approval, Paid and cancellation](#6-approval-paid-and-cancellation-modals)
- [Friday-to-Friday weekly report](#7-weekly-report-inside-requests)
- [Email notification worker](#8-weekly-email-notification)
- [Financial Updates and batches](#9-financial-updates-tab-budget-level-checkpoint-model)
- [Manage](#10-manage-tab)
- [Insights and Pending time](#11-insights-tab)
- [Currency and rounding](#12-currency-valuation-and-rounding)
- [References](#13-references)
- [Architecture and transactions](#14-technical-architecture-and-boundaries)
- [Expected outputs](#17-expected-outputs-by-area)
- [Acceptance tests](#18-acceptance-tests)
- [Agent instructions](#20-agentdeveloper-instructions)
- [Review assumptions](#23-explicit-assumptions-requiring-review)

---

## 0. Read this first

This document replaces the earlier three-tab specification and the later four-tab drafts. It incorporates the user's final clarifications about budget-level checkpoints, USD batches, Friday-to-Friday reports, PostgreSQL email recipients, and Pending-only waiting time.

### 0.1 Requirement language

- **CONFIRMED** means explicitly requested/agreed by the user.
- **BASELINE ASSUMPTION** means an exact proposed implementation rule needed to make the system deterministic. It is not represented as an explicit user decision. All such rules are collected in section 23 for review.
- **MUST** means required behavior under this review candidate.
- **SHOULD** means a recommendation that may be changed through an explicit decision.
- **DEFERRED** means not in this release.

The user will review this specification and database design before detailed file structure, final UI generation, and implementation. Do not use this review candidate as authorization to silently expand scope.

### 0.2 Authority and change control

1. Approved requirements/decisions in this document govern behavior.
2. `DATABASE_DESIGN.md` governs data definitions and integrity.
3. Approved UI frames govern appearance, not business rules.
4. Existing generated code is an implementation reference, not a requirements authority.

If two sources conflict, stop and resolve the conflict in a small documented change. Do not let an agent decide a new state, relationship, currency rule, or checkpoint scope on its own.

Requirement IDs below are stable review/test references. A change must identify affected IDs, database effects, UI effects, and tests.

### 0.3 One application and sample records

There is one application. Fictional sample records are seeded into it for development and acceptance testing. There is no separate demo product, no hidden live/demo business behavior, and no permanent demo banner requirement.

A seed command MUST be explicit and idempotent. App startup MUST NOT recreate deleted samples or silently switch databases. Synthetic exchange-rate fixtures MUST carry a truthful source label in metadata; they must never be labelled CBE.

Automated tests use isolated temporary databases/schemas to avoid corrupting shared test records. That isolation does not introduce another product or user-facing mode.

---

## 1. Product purpose and boundaries

### 1.1 Purpose [CONFIRMED]

Logistics/admin staff register procurement requests, review them, progress approved procurement, mark requests Paid, and give finance exact paid-only deductions to apply to external budget records.

### 1.2 The application does not

- Execute payments or transfer money.
- Require external bank/payment verification before marking Paid.
- Store confidential budget ceilings, allocated totals, or remaining balances.
- Modify external finance workbooks automatically.
- Split one request across several budget lines.
- Support partial payments or multiple payments per request in this release.
- Calculate procurement scenarios automatically in this release.
- Include a paid correction/refund/reversal workflow in this release.
- Add a separate Project level above Budget.

App-level authentication/roles remain outside the agreed baseline. Hosting access restrictions and credentials are still necessary operational configuration; no login screen should be invented by an agent.

### 1.3 Core relationship [CONFIRMED]

**Budget -> Budget line -> Request.**

A request has exactly one budget line. The parent budget is derived from that line. The sample workbook's Project Code occupies the budget-code position in the reference pattern. The UI uses Budget consistently.

### 1.4 Amount meaning [CONFIRMED]

The entered amount is the entire request/PO total. Quantity and unit describe the purchase, for example 10 Days. They do not multiply the entered total. Marking Paid uses that same request amount; there is no separate actual-paid amount.

---

## 2. Navigation and shared presentation

NAV-01 [CONFIRMED]: exactly four main tabs, in this order:
1. **Requests**.
2. **Financial Updates**.
3. **Manage**.
4. **Insights** (proposed final label for the confirmed analytical tab).

NAV-02: weekly reports are a temporary section within Requests, not a fifth tab.

NAV-03: Requests is the landing tab. New Request and request details use modals, not a full-page creation flow or a details sidebar.

NAV-04: consistent faded-white, dark-grey and light-blue visual system, with red for cancellation/destructive actions. Light/dark modes must be complete across all tabs, modals, tables, dropdowns and charts.

NAV-05 [BASELINE ASSUMPTION]: use a consistent left filter sidebar in Requests and Insights. The exact top-versus-vertical main navigation placement remains a UI-layout decision for the next phase; it must not change the four-tab structure or reduce the working table to an unusable width.

NAV-06 [BASELINE ASSUMPTION]: primary actions use blue; cancellation uses red. Earlier green Proceed/Paid requests are superseded only if this palette assumption is accepted at UI review. Button text and confirmations must convey meaning independently of color.

NAV-07: no invented profile/account menu when authentication is not implemented. Do not imply that a selected contact name is a logged-in identity.

NAV-08: long references wrap or truncate with an accessible full-value view. Do not invent shortened PO numbers as a substitute for the real reference.

---

## 3. Request lifecycle

### 3.1 States [CONFIRMED]

| Internal code | Visible label | Meaning |
| --- | --- | --- |
| `under_review` | Under review | Registered and awaiting review |
| `pending` | Pending | Approved; procurement progressing |
| `paid` | Paid | Marked Paid in this system |
| `cancelled` | Cancelled | Cancelled before Paid |

No Waiting, Rejected, Fulfilled or additional state is introduced. Delivery/fulfilment is not an independent state in this release.

### 3.2 Allowed transitions

- Creation -> Under review.
- Under review -> Pending through Proceed with purchase order.
- Under review -> Cancelled.
- Pending -> Paid.
- Pending -> Cancelled.
- Paid and Cancelled are terminal in this release.

A generic editable state dropdown MUST NOT bypass transition requirements.

### 3.3 Atomic writes

Every successful creation, edit, approval, cancellation, or Paid transition must update its relevant records and audit/state event in one database transaction. Failure must leave the prior state intact. Database constraints and transaction locks are required in addition to UI validation. PostgreSQL supports foreign keys, unique constraints and row locks for these guarantees [S1, S2].

### 3.4 Retry and concurrency

- Each new-request form has a stable submission key reused when retrying the same save.
- Duplicate clicks must not create duplicate requests, PO references, payment events or batches.
- Edits/transitions submit the version loaded when the modal opened.
- A stale version is rejected with an instruction to reopen; no silent overwrite.
- Request IDs, not visible row positions, identify the record being acted on.
- Shared refresh must not overwrite unsaved form fields.

---

## 4. Requests tab

### 4.1 Layout [CONFIRMED]

REQ-01: prominent New Request button.

REQ-02: four large clickable state-count cards.

REQ-03: a request table immediately below the state cards. No large chart or report panel between the cards and the table.

REQ-04: compact sidebar filters, rather than a large horizontal filter area.

REQ-05: newest registered requests first, using `registered_at DESC, id DESC` as a deterministic ordering.

REQ-06: one click on a row or its reference opens the details modal. Keyboard activation must be possible. No select-row-then-Open requirement.

REQ-07: Excel export remains available.

### 4.2 Defaults and filters

REQ-08 [CONFIRMED]: Under review selected initially; This month is the default calendar scope; ETB is the default display currency.

REQ-09: sidebar contains period, budget, budget line, area, search and display currency. Additional requester/focal filters may be added only if they fit the approved compact design.

REQ-10: periods include This week, This month, All time and Custom. Last month is an optional convenience, not a release blocker.

REQ-11 [CONFIRMED]: ordinary This week filtering uses **Monday 00:00 through the next Monday 00:00**, in the business timezone. This is distinct from the Friday-to-Friday weekly-report period.

REQ-12 [BASELINE ASSUMPTION]: the ordinary date filter uses request submission date, a DATE field, with inclusive selected calendar dates. It is labelled Request date. Weekly report membership uses registration timestamp instead; see section 7.

REQ-13: selecting a budget limits line choices to that budget. An invalid previously selected line clears immediately. The server independently validates the relationship.

REQ-14: state-card counts reflect all current non-state filters, but do not apply the selected state to the other cards. Clicking a card changes the table state immediately.

REQ-15 [BASELINE ASSUMPTION]: filters apply immediately; no duplicate sidebar Status dropdown and no extra Apply Filters button.

REQ-16: Reset returns to Under review, This month, all budgets/lines/areas, empty search and ETB.

REQ-17: an All time affordance and/or older-open indicator prevents the monthly default from making older unfinished work undiscoverable.

### 4.3 Table contract

Visible baseline columns:
- Request reference, with PO reference below it when present.
- Description.
- Requester.
- Budget / budget line.
- Display amount and explicit display currency.
- State badge.
- Request date.

The original amount/currency remains available in details and export. A display selector set to ETB must not show unconverted USD values under an ETB heading. No user-controlled HTML is rendered unescaped.

Search covers references, description, requester and budget/line codes. Pagination is presentation only; export includes all matching rows.

### 4.4 Empty, loading and error behavior

- Empty database: explain how to add reference data and a request.
- No matches: show filters/reset guidance, not a disconnected session.
- Invalid date range: show a local validation message; do not silently use all records.
- Missing conversion: original amount remains visible with Conversion unavailable; never zero.
- Database/network error: show a recoverable message and retry path; preserve form state.
- Zero-row dropdown choices must have zero labels and zero values. Do not override base R functions globally to conceal mismatches.

---

## 5. New Request modal and editing

### 5.1 Form organization

A grouped, scrollable modal with clear sections is the baseline assumption; a multi-step wizard is not an approved requirement.

1. Request details.
2. Budget, amount and procurement information.
3. Remarks.

Supplier is absent. State is assigned by the server as Under review. The request reference is issued when saved.

### 5.2 Field contract

| Field | Creation | Later behavior |
| --- | --- | --- |
| Description | Required | Editable before approval |
| Requester | Required | Managed choice; inline add allowed |
| Logistics focal point | Optional baseline assumption | Managed choice; inline add allowed |
| Budget | Required UI selection | Stored through the chosen line |
| Budget line | Required | Must belong to selected budget |
| Area | Required | Supplies area code/name and country |
| Department | Required | Controlled configured list; actual codes supplied by the team |
| Request submission date | Required; today default | Business date, separate from registration |
| Reference date | Required; today default | Used in OF reference |
| Processing date | Optional | Set to approval date if still blank |
| Total amount | Required, positive | Not a unit price |
| Currency | Required; ETB default | ETB/USD/EUR/GBP |
| Quantity and unit | Optional pair | Both or neither; quantity positive |
| Procurement scenario | Manual, optional baseline | A/B/C/D/E/G; no automatic calculation |
| Waiver/memo | Optional | Text; no automatic scenario override |
| Submitted-to-finance date | Optional | May be updated while Pending |
| Logistics remarks | Optional | Separate from program remarks |
| Program/other remarks | Optional | Separate field |
| Supplier | Not collected | Required at approval |
| PO reason/date | Not collected | Required at approval |
| Payment date | Not collected | Required when marking Paid |
| Cancellation reason | Not collected | Required when cancelling, baseline assumption |

Optional fields must remain genuinely optional. Missing dates remain NULL; they are not silently replaced with today.

### 5.3 Validation and limits [BASELINE ASSUMPTIONS]

- Trim surrounding whitespace; preserve meaningful internal text.
- Description maximum 10,000 characters; each remarks field 20,000.
- Codes: uppercase letters/digits, hyphen/underscore; no slash; maximum 40 characters.
- Original amount: greater than zero, finite, at most two decimals, configurable maximum initially 1,000,000,000,000.
- Quantity: positive finite decimal, at most three decimal places; unit maximum 80 characters.
- Email: syntactic validation and normalization; do not claim delivery is proven by syntax.
- Future request/reference/PO/payment dates rejected; processing/finance dates may not be future.
- Do not require reference date to equal submission date or impose a strict date ordering that contradicts the sample workbook.
- All server errors are actionable and preserve input. No generic success after a failed transaction.

### 5.4 Editing policy [BASELINE ASSUMPTION, review explicitly]

The user confirmed Paid locking, not every intermediate edit restriction. This review candidate proposes:

- Under review: description, people, amount/currency, quantity/unit, scenario, memo, request submission/processing/finance dates and remarks can be corrected with audit. Reference date is excluded because it defines the issued reference.
- Issued reference-defining classification (budget line, area, department, reference date) is not silently changed. A correction requires cancellation and a replacement request in this first release. This is an explicit proposed restriction, not a previously confirmed user requirement.
- Pending: original amount/currency and classification are locked; supplier, submitted-to-finance date and remarks may be corrected with audit. Issued PO reason/date/reference stay fixed.
- Paid: entire request locked in this release, including notes.
- Cancelled: read-only.

If this policy is rejected at review, update the reference-history design before implementation. Do not allow a reference to silently describe a different budget/line after editing.

---

## 6. Approval, Paid and cancellation modals

### 6.1 Proceed with purchase order

ACT-01: available only Under review.

ACT-02: collect supplier, PO reason and PO date. Inline supplier addition is allowed and deduplicated against the managed list.

ACT-03: confirmation shows request reference, original total, budget and line.

ACT-04: on confirmation, generate the PO reference, save supplier/PO information, set Pending and record `pending_entered_at` in one transaction.

ACT-05: approval does not create a finance deduction.

### 6.2 Mark Paid

ACT-06: available only Pending.

ACT-07: show original total/currency, budget/line, rate source/date and lock warning; require payment date and confirmation checkbox.

ACT-08 [CONFIRMED]: this is a user assertion inside the tracker. No external payment verification is required and no payment is executed.

ACT-09: create one immutable payment event using the original request amount; set Paid, record `pending_exited_at`, append state/audit events, all atomically.

ACT-10: a unique payment-event/request relationship prevents duplicate payment events. Paid requests cannot be edited or cancelled through ordinary application actions.

ACT-11: if a request is mistakenly marked Paid, the release has no ordinary reversal feature. Show that limitation before confirmation. Do not advise direct database editing as a routine correction mechanism.

### 6.3 Cancel

ACT-12: available Under review and Pending only.

ACT-13: red action; reason and explicit confirmation.

ACT-14: preserve references, supplier and previous procurement dates; do not erase history. Record Pending exit if cancelling from Pending.

ACT-15: no payment event or finance deduction is created.

---

## 7. Weekly report inside Requests

### 7.1 Two different week definitions [CONFIRMED]

WEEK-01: reporting week is **Friday afternoon to the next Friday afternoon**.

WEEK-02: calendar filter week is **Monday to Sunday**. These are separate named concepts and must never share an ambiguous helper called only `week`.

WEEK-03 [BASELINE ASSUMPTION]: use Friday **14:00 Africa/Addis_Ababa** as the configurable logical cutoff. Email may arrive later. Approximate delivery does not make the report boundaries drift with the actual worker start time.

For example, with that cutoff:
- Period start: Friday 23 October 2026, 14:00 EAT.
- Period end: Friday 30 October 2026, 14:00 EAT.
- Membership uses `start <= registered_at < end`.
- A request registered exactly at 30 October 14:00 belongs to the next period.

### 7.2 Membership and report meaning

WEEK-04 [BASELINE ASSUMPTION]: use immutable server registration timestamp (`registered_at`), not an editable date-only submission field, for Friday-afternoon membership. This prevents ambiguous half-day allocation and late backdated entries disappearing from the next notification period.

WEEK-05: include all four states. Do not inherit the selected request state, budget, area or ordinary calendar filters.

WEEK-06: the report is a bounded database query, not a generated attachment. Membership is determined by registration time; displayed state and descriptive values are current at view time. It is not an immutable historical snapshot of what the state was at Friday cutoff.

WEEK-07: label the period, timezone and generated/viewed time. If a historical status-at-cutoff report is later required, add a separate approved snapshot/as-of feature; do not claim the live table provides it.

WEEK-08: no hard deletion or backdating of `registered_at`. A very short transaction begun near cutoff may commit just afterward; the bounded query may reveal that committed row on refresh. This is acceptable for this live weekly table and is not presented as an archived snapshot.

### 7.3 Presentation

WEEK-09: a temporary Weekly Report section appears within Requests for the latest completed reporting period. It must not displace the request table from immediately below the state cards; place the section after the table or expose it through a noticeable Weekly Report button/notice.

WEEK-10 [BASELINE ASSUMPTION]: show the latest completed period until the next cutoff replaces it. The section can be collapsed/dismissed for the session and reopened. On Friday, a once-per-session prompt may open the report modal when the app is in use; no claim of per-person acknowledgment without login.

WEEK-11: the email deep link opens Requests and the corresponding period, even if the latest period has changed. Validate period parameters on the server.

WEEK-12: columns: reference/PO, description, requester, budget/line, original amount/currency, current state, request date and registered time as needed. Export Excel and Close/Collapse are available.

WEEK-13: no requests is a valid report; show No requests registered in this period. Opening/exporting the report does not alter requests or finance checkpoints.

---

## 8. Weekly email notification

### 8.1 Purpose [CONFIRMED]

MAIL-01: send a short notification, not a report attachment:

> Your weekly requests report is ready. Open the application to view it: [configured application link].

Include the reporting period if useful. No supplier, request amount, budget figures or recipient list is embedded in the email body.

MAIL-02: the worker does not generate the report. The application can always query the completed period directly from PostgreSQL. The worker records period/delivery metadata only.

MAIL-03: active recipients are read from the same PostgreSQL database that Manage updates. No second hard-coded recipient list.

MAIL-04: send separately to each recipient. Do not expose other addresses in To/CC.

### 8.2 Sender and authentication

A temporary sender account owned by the team is acceptable. Its address can be supplied later. Changing to an approved domain later is configuration, not a product rewrite.

**An address alone does not authorize sending.** At runtime the selected provider must have valid SMTP authentication, OAuth credentials, an API key, or another explicitly configured authenticated relay. Credentials can be configured during implementation, but cannot be omitted while claiming notifications work. For eligible Gmail accounts, app passwords require two-step verification; not every account offers that option [S5].

Secrets live in worker/deployment secret configuration, never Manage, source control, database recipient rows, logs or the browser.

If credentials are absent, the worker reports Not configured and does not mark anything sent. The rest of the app remains usable. A provider sandbox/dry-run can test templates, but is not proof of delivery.

### 8.3 Scheduling [BASELINE ASSUMPTION]

- Independent scheduled worker, initially GitHub Actions or equivalent.
- Scheduled polling after the Friday cutoff; a manual trigger for testing.
- Compute the most recent completed reporting period in the configured timezone.
- Use a unique run per period and a unique delivery per run/recipient.
- Proposed hourly checks, away from the top of the hour; notify only for the latest period within a configurable 72-hour catch-up window.
- Scheduler delays are acceptable. GitHub documents that scheduled workflows can be delayed, so the logical cutoff must not depend on precise execution time [S4].
- App/browser activity is not required. The worker reads PostgreSQL directly.
- A zero-request period may still send the notification; the report clearly shows zero rows.

### 8.4 Delivery guarantees and failures

- Claim queued deliveries transactionally; do not hold a DB transaction open while calling the email provider.
- Record attempts, timestamps, provider result/message ID and a sanitized error.
- Retry known pre-acceptance/transient failures with bounded backoff. Proposed baseline: five automated attempts maximum, 30-second provider timeout and five-minute claim lease. A timeout after possible acceptance is Uncertain, not a known safe retry.
- SMTP cannot provide a database transaction with exactly-once delivery. If the provider may have accepted a message but the worker crashes before recording success, mark the delivery Uncertain and require deliberate retry; do not promise no duplicates under every failure.
- A sent flag means provider accepted the send, not proof the recipient read it or that it reached the inbox.
- A queued/known-failed delivery beyond the catch-up window is suppressed with period_expired; an Uncertain outcome remains visible for deliberate review. Do not silently resend old periods.
- Recheck recipient active status immediately before sending; removal stops future sends, but cannot recall an already accepted email.
- Newly added recipients can receive the current period during the catch-up window if no delivery exists; do not backfill months of old mail.
- Logs must not print credentials or full recipient lists.

---

## 9. Financial Updates tab: budget-level checkpoint model

### 9.1 Confirmed principles

FIN-01: Financial Updates defaults to **USD**. Batch currency and the simple batch export are USD. Requests may still default to ETB. An alternative report-view currency selector is not required by this baseline; its UI can be reviewed later, but it must never change a prepared batch currency.

FIN-02: only Paid events contribute. Display deductions as negative amounts.

FIN-03: only **budgets** are selectable. Budget lines have no selection checkbox.

FIN-04: selecting a budget automatically includes its eligible nonzero lines. No individual line checkpoint or line-level checkpoint timestamp exists.

FIN-05: every line under a budget shares the budget's latest applied checkpoint context. The UI shows the timestamp on the budget card only.

FIN-06: all active budgets appear by default, including zero-change budgets. Provide **Only budgets needing update** filter.

FIN-07: a prepared draft is frozen. To change membership, abandon it and prepare another.

FIN-08: a budget is disabled for another batch while it belongs to any current draft, even if new payments arrive.

FIN-09: distinguish reserved amount from new amount after preparation.

FIN-10: applying requires confirmation checkbox plus optional external-workbook/reference note. History and exports remain permanent.

### 9.2 Budget cards

Wide horizontal cards show:
- Checkbox, enabled only when eligible and not draft-reserved.
- Budget code and readable name.
- Total unapplied USD deduction.
- Last applied checkpoint date/time, or Never.
- Expand/collapse control.
- If reserved: current batch reference, reserved amount, new unreserved amount.

Expanding reveals noticeably smaller line rows/cards:
- Line code and description.
- Unapplied USD difference.
- Optional reserved/new split when relevant.
- No checkbox and no checkpoint date.

Nonzero lines sort first, then zero lines, then code. Within nonzero lines use code ordering unless a later approved UI specifies magnitude ordering.

Amounts on cards may be abbreviated for overview, but exact values must be available on expansion/confirmation. Exports always contain exact two-decimal values, not -100k strings.

### 9.3 What 'since checkpoint' means

It means paid events **not acknowledged by an applied batch**, not events with a date greater than the checkpoint timestamp.

A backdated payment registered after a checkpoint remains unapplied. A new payment arriving during a draft remains outside that draft. A checkpoint timestamp never sweeps up all earlier-dated rows.

The last checkpoint is informational context. Exact event membership is the accounting control.

### 9.4 Amount definitions

- **Applied:** payment event belongs to an applied batch.
- **Reserved:** payment event belongs to a draft batch.
- **Available/new:** payment event has no current draft/applied membership.
- **Unapplied:** reserved plus available/new.

Never count abandoned memberships as current. Never count the same payment twice through a history join.

At line level, monetary totals are based on saved USD valuations. At budget level, sum the displayed line totals so expansion reconciles. Reserved batch line totals are frozen. The available/new portion is calculated separately; small rounding differences are explicitly handled in section 12 and the database design.

### 9.5 Prepare selected budgets

1. User selects one or more eligible budgets.
2. Noticeable Batch selected budgets button opens a small preview/confirmation modal.
3. Preview shows selected budget codes, eligible line totals, overall USD deduction and a preview token.
4. Confirm rechecks versions, reservations and the exact candidate event set.
5. If relevant data changed since preview, require refreshed confirmation rather than silently include different amounts.
6. Reserve budgets and exact events, freeze line totals, seal the prepared draft and create audit, all in one transaction. No committed unsealed batch is allowed.
7. Show the draft in Current batches and disable its budgets for other batches.

A zero-change budget cannot create an empty draft. If any selected budget is already reserved or changed, fail the preparation atomically with an actionable message; do not silently prepare only a subset.

### 9.6 New payments during a draft

Example:
- Reserved in Batch 12: USD -20,000.00.
- New since preparation: USD -3,000.00.
- Total unapplied: USD -23,000.00.

The existing draft stays at -20,000.00. The budget remains unavailable for another batch until Batch 12 is applied or abandoned. After application, -3,000.00 remains for the next batch.

### 9.7 Current batch cards

Below the budget list, each card shows:
- Batch reference.
- Prepared date/time.
- Included budget codes.
- Frozen USD deduction.
- Export Excel button on the right.
- Confirm applied button on the right.
- Secondary Abandon draft action, not a primary destructive shortcut.

No membership editing. Export does not apply the batch.

### 9.8 Confirm applied

- Modal shows batch reference, budgets and exact frozen USD total.
- Required checkbox: these exact figures have been applied externally.
- Optional external workbook/reference note.
- Confirm locks/rechecks draft state, marks applied, records time/note, releases budget reservations and preserves payment acknowledgments atomically.
- All included budgets receive the same batch application timestamp as their latest checkpoint. All their lines inherit that budget context, including lines with zero included events.
- New unbatched events remain unapplied.
- Repeated confirmation cannot apply a batch twice.

The app cannot verify an external workbook update. It records a deliberate user acknowledgment, not external execution.

### 9.9 Abandon draft

- Require confirmation that it was not applied externally.
- Release budget reservations and event memberships atomically.
- Retain the abandoned batch, its line totals and its historical membership.
- Released events can enter a later batch once.
- Applied batches cannot be abandoned.

### 9.10 Batch history

At the bottom show applied batches and, through a history filter, abandoned drafts. Include reference, budgets, prepared/applied/abandoned time, note and exact export. No edit/reset/delete action for applied history.

### 9.11 Simple Excel contract [CONFIRMED direction]

One simple worksheet, not a technical workbook bundle.

Header metadata: batch/report reference, status, date/time, currency USD.

Data columns:
1. Budget code.
2. Budget-line code.
3. Difference (USD).

Only nonzero lines. Group by budget. Optional clearly labelled budget subtotals and one grand total may be included, but never as rows that can be mistaken for extra deductions. No raw payment/rate worksheets by default. Supporting event IDs, rates and history remain in the database.

A batch export always uses its frozen line totals. A current-unapplied export must be labelled **Current view, not a checkpoint batch** and must not be used as a substitute for batch confirmation.

---

## 10. Manage tab

MAN-01: create budgets and lines under them. Codes unique at the correct scope.

MAN-02: manage areas/country codes, requesters, focal points and suppliers.

MAN-03: active/inactive status instead of deleting referenced records. Inactive records remain visible in history and cannot be selected for new requests.

MAN-04 [BASELINE ASSUMPTION]: prevent deactivating a budget/line/area while open requests depend on it; finish/cancel those requests first. Budget deactivation also requires no current draft reservation and no unapplied paid events. Do not hide financial work through deactivation.

MAN-05: used codes and parent relationships are immutable. Descriptive labels can be corrected with audit; issued reference snapshots remain unchanged.

MAN-06: normalize inline contact matching within role. Same display name may exist in different roles; contacts are not authenticated users.

MAN-07: unit remains a descriptive text field with suggested choices for this release. Department/scenario choices are configuration, not new mandatory management pages.

MAN-08: show CBE rate source, effective date, buying type, freshness and refresh status. No credentials in this view.

### 10.1 Email recipients

MAN-09: list name (optional), email address, active state and added date.

MAN-10: add recipient, validate, prevent normalized duplicates.

MAN-11: Remove deactivates the address and suppresses future queued deliveries. Historical delivery records are retained. Reactivation reuses the same recipient identity rather than creating a duplicate.

MAN-12: editing an address is implemented as deactivating the old address and adding the new one, preserving old send history.

MAN-13: sender address/provider is deployment configuration. Sender authentication secrets are not ordinary Manage fields.

---

## 11. Insights tab

INS-01: wide main chart area; chart-selector dropdown at upper right; compact left filter sidebar consistent with Requests. This is the baseline layout assumption from the user's two proposed arrangements.

INS-02: filters change with selected chart. Hidden filters from another chart must not silently affect results.

INS-03: baseline chart choices are provisional and extensible:
- Requests arriving.
- Paid spending.
- Pending waiting time / overdue.

INS-04: no chart calculates budget remaining balances.

### 11.1 Requests arriving

- Count registrations using immutable registered_at.
- Day/week/month grouping; calendar weeks Monday-Sunday.
- Time, budget, line and area filters as appropriate.
- Clearly label the date basis; do not call registration counts payment activity.

### 11.2 Spending

- Paid events only.
- Actual payment-date period, budget/line and currency filters.
- Use saved valuations, not current rates applied retrospectively.
- Applied and unapplied events both remain in historical spending.
- Default Insights spending currency USD as a baseline assumption; label it explicitly.

### 11.3 Pending waiting time [CONFIRMED]

Waiting time is time spent in Pending only. Under-review time is not included.

- Current Pending age = as-of time minus pending_entered_at.
- Completed Pending duration = pending_exited_at minus pending_entered_at, ending at Paid or cancellation.
- Keep paid-completed and cancelled-completed durations distinguishable; do not imply cancelled cases are successful procurement completion.
- Default filters: Pending-entry period, budget, line and area. Label the cohort date basis.
- Default view: current Pending ages/oldest cases and completed-duration distribution, as selected.
- Calendar elapsed hours/days, not working days, is the baseline assumption.
- Do not reset age when a note or supplier is edited.

**Overdue threshold has not been supplied.** Store it as nullable configuration. Until configured, show Pending age and state Overdue threshold not configured. Do not invent an SLA or label cases overdue based on an arbitrary number.

When configured, overdue means current Pending elapsed hours strictly greater than the configured threshold. Show the threshold and as-of time. More complex per-scenario SLAs are deferred.

INS-05: charts and tables must have meaningful empty/error states, readable labels, theme-aware colors and optional tabular values for reconciliation.

---

## 12. Currency, valuation and rounding

### 12.1 Confirmed and assumed choices

- Supported: ETB, USD, EUR, GBP.
- Request entry/display default: ETB.
- Financial Updates default: USD. Checkpoint batches and their simple exports: USD, fixed for this release.
- CBE buying is the source.
- **BASELINE ASSUMPTION:** transactionalBuying, not cashBuying, until the user changes the configured buying subtype. CBE documents both fields [S6].

### 12.2 Rate ingestion

FX-01: server-side rate-adapter HTTP only. No per-row browser API requests. The notification worker does not fetch rates or write rate/payment data.

FX-02: fetch/cache one complete set for the effective date and subtype. ETB=1; USD/EUR/GBP must all be positive finite values.

FX-03: accept the documented object/array response forms, validate date/schema, reject incomplete sets as complete. The Date parameter is required and weekends/holidays may have no data [S6].

FX-04: bounded timeouts/retries, clear error and last-success information. No fake live fallback.

FX-05: published rate sets are immutable. A provider revision creates a new version/hash, not an overwrite of rates used by a paid event.

### 12.3 Valuation basis [BASELINE ASSUMPTION]

- A request may be registered even if CBE is unavailable; its original amount/currency is valid independently of conversion.
- Entry-rate set is saved when available. Requests-table conversions use that saved set; if absent, show original amount and Conversion unavailable. A same-currency display needs no rate lookup. An explicit audited Attach missing entry rate action may fill a previously NULL entry_rate_set_id with a complete current set; it must not overwrite an existing set or a Paid record, and the UI labels its effective date.
- Marking Paid requires a complete rate set no more than seven calendar days old, with explicit acknowledgment if not today's effective date.
- Use the latest available rate at the moment Mark Paid is confirmed, even for a backdated payment date. Export/detail metadata must show the rate date so this is not mistaken for a historical payment-date rate.
- If no acceptable rate exists, approval and navigation still work, but Paid is blocked with a specific refresh/retry message.
- No manual rate override in ordinary UI for this release.

These are proposed exact rules, not a claim the user explicitly selected the seven-day limit or valuation date. Review section 23 before implementation.

### 12.4 Formula

`target_amount = original_amount * ETB_per_original_currency / ETB_per_target_currency`

Authoritative financial arithmetic uses PostgreSQL NUMERIC, not binary floating-point R calculations. PostgreSQL documents NUMERIC as exact decimal and appropriate for monetary amounts [S3].

### 12.5 Precision and small amounts [BASELINE ASSUMPTION]

- Original amount: NUMERIC(18,2).
- Rates: NUMERIC(24,10).
- Paid converted valuations: NUMERIC(28,8), frozen once.
- Display/export financial line totals: round to two decimals, half away from zero, using PostgreSQL decimal rounding.
- Sum high-precision USD valuations at line level, then round. Do not round each tiny event to zero before summing.
- A line with rounded available USD difference zero is not included in a batch/export. Its events remain unacknowledged and accumulate until the line becomes nonzero; they are not discarded.
- A batch freezes each included line's raw sum, two-decimal total and rounding difference. Budget and batch totals sum frozen line totals, not an independently rounded grand total.
- During a draft, reserved line totals and new available line totals are rounded separately; unapplied display is their sum, so the visible split reconciles. Do not silently re-round reserved and new raw values together and show a conflicting total.
- Example: two available events valued at USD 0.004 each produce a line total of USD 0.01, not USD 0.00.

No negative corrections/refunds exist in this release. Stored original payment amounts are positive and converted values nonnegative; deduction displays negate them once.

Rounding reconciliation: raw historical spending equals acknowledged raw values plus unapplied raw values. The sum of exported batch deductions can differ by cents from rounding all historical spending once, because batches round line totals separately. Saved rounding_delta explains that difference; never alter old batches to force a match.

---

## 13. References

REF-01 [CONFIRMED]: preserve workbook patterns:

`OF/{budget}/{line}/{country}/{area}/{department}/{dd-mm-yyyy}`

`PO/OF/{budget}/{line}/{country}/{area}/{department}/{reason}/{dd-mm-yyyy}`

Examples:
- `OF/29FZN/SF9AVR/ETH/MEK/REACH/01-07-2026`
- `PO/OF/29FZN/SF9AVR/ETH/MEK/REACH/Vehicle Rent/02-06-2026`

REF-02: generate request reference at creation and PO reference at approval, server-side and transactionally.

REF-03: issued references are stable; retain their exact component snapshot and do not reuse cancelled references.

REF-04 [BASELINE ASSUMPTION]: first occurrence uses exact base; collisions append /002, /003, etc. Use a transaction-scoped advisory lock keyed by reference kind/base plus a database unique constraint. Never use unlocked MAX(id)+1. This collision policy requires review because the sample does not define it.

REF-05: do not embed a different budget/project hierarchy or invent PO-0012 display identifiers as replacements.

---

## 14. Technical architecture and boundaries

Detailed file/folder structure is deliberately deferred to the next phase. The following logical boundaries are required now:

1. **Presentation:** four Shiny page modules, request/action modals, shared theme/components. No SQL or financial calculations in render functions.
2. **Application services:** validation, transitions, reference issuance, exact batch workflow, weekly-period calculation, notification orchestration.
3. **Repositories:** parameterized SQL and typed row mapping. No Shiny input/output access.
4. **PostgreSQL:** relationships, uniqueness, immutable financial history, transactional concurrency and audit.
5. **CBE adapter:** schema parsing, fetching, validation, caching and immutable rate sets.
6. **Export adapter:** simple workbooks from the same approved service results as the UI.
7. **Scheduled worker:** PostgreSQL recipient/delivery access and authenticated notification sending; no report generation or financial writes.

### 14.1 Shared refresh

- Successful writes advance a database revision and invalidate session queries.
- Other sessions poll a lightweight revision, target refresh within five seconds under normal conditions.
- No global mutable in-memory data frame as the shared source of truth.
- Database connections are released on success/error/session termination.
- No network/API calls inside database transactions.

### 14.2 Security and secrets

- TLS for remote PostgreSQL and email provider connections.
- Runtime app and worker use distinct least-privilege database roles.
- Worker cannot modify requests, payments or checkpoints.
- Migration credentials are not ordinary runtime credentials.
- No passwords/API keys in repository, sample data, app_settings, emails, URLs or logs.
- Validate deep-link parameters; use configured owned application origin, not arbitrary user-supplied redirects.
- Public links are not authentication. Do not expose recipient lists or sensitive data merely because the link is owned by the team.

### 14.3 No hidden migrations or resets

Migrations run explicitly and once. App startup checks schema version; it does not drop/recreate tables or reseed. Sample reset is an explicit controlled test operation, not a Manage button that deletes financial history.

---

## 15. Transaction contracts

### 15.1 Create request

Validate -> lock parent budget and validate active line/area/contact assignments -> lock/reference generation -> insert request with unique submission key -> initial state event -> audit -> revision -> commit. Retry same submission key returns existing request if payload matches; different payload with same key is a conflict.

### 15.2 Approve

Lock request -> expected version/state check -> validate active supplier -> generate unique PO reference -> set Pending/pending_entered_at -> state event/audit -> revision -> commit.

### 15.3 Mark Paid

Read/validate chosen immutable rate set before transaction. Within transaction lock request then parent budget using the agreed lock order -> recheck state/version/rate eligibility -> calculate/freeze valuations in NUMERIC -> insert unique payment event -> set Paid/pending_exited_at -> state/audit/revision -> commit.

### 15.4 Prepare batch

Preview outside write transaction. On confirm lock selected budgets in ascending ID order -> reject any active draft reservation -> recheck preview event-set fingerprint -> identify nonzero eligible lines -> insert draft/budget reservations/line totals/exact items -> audit/revision -> commit. All selected budgets succeed or none do.

### 15.5 Apply/abandon

Lock batch, then its budgets in ascending order -> recheck draft/version -> update state/timestamps -> release budget reservations; release item memberships only on abandonment -> audit/revision -> commit. Applied payment membership stays active forever in this release.

Use one documented lock order consistently. Do not lock a budget and then request in one service while doing the reverse elsewhere. The database design gives the final order and rationale.

### 15.6 Notification send

Short transaction claims delivery with lease -> commit -> provider call -> short transaction records result. Never hold a row lock during a slow SMTP call. Expired ambiguous sends become Uncertain, not automatically Sent or blindly retried.

---

## 16. Service result and error contracts

Every service returns a structured success or typed error. Error categories: validation, not_found, stale_version, invalid_transition, budget_reserved, preview_changed, fx_unavailable, dependency_unavailable, permission/configuration.

- UI shows actionable messages, not raw SQL/credentials.
- Empty queries return typed zero-row results, not unexpected NULL.
- Choice labels and values are built from the same rows.
- Exports use the same scoped result, not a second hidden filter implementation.
- Preview tokens are held in the server session or signed server-side, expire after a proposed ten minutes, and include sorted budget/event IDs, frozen valuation values and settings version. Never trust a browser-submitted total.
- Services do not read `input$` or construct modals.
- Modals do not bypass services with direct writes.
- No base-function monkey patch such as redefining setNames().

Expected logical operations:
- list/count/get/create/edit/approve/cancel/mark-paid requests.
- list/manage reference data and recipients.
- get current weekly period/report.
- get financial budget/line view; preview/prepare/get/apply/abandon batch.
- fetch/validate/cache/get rate set.
- export request/weekly/current-finance/batch view.
- queue/claim/send/record weekly notification deliveries.

Exact R signatures and physical files are agreed in the next phase.

---

## 17. Expected outputs by area

| Area | Expected output |
| --- | --- |
| Requests | Filtered newest-first table, four accurate counts, modals, exact-scope Excel |
| Weekly section | Latest completed Friday-to-Friday request table, period link, simple export |
| Financial Updates | Budget cards, nested line differences, reserved/new split, USD-only batches/history |
| Batch export | One concise sheet with nonzero budget/line USD deductions and header metadata |
| Manage | Connected reference lists and active email-recipient list |
| Insights | Selectable chart area, context-specific filters, Pending-only age/duration |
| Notifications | Authenticated link-only emails, recipient-level delivery status, no report attachment |
| Database | Enforced relationships, immutable paid/checkpoint history, shared persistent data |
| Documentation | Approved spec/design, later setup/test/deploy instructions and known limitations |

---

## 18. Acceptance tests

### 18.1 Requests and references

T01 Empty database: all four tabs, dropdowns and exports render without disconnecting.
T02 Idempotent seed: rerun inserts no duplicates and does not switch app behavior/database.
T03 Create: one valid request -> Under review, one reference, one initial state event.
T04 Validation: missing required/negative/nonfinite/excess-decimal amount rejected; values retained.
T05 Optional dates remain NULL; display format does not corrupt ISO storage.
T06 Budget/line dependency enforced in UI and server; forged mismatch rejected.
T07 Double Save/retry creates one request.
T08 Approval requires supplier/reason/date/confirmation and creates one PO/Pending event.
T09 Approval does not change finance totals.
T10 Cancellation preserves references/history and creates no payment.
T11 Paid creates one immutable payment event atomically and locks the request.
T12 Double Paid creates no duplicate event; stale edit/transition rejected.
T13 Reference collisions under concurrent requests follow approved suffix rule.
T14 Calendar This week is Monday-Sunday; newest ordering deterministic; export covers all matching pages.
T15 Theme switch/shared refresh preserves unsaved form values.

### 18.2 Weekly reporting and mail

T16 Friday boundary: just before cutoff belongs to closing period; exactly at cutoff belongs to next.
T17 Reporting Friday-Friday and filter Monday-Sunday give intentionally different memberships.
T18 Backdated submission entered after cutoff appears in next registration-based report.
T19 Weekly report includes all states and ignores sidebar state/budget filters.
T20 Email link opens the correct Requests report period; no fifth tab.
T21 Worker reads active recipients from PostgreSQL; removed recipients suppressed before send.
T22 Same period/recipient is not routinely requeued/sent on repeated scheduler runs.
T23 Missing credentials -> Not configured, no false Sent result.
T24 Provider failure/timeout/crash -> accurate failed/uncertain status; no claimed exactly-once SMTP guarantee.
T25 Worker does not generate a report attachment or modify procurement/finance data.
T26 Zero-request week displays correctly and may still notify.

### 18.3 Financial Updates

T27 All active budgets visible; Only budgets needing update filters correctly.
T28 Only budget checkboxes; nonzero lines auto-included; zero lines omitted from export.
T29 No line checkpoint timestamp; budget checkpoint context shared across lines.
T30 Draft freezes membership/totals; budget disabled for any second draft.
T31 Concurrent overlapping selections cannot reserve the same budget or event.
T32 New payment during draft shown separately and excluded from existing export.
T33 Changed preview requires reconfirmation; no silently changed batch amount.
T34 Apply requires checkbox, optional note; exact events acknowledged once.
T35 Applying releases budget for next batch but leaves new events unapplied.
T36 Abandon releases reservations, retains history and allows re-batching once.
T37 Applied batches cannot be edited/abandoned/deleted.
T38 Backdated payment recorded later remains unapplied regardless of checkpoint time.
T39 Simple Excel has only nonzero line deductions, correct USD/date/reference and reconciled totals.
T40 Tiny USD valuations accumulate before line rounding; no silent loss.
T41 Paid historical spending unchanged by checkpoint application or rate refresh.

### 18.4 FX, Insights and deployment

T42 CBE parser: documented object/array forms, missing currency, bad/future date, nonpositive rates.
T43 FX outage does not disconnect app; original request can save under approved policy; Paid blocked clearly if required valuation unavailable.
T44 Frozen payment/batch valuations do not change after new rate fetch.
T45 Pending age excludes Under review; stops on Paid/cancellation; note edit does not reset it.
T46 No overdue labels before threshold configured; correct boundary afterward.
T47 Chart-specific filters do not leak between charts; date basis labelled.
T48 Light/dark, keyboard focus, long references, narrow screens and error states checked.
T49 Hosted create/restart/redeploy preserves PostgreSQL records and batch history.
T50 Two browser sessions see committed changes and reject stale writes.
T51 Worker DB role cannot write requests/payments/checkpoints; secrets absent from bundle/logs.
T52 Backup restored to isolated database and record/payment/batch totals reconcile.
T53 A committed batch is sealed; direct insertion of later budget/line/item membership is rejected.
T54 Raw payment totals reconcile with unapplied/applied raw values; saved batch rounding deltas explain any cent difference from exported totals.

### 18.5 Deterministic finance fixture

All amounts USD for this fixture:
- B1/L1: Paid 100.
- B1/L2: Paid 200.
- B1/L3: zero.
- B2/L1: Paid 50.
- Additional Under review/Pending/Cancelled records do not contribute.

Initial B1=-300, B2=-50. Prepare B1 batch: include L1/L2, omit L3, reserve B1. Mark another B1/L1 request Paid for 25. Display reserved=-300, new=-25, total=-325. Second B1 batch is blocked. Apply first: B1 checkpoint updates, outstanding=-25, all B1 lines share that checkpoint context. Prepare next B1 batch: only new L1 amount. Historical paid spending across both budgets remains 375.

---

## 19. Implementation sequence and release gate

The detailed folder structure and UI assets come after review of these two documents. Then:
1. Approve assumptions and freeze schema/service contracts.
2. Establish PostgreSQL connection/migrations and explicit sample seed.
3. Deliver one create -> open -> approve vertical slice.
4. Deliver Paid event/locking and exact budget-level batch workflow.
5. Deliver simple exports and recipient management.
6. Deliver weekly query/deep link and authenticated notification worker.
7. Deliver baseline Insights and both themes.
8. Execute acceptance tests, document results, fix blockers and deploy release candidate.

Do not postpone hosted database connectivity/persistence checks until the last hour. Do not use the deadline to claim tests ran when only syntax parsing ran.

Release requires passing critical lifecycle, checkpoint, persistence and notification-configuration checks, or an explicit documented exception accepted by the team. An absent email credential is a known incomplete notification feature, not a successful send.

---

## 20. Agent/developer instructions

- Read both documents before coding.
- Implement only assigned requirement IDs.
- Do not add a fifth tab, extra state, project hierarchy, allocations, line checkboxes or line checkpoint dates.
- Do not change USD batch currency or silently revalue historical amounts.
- Do not generate report attachments in the notification worker.
- Do not invent overdue thresholds, automatic scenarios or authentication screens.
- Do not add hidden mode switches or seed on every startup.
- Preserve the simple user-facing Excel contract; internal traceability stays in PostgreSQL.
- Keep UI, services, SQL, FX, exports and worker boundaries separate.
- Changes to shared schema/contracts require review before implementation.
- Report actual changed files, commands, test results and untested limitations.

---

## 21. Decision register

D01 CONFIRMED: four tabs; Requests/Financial Updates/Manage/analytical tab.
D02 CONFIRMED: four states; new request Under review; modal workflow.
D03 CONFIRMED: one budget -> many lines; request -> exactly one line.
D04 CONFIRMED: table below state cards; compact sidebar; newest first.
D05 CONFIRMED: weekly report Friday afternoon to next Friday; calendar week Monday-Sunday.
D06 CONFIRMED: weekly section inside Requests; notification only links to owned app.
D07 CONFIRMED: PostgreSQL stores shared records and recipient list; hosting migration deferred.
D08 CONFIRMED: USD financial default and USD batch currency; ETB request default; selectable entry currencies.
D09 CONFIRMED: budget-only selection; nonzero lines auto-included; budget-level checkpoint only.
D10 CONFIRMED: all active budgets plus Only budgets needing update filter.
D11 CONFIRMED: frozen drafts; budget blocked until apply/abandon; reserved/new split.
D12 CONFIRMED: confirmation checkbox, optional external note, permanent history/simple export.
D13 CONFIRMED: Pending-only waiting time; overdue specifics later.
D14 CONFIRMED: manual procurement scenario; original total used at Paid; Paid locked.
D15 CONFIRMED: one application with sample records, no separate demo/live product split.

---

## 22. Operational configuration needed later, not another product debate

- PostgreSQL host/database/credentials/TLS and app/worker roles.
- Owned application URL and report deep-link route.
- Temporary sender address plus valid provider authentication.
- Scheduler secrets and manual-run access.
- Business cutoff/timezone and selected CBE buying subtype.
- Optional overdue threshold; absence disables overdue classification, not Pending-age charts.
- Package/R versions, migrations, seed command, backup/restore and release reviewer.

No sender password needs to be supplied in this document or chat. Actual sending still requires authenticated configuration when implemented.

---

## 23. Explicit assumptions requiring review

A01: Insights is the fourth tab's final label; left filter sidebars are consistent across Requests/Insights.
A02: logical Friday cutoff is 14:00 Africa/Addis_Ababa; delivery may be delayed.
A03: weekly membership uses immutable registered_at, not editable submission date; report displays current state rather than frozen Friday state.
A04: weekly section shows latest completed period until next cutoff; once-per-session prompt/reopen behavior, not per-user tracking.
A05: requester required, focal optional; manual scenario optional; quantity/unit paired; country from area.
A06: exact limits/date validation in section 5.3.
A07: reference-defining classification fixed after issue; cancel/recreate for correction; Pending financial fields locked; Paid entire record locked.
A08: reference collision suffix /002, /003 while preserving base pattern.
A09: CBE transactionalBuying; seven-day acceptable age; latest-at-mark-paid valuation; create allowed without FX, Paid blocked without complete acceptable FX.
A10: decimal precision and line-level rounding/tiny-value handling in section 12.5.
A11: calendar elapsed Pending duration; overdue threshold nullable until specified.
A12: hourly scheduled checks with 72-hour catch-up; recipient-level sends; five automated attempts, 30-second provider timeout, five-minute lease; ambiguous provider outcomes require deliberate retry; batch preview expires after ten minutes.
A13: no deactivation that hides open requests, current drafts or unapplied finance work.
A14: blue primary actions/red cancellation, subject to final UI review of earlier green-action preference.
A15: Financial Updates uses its USD default for this baseline; an alternative view-currency selector can be decided at UI review without changing fixed USD batch exports.

These assumptions are not hidden blockers. They are the exact proposed defaults to approve or amend before coding. The user need only change the assumptions that are wrong.

---

## 24. Technical references

[S1] PostgreSQL constraints: https://www.postgresql.org/docs/current/ddl-constraints.html

[S2] PostgreSQL explicit/row/advisory locking: https://www.postgresql.org/docs/current/explicit-locking.html

[S3] PostgreSQL exact numeric types: https://www.postgresql.org/docs/current/datatype-numeric.html

[S4] GitHub Actions scheduled workflow behavior: https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#schedule

[S5] Google account app-password eligibility/authentication: https://support.google.com/accounts/answer/185833

[S6] CBE documented endpoint, response fields and no-data dates: https://cbetradeunion.cbe.com.et/Exchange-rate-documentation?srcPage=home

These sources support technical platform behavior. Product decisions above come from the user's requirements, not from those sources.
