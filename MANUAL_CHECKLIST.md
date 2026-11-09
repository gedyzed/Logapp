# Manual Release Verification Checklist

This checklist documents requirements that rely on visual rendering, physical browser mouse clicks, and client-side CSS/DOM compositing (such as backdrop blur filters). Run these short, numbered steps before each release.

---

### 1. Modal Background Blur & Dialog Sharpness `[6]`
**Objective**: Verify that opening any popup blurs the page behind it and keeps the dialog sharp and legible.
- **Step 1.1**: Log in as `acted` / `logapp`.
- **Step 1.2**: On the **Requests** tab, click **"New Request"**.
  - **Expected**: Background page shell (table, filters, navbar) blurs (`filter: blur(4px)`); the modal dialog remains crisp and completely unblurred.
- **Step 1.3**: Open request details by clicking any row. Click **"Edit"**.
  - **Expected**: Edit modal appears; background remains blurred; modal is sharp.
- **Step 1.4**: Open a request in status *under_review* and click **"Proceed"**.
  - **Expected**: Proceed dialog opens with blurred background.
- **Step 1.5**: Open a request in status *pending* and click **"Mark as Paid"**.
  - **Expected**: Payment confirmation dialog opens with blurred background.
- **Step 1.6**: Click **"Cancel"** on any open request.
  - **Expected**: Cancel confirmation dialog opens with blurred background.
- **Step 1.7**: In the weekly report banner, click a request row.
  - **Expected**: Weekly read-only modal opens with blurred background.
- **Step 1.8**: On the **Financial Updates** tab, select a project and click **"Batch Selected"**.
  - **Expected**: "Name this batch" modal opens with blurred background.
- **Step 1.9**: On an active batch, click **"Confirm Applied"** and **"Reject"**.
  - **Expected**: Respective confirmation dialogs blur background.
- **Step 1.10**: On the **Manage** tab, select a project, supplier, or person and click **"Edit selected"**.
  - **Expected**: Edit dialog opens with sharp controls and blurred background.

---

### 2. Physical Row Click Navigation `[5]`
**Objective**: Verify that clicking anywhere on a table row opens the corresponding request modal.
- **Step 2.1**: On the **Requests** tab, move the cursor over any request row.
  - **Expected**: The cursor turns to a pointer (`cursor: pointer`), and the row highlights on hover.
- **Step 2.2**: Click anywhere inside the row (e.g. on description or requester).
  - **Expected**: The request details modal opens immediately for that specific request ID.
- **Step 2.3**: If the description has a "See more" link (`...`), click specifically on the link.
  - **Expected**: The "See more" text preview popup opens rather than opening the whole request.

---

### 3. Request Fields Visibility & Layout `[3b]`
**Objective**: Verify all required fields are visible inside dialogs and not clipped or hidden.
- **Step 3.1**: Open **"New Request"**.
  - **Expected**: Description, Requested by, Project code, Budget line, Department, Area, Quantity, Unit, Amount, Currency, and Scenario fields are fully visible and readable.
- **Step 3.2**: Open a request in *under_review*, *pending*, *paid*, and *cancelled*.
  - **Expected**: Read-only details show Amount and Currency in the header. Pending and Paid show PO reason, PO date, and PO reference. Paid shows Payment date.
- **Step 3.3**: Verify that long descriptions or remarks wrap cleanly without horizontal clipping.

---

### 4. Read-Only Enforcement for Weekly Report & Finalized Requests `[2]`, `[3]`
**Objective**: Verify read-only enforcement in the visual interface.
- **Step 4.1**: In the **Weekly Report** banner table, click any row.
  - **Expected**: The weekly details modal has NO "Edit", "Submit", "Cancel", "Delete", or status dropdown controls. It contains only a "Close" button.
- **Step 4.2**: In the **Requests** register, open a request in status *paid* or *cancelled*.
  - **Expected**: No "Edit", "Proceed", "Mark as Paid", or "Cancel" action buttons are visible or enabled.

---

### 5. DataTables Ajax & Search Responsiveness
**Objective**: Verify table rendering without client-side alerts.
- **Step 5.1**: Use the search box to filter requests.
  - **Expected**: Table re-renders smoothly with no DataTables warning alerts or JavaScript console errors.
- **Step 5.2**: Change pagination length and click next/previous.
  - **Expected**: Displays 10 rows per page smoothly.

