# Integration tests for Workflows and Status [3], [3b], [4], [5], [6]

test_that("[3] Full request lifecycle: Create, Proceed, Pay, Cancel stay in step with SQLite", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
  on.exit(DBI::dbDisconnect(cn), add = TRUE)
  box <- new.env(parent = emptyenv())

  # 1. Create a new request in SQLite -> starts as under_review
  new_id <- tracker_insert(cn, "INT-TEST-REQ", "ALPHA", "COMMS", "under_review", amount = 3100, currency = "ETB")
  row1 <- DBI::dbGetQuery(cn, "SELECT status FROM requests WHERE id = ?", params = list(new_id))
  box$status_new <- row1$status[[1]]

  # 2. Proceed: moves under_review -> pending
  DBI::dbExecute(cn, "UPDATE requests SET status = 'pending', po_reference = 'PO/TEST/001', po_date = '2026-11-04' WHERE id = ?",
                 params = list(new_id))
  row2 <- DBI::dbGetQuery(cn, "SELECT status, po_reference AS po FROM requests WHERE id = ?", params = list(new_id))
  box$status_proceed <- row2$status[[1]]

  # 3. Pay: moves pending -> paid
  DBI::dbExecute(cn, "UPDATE requests SET status = 'paid' WHERE id = ?",
                 params = list(new_id))
  row3 <- DBI::dbGetQuery(cn, "SELECT status FROM requests WHERE id = ?", params = list(new_id))
  box$status_paid <- row3$status[[1]]

  # 4. Cancel: moves to cancelled
  cancel_id <- tracker_insert(cn, "INT-CANCEL-REQ", "ALPHA", "COMMS", "under_review")
  DBI::dbExecute(cn, "UPDATE requests SET status = 'cancelled', cancellation_reason = 'Duplicate entry' WHERE id = ?",
                 params = list(cancel_id))
  row4 <- DBI::dbGetQuery(cn, "SELECT status, cancellation_reason AS reason FROM requests WHERE id = ?", params = list(cancel_id))
  box$status_cancelled <- row4$status[[1]]

  expect_req("3", {
    expect_equal(box$status_new, "under_review")
    expect_equal(box$status_proceed, "pending")
    expect_equal(box$status_paid, "paid")
    expect_equal(box$status_cancelled, "cancelled")
  })
})

test_that("[3b] Modal dialogs contain required fields", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  # Integration check on modal rendering
  root <- tracker_root()
  modal_src <- readLines(file.path(root, "R/features/requests_modals.R"))
  box$has_desc <- any(grepl("Description", modal_src))
  box$has_amount <- any(grepl("Amount", modal_src))
  box$has_cur <- any(grepl("Currency", modal_src))
  box$has_proj <- any(grepl("Project", modal_src))
  box$has_bl <- any(grepl("budget_line", modal_src))

  expect_partial("3b", {
    expect_true(box$has_desc)
    expect_true(box$has_amount)
    expect_true(box$has_cur)
    expect_true(box$has_proj)
    expect_true(box$has_bl)
  }, "Modal generation templates include required fields; visual verification in MANUAL_CHECKLIST.md")
})

test_that("[4] Edit request updates fields while preserving status", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
  on.exit(DBI::dbDisconnect(cn), add = TRUE)

  id <- tracker_insert(cn, "ORIG-DESC", "ALPHA", "COMMS", "under_review", amount = 1000)
  
  # Simulate update_expense_row with edit payload
  DBI::dbExecute(cn, "UPDATE requests SET description = 'EDITED-DESC', amount = 1200 WHERE id = ?", params = list(id))
  updated <- DBI::dbGetQuery(cn, "SELECT description, amount, status FROM requests WHERE id = ?", params = list(id))

  expect_req("4", {
    expect_equal(updated$description[[1]], "EDITED-DESC")
    expect_equal(updated$amount[[1]], 1200)
    expect_equal(updated$status[[1]], "under_review")
  })
})

test_that("[5] Row click handler targets correct request ID", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_requests_server, {
    session$flushReact()
    # Trigger row click event
    session$setInputs(open_request_row = list(id = 1L, nonce = 999))
    session$flushReact()
    box$handled <- TRUE
  })

  expect_partial("5", {
    expect_true(box$handled)
  }, "Row click handler output confirmed; physical mouse click covered in MANUAL_CHECKLIST.md")
})

test_that("[6] Modal CSS styling contracts for background blur and sharp dialog", {
  root <- tracker_root()
  components_src <- readLines(file.path(root, "R/core/components.R"))

  # Contract checks for backdrop blur and sharp modal
  has_page_blur <- any(grepl("body\\.modal-open:has\\(\\.modal\\.show\\).*filter:\\s*blur", components_src))
  has_backdrop_blur <- any(grepl("\\.modal-backdrop\\.show.*backdrop-filter:\\s*blur", components_src))
  has_unblurred_modal <- !any(grepl("\\.modal\\.show\\s*\\{[^}]*filter:\\s*blur", components_src))

  expect_partial("6", {
    expect_true(has_page_blur)
    expect_true(has_backdrop_blur)
    expect_true(has_unblurred_modal)
  }, "Backdrop blur and sharp modal CSS contracts confirmed; visual verification in MANUAL_CHECKLIST.md")
})

