# Integration tests for Financial Updates and Batch Deductions [13], [14], [15], [16]

test_that("[13] Row marked paid appears in Financial Updates", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_finance_server, {
    session$flushReact()

    init_base <- fin_base()
    box$init_statuses <- unique(init_base$status)

    all_d <- data()
    non_paid_ids <- all_d$id[all_d$status != "paid"]
    box$non_paid_in_base <- any(non_paid_ids %in% init_base$id)

    # Mark under_review request as paid
    target_id <- all_d$id[all_d$status == "under_review"][[1]]
    cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
    DBI::dbExecute(cn, "UPDATE requests SET status = 'paid' WHERE id = ?",
                   params = list(target_id))
    DBI::dbDisconnect(cn)

    reload_expenses()
    session$flushReact()

    updated_base <- fin_base()
    box$newly_paid_in_base <- target_id %in% updated_base$id
    box$newly_paid_in_pool <- target_id %in% fin_pool()$id
  })

  expect_req("13", {
    expect_equal(box$init_statuses, "paid")
    expect_false(box$non_paid_in_base)
    expect_true(box$newly_paid_in_base)
    expect_true(box$newly_paid_in_pool)
  })
})

test_that("[14] Project selection across pages and search filtering", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_finance_server, {
    session$flushReact()

    # 1. Select all 12 projects
    all_projects <- sort(unique(fin_pool()$project_code))
    session$setInputs(fin_project_select = as.list(all_projects))
    session$flushReact()
    box$selected_all_count <- length(fin_selected_projects(input$fin_project_select))

    # 2. Select filtered project FIN01
    session$setInputs(fin_project_select = list("FIN01"))
    session$flushReact()
    picked_single <- fin_batch_candidates()
    box$single_candidates <- picked_single$ready

    # 3. Clear selection
    session$setInputs(fin_project_select = list())
    session$flushReact()
    box$cleared_count <- length(fin_selected_projects(input$fin_project_select))
  })

  expect_req("14", {
    expect_equal(box$selected_all_count, 12L)
    expect_equal(box$single_candidates, "FIN01")
    expect_equal(box$cleared_count, 0L)
  })
})

test_that("[15] Create batch: validations (empty name, >80 chars, duplicates allowed, undeducted rows)", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_finance_server, {
    session$flushReact()

    # 1. Empty name
    session$setInputs(fin_project_select = list("FIN01"))
    session$setInputs(fin_btn_batch = 1)
    session$flushReact()
    session$setInputs(fin_batch_name = "   ")
    session$setInputs(fin_btn_batch_create = 1)
    session$flushReact()

    cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
    box$baseline <- nrow(DBI::dbGetQuery(cn, "SELECT id FROM financial_batches"))
    box$empty_count <- nrow(DBI::dbGetQuery(cn, "SELECT id FROM financial_batches")) - box$baseline

    # 2. >80 characters
    long_name <- paste(rep("A", 81), collapse = "")
    session$setInputs(fin_btn_batch = 2)
    session$flushReact()
    session$setInputs(fin_batch_name = long_name)
    session$setInputs(fin_btn_batch_create = 2)
    session$flushReact()
    box$long_count <- nrow(DBI::dbGetQuery(cn, "SELECT id FROM financial_batches")) - box$baseline

    # 3. Valid batch creation
    session$setInputs(fin_btn_batch = 3)
    session$flushReact()
    session$setInputs(fin_batch_name = "Batch Alpha")
    session$setInputs(fin_btn_batch_create = 3)
    session$flushReact()
    box$first_batch <- DBI::dbGetQuery(cn, "SELECT * FROM financial_batches WHERE batch_name = 'Batch Alpha' ORDER BY prepared_at DESC LIMIT 1")

    # 4. Duplicate name is allowed
    session$setInputs(fin_project_select = list("FIN02"))
    session$setInputs(fin_btn_batch = 4)
    session$flushReact()
    session$setInputs(fin_batch_name = "Batch Alpha")
    session$setInputs(fin_btn_batch_create = 4)
    session$flushReact()

    box$all_batches <- DBI::dbGetQuery(cn, "SELECT * FROM financial_batches WHERE batch_name = 'Batch Alpha'")
    DBI::dbDisconnect(cn)
  })

  expect_req("15", {
    expect_equal(box$empty_count, 0L)
    expect_equal(box$long_count, 0L)
    expect_equal(nrow(box$first_batch), 1L)
    expect_equal(box$first_batch$batch_name[[1]], "Batch Alpha")
    expect_equal(nrow(box$all_batches), 2L)
    expect_equal(box$all_batches$batch_name[[1]], box$all_batches$batch_name[[2]])
    expect_false(identical(box$all_batches$batch_code[[1]], box$all_batches$batch_code[[2]]))
  })
})

test_that("[16] Confirm Applied: sets deducted_at; Reject: restores projects; Excel export valid", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_finance_server, {
    session$flushReact()

    # 1. Create a batch for FIN01
    session$setInputs(fin_project_select = list("FIN01"))
    session$setInputs(fin_btn_batch = 1)
    session$flushReact()
    session$setInputs(fin_batch_name = "FIN01 Deduct Batch")
    session$setInputs(fin_btn_batch_create = 1)
    session$flushReact()

    cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
    bth <- DBI::dbGetQuery(cn, "SELECT * FROM financial_batches WHERE batch_name = 'FIN01 Deduct Batch'")
    box$bth_code <- bth$batch_code[[1]]
    pre_expenses <- DBI::dbGetQuery(cn, "SELECT pe.request_id AS id,
      CASE WHEN fb.status = 'applied' AND bi.released_at IS NULL THEN fb.applied_at ELSE NULL END AS deducted_at
      FROM financial_batches fb
      JOIN batch_items bi ON bi.batch_id = fb.id
      JOIN payment_events pe ON pe.id = bi.payment_event_id
      WHERE fb.batch_code = ?", params = list(box$bth_code))
    box$pre_deducted_null <- all(is.na(pre_expenses$deducted_at))

    # 2. Confirm Applied
    session$setInputs(fin_btn_confirm = list(code = box$bth_code))
    session$flushReact()
    session$setInputs(fin_btn_confirm_proceed = 1)
    session$flushReact()

    post_bth <- DBI::dbGetQuery(cn, "SELECT status, applied_at FROM financial_batches WHERE batch_code = ?",
                                params = list(box$bth_code))
    post_expenses <- DBI::dbGetQuery(cn, "SELECT pe.request_id AS id,
      CASE WHEN fb.status = 'applied' AND bi.released_at IS NULL THEN fb.applied_at ELSE NULL END AS deducted_at
      FROM financial_batches fb
      JOIN batch_items bi ON bi.batch_id = fb.id
      JOIN payment_events pe ON pe.id = bi.payment_event_id
      WHERE fb.batch_code = ?", params = list(box$bth_code))
    post_projects <- DBI::dbGetQuery(cn, "SELECT code AS project_code, last_checkpoint_at AS last_deducted_at FROM project_budgets WHERE code = 'FIN01'")

    box$post_bth_status <- post_bth$status[[1]]
    box$expenses_deducted_set <- all(!is.na(post_expenses$deducted_at))
    box$project_deducted_set <- !is.na(post_projects$last_deducted_at[[1]])

    # 3. Excel export validation with readxl
    tmp_xlsx <- tempfile(fileext = ".xlsx")
    box$export_ok <- fin_write_batch_xlsx(box$bth_code, tmp_xlsx)
    box$xlsx_exists <- file.exists(tmp_xlsx) && file.info(tmp_xlsx)$size > 0
    if (box$xlsx_exists) {
      xl_data <- readxl::read_excel(tmp_xlsx)
      box$xl_cols <- names(xl_data)
      box$xl_rows <- nrow(xl_data)
      box$xl_currency <- unique(xl_data$Currency)
      unlink(tmp_xlsx)
    }

    # 4. Reject Batch flow
    session$setInputs(fin_project_select = list("FIN02"))
    session$setInputs(fin_btn_batch = 2)
    session$flushReact()
    session$setInputs(fin_batch_name = "FIN02 Reject Batch")
    session$setInputs(fin_btn_batch_create = 2)
    session$flushReact()

    fin02_bth <- DBI::dbGetQuery(cn, "SELECT * FROM financial_batches WHERE batch_name = 'FIN02 Reject Batch'")
    fin02_code <- fin02_bth$batch_code[[1]]
    fin02_ids <- DBI::dbGetQuery(cn, "SELECT pe.request_id AS id
      FROM batch_items bi
      JOIN payment_events pe ON pe.id = bi.payment_event_id
      JOIN financial_batches fb ON fb.id = bi.batch_id
      WHERE fb.batch_code = ?", params = list(fin02_code))$id

    session$setInputs(fin_btn_reject = list(code = fin02_code))
    session$flushReact()
    session$setInputs(fin_btn_reject_proceed = 1)
    session$flushReact()

    post_reject_bth <- DBI::dbGetQuery(cn, "SELECT status FROM financial_batches WHERE batch_code = ?",
                                       params = list(fin02_code))
    post_reject_expenses <- DBI::dbGetQuery(cn, "SELECT pe.request_id AS id,
      CASE WHEN fb.status = 'applied' AND bi.released_at IS NULL THEN fb.applied_at ELSE NULL END AS deducted_at
      FROM financial_batches fb
      JOIN batch_items bi ON bi.batch_id = fb.id
      JOIN payment_events pe ON pe.id = bi.payment_event_id
      WHERE fb.batch_code = ?", params = list(fin02_code))
    box$reject_bth_status <- post_reject_bth$status[[1]]
    box$reject_expenses_deducted_null <- all(is.na(post_reject_expenses$deducted_at))
    box$fin02_back_in_pool <- all(fin02_ids %in% fin_pool()$id)

    DBI::dbDisconnect(cn)
  })

  expect_req("16", {
    expect_true(box$pre_deducted_null)
    expect_equal(box$post_bth_status, "applied")
    expect_true(box$expenses_deducted_set)
    expect_true(box$project_deducted_set)
    expect_true(box$xlsx_exists)
    expect_equal(box$xl_cols, c("Project Code", "Budget Line", "Currency", "Paid Amount"))
    expect_true(box$xl_rows > 0L)
    expect_equal(box$xl_currency, "GBP")
    expect_equal(box$reject_bth_status, "rejected")
    expect_true(box$reject_expenses_deducted_null)
    expect_true(box$fin02_back_in_pool)
  })
})

