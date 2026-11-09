# Integration tests for Global Currency Switcher [22]

test_that("[22] Global currency switcher updates rates, Financial Updates, Insights, and Excel export", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  # 1. Requests columns gain ETB/USD/EUR/GBP computed with usd * RATES[[k]]
  shiny::testServer(tracker_requests_server, {
    session$flushReact()
    d <- data()
    box$names <- names(d)
    box$etb_col <- d$ETB
    box$usd_col <- d$USD
    box$eur_col <- d$EUR
    box$gbp_col <- d$GBP
    box$usd_base <- d$usd
    box$currencies <- d$currency
  })

  # 2. Financial Updates: fin_to_display_minor with fallback and EuroInfo
  shiny::testServer(tracker_finance_server, {
    session$flushReact()
    session$setInputs(global_cur = "ETB")
    session$flushReact()
    etb_base <- fin_base()
    box$fin_etb_minor <- sum(etb_base$display_minor)

    session$setInputs(global_cur = "USD")
    session$flushReact()
    usd_base <- fin_base()
    box$fin_usd_minor <- sum(usd_base$display_minor)

    session$setInputs(global_cur = "GBP")
    session$flushReact()
    gbp_base <- fin_base()
    box$fin_gbp_minor <- sum(gbp_base$display_minor)

    rates_fallback <- fin_effective_rates("2099-01")
    box$fallback_keys <- names(rates_fallback)
    box$fallback_usd <- rates_fallback[["USD"]]
  })

  # 3. Insights: sums the active currency column
  shiny::testServer(tracker_insights_server, {
    session$flushReact()
    session$setInputs(dash_period_type = "year", dash_year = "2026")

    session$setInputs(global_cur = "ETB")
    session$flushReact()
    paid_etb <- dashboard_paid_filtered()
    box$insights_etb_sum <- sum(paid_etb$ETB)

    session$setInputs(global_cur = "USD")
    session$flushReact()
    paid_usd <- dashboard_paid_filtered()
    box$insights_usd_sum <- sum(paid_usd$USD)

    session$setInputs(global_cur = "EUR")
    session$flushReact()
    paid_eur <- dashboard_paid_filtered()
    box$insights_eur_sum <- sum(paid_eur$EUR)

    session$setInputs(global_cur = "GBP")
    session$flushReact()
    paid_gbp <- dashboard_paid_filtered()
    box$insights_gbp_sum <- sum(paid_gbp$GBP)
  })

  # 4. Excel export format
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  shiny::testServer(tracker_finance_server, {
    session$flushReact()
    session$setInputs(fin_project_select = list("FIN01"))
    session$setInputs(fin_btn_batch = 1)
    session$flushReact()
    session$setInputs(fin_batch_name = "Currency Export")
    session$setInputs(fin_btn_batch_create = 1)
    session$flushReact()

    batches <- fin_load_batches()
    code <- batches$batch_code[[1]]
    box$export_res <- fin_write_batch_xlsx(code, tmp_xlsx)
  })
  box$export_exists <- file.exists(tmp_xlsx) && file.info(tmp_xlsx)$size > 0
  if (box$export_exists) {
    xl <- readxl::read_excel(tmp_xlsx)
    box$export_cols <- names(xl)
    box$export_curr <- unique(xl$Currency)
    unlink(tmp_xlsx)
  }

  # 5. Requests table and weekly report show the selected currency only.
  # The stored amount and currency columns stay unchanged.
  shiny::testServer(tracker_insights_server, {
    session$setInputs(global_cur = "USD")
    session$flushReact()
    table_data <- filtered()
    anchor_row <- table_data[table_data$description == "ANCHOR-REVIEW", , drop = FALSE]
    box$anchor_stored_cur <- anchor_row$currency[[1]]
    box$anchor_stored_amt <- anchor_row$amount[[1]]
    box$anchor_label <- selected_amount_label(anchor_row)
    weekly_anchor <- weekly_data()
    weekly_anchor <- weekly_anchor[weekly_anchor$description == "ANCHOR-REVIEW", , drop = FALSE]
    box$weekly_stored_cur <- weekly_anchor$currency[[1]]
    box$weekly_label <- selected_amount_label(weekly_anchor)
  })

  expect_req("22", {
    # 1. Currency columns added
    expect_true(all(c("ETB", "USD", "EUR", "GBP") %in% box$names))
    expect_equal(box$etb_col, ifelse(box$currencies == "ETB", box$etb_col, round(box$usd_base * RATES[["ETB"]], 2)))
    expect_equal(box$usd_col, ifelse(box$currencies == "USD", box$usd_col, round(box$usd_base * RATES[["USD"]], 2)))
    expect_equal(box$eur_col, ifelse(box$currencies == "EUR", box$eur_col, round(box$usd_base * RATES[["EUR"]], 2)))
    expect_equal(box$gbp_col, ifelse(box$currencies == "GBP", box$gbp_col, round(box$usd_base * RATES[["GBP"]], 2)))

    # 2. Financial Updates conversion
    expect_true(box$fin_etb_minor > box$fin_usd_minor)
    expect_equal(box$fallback_keys, c("USD", "EUR", "GBP", "ETB"))
    expect_equal(box$fallback_usd, 155)

    # 3. Insights sums the active currency column
    expect_true(box$insights_etb_sum > box$insights_usd_sum)
    expect_true(box$insights_etb_sum > box$insights_eur_sum)
    expect_true(box$insights_etb_sum > box$insights_gbp_sum)

    # 4. Excel export format
    expect_true(box$export_exists)
    expect_equal(box$export_cols, c("Project Code", "Budget Line", "Currency", "Paid Amount"))
    expect_equal(box$export_curr, "GBP")

    # 5. Display uses the selected currency. Stored amount and currency stay put.
    expect_equal(box$anchor_stored_cur, "ETB")
    expect_equal(box$anchor_stored_amt, 1550)
    expect_equal(box$anchor_label, paste("USD", money(10)))
    expect_false(grepl("ETB", box$anchor_label))
    expect_equal(box$weekly_stored_cur, "ETB")
    expect_equal(box$weekly_label, paste("USD", money(10)))
    expect_false(grepl("ETB", box$weekly_label))
  })
})

