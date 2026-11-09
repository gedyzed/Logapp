# Integration tests for Insights [18], [19], [20]

test_that("[18] Chart totals and KPI cards match underlying rows in active currency", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_insights_server, {
    # ETB currency
    session$setInputs(global_cur = "ETB", dash_period_type = "year", dash_year = "2026")
    session$flushReact()

    paid_d <- dashboard_paid_filtered()
    all_d <- dashboard_filtered()
    box$paid_count <- nrow(paid_d)
    box$paid_sum_etb <- sum(paid_d$usd * RATES[["ETB"]])
    box$open_count <- sum(all_d$status %in% c("under_review", "pending"))
    box$review_count <- sum(all_d$status == "under_review")
    box$pending_count <- sum(all_d$status == "pending")

    # USD currency
    session$setInputs(global_cur = "USD")
    session$flushReact()
    box$paid_sum_usd <- sum(paid_d$usd * RATES[["USD"]])
  })

  expect_req("18", {
    expect_true(box$paid_count > 0L)
    expect_equal(box$paid_sum_etb, box$paid_sum_usd * 155)
    expect_equal(box$open_count, box$review_count + box$pending_count)
  })
})

test_that("[19] Days to complete reactive pipeline", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_insights_server, {
    session$setInputs(dash_period_type = "month", dash_year = "2026", dash_month = "10")
    session$flushReact()

    paid_oct <- dashboard_paid_filtered()
    order_d <- suppressWarnings(as.Date(paid_oct$order_date))
    pay_d <- suppressWarnings(as.Date(paid_oct$payment_date))
    ok <- !is.na(order_d) & !is.na(pay_d)
    actual <- as.numeric(pay_d[ok] - order_d[ok])
    scen <- row_scenario(paid_oct[ok, , drop = FALSE])
    expected <- vapply(scen, function(s) {
      if (s %in% names(SCENARIO_DAYS)) as.numeric(SCENARIO_DAYS[[s]]) else NA_real_
    }, numeric(1))

    box$actual <- actual
    box$expected <- expected
    box$on_time <- !is.na(expected) & actual <= expected
    box$delay <- actual - expected
    box$avg_delay <- mean(box$delay, na.rm = TRUE)
    box$on_time_pct <- sum(box$on_time) / length(box$on_time)
  })

  expect_req("19", {
    expect_equal(length(box$actual), 3L)
    expect_equal(unname(box$expected), c(7, 7, 7))
    ord <- order(box$actual)
    expect_equal(unname(box$actual[ord]), c(2, 7, 19))
    expect_equal(unname(box$on_time[ord]), c(TRUE, TRUE, FALSE))
    expect_equal(box$on_time_pct, 2 / 3)
    expect_equal(unname(box$delay[ord]), c(-5, 0, 12))
    expect_equal(box$avg_delay, 7 / 3, tolerance = 1e-4)
  })
})

test_that("[20] Insights and Requests filter resets operate independently", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_insights_server, {
    session$setInputs(
      f_month = "2026-11",
      f_project = "ALPHA",
      f_query = "SEARCH_TERM",
      dash_project = "FIN01",
      dash_department = "PANDA"
    )
    session$flushReact()

    session$setInputs(insights_clear = 1)
    session$flushReact()

    box$f_month_after_insights_clear <- input$f_month
    box$f_project_after_insights_clear <- input$f_project
    box$f_query_after_insights_clear <- input$f_query

    session$setInputs(dash_project = "FIN01", dash_department = "PANDA")
    session$setInputs(reset = 1)
    session$flushReact()

    box$dash_project_after_req_reset <- input$dash_project
    box$dash_dept_after_req_reset <- input$dash_department
  })

  expect_req("20", {
    expect_equal(box$f_month_after_insights_clear, "2026-11")
    expect_equal(box$f_project_after_insights_clear, "ALPHA")
    expect_equal(box$f_query_after_insights_clear, "SEARCH_TERM")
    expect_equal(box$dash_project_after_req_reset, "FIN01")
    expect_equal(box$dash_dept_after_req_reset, "PANDA")
  })
})

