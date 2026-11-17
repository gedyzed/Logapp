# Requests page in a real browser, against an isolated SQLite file.
# Not collected by run_tests.R. Run with: Rscript scripts/test_browser.R

test_that("requests page shows the register, filters by status, and opens New Request", {
  problem <- tracker_browser_problem()
  skip_if(nzchar(problem), problem)

  fixture <- local_tracker_db("2026-11-06 13:00:00")
  app <- tracker_drive(fixture$path, "2026-11-06 13:00:00")
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  started <- proc.time()[["elapsed"]]
  tracker_login(app)
  tracker_wait_js(
    app,
    "document.querySelector('.request-table-card') && document.querySelector('.request-table-card').innerText.indexOf('ANCHOR-REVIEW') >= 0 ? 'yes' : 'no'",
    seconds = 25
  )
  login_s <- proc.time()[["elapsed"]] - started
  expect_match(tracker_table_text(app), "ANCHOR-REVIEW", fixed = TRUE)

  tracker_install_busy_probe(app)

  clicked <- proc.time()[["elapsed"]]
  app$click(selector = "#card_status_paid")
  tracker_wait_js(
    app,
    "var text = document.querySelector('.request-table-card') ? document.querySelector('.request-table-card').innerText : ''; text.indexOf('ANCHOR-USD') >= 0 && text.indexOf('ANCHOR-REVIEW') < 0 ? 'yes' : 'no'",
    seconds = 20
  )
  click_s <- proc.time()[["elapsed"]] - clicked
  paid <- tracker_table_text(app)
  expect_match(paid, "ANCHOR-USD", fixed = TRUE)
  expect_false(grepl("ANCHOR-REVIEW", paid, fixed = TRUE))

  opened <- proc.time()[["elapsed"]]
  app$click(selector = "#new_request")
  tracker_wait_js(app, "document.querySelector('#description') ? 'yes' : 'no'", seconds = 20)
  modal_s <- proc.time()[["elapsed"]] - opened
  expect_match(app$get_html(".modal"), "New Request", fixed = TRUE)

  app$run_js("if (window.__busyProbe) { window.__busyProbe.busy = 0; window.__busyProbe.idle = 0; window.__busyProbe.started = performance.now(); }")
  Sys.sleep(3)
  probe <- tracker_busy_probe(app)
  cat(sprintf(
    "\nbrowser timings: login_to_row=%.2fs paid_filter=%.2fs new_request_modal=%.2fs busy_events=%s idle_events=%s over_%.1fs now_busy=%s\n",
    login_s, click_s, modal_s,
    probe$busy, probe$idle, probe$ms / 1000,
    if (isTRUE(probe$now_busy)) "yes" else "no"
  ))

  expect_lt(login_s, 60)
  expect_lt(click_s, 20)
  expect_lt(modal_s, 20)
  expect_true(is.finite(probe$ms) && probe$ms >= 2500)
})
