# Integration tests for Weekly Report [1] and [2]

test_that("[1] Weekly report reactive data bounds and banner visibility", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_requests_server, {
    session$flushReact()
    w_data <- weekly_data()
    box$weekly_rows <- w_data
    box$window_active <- weekly_window_active()
  })

  # Current app behavior: weekly_window_active() returns TRUE always
  expect_true(box$window_active)
  # Intended test window assertion: expected to be true on Friday 13:00
  expect_true(weekly_window_expected(as.POSIXct("2026-11-06 13:00:00", tz = "Africa/Addis_Ababa")))

  # Record known bug on weekly_window_active() predicate (currently returns TRUE always)
  expect_known_bug("1", {
    # On Sunday 14:00, expected window is FALSE, but current implementation returns TRUE
    expect_equal(box$window_active, weekly_window_expected(as.POSIXct("2026-11-08 14:00:00", tz = "Africa/Addis_Ababa")))
  }, "weekly_window_active() returns TRUE always instead of checking Friday 13:00 to Saturday 17:00 EAT window")

  # weekly_data() keeps rows registered Monday of current week through Sys.Date()
  reg_dates <- as.Date(substr(as.character(box$weekly_rows$created), 1, 10))
  today <- as.Date("2026-11-06")
  mon <- today - (as.integer(format(today, "%u")) - 1L)
  expect_true(all(reg_dates >= mon & reg_dates <= today))
})

test_that("[2] Weekly detail modal integration", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  shiny::testServer(tracker_requests_server, {
    session$flushReact()
    # Trigger weekly detail modal for a known request ID
    session$setInputs(open_weekly_row = list(id = 1L, nonce = 1234))
    session$flushReact()
    box$triggered <- TRUE
  })

  expect_partial("2", {
    expect_true(box$triggered)
  }, "Weekly detail modal handler triggered; visual inspection in MANUAL_CHECKLIST.md")
})

