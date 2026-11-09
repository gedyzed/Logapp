# Unit tests for Weekly Report requirements [1] and [2]

test_that("[1] Weekly report window calculation (Fri 13:00 to Sat 17:00 EAT)", {
  # Friday 12:59:59 EAT -> FALSE
  expect_false(weekly_window_expected(as.POSIXct("2026-11-06 12:59:59", tz = "Africa/Addis_Ababa")))
  # Friday 13:00:00 EAT -> TRUE
  expect_true(weekly_window_expected(as.POSIXct("2026-11-06 13:00:00", tz = "Africa/Addis_Ababa")))
  # Saturday 16:59:59 EAT -> TRUE
  expect_true(weekly_window_expected(as.POSIXct("2026-11-07 16:59:59", tz = "Africa/Addis_Ababa")))
  # Saturday 17:00:00 EAT -> FALSE
  expect_false(weekly_window_expected(as.POSIXct("2026-11-07 17:00:00", tz = "Africa/Addis_Ababa")))
  # Sunday through Thursday -> FALSE
  expect_false(weekly_window_expected(as.POSIXct("2026-11-08 14:00:00", tz = "Africa/Addis_Ababa"))) # Sunday
  expect_false(weekly_window_expected(as.POSIXct("2026-11-02 14:00:00", tz = "Africa/Addis_Ababa"))) # Monday
  expect_false(weekly_window_expected(as.POSIXct("2026-11-04 14:00:00", tz = "Africa/Addis_Ababa"))) # Wednesday
  expect_false(weekly_window_expected(as.POSIXct("2026-11-05 14:00:00", tz = "Africa/Addis_Ababa"))) # Thursday

  record_requirement("1", "PASS", "Weekly window pure math verified")
})

test_that("[2] Weekly detail definition excludes edit, submit, cancel, delete, status controls", {
  root <- tracker_root()
  modals_src <- readLines(file.path(root, "R/features/requests_modals.R"))
  
  # Locate weekly detail modal definition
  weekly_start <- grep("open_weekly_row", modals_src)[[1]]
  weekly_lines <- modals_src[weekly_start:min(length(modals_src), weekly_start + 60)]
  
  # Ensure no edit, submit, cancel, or delete action buttons are created in weekly modal
  expect_false(any(grepl('actionButton\\(".*(edit|submit|cancel|delete).*\\)', weekly_lines, ignore.case = TRUE)))
  # Ensure no status input is created in weekly modal
  expect_false(any(grepl('selectInput\\(".*status.*\\)', weekly_lines, ignore.case = TRUE)))

  record_partial("2", "Weekly modal definition confirmed read-only in unit test; visual confirmation in MANUAL_CHECKLIST.md")
})

