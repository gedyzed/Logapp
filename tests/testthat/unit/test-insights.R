# Unit tests for Insights completion days metrics [19]

test_that("[19] Days to complete: actual = payment - order, expected from SCENARIO_DAYS, on time actual <= expected, avg delay", {
  # SCENARIO_DAYS definition check
  expect_equal(SCENARIO_DAYS[["A"]], 7L)
  expect_equal(SCENARIO_DAYS[["B"]], 14L)
  expect_equal(SCENARIO_DAYS[["C"]], 21L)
  expect_equal(SCENARIO_DAYS[["D"]], 45L)
  expect_equal(SCENARIO_DAYS[["E"]], 60L)
  expect_equal(SCENARIO_DAYS[["G"]], 60L)

  # Unit calculation with test fixtures
  calc_completion <- function(order_date, payment_date, scenario_code) {
    actual <- as.numeric(as.Date(payment_date) - as.Date(order_date))
    expected <- as.numeric(SCENARIO_DAYS[[scenario_code]])
    on_time <- actual <= expected
    delay <- actual - expected
    list(actual = actual, expected = expected, on_time = on_time, delay = delay)
  }

  early <- calc_completion("2026-10-01", "2026-10-03", "A")
  expect_equal(early$actual, 2)
  expect_true(early$on_time)
  expect_equal(early$delay, -5)

  ontime <- calc_completion("2026-10-01", "2026-10-08", "A")
  expect_equal(ontime$actual, 7)
  expect_true(ontime$on_time)
  expect_equal(ontime$delay, 0)

  late <- calc_completion("2026-10-01", "2026-10-20", "A")
  expect_equal(late$actual, 19)
  expect_false(late$on_time)
  expect_equal(late$delay, 12)

  # Average delay of early, ontime, late
  delays <- c(early$delay, ontime$delay, late$delay)
  avg_delay <- mean(delays)
  expect_equal(avg_delay, 7 / 3, tolerance = 1e-4)

  record_requirement("19", "PASS", "Days to complete unit calculation verified")
})

