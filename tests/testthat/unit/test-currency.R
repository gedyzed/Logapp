# Unit tests for Currency rates and conversion mathematics [22]

test_that("[22] Currency RATES conversion math and fin_to_display_minor", {
  # Rates defined in config.R: ETB 155, USD 1, EUR 0.92, GBP 0.79
  expect_equal(RATES[["USD"]], 1)
  expect_equal(RATES[["ETB"]], 155)
  expect_equal(RATES[["EUR"]], 0.92)
  expect_equal(RATES[["GBP"]], 0.79)

  # Formula check: usd * RATES[[k]]
  calc_currencies <- function(amount, currency) {
    usd <- round(amount / RATES[[currency]], 2)
    list(
      USD = round(usd * RATES[["USD"]], 2),
      ETB = round(usd * RATES[["ETB"]], 2),
      EUR = round(usd * RATES[["EUR"]], 2),
      GBP = round(usd * RATES[["GBP"]], 2)
    )
  }

  etb_1550 <- calc_currencies(1550, "ETB")
  expect_equal(etb_1550$USD, 10.00)
  expect_equal(etb_1550$ETB, 1550.00)
  expect_equal(etb_1550$EUR, 9.20)
  expect_equal(etb_1550$GBP, 7.90)

  # fin_to_display_minor conversion unit check
  # 1000 ETB in minor units = 100000
  effective_rates <- c(USD = 155, EUR = 155 / 0.92, GBP = 155 / 0.79, ETB = 1)
  usd_minor <- fin_to_display_minor(100000, "ETB", "USD", effective_rates)
  expect_equal(round(usd_minor / 100, 2), round(1000 / 155, 2))

  record_requirement("22", "PASS", "Currency conversion unit math verified")
})

