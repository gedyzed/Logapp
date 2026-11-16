# Read this month's EuroInfo rates from the database. No rate service is called.
# A missing table or a missing month returns NULL. Finance then counts those amounts as zero.

euroinfo_month_rates <- function(month = format(app_today(), "%Y-%m")) {
  row <- tryCatch(
    db_get("SELECT etb_per_eur, usd_per_eur, gbp_per_eur FROM fx_rate_months WHERE rate_month = ?", month),
    error = function(e) NULL
  )
  if (is.null(row) || !nrow(row)) return(NULL)
  etb <- suppressWarnings(as.numeric(row$etb_per_eur[[1]]))
  usd <- suppressWarnings(as.numeric(row$usd_per_eur[[1]]))
  gbp <- suppressWarnings(as.numeric(row$gbp_per_eur[[1]]))
  if (!all(is.finite(c(etb, usd, gbp))) || any(c(etb, usd, gbp) <= 0)) return(NULL)
  c(ETB = 1, USD = etb / usd, EUR = etb, GBP = etb / gbp)
}

# Return EuroInfo rates if available in the database; otherwise fall back to default
# conversion rates (from RATES in config.R) so finance calculations and exports work
# smoothly until EuroInfo tables are loaded.
fin_effective_rates <- function(month = format(app_today(), "%Y-%m")) {
  rates <- euroinfo_month_rates(month)
  default_rates <- c(
    USD = unname(RATES[["ETB"]] / RATES[["USD"]]),
    EUR = unname(RATES[["ETB"]] / RATES[["EUR"]]),
    GBP = unname(RATES[["ETB"]] / RATES[["GBP"]]),
    ETB = 1
  )
  if (is.null(rates)) return(default_rates)
  for (k in names(default_rates)) {
    if (!k %in% names(rates) || is.na(rates[[k]]) || rates[[k]] <= 0) {
      rates[[k]] <- default_rates[[k]]
    }
  }
  rates
}


