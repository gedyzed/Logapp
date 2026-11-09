# Read this month's EuroInfo rates from the database. No rate service is called.
# A missing table or a missing month returns NULL. Finance then counts those amounts as zero.

euroinfo_month_rates <- function(month = format(app_today(), "%Y-%m")) {
  if (!exists("con", mode = "function")) return(NULL)
  cn <- tryCatch(con(), error = function(e) NULL)
  if (is.null(cn)) return(NULL)
  on.exit(try(dbDisconnect(cn), silent = TRUE), add = TRUE)

  tables <- tryCatch(dbListTables(cn), error = function(e) character())
  if (!"fx" %in% tables) return(NULL)
  fields <- tryCatch(dbListFields(cn, "fx"), error = function(e) character())
  if (!"currency" %in% fields || !"etb" %in% fields) return(NULL)

  month_col <- intersect(c("rate_month", "month", "period"), fields)[1]
  date_col <- intersect(c("rate_date", "fetched_at"), fields)[1]
  sql <- "SELECT currency, etb AS rate FROM fx WHERE etb > 0"
  params <- list()
  if ("source" %in% fields) sql <- paste(sql, "AND LOWER(source) LIKE '%euroinfo%'")
  if (!is.na(month_col) && nzchar(month_col)) {
    sql <- paste(sql, "AND", month_col, "= ?")
    params <- c(params, list(month))
  } else if (!is.na(date_col) && nzchar(date_col)) {
    sql <- paste(sql, "AND substr(", date_col, ", 1, 7) = ?")
    params <- c(params, list(month))
  } else {
    return(NULL)
  }

  rows <- tryCatch(dbGetQuery(cn, sql, params = params), error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(NULL)
  rates <- stats::setNames(as.numeric(rows$rate), toupper(trimws(rows$currency)))
  rates <- rates[is.finite(rates) & rates > 0]
  if (!length(rates)) NULL else rates
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


