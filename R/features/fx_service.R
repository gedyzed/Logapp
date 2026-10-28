<<<<<<< HEAD
# Read this month's EuroInfo rates from the database. No rate service is called.
# A missing table or a missing month returns NULL. Finance then counts those amounts as zero.

euroinfo_month_rates <- function(month = format(Sys.Date(), "%Y-%m")) {
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
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/fx_service.R
# Owner: Dagim
# Purpose: Own rate validation, caching, publication and rate-set selection.
# Expected code: Validate complete four-currency sets, select approved immutable sets, freshness checks, explicit refresh and publish orchestration. Rate-table SQL adapters remain local to this file for the baseline.
# Connections: Calls cbe_client and db.R; used by Requests/Manage and seed/test adapters.
# Boundary: No notification-worker dependency. HTTP happens before short publication transaction. Financial conversion is PostgreSQL NUMERIC; do not add an R floating-point financial engine. Split fx_repo only if this file becomes hard to maintain.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
