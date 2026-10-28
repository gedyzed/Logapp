<<<<<<< HEAD
# Shared amount checks. The currency list stays in R/core/config.R.

s <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("")
  trimws(as.character(x[[1]]))
}

num <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.numeric(x)) return(FALSE)
  is.finite(x)
}

# Converts a decimal amount to integer minor units.
minor <- function(x) {
  if (!num(x) || x <= 0 || x > 1e10 || abs(x * 100 - round(x * 100)) > 1e-5) {
    stop("Amount must be positive, at most 10 billion, with at most two decimals.")
  }
  round(x * 100)
}

# Formats minor units for display. This is not the date formatter fmt().
fmt_amount <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) return(character())
  x[!is.na(x) & x == 0] <- 0
  ifelse(
    is.na(x),
    "Unavailable",
    formatC(x / 100, format = "f", digits = 2, big.mark = ",")
  )
}

currency_value <- function(x) {
  if (s(x) %in% CUR) s(x) else "ETB"
}

# Rewrite stored request statuses to the current names. Safe to run more than once.
align_request_statuses <- function() {
  if (!exists("con", mode = "function")) return(invisible(FALSE))
  cn <- con()
  on.exit(dbDisconnect(cn), add = TRUE)
  if (!"expenses" %in% dbListTables(cn)) return(invisible(FALSE))
  dbExecute(cn, "UPDATE expenses SET status = 'under_review' WHERE status = 'waiting'")
  dbExecute(cn, "UPDATE expenses SET status = 'pending' WHERE status = 'under validation'")
  dbExecute(cn, "UPDATE expenses SET status = 'cancelled' WHERE status = 'rejected'")
  invisible(TRUE)
}
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/core/contracts.R
# Owner: Integrator
# Purpose: Define cross-feature data and error contracts before parallel work.
# Expected code: Typed result/error constructors, zero-row prototypes, filter normalization, decimal/UUID serialization conventions and boundary validators.
# Connections: Used by services, repositories, modules and tests. Exact public signatures are recorded in docs/FILE_STRUCTURE.md and implemented here only where genuinely shared.
# Boundary: No miscellaneous helper dumping ground, no SQL or UI. Authoritative money arithmetic stays in PostgreSQL NUMERIC; return decimal text/minor-unit-safe representations, not silently rounded R doubles.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
