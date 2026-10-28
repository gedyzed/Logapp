# Constants and environment-backed settings. No SQL.
# CUR and RATES are the startup fallback. load.R replaces them with active
# currency rows when the database has any.

options(scipen = 999)

configured_db <- trimws(Sys.getenv("TRACKER_DB_PATH", unset = ""))
db_target <- if (nzchar(configured_db)) path.expand(configured_db) else file.path(APP_DIR, "tracker.db")
if (nzchar(configured_db) && !file.exists(db_target)) {
  stop("TRACKER_DB_PATH points to a database file that does not exist: ", db_target,
       ". Correct the path or unset TRACKER_DB_PATH to use tracker.db beside the app.", call. = FALSE)
}
if (!dir.exists(dirname(db_target))) {
  stop("The folder containing the SQLite database does not exist: ", dirname(db_target), call. = FALSE)
}
DB <- normalizePath(db_target, winslash = "/", mustWork = FALSE)
message("Expense Tracker SQLite database: ", DB)

CUR    <- c("ETB", "USD", "EUR", "GBP")
STATUS <- c("waiting", "under validation", "paid", "rejected")
DEPTS  <- c("REACH", "PANDA", "ACTED")

# values the new-request form starts with
DEFAULTS <- list(submitted_by = "Betty T", currency = "ETB", country = "ETH")

SCEN_NAME <- c(
  A = "Direct purchase", B = "Quotations", C = "Contract",
  D = "National/Local call for tender", E = "International call for tender",
  G = "Framework agreement"
)
LEAD <- c(A = "< 1 week", B = "~2-3 weeks", C = "~2-3 weeks",
          D = "~1.5 months", E = "~2 months", G = "~2 months")
# Units per 1 USD. VERIFY these before real use. Each request stores its own rate.
RATES <- c(ETB = 155, USD = 1, EUR = 0.92, GBP = 0.79)

STATUS_COL <- c(waiting = "#94a3b8", `under validation` = "#f59e0b",
                paid = "#16a34a", rejected = "#dc2626")
# Department pie colors: preserve ACTED's blue, use red for REACH and green for PANDA.
DEPT_PIE_COL <- c(REACH = "#e94f52", PANDA = "#006b45", ACTED = "#0284c7")
DIMS <- c(Project = "project_code", Requester = "requested_by",
          Department = "department")
