# Check dependencies up front so the R console reports actionable startup errors.
required_packages <- c("shiny", "bslib", "DBI", "RSQLite", "DT", "dplyr",
                       "writexl", "ggplot2", "scales", "shinymanager")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    paste0(
      "Expense Tracker cannot start because these R packages are missing: ",
      paste(missing_packages, collapse = ", "),
      ". Install them with: install.packages(c(",
      paste(sprintf("\"%s\"", missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

# Literal library() calls so Posit Connect's scanner installs these packages.
# A character vector passed to library() is invisible to that scan.
library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(writexl)
library(ggplot2)
library(shinymanager)

needed_bslib_exports <- c("page_navbar", "nav_panel", "navset_tab", "card",
                          "card_header", "input_dark_mode")
missing_bslib_exports <- setdiff(needed_bslib_exports, getNamespaceExports("bslib"))
if (length(missing_bslib_exports)) {
  stop(
    paste0("Your installed bslib package is too old or incomplete. Missing: ",
           paste(missing_bslib_exports, collapse = ", "),
           ". Update it with install.packages('bslib'), restart R, and run the app again."),
    call. = FALSE
  )
}

options(scipen = 999)          # never show scientific notation

# Use one persistent SQLite database for this application. No demo database fallback.
script_file <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  candidates <- character()
  if (length(file_arg)) candidates <- c(candidates, sub("^--file=", "", file_arg[[1]]))
  frames <- sys.frames()
  ofiles <- vapply(frames, function(fr) {
    if (!is.null(fr$ofile)) as.character(fr$ofile) else ""
  }, character(1))
  candidates <- c(candidates, rev(ofiles[nzchar(ofiles)]))
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  preferred <- existing[basename(existing) %in% c("app.R", "expense_tracker_refactored.R")]
  if (length(preferred)) preferred[[1]] else if (length(existing)) existing[[1]] else ""
})
configured_app_dir <- if (exists("APP_DIR", inherits = TRUE) &&
                            is.character(get("APP_DIR", inherits = TRUE)) &&
                            length(get("APP_DIR", inherits = TRUE)) &&
                            nzchar(get("APP_DIR", inherits = TRUE)[[1]])) {
  get("APP_DIR", inherits = TRUE)[[1]]
} else if (nzchar(script_file) && file.exists(script_file)) {
  dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
# By default, use tracker.db beside app.R. An explicit path can be supplied when
# launching the app, which avoids accidentally opening a different/empty database.
configured_db <- trimws(Sys.getenv("TRACKER_DB_PATH", unset = ""))
db_target <- if (nzchar(configured_db)) path.expand(configured_db) else file.path(configured_app_dir, "tracker.db")
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
STATUS <- c("under_review", "pending", "paid", "cancelled")
DEPTS  <- c("REACH", "PANDA", "ACTED")


# ---- Authentication credentials ----
# Single shared credential for all authorized users
AUTH_CREDENTIALS <- data.frame(
  user = "acted",
  password = "logapp",
  admin = FALSE,
  stringsAsFactors = FALSE
)
# values the new-request form starts with
DEFAULTS <- list(submitted_by = "Betty T", currency = "ETB", country = "ETH")

SCEN_NAME <- c(
  A = "Direct purchase", B = "Quotations", C = "Contract",
  D = "National/Local call for tender", E = "International call for tender",
  G = "Framework agreement"
)
LEAD <- c(A = "< 1 week", B = "~2-3 weeks", C = "~2-3 weeks",
          D = "~1.5 months", E = "~2 months", G = "~2 months")
SCENARIO_DAYS <- c(A = 7L, B = 14L, C = 21L, D = 45L, E = 60L, G = 60L)
# Units per 1 USD. VERIFY these before real use. Each request stores its own rate.
RATES <- c(ETB = 155, USD = 1, EUR = 0.92, GBP = 0.79)

STATUS_COL <- c(under_review = "#94a3b8", pending = "#f59e0b",
                paid = "#16a34a", cancelled = "#dc2626")
# Department pie colors: preserve ACTED's blue, use red for REACH and green for PANDA.
DEPT_PIE_COL <- c(REACH = "#e94f52", PANDA = "#006b45", ACTED = "#0284c7")
DIMS <- c(Project = "project_code", Requester = "requested_by",
          Department = "department")

`%||%` <- function(a, b) if (is.null(a)) b else a

month_choices <- function(month_values = NULL) {
  if (is.null(month_values)) {
    yr <- format(Sys.Date(), "%Y")
    month_values <- sprintf("%s-%02d", yr, 1:12)
  }
  month_values <- sort(unique(as.character(month_values[!is.na(month_values) & nzchar(month_values)])),
                       decreasing = FALSE)
  if (!length(month_values)) return(character())
  setNames(month_values, format(as.Date(paste0(month_values, "-01")), "%B %Y"))
}

# A <500 | B 500-<5k | C 5k-<30k | D 30k-<150k | E >=150k | G if framework
scenario <- function(usd, waiver) {
  usd    <- as.numeric(usd)
  waiver <- as.character(waiver)
  base <- as.character(cut(usd, c(-Inf, 500, 5000, 30000, 150000, Inf),
                           labels = c("A", "B", "C", "D", "E"), right = FALSE))
  as.character(ifelse(waiver %in% "Framework agreement", "G", base))
}

