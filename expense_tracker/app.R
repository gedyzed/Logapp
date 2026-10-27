# Check dependencies up front so the R console reports actionable startup errors.
required_packages <- c("shiny", "bslib", "DBI", "RSQLite", "DT", "dplyr",
                       "writexl", "ggplot2", "scales")
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

invisible(lapply(c("shiny", "bslib", "DBI", "RSQLite", "DT", "dplyr",
                   "writexl", "ggplot2"), library, character.only = TRUE))

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
APP_DIR <- if (nzchar(script_file) && file.exists(script_file)) {
  dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
# By default, use tracker.db beside app.R. An explicit path can be supplied when
# launching the app, which avoids accidentally opening a different/empty database.
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

`%||%` <- function(a, b) if (is.null(a)) b else a

month_choices <- function(month_values = NULL) {
  if (is.null(month_values)) {
    month_values <- format(seq(as.Date(format(Sys.Date(), "%Y-%m-01")),
                               by = "-1 month", length.out = 24), "%Y-%m")
  }
  month_values <- sort(unique(as.character(month_values[!is.na(month_values) & nzchar(month_values)])),
                       decreasing = TRUE)
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

# ---- number formatting -----------------------------------------------------
abbr <- function(x) {   # 1,250,000 -> 1.25M
  tr <- function(y, suf) paste0(sub("\\.?0+$", "", formatC(y, format = "f", digits = 2)), suf)
  ifelse(abs(x) >= 1e9, tr(x / 1e9, "B"),
         ifelse(abs(x) >= 1e6, tr(x / 1e6, "M"),
                ifelse(abs(x) >= 1e3, tr(x / 1e3, "K"), tr(x, ""))))
}
money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

# ---- normalized SQLite database ---------------------------------------------
con <- function() {
  cn <- dbConnect(SQLite(), DB)
  dbExecute(cn, "PRAGMA busy_timeout = 5000")
  dbExecute(cn, "PRAGMA foreign_keys = ON")
  cn
}

db_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")

ensure_project_record <- function(cn, project_code, department = "", location = "") {
  clean_value <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1]])) "" else trimws(as.character(x[[1]]))
  }
  code <- clean_value(project_code)
  if (!nzchar(code)) return(NA_integer_)
  dep <- clean_value(department)
  loc <- clean_value(location)
  dbExecute(cn,
            "INSERT OR IGNORE INTO projects(project_code, department, location, created, updated) VALUES(?,?,?,?,?)",
            params = list(code, if (nzchar(dep)) dep else NA_character_,
                          if (nzchar(loc)) loc else NA_character_, db_stamp(), db_stamp()))
  # Fill blank catalog metadata without overwriting an existing project setup.
  dbExecute(cn,
            "UPDATE projects SET department = CASE WHEN department IS NULL OR TRIM(department) = '' THEN ? ELSE department END,
                         location = CASE WHEN location IS NULL OR TRIM(location) = '' THEN ? ELSE location END,
                         updated = ? WHERE project_code = ? COLLATE NOCASE",
            params = list(if (nzchar(dep)) dep else NA_character_,
                          if (nzchar(loc)) loc else NA_character_, db_stamp(), code))
  z <- dbGetQuery(cn, "SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE", params = list(code))
  if (nrow(z)) as.integer(z$id[[1]]) else NA_integer_
}

ensure_budget_line_record <- function(cn, project_id, budget_line_code) {
  code <- if (is.null(budget_line_code) || !length(budget_line_code) || is.na(budget_line_code[[1]])) "" else trimws(as.character(budget_line_code[[1]]))
  if (is.null(project_id) || !length(project_id) || is.na(project_id[[1]]) || !nzchar(code)) return(NA_integer_)
  dbExecute(cn,
            "INSERT OR IGNORE INTO budget_lines(project_id, budget_line_code, created, updated) VALUES(?,?,?,?)",
            params = list(as.integer(project_id), code, db_stamp(), db_stamp()))
  z <- dbGetQuery(cn,
                  "SELECT id FROM budget_lines WHERE project_id = ? AND budget_line_code = ? COLLATE NOCASE",
                  params = list(as.integer(project_id), code))
  if (nrow(z)) as.integer(z$id[[1]]) else NA_integer_
}

create_normalized_tables <- function(cn) {
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS projects(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_code TEXT NOT NULL COLLATE NOCASE UNIQUE,
    department TEXT, location TEXT, created TEXT, updated TEXT)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS budget_lines(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
    budget_line_code TEXT NOT NULL COLLATE NOCASE,
    created TEXT, updated TEXT,
    UNIQUE(project_id, budget_line_code))")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS currencies(
    code TEXT PRIMARY KEY COLLATE NOCASE,
    name TEXT NOT NULL,
    units_per_usd REAL NOT NULL,
    active INTEGER NOT NULL DEFAULT 1,
    sort_order INTEGER NOT NULL DEFAULT 100)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS app_defaults(
    setting_key TEXT PRIMARY KEY,
    setting_value TEXT NOT NULL,
    updated TEXT NOT NULL)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS suppliers(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    supplier_name TEXT NOT NULL COLLATE NOCASE UNIQUE,
    email TEXT, phone TEXT, notes TEXT,
    active INTEGER NOT NULL DEFAULT 1,
    created TEXT, updated TEXT)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS people(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    full_name TEXT NOT NULL COLLATE NOCASE UNIQUE,
    email TEXT COLLATE NOCASE UNIQUE,
    department TEXT,
    active INTEGER NOT NULL DEFAULT 1,
    created TEXT, updated TEXT)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS notification_emails(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT NOT NULL COLLATE NOCASE UNIQUE,
    weekly_report INTEGER NOT NULL DEFAULT 1,
    request_created INTEGER NOT NULL DEFAULT 0,
    status_changed INTEGER NOT NULL DEFAULT 0,
    active INTEGER NOT NULL DEFAULT 1,
    created TEXT, updated TEXT)")
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS expenses(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    description TEXT,
    budget_line_id INTEGER REFERENCES budget_lines(id) ON DELETE RESTRICT,
    country TEXT, location TEXT, department TEXT, reason TEXT,
    order_form TEXT, po TEXT, requested_by TEXT, submitted_by TEXT,
    status TEXT, waiver TEXT, supplier TEXT, unit TEXT, quantity REAL,
    amount REAL, currency TEXT REFERENCES currencies(code), usd REAL, rate_used REAL,
    order_date TEXT, po_date TEXT, payment_date TEXT, remarks TEXT,
    created TEXT, updated TEXT)")
  dbExecute(cn, "CREATE INDEX IF NOT EXISTS idx_expenses_order_date ON expenses(order_date)")
  dbExecute(cn, "CREATE INDEX IF NOT EXISTS idx_expenses_status ON expenses(status)")
  dbExecute(cn, "CREATE INDEX IF NOT EXISTS idx_expenses_budget_line ON expenses(budget_line_id)")
}

seed_catalog_defaults <- function(cn) {
  currency_names <- c(ETB = "Ethiopian Birr", USD = "US Dollar", EUR = "Euro", GBP = "British Pound")
  for (code in names(RATES)) {
    dbExecute(cn,
              "INSERT OR IGNORE INTO currencies(code, name, units_per_usd, active, sort_order) VALUES(?,?,?,?,?)",
              params = list(code, unname(currency_names[[code]] %||% code), unname(RATES[[code]]), 1L,
                            match(code, names(RATES))))
  }
  defaults_to_seed <- list(submitted_by = "Betty T", country = "ETH", currency = "ETB")
  for (key in names(defaults_to_seed)) {
    dbExecute(cn,
              "INSERT OR IGNORE INTO app_defaults(setting_key, setting_value, updated) VALUES(?,?,?)",
              params = list(key, defaults_to_seed[[key]], db_stamp()))
  }
}

init_db <- function() {
  cn <- con(); on.exit(dbDisconnect(cn))
  # Detect the former denormalized schema and migrate it once, preserving IDs and rows.
  tables <- dbListTables(cn)
  has_expenses <- "expenses" %in% tables
  has_projects <- "projects" %in% tables
  exp_fields <- if (has_expenses) dbListFields(cn, "expenses") else character()
  project_fields <- if (has_projects) dbListFields(cn, "projects") else character()
  normalized_expenses <- has_expenses && "budget_line_id" %in% exp_fields &&
    !any(c("project_code", "budget_line") %in% exp_fields)
  normalized_projects <- has_projects && all(c("id", "project_code") %in% project_fields)
  needs_migration <- (has_expenses && !normalized_expenses) || (has_projects && !normalized_projects)
  
  if (needs_migration) {
    legacy_expenses <- if (has_expenses && !normalized_expenses) dbReadTable(cn, "expenses") else NULL
    legacy_projects <- if (has_projects && !normalized_projects) dbReadTable(cn, "projects") else NULL
    # Make a SQLite-consistent snapshot before the one-time schema migration.
    # If the backup fails, stop rather than risk transforming the only copy.
    backup_stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    backup_path <- paste0(DB, ".pre-normalization-", backup_stamp, "-", Sys.getpid(), ".sqlite")
    backup_sql_path <- gsub("'", "''", backup_path, fixed = TRUE)
    tryCatch(
      dbExecute(cn, paste0("VACUUM INTO '", backup_sql_path, "'")),
      error = function(e) stop("Could not create the pre-migration database backup at ", backup_path,
                               ": ", conditionMessage(e), ". Migration was cancelled."))
    message("Created database migration backup: ", backup_path)
    dbExecute(cn, "PRAGMA foreign_keys = OFF")
    dbBegin(cn)
    tryCatch({
      if (!is.null(legacy_expenses)) dbExecute(cn, "ALTER TABLE expenses RENAME TO expenses_legacy_migration")
      if (!is.null(legacy_projects)) dbExecute(cn, "ALTER TABLE projects RENAME TO projects_legacy_migration")
      create_normalized_tables(cn)
      seed_catalog_defaults(cn)
      
      # Carry the old project catalogue forward first.
      if (!is.null(legacy_projects) && nrow(legacy_projects)) {
        for (i in seq_len(nrow(legacy_projects))) {
          rec <- as.list(legacy_projects[i, , drop = FALSE])
          val <- function(key, fallback = "") {
            if (!key %in% names(rec)) return(fallback)
            z <- rec[[key]][[1]]
            if (length(z) == 0 || is.na(z)) fallback else as.character(z)
          }
          ensure_project_record(cn, val("project_code"), val("department"), val("location"))
        }
      }
      
      if (!is.null(legacy_expenses) && nrow(legacy_expenses)) {
        legacy_value <- function(i, key, fallback = NA) {
          if (!key %in% names(legacy_expenses)) return(fallback)
          z <- legacy_expenses[[key]][i]
          if (!length(z) || is.na(z[[1]])) fallback else z[[1]]
        }
        for (i in seq_len(nrow(legacy_expenses))) {
          code <- as.character(legacy_value(i, "project_code", ""))
          budget <- as.character(legacy_value(i, "budget_line", ""))
          dep <- as.character(legacy_value(i, "department", ""))
          loc <- as.character(legacy_value(i, "location", ""))
          pid <- ensure_project_record(cn, code, dep, loc)
          bid <- ensure_budget_line_record(cn, pid, budget)
          curr <- as.character(legacy_value(i, "currency", "ETB"))
          if (is.na(curr) || !nzchar(curr)) curr <- "ETB"
          # Preserve legacy currencies as selectable catalog entries if they are not one of the defaults.
          currency_exists <- dbGetQuery(cn, "SELECT code FROM currencies WHERE code = ? COLLATE NOCASE", params = list(curr))
          if (nrow(currency_exists) == 0L) {
            rate <- suppressWarnings(as.numeric(legacy_value(i, "rate_used", 1)))
            if (is.na(rate) || rate <= 0) rate <- 1
            dbExecute(cn, "INSERT OR IGNORE INTO currencies(code,name,units_per_usd,active,sort_order) VALUES(?,?,?,?,?)",
                      params = list(curr, curr, rate, 1L, 100L))
          }
          st <- as.character(legacy_value(i, "status", "waiting"))
          if (identical(st, "cancelled")) st <- "rejected"
          row <- list(
            id = as.integer(legacy_value(i, "id", NA_integer_)),
            description = legacy_value(i, "description", NA_character_),
            budget_line_id = bid,
            country = legacy_value(i, "country", NA_character_),
            location = legacy_value(i, "location", NA_character_),
            department = legacy_value(i, "department", NA_character_),
            reason = legacy_value(i, "reason", NA_character_),
            order_form = legacy_value(i, "order_form", NA_character_),
            po = legacy_value(i, "po", NA_character_),
            requested_by = legacy_value(i, "requested_by", NA_character_),
            submitted_by = legacy_value(i, "submitted_by", NA_character_),
            status = st, waiver = legacy_value(i, "waiver", NA_character_),
            supplier = legacy_value(i, "supplier", NA_character_),
            unit = legacy_value(i, "unit", NA_character_),
            quantity = suppressWarnings(as.numeric(legacy_value(i, "quantity", NA_real_))),
            amount = suppressWarnings(as.numeric(legacy_value(i, "amount", NA_real_))),
            currency = curr,
            usd = suppressWarnings(as.numeric(legacy_value(i, "usd", NA_real_))),
            rate_used = suppressWarnings(as.numeric(legacy_value(i, "rate_used", NA_real_))),
            order_date = legacy_value(i, "order_date", NA_character_),
            po_date = legacy_value(i, "po_date", NA_character_),
            payment_date = legacy_value(i, "payment_date", NA_character_),
            remarks = legacy_value(i, "remarks", NA_character_),
            created = legacy_value(i, "created", NA_character_),
            updated = legacy_value(i, "updated", NA_character_)
          )
          if (is.na(row$id)) row$id <- NULL
          dbExecute(cn, sprintf("INSERT INTO expenses(%s) VALUES(%s)",
                                paste(names(row), collapse = ","), paste(rep("?", length(row)), collapse = ",")),
                    params = unname(row))
        }
      }
      if (!is.null(legacy_expenses)) dbExecute(cn, "DROP TABLE expenses_legacy_migration")
      if (!is.null(legacy_projects)) dbExecute(cn, "DROP TABLE projects_legacy_migration")
      create_normalized_tables(cn) # recreate indexes after legacy index names are freed
      dbCommit(cn)
    }, error = function(e) {
      try(dbRollback(cn), silent = TRUE)
      stop("Database schema migration failed: ", conditionMessage(e))
    })
    dbExecute(cn, "PRAGMA foreign_keys = ON")
  } else {
    create_normalized_tables(cn)
    seed_catalog_defaults(cn)
  }
  # Defaults selected in Settings are always backed by an available currency.
  dbExecute(cn, "UPDATE app_defaults SET setting_value = 'ETB', updated = ?
                   WHERE setting_key = 'currency' AND setting_value NOT IN (SELECT code FROM currencies WHERE active = 1)",
            params = list(db_stamp()))
  # Seed a supplier catalogue from historical expense text values. This is safe to run repeatedly.
  dbExecute(cn, "INSERT OR IGNORE INTO suppliers(supplier_name, created, updated)
                 SELECT DISTINCT TRIM(supplier), ?, ? FROM expenses
                 WHERE supplier IS NOT NULL AND TRIM(supplier) <> ''",
            params = list(db_stamp(), db_stamp()))
  # Financial update schema extensions
  exp_fields <- dbListFields(cn, "expenses")
  if (!"deducted_at" %in% exp_fields) {
    try(dbExecute(cn, "ALTER TABLE expenses ADD COLUMN deducted_at TEXT"), silent = TRUE)
  }
  proj_fields <- dbListFields(cn, "projects")
  if (!"last_deducted_at" %in% proj_fields) {
    try(dbExecute(cn, "ALTER TABLE projects ADD COLUMN last_deducted_at TEXT"), silent = TRUE)
  }
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS financial_batches(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    batch_code TEXT UNIQUE,
    project_codes TEXT,
    request_ids TEXT,
    total_amount REAL,
    currency TEXT,
    status TEXT DEFAULT 'batched',
    created TEXT,
    confirmed_at TEXT
  )")
}
tryCatch(
  init_db(),
  error = function(e) {
    stop(
      paste0("Expense Tracker could not initialize SQLite database at ", DB,
             ". Original error: ", conditionMessage(e),
             ". Check database permissions and make a backup before retrying any migration."),
      call. = FALSE
    )
  }
)

db_get <- function(sql, ...) {
  cn <- con(); on.exit(dbDisconnect(cn)); a <- list(...)
  dbGetQuery(cn, sql, params = if (length(a)) a)
}

read_app_defaults <- function() {
  x <- db_get("SELECT setting_key, setting_value FROM app_defaults")
  value <- function(key, fallback, allow_empty = FALSE) {
    z <- x$setting_value[x$setting_key == key]
    if (!length(z) || is.na(z[[1]]) || (!allow_empty && !nzchar(z[[1]]))) fallback else as.character(z[[1]])
  }
  list(submitted_by = value("submitted_by", "Betty T"),
       country = value("country", "ETH", allow_empty = TRUE),
       currency = value("currency", "ETB"))
}

# Read the selectable currency list and rates from the database, not just hard-coded constants.
currency_catalog <- db_get("SELECT code, name, units_per_usd FROM currencies WHERE active = 1 ORDER BY sort_order, code")
if (nrow(currency_catalog)) {
  CUR <- as.character(currency_catalog$code)
  RATES <- setNames(as.numeric(currency_catalog$units_per_usd), CUR)
}
DEFAULTS <- read_app_defaults()

project_codes_db <- function() {
  z <- db_get("SELECT project_code FROM projects ORDER BY project_code COLLATE NOCASE")
  as.character(z$project_code)
}

budget_catalog_db <- function(project_codes = character()) {
  # This catalogue, not the expense rows, is the single source of truth for filter choices.
  z <- db_get("SELECT bl.id, p.project_code, bl.budget_line_code
               FROM budget_lines bl JOIN projects p ON p.id = bl.project_id
               ORDER BY p.project_code COLLATE NOCASE, bl.budget_line_code COLLATE NOCASE")
  if (length(project_codes)) {
    selected_codes <- tolower(trimws(as.character(project_codes)))
    z <- z[tolower(z$project_code) %in% selected_codes, , drop = FALSE]
  }
  z
}

supplier_catalog_db <- function(active_only = TRUE) {
  sql <- if (active_only) "SELECT supplier_name FROM suppliers WHERE active = 1 ORDER BY supplier_name COLLATE NOCASE"
  else "SELECT supplier_name FROM suppliers ORDER BY supplier_name COLLATE NOCASE"
  z <- db_get(sql)
  as.character(z$supplier_name)
}

# Resolve human-readable project/budget-line values to their normalized foreign keys.
# Inserts and updates fail loudly if the chosen mapping was not configured in Settings.
prepare_expense_row <- function(cn, row) {
  code <- if (is.null(row$project_code) || !length(row$project_code) || is.na(row$project_code[[1]])) "" else trimws(as.character(row$project_code[[1]]))
  budget <- if (is.null(row$budget_line) || !length(row$budget_line) || is.na(row$budget_line[[1]])) "" else trimws(as.character(row$budget_line[[1]]))
  if (!nzchar(code) || !nzchar(budget)) stop("Choose a configured project code and budget line.")
  bl <- dbGetQuery(cn,
                   "SELECT bl.id FROM budget_lines bl JOIN projects p ON p.id = bl.project_id
     WHERE p.project_code = ? COLLATE NOCASE AND bl.budget_line_code = ? COLLATE NOCASE",
                   params = list(code, budget))
  if (!nrow(bl)) stop("This budget line does not belong to the selected project code. Configure it in Settings first.")
  row$budget_line_id <- as.integer(bl$id[[1]])
  row <- row[!names(row) %in% c("project_code", "budget_line")]
  row
}

insert_expense_row <- function(cn, row) {
  storage <- prepare_expense_row(cn, row)
  dbExecute(cn, sprintf("INSERT INTO expenses(%s) VALUES(%s)",
                        paste(names(storage), collapse = ","), paste(rep("?", length(storage)), collapse = ",")),
            params = unname(storage))
}

update_expense_row <- function(cn, id, row) {
  storage <- prepare_expense_row(cn, row)
  dbExecute(cn, sprintf("UPDATE expenses SET %s WHERE id=?",
                        paste0(names(storage), "=?", collapse = ",")),
            params = c(unname(storage), list(as.integer(id))))
}

now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")

fmt <- function(d) format(d, "%d-%m-%Y")
v   <- function(x) if (is.null(x) || !length(x) || is.na(x)) "" else trimws(x)
nz  <- function(x) if (nzchar(v(x))) v(x) else NA_character_
has_date <- function(d) !is.null(d) && length(d) == 1 && !is.na(d)
has_num  <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
uniq <- function(x) { x <- as.character(x); sort(unique(x[!is.na(x) & nzchar(x)])) }
with_sel <- function(ch, sel) { miss <- setdiff(sel, ch); c(ch, setNames(miss, miss)) }

date_in <- function(id, label, value = NA, ...)
  suppressWarnings(dateInput(id, label, value = value, format = "dd-mm-yyyy", ...))

# Order form / PO references. Country is optional and skipped when empty.
mk_ref <- function(r, kind = c("OF", "PO")) {
  kind <- match.arg(kind)
  g <- function(k) v(r[[k]])
  if (!all(nzchar(c(g("project_code"), g("budget_line"), g("location"), g("department")))))
    return(NA_character_)
  parts <- c(g("project_code"), g("budget_line"), g("country"), g("location"), g("department"))
  parts <- paste(parts[nzchar(parts)], collapse = "/")
  if (kind == "OF") {
    d <- suppressWarnings(as.Date(g("order_date")))
    if (is.na(d)) return(NA_character_)
    paste0("OF/", parts, "/", fmt(d))
  } else {
    d <- suppressWarnings(as.Date(g("po_date")))
    if (is.na(d) || !nzchar(g("reason"))) return(NA_character_)
    paste0("PO/OF/", parts, "/", g("reason"), "/", fmt(d))
  }
}

# short fields whose dropdowns are filled from the shared database
CACHE <- list(
  requested_by = c("requested_by", "submitted_by"),
  submitted_by = c("requested_by", "submitted_by"),
  country = "country", location = "location",
  supplier = "supplier", unit = "unit"
)
cache_choices <- function(d, id) {
  vals <- uniq(unlist(d[CACHE[[id]]], use.names = FALSE))
  if (identical(id, "supplier")) vals <- supplier_catalog_db(active_only = TRUE)
  if (id %in% c("requested_by", "submitted_by")) {
    people <- db_get("SELECT full_name FROM people WHERE active=1 ORDER BY full_name COLLATE NOCASE")$full_name
    vals <- uniq(c(vals, people))
  }
  vals
}

# ---- UI helpers ---------------------------------------------------------------
R   <- function(x) paste0(x, " *")
OPT <- function(x) x
col <- function(n, ...) div(class = paste0("c", n), ...)

cached <- function(id, label, req = TRUE) {
  d <- DEFAULTS[[id]]
  selectizeInput(id, if (req) R(label) else OPT(label), choices = d, selected = d,
                 width = "100%",
                 options = list(create = TRUE, placeholder = "Type or pick"))
}
pick <- function(id, label, choices)
  selectInput(id, R(label), c("Select" = "", choices),
              selected = DEFAULTS[[id]] %||% "", width = "100%")

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#09090b",
  primary = "#0284c7", # shadcn Sky
  secondary = "#f4f4f5", # shadcn Zinc
  base_font = "Inter",
  heading_font = "Inter",
  "border-radius" = "0.6rem"
)

css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
:root{
  --app-bg:#fafafa; --app-card-bg:#ffffff; --app-input-bg:#ffffff;
  --app-border:#d4d4d8; --app-muted:#f4f4f5; --app-muted-fg:#52525b;
  --app-accent:#0284c7; --app-accent-soft:#e0f2fe; --app-radius:.6rem;
}
[data-bs-theme='dark']{
  --app-bg:#09090b; --app-card-bg:#18181b; --app-input-bg:#18181b;
  --app-border:#52525b; --app-muted:#27272a; --app-muted-fg:#d4d4d8;
  --app-accent:#38bdf8; --app-accent-soft:#082f49;
  --bs-body-bg:#09090b; --bs-body-color:#fafafa; --bs-secondary-color:#d4d4d8;
  --bs-tertiary-bg:#27272a; --bs-border-color:#52525b; --bs-card-bg:#18181b;
}
html,body,.navbar,.btn,.form-control,.form-select,.selectize-input,.selectize-dropdown{
  font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
}
body{background:var(--bs-body-bg);color:var(--bs-body-color);font-size:.94rem}
.bslib-grid>*{min-width:0}
.navbar{border-bottom:1px solid var(--app-border);box-shadow:none;background:var(--bs-body-bg)}
.navbar .container-fluid{max-width:1800px}
a.brand{font-weight:700;letter-spacing:-.035em;color:var(--bs-body-color)!important;text-decoration:none;font-size:1.05rem}
.navbar .nav-link[data-value='requests']{font-weight:600;color:var(--bs-body-color)!important}
.navbar .nav-link[data-value='new']{display:none!important}
.navbar .nav-link.active[data-value='requests']{color:var(--app-accent)!important;background:transparent}
.page-shell{width:100%;max-width:1760px;margin:0 auto;padding:1.3rem clamp(1rem,2.2vw,2.5rem) 2.5rem}
.page-heading{display:flex;align-items:center;justify-content:space-between;gap:1rem;flex-wrap:wrap;margin-bottom:1.15rem}
.page-heading h1{font-size:1.55rem;font-weight:650;letter-spacing:-.04em;margin:0}
.page-heading p{color:var(--app-muted-fg);margin:.3rem 0 0;font-size:.9rem}
.toolbar{display:flex;align-items:center;gap:.65rem;flex-wrap:wrap;margin:1.15rem 0 1.2rem}
.toolbar .shiny-input-container{margin:0}
.toolbar .btn{min-height:2.45rem}
.filter-dd .dropdown-menu{width:min(94vw,600px);max-height:75vh;overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);box-shadow:0 14px 35px rgba(0,0,0,.12)}
.fgrid{display:grid;grid-template-columns:1fr 1fr;gap:.15rem .85rem}
.fgrid .wide{grid-column:1/-1}
.fgrid .shiny-input-container{width:100%;margin:.15rem 0 .65rem}
.fgrid .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
@media(max-width:620px){.fgrid{grid-template-columns:1fr}.fgrid .wide{grid-column:auto}}
.btn-primary{--bs-btn-bg:#0284c7;--bs-btn-border-color:#0284c7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1;--bs-btn-active-bg:#075985;font-weight:600;color:#fff!important}
.btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:disabled,[data-bs-theme='dark'] .btn-primary{color:#fff!important}
[data-bs-theme='dark'] .btn-primary{--bs-btn-bg:#0284c7;--bs-btn-border-color:#0284c7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1}
.btn-outline-secondary{border-color:var(--app-border);color:var(--bs-body-color);font-weight:500}
.btn-outline-secondary:hover{background:var(--app-muted);border-color:var(--app-border);color:var(--bs-body-color)}
.card{border:1px solid var(--app-border);border-radius:var(--app-radius);box-shadow:none;background:var(--bs-card-bg,var(--bs-body-bg));min-width:0}
.card-header{padding:1rem 1.15rem;background:transparent;border-bottom:1px solid var(--app-border);font-weight:600}
.card-body{padding:1.1rem 1.15rem}
.metrics-grid{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:.8rem;margin-bottom:1rem}
@media(max-width:1150px){.metrics-grid{grid-template-columns:repeat(3,minmax(0,1fr))}}
@media(max-width:720px){.metrics-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:440px){.metrics-grid{grid-template-columns:1fr}}
.metric-card{height:100%;padding:1rem 1.1rem;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--bs-body-bg);min-width:0}
.metric-label{font-size:.82rem;font-weight:500;color:var(--app-muted-fg);margin:0 0 .45rem}
.metric-value{font-size:clamp(1.25rem,1.65vw,1.75rem);font-weight:650;letter-spacing:-.045em;line-height:1.15;overflow-wrap:anywhere;margin:0}
.metric-note{font-size:.78rem;color:var(--app-muted-fg);margin:.4rem 0 0}
.chart-card .card-body{padding:.75rem 1rem 1rem}
.chart-note{color:var(--app-muted-fg);font-size:.82rem;margin:-.25rem 0 .5rem}
.table-head{width:100%;display:flex;align-items:center;justify-content:space-between;gap:.8rem;flex-wrap:wrap}
.table-head .buttons{margin-left:auto;display:flex;align-items:center;gap:.5rem;flex-wrap:wrap}
.nav-tabs{border-bottom:1px solid var(--app-border);gap:.2rem;flex-wrap:wrap}
.nav-tabs .nav-link{border:0;border-radius:.45rem .45rem 0 0;color:var(--app-muted-fg);font-weight:500;padding:.65rem .9rem}
.nav-tabs .nav-link.active{color:var(--bs-body-color);background:var(--app-muted);border-bottom:2px solid var(--app-accent)}
.tab-content{padding-top:.9rem}
.dataTables_wrapper{width:100%;min-width:0}
table.dataTable{width:100%!important;border-collapse:separate!important;border-spacing:0}
table.dataTable thead th{font-size:.78rem;color:var(--app-muted-fg);font-weight:600;border-bottom:1px solid var(--app-border)!important;background:var(--bs-body-bg)}
table.dataTable tbody td{font-size:.86rem;vertical-align:middle;border-bottom:1px solid var(--app-border)!important;max-width:260px}
table.dataTable tbody tr:hover{background:var(--app-muted)!important}
table.dataTable tbody tr.selected>*{box-shadow:inset 0 0 0 9999px var(--app-accent-soft)!important;color:var(--bs-body-color)!important}
a.dmore{color:var(--app-accent);font-weight:600;font-size:.8rem;text-decoration:none;white-space:nowrap}
.dt-top{display:flex;justify-content:flex-end;align-items:center;gap:.65rem;flex-wrap:wrap;margin-bottom:.8rem}
.dt-top .dataTables_filter{order:2}
.dt-status-control{display:flex;align-items:center;gap:.45rem;order:1}
.dt-status-control label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg);margin:0;white-space:nowrap}
.dt-status-control select{min-width:175px;border:1px solid var(--app-border);border-radius:.5rem;padding:.48rem .7rem;background:var(--bs-body-bg);color:var(--bs-body-color)}
.dt-status-control select:focus{outline:0;border-color:var(--app-accent);box-shadow:0 0 0 3px rgba(2,132,199,.14)}
@media(max-width:640px){.dt-top{align-items:stretch;justify-content:stretch}.dt-status-control{width:100%;order:1}.dt-status-control select{flex:1;min-width:0}.dt-top .dataTables_filter{width:100%;order:2}.dt-top .dataTables_filter input{width:100%;min-width:0}}
.dt-foot{display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:.75rem;margin-top:.8rem;color:var(--app-muted-fg);font-size:.82rem}
div.dataTables_wrapper div.dataTables_filter{margin:0}
div.dataTables_wrapper div.dataTables_filter input{min-width:min(280px,55vw);margin-left:0!important;padding:.48rem .7rem;border-radius:.5rem;border:1px solid var(--app-border)!important;background:var(--bs-body-bg)!important;color:var(--bs-body-color);outline:none;box-shadow:none}
div.dataTables_wrapper div.dataTables_filter input:focus{border-color:var(--app-accent)!important;box-shadow:0 0 0 3px rgba(2,132,199,.14)!important}
.dataTables_wrapper .dataTables_paginate ul.pagination{gap:.3rem;margin:0}
.dataTables_wrapper .dataTables_paginate .paginate_button{border:0!important;padding:0!important;margin:0!important;background:transparent!important}
.dataTables_wrapper .dataTables_paginate .page-link{border:1px solid var(--app-border)!important;border-radius:.45rem!important;padding:.28rem .7rem;color:var(--bs-body-color)!important;background:transparent!important}
.dataTables_wrapper .dataTables_paginate .active .page-link{background:var(--app-accent)!important;border-color:var(--app-accent)!important;color:#fff!important}
.fg{display:grid;grid-template-columns:repeat(12,minmax(0,1fr));gap:.1rem 1rem}
.c3{grid-column:span 3}.c4{grid-column:span 4}.c5{grid-column:span 5}.c6{grid-column:span 6}.c8{grid-column:span 8}.c12{grid-column:span 12}
@media(max-width:900px){.fg>.c3,.fg>.c4,.fg>.c5{grid-column:span 6}}
@media(max-width:560px){.fg>*{grid-column:span 12!important}}
.nr .control-label{display:block;font-size:.78rem;font-weight:600;margin-bottom:.35rem;color:var(--bs-body-color)}
.nr .shiny-input-container{margin-bottom:.8rem;width:100%}
.nr .form-control,.nr .form-select,.nr .selectize-input{border:1px solid var(--app-border)!important;border-radius:.5rem;background:var(--bs-body-bg)!important;color:var(--bs-body-color);box-shadow:none}
.nr input.form-control,.nr .form-select{min-height:2.65rem}
.nr .selectize-input{min-height:2.65rem;display:flex;align-items:center;flex-wrap:wrap}
.nr textarea.form-control{height:auto;resize:vertical}
.nr .form-control:focus,.nr .form-select:focus,.nr .selectize-input.focus{border-color:var(--app-accent)!important;box-shadow:0 0 0 3px rgba(2,132,199,.15)!important}
.nr .req-bad .control-label{color:#dc2626}
.nr .req-bad .form-control,.nr .req-bad .form-select,.nr .req-bad .selectize-input{border-color:#dc2626!important;background:rgba(220,38,38,.05)!important;box-shadow:0 0 0 3px rgba(220,38,38,.12)!important}
.form-intro{font-size:.85rem;color:var(--app-muted-fg);margin:-.2rem 0 1rem}
.form-hint{font-size:.78rem;color:var(--app-muted-fg);margin:-.2rem 0 .65rem}
.form-section-title{font-weight:650;font-size:.95rem;margin:.65rem 0 .8rem}
.form-section-rule{border-top:1px solid var(--app-border);margin:.5rem 0 1rem}
.action-bar{position:sticky;bottom:0;z-index:5;display:flex;align-items:center;justify-content:flex-end;gap:.65rem;padding:.9rem 0 .1rem;margin-top:.5rem;background:var(--bs-body-bg);border-top:1px solid var(--app-border)}
.modal-content{border:1px solid var(--app-border);border-radius:.8rem;background:var(--bs-body-bg)}
.modal-header,.modal-footer{border-color:var(--app-border)}
.ref{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.8rem;word-break:break-word;color:var(--app-muted-fg)}
.reference-panel{margin-top:1rem;padding:1rem;border:1px solid var(--app-border);border-radius:.65rem;background:var(--app-muted)}
.reference-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:.8rem}
.request-details-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:.75rem .9rem;margin-top:.6rem}
.detail-item{background:var(--app-muted);padding:.65rem .85rem;border-radius:.5rem;border:1px solid var(--app-border)}
.detail-item.wide{grid-column:1/-1}
.detail-item label{display:block;font-size:.72rem;font-weight:600;text-transform:uppercase;letter-spacing:.03em;color:var(--app-muted-fg);margin-bottom:.2rem}
.detail-val{font-size:.88rem;font-weight:550;color:var(--bs-body-color);word-break:break-word}
@media(max-width:768px){.request-details-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:480px){.request-details-grid{grid-template-columns:1fr}}
.status-pill{font-size:.78rem;font-weight:600;padding:.35rem .65rem;border-radius:1rem}
.status-waiting{background:#f1f5f9;color:#475569}
.status-under-validation{background:#fef3c7;color:#92400e}
.status-paid{background:#dcfce7;color:#166534}
.status-rejected{background:#fee2e2;color:#991b1b}
[data-bs-theme='dark'] .status-waiting{background:rgba(148,163,184,.15);color:#94a3b8}
[data-bs-theme='dark'] .status-under-validation{background:rgba(245,158,11,.15);color:#f59e0b}
[data-bs-theme='dark'] .status-paid{background:rgba(22,163,74,.15);color:#4ade80}
[data-bs-theme='dark'] .status-rejected{background:rgba(220,38,38,.15);color:#f87171}
.reference-item{min-width:0;padding:.8rem;border:1px solid var(--app-border);border-radius:.55rem;background:var(--bs-body-bg)}
.reference-label{font-size:.76rem;font-weight:650;color:var(--app-muted-fg);margin-bottom:.35rem}
.reference-value{display:block;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.78rem;line-height:1.5;overflow-wrap:anywhere;color:var(--bs-body-color);white-space:normal}
.reference-hint{font-size:.76rem;color:var(--app-muted-fg);margin:.7rem 0 0}
@media(max-width:560px){.reference-grid{grid-template-columns:1fr}}
.badge-scen{font-size:.9rem;background:var(--app-accent);color:#fff;padding:.2rem .5rem;border-radius:.4rem}
.shiny-notification{opacity:1;border:1px solid #d4d4d8;border-radius:.6rem;font-weight:600;box-shadow:0 8px 24px rgba(0,0,0,.16);background:#f4f4f5!important;color:#27272a!important}
.shiny-notification-message{background:#ecfdf5!important;color:#166534!important;border-color:#86efac!important}
.shiny-notification-warning{background:#fffbeb!important;color:#92400e!important;border-color:#fcd34d!important}
.shiny-notification-error{background:#fef2f2!important;color:#991b1b!important;border-color:#fca5a5!important}
[data-bs-theme='dark'] .shiny-notification{background:#27272a!important;color:#f4f4f5!important;border-color:#52525b!important}
[data-bs-theme='dark'] .shiny-notification-message{background:#052e16!important;color:#bbf7d0!important;border-color:#166534!important}
[data-bs-theme='dark'] .shiny-notification-warning{background:#451a03!important;color:#fde68a!important;border-color:#92400e!important}
[data-bs-theme='dark'] .shiny-notification-error{background:#450a0a!important;color:#fecaca!important;border-color:#991b1b!important}
.shiny-notification-close{color:inherit!important}
@media(min-width:1900px){.page-shell{max-width:1840px;padding-left:2.75rem;padding-right:2.75rem}.card-body{padding:1.3rem 1.4rem}}

/* Final contrast/accessibility pass inspired by the reference dashboard. */
body,.navbar{background:var(--app-bg)!important;color:var(--bs-body-color)!important}
.page-heading p,.chart-note,.metric-label,.metric-note,.dt-foot,.fgrid .control-label{color:var(--app-muted-fg)!important}
.card,.metric-card{background:var(--app-card-bg)!important;border:1px solid var(--app-border)!important;box-shadow:0 1px 2px rgba(0,0,0,.07)}
.card-header{background:var(--app-card-bg)!important;border-bottom:1px solid var(--app-border)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .card,[data-bs-theme='dark'] .metric-card{box-shadow:0 0 0 1px rgba(212,212,216,.08),0 8px 22px rgba(0,0,0,.22)}
.btn-outline-secondary{border-color:var(--app-border)!important;color:var(--bs-body-color)!important}
.btn-outline-secondary:hover,.btn-outline-secondary:focus{border-color:var(--app-muted-fg)!important;background:var(--app-muted)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .btn-outline-secondary{border-color:#71717a!important;color:#fafafa!important}
.form-control,.form-select,.selectize-input,.selectize-dropdown,.input-group-text{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border-color:var(--app-border)!important}
.form-control::placeholder,.selectize-input input::placeholder{color:var(--app-muted-fg)!important;opacity:1}
.selectize-dropdown{border:1px solid var(--app-border)!important;box-shadow:0 8px 20px rgba(0,0,0,.14)!important}
.selectize-dropdown .option{background:var(--app-card-bg)!important;color:var(--bs-body-color)!important}
.selectize-dropdown .active{background:var(--app-muted)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .selectize-input input{color:#fafafa!important}
[data-bs-theme='dark'] .form-control:focus,[data-bs-theme='dark'] .form-select:focus,[data-bs-theme='dark'] .selectize-input.focus{border-color:#38bdf8!important;box-shadow:0 0 0 3px rgba(56,189,248,.2)!important}
.nav-tabs{border-bottom:1px solid var(--app-border)!important}
.nav-tabs .nav-link{color:var(--app-muted-fg)!important;border:1px solid transparent!important}
.nav-tabs .nav-link.active{color:var(--bs-body-color)!important;background:var(--app-muted)!important;border-color:var(--app-border) var(--app-border) var(--app-accent)!important}
table.dataTable thead th{color:var(--bs-body-color)!important;border-bottom:2px solid var(--app-border)!important}
table.dataTable tbody td{color:var(--bs-body-color)!important;border-bottom:1px solid var(--app-border)!important}
table.dataTable tbody tr:nth-child(even)>td{background:var(--app-card-bg)!important}
table.dataTable tbody tr:nth-child(odd)>td{background:var(--app-bg)!important}
table.dataTable tbody tr:hover>td{background:var(--app-muted)!important}
[data-bs-theme='dark'] table.dataTable tbody tr.selected>td{background:#164e63!important;color:#f0f9ff!important}
[data-bs-theme='dark'] .dropdown-menu{background:#18181b!important;color:#fafafa!important;border-color:#52525b!important}
[data-bs-theme='dark'] .dropdown-item{color:#f4f4f5!important}
[data-bs-theme='dark'] .dropdown-item:hover{background:#27272a!important;color:#ffffff!important}
[data-bs-theme='dark'] .text-muted,[data-bs-theme='dark'] .muted{color:#d4d4d8!important}
.btn-primary,.btn-primary:hover,.btn-primary:focus,.btn-primary:active{color:#ffffff!important}

/* Redesigned information architecture: persistent request filters, dashboard workspace, management pages. */
.navbar .nav-link[data-value='requests'],.navbar .nav-link[data-value='dashboards'],.navbar .nav-link[data-value='manage']{font-weight:600;color:var(--bs-body-color)!important}
.navbar .nav-link.active[data-value='requests'],.navbar .nav-link.active[data-value='dashboards'],.navbar .nav-link.active[data-value='manage']{color:var(--app-accent)!important;background:transparent}
.navbar .nav-link[data-value='new'],.navbar .nav-link[data-value='manage']{display:none!important}
.global-currency-wrap .shiny-input-container{margin:0!important;width:auto!important}
.global-currency-wrap .form-select,.global-currency-wrap .form-control{padding:.26rem .55rem;font-size:.84rem;font-weight:600;border-radius:.45rem;border:1px solid var(--app-border)!important;background:var(--app-input-bg)!important;color:var(--bs-body-color)!important}
.dashboard-filter-section{margin-bottom:.5rem}
.tracking-wide{letter-spacing:.04em}
.requests-layout{display:grid;grid-template-columns:minmax(0,1fr) minmax(245px,285px);gap:1.15rem;align-items:start;max-width:1900px;margin:0 auto;padding:1.2rem clamp(.8rem,1.6vw,1.8rem) 2rem;min-height:calc(100vh - 150px);transition:grid-template-columns .2s ease}
.requests-layout.sidebar-collapsed{grid-template-columns:minmax(0,1fr) 0;gap:0}
.requests-layout.sidebar-collapsed .requests-sidebar{display:none!important}
.requests-sidebar{position:sticky;top:1rem;max-height:calc(100vh - 6rem);overflow:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg)}
.requests-sidebar-head{display:flex;align-items:center;gap:.55rem;padding:1rem 1rem .85rem;border-bottom:1px solid var(--app-border);font-size:.96rem;font-weight:650}
.requests-sidebar-body{padding:1rem}
.requests-sidebar .shiny-input-container{width:100%;margin:0 0 .8rem}
.requests-sidebar .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
.requests-sidebar .form-control,.requests-sidebar .form-select,.requests-sidebar .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.requests-sidebar .sidebar-divider{height:1px;background:var(--app-border);margin:.9rem 0}
.requests-main{min-width:0;grid-column:1;grid-row:1}
.requests-sidebar{grid-column:2;grid-row:1}
.sidebar-close-btn{border:none!important;background:transparent!important;color:var(--app-muted-fg)!important;padding:.2rem .4rem;font-size:1.05rem;line-height:1;border-radius:.35rem;cursor:pointer;display:inline-flex;align-items:center;justify-content:center}
.sidebar-close-btn:hover{color:var(--bs-body-color)!important;background:var(--app-muted)!important}
.filter-toggle-btn.is-active{background:var(--app-muted)!important;color:var(--app-accent)!important;border-color:var(--app-accent)!important}
.request-status-grid{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:.65rem;margin:0 0 1rem}
.status-card{display:block;width:100%;min-width:0;text-align:left;padding:.85rem 1rem;border:1px solid var(--app-border)!important;border-radius:.65rem;background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;white-space:normal}
.status-card:hover,.status-card:focus{border-color:var(--app-accent)!important;box-shadow:0 0 0 2px rgba(2,132,199,.12);color:var(--bs-body-color)!important}
.status-card.is-active{border-color:var(--app-accent)!important;box-shadow:inset 0 -2px 0 var(--app-accent);background:var(--app-muted)!important}
.status-card-label{display:flex;align-items:center;gap:.45rem;color:var(--app-muted-fg);font-size:.78rem;font-weight:600;margin-bottom:.45rem}
.status-card-value{font-size:1.65rem;line-height:1.1;letter-spacing:-.04em;font-weight:700}
.status-dot{display:inline-block;width:.55rem;height:.55rem;border-radius:100%;flex:0 0 auto}
.status-dot-all{background:var(--app-accent)}.status-dot-waiting{background:#94a3b8}.status-dot-validation{background:#f59e0b}.status-dot-paid{background:#16a34a}.status-dot-rejected{background:#dc2626}
.request-table-card{min-height:480px}
.request-table-card .card-body{padding:.65rem 1rem 1rem;min-height:420px}
.dashboard-layout{display:grid;grid-template-columns:minmax(0,1fr) minmax(260px,310px);gap:1.15rem;align-items:start;max-width:1900px;margin:0 auto;padding:1.25rem clamp(1rem,2.1vw,2.4rem) 2rem;transition:grid-template-columns .2s ease}
.dashboard-layout.sidebar-collapsed{grid-template-columns:minmax(0,1fr) 0;gap:0}
.dashboard-layout.sidebar-collapsed .dashboard-sidebar{display:none!important}
@media(max-width:1150px){
  .requests-layout,.dashboard-layout{display:block;position:relative}
  .requests-sidebar{display:none;position:absolute;top:4.8rem;left:clamp(.5rem,1.8vw,1.5rem);z-index:1050;width:min(94vw,620px);max-height:80vh;overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg)!important;box-shadow:0 16px 40px rgba(0,0,0,.35)}
  .requests-sidebar.is-open{display:block!important;animation:popoverFadeIn .18s ease-out}
  .requests-sidebar-head{display:none!important}
  .requests-sidebar-body{display:grid;grid-template-columns:1fr 1fr;gap:.2rem .85rem;padding:1rem 1.15rem 1.15rem}
  .requests-sidebar-body .wide{grid-column:1/-1}
  .requests-sidebar-body .sidebar-divider{display:none}
  .requests-sidebar-body .clear-filter-row{grid-column:1/-1;display:flex;justify-content:flex-end;margin-top:.4rem}
  .requests-sidebar-body #reset{width:auto!important;padding:.35rem .85rem;font-size:.85rem}
  .dashboard-sidebar{display:none;position:absolute;top:5.5rem;right:clamp(.5rem,2vw,1.5rem);z-index:1050;width:min(94vw,540px);max-height:80vh;overflow-y:auto;border-radius:var(--app-radius);box-shadow:0 16px 40px rgba(0,0,0,.35)}
  .dashboard-sidebar.is-open{display:block!important;animation:popoverFadeIn .18s ease-out}
  .dashboard-sidebar .dashboard-filter-card{border:1px solid var(--app-border)!important}
  .dashboard-sidebar .card-header{display:none!important}
  .dashboard-sidebar .card-body{display:grid;grid-template-columns:1fr 1fr;gap:.25rem .85rem;padding:1.15rem}
  .dashboard-sidebar .card-body .chart-note{grid-column:1/-1}
}
@media(max-width:600px){
  .requests-sidebar-body,.dashboard-sidebar .card-body{grid-template-columns:1fr}
  .requests-sidebar-body .clear-filter-row #reset{width:100%!important}
}
@keyframes popoverFadeIn{from{opacity:0;transform:translateY(-6px)}to{opacity:1;transform:translateY(0)}}
.dashboard-main{min-width:0}
.dashboard-sidebar{position:sticky;top:1rem;max-height:calc(100vh - 4rem);overflow-y:auto}
.dashboard-filter-card{border:1px solid var(--app-border)!important;border-radius:var(--app-radius);background:var(--app-card-bg)!important}
.dashboard-filter-card .card-body{padding:1rem}
.dashboard-filter-card .shiny-input-container{width:100%;margin:0 0 .75rem}
.dashboard-filter-card .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg);margin-bottom:.25rem}
.dashboard-filter-card .form-control,.dashboard-filter-card .form-select,.dashboard-filter-card .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.dashboard-filter-heading{display:flex;align-items:center;gap:.5rem;font-size:.95rem;font-weight:650}
.dashboard-chart-title{display:flex;align-items:center;justify-content:space-between;gap:.75rem;flex-wrap:wrap}
.dashboard-chart-title .shiny-input-container{margin:0;min-width:230px}
.dashboard-main-card{margin-top:1rem}
.dashboard-main-chart-body{padding:.7rem 1.1rem 1.1rem}
.currency-btn-group .shiny-options-group{display:inline-flex;align-items:center;background:var(--app-muted);border:1px solid var(--app-border);border-radius:.5rem;padding:3px;gap:3px}
.currency-btn-group .radio-inline{position:relative;margin:0!important;padding:0!important;cursor:pointer}
.currency-btn-group .radio-inline input[type='radio']{position:absolute;opacity:0;width:0;height:0;pointer-events:none}
.currency-btn-group .radio-inline span{display:inline-block;padding:.32rem .85rem;font-size:.8rem;font-weight:600;color:var(--app-muted-fg);border-radius:.4rem;transition:all .15s ease-in-out;user-select:none}
.currency-btn-group .radio-inline input[type='radio']:checked+span{background:var(--app-accent);color:#fff!important;box-shadow:0 1px 3px rgba(0,0,0,.15)}
.currency-btn-group .radio-inline:hover span{color:var(--bs-body-color)}
.currency-btn-group .radio-inline input[type='radio']:checked:hover span{color:#fff!important}
.currency-btn-group .shiny-input-container{margin-bottom:0!important}
.currency-btn-group .control-label{display:none!important}
.dashboard-lower-grid{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1.3fr);gap:1rem;margin-top:1rem}
.dashboard-heading{align-items:flex-start}
.dashboard-meta{display:flex;align-items:center;gap:.5rem 1rem;justify-content:flex-end;flex-wrap:wrap;color:var(--app-muted-fg);font-size:.78rem;padding-top:.35rem}
.dashboard-meta span{display:inline-flex;align-items:center;gap:.35rem}
.metric-card{position:relative}
.metric-card::before{content:'';position:absolute;left:0;top:.75rem;bottom:.75rem;width:3px;border-radius:3px;background:var(--app-accent)}
.metric-card.metric-paid::before{background:#16a34a}.metric-card.metric-waiting::before{background:#94a3b8}.metric-card.metric-validation::before{background:#f59e0b}.metric-card.metric-rejected::before{background:#dc2626}
.metric-comparison{display:inline-flex;align-items:center;gap:.3rem;font-weight:600;font-size:.76rem;margin-top:.3rem}
/* Spending changes are neutral analytics, not a good/bad indicator. */
.metric-comparison.positive,.metric-comparison.negative{color:var(--app-muted-fg)}
.dashboard-metrics-grid{grid-template-columns:repeat(4,minmax(0,1fr))!important}
.manage-layout{max-width:1700px;margin:0 auto;padding:1.3rem clamp(1rem,2.2vw,2.5rem) 2rem}
.manage-two-col{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem;align-items:start}
.manage-card .card-body{padding:1rem}
.defaults-card{position:sticky;top:1rem}
.defaults-card .action-bar{position:static;margin-top:.8rem}
.manage-card .shiny-input-container{width:100%;margin-bottom:.75rem}
.manage-card .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
.manage-card .form-control,.manage-card .form-select,.manage-card .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.manage-note{font-size:.82rem;line-height:1.55;color:var(--app-muted-fg)}
html,body{min-height:100vh}
body.bslib-page-navbar{min-height:100vh;display:flex;flex-direction:column}
.tab-content{flex:1 0 auto}
.app-footer{max-width:1900px;margin:auto auto 0;padding:1.25rem 2rem;border-top:1px solid var(--app-border);color:var(--app-muted-fg);font-size:.78rem;text-align:center;width:100%}
@media(max-width:1150px){.request-status-grid{grid-template-columns:repeat(3,minmax(0,1fr))}.metrics-grid{grid-template-columns:repeat(3,minmax(0,1fr))}.dashboard-metrics-grid{grid-template-columns:repeat(2,minmax(0,1fr))!important}.dashboard-lower-grid{grid-template-columns:1fr}}
@media(max-width:992px){.dashboard-layout{grid-template-columns:1fr}.dashboard-sidebar{position:static;max-height:none;order:-1}}
@media(max-width:920px){.requests-layout{grid-template-columns:1fr}.requests-sidebar{position:static;max-height:none;grid-column:auto;grid-row:auto;order:-1}.requests-main{grid-column:auto;grid-row:auto}.manage-two-col{grid-template-columns:1fr}}
@media(max-width:720px){.dashboard-breakdown-title{align-items:flex-start}}
@media(max-width:600px){.request-status-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.status-card-value{font-size:1.35rem}.requests-layout,.dashboard-layout{padding:.75rem}.dashboard-shell,.manage-layout{padding:.8rem .75rem 1.2rem}.metrics-grid{grid-template-columns:1fr}.dashboard-metrics-grid{grid-template-columns:1fr!important}.dashboard-chart-title .shiny-input-container{min-width:100%}}

"

js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('setTableStatus', function(m) {
    var status = (m && m.status) ? m.status : '';
    var control = $('#request-status-filter');
    if (control.length) control.val(status).trigger('change');
    else Shiny.setInputValue('table_status_filter', status, {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('mark', function(m) {
    $('.req-bad').removeClass('req-bad');
    (m.ids || []).forEach(function(id) {
      $('#' + id).closest('.shiny-input-container').addClass('req-bad');
    });
  });
  Shiny.addCustomMessageHandler('scrollbad', function(m) {
    setTimeout(function() {
      var el = $('.req-bad').first();
      if (el.length) el[0].scrollIntoView({behavior: 'smooth', block: 'center'});
    }, 80);
  });
  Shiny.addCustomMessageHandler('badge', function(m) {
    $('#fcount').text(m.n).css('display', m.n > 0 ? 'inline-block' : 'none');
  });
});
// a field counts as 'touched' once the user leaves it
$(document).on('focusout', '.nr .shiny-input-container', function() {
  var id = this.id || $(this).find('select,input,textarea').first().attr('id');
  if (!id) return;
  setTimeout(function() {
    Shiny.setInputValue('touched', id, {priority: 'event'});
  }, 300);
});
// keep the filter dropdown open while picking dates / options
$(document).on('hide.bs.dropdown', '.filter-dd', function(e) {
  var ce = e.clickEvent;
  if (ce && $(ce.target).closest('.datepicker, .datepicker-dropdown, .selectize-dropdown').length)
    e.preventDefault();
});
// copy button in the share dialog
$(document).on('click', '#copy_link', function() {
  var t = document.getElementById('share_url');
  t.select();
  if (navigator.clipboard) navigator.clipboard.writeText(t.value);
  else document.execCommand('copy');
  $(this).text('Copied');
});
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('badge', function(m) {
    $('#fcount').text(m.n).css('display', m.n > 0 ? 'inline-block' : 'none');
  });
});
// Toggle and close collapsible/popover filter for Requests
$(document).on('click', '#toggle_requests_filter', function(e) {
  e.preventDefault();
  e.stopPropagation();
  if (window.innerWidth <= 1150) {
    $('.requests-sidebar').toggleClass('is-open');
    var isOpen = $('.requests-sidebar').hasClass('is-open');
    $('#toggle_requests_filter').toggleClass('is-active', isOpen);
  } else {
    var layout = $('.requests-layout');
    layout.toggleClass('sidebar-collapsed');
    var isCollapsed = layout.hasClass('sidebar-collapsed');
    $('#toggle_requests_filter').toggleClass('is-active', !isCollapsed);
    if (window.DataTable) {
      setTimeout(function() {
        $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
      }, 220);
    }
  }
});
$(document).on('click', '#close_requests_filter', function(e) {
  e.preventDefault();
  if (window.innerWidth <= 1150) {
    $('.requests-sidebar').removeClass('is-open');
    $('#toggle_requests_filter').removeClass('is-active');
  } else {
    $('.requests-layout').addClass('sidebar-collapsed');
    $('#toggle_requests_filter').removeClass('is-active');
  }
});
// Toggle and close collapsible/popover filter for Dashboards
$(document).on('click', '#toggle_dash_filter', function(e) {
  e.preventDefault();
  e.stopPropagation();
  if (window.innerWidth <= 1150) {
    $('.dashboard-sidebar').toggleClass('is-open');
    var isOpen = $('.dashboard-sidebar').hasClass('is-open');
    $('#toggle_dash_filter').toggleClass('is-active', isOpen);
  } else {
    var layout = $('.dashboard-layout');
    layout.toggleClass('sidebar-collapsed');
    var isCollapsed = layout.hasClass('sidebar-collapsed');
    $('#toggle_dash_filter').toggleClass('is-active', !isCollapsed);
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
    }, 220);
  }
});
$(document).on('click', '#close_dash_filter', function(e) {
  e.preventDefault();
  if (window.innerWidth <= 1150) {
    $('.dashboard-sidebar').removeClass('is-open');
    $('#toggle_dash_filter').removeClass('is-active');
  } else {
    $('.dashboard-layout').addClass('sidebar-collapsed');
    $('#toggle_dash_filter').removeClass('is-active');
  }
});
// Close popovers on click outside on medium/lower screens
$(document).on('click', function(e) {
  if (window.innerWidth <= 1150) {
    if (!$(e.target).closest('.requests-sidebar, #toggle_requests_filter, .datepicker, .datepicker-dropdown, .selectize-dropdown').length) {
      $('.requests-sidebar').removeClass('is-open');
      $('#toggle_requests_filter').removeClass('is-active');
    }
    if (!$(e.target).closest('.dashboard-sidebar, #toggle_dash_filter, .datepicker, .datepicker-dropdown, .selectize-dropdown').length) {
      $('.dashboard-sidebar').removeClass('is-open');
      $('#toggle_dash_filter').removeClass('is-active');
    }
  }
});
$(document).on('click', '.requests-sidebar, .dashboard-sidebar', function(e) {
  if (window.innerWidth <= 1150) {
    e.stopPropagation();
  }
});
"

# first 5 words in the row; 'see more' opens the full text in a popup
more_render <- function(title) JS(sub("TITLE", title, fixed = TRUE, r"---(
function(data, type, row) {
  if (type !== 'display' || data === null || data === undefined) return data;
  var esc = function(s) {
    return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;')
                    .replace(/>/g,'&gt;').replace(/"/g,'&quot;');
  };
  var words = String(data).trim().split(/\s+/);
  if (words.length <= 5) return esc(data);
  return esc(words.slice(0,5).join(' ')) + '\u2026 <a href="#" class="dmore" ' +
         'data-title="TITLE #' + row[0] + '" data-full="' + esc(data) +
         '">see more</a>';
}
)---"))

# DataTables toolbar adds a status selector beside the global search field.
# The same initializer wires each row's Open button and the description's see-more link.
request_table_init <- function(selected_status = "") {
  selected_status <- if (selected_status %in% STATUS) selected_status else ""
  # Values come from the fixed STATUS catalogue; none contains quote characters.
  # A quoted JavaScript string is sufficient and avoids a separate JSON dependency.
  selected_json <- paste0('"', selected_status, '"')
  JS(sprintf(r"---(
function(settings, json) {
  var table = this.api().table().node();
  var wrapper = $(this.api().table().container());
  var top = wrapper.find('.dt-top');
  var control = $('<div class="dt-status-control"><label for="request-status-filter">Status</label><select id="request-status-filter" class="form-select form-select-sm"><option value="">All statuses</option><option value="waiting">Under review</option><option value="under validation">Pending</option><option value="paid">Paid</option><option value="rejected">Rejected</option></select></div>');
  control.find('select').val(%s);
  top.append(control);
  control.find('select').on('change', function() {
    Shiny.setInputValue('table_status_filter', $(this).val(), {priority: 'event'});
  });
  $(table).off('click.expenseOpen', 'button.open-row').on('click.expenseOpen', 'button.open-row', function(e) {
    e.preventDefault(); e.stopPropagation();
    Shiny.setInputValue('open_request_row', {id: $(this).attr('data-id'), nonce: Math.random()}, {priority: 'event'});
  });
  $(table).off('click.expenseMore', 'a.dmore').on('click.expenseMore', 'a.dmore', function(e) {
    e.preventDefault(); e.stopPropagation();
    Shiny.setInputValue('see_more', {title: $(this).attr('data-title'), text: $(this).attr('data-full'), nonce: Math.random()}, {priority: 'event'});
  });
}
)---", selected_json))
}

dt_language <- list(
  search = "", searchPlaceholder = "Search all columns...",
  paginate = list(previous = "\u2190", `next` = "\u2192"),
  emptyTable = "No requests yet", zeroRecords = "No matching requests")

# =============================================================================
# UI
# =============================================================================
ui <- function(request) {
  page_navbar(
    id = "nav", selected = "requests", fillable = FALSE,
    title = tags$a(href = ".", class = "brand", "Expense Tracker"),
    window_title = "Expense Tracker", theme = app_theme,
    header = tagList(tags$style(css), tags$script(HTML(js))),
    footer = tags$footer(class = "app-footer", "Expense Tracker · Logistics procurement tracking"),
    
    # Requests: filters remain open in a persistent left sidebar; the main workspace is the register.
    nav_panel(
      title = "Requests", value = "requests",
      div(class = "requests-layout",
          tags$aside(class = "requests-sidebar",
                     div(class = "requests-sidebar-head d-flex align-items-center justify-content-between",
                         div(class = "d-flex align-items-center gap-2", icon("sliders"), span("Filters")),
                         tags$button(type = "button", id = "close_requests_filter", class = "sidebar-close-btn",
                                     title = "Collapse filters", icon("xmark"))
                     ),
                     div(class = "requests-sidebar-body",
                         selectInput("f_month", "Month",
                                     choices = c("All months" = "", month_choices(format(Sys.Date(), "%Y-%m"))),
                                     selected = format(Sys.Date(), "%Y-%m"), width = "100%"),
                         selectizeInput("f_week", "Week", NULL, multiple = TRUE,
                                        options = list(placeholder = "All weeks")),
                         div(class = "sidebar-divider"),
                         selectizeInput("f_project", "Project code", NULL, multiple = TRUE,
                                        options = list(placeholder = "All projects")),
                         selectizeInput("f_budget_line", "Budget line", NULL, multiple = TRUE,
                                        options = list(placeholder = "Select project code(s) first")),
                         selectizeInput("f_person", "Requested by", NULL, multiple = TRUE,
                                        options = list(placeholder = "All requesters")),
                         selectizeInput("f_submitter", "Submitted by", NULL, multiple = TRUE,
                                        options = list(placeholder = "All focal points")),
                         div(class = "sidebar-divider"),
                         div(class = "clear-filter-row",
                             actionButton("reset", tagList(icon("undo"), "Clear filters"),
                                          class = "btn-outline-secondary btn-sm"))
                     )
          ),
          tags$main(class = "requests-main",
                    div(class = "page-heading",
                        div(h1("Requests"), p("Create, find, and update procurement requests.")),
                        div(class = "d-flex align-items-center gap-2",
                            actionButton("toggle_requests_filter", tagList(icon("sliders"), "Filters", tags$span(id = "fcount", class = "badge rounded-pill text-bg-primary ms-1", style = "display:none")), class = "btn-outline-secondary filter-toggle-btn is-active"),
                            actionButton("share", "Share view", icon = icon("link"), class = "btn-outline-secondary"),
                            actionButton("new_request", tagList(icon("plus"), "New request"), class = "btn-primary")
                        )
                    ),
                    uiOutput("request_status_cards"),
                    card(class = "request-table-card", fill = FALSE,
                         card_header(div(class = "table-head w-100",
                                         div(span("Request register"), p(class = "chart-note mb-0", "Select a status card or use the table status filter. Open any row to review its details.")),
                                         div(class = "buttons ms-auto",
                                             downloadButton("export", "Export", class = "btn-outline-secondary")
                                         )
                         )),
                         div(class = "card-body", DTOutput("table_requests", fill = FALSE))
                    )
          )
      )
    ),
    
    # Financial Update: displays current (un-deducted) project spending for batching and budget deduction.
    nav_panel(
      title = "Financial Update", value = "financial_update",
      div(class = "page-shell",
          div(class = "page-heading",
              div(
                h1("Financial Update"),
                p("Review current (un-deducted) expenditures grouped by project, create finance batches, export batch sheets, and confirm budget deduction.")
              )
          ),
          
          # Summary KPI Cards
          uiOutput("fin_metrics"),
          
          # Action and Filter Toolbar
          card(class = "mt-3", fill = FALSE,
               card_header(
                 div(class = "d-flex align-items-center justify-content-between flex-wrap gap-2 w-100",
                     div(class = "d-flex align-items-center gap-2 flex-grow-1", style = "max-width: 440px;",
                         selectizeInput("fin_project_filter", NULL, choices = NULL, multiple = TRUE,
                                        options = list(placeholder = "Filter by project code(s)...", plugins = list("remove_button")),
                                        width = "100%")
                     ),
                     div(class = "d-flex align-items-center gap-2 flex-wrap",
                         actionButton("fin_btn_batch", tagList(icon("layer-group"), "Batch lines"), class = "btn-primary btn-sm"),
                         downloadButton("fin_btn_export", "Export batch", class = "btn-outline-secondary btn-sm"),
                         actionButton("fin_btn_confirm", tagList(icon("check-double"), "Confirm update"), class = "btn-success btn-sm text-white")
                     )
                 )
               ),
               div(class = "card-body",
                   uiOutput("fin_batch_alert"),
                   div(class = "table-responsive",
                       DTOutput("fin_projects_table")
                   ),
                   div(class = "mt-4 pt-3 border-top",
                       div(class = "d-flex align-items-center justify-content-between mb-2",
                           span(class = "fw-bold", "Itemized Request Lines Awaiting Deduction"),
                           uiOutput("fin_lines_count")
                       ),
                       DTOutput("fin_details_table")
                   )
               )
          )
      )
    ),
    
    # Management dashboards are separate from the request register and update workflow.
    nav_panel(
      title = "Dashboards", value = "dashboards",
      div(class = "dashboard-layout",
          div(class = "dashboard-main",
              div(class = "page-heading dashboard-heading",
                  div(h1("Dashboards"), p("A clear view of paid expenditure, request value, and procurement activity.")),
                  div(class = "d-flex flex-column align-items-end gap-1",
                      div(class = "d-flex align-items-center gap-2",
                          actionButton("share_dashboard", "Share view", icon = icon("link"), class = "btn-outline-secondary btn-sm"),
                          actionButton("toggle_dash_filter", tagList(icon("sliders"), "Filters"), class = "btn-outline-secondary btn-sm filter-toggle-btn is-active")
                      ),
                      uiOutput("dashboard_meta")
                  )
              ),
              uiOutput("totals"),
              card(class = "dashboard-breakdown-card mt-3", fill = FALSE,
                   card_header(div(class = "dashboard-chart-title dashboard-breakdown-title d-flex align-items-center justify-content-between flex-wrap gap-2",
                                   div(class = "d-flex align-items-center gap-2",
                                       span(class = "fw-semibold", "Chart view:"),
                                       selectInput("dash_chart_select", NULL,
                                                   choices = c(
                                                     "Spending" = "spending",
                                                     "Breakdown" = "breakdown",
                                                     "Days to complete" = "days",
                                                     "Year comparison" = "year_compare",
                                                     "Quarter comparison" = "quarter_compare"
                                                   ),
                                                   selected = "spending", width = "220px")
                                   )
                   )),
                   div(class = "card-body",
                       conditionalPanel("input.dash_chart_select === 'spending'",
                                        p(class = "chart-note mb-3",
                                          "Spending: Tracks total paid expenditure over time by payment date. Displays daily spending across a selected month, and monthly spending across a quarter or year in the chosen currency."),
                                        plotOutput("plot_trend", height = "380px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'breakdown'",
                                        p(class = "chart-note mb-3",
                                          "Breakdown: Visualizes the distribution of requests across project codes, budget lines, requesters, or departments across procurement workflow stages."),
                                        plotOutput("plot_dim", height = "auto")
                       ),
                       conditionalPanel("input.dash_chart_select === 'days'",
                                        p(class = "chart-note mb-3",
                                          "Days to complete: Compares actual processing days taken against expected lead time based on ACTED procurement scenario standards for the selected reporting period and filters."),
                                        plotOutput("plot_days", height = "410px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'year_compare'",
                                        p(class = "chart-note mb-3",
                                          "Year comparison: Compares monthly spending (Jan–Dec) year-over-year side-by-side in the chosen currency."),
                                        plotOutput("plot_year_compare", height = "390px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'quarter_compare'",
                                        p(class = "chart-note mb-3",
                                          "Quarter comparison: Compares paid spending across quarters (Q1–Q4) side-by-side across years in the chosen currency."),
                                        plotOutput("plot_quarter_compare", height = "390px")
                       )
                   )
              )
          ),
          div(class = "dashboard-sidebar",
              card(class = "dashboard-filter-card", fill = FALSE,
                   card_header(div(class = "dashboard-filter-heading d-flex align-items-center justify-content-between w-100",
                                   div(class = "d-flex align-items-center gap-2",
                                       icon("sliders"), span("Reporting filters")
                                   ),
                                   tags$button(type = "button", id = "close_dash_filter", class = "sidebar-close-btn",
                                               title = "Collapse filters", icon("xmark"))
                   )),
                   div(class = "card-body",
                       p(class = "chart-note mb-3", "Independent from Requests filters; request value uses order date, paid spend uses payment date."),
                       
                       # Reporting period filters for single-period charts (Spending, Breakdown, Days)
                       conditionalPanel("['spending', 'breakdown', 'days'].indexOf(input.dash_chart_select) !== -1",
                                        div(class = "dashboard-filter-section mb-2",
                                            div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Reporting Period"),
                                            selectInput("dash_period_type", "Period", choices = c(
                                              "Month" = "month", "Quarter" = "quarter", "Year" = "year"
                                            ), selected = "month", width = "100%"),
                                            selectInput("dash_year", "Year", choices = setNames(format(Sys.Date(), "%Y"), format(Sys.Date(), "%Y")),
                                                        selected = format(Sys.Date(), "%Y"), width = "100%"),
                                            conditionalPanel("input.dash_period_type === 'month'",
                                                             selectInput("dash_month", "Month", choices = setNames(sprintf("%02d", 1:12), month.name),
                                                                         selected = format(Sys.Date(), "%m"), width = "100%")
                                            ),
                                            conditionalPanel("input.dash_period_type === 'quarter'",
                                                             selectInput("dash_quarter", "Quarter", choices = c(
                                                               "Q1 · Jan–Mar" = "Q1", "Q2 · Apr–Jun" = "Q2",
                                                               "Q3 · Jul–Sep" = "Q3", "Q4 · Oct–Dec" = "Q4"
                                                             ), selected = paste0("Q", ceiling(as.integer(format(Sys.Date(), "%m")) / 3)), width = "100%")
                                            )
                                        )
                       ),
                       
                       # Comparison charts notice
                       conditionalPanel("['year_compare', 'quarter_compare'].indexOf(input.dash_chart_select) !== -1",
                                        div(class = "alert alert-info py-2 px-3 small mb-3",
                                            icon("circle-info"), " ",
                                            span("Compares paid spend across all years side-by-side.")
                                        )
                       ),
                       
                       # Breakdown-specific filters: moved together to filter section
                       conditionalPanel("input.dash_chart_select === 'breakdown'",
                                        div(class = "dashboard-filter-section pt-2 border-top mb-2",
                                            div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Breakdown Settings"),
                                            selectInput("breakdown_status", "Status", choices = c(
                                              "Paid" = "paid", "Under review" = "waiting", "Pending" = "under validation", "Rejected" = "rejected"
                                            ), selected = "paid", width = "100%"),
                                            selectInput("dim", "Group by", DIMS, selected = "project_code", width = "100%"),
                                            conditionalPanel("input.dim === 'project_code'",
                                                             div(class = "mb-2",
                                                                 tags$label(class = "control-label", "Project drill-down"),
                                                                 div(class = "d-flex align-items-center gap-1",
                                                                     div(class = "flex-grow-1",
                                                                         selectInput("breakdown_project", NULL, choices = c("Select a project" = ""),
                                                                                     selected = "", width = "100%")
                                                                     ),
                                                                     actionButton("clear_breakdown_project", "Clear", class = "btn btn-outline-secondary btn-sm mb-3", title = "Clear project drill-down")
                                                                 ),
                                                                 p(class = "form-hint small text-muted mt-n2 mb-2", "Select a project to inspect its budget lines.")
                                                             )
                                            )
                                        )
                       ),
                       
                       # Scope filters (Department & Project) apply across charts
                       div(class = "dashboard-filter-section pt-2 border-top",
                           div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Scope Filters"),
                           selectizeInput("dash_department", "Department", NULL, multiple = TRUE,
                                          options = list(placeholder = "All departments")),
                           selectizeInput("dash_project", "Project code", NULL, multiple = TRUE,
                                          options = list(placeholder = "All projects"))
                       ),
                       
                       # Reset button for reporting filters
                       div(class = "pt-2 mt-2 border-top",
                           actionButton("reset_dash_filter", tagList(icon("rotate-left"), "Reset filters"),
                                        class = "btn btn-outline-secondary btn-sm w-100")
                       )
                   )
              )
          )
      )
    ),
    
    # Manage keeps the project catalogue, suppliers, email preferences, people, and defaults together.
    nav_panel(
      title = "Manage", value = "manage",
      div(class = "manage-layout",
          div(class = "page-heading",
              div(h1("Manage"), p("Maintain project catalogues, suppliers, contacts, notification recipients, and request defaults."))
          ),
          navset_tab(
            nav_panel("Project & Supplier", value = "manage_catalog",
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("folder-open"), " Projects & budget lines")),
                               div(class = "card-body nr",
                                   div(class = "form-section-title", "Add a project code"),
                                   div(class = "fg",
                                       col(12, textInput("cfg_project_code", "Project code *", "", width = "100%", placeholder = "e.g. EDUAMH2026")),
                                       col(12, selectInput("cfg_project_department", "Department", c("Not specified" = "", DEPTS), selected = "", width = "100%")),
                                       col(12, p(class = "form-hint", "Letters-and-numbers examples are guidance only; existing naming formats are allowed.")),
                                       col(12, actionButton("cfg_add_project", tagList(icon("plus"), "Add project"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Add a budget line"),
                                   div(class = "fg",
                                       col(12, selectInput("cfg_budget_project", "Project code *", choices = c("Select project" = ""), selected = "", width = "100%")),
                                       col(12, textInput("cfg_budget_line", "Budget line *", "", width = "100%", placeholder = "e.g. TRAVEL01")),
                                       col(12, actionButton("cfg_add_budget_line", tagList(icon("plus"), "Add budget line"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Configured projects"),
                                   DTOutput("settings_catalog_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_project_selected", tagList(icon("edit"), "Edit selected project"), class = "btn-outline-secondary btn-sm"))
                               )
                          ),
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("truck"), " Suppliers")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Supplier names added here are available in the New request form. Historical expense supplier text is preserved."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_supplier_name", "Supplier name *", "", width = "100%", placeholder = "Supplier or vendor name")),
                                       col(12, actionButton("cfg_add_supplier", tagList(icon("plus"), "Add supplier"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Supplier register"),
                                   DTOutput("settings_supplier_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_supplier_selected", tagList(icon("edit"), "Edit selected supplier"), class = "btn-outline-secondary btn-sm"))
                               )
                          )
                      )
            ),
            nav_panel("Emails, People & other", value = "manage_people",
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("envelope"), " Email notifications")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Store recipients and the events they want to receive. Actual sending still needs an SMTP/mail provider and a scheduled job."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_notify_email", "Email address *", "", width = "100%", placeholder = "team@example.org")),
                                       col(12, checkboxInput("cfg_notify_weekly", "Weekly request summary", value = TRUE)),
                                       col(12, actionButton("cfg_add_notification_email", tagList(icon("plus"), "Save recipient"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Recipients and subscriptions"),
                                   DTOutput("settings_email_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_email_selected", tagList(icon("edit"), "Edit selected recipient"), class = "btn-outline-secondary btn-sm"))
                               )
                          ),
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("users"), " People")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Maintain the staff/contact list separately from request records. Existing requester names remain available for historical data."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_person_name", "Full name *", "", width = "100%", placeholder = "Staff member name")),
                                       col(12, selectInput("cfg_person_department", "Department", c("Not specified" = "", DEPTS), selected = "", width = "100%")),
                                       col(12, actionButton("cfg_add_person", tagList(icon("plus"), "Add person"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "People register"),
                                   DTOutput("settings_people_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_person_selected", tagList(icon("edit"), "Edit selected person"), class = "btn-outline-secondary btn-sm"))
                               )
                          )
                      ),
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("cog"), " Defaults")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Defaults prefill future requests. Changing them does not rewrite existing records."),
                                   div(class = "fg",
                                       col(12, selectizeInput("cfg_default_submitter", "Logistics focal point", choices = DEFAULTS$submitted_by,
                                                              selected = DEFAULTS$submitted_by, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a name"))),
                                       col(12, selectizeInput("cfg_default_country", "Default country", choices = DEFAULTS$country,
                                                              selected = DEFAULTS$country, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a country"))),
                                       col(12, selectInput("cfg_default_currency", "Default currency", choices = CUR,
                                                           selected = DEFAULTS$currency, width = "100%")),
                                       col(12, div(class = "action-bar", actionButton("cfg_save_defaults", tagList(icon("save"), "Save defaults"), class = "btn-primary")))
                                   )
                               )
                          )
                      )
            )
          )
      )
    ),
    
    # Full-page request form; kept as a programmatically opened page.
    nav_panel(
      title = "New request", value = "new",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("New request"), p("Enter a procurement request. Fields without an asterisk are optional.")),
              actionButton("cancel_new", tagList(icon("arrow-left"), "Back to requests"), class = "btn-outline-secondary")
          ),
          layout_columns(fill = FALSE, col_widths = c(8, 4),
                         card(fill = FALSE,
                              div(class = "card-body",
                                  uiOutput("new_form_ui"),
                                  div(class = "action-bar",
                                      actionButton("clear", "Clear form", class = "btn-outline-secondary"),
                                      actionButton("cancel_new_bottom", "Cancel", class = "btn-outline-secondary"),
                                      actionButton("add", "Add request", class = "btn-primary")
                                  )
                              )
                         ),
                         card(card_header("Request preview"), div(class = "card-body", uiOutput("preview")))
          )
      )
    ),
    nav_spacer(),
    nav_item(
      div(class = "d-flex align-items-center gap-1 global-currency-wrap",
          tags$label(`for` = "global_cur", class = "text-muted small fw-semibold text-nowrap me-1 mb-0", "Currency:"),
          selectInput("global_cur", NULL, choices = CUR, selected = DEFAULTS$currency, width = "95px")
      )
    ),
    nav_item(actionButton("settings_open", NULL, icon = icon("gear"),
                          class = "btn-outline-secondary", title = "Manage", `aria-label` = "Manage")),
    nav_item(input_dark_mode(id = "mode"))
  )
}
# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  touched  <- reactiveVal(character())
  show_all <- reactiveVal(FALSE)
  edit_id  <- reactiveVal(NULL)
  request_table_status <- reactiveVal("")
  project_edit_id <- reactiveVal(NULL)
  supplier_edit_id <- reactiveVal(NULL)
  person_edit_id <- reactiveVal(NULL)
  email_edit_id <- reactiveVal(NULL)
  
  observeEvent(input$table_status_filter, {
    request_table_status(v(input$table_status_filter))
  }, ignoreInit = FALSE)
  set_request_status <- function(status = "") {
    status <- if (status %in% STATUS) status else ""
    request_table_status(status)
    session$sendCustomMessage("setTableStatus", list(status = status))
  }
  observeEvent(input$card_status_all, set_request_status(""), ignoreInit = TRUE)
  observeEvent(input$card_status_waiting, set_request_status("waiting"), ignoreInit = TRUE)
  observeEvent(input$card_status_validation, set_request_status("under validation"), ignoreInit = TRUE)
  observeEvent(input$card_status_paid, set_request_status("paid"), ignoreInit = TRUE)
  observeEvent(input$card_status_rejected, set_request_status("rejected"), ignoreInit = TRUE)
  
  
  # ---- read live rows from the shared SQLite database ----
  # Keep this connection open for PRAGMA data_version, which changes after commits
  # by other connections (including other Shiny sessions and external writers).
  poll_cn <- con()
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(poll_cn)) DBI::dbDisconnect(poll_cn)
  })
  db_poll <- reactivePoll(
    1000, session,
    checkFunc = function() as.character(dbGetQuery(poll_cn, "PRAGMA data_version")[[1]][[1]]),
    valueFunc = function() {
      rows <- dbGetQuery(poll_cn,
                         "SELECT e.*, p.project_code, bl.budget_line_code AS budget_line
         FROM expenses e
         LEFT JOIN budget_lines bl ON bl.id = e.budget_line_id
         LEFT JOIN projects p ON p.id = bl.project_id
         ORDER BY e.id DESC")
      # Include the database revision so catalogue/default changes invalidate dependants
      # even when the expense rows themselves are unchanged.
      attr(rows, "database_revision") <- as.character(dbGetQuery(poll_cn, "PRAGMA data_version")[[1]][[1]])
      rows
    }
  )
  dashboard_last_refresh <- reactiveVal(Sys.time())
  observeEvent(db_poll(), dashboard_last_refresh(Sys.time()), ignoreInit = FALSE)
  
  data <- reactive({
    d <- db_poll()
    # Backward compatibility for older rows saved with the status "cancelled".
    if ("status" %in% names(d)) d$status[!is.na(d$status) & d$status == "cancelled"] <- "rejected"
    # USD is fixed at entry, so scenario and totals don't move when RATES change.
    # as.numeric() keeps this working when the table is still empty.
    usd <- as.numeric(ifelse(is.na(d$usd),
                             as.numeric(d$amount) / unname(RATES[d$currency]),
                             d$usd))
    for (k in CUR)
      d[[k]] <- as.numeric(ifelse(d$currency %in% k, d$amount,
                                  round(usd * RATES[[k]], 2)))
    d$scenario      <- scenario(usd, d$waiver)
    date_value <- suppressWarnings(as.Date(d$order_date))
    d$week          <- format(date_value, "%G-W%V")
    d$month         <- format(date_value, "%Y-%m")
    d$year          <- format(date_value, "%Y")
    month_number    <- suppressWarnings(as.integer(format(date_value, "%m")))
    d$quarter       <- ifelse(!is.na(month_number), paste0("Q", ceiling(month_number / 3)), NA_character_)
    d
  })
  
  week_choices <- function(d, selected_month = "") {
    d <- d[!is.na(d$week), , drop = FALSE]
    if (nzchar(v(selected_month))) d <- d[d$month == selected_month, , drop = FALSE]
    if (!nrow(d)) return(character())
    summary <- d |> group_by(week) |>
      summarise(first = min(as.Date(order_date)), .groups = "drop") |>
      arrange(desc(week))
    monday <- summary$first - (as.integer(format(summary$first, "%u")) - 1L)
    setNames(summary$week, paste0(summary$week, " (from ", format(monday, "%d %b"), ")"))
  }
  
  budget_filter_choices <- function(project_values = character()) {
    # Budget lines are only selectable after a project is selected, and always come
    # from the configured project/budget-line catalogue in tracker.db.
    if (is.null(project_values) || !length(project_values)) return(character())
    catalog <- budget_catalog_db(project_values)
    if (!nrow(catalog)) return(character())
    setNames(as.character(catalog$id), paste0(catalog$project_code, " — ", catalog$budget_line_code))
  }
  
  # Populate filters from the database. Month constrains Week; selecting a project
  # constrains Budget line to that project's configured mappings.
  observe({
    d <- data()
    keep <- function(id) {
      x <- isolate(input[[id]])
      if (is.null(x)) character() else as.character(x)
    }
    current_month <- format(Sys.Date(), "%Y-%m")
    available_months <- unique(c(d$month, current_month))
    month_selected <- isolate(input$f_month) %||% current_month
    updateSelectInput(session, "f_month",
                      choices = c("All months" = "", month_choices(available_months)),
                      selected = month_selected)
    # Project choices come from the projects table, including newly configured projects
    # that do not yet have an expense record.
    project_choices <- project_codes_db()
    project_selected <- intersect(keep("f_project"), project_choices)
    updateSelectizeInput(session, "f_project", choices = project_choices,
                         selected = project_selected)
    updateSelectInput(session, "cfg_budget_project",
                      choices = c("Select project" = "", setNames(project_choices, project_choices)),
                      selected = v(isolate(input$cfg_budget_project)))
    department_choices <- uniq(c(DEPTS, d$department))
    updateSelectizeInput(session, "dash_department", choices = department_choices,
                         selected = intersect(keep("dash_department"), department_choices))

    # Dynamic cascading: if department(s) are selected in dashboard, restrict dash_project choices
    selected_dash_dept <- if (is.null(input$dash_department)) character() else as.character(input$dash_department)
    if (length(selected_dash_dept)) {
      db_projs <- tryCatch(
        db_get("SELECT project_code, department FROM projects"),
        error = function(e) data.frame(project_code = character(), department = character())
      )
      matched_db <- db_projs$project_code[db_projs$department %in% selected_dash_dept]
      matched_data <- d$project_code[d$department %in% selected_dash_dept]
      dash_project_choices <- sort(unique(c(matched_db, matched_data)))
      dash_project_choices <- dash_project_choices[!is.na(dash_project_choices) & nzchar(dash_project_choices)]
    } else {
      dash_project_choices <- project_choices
    }

    dash_project_selected <- intersect(keep("dash_project"), dash_project_choices)
    updateSelectizeInput(session, "dash_project", choices = dash_project_choices,
                         selected = dash_project_selected)
    years <- sort(unique(c(d$year,
                           format(suppressWarnings(as.Date(d$payment_date)), "%Y"),
                           format(Sys.Date(), "%Y"))), decreasing = TRUE)
    years <- years[!is.na(years) & nzchar(years)]
    updateSelectInput(session, "dash_year", choices = setNames(years, years),
                      selected = isolate(input$dash_year) %||% format(Sys.Date(), "%Y"))
    updateSelectizeInput(session, "f_person", choices = uniq(d$requested_by),
                         selected = keep("f_person"))
    updateSelectizeInput(session, "f_submitter", choices = uniq(d$submitted_by),
                         selected = keep("f_submitter"))
    
    # Keep these as reactive reads so the dependent choices refresh when the user changes them.
    project_selection <- if (is.null(input$f_project)) character() else as.character(input$f_project)
    selected_month <- input$f_month %||% ""
    bchoices <- budget_filter_choices(project_selection)
    budget_selected <- intersect(keep("f_budget_line"), unname(bchoices))
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = budget_selected,
                         options = list(placeholder = if (length(project_selection))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    
    # Request breakdown uses a separate, database-backed project selector when
    # Group by = Project. Without a selected project, show project totals only;
    # selecting one project reveals its total followed by its budget lines.
    current_dim <- isolate(input$dim) %||% "project_code"
    if (!(current_dim %in% unname(DIMS))) current_dim <- "project_code"
    updateSelectInput(session, "dim", choices = DIMS, selected = current_dim)
    
    breakdown_projects <- if (length(selected_dash_dept)) dash_project_choices else project_codes_db()
    if (length(dash_project_selected)) {
      breakdown_projects <- intersect(breakdown_projects, dash_project_selected)
    }
    breakdown_project_selected <- keep("breakdown_project")
    if (!length(breakdown_project_selected) ||
        !breakdown_project_selected[[1]] %in% breakdown_projects) {
      breakdown_project_selected <- ""
    } else {
      breakdown_project_selected <- breakdown_project_selected[[1]]
    }
    updateSelectInput(session, "breakdown_project",
                      choices = c("Select a project" = "", breakdown_projects),
                      selected = breakdown_project_selected)
    
    wchoices <- week_choices(d, selected_month)
    week_selected <- intersect(keep("f_week"), unname(wchoices))
    updateSelectizeInput(session, "f_week", choices = wchoices, selected = week_selected,
                         options = list(placeholder = "All weeks in selected month"))
  })

  active_currency <- function() {
    c_val <- v(input$global_cur)
    if (is.null(c_val) || !nzchar(c_val) || !c_val %in% CUR) "ETB" else c_val
  }

  observeEvent(input$reset_dash_filter, {
    updateSelectInput(session, "dash_period_type", selected = "month")
    updateSelectInput(session, "dash_year", selected = format(Sys.Date(), "%Y"))
    updateSelectInput(session, "dash_month", selected = format(Sys.Date(), "%m"))
    updateSelectInput(session, "dash_quarter", selected = paste0("Q", ceiling(as.integer(format(Sys.Date(), "%m")) / 3)))
    updateSelectizeInput(session, "dash_project", selected = character(0))
    updateSelectizeInput(session, "dash_department", selected = character(0))
    updateSelectInput(session, "breakdown_status", selected = "paid")
    updateSelectInput(session, "dim", selected = "project_code")
    updateSelectInput(session, "breakdown_project", selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_breakdown_project, {
    updateSelectInput(session, "breakdown_project", selected = "")
    updateSelectizeInput(session, "dash_project", selected = character(0))
  }, ignoreInit = TRUE)
  
  # ---- filters: month + week work together ----
  base <- reactive({
    d <- data()
      if (nzchar(v(input$f_month))) d <- filter(d, month == input$f_month)
      if (length(input$f_week)) d <- filter(d, week %in% input$f_week)
    if (length(input$f_project)) d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_budget_line)) d <- filter(d, budget_line_id %in% as.integer(input$f_budget_line))
    if (length(input$f_person)) d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_submitter)) d <- filter(d, submitted_by %in% input$f_submitter)
    d
  })
  filtered <- reactive(base())
  
  # Dashboard period helpers. Request values/status counts follow order_date; actual paid spend follows payment_date.
  dashboard_period_bounds <- function(offset = 0L) {
    period_type <- v(input$dash_period_type)
    if (!period_type %in% c("month", "quarter", "year")) period_type <- "month"
    selected_year <- suppressWarnings(as.integer(v(input$dash_year)))
    if (is.na(selected_year)) selected_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (period_type == "month") {
      month_num <- suppressWarnings(as.integer(v(input$dash_month)))
      if (is.na(month_num) || month_num < 1 || month_num > 12) {
        month_num <- as.integer(format(Sys.Date(), "%m"))
      }
      selected_start <- as.Date(sprintf("%04d-%02d-01", selected_year, month_num))
      shift_month_start <- function(date, month_offset) {
        month_index <- as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m")) - 1L + as.integer(month_offset)
        new_year <- month_index %/% 12L
        new_month <- month_index %% 12L + 1L
        as.Date(sprintf("%04d-%02d-01", new_year, new_month))
      }
      start_date <- shift_month_start(selected_start, offset)
      next_start <- shift_month_start(start_date, 1L)
    } else if (period_type == "quarter") {
      quarter_num <- suppressWarnings(as.integer(gsub("[^1-4]", "", v(input$dash_quarter))))
      if (is.na(quarter_num) || quarter_num < 1 || quarter_num > 4) {
        quarter_num <- ceiling(as.integer(format(Sys.Date(), "%m")) / 3)
      }
      start_month <- (quarter_num - 1L) * 3L + 1L
      selected_start <- as.Date(sprintf("%04d-%02d-01", selected_year, start_month))
      month_index <- as.integer(format(selected_start, "%Y")) * 12L + as.integer(format(selected_start, "%m")) - 1L + (3L * as.integer(offset))
      shifted_year <- month_index %/% 12L
      shifted_month <- month_index %% 12L + 1L
      start_date <- as.Date(sprintf("%04d-%02d-01", shifted_year, shifted_month))
      next_month_index <- month_index + 3L
      next_year <- next_month_index %/% 12L
      next_month <- next_month_index %% 12L + 1L
      next_start <- as.Date(sprintf("%04d-%02d-01", next_year, next_month))
    } else {
      start_date <- as.Date(sprintf("%04d-01-01", selected_year + offset))
      next_start <- as.Date(sprintf("%04d-01-01", selected_year + offset + 1L))
    }
    list(start = start_date, end = next_start - 1)
  }
  
  dashboard_period_label <- function(offset = 0L) {
    bounds <- dashboard_period_bounds(offset)
    period_type <- v(input$dash_period_type)
    if (period_type == "quarter") {
      paste0("Q", ceiling(as.integer(format(bounds$start, "%m")) / 3), " ", format(bounds$start, "%Y"))
    } else if (period_type == "year") {
      format(bounds$start, "%Y")
    } else {
      format(bounds$start, "%B %Y")
    }
  }
  
  dashboard_scope <- function(d, date_column = "order_date", offset = 0L) {
    bounds <- dashboard_period_bounds(offset)
    dates <- suppressWarnings(as.Date(d[[date_column]]))
    keep <- !is.na(dates) & dates >= bounds$start & dates <= bounds$end
    d <- d[keep, , drop = FALSE]
    if (length(input$dash_project)) d <- d[d$project_code %in% input$dash_project, , drop = FALSE]
    if (length(input$dash_department)) d <- d[d$department %in% input$dash_department, , drop = FALSE]
    d
  }
  
  # Dashboard filters are independent from the Requests sidebar filters.
  dashboard_filtered <- reactive(dashboard_scope(data(), "order_date"))
  dashboard_paid_filtered <- reactive({
    dashboard_scope(data(), "payment_date") |>
      filter(status == "paid")
  })
  dashboard_previous_paid <- reactive({
    dashboard_scope(data(), "payment_date", offset = -1L) |>
      filter(status == "paid")
  })
  
  output$dashboard_meta <- renderUI({
    invisible(data())
    div(class = "dashboard-meta",
        span(icon("calendar-days"), paste("Period:", dashboard_period_label())),
        span(icon("coins"), paste("Currency:", active_currency()))
    )
  })
  
  observeEvent(input$reset, {
    # Clear means no active filters (All months), unlike the fresh-session default,
    # which intentionally starts on the current month.
    updateSelectInput(session, "f_month", selected = "")
    updateSelectizeInput(session, "f_week", selected = character(0))
    for (id in c("f_project", "f_budget_line", "f_person", "f_submitter"))
      updateSelectizeInput(session, id, selected = character(0))
    # Also clear the project selector within Request breakdown so no secondary
    # selection continues to narrow the breakdown after dashboard filters reset.
    updateSelectInput(session, "breakdown_project", selected = "")
  }, ignoreInit = TRUE)
  
  observe({
    n <- sum(nzchar(v(input$f_month)), length(input$f_week) > 0,
             length(input$f_project) > 0, length(input$f_budget_line) > 0,
             length(input$f_person) > 0, length(input$f_submitter) > 0)
    session$sendCustomMessage("badge", list(n = n))
  })
  
  # Short, shareable URL containing only active filters.
  share_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    q <- list(
      v = "r2",
      m = if (nzchar(v(input$f_month))) input$f_month else NULL,
      w = if (length(input$f_week)) enc(input$f_week) else NULL,
      p = if (length(input$f_project)) enc(input$f_project) else NULL,
      b = if (length(input$f_budget_line)) enc(as.character(input$f_budget_line)) else NULL,
      r = if (length(input$f_person)) enc(input$f_person) else NULL,
      s = if (length(input$f_submitter)) enc(input$f_submitter) else NULL
    )
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname,
                   if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }
  
  observeEvent(input$share, {
    showModal(modalDialog(
      title = "Share this view",
      p(class = "chart-note", "Anyone with the link can open this view with the same filters. No request data is embedded in the URL."),
      div(class = "input-group",
          tags$input(id = "share_url", class = "form-control", readonly = NA,
                     value = share_url(), onclick = "this.select()"),
          tags$button(id = "copy_link", type = "button", class = "btn btn-primary", "Copy link")),
      footer = modalButton("Close"), easyClose = TRUE))
  })
  
  # The gear icon now navigates to the full Manage workspace instead of opening a modal.
  observeEvent(input$settings_open, nav_select("nav", "manage", session = session))
  
  project_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT p.id, p.project_code, COALESCE(p.department,'') AS department,
      COALESCE(p.location,'') AS location,
      COALESCE(GROUP_CONCAT(bl.budget_line_code, ', '), '') AS budget_lines,
      COALESCE(p.last_deducted_at, '') AS last_deducted_at
      FROM projects p LEFT JOIN budget_lines bl ON bl.project_id = p.id
      GROUP BY p.id ORDER BY p.project_code COLLATE NOCASE")
  })
  
  supplier_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, supplier_name, COALESCE(email,'') AS email, COALESCE(phone,'') AS phone,
                   active FROM suppliers ORDER BY supplier_name COLLATE NOCASE")
  })
  
  people_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, full_name, COALESCE(email,'') AS email, COALESCE(department,'') AS department,
                   active FROM people ORDER BY full_name COLLATE NOCASE")
  })
  
  notification_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, email, weekly_report, request_created, status_changed, active
            FROM notification_emails ORDER BY email COLLATE NOCASE")
  })
  
  output$settings_catalog_table <- renderDT({
    catalog <- project_catalog_view()
    catalog$last_deducted_at <- NULL
    datatable(catalog, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 8, lengthChange = FALSE, searching = TRUE,
                             scrollX = TRUE, dom = "ftip", columnDefs = list(
                               list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Project code", "Department", "Location", "Budget lines"))
  })
  
  output$settings_supplier_table <- renderDT({
    sup <- supplier_catalog_view() |>
      mutate(status = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, supplier_name, status)
    datatable(sup, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 8, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Supplier", "Status"))
  })
  
  output$settings_people_table <- renderDT({
    ppl <- people_catalog_view() |>
      mutate(status = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, full_name, department, status)
    datatable(ppl, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Full name", "Department", "Status"))
  })
  
  output$settings_email_table <- renderDT({
    em <- notification_catalog_view() |>
      mutate(weekly_label = ifelse(weekly_report == 1, "Yes", "No"),
             active_label = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, email, weekly_label, active_label)
    datatable(em, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Email", "Weekly summary", "Status"))
  })
  
  # Edit forms are opened from a selected catalogue row. Records are updated in-place;
  # no request history is deleted when a catalogue label changes.
  observeEvent(input$edit_project_selected, {
    idx <- input$settings_catalog_table_rows_selected
    catalog <- project_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(catalog)) {
      showNotification("Select a project row first.", type = "warning"); return()
    }
    r <- catalog[idx[[1]], , drop = FALSE]
    showModal(modalDialog(title = paste("Edit project", r$project_code),
                          textInput("edit_project_code", "Project code", r$project_code, width = "100%"),
                          selectInput("edit_project_department", "Department", with_sel(c("Not specified" = "", DEPTS), r$department),
                                      selected = r$department, width = "100%"),
                          textInput("edit_project_location", "Default location", r$location, width = "100%"),
                          footer = tagList(modalButton("Cancel"), actionButton("save_project_edit", "Save changes", class = "btn-primary")),
                          easyClose = TRUE))
    project_edit_id(as.integer(r$id[[1]]))
  })
  
  observeEvent(input$save_project_edit, {
    id <- project_edit_id(); code <- trimws(v(input$edit_project_code))
    if (is.null(id) || !nzchar(code)) { showNotification("Project code is required.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE AND id <> ?", code, as.integer(id))) > 0) {
      showNotification("That project code is already in use.", type = "warning"); return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE projects SET project_code=?, department=?, location=?, updated=? WHERE id=?",
              params = list(code, if (nzchar(v(input$edit_project_department))) v(input$edit_project_department) else NA_character_,
                            if (nzchar(v(input$edit_project_location))) v(input$edit_project_location) else NA_character_, now(), as.integer(id)))
    project_edit_id(NULL); removeModal()
    showNotification("Project updated.", type = "message")
  })
  
  observeEvent(input$edit_supplier_selected, {
    idx <- input$settings_supplier_table_rows_selected; sup <- supplier_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(sup)) { showNotification("Select a supplier row first.", type = "warning"); return() }
    r <- sup[idx[[1]], , drop = FALSE]; supplier_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit supplier", r$supplier_name),
                          textInput("edit_supplier_name", "Supplier name", r$supplier_name, width = "100%"),
                          checkboxInput("edit_supplier_active", "Active supplier", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_supplier_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_supplier_edit, {
    id <- supplier_edit_id(); name <- trimws(v(input$edit_supplier_name))
    if (is.null(id) || !nzchar(name)) { showNotification("Supplier name is required.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM suppliers WHERE supplier_name = ? COLLATE NOCASE AND id <> ?", name, as.integer(id))) > 0) { showNotification("That supplier name already exists.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE suppliers SET supplier_name=?,active=?,updated=? WHERE id=?",
              params = list(name, as.integer(isTRUE(input$edit_supplier_active)), now(), as.integer(id)))
    supplier_edit_id(NULL); removeModal()
    updateSelectizeInput(session, "supplier", choices = c("", supplier_catalog_db()), selected = if (isTRUE(input$edit_supplier_active)) name else "",
                         options = list(create = TRUE, placeholder = "Type or choose a supplier"))
    showNotification("Supplier updated. Existing expense text was left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_person_selected, {
    idx <- input$settings_people_table_rows_selected; ppl <- people_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(ppl)) { showNotification("Select a person row first.", type = "warning"); return() }
    r <- ppl[idx[[1]], , drop = FALSE]; person_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit person", r$full_name),
                          textInput("edit_person_name", "Full name", r$full_name, width = "100%"),
                          selectInput("edit_person_department", "Department", with_sel(c("Not specified" = "", DEPTS), r$department), selected = r$department, width = "100%"),
                          checkboxInput("edit_person_active", "Active person", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_person_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_person_edit, {
    id <- person_edit_id(); name <- trimws(v(input$edit_person_name)); dept <- v(input$edit_person_department)
    if (is.null(id) || !nzchar(name)) { showNotification("Person name is required.", type = "warning"); return() }
    # email removed
    if (nrow(db_get("SELECT id FROM people WHERE full_name = ? COLLATE NOCASE AND id <> ?", name, as.integer(id))) > 0) { showNotification("That name already belongs to another person.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE people SET full_name=?,department=?,active=?,updated=? WHERE id=?",
              params = list(name, if (nzchar(dept)) dept else NA_character_, as.integer(isTRUE(input$edit_person_active)), now(), as.integer(id)))
    person_edit_id(NULL); removeModal(); showNotification("Person updated. Historical request names were left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_email_selected, {
    idx <- input$settings_email_table_rows_selected; em <- notification_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(em)) { showNotification("Select an email recipient row first.", type = "warning"); return() }
    r <- em[idx[[1]], , drop = FALSE]; email_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit recipient", r$email),
                          textInput("edit_notify_email", "Email address", r$email, width = "100%"),
                          checkboxInput("edit_notify_weekly", "Weekly request summary", value = as.integer(r$weekly_report) == 1),
                          checkboxInput("edit_notify_active", "Active recipient", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_email_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_email_edit, {
    id <- email_edit_id(); email <- tolower(trimws(v(input$edit_notify_email)))
    if (is.null(id) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) { showNotification("Enter a valid email address.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM notification_emails WHERE email = ? COLLATE NOCASE AND id <> ?", email, as.integer(id))) > 0) { showNotification("That notification email already exists.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE notification_emails SET email=?,weekly_report=?,request_created=0,status_changed=0,active=?,updated=? WHERE id=?",
              params = list(email, as.integer(isTRUE(input$edit_notify_weekly)), as.integer(isTRUE(input$edit_notify_active)), now(), as.integer(id)))
    email_edit_id(NULL); removeModal(); showNotification("Notification recipient updated.", type = "message")
  })
  
  observeEvent(input$cfg_add_project, {
    code <- trimws(v(input$cfg_project_code))
    if (!nzchar(code)) {
      showNotification("Enter a project code first.", type = "warning")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE", code)) > 0
    if (exists) {
      showNotification("That project code already exists. Choose it in the budget-line section instead.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    ensure_project_record(cn, code, v(input$cfg_project_department), "")
    updateTextInput(session, "cfg_project_code", value = "")
    codes <- project_codes_db()
    updateSelectInput(session, "cfg_budget_project",
                      choices = c("Select project" = "", setNames(codes, codes)), selected = code)
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(codes, codes)),
                         selected = isolate(input$project_code) %||% "")
    # Refresh the dashboard filter from the catalogue immediately, not after the next poll.
    selected_projects <- isolate(input$f_project) %||% character()
    updateSelectizeInput(session, "f_project", choices = codes, selected = selected_projects)
    bchoices <- budget_filter_choices(selected_projects)
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = intersect(isolate(input$f_budget_line) %||% character(), unname(bchoices)),
                         options = list(placeholder = if (length(selected_projects))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    showNotification(paste("Project", code, "added."), type = "message")
  })
  
  observeEvent(input$cfg_add_budget_line, {
    code <- trimws(v(input$cfg_budget_project))
    line <- trimws(v(input$cfg_budget_line))
    if (!nzchar(code) || !nzchar(line)) {
      showNotification("Choose a project and enter a budget line.", type = "warning")
      return()
    }
    p_row <- db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE", code)
    if (!nrow(p_row)) {
      showNotification("The selected project code is no longer available.", type = "error")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM budget_lines WHERE project_id = ? AND budget_line_code = ? COLLATE NOCASE",
                          as.integer(p_row$id[[1]]), line)) > 0
    if (exists) {
      showNotification("That budget line is already configured for this project.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    ensure_budget_line_record(cn, as.integer(p_row$id[[1]]), line)
    updateTextInput(session, "cfg_budget_line", value = "")
    if (identical(tolower(v(isolate(input$project_code))), tolower(code))) {
      lines <- budget_catalog_db(code)$budget_line_code
      updateSelectizeInput(session, "budget_line", choices = c("Select budget line" = "", setNames(lines, lines)),
                           selected = "", options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
    }
    # Refresh the dependent dashboard budget-line filter directly after the insert.
    selected_projects <- isolate(input$f_project) %||% character()
    bchoices <- budget_filter_choices(selected_projects)
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = intersect(isolate(input$f_budget_line) %||% character(), unname(bchoices)),
                         options = list(placeholder = if (length(selected_projects))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    showNotification(paste("Budget line added to", code, "."), type = "message")
  })
  
  observeEvent(input$cfg_add_supplier, {
    supplier_name <- trimws(v(input$cfg_supplier_name))
    if (!nzchar(supplier_name)) {
      showNotification("Enter a supplier name first.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM suppliers WHERE supplier_name = ? COLLATE NOCASE", params = list(supplier_name))) > 0
    if (exists) {
      showNotification("That supplier already exists.", type = "warning")
      return()
    }
    dbExecute(cn, "INSERT INTO suppliers(supplier_name,email,phone,active,created,updated) VALUES(?,?,?,?,?,?)",
              params = list(supplier_name, NA_character_, NA_character_, 1L, now(), now()))
    updateTextInput(session, "cfg_supplier_name", value = "")
    updateSelectizeInput(session, "supplier", choices = c("", supplier_catalog_db()), selected = supplier_name,
                         options = list(create = TRUE, placeholder = "Type or pick"))
    showNotification(paste("Supplier", supplier_name, "added."), type = "message")
  })
  
  observeEvent(input$cfg_add_person, {
    full_name <- trimws(v(input$cfg_person_name))
    department <- v(input$cfg_person_department)
    if (!nzchar(full_name)) {
      showNotification("Enter the person's name first.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM people WHERE full_name = ? COLLATE NOCASE",
                              params = list(full_name))) > 0
    if (exists) {
      showNotification("That person already exists.", type = "warning")
      return()
    }
    dbExecute(cn, "INSERT INTO people(full_name,department,active,created,updated) VALUES(?,?,?,?,?)",
              params = list(full_name, if (nzchar(department)) department else NA_character_, 1L, now(), now()))
    updateTextInput(session, "cfg_person_name", value = "")
    updateSelectInput(session, "cfg_person_department", selected = "")
    showNotification(paste(full_name, "added to People."), type = "message")
  })
  
  observeEvent(input$cfg_add_notification_email, {
    email <- tolower(trimws(v(input$cfg_notify_email)))
    if (!nzchar(email) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
      showNotification("Enter a valid notification email address.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "INSERT INTO notification_emails(email,weekly_report,request_created,status_changed,active,created,updated)
      VALUES(?,?,0,0,1,?,?)
      ON CONFLICT(email) DO UPDATE SET weekly_report=excluded.weekly_report,
        request_created=0,status_changed=0,
        active=excluded.active,updated=excluded.updated",
              params = list(email, as.integer(isTRUE(input$cfg_notify_weekly)), now(), now()))
    updateTextInput(session, "cfg_notify_email", value = "")
    updateCheckboxInput(session, "cfg_notify_weekly", value = TRUE)
    showNotification("Notification recipient saved.", type = "message")
  })
  
  observeEvent(input$cfg_save_defaults, {
    focal <- trimws(v(input$cfg_default_submitter))
    country <- trimws(v(input$cfg_default_country))
    currency <- v(input$cfg_default_currency)
    if (!nzchar(focal) || !currency %in% CUR) {
      showNotification("Enter a logistics focal point and choose an available default currency.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbBegin(cn)
    saved_defaults <- tryCatch({
      values <- list(submitted_by = focal, country = country, currency = currency)
      for (key in names(values)) {
        dbExecute(cn, "INSERT OR REPLACE INTO app_defaults(setting_key, setting_value, updated) VALUES(?,?,?)",
                  params = list(key, values[[key]], now()))
      }
      dbCommit(cn)
      TRUE
    }, error = function(e) {
      try(dbRollback(cn), silent = TRUE)
      showNotification(paste("Could not save defaults:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved_defaults) return()
    DEFAULTS <<- read_app_defaults()
    showNotification("Request defaults saved to the database.", type = "message")
    # Apply the new defaults immediately to this session; future sessions also read them from SQLite.
    updateSelectizeInput(session, "submitted_by", selected = focal,
                         choices = with_sel(uniq(c(data()$submitted_by, focal)), focal))
    updateSelectizeInput(session, "country", selected = country,
                         choices = with_sel(uniq(c(data()$country, country)), country))
    updateSelectInput(session, "currency", selected = currency)
    updateRadioButtons(session, "cur", selected = currency)
  })
  
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q) || is.null(q[["v"]])) return()
    vals <- function(k) if (is.null(q[[k]]) || !nzchar(q[[k]])) character()
    else strsplit(q[[k]], "|", fixed = TRUE)[[1]]

    if (identical(q[["v"]], "dash")) {
      nav_select("nav", "dashboards", session = session)
      if (!is.null(q[["cur"]])) updateSelectInput(session, "global_cur", selected = q[["cur"]])
      if (!is.null(q[["period"]])) updateSelectInput(session, "dash_period_type", selected = q[["period"]])
      if (!is.null(q[["yr"]])) updateSelectInput(session, "dash_year", selected = q[["yr"]])
      if (!is.null(q[["m"]])) updateSelectInput(session, "dash_month", selected = q[["m"]])
      if (!is.null(q[["q"]])) updateSelectInput(session, "dash_quarter", selected = q[["q"]])
      if (!is.null(q[["p"]])) {
        psel <- vals("p")
        updateSelectizeInput(session, "dash_project", selected = psel)
      }
      if (!is.null(q[["d"]])) {
        dsel <- vals("d")
        updateSelectizeInput(session, "dash_department", selected = dsel)
      }
      if (!is.null(q[["chart"]])) updateSelectInput(session, "dash_chart_select", selected = q[["chart"]])
      if (!is.null(q[["bstat"]])) updateSelectInput(session, "breakdown_status", selected = q[["bstat"]])
      if (!is.null(q[["bdim"]])) updateSelectInput(session, "dim", selected = q[["bdim"]])
      if (!is.null(q[["bp"]])) updateSelectInput(session, "breakdown_project", selected = q[["bp"]])
      return()
    }

    if (!(q[["v"]] %in% c("r", "r2"))) return()
    d <- isolate(data())
    if (!is.null(q[["m"]]) && !is.na(q[["m"]])) {
      updateSelectInput(session, "f_month", selected = q[["m"]])
    } else {
      # A shared view without an explicit month represents an unfiltered
      # date view; do not reapply the fresh-session current-month default.
      updateSelectInput(session, "f_month", selected = "")
    }
    set <- function(id, key, choices) {
      selected <- vals(key)
      if (length(selected)) updateSelectizeInput(session, id,
                                                 choices = with_sel(choices, selected), selected = selected)
    }
    set("f_project", "p", project_codes_db())
    set("f_person", "r", uniq(d$requested_by))
    set("f_submitter", "s", uniq(d$submitted_by))
    set("f_week", "w", week_choices(d, if (is.null(q[["m"]])) "" else q[["m"]]))
    bchoices <- budget_filter_choices(vals("p"))
    bsel <- intersect(vals("b"), unname(bchoices))
    updateSelectizeInput(session, "f_budget_line", choices = bchoices, selected = bsel)
  }, once = TRUE)
  
  # ---- New request is a full page, not a modal ----
  observeEvent(input$new_request, {
    DEFAULTS <<- read_app_defaults()
    touched(character()); show_all(FALSE)
    updateSelectizeInput(session, "submitted_by", selected = DEFAULTS$submitted_by,
                         choices = with_sel(uniq(c(isolate(data())$submitted_by, DEFAULTS$submitted_by)), DEFAULTS$submitted_by))
    updateSelectizeInput(session, "country", selected = DEFAULTS$country,
                         choices = with_sel(uniq(c(isolate(data())$country, DEFAULTS$country)), DEFAULTS$country))
    updateSelectInput(session, "currency", selected = DEFAULTS$currency)
    nav_select("nav", "new", session = session)
  })
  observeEvent(input$cancel_new, nav_select("nav", "requests", session = session))
  observeEvent(input$cancel_new_bottom, nav_select("nav", "requests", session = session))
  
  output$new_form_ui <- renderUI({
    codes <- project_codes_db()
    div(class = "nr",
        div(class = "form-section-title", "Request details"),
        div(class = "fg",
            col(12, textAreaInput("description", R("Description"), rows = 2, width = "100%")),
            col(4, cached("requested_by", "Requested by")),
            col(4, selectizeInput("project_code", R("Project code"),
                                  choices = c("Select project" = "", setNames(codes, codes)),
                                  selected = "", width = "100%",
                                  options = list(create = FALSE, placeholder = "Choose a configured project"))),
            col(4, selectizeInput("budget_line", R("Budget line"), choices = character(),
                                  selected = "", width = "100%",
                                  options = list(create = FALSE, placeholder = "Choose project first"))),
            col(4, pick("department", "Department", DEPTS)),
            col(4, cached("location", "Location")),
            col(4, selectizeInput("supplier", R("Supplier"),
                                  choices = c("" , setNames(supplier_catalog_db(), supplier_catalog_db())),
                                  selected = "", width = "100%",
                                  options = list(create = TRUE, placeholder = "Type or choose a supplier"))),
            col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
            col(4, pick("currency", "Currency", CUR))
        ),
        div(class = "form-section-rule"),
        div(class = "form-section-title", "Optional details"),
        div(class = "fg",
            col(4, cached("country", "Country", req = FALSE)),
            col(4, numericInput("quantity", "Quantity", NA, min = 0, width = "100%")),
            col(4, cached("unit", "Unit", req = FALSE)),
            col(4, div(style = "padding-top:1.9rem", checkboxInput("framework", "Framework agreement"))),
            col(6, cached("submitted_by", "Submitted by (focal point)", req = FALSE)),
            col(6, date_in("of_date", "Order date", Sys.Date(), width = "100%")),
            col(12, textAreaInput("remarks", "Remarks", rows = 2, width = "100%"))
        )
    )
  })
  
  # ---- new request: required fields (TRUE = filled) ----
  valid <- reactive(c(
    description  = nzchar(v(input$description)),
    requested_by = nzchar(v(input$requested_by)),
    supplier     = nzchar(v(input$supplier)),
    project_code = nzchar(v(input$project_code)),
    budget_line  = nzchar(v(input$budget_line)),
    department   = nzchar(v(input$department)),
    location     = nzchar(v(input$location)),
    amount       = has_num(input$amount) && input$amount > 0,
    currency     = nzchar(v(input$currency))
  ))
  
  observeEvent(input$touched, touched(union(touched(), input$touched)))
  
  # red = required but empty (after leaving the field, or after pressing Add)
  observe({
    bad  <- names(valid())[!valid()]
    show <- if (show_all()) bad else intersect(bad, touched())
    session$sendCustomMessage("mark", list(ids = as.list(show)))
  })
  
  waiver_val <- reactive(if (isTRUE(input$framework)) "Framework agreement" else "No")
  odate <- reactive(if (has_date(input$of_date)) input$of_date else Sys.Date())
  
  form_rec <- reactive(list(
    project_code = v(input$project_code), budget_line = v(input$budget_line),
    country = v(input$country), location = v(input$location),
    department = v(input$department), order_date = as.character(odate())))
  
  # picking a project suggests its department and location (still editable)
  observeEvent(input$project_code, {
    pc <- v(input$project_code); req(nzchar(pc))
    p <- db_get("SELECT department, location FROM projects WHERE project_code = ?", pc)
    if (!nrow(p)) return()
    if (nzchar(v(p$department)))
      updateSelectInput(session, "department", selected = p$department)
    if (nzchar(v(p$location)))
      updateSelectizeInput(session, "location",
                           choices = c("", union(cache_choices(isolate(data()), "location"), p$location)),
                           selected = p$location,
                           options = list(create = TRUE, placeholder = "Type or pick"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$project_code, {
    pc <- v(input$project_code)
    lines <- if (nzchar(pc)) budget_catalog_db(pc)$budget_line_code else character()
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = "", setNames(lines, lines)),
                         selected = character(0),
                         options = list(create = FALSE, placeholder = if (nzchar(pc)) "Choose a mapped budget line" else "Choose project first"))
  }, ignoreInit = TRUE)
  
  observe({
    invisible(data())
    codes <- project_codes_db()
    current_project <- isolate(input$project_code) %||% ""
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(codes, codes)), selected = current_project,
                         options = list(create = FALSE, placeholder = "Choose a configured project"))
    lines <- if (nzchar(v(current_project))) budget_catalog_db(current_project)$budget_line_code else character()
    current_line <- isolate(input$budget_line) %||% ""
    selected_line <- if (current_line %in% lines) current_line else ""
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = "", setNames(lines, lines)), selected = selected_line,
                         options = list(create = FALSE, placeholder = if (nzchar(v(current_project))) "Choose a mapped budget line" else "Choose project first"))
  })
  
  output$preview <- renderUI({
    of  <- mk_ref(form_rec(), "OF")
    cur <- v(input$currency); amt <- input$amount
    shown <- function(x) if (is.na(x)) "\u2014" else x
    top <- if (!nzchar(cur) || !has_num(amt)) {
      p(class = "muted", "Enter amount and currency to see the scenario.")
    } else {
      usd <- amt / RATES[[cur]]
      s   <- scenario(usd, waiver_val())
      tagList(
        p(span(paste("Scenario", s), class = "badge badge-scen"), " ",
          span(SCEN_NAME[[s]], class = "muted")),
        p(class = "mb-2", tags$b("Expected time: "), LEAD[[s]]),
        tags$table(class = "table table-sm",
                   lapply(CUR, function(k)
                     tags$tr(tags$td(k, class = "muted"),
                             tags$td(money(usd * RATES[[k]]), style = "text-align:right"))))
      )
    }
    div(top,
        p(class = "muted mb-0", "Order form"), p(shown(of), class = "ref"),
        p(class = "muted mb-0", "Purchase order"),
        p("Generated when status moves from Under review to Pending", class = "ref muted"))
  })
  
  # back to the starting values (defaults prefilled, the rest empty)
  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    DEFAULTS <<- read_app_defaults()
    d <- isolate(data())
    for (id in names(CACHE)) {
      val <- DEFAULTS[[id]] %||% ""
      ch <- cache_choices(d, id)
      if (identical(id, "supplier")) ch <- uniq(c(ch, supplier_catalog_db()))
      ch <- union(ch, val[nzchar(val)])
      updateSelectizeInput(session, id, choices = c("", ch), selected = val,
                           options = list(create = TRUE, placeholder = "Type or pick"))
    }
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(project_codes_db(), project_codes_db())), selected = "")
    updateSelectizeInput(session, "budget_line", choices = character(), selected = "",
                         options = list(create = FALSE, placeholder = "Choose project first"))
    for (id in c("department", "currency"))
      updateSelectInput(session, id, selected = DEFAULTS[[id]] %||% "")
    updateCheckboxInput(session, "framework", value = FALSE)
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    suppressWarnings(updateDateInput(session, "of_date", value = Sys.Date()))
    updateTextAreaInput(session, "remarks", value = "")
    touched(character()); show_all(FALSE)
  }
  observeEvent(input$clear, reset_form())
  
  observeEvent(input$add, {
    if (!all(valid())) {      # button stays active: show what is missing instead
      show_all(TRUE)
      session$sendCustomMessage("scrollbad", list())
      showNotification("Some required fields are empty.", type = "warning")
      return()
    }
    cur  <- input$currency
    rate <- RATES[[cur]]
    rec  <- form_rec()
    row <- list(
      description = v(input$description), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$country),
      location = rec$location, department = rec$department,
      reason = NA_character_, order_form = mk_ref(rec, "OF"), po = NA_character_,
      requested_by = v(input$requested_by),
      submitted_by = if (nzchar(v(input$submitted_by))) v(input$submitted_by)
      else DEFAULTS$submitted_by,
      status = "waiting", waiver = waiver_val(), supplier = v(input$supplier),
      unit = nz(input$unit),
      quantity = if (has_num(input$quantity)) input$quantity else NA_real_,
      amount = input$amount, currency = cur,
      usd = input$amount / rate, rate_used = rate,
      order_date = rec$order_date, po_date = NA_character_,
      payment_date = NA_character_, remarks = nz(input$remarks),
      created = now(), updated = now())
    cn <- con(); on.exit(dbDisconnect(cn))
    supplier_name <- trimws(v(row$supplier))
    if (nzchar(supplier_name)) {
      dbExecute(cn, "INSERT OR IGNORE INTO suppliers(supplier_name,active,created,updated) VALUES(?,1,?,?)",
                params = list(supplier_name, now(), now()))
    }
    saved <- tryCatch({ insert_expense_row(cn, row); TRUE }, error = function(e) {
      showNotification(paste("Request was not saved:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved) return()
    reset_form()
    nav_select("nav", "requests", session = session)
    showNotification("Request added.", type = "message")
  })
  
  # ---- Open an individual request using the row-level Open button ----
  render_request_modal <- function(id, edit_mode = FALSE) {
    r <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(r)) {
      showNotification("That request is no longer available. Refresh the table and try again.", type = "warning")
      return()
    }
    r <- r[1, , drop = FALSE]
    
    reference_record <- list(
      project_code = v(r$project_code), budget_line = v(r$budget_line),
      country = v(r$country), location = v(r$location), department = v(r$department),
      order_date = v(r$order_date), reason = v(r$reason), po_date = v(r$po_date)
    )
    order_ref <- v(r$order_form)
    if (!nzchar(order_ref)) order_ref <- v(mk_ref(reference_record, "OF"))
    if (!nzchar(order_ref)) order_ref <- "Unavailable — check project, budget line, location, department and order date"
    
    po_ref <- v(r$po)
    po_is_preview <- FALSE
    if (!nzchar(po_ref) && v(r$status) %in% c("under validation", "paid")) {
      candidate_po <- v(mk_ref(reference_record, "PO"))
      if (nzchar(candidate_po)) {
        po_ref <- candidate_po
        po_is_preview <- TRUE
      }
    }
    if (!nzchar(po_ref)) {
      po_ref <- if (v(r$status) %in% c("under validation", "paid")) {
        "Not generated — provide a PO reason and PO date, then save."
      } else {
        "Generated when the request moves from Under review to Pending."
      }
    }
    
    if (!edit_mode) {
      showModal(modalDialog(
        title = div(class = "d-flex justify-content-between align-items-center w-100 pe-3",
                    span(paste0("Request #", r$id)),
                    span(class = paste0("badge status-pill status-", gsub(" ", "-", v(r$status))), status_label(v(r$status)))),
        size = "l", easyClose = TRUE,
        div(class = "request-view-container",
            div(class = "d-flex justify-content-between align-items-center mb-3 pb-2 border-bottom",
                div(
                  div(class = "text-muted small", "Amount"),
                  div(class = "fs-4 fw-bold", paste(r$currency, money(r$amount)))
                ),
                actionButton("start_edit_modal", tagList(icon("pen-to-square"), "Edit request"), class = "btn btn-primary")
            ),
            div(class = "form-section-title", "Request details"),
            div(class = "request-details-grid",
                div(class = "detail-item wide", tags$label("Description"), div(class = "detail-val", v(r$description))),
                div(class = "detail-item", tags$label("Requested by"), div(class = "detail-val", v(r$requested_by))),
                div(class = "detail-item", tags$label("Submitted by"), div(class = "detail-val", if (nzchar(v(r$submitted_by))) v(r$submitted_by) else "-")),
                div(class = "detail-item", tags$label("Project code"), div(class = "detail-val", v(r$project_code))),
                div(class = "detail-item", tags$label("Budget line"), div(class = "detail-val", v(r$budget_line))),
                div(class = "detail-item", tags$label("Department"), div(class = "detail-val", v(r$department))),
                div(class = "detail-item", tags$label("Location"), div(class = "detail-val", v(r$location))),
                div(class = "detail-item", tags$label("Country"), div(class = "detail-val", if (nzchar(v(r$country))) v(r$country) else "-")),
                div(class = "detail-item", tags$label("Supplier"), div(class = "detail-val", v(r$supplier))),
                div(class = "detail-item", tags$label("Quantity & Unit"), div(class = "detail-val", paste(if (!is.na(r$quantity)) r$quantity else "-", if (nzchar(v(r$unit))) v(r$unit) else ""))),
                div(class = "detail-item", tags$label("Framework agreement"), div(class = "detail-val", if (identical(v(r$waiver), "Framework agreement")) "Yes" else "No")),
                div(class = "detail-item", tags$label("Order date"), div(class = "detail-val", if (!is.na(r$order_date)) format(as.Date(r$order_date), "%d %b %Y") else "-")),
                if (v(r$status) %in% c("under validation", "paid"))
                  tagList(
                    div(class = "detail-item", tags$label("PO reason"), div(class = "detail-val", if (nzchar(v(r$reason))) v(r$reason) else "-")),
                    div(class = "detail-item", tags$label("PO date"), div(class = "detail-val", if (!is.na(r$po_date)) format(as.Date(r$po_date), "%d %b %Y") else "-"))
                  ),
                if (identical(v(r$status), "paid"))
                  div(class = "detail-item", tags$label("Payment date"), div(class = "detail-val", if (!is.na(r$payment_date)) format(as.Date(r$payment_date), "%d %b %Y") else "-")),
                if (nzchar(v(r$remarks)))
                  div(class = "detail-item wide", tags$label("Remarks"), div(class = "detail-val", v(r$remarks)))
            ),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item",
                        div(class = "reference-label", "Order Form (OF)"),
                        tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item",
                        div(class = "reference-label", "Purchase Order (PO)"),
                        tags$code(class = "reference-value", po_ref))
                )
            )
        ),
        footer = tagList(
          modalButton("Close"),
          actionButton("start_edit_modal", tagList(icon("pen-to-square"), "Edit request"), class = "btn-primary")
        )
      ))
    } else {
      showModal(modalDialog(
        title = paste0("Edit Request #", r$id), size = "l", easyClose = TRUE,
        div(class = "form-intro", "Fields marked * are required. Edit values below and click Save changes."),
        div(class = "nr",
            div(class = "form-section-title", "Request details"),
            div(class = "fg",
                col(12, textAreaInput("m_desc", "Description *", v(r$description), rows = 2, width = "100%")),
                col(6, textInput("m_requested_by", "Requested by *", v(r$requested_by), width = "100%")),
                col(6, textInput("m_submitted_by", "Submitted by", v(r$submitted_by), width = "100%")),
                col(4, selectizeInput("m_project", "Project code *",
                                      choices = setNames(project_codes_db(), project_codes_db()), selected = v(r$project_code), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a configured project"))),
                col(4, selectizeInput("m_budget", "Budget line *",
                                      choices = setNames(budget_catalog_db(v(r$project_code))$budget_line_code,
                                                         budget_catalog_db(v(r$project_code))$budget_line_code),
                                      selected = v(r$budget_line), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a mapped budget line"))),
                col(4, selectInput("m_dept", "Department *", unique(c(DEPTS, v(r$department))), selected = v(r$department), width = "100%")),
                col(4, textInput("m_loc", "Location *", v(r$location), width = "100%")),
                col(4, textInput("m_country", "Country", v(r$country), width = "100%")),
                col(4, textInput("m_supplier", "Supplier *", v(r$supplier), width = "100%")),
                col(4, numericInput("m_amount", "Amount *", r$amount, min = 0, width = "100%")),
                col(4, selectInput("m_cur", "Currency *", CUR, selected = r$currency, width = "100%")),
                col(4, numericInput("m_quantity", "Quantity", if (is.na(r$quantity)) NA else r$quantity, min = 0, width = "100%")),
                col(4, textInput("m_unit", "Unit", v(r$unit), width = "100%")),
                col(4, div(style = "padding-top:1.9rem", checkboxInput("m_framework", "Framework agreement", value = identical(v(r$waiver), "Framework agreement")))),
                col(4, selectInput("m_status", "Status *", choices = c(
                  "Under review" = "waiting", "Pending" = "under validation", "Paid" = "paid", "Rejected" = "rejected"
                ), selected = v(r$status), width = "100%")),
                col(8, textAreaInput("m_remarks", "Remarks", v(r$remarks), rows = 2, width = "100%")),
                col(6, date_in("m_order_date", "Order date", if (is.na(r$order_date)) Sys.Date() else as.Date(r$order_date), width = "100%")),
                col(6, conditionalPanel(
                  "input.m_status == 'under validation' || input.m_status == 'paid'",
                  div(class = "fg",
                      col(6, selectizeInput("m_reason", "PO reason *",
                                            choices = c("", uniq(c(data()$reason, r$reason))),
                                            selected = v(r$reason), width = "100%",
                                            options = list(create = TRUE, placeholder = "Type or pick"))),
                      col(6, date_in("m_po_date", "PO date *",
                                     if (is.na(r$po_date)) Sys.Date() else as.Date(r$po_date), width = "100%"))))),
                col(6, conditionalPanel(
                  "input.m_status == 'paid'",
                  date_in("m_date", "Payment date *",
                          if (is.na(r$payment_date)) Sys.Date() else as.Date(r$payment_date), width = "100%")))
            ),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item",
                        div(class = "reference-label", "Order Form (OF)"),
                        tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item",
                        div(class = "reference-label", "Purchase Order (PO)"),
                        tags$code(class = "reference-value", po_ref))),
                p(class = "reference-hint",
                  if (po_is_preview) {
                    "Preview only: save the request to persist this generated PO reference."
                  } else if (v(r$status) %in% c("under validation", "paid") && !nzchar(v(r$po))) {
                    "The PO reference is generated from the project, budget line, country, location, department, PO reason and PO date."
                  } else {
                    "The OF and PO references are stored with the request and included in exports. A PO is created when the request moves from Under review to Pending."
                  }))
        ),
        footer = tagList(
          actionButton("cancel_edit_modal", "Back to view", class = "btn-outline-secondary"),
          actionButton("m_save", tagList(icon("check"), "Save changes"), class = "btn-primary")
        )
      ))
    }
  }

  observeEvent(input$open_request_row, {
    id <- suppressWarnings(as.integer(input$open_request_row$id))
    if (length(id) != 1L || is.na(id)) {
      showNotification("Could not identify that request. Refresh the table and try again.", type = "warning")
      return()
    }
    edit_id(id)
    render_request_modal(id, edit_mode = FALSE)
  })
  
  observeEvent(input$start_edit_modal, {
    id <- edit_id()
    if (!is.null(id)) render_request_modal(id, edit_mode = TRUE)
  })
  
  observeEvent(input$cancel_edit_modal, {
    id <- edit_id()
    if (!is.null(id)) render_request_modal(id, edit_mode = FALSE)
  })
  
  observeEvent(input$m_project, {
    pc <- v(input$m_project)
    lines <- if (nzchar(pc)) budget_catalog_db(pc)$budget_line_code else character()
    # Keep the saved budget line when the modal first opens; only clear it if it no
    # longer belongs to the selected project.
    current <- isolate(input$m_budget)
    keep_line <- if (!is.null(current) && current %in% lines) current else character(0)
    updateSelectizeInput(session, "m_budget", choices = setNames(lines, lines), selected = keep_line,
                         options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$m_save, {
    id <- edit_id()
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old)) { removeModal(); return() }
    old <- old[1, , drop = FALSE]
    need_po <- input$m_status %in% c("under validation", "paid")
    ok <- all(nzchar(c(v(input$m_desc), v(input$m_requested_by), v(input$m_project),
                       v(input$m_budget), v(input$m_dept), v(input$m_loc), v(input$m_supplier)))) &&
      has_num(input$m_amount) && input$m_amount > 0 && nzchar(v(input$m_cur)) &&
      (!need_po || (nzchar(v(input$m_reason)) && has_date(input$m_po_date))) &&
      (input$m_status != "paid" || has_date(input$m_date))
    if (!ok) {
      showNotification("Please complete the required fields before saving.", type = "warning")
      return()
    }
    
    changed <- !isTRUE(all.equal(input$m_amount, old$amount)) || input$m_cur != old$currency
    rate <- if (changed || is.na(old$rate_used)) RATES[[input$m_cur]] else old$rate_used
    usd <- if (changed || is.na(old$usd)) input$m_amount / rate else old$usd
    order_date <- if (has_date(input$m_order_date)) as.character(input$m_order_date) else as.character(old$order_date)
    reason <- if (need_po) v(input$m_reason) else v(old$reason)
    po_date <- if (need_po) as.character(input$m_po_date) else as.character(old$po_date)
    rec <- list(project_code = v(input$m_project), budget_line = v(input$m_budget),
                country = v(input$m_country), location = v(input$m_loc),
                department = v(input$m_dept), order_date = order_date,
                reason = reason, po_date = po_date)
    
    # Generate the PO while a request is Under validation or Paid. This covers
    # the transition into Under validation and also repairs a missing reference
    # on an older row. If the request later leaves these statuses, retain an
    # already-issued PO reference rather than erasing it.
    order_ref <- mk_ref(rec, "OF")
    if (is.na(order_ref) || !nzchar(v(order_ref))) {
      showNotification("Could not generate the Order Form reference. Check project, budget line, location, department and order date.", type = "error")
      return()
    }
    generated_po <- if (need_po) mk_ref(rec, "PO") else NA_character_
    if (need_po && (is.na(generated_po) || !nzchar(v(generated_po)))) {
      showNotification("Could not generate the Purchase Order reference. Check the PO reason and PO date.", type = "error")
      return()
    }
    saved_po <- if (need_po) generated_po else if (nzchar(v(old$po))) v(old$po) else NA_character_
    
    row <- list(
      description = v(input$m_desc), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$m_country),
      location = rec$location, department = rec$department,
      supplier = v(input$m_supplier), requested_by = v(input$m_requested_by),
      submitted_by = if (nzchar(v(input$m_submitted_by))) v(input$m_submitted_by) else DEFAULTS$submitted_by,
      quantity = if (has_num(input$m_quantity)) input$m_quantity else NA_real_,
      unit = nz(input$m_unit), remarks = nz(input$m_remarks),
      waiver = if (isTRUE(input$m_framework)) "Framework agreement" else "No",
      amount = input$m_amount, currency = input$m_cur, usd = usd, rate_used = rate,
      status = input$m_status,
      reason = if (nzchar(reason)) reason else NA_character_,
      order_date = order_date,
      po_date = if (need_po) as.character(input$m_po_date) else as.character(old$po_date),
      order_form = order_ref,
      po = saved_po,
      payment_date = if (input$m_status == "paid") as.character(input$m_date) else NA_character_,
      updated = now()
    )
    cn <- con(); on.exit(dbDisconnect(cn))
    supplier_name <- trimws(v(row$supplier))
    if (nzchar(supplier_name)) {
      dbExecute(cn, "INSERT OR IGNORE INTO suppliers(supplier_name,active,created,updated) VALUES(?,1,?,?)",
                params = list(supplier_name, now(), now()))
    }
    saved <- tryCatch({ update_expense_row(cn, id, row); TRUE }, error = function(e) {
      showNotification(paste("Request was not updated:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved) return()
    removeModal()
    status_notice <- switch(input$m_status,
                            waiting = list(text = "Request moved to Under review.", type = "default"),
                            `under validation` = list(text = "Request moved to Pending; the PO reference was generated.", type = "warning"),
                            paid = list(text = "Request marked as Paid.", type = "message"),
                            rejected = list(text = "Request rejected.", type = "error"),
                            list(text = "Request updated.", type = "message"))
    showNotification(status_notice$text, type = status_notice$type)
  })
  
  # popup with the full description / remarks
  observeEvent(input$see_more, {
    showModal(modalDialog(
      title = input$see_more$title,
      div(style = "white-space:pre-wrap", input$see_more$text),
      footer = modalButton("Close"), easyClose = TRUE))
  })
  
  # ---- request status cards (counts only; monetary totals live in Dashboards) ----
  output$request_status_cards <- renderUI({
    d <- filtered()
    selected <- request_table_status()
    count_for <- function(status) if (!nzchar(status)) nrow(d) else sum(d$status == status, na.rm = TRUE)
    make_card <- function(id, label, status, dot, active = FALSE) {
      actionButton(id,
                   tagList(div(class = "status-card-label", span(class = paste("status-dot", dot)), label),
                           div(class = "status-card-value", format(count_for(status), big.mark = ","))),
                   class = paste("status-card", if (active) "is-active" else ""))
    }
    div(class = "request-status-grid",
        make_card("card_status_all", "All requests", "", "status-dot-all", !nzchar(selected)),
        make_card("card_status_waiting", "Under review", "waiting", "status-dot-waiting", identical(selected, "waiting")),
        make_card("card_status_validation", "Pending", "under validation", "status-dot-validation", identical(selected, "under validation")),
        make_card("card_status_paid", "Paid", "paid", "status-dot-paid", identical(selected, "paid")),
        make_card("card_status_rejected", "Rejected", "rejected", "status-dot-rejected", identical(selected, "rejected")))
  })
  
  # ---- dashboard KPIs ----
  output$totals <- renderUI({
    requests_period <- dashboard_filtered()
    paid_period <- dashboard_paid_filtered()
    paid_previous <- dashboard_previous_paid()
    currency <- active_currency()
    
    request_value <- if (nrow(requests_period)) sum(requests_period[[currency]], na.rm = TRUE) else 0
    paid_value <- if (nrow(paid_period)) sum(paid_period[[currency]], na.rm = TRUE) else 0
    previous_paid_value <- if (nrow(paid_previous)) sum(paid_previous[[currency]], na.rm = TRUE) else 0
    waiting_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "waiting"], na.rm = TRUE) else 0
    validation_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "under validation"], na.rm = TRUE) else 0
    rejected_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "rejected"], na.rm = TRUE) else 0
    
    if (previous_paid_value > 0) {
      delta <- paid_value - previous_paid_value
      pct <- (delta / previous_paid_value) * 100
      delta_class <- if (delta >= 0) "positive" else "negative"
      delta_sign <- if (delta >= 0) "+" else "−"
      paid_comparison <- div(class = paste("metric-comparison", delta_class),
                             paste0(delta_sign, currency, " ", abbr(abs(delta)), " · ", delta_sign,
                                    formatC(abs(pct), format = "f", digits = 1), "% vs ", dashboard_period_label(-1L)))
    } else if (paid_value > 0) {
      paid_comparison <- span(class = "metric-note", paste("No paid spend in", dashboard_period_label(-1L)))
    } else {
      paid_comparison <- span(class = "metric-note", paste("No paid spend in", dashboard_period_label(), "or the prior period"))
    }
    
    metric <- function(label, value, note, extra_class = "") {
      div(class = paste("metric-card", extra_class),
          p(class = "metric-label", label),
          p(class = "metric-value", paste(currency, abbr(value))),
          p(class = "metric-note", note)
      )
    }
    div(class = "metrics-grid dashboard-metrics-grid",
        div(class = "metric-card metric-paid",
            p(class = "metric-label", "Paid spend"),
            p(class = "metric-value", paste(currency, abbr(paid_value))),
            p(class = "metric-note", paste(nrow(paid_period), "paid requests by payment date")),
            # paid comparison removed
        ),
        metric("Under review", waiting_value,
               paste(sum(requests_period$status == "waiting", na.rm = TRUE), "requests awaiting initial review"), "metric-waiting"),
        metric("Pending", validation_value,
               paste(sum(requests_period$status == "under validation", na.rm = TRUE), "requests awaiting procurement completion"), "metric-validation"),
        metric("Rejected", rejected_value,
               paste(sum(requests_period$status == "rejected", na.rm = TRUE), "rejected requests"), "metric-rejected")
    )
  })
  
  # Status palette is consistent across charts, notifications and status changes.
  fg_col <- function() if (identical(input$mode, "dark")) "#e4e4e7" else "#27272a"
  status_label <- function(status) switch(status,
                                          waiting = "Under review",
                                          `under validation` = "Pending",
                                          paid = "Paid",
                                          rejected = "Rejected",
                                          status)
  
  chart_theme <- function(fg)
    theme_minimal(base_size = 12, base_family = "Inter") +
    theme(text = element_text(colour = fg), axis.text = element_text(colour = fg),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = scales::alpha(fg, 0.16)),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "bottom", legend.title = element_blank(),
          plot.margin = margin(10, 18, 5, 5))
  
  # Main report selector: request-stage reports use request/order date; spend reports use payment date.
  output$plot_dashboard_main <- renderPlot({
    currency <- active_currency()
    report <- v(input$dash_report)
    fg <- fg_col()
    
    if (report %in% c("request_value_status", "status_count")) {
      d <- dashboard_filtered()
      validate(need(nrow(d) > 0, "No requests in the selected period and scope."))
      g <- d |>
        group_by(status) |>
        summarise(total = if (report == "status_count") n() else sum(.data[[currency]], na.rm = TRUE),
                  .groups = "drop")
      g <- data.frame(status = STATUS, stringsAsFactors = FALSE) |>
        left_join(g, by = "status")
      g$total[is.na(g$total)] <- 0
      g$status <- factor(g$status, levels = STATUS)
      g$label <- vapply(as.character(g$status), status_label, character(1))
      plot <- ggplot(g, aes(x = reorder(label, total), y = total, fill = status)) +
        geom_col(width = .66) +
        geom_text(aes(label = if (report == "status_count") as.character(round(total)) else abbr(total)),
                  hjust = -.15, colour = fg, size = 3.5) +
        coord_flip(clip = "off") +
        scale_fill_manual(values = STATUS_COL, breaks = STATUS,
                          labels = vapply(STATUS, status_label, character(1)), drop = FALSE) +
        scale_y_continuous(labels = if (report == "status_count") scales::label_number(big.mark = ",") else abbr,
                           expand = expansion(mult = c(0, .27))) +
        labs(x = NULL, y = NULL,
             subtitle = if (report == "status_count") {
               paste("Request count · request/order date ·", dashboard_period_label())
             } else {
               paste("Request value by status · request/order date ·", currency, "·", dashboard_period_label())
             }) + chart_theme(fg)
      return(plot)
    }
    
    # Department and project spending must be assigned to the period in which payment occurred.
    d <- dashboard_paid_filtered()
    validate(need(nrow(d) > 0, "No paid requests with a payment date in this period and scope."))
    
    if (identical(report, "department_spend")) {
      g <- d |>
        group_by(label = coalesce(as.character(department), "Not specified")) |>
        summarise(total = sum(.data[[currency]], na.rm = TRUE), .groups = "drop") |>
        filter(total > 0) |>
        arrange(desc(total))
      validate(need(nrow(g) > 0, paste("No department paid spend in", currency, "for this period.")))
      g$share <- g$total / sum(g$total)
      g$pie_label <- paste0(abbr(g$total), "\n", round(100 * g$share), "%")
      pie_cols <- c("#0284c7", "#0369a1", "#075985", "#0ea5e9", "#38bdf8", "#7dd3fc")
      pie_values <- setNames(rep(pie_cols, length.out = nrow(g)), g$label)
      for (department in names(pie_values)) {
        key <- toupper(trimws(department))
        if (key %in% names(DEPT_PIE_COL)) pie_values[[department]] <- unname(DEPT_PIE_COL[[key]])
      }
      return(ggplot(g, aes(x = "", y = total, fill = label)) +
               geom_col(width = 1, colour = if (identical(input$mode, "dark")) "#18181b" else "#ffffff", linewidth = .8) +
               coord_polar(theta = "y") +
               geom_text(aes(label = pie_label), position = position_stack(vjust = .5),
                         colour = "white", fontface = "bold", size = 3.5) +
               scale_fill_manual(values = pie_values, name = "Department") +
               labs(subtitle = paste("Paid spend by department · payment date ·", currency, "·", dashboard_period_label()), x = NULL, y = NULL) +
               chart_theme(fg) + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                                       panel.grid = element_blank(), axis.title = element_blank(), legend.position = "right"))
    }
    
    g <- d |>
      group_by(label = coalesce(as.character(project_code), "Not specified")) |>
      summarise(total = sum(.data[[currency]], na.rm = TRUE), requests = n(), .groups = "drop") |>
      filter(total > 0) |>
      arrange(desc(total)) |>
      head(12)
    validate(need(nrow(g) > 0, paste("No project paid spend in", currency, "for this period.")))
    g$label <- factor(g$label, levels = rev(g$label))
    ggplot(g, aes(x = label, y = total)) +
      geom_col(fill = "#0284c7", width = .64) +
      geom_text(aes(label = paste0(abbr(total), " · ", requests, " req.")), hjust = -.15, colour = fg, size = 3.2) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .32))) +
      labs(subtitle = paste("Top projects by paid spend · payment date ·", currency, "·", dashboard_period_label()),
           x = NULL, y = NULL) + chart_theme(fg)
  }, bg = "transparent", res = 96)
  
  # Paid spending trend is bucketed by the selected reporting period and payment_date.
  output$plot_trend <- renderPlot({
    d <- dashboard_paid_filtered()
    currency <- active_currency()
    period_type <- v(input$dash_period_type)
    bounds <- dashboard_period_bounds()
    fg <- fg_col()
    validate(need(nrow(d) > 0, "No paid spending with a payment date in this period."))
    
    paid_dates <- suppressWarnings(as.Date(d$payment_date))
    if (period_type == "month") {
      agg <- data.frame(bucket = paid_dates, amount = as.numeric(d[[currency]])) |>
        group_by(bucket) |>
        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
      buckets <- seq(bounds$start, bounds$end, by = "day")
      date_breaks <- "1 week"
      date_labels <- "%d %b"
    } else {
      month_starts <- as.Date(paste0(format(paid_dates, "%Y-%m"), "-01"))
      agg <- data.frame(bucket = month_starts, amount = as.numeric(d[[currency]])) |>
        group_by(bucket) |>
        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
      buckets <- seq(as.Date(format(bounds$start, "%Y-%m-01")),
                     as.Date(format(bounds$end, "%Y-%m-01")), by = "month")
      date_breaks <- if (period_type == "quarter") "1 month" else "2 months"
      date_labels <- "%b"
    }
    g <- data.frame(bucket = buckets) |>
      left_join(agg, by = "bucket") |>
      arrange(bucket)
    g$total[is.na(g$total)] <- 0
    
    ggplot(g, aes(x = bucket, y = total)) +
      geom_line(colour = "#0284c7", linewidth = 1.15, lineend = "round") +
      geom_point(colour = "#0284c7", size = 2.2) +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                   limits = c(min(buckets), max(buckets))) +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .14))) +
      labs(subtitle = paste("Paid spend by payment date ·", dashboard_period_label(), "·", currency),
           x = NULL, y = NULL) + chart_theme(fg) +
      theme(axis.text.x = element_text(angle = if (period_type == "month") 0 else 0, hjust = .5))
  }, bg = "transparent", res = 96)
  
  # ---- comparison & days turnaround charts (strictly paid spend) ----
  # Scope for comparison charts respects currency, project, and department filters.
  # When Period is set to Month or Quarter, it filters to that month or quarter across years.
  compare_data_scope <- function() {
    d <- data()
    if (length(input$dash_project)) d <- d[d$project_code %in% input$dash_project, , drop = FALSE]
    if (length(input$dash_department)) d <- d[d$department %in% input$dash_department, , drop = FALSE]
    d <- d[d$status == "paid" & !is.na(d$payment_date) & nzchar(trimws(d$payment_date)), , drop = FALSE]
    d$plot_date <- suppressWarnings(as.Date(d$payment_date))
    d <- d[!is.na(d$plot_date), , drop = FALSE]
    d
  }

  share_dashboard_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    q <- list(
      v = "dash",
      cur = if (nzchar(active_currency())) active_currency() else NULL,
      period = if (nzchar(v(input$dash_period_type))) input$dash_period_type else NULL,
      yr = if (nzchar(v(input$dash_year))) input$dash_year else NULL,
      m = if (identical(v(input$dash_period_type), "month") && nzchar(v(input$dash_month))) input$dash_month else NULL,
      q = if (identical(v(input$dash_period_type), "quarter") && nzchar(v(input$dash_quarter))) input$dash_quarter else NULL,
      p = if (length(input$dash_project)) enc(input$dash_project) else NULL,
      d = if (length(input$dash_department)) enc(input$dash_department) else NULL,
      chart = if (nzchar(v(input$dash_chart_select))) input$dash_chart_select else NULL,
      bstat = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$breakdown_status))) input$breakdown_status else NULL,
      bdim = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$dim))) input$dim else NULL,
      bp = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$breakdown_project))) input$breakdown_project else NULL
    )
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname,
                   if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }

  observeEvent(input$share_dashboard, {
    showModal(modalDialog(
      title = "Share this view",
      p(class = "chart-note", "Anyone with the link can open this view with the same filters. No request data is embedded in the URL."),
      div(class = "input-group",
          tags$input(id = "share_url", class = "form-control", readonly = NA,
                     value = share_dashboard_url(), onclick = "this.select()"),
          tags$button(id = "copy_link", type = "button", class = "btn btn-primary", "Copy link")),
      footer = modalButton("Close"), easyClose = TRUE))
  })

  output$plot_year_compare <- renderPlot({
    currency <- active_currency()
    fg <- fg_col()

    df <- compare_data_scope()
    validate(need(nrow(df) > 0, paste("No paid data available in", currency, "for the selected filters.")))

    df$year <- format(df$plot_date, "%Y")
    df$month <- factor(format(df$plot_date, "%b"), levels = month.abb)
    df$amount <- as.numeric(df[[currency]])

    years <- sort(unique(df$year))
    validate(need(length(years) > 0, "No records with valid payment dates found."))

    target_months <- month.abb

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    current_m <- as.integer(format(Sys.Date(), "%m"))

    full_grid <- expand.grid(month = factor(target_months, levels = month.abb),
                             year = factor(years, levels = years))
    full_grid$m_num <- as.integer(full_grid$month)
    full_grid$yr_num <- as.integer(as.character(full_grid$year))
    # Exclude months that are in the future relative to today
    full_grid <- full_grid[!(full_grid$yr_num > current_year |
                             (full_grid$yr_num == current_year & full_grid$m_num > current_m)), ]
    full_grid$m_num <- NULL
    full_grid$yr_num <- NULL

    agg <- df |>
      group_by(month, year) |>
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

    g <- merge(full_grid, agg, by = c("month", "year"), all.x = TRUE)
    g$total[is.na(g$total)] <- 0

    yr_totals <- g |>
      group_by(year) |>
      summarise(yr_sum = sum(total, na.rm = TRUE), .groups = "drop")
    yr_summary <- paste(paste0(yr_totals$year, ": ", currency, " ", abbr(yr_totals$yr_sum)), collapse = "   |   ")

    pal_base <- c("#0284c7", "#f59e0b", "#10b981", "#8b5cf6", "#ec4899", "#06b6d4")
    if (length(years) <= length(pal_base)) {
      pal <- setNames(pal_base[seq_along(years)], years)
    } else {
      pal <- setNames(scales::hue_pal()(length(years)), years)
    }

    ggplot(g, aes(x = month, y = total, fill = year)) +
      geom_col(position = position_dodge(width = 0.78), width = 0.7) +
      geom_text(aes(label = ifelse(total > 0, abbr(total), "")),
                position = position_dodge(width = 0.78), vjust = -0.4, size = 3, colour = fg) +
      scale_fill_manual(values = pal, name = "Year") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, 0.16))) +
      labs(subtitle = paste0("Paid spend by month · Jan–Dec · ", yr_summary),
           x = NULL, y = NULL) +
      chart_theme(fg) +
      theme(legend.position = "top", legend.justification = "right",
            axis.text.x = element_text(face = "bold"),
            panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
            panel.grid.major.x = element_blank())
  }, bg = "transparent", res = 96)

  output$plot_quarter_compare <- renderPlot({
    currency <- active_currency()
    fg <- fg_col()

    df <- compare_data_scope()
    validate(need(nrow(df) > 0, paste("No paid data available in", currency, "for the selected filters.")))

    df$year <- format(df$plot_date, "%Y")
    q_nums <- ceiling(as.integer(format(df$plot_date, "%m")) / 3)
    q_labels <- c("Q1 (Jan–Mar)", "Q2 (Apr–Jun)", "Q3 (Jul–Sep)", "Q4 (Oct–Dec)")
    df$quarter <- factor(q_labels[q_nums], levels = q_labels)
    df$amount <- as.numeric(df[[currency]])

    years <- sort(unique(df$year))
    validate(need(length(years) > 0, "No records with valid payment dates found."))

    target_quarters <- q_labels

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    current_q <- ceiling(as.integer(format(Sys.Date(), "%m")) / 3)

    full_grid <- expand.grid(quarter = factor(target_quarters, levels = q_labels),
                             year = factor(years, levels = years))
    full_grid$q_num <- as.integer(full_grid$quarter)
    full_grid$yr_num <- as.integer(as.character(full_grid$year))
    # Exclude quarters that are in the future relative to today
    full_grid <- full_grid[!(full_grid$yr_num > current_year |
                             (full_grid$yr_num == current_year & full_grid$q_num > current_q)), ]
    full_grid$q_num <- NULL
    full_grid$yr_num <- NULL

    agg <- df |>
      group_by(quarter, year) |>
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

    g <- merge(full_grid, agg, by = c("quarter", "year"), all.x = TRUE)
    g$total[is.na(g$total)] <- 0

    yr_totals <- g |>
      group_by(year) |>
      summarise(yr_sum = sum(total, na.rm = TRUE), .groups = "drop")
    yr_summary <- paste(paste0(yr_totals$year, ": ", currency, " ", abbr(yr_totals$yr_sum)), collapse = "   |   ")

    pal_base <- c("#0284c7", "#f59e0b", "#10b981", "#8b5cf6", "#ec4899", "#06b6d4")
    if (length(years) <= length(pal_base)) {
      pal <- setNames(pal_base[seq_along(years)], years)
    } else {
      pal <- setNames(scales::hue_pal()(length(years)), years)
    }

    ggplot(g, aes(x = quarter, y = total, fill = year)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = ifelse(total > 0, abbr(total), "")),
                position = position_dodge(width = 0.75), vjust = -0.4, size = 3.3, colour = fg) +
      scale_fill_manual(values = pal, name = "Year") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, 0.16))) +
      labs(subtitle = paste0("Paid spend by quarter · payment date · ", yr_summary),
           x = NULL, y = NULL) +
      chart_theme(fg) +
      theme(legend.position = "top", legend.justification = "right",
            axis.text.x = element_text(face = "bold"),
            panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
            panel.grid.major.x = element_blank())
  }, bg = "transparent", res = 96)

  output$plot_days <- renderPlot({
    fg <- fg_col()
    # Fully filtered using the active reporting filters (Period, Project, Department)
    d <- dashboard_paid_filtered()
    validate(need(nrow(d) > 0, paste("No paid requests found for", dashboard_period_label(), "with the selected filters.")))

    d <- d[!is.na(d$payment_date) & !is.na(d$order_date) &
             nzchar(trimws(d$payment_date)) & nzchar(trimws(d$order_date)), , drop = FALSE]
    validate(need(nrow(d) > 0, "No paid requests with valid order and payment dates found in this reporting period."))

    d$order_dt <- suppressWarnings(as.Date(d$order_date))
    d$pay_dt <- suppressWarnings(as.Date(d$payment_date))
    d <- d[!is.na(d$order_dt) & !is.na(d$pay_dt), , drop = FALSE]
    validate(need(nrow(d) > 0, "No records with parseable order and payment dates found in this reporting period."))

    d$actual_days <- pmax(0, as.numeric(d$pay_dt - d$order_dt))
    d$usd_amt <- suppressWarnings(as.numeric(d$usd))
    d$scen <- mapply(scenario, ifelse(is.na(d$usd_amt), 0, d$usd_amt), d$waiver)

    scen_lead_days <- c(A = 7, B = 18, C = 21, D = 45, E = 60, G = 60)
    d$expected_days <- unname(scen_lead_days[d$scen])
    d$expected_days[is.na(d$expected_days)] <- 21

    d <- d[order(d$order_dt, d$id), , drop = FALSE]
    d$idx <- seq_len(nrow(d))
    d$is_ontime <- d$actual_days <= d$expected_days
    d$perf_status <- ifelse(d$is_ontime, "On-time (≤ Expected)", "Delayed (> Expected)")

    ontime_count <- sum(d$is_ontime)
    total_count <- nrow(d)
    ontime_pct <- round((ontime_count / total_count) * 100, 1)
    avg_actual <- round(mean(d$actual_days), 1)
    avg_expected <- round(mean(d$expected_days), 1)

    sub_txt <- paste0(
      dashboard_period_label(), " · ",
      ontime_count, "/", total_count, " on-time (", ontime_pct, "%) · ",
      "Avg actual: ", avg_actual, " days · Avg standard: ", avg_expected, " days"
    )

    ggplot(d) +
      geom_segment(aes(x = idx, xend = idx, y = expected_days, yend = actual_days),
                   linetype = "dotted", colour = scales::alpha(fg, 0.45), linewidth = 0.6) +
      geom_point(aes(x = idx, y = expected_days, shape = "Expected standard"),
                 colour = "#0284c7", size = 3) +
      geom_point(aes(x = idx, y = actual_days, colour = perf_status),
                 size = 3) +
      scale_colour_manual(
        name = "Actual turnaround",
        values = c("On-time (≤ Expected)" = "#10b981", "Delayed (> Expected)" = "#ef4444")
      ) +
      scale_shape_manual(
        name = "Scenario standard",
        values = c("Expected standard" = 18)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = min(12, total_count))) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      labs(
        subtitle = sub_txt,
        x = "Paid requests in period (chronological by order date)",
        y = "Days"
      ) +
      chart_theme(fg) +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "right",
        axis.title.x = element_text(size = 9, colour = scales::alpha(fg, 0.8), margin = margin(t = 8)),
        axis.title.y = element_text(size = 9, colour = scales::alpha(fg, 0.8), margin = margin(r = 8)),
        panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }, bg = "transparent", res = 96)
  
  # Paid breakdown follows payment date; all non-paid workflow stages follow request/order date.
  breakdown_filtered <- reactive({
    status_value <- v(input$breakdown_status)
    if (identical(status_value, "paid")) dashboard_paid_filtered() else dashboard_filtered()
  })
  
  # Aggregate one row per label for non-project views.
  totals_by <- function(key, status_value = input$breakdown_status %||% "paid") {
    if (is.null(status_value) || !status_value %in% STATUS) status_value <- "paid"
    breakdown_filtered() |>
      filter(.data$status == status_value) |>
      group_by(label = coalesce(as.character(.data[[key]]), "(none)")) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }
  
  # When Group by = Project, show only non-zero project totals in the overview.
  # After a project is selected, switch to its non-zero budget-line amounts only;
  # do not repeat the project total in the budget-line view.
  project_breakdown <- function(status_value = input$breakdown_status %||% "paid") {
    scope <- breakdown_filtered()
    if (is.null(status_value) || !status_value %in% STATUS) status_value <- "paid"
    selected_currency <- active_currency()
    
    breakdown_project <- v(input$breakdown_project)
    if (nzchar(breakdown_project)) {
      projects <- breakdown_project
    } else if (length(input$dash_project)) {
      projects <- unique(as.character(input$dash_project))
    } else {
      projects <- project_codes_db()
    }
    projects <- projects[!is.na(projects) & nzchar(projects)]
    if (!length(projects)) return(data.frame())
    
    selected <- scope |>
      filter(.data$status == status_value, .data$project_code %in% projects)
    
    totals <- selected |>
      group_by(project_code) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    totals <- data.frame(project_code = projects, stringsAsFactors = FALSE) |>
      left_join(totals, by = "project_code")
    if (!"requests" %in% names(totals)) totals$requests <- 0L
    totals$requests[is.na(totals$requests)] <- 0L
    for (cc in CUR) {
      if (!cc %in% names(totals)) totals[[cc]] <- 0
      totals[[cc]][is.na(totals[[cc]])] <- 0
    }
    total_rows <- totals |>
      mutate(row_type = "Project total", budget_line = "",
             plot_label = project_code, row_order = 0L)
    
    # Without a selected project, show only projects with a non-zero value in the
    # selected currency/status/filter scope (matching the compact project overview).
    if (!nzchar(breakdown_project)) {
      return(total_rows |>
               filter(abs(.data[[selected_currency]]) > 0) |>
               arrange(desc(.data[[selected_currency]]), project_code) |>
               mutate(plot_label = factor(plot_label, levels = rev(unique(plot_label)))))
    }
    
    # When a project is selected, show only its configured budget lines with a
    # non-zero value. Never add a project-total row to this budget-line view.
    catalogue <- budget_catalog_db(projects)
    selected_budget_ids <- suppressWarnings(as.integer(input$f_budget_line %||% character()))
    selected_budget_ids <- selected_budget_ids[!is.na(selected_budget_ids)]
    if (length(selected_budget_ids)) {
      catalogue <- catalogue[catalogue$id %in% selected_budget_ids, , drop = FALSE]
    }
    
    line_spend <- selected |>
      mutate(budget_line = coalesce(as.character(budget_line), "")) |>
      group_by(project_code, budget_line) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    lines <- catalogue |>
      transmute(project_code = as.character(project_code),
                budget_line = as.character(budget_line_code)) |>
      distinct() |>
      left_join(line_spend, by = c("project_code", "budget_line"))
    if (!"requests" %in% names(lines)) lines$requests <- 0L
    lines$requests[is.na(lines$requests)] <- 0L
    for (cc in CUR) {
      if (!cc %in% names(lines)) lines[[cc]] <- 0
      lines[[cc]][is.na(lines[[cc]])] <- 0
    }
    
    lines |>
      filter(abs(.data[[selected_currency]]) > 0) |>
      mutate(row_type = "Budget line",
             plot_label = paste0(project_code, " · ", budget_line),
             row_order = 1L) |>
      arrange(desc(.data[[selected_currency]]), budget_line) |>
      mutate(plot_label = factor(plot_label, levels = rev(unique(plot_label))))
  }
  
  output$plot_dim <- renderPlot({
    status_value <- input$breakdown_status %||% "paid"
    group_by_value <- input$dim %||% "project_code"
    currency <- active_currency()
    fg <- fg_col()
    
    # Project overview shows only non-zero project totals. Selecting a project
    # switches the chart to that project's non-zero budget lines, without a total row.
    if (identical(group_by_value, "project_code")) {
      g <- project_breakdown(status_value)
      empty_msg <- if (nzchar(v(input$breakdown_project))) {
        paste("No non-zero budget lines for", v(input$breakdown_project),
              "with", tolower(status_label(status_value)), "in", currency)
      } else {
        paste("No non-zero project amounts for", tolower(status_label(status_value)),
              "in", currency)
      }
      validate(need(nrow(g) > 0, empty_msg))
      g$total <- as.numeric(g[[currency]])
      g$total[is.na(g$total)] <- 0
      g$plot_label <- factor(as.character(g$plot_label), levels = levels(g$plot_label))
      # Use the same status color for project totals and budget-line bars. The
      # department pie chart is the intentional exception and uses department colors.
      status_color <- unname(STATUS_COL[[status_value]])
      return(
        ggplot(g, aes(x = plot_label, y = total)) +
          geom_col(fill = status_color, width = .68) +
          geom_text(aes(label = abbr(total)), hjust = -.15, colour = fg, size = 3.2) +
          coord_flip(clip = "off") +
          scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .3))) +
          labs(subtitle = if (nzchar(v(input$breakdown_project))) {
            paste("Non-zero budget lines for", v(input$breakdown_project),
                  "—", tolower(status_label(status_value)), "in", currency)
          } else {
            paste("Projects with non-zero", tolower(status_label(status_value)),
                  "amounts in", currency, "· select a project to see its budget lines")
          }, x = NULL, y = NULL) +
          chart_theme(fg) +
          theme(legend.position = "bottom", axis.text.y = element_text(size = 8.5),
                plot.margin = margin(8, 28, 8, 8))
      )
    }
    
    g <- totals_by(group_by_value, status_value)
    validate(need(nrow(g) > 0, paste("No", status_label(status_value), "requests in this view")))
    g$total <- as.numeric(g[[currency]])
    g$total[is.na(g$total)] <- 0
    
    # Department remains a pie chart with stable department colors.
    if (identical(group_by_value, "department")) {
      g <- g |> filter(total > 0) |> arrange(desc(total))
      validate(need(nrow(g) > 0, paste("No positive", tolower(status_label(status_value)), "spend by department in", currency)))
      g$share <- g$total / sum(g$total)
      g$label <- ifelse(is.na(g$label) | !nzchar(g$label), "(none)", as.character(g$label))
      g$pie_label <- paste0(abbr(g$total), "\n", round(g$share * 100), "%")
      pie_cols <- c("#0284c7", "#0369a1", "#075985", "#0ea5e9", "#38bdf8", "#7dd3fc")
      pie_values <- setNames(rep(pie_cols, length.out = nrow(g)), g$label)
      for (department in names(pie_values)) {
        key <- toupper(trimws(department))
        if (key %in% names(DEPT_PIE_COL)) pie_values[[department]] <- unname(DEPT_PIE_COL[[key]])
      }
      return(
        ggplot(g, aes(x = "", y = total, fill = label)) +
          geom_col(width = 1, colour = if (identical(input$mode, "dark")) "#18181b" else "#ffffff",
                   linewidth = .8) +
          coord_polar(theta = "y") +
          geom_text(aes(label = pie_label), position = position_stack(vjust = .5),
                    colour = "white", fontface = "bold", size = 3.6, lineheight = .95) +
          scale_fill_manual(values = pie_values, name = "Department") +
          labs(subtitle = paste("Department share of", tolower(status_label(status_value)),
                                "spend in", currency), x = NULL, y = NULL) +
          chart_theme(fg) +
          theme(axis.text = element_blank(), axis.ticks = element_blank(),
                panel.grid = element_blank(), axis.title = element_blank(),
                legend.position = "right", plot.margin = margin(8, 12, 8, 8))
      )
    }
    
    g <- head(arrange(g, desc(total)), 10)
    g$label <- factor(g$label, levels = rev(g$label))
    ggplot(g, aes(x = label, y = total)) +
      geom_col(fill = STATUS_COL[[status_value]], width = .62) +
      geom_text(aes(label = abbr(total)), hjust = -.15, colour = fg, size = 3.4) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .3))) +
      labs(subtitle = paste("Top 10", tolower(status_label(status_value)), "requests in", currency),
           x = NULL, y = NULL) + chart_theme(fg)
  }, bg = "transparent", res = 96,
  height = function() {
    if (identical(isolate(input$dim) %||% "project_code", "project_code")) {
      rows <- tryCatch(nrow(project_breakdown(isolate(input$breakdown_status) %||% "paid")), error = function(e) 0L)
      max(380, rows * 28 + 130)
    } else 380
  })
  
  output$by_dim <- renderDT({
    status_value <- input$breakdown_status %||% "paid"
    group_by_value <- input$dim %||% "project_code"
    if (identical(group_by_value, "project_code")) {
      g <- project_breakdown(status_value)
      if (!nrow(g)) return(datatable(data.frame(), options = list(dom = "t", language = dt_language)))
      display <- g |>
        mutate(`Row type` = row_type) |>
        select(`Row type`, project_code, budget_line, requests, all_of(CUR))
      names(display) <- c("Type", "Project", "Budget line", "Requests", CUR)
      datatable(display, rownames = FALSE, selection = "none", fillContainer = FALSE,
                options = list(dom = "t", language = dt_language, scrollX = TRUE)) |>
        formatRound(CUR, 2)
    } else {
      lab <- switch(group_by_value,
                    requested_by = "Requester", department = "Department", "Group")
      datatable(totals_by(group_by_value, status_value), rownames = FALSE, selection = "none",
                fillContainer = FALSE, colnames = c(lab, "Requests", CUR),
                options = list(dom = "t", language = dt_language, scrollX = TRUE)) |>
        formatRound(CUR, 2)
    }
  })
  
  # ---- request register: search + adjacent status filter + row-level Open action ----
  output$table_requests <- renderDT({
    d <- filtered()
    selected_status <- request_table_status()
    if (nzchar(selected_status) && selected_status %in% STATUS) {
      d <- d[d$status == selected_status, , drop = FALSE]
    }
    
    display <- data.frame(
      id = d$id,
      description = ifelse(is.na(d$description), "", as.character(d$description)),
      project_code = ifelse(is.na(d$project_code), "", as.character(d$project_code)),
      budget_line = ifelse(is.na(d$budget_line), "", as.character(d$budget_line)),
      requested_by = ifelse(is.na(d$requested_by), "", as.character(d$requested_by)),
      amount = ifelse(is.na(d$amount), "", paste0(d$currency, " ", vapply(d$amount, money, character(1)))),
      open = rep("Open", nrow(d)),
      check.names = FALSE
    )
    names(display) <- c("id", "Description", "Project code", "Budget line", "Requested by", "Amount", "Open")
    
    datatable(display, rownames = FALSE, selection = "none", filter = "none",
              fillContainer = FALSE, escape = TRUE,
              options = list(
                scrollX = TRUE, autoWidth = TRUE, pageLength = 8,
                dom = "<'dt-top'f>rt<'dt-foot'ip>",
                language = dt_language, initComplete = request_table_init(request_table_status()),
                columnDefs = list(
                  list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE),
                  list(targets = 1, width = "32%", render = more_render("Description")),
                  list(targets = 6, width = "90px", orderable = FALSE, searchable = FALSE,
                       className = "dt-body-center",
                       render = JS(r"---(
function(data, type, row) {
  if (type !== 'display') return data;
  return '<button type="button" class="btn btn-sm btn-primary open-row" data-id="' +
         String(row[0]).replace(/"/g, '&quot;') + '">Open</button>';
}
)---")),
list(targets = c(2, 3, 4, 5), className = "dt-body-left")
                )
              ))
  }, server = TRUE)
  
  output$export <- downloadHandler(
    filename = "expenses.xlsx",
    content  = function(file) {
      d <- filtered()
      selected_status <- request_table_status()
      if (nzchar(selected_status) && selected_status %in% STATUS)
        d <- d[d$status == selected_status, , drop = FALSE]
      write_xlsx(select(d, -any_of(c("week", "month", "budget_line_id"))), file)
    }
  )

  # =============================================================================
  # Financial Update Server Logic
  # =============================================================================
  fin_active_batch <- reactiveVal(NULL)

  observe({
    projs <- project_codes_db()
    updateSelectizeInput(session, "fin_project_filter",
                         choices = setNames(projs, projs),
                         selected = isolate(input$fin_project_filter))
  })

  # Pending requests are paid expenses that have not been deducted yet
  fin_pending_data <- reactive({
    d <- data()
    d <- d[d$status == "paid" & (is.na(d$deducted_at) | !nzchar(trimws(as.character(d$deducted_at)))), , drop = FALSE]
    if (length(input$fin_project_filter)) {
      d <- d[d$project_code %in% input$fin_project_filter, , drop = FALSE]
    }
    d
  })

  fin_project_summary <- reactive({
    projs <- project_catalog_view()
    if (length(input$fin_project_filter)) {
      projs <- projs[projs$project_code %in% input$fin_project_filter, , drop = FALSE]
    }
    pend <- fin_pending_data()
    cur <- active_currency()

    if (nrow(pend) > 0) {
      agg <- pend |>
        group_by(project_code) |>
        summarise(
          pending_total = sum(.data[[cur]], na.rm = TRUE),
          pending_count = n(),
          .groups = "drop"
        )
    } else {
      agg <- data.frame(project_code = character(), pending_total = numeric(), pending_count = integer())
    }

    res <- merge(projs, agg, by = "project_code", all.x = TRUE)
    res$pending_total[is.na(res$pending_total)] <- 0
    res$pending_count[is.na(res$pending_count)] <- 0L

    res$last_seen <- ifelse(
      is.na(res$last_deducted_at) | !nzchar(trimws(res$last_deducted_at)),
      "Never confirmed",
      format(suppressWarnings(as.POSIXct(res$last_deducted_at)), "%Y-%m-%d %H:%M")
    )
    res[order(-res$pending_total, res$project_code), , drop = FALSE]
  })

  output$fin_metrics <- renderUI({
    pend <- fin_pending_data()
    cur <- active_currency()

    total_pend <- if (nrow(pend) > 0) sum(pend[[cur]], na.rm = TRUE) else 0
    total_reqs <- nrow(pend)
    active_projs <- length(unique(pend$project_code[pend[[cur]] > 0]))
    batch <- fin_active_batch()

    batch_status_text <- if (!is.null(batch)) {
      paste0(batch$batch_id, " · ", length(batch$request_ids), " items")
    } else {
      "No active batch"
    }

    metric <- function(label, value, note, extra_class = "") {
      div(class = paste("metric-card", extra_class),
          p(class = "metric-label", label),
          p(class = "metric-value", value),
          p(class = "metric-note", note)
      )
    }

    div(class = "metrics-grid dashboard-metrics-grid",
        metric("Pending deduction", paste(cur, abbr(total_pend)), "Current non-deducted spend", "metric-paid"),
        metric("Requests to deduct", total_reqs, "Paid requests awaiting budget deduction", "metric-waiting"),
        metric("Active projects", active_projs, "Projects with un-deducted spend", "metric-validation"),
        metric("Batch status", batch_status_text, if (!is.null(batch)) "Ready to export / confirm" else "Click Batch lines to group", "")
    )
  })

  output$fin_batch_alert <- renderUI({
    batch <- fin_active_batch()
    if (is.null(batch)) return(NULL)
    cur <- active_currency()
    pend <- fin_pending_data()
    b_pend <- pend[pend$id %in% batch$request_ids, , drop = FALSE]
    b_tot <- if (nrow(b_pend) > 0) sum(b_pend[[cur]], na.rm = TRUE) else 0

    div(class = "alert alert-primary d-flex align-items-center justify-content-between py-2 mb-3",
        div(class = "d-flex align-items-center gap-2",
            icon("layer-group"),
            tags$span(tags$b("Active Batch: "), batch$batch_id, " · ", length(batch$request_ids), " request lines (", cur, " ", money(b_tot), ") across ", length(batch$project_codes), " project(s).")
        ),
        actionButton("fin_btn_clear_batch", "Dismiss batch", class = "btn btn-sm btn-outline-primary py-0")
    )
  })

  observeEvent(input$fin_btn_clear_batch, {
    fin_active_batch(NULL)
    showNotification("Batch dismissed.", type = "message")
  })

  observeEvent(input$fin_btn_batch, {
    pend <- fin_pending_data()
    if (nrow(pend) == 0) {
      showNotification("No pending requests available to batch.", type = "warning")
      return()
    }
    batch_id <- paste0("BATCH-", format(Sys.Date(), "%Y%m%d-"), sprintf("%04d", sample.int(9999, 1)))
    req_ids <- pend$id
    projs <- unique(pend$project_code)
    fin_active_batch(list(
      batch_id = batch_id,
      request_ids = req_ids,
      project_codes = projs,
      created = Sys.time()
    ))
    showNotification(paste0("Batch ", batch_id, " created with ", length(req_ids), " request lines. Export or Confirm update is ready."), type = "message")
  })

  output$fin_projects_table <- renderDT({
    summary_df <- fin_project_summary()
    cur <- active_currency()

    display_df <- data.frame(
      project_code = summary_df$project_code,
      department = summary_df$department,
      location = summary_df$location,
      current_total = paste(cur, vapply(summary_df$pending_total, money, character(1))),
      request_count = summary_df$pending_count,
      last_seen = summary_df$last_seen,
      stringsAsFactors = FALSE
    )
    names(display_df) <- c("Project code", "Department", "Location", paste0("Current period total (", cur, ")"), "Pending requests", "Last seen (Confirmation)")

    datatable(display_df, rownames = FALSE, selection = "none", filter = "none",
              options = list(
                pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
                language = dt_language,
                columnDefs = list(list(targets = c(3, 4), className = "dt-body-right"))
              ))
  })

  output$fin_lines_count <- renderUI({
    pend <- fin_pending_data()
    span(class = "text-muted small", paste(nrow(pend), "requests awaiting budget deduction"))
  })

  output$fin_details_table <- renderDT({
    pend <- fin_pending_data()
    cur <- active_currency()

    if (nrow(pend) == 0) {
      empty_df <- data.frame(Message = "No pending request lines awaiting deduction.")
      return(datatable(empty_df, rownames = FALSE, selection = "none", options = list(dom = "t", language = dt_language)))
    }

    display_df <- data.frame(
      id = pend$id,
      description = ifelse(is.na(pend$description), "", as.character(pend$description)),
      project_code = ifelse(is.na(pend$project_code), "", as.character(pend$project_code)),
      budget_line = ifelse(is.na(pend$budget_line), "", as.character(pend$budget_line)),
      po = ifelse(is.na(pend$po) | !nzchar(pend$po), "—", as.character(pend$po)),
      supplier = ifelse(is.na(pend$supplier), "", as.character(pend$supplier)),
      amount = paste(pend$currency, vapply(pend$amount, money, character(1))),
      cur_amount = paste(cur, vapply(pend[[cur]], money, character(1))),
      payment_date = ifelse(is.na(pend$payment_date), "", as.character(pend$payment_date)),
      stringsAsFactors = FALSE
    )
    names(display_df) <- c("ID", "Description", "Project", "Budget line", "PO", "Supplier", "Original amount", paste0("Value in ", cur), "Payment date")

    datatable(display_df, rownames = FALSE, selection = "none", filter = "none",
              options = list(
                pageLength = 8, searching = TRUE, scrollX = TRUE, dom = "ftip",
                language = dt_language,
                columnDefs = list(
                  list(targets = 1, width = "28%", render = more_render("Description")),
                  list(targets = c(6, 7), className = "dt-body-right")
                )
              ))
  })

  output$fin_btn_export <- downloadHandler(
    filename = function() {
      paste0("financial_batch_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      cur <- active_currency()
      batch <- fin_active_batch()
      pend <- fin_pending_data()
      if (!is.null(batch)) {
        pend <- pend[pend$id %in% batch$request_ids, , drop = FALSE]
      }

      proj_summary <- fin_project_summary()
      if (!is.null(batch)) {
        proj_summary <- proj_summary[proj_summary$project_code %in% batch$project_codes, , drop = FALSE]
      }

      sheet_summary <- data.frame(
        `Project Code` = proj_summary$project_code,
        Department = proj_summary$department,
        Location = proj_summary$location,
        `Pending Amount` = proj_summary$pending_total,
        Currency = rep(cur, nrow(proj_summary)),
        `Request Count` = proj_summary$pending_count,
        `Last Confirmed` = proj_summary$last_seen,
        `Batch Code` = rep(if (!is.null(batch)) batch$batch_id else "Unbatched", nrow(proj_summary)),
        check.names = FALSE
      )

      sheet_lines <- data.frame(
        `Request ID` = pend$id,
        Description = pend$description,
        `Project Code` = pend$project_code,
        `Budget Line` = pend$budget_line,
        PO = pend$po,
        `Requested By` = pend$requested_by,
        Supplier = pend$supplier,
        `Original Amount` = pend$amount,
        `Original Currency` = pend$currency,
        `Amount in Display Currency` = pend[[cur]],
        `Display Currency` = cur,
        `Payment Date` = pend$payment_date,
        check.names = FALSE
      )

      write_xlsx(list("Project_Summary" = sheet_summary, "Request_Lines" = sheet_lines), file)
    }
  )

  observeEvent(input$fin_btn_confirm, {
    batch <- fin_active_batch()
    d <- fin_pending_data()
    if (!is.null(batch)) {
      d <- d[d$id %in% batch$request_ids, , drop = FALSE]
    }
    if (nrow(d) == 0) {
      showNotification("No pending requests available to confirm.", type = "warning")
      return()
    }
    cur <- active_currency()
    tot <- sum(d[[cur]], na.rm = TRUE)
    projs <- unique(d$project_code)

    showModal(modalDialog(
      title = tagList(icon("check-double"), " Confirm Budget Deduction"),
      div(
        p("Confirm that the following project expenditures have been deducted from the actual budget outside this application:"),
        tags$ul(
          tags$li(tags$b("Projects: "), paste(projs, collapse = ", ")),
          tags$li(tags$b("Total Requests: "), nrow(d)),
          tags$li(tags$b("Total Amount: "), paste(cur, money(tot))),
          tags$li(tags$b("Batch: "), if (!is.null(batch)) batch$batch_id else "Current unbatched selection")
        ),
        div(class = "alert alert-warning py-2 mb-0",
            icon("triangle-exclamation"),
            " Once confirmed, current-period totals for these projects will reset to zero, and their 'Last seen' confirmation timestamps will be updated."
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("fin_btn_confirm_proceed", "Confirm deduction", class = "btn-success text-white")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$fin_btn_confirm_proceed, {
    removeModal()
    batch <- fin_active_batch()
    d <- fin_pending_data()
    if (!is.null(batch)) {
      d <- d[d$id %in% batch$request_ids, , drop = FALSE]
    }
    if (nrow(d) == 0) return()

    stamp <- now()
    req_ids <- d$id
    projs <- unique(d$project_code)

    cn <- con(); on.exit(dbDisconnect(cn))
    id_placeholders <- paste(rep("?", length(req_ids)), collapse = ",")
    dbExecute(cn, paste0("UPDATE expenses SET deducted_at = ? WHERE id IN (", id_placeholders, ")"),
              params = c(list(stamp), as.list(as.integer(req_ids))))

    proj_placeholders <- paste(rep("?", length(projs)), collapse = ",")
    dbExecute(cn, paste0("UPDATE projects SET last_deducted_at = ? WHERE project_code IN (", proj_placeholders, ")"),
              params = c(list(stamp), as.list(projs)))

    batch_code <- if (!is.null(batch)) batch$batch_id else paste0("BATCH-", format(Sys.Date(), "%Y%m%d-"), sprintf("%04d", sample.int(9999, 1)))
    cur <- active_currency()
    tot <- sum(d[[cur]], na.rm = TRUE)

    dbExecute(cn, "INSERT INTO financial_batches(batch_code, project_codes, request_ids, total_amount, currency, status, created, confirmed_at) VALUES(?,?,?,?,?,?,?,?)",
              params = list(batch_code, paste(projs, collapse = "|"), paste(req_ids, collapse = ","), tot, cur, "confirmed", stamp, stamp))

    fin_active_batch(NULL)
    showNotification(paste0("Success: ", length(req_ids), " requests confirmed and deducted! Current totals for project(s) ", paste(projs, collapse = ", "), " reset to zero."), type = "message")
  })
}

shinyApp(ui, server)
