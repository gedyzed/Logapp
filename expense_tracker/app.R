library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(writexl)
library(ggplot2)

options(scipen = 999)          # never show scientific notation

# Use one persistent SQLite database for this application. No demo database fallback.
script_file <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else {
    frames <- sys.frames()
    ofiles <- vapply(frames, function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles)) ofiles[[length(ofiles)]] else ""
  }
})
APP_DIR <- if (nzchar(script_file) && file.exists(script_file)) dirname(normalizePath(script_file, mustWork = TRUE)) else normalizePath(getwd(), mustWork = FALSE)
DB <- normalizePath(file.path(APP_DIR, "tracker.db"), mustWork = FALSE)
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
}
init_db()

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
cache_choices <- function(d, id) uniq(unlist(d[CACHE[[id]]], use.names = FALSE))

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
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
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
.table-head{display:flex;align-items:center;justify-content:space-between;gap:.8rem;flex-wrap:wrap}
.table-head .buttons{display:flex;align-items:center;gap:.5rem;flex-wrap:wrap}
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

"

js <- "
function setRangePlaceholders(){
  $('#f_dates-start').attr('placeholder','Start date');
  $('#f_dates-end').attr('placeholder','End date');
}
$(document).on('shiny:connected', setRangePlaceholders);
$(document).on('shown.bs.dropdown', '.filter-dd', setRangePlaceholders);
$(document).on('shiny:connected', function() {
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
  selected_json <- as.character(jsonlite::toJSON(selected_status, auto_unbox = TRUE))
  JS(sprintf(r"---(
function(settings, json) {
  var table = this.api().table().node();
  var wrapper = $(this.api().table().container());
  var top = wrapper.find('.dt-top');
  var control = $('<div class="dt-status-control"><label for="request-status-filter">Status</label><select id="request-status-filter" class="form-select form-select-sm"><option value="">All statuses</option><option value="waiting">Waiting</option><option value="under validation">Under validation</option><option value="paid">Paid</option><option value="rejected">Rejected</option></select></div>');
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
    
    nav_panel(
      title = "Requests", value = "requests",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("Requests"), p("Track procurement requests and understand spending over time.")),
              actionButton("new_request", tagList(icon("plus"), "New request"), class = "btn-primary")),
          
          div(class = "toolbar",
              div(class = "dropdown filter-dd",
                  tags$button(class = "btn btn-outline-secondary", type = "button",
                              `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside",
                              `aria-expanded` = "false", icon("filter"), " Filters",
                              span(id = "fcount", class = "badge rounded-pill text-bg-primary ms-2",
                                   style = "display:none")),
                  div(class = "dropdown-menu p-3 fmenu",
                      div(class = "fgrid",
                          selectInput("f_month", "Month",
                                      choices = c("All months" = "", month_choices(format(Sys.Date(), "%Y-%m"))),
                                      selected = format(Sys.Date(), "%Y-%m"), width = "100%"),
                          selectizeInput("f_week", "Week", NULL, multiple = TRUE,
                                         options = list(placeholder = "All weeks")),
                          selectizeInput("f_project", "Project code", NULL, multiple = TRUE,
                                         options = list(placeholder = "All projects")),
                          selectizeInput("f_budget_line", "Budget line", NULL, multiple = TRUE,
                                         options = list(placeholder = "Select project code(s) first")),
                          selectizeInput("f_person", "Requested by", NULL, multiple = TRUE,
                                         options = list(placeholder = "All requesters")),
                          selectizeInput("f_submitter", "Submitted by", NULL, multiple = TRUE,
                                         options = list(placeholder = "All focal points")),
                          div(class = "wide", dateRangeInput(
                            "f_dates", "Date range (overrides month and week)", start = NULL, end = NULL,
                            separator = " to ", format = "dd-mm-yyyy", width = "100%")),
                          div(class = "wide d-flex justify-content-end gap-2 mt-1",
                              actionButton("reset", "Clear filters", class = "btn-outline-secondary btn-sm"))))),
              div(class = "ms-auto d-flex align-items-center gap-2 flex-wrap",
                  selectInput("cur", NULL, CUR, selected = DEFAULTS$currency, width = "110px"),
                  actionButton("share", "Share view", icon = icon("link"), class = "btn-outline-secondary"))),
          
          uiOutput("totals"),
          
          card(class = "mt-3", card_header(div(class = "card-head",
                                               span("Request breakdown"),
                                               div(class = "d-flex align-items-end flex-wrap gap-2",
                                                   selectInput("breakdown_status", "Status",
                                                               choices = c("Paid" = "paid", "Waiting" = "waiting",
                                                                           "Under validation" = "under validation", "Rejected" = "rejected"),
                                                               selected = "paid", width = "170px"),
                                                   selectInput("dim", "Group by", DIMS, selected = "project_code", width = "180px"),
                                                   conditionalPanel(
                                                     condition = "input.dim === 'project_code'",
                                                     div(class = "d-flex align-items-end gap-1",
                                                         selectInput("breakdown_project", "Project code",
                                                                     choices = c("Select a project" = ""),
                                                                     selected = "", width = "220px"),
                                                         actionButton("clear_breakdown_project", "Clear",
                                                                      class = "btn-outline-secondary btn-sm mb-3")))))),
               navset_tab(
                 nav_panel("Chart", plotOutput("plot_dim", height = "300px")),
                 nav_panel("Table", DTOutput("by_dim", fill = FALSE)))),
          
          card(class = "chart-card mt-3", card_header("Monthly request amounts by status"),
               div(class = "card-body",
                   p(class = "chart-note", "Compare paid, waiting and under-validation request values month by month."),
                   plotOutput("plot_trend", height = "330px"))),
          
          
          
          card(class = "mt-3", fill = FALSE,
               card_header(div(class = "table-head",
                               div(span("Request register"), p(class = "chart-note mb-0", "Use Open on any row to review or update its details.")),
                               div(class = "buttons",
                                   downloadButton("export", "Export", class = "btn-outline-secondary")))),
               DTOutput("table_requests", fill = FALSE))
      )
    ),
    # Full-page new request; the navbar link is hidden and navigation is controlled by buttons.
    nav_panel(
      title = "New request", value = "new",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("New request"),
                  p("Enter a procurement request. Fields without an asterisk are optional.")),
              actionButton("cancel_new", tagList(icon("arrow-left"), "Back to requests"),
                           class = "btn-outline-secondary")),
          layout_columns(
            fill = FALSE, col_widths = c(8, 4),
            card(fill = FALSE,
                 div(class = "card-body",
                     uiOutput("new_form_ui"),
                     div(class = "action-bar",
                         actionButton("clear", "Clear form", class = "btn-outline-secondary"),
                         actionButton("cancel_new_bottom", "Cancel", class = "btn-outline-secondary"),
                         actionButton("add", "Add request", class = "btn-primary")))),
            card(card_header("Request preview"),
                 div(class = "card-body", uiOutput("preview")))
          ))
    ),
    nav_spacer(),
    nav_item(actionButton("settings_open", NULL, icon = icon("gear"),
                          class = "btn-outline-secondary", title = "Settings", `aria-label` = "Settings")),
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
  
  # Clear Shiny's default date-range value on first render so the default current-month
  # filter is active. A deliberately selected date range still overrides month/week.
  session$onFlushed(function() {
    suppressWarnings(updateDateRangeInput(session, "f_dates", start = NA, end = NA))
  }, once = TRUE)
  
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
    d$week          <- format(as.Date(d$order_date), "%G-W%V")
    d$month         <- format(as.Date(d$order_date), "%Y-%m")
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
    
    breakdown_projects <- project_codes_db()
    if (length(keep("f_project"))) {
      breakdown_projects <- intersect(breakdown_projects, keep("f_project"))
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
  
  observeEvent(input$clear_breakdown_project, {
    # Clear the breakdown selection and any inherited dashboard project filters,
    # so they do not silently re-apply a default project/budget-line selection.
    updateSelectInput(session, "breakdown_project", selected = "")
    updateSelectizeInput(session, "f_project", selected = character(0))
    updateSelectizeInput(session, "f_budget_line", selected = character(0))
  }, ignoreInit = TRUE)
  
  # ---- filters: month + week work together; any date range overrides both ----
  date_bound <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]]))))
      return(as.Date(NA))
    suppressWarnings(as.Date(as.character(x[[1]])))
  }
  base <- reactive({
    d <- data()
    date_range <- input$f_dates
    from <- if (length(date_range) >= 1) date_bound(date_range[[1]]) else as.Date(NA)
    to <- if (length(date_range) >= 2) date_bound(date_range[[2]]) else as.Date(NA)
    date_range_active <- !is.na(from) || !is.na(to)
    
    if (date_range_active) {
      if (!is.na(from)) d <- filter(d, !is.na(order_date), as.Date(order_date) >= from)
      if (!is.na(to)) d <- filter(d, !is.na(order_date), as.Date(order_date) <= to)
    } else {
      if (nzchar(v(input$f_month))) d <- filter(d, month == input$f_month)
      if (length(input$f_week)) d <- filter(d, week %in% input$f_week)
    }
    if (length(input$f_project)) d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_budget_line)) d <- filter(d, budget_line_id %in% as.integer(input$f_budget_line))
    if (length(input$f_person)) d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_submitter)) d <- filter(d, submitted_by %in% input$f_submitter)
    d
  })
  filtered <- reactive(base())
  
  observeEvent(input$reset, {
    # Clear means no active filters (All months), unlike the fresh-session default,
    # which intentionally starts on the current month.
    updateSelectInput(session, "f_month", selected = "")
    updateSelectizeInput(session, "f_week", selected = character(0))
    for (id in c("f_project", "f_budget_line", "f_person", "f_submitter"))
      updateSelectizeInput(session, id, selected = character(0))
    suppressWarnings(updateDateRangeInput(session, "f_dates", start = NA, end = NA))
    # Also clear the project selector within Request breakdown so no secondary
    # selection continues to narrow the breakdown after dashboard filters reset.
    updateSelectInput(session, "breakdown_project", selected = "")
  }, ignoreInit = TRUE)
  
  # Count only the effective date filter: a date range overrides the month and week.
  observe({
    dr <- input$f_dates
    has_from <- length(dr) >= 1 && !is.na(date_bound(dr[[1]]))
    has_to <- length(dr) >= 2 && !is.na(date_bound(dr[[2]]))
    has_range <- has_from || has_to
    n <- sum(if (has_range) c(has_from, has_to) else c(nzchar(v(input$f_month)), length(input$f_week) > 0),
             length(input$f_project) > 0, length(input$f_budget_line) > 0,
             length(input$f_person) > 0, length(input$f_submitter) > 0)
    session$sendCustomMessage("badge", list(n = n))
  })
  
  # Short, shareable URL containing only active filters. Dates exclude month/week
  # from the link because the date range takes precedence over them.
  share_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    dr <- input$f_dates
    from <- if (length(dr) >= 1) date_bound(dr[[1]]) else as.Date(NA)
    to <- if (length(dr) >= 2) date_bound(dr[[2]]) else as.Date(NA)
    date_active <- !is.na(from) || !is.na(to)
    q <- list(
      v = "r2",
      m = if (!date_active && nzchar(v(input$f_month))) input$f_month else NULL,
      f = if (!is.na(from)) as.character(from) else NULL,
      t = if (!is.na(to)) as.character(to) else NULL,
      w = if (!date_active && length(input$f_week)) enc(input$f_week) else NULL,
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
  
  # ---- Settings: project/budget-line catalog and defaults persisted in SQLite ----
  observeEvent(input$settings_open, {
    defaults <- read_app_defaults()
    codes <- project_codes_db()
    d <- isolate(data())
    submitters <- uniq(c(d$submitted_by, d$requested_by, defaults$submitted_by))
    countries <- uniq(c(d$country, defaults$country))
    showModal(modalDialog(
      title = tagList(icon("gear"), " Settings"),
      size = "l", easyClose = TRUE,
      p(class = "form-intro", "Project and budget-line additions are saved to the database immediately. Request defaults are saved when you select Save defaults."),
      navset_tab(
        nav_panel("Projects & budget lines", value = "catalog",
                  div(class = "nr pt-2",
                      div(class = "form-section-title", "Add a project code"),
                      div(class = "fg",
                          col(4, textInput("cfg_project_code", "Project code *", "", width = "100%",
                                           placeholder = "e.g. EDUAMH2026")),
                          col(4, selectInput("cfg_project_department", "Department", c("Not specified" = "", DEPTS), selected = "", width = "100%")),
                          col(4, textInput("cfg_project_location", "Default location", "", width = "100%")),
                          col(12, p(class = "form-hint", "Example project code: EDUAMH2026. Letters and numbers are examples only; no format is enforced.")),
                          col(12, actionButton("cfg_add_project", tagList(icon("plus"), "Add project"), class = "btn-primary"))
                      ),
                      div(class = "form-section-rule"),
                      div(class = "form-section-title", "Add a budget line to a project"),
                      div(class = "fg",
                          col(6, selectInput("cfg_budget_project", "Project code *",
                                             choices = c("Select project" = "", setNames(codes, codes)), selected = "", width = "100%")),
                          col(6, textInput("cfg_budget_line", "Budget line *", "", width = "100%",
                                           placeholder = "e.g. TRAVEL01")),
                          col(12, p(class = "form-hint", "Example budget-line code: TRAVEL01. This is guidance only; descriptive names and other formats are allowed.")),
                          col(12, actionButton("cfg_add_budget_line", tagList(icon("plus"), "Add budget line"), class = "btn-primary"))
                      ),
                      div(class = "form-section-rule"),
                      div(class = "form-section-title", "Configured projects"),
                      DTOutput("settings_catalog_table")
                  )
        ),
        nav_panel("Request defaults", value = "defaults",
                  div(class = "nr pt-2",
                      p(class = "form-intro", "These values prefill the New request form. They do not overwrite existing requests."),
                      div(class = "fg",
                          col(6, selectizeInput("cfg_default_submitter", "Logistics focal point", choices = with_sel(submitters, defaults$submitted_by),
                                                selected = defaults$submitted_by, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a name"))),
                          col(6, selectizeInput("cfg_default_country", "Default country", choices = with_sel(countries, defaults$country),
                                                selected = defaults$country, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a country"))),
                          col(6, selectInput("cfg_default_currency", "Default currency", choices = CUR,
                                             selected = defaults$currency, width = "100%"))
                      ),
                      div(class = "action-bar",
                          actionButton("cfg_save_defaults", tagList(icon("save"), "Save defaults"), class = "btn-primary"))
                  )
        )
      ),
      footer = tagList(modalButton("Cancel"), actionButton("settings_done", "Done", class = "btn-outline-secondary"))
    ))
  })
  
  output$settings_catalog_table <- renderDT({
    invisible(data()) # reacts to changes from other sessions as well
    catalog <- db_get("SELECT p.project_code, COALESCE(p.department,'') AS department,
      COALESCE(p.location,'') AS location,
      COALESCE(GROUP_CONCAT(bl.budget_line_code, ', '), '') AS budget_lines
      FROM projects p LEFT JOIN budget_lines bl ON bl.project_id = p.id
      GROUP BY p.id ORDER BY p.project_code COLLATE NOCASE")
    datatable(catalog, rownames = FALSE, selection = "none", filter = "none",
              options = list(pageLength = 6, lengthChange = FALSE, searching = TRUE,
                             scrollX = TRUE, dom = "ftip", language = dt_language),
              colnames = c("Project code", "Department", "Location", "Budget lines"))
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
    ensure_project_record(cn, code, v(input$cfg_project_department), v(input$cfg_project_location))
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
  })
  
  observeEvent(input$settings_done, removeModal())
  
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q) || is.null(q[["v"]]) || !(q[["v"]] %in% c("r", "r2"))) return()
    d <- isolate(data())
    vals <- function(k) if (is.null(q[[k]]) || !nzchar(q[[k]])) character()
    else strsplit(q[[k]], "|", fixed = TRUE)[[1]]
    as_d <- function(x) tryCatch(as.Date(x), error = function(e) NA)
    from <- if (!is.null(q[["f"]])) as_d(q[["f"]]) else as.Date(NA)
    to <- if (!is.null(q[["t"]])) as_d(q[["t"]]) else as.Date(NA)
    if (!is.na(from) || !is.na(to))
      suppressWarnings(updateDateRangeInput(session, "f_dates",
                                            start = if (is.na(from)) NA else from, end = if (is.na(to)) NA else to))
    if (!is.null(q[["m"]]) && !is.na(q[["m"]])) {
      updateSelectInput(session, "f_month", selected = q[["m"]])
    } else if (is.na(from) && is.na(to)) {
      # A shared view without an explicit month/date range represents an unfiltered
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
    if (!is.na(from) || !is.na(to)) {
      # A date range takes priority over month/week on links as well as on the UI.
      updateSelectInput(session, "f_month", selected = "")
      updateSelectizeInput(session, "f_week", selected = character(0))
    } else {
      set("f_week", "w", week_choices(d, if (is.null(q[["m"]])) "" else q[["m"]]))
    }
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
            col(4, cached("supplier", "Supplier")),
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
        p("Created when status moves to under validation", class = "ref muted"))
  })
  
  # back to the starting values (defaults prefilled, the rest empty)
  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    DEFAULTS <<- read_app_defaults()
    d <- isolate(data())
    for (id in names(CACHE)) {
      val <- DEFAULTS[[id]] %||% ""
      ch  <- union(cache_choices(d, id), val[nzchar(val)])
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
  observeEvent(input$open_request_row, {
    id <- suppressWarnings(as.integer(input$open_request_row$id))
    if (length(id) != 1L || is.na(id)) {
      showNotification("Could not identify that request. Refresh the table and try again.", type = "warning")
      return()
    }
    r <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(r)) {
      showNotification("That request is no longer available. Refresh the table and try again.", type = "warning")
      return()
    }
    r <- r[1, , drop = FALSE]
    
    # References are stored on the expense row. Show them in the request details
    # modal as well as in exports. Older rows may not have a saved reference yet,
    # so build a safe preview from the same reference rules where possible.
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
        "Generated when the request moves to Under validation."
      }
    }
    
    edit_id(id)
    showModal(modalDialog(
      title = paste0("Request #", r$id), size = "l", easyClose = TRUE,
      div(class = "form-intro", "Fields marked * are required. Country, quantity, unit, framework agreement, order date, submitted by and remarks are optional."),
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
              col(4, selectInput("m_status", "Status *", STATUS, selected = v(r$status), width = "100%")),
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
                  "The OF and PO references are stored with the request and included in exports. A PO is created when the request enters Under validation."
                }))
      ),
      footer = tagList(modalButton("Cancel"), actionButton("m_save", "Save changes", class = "btn-primary"))
    ))
  })
  
  observeEvent(input$m_project, {
    pc <- v(input$m_project)
    lines <- if (nzchar(pc)) budget_catalog_db(pc)$budget_line_code else character()
    updateSelectizeInput(session, "m_budget", choices = setNames(lines, lines), selected = character(0),
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
    saved <- tryCatch({ update_expense_row(cn, id, row); TRUE }, error = function(e) {
      showNotification(paste("Request was not updated:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved) return()
    removeModal()
    status_notice <- switch(input$m_status,
                            waiting = list(text = "Request status set to Waiting.", type = "default"),
                            `under validation` = list(text = "Request moved to Under validation.", type = "warning"),
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
  
  # ---- dashboard summary ----
  output$totals <- renderUI({
    d <- filtered(); k <- input$cur %||% "ETB"
    total <- sum(d[[k]], na.rm = TRUE)
    paid <- sum(d[[k]][d$status == "paid"], na.rm = TRUE)
    waiting <- sum(d[[k]][d$status == "waiting"], na.rm = TRUE)
    validation <- sum(d[[k]][d$status == "under validation"], na.rm = TRUE)
    metric <- function(label, value, note) div(class = "metric-card",
                                               p(class = "metric-label", label),
                                               p(class = "metric-value", paste(k, abbr(value))),
                                               p(class = "metric-note", note))
    rejected <- sum(d[[k]][d$status == "rejected"], na.rm = TRUE)
    div(class = "metrics-grid",
        metric("Total requested", total, paste(nrow(d), "requests in the selected view")),
        metric("Paid spend", paid, paste(sum(d$status == "paid", na.rm = TRUE), "paid requests")),
        metric("Waiting", waiting, paste(sum(d$status == "waiting", na.rm = TRUE), "requests awaiting review")),
        metric("Under validation", validation, paste(sum(d$status == "under validation", na.rm = TRUE), "requests in validation")),
        metric("Rejected", rejected, paste(sum(d$status == "rejected", na.rm = TRUE), "rejected requests")))
  })
  
  
  # Status palette is consistent across charts, notifications and status changes.
  fg_col <- function() if (identical(input$mode, "dark")) "#e4e4e7" else "#27272a"
  status_label <- function(status) switch(status,
                                          waiting = "Waiting",
                                          `under validation` = "Under validation",
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
  
  # One monthly stacked chart compares paid, waiting and under-validation amounts.
  output$plot_trend <- renderPlot({
    chart_statuses <- c("waiting", "under validation", "paid")
    d <- filtered() |> filter(status %in% chart_statuses, !is.na(month))
    k <- input$cur %||% "ETB"
    fg <- fg_col()
    validate(need(nrow(d) > 0, "No paid, waiting or under-validation requests in this view"))
    
    g <- d |>
      group_by(month, status) |>
      summarise(total = sum(.data[[k]], na.rm = TRUE), .groups = "drop")
    month_levels <- tail(sort(unique(g$month)), 12)
    g <- filter(g, month %in% month_levels)
    g$month <- factor(g$month, levels = month_levels)
    g$status <- factor(g$status, levels = chart_statuses)
    label_data <- g |> group_by(month) |> summarise(total = sum(total), .groups = "drop")
    
    ggplot(g, aes(x = month, y = total, fill = status)) +
      geom_col(width = .68, position = "stack") +
      geom_text(data = label_data, aes(x = month, y = total, label = abbr(total)),
                inherit.aes = FALSE, vjust = -.45, colour = fg, size = 3.5) +
      scale_fill_manual(values = STATUS_COL[chart_statuses],
                        breaks = chart_statuses,
                        labels = c("Waiting", "Under validation", "Paid"),
                        drop = FALSE) +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .16))) +
      labs(subtitle = paste("Monthly request values in", k), x = NULL, y = NULL) +
      chart_theme(fg)
  }, bg = "transparent", res = 96)
  
  # Request breakdown respects dashboard filters. Project grouping shows non-zero
  # project totals until a project code is selected, then shows its non-zero budget lines.
  breakdown_filtered <- reactive(filtered())
  
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
    selected_currency <- v(input$cur)
    if (!selected_currency %in% CUR) selected_currency <- "ETB"
    
    breakdown_project <- v(input$breakdown_project)
    if (nzchar(breakdown_project)) {
      projects <- breakdown_project
    } else if (length(input$f_project)) {
      projects <- unique(as.character(input$f_project))
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
    currency <- input$cur %||% "ETB"
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
    selected_status <- v(input$table_status_filter)
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
                language = dt_language, initComplete = request_table_init(selected_status),
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
      selected_status <- v(input$table_status_filter)
      if (nzchar(selected_status) && selected_status %in% STATUS)
        d <- d[d$status == selected_status, , drop = FALSE]
      write_xlsx(select(d, -any_of(c("week", "month", "budget_line_id"))), file)
    }
  )
}

shinyApp(ui, server)