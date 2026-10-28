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
          st <- as.character(legacy_value(i, "status", "under_review"))
          st <- switch(st,
                       waiting = "under_review",
                       "under validation" = "pending",
                       rejected = "cancelled",
                       st)
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

