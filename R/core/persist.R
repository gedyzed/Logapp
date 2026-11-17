# Request, catalogue, and batch writes against the ERD tables.
# The screen still reads project_code, budget_line, order_form, and po.
# Those values are filled from project_budgets, budget_lines, and the reference columns.

new_uuid <- function() {
  hex <- c(0:9, letters[1:6])
  chunk <- function(n) paste(sample(hex, n, replace = TRUE), collapse = "")
  paste(chunk(8), chunk(4), paste0("4", chunk(3)),
        paste0(sample(c("8", "9", "a", "b"), 1), chunk(3)), chunk(12), sep = "-")
}

db_bool <- function(flag) {
  if (db_is_postgres()) isTRUE(flag) else as.integer(isTRUE(flag))
}

bump_revision <- function(cn) {
  dbExecute(cn, "UPDATE settings SET data_revision = data_revision + 1, updated_at = ? WHERE id = 1",
            params = list(db_stamp()))
}

reference_areas <- function() {
  data.frame(
    code = c("ADD", "DIR", "AFR", "AMH", "BNG", "CET", "GAM", "HAR", "ORO", "SID", "SOM", "SET", "SWE", "TIG",
             "SEM", "BDR", "ASO", "HOS", "ADA", "HAW", "JIJ", "WOS", "BON", "MEK"),
    name = c("Addis Ababa", "Dire Dawa", "Afar", "Amhara", "Benishangul-Gumuz", "Central Ethiopia",
             "Gambela", "Harari", "Oromia", "Sidama", "Somali", "South Ethiopia",
             "South West Ethiopia Peoples", "Tigray",
             "Semera", "Bahir Dar", "Asosa", "Hosaena", "Adama", "Hawassa", "Jijiga",
             "Wolaita Sodo", "Bonga", "Mekelle"),
    stringsAsFactors = FALSE
  )
}

seed_reference_data <- function(cn) {
  stamp <- db_stamp()
  departments <- c(REACH = "REACH", PANDA = "PANDA", ACTED = "ACTED")
  for (code in names(departments)) {
    dbExecute(cn, "INSERT INTO departments(code, name, active) VALUES(?,?,?)
                   ON CONFLICT(code) DO NOTHING",
              params = list(code, unname(departments[[code]]), db_bool(TRUE)))
  }
  currency_names <- c(ETB = "Ethiopian Birr", USD = "US Dollar", EUR = "Euro", GBP = "British Pound")
  for (code in names(currency_names)) {
    dbExecute(cn, "INSERT INTO currencies(code, name, active) VALUES(?,?,?)
                   ON CONFLICT(code) DO NOTHING",
              params = list(code, unname(currency_names[[code]]), db_bool(TRUE)))
  }
  areas <- reference_areas()
  for (i in seq_len(nrow(areas))) {
    dbExecute(cn, "INSERT INTO areas(id, code, name, country_code, active, created_at, updated_at)
                   VALUES(?,?,?,?,?,?,?)
                   ON CONFLICT(code) DO NOTHING",
              params = list(new_uuid(), areas$code[[i]], areas$name[[i]], "ETH",
                            db_bool(TRUE), stamp, stamp))
  }
  exists <- dbGetQuery(cn, "SELECT id FROM settings WHERE id = 1")
  if (!nrow(exists)) {
    dbExecute(cn, "INSERT INTO settings(id, business_timezone, weekly_cutoff_local, default_currency,
                 default_country, default_submitted_by, notification_catchup_hours, data_revision, updated_at)
                 VALUES(1, 'Africa/Addis_Ababa', '14:00:00', 'ETB', 'ETH', 'Betty T', 72, 0, ?)",
              params = list(stamp))
  }
  month <- format(Sys.Date(), "%Y-%m")
  have <- dbGetQuery(cn, "SELECT id FROM fx_rate_months WHERE rate_month = ?", params = list(month))
  if (!nrow(have)) {
    eur_per_usd <- unname(RATES[["EUR"]])
    if (is.na(eur_per_usd) || eur_per_usd <= 0) eur_per_usd <- 0.92
    dbExecute(cn, "INSERT INTO fx_rate_months(id, rate_month, etb_per_eur, usd_per_eur, gbp_per_eur, source, fetched_at)
                   VALUES(?,?,?,?,?,?,?)",
              params = list(new_uuid(), month,
                            unname(RATES[["ETB"]]) / eur_per_usd,
                            unname(RATES[["USD"]]) / eur_per_usd,
                            unname(RATES[["GBP"]]) / eur_per_usd,
                            "seed", stamp))
  }
  invisible(TRUE)
}

install_local_schema <- function(cn) {
  if ("requests" %in% dbListTables(cn)) return(invisible(FALSE))
  schema <- file.path(configured_app_dir, "sql", "sqlite_schema.sql")
  apply_sql_file(cn, schema)
  seed_reference_data(cn)
  invisible(TRUE)
}

init_db <- function() {
  cn <- con()
  on.exit(db_release(cn), add = TRUE)
  if (db_is_postgres()) {
    tables <- dbListTables(cn)
    if (!"schema_migrations" %in% tables) {
      stop("Neon has no schema yet. Run scripts/migrate.R before starting the app.", call. = FALSE)
    }
    applied <- dbGetQuery(cn, "SELECT version FROM schema_migrations")
    missing <- setdiff(1:5, applied$version)
    if (length(missing)) {
      stop("Neon is missing migrations: ", paste(missing, collapse = ", "),
           ". Run scripts/migrate.R.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  install_local_schema(cn)
  invisible(TRUE)
}

db_get <- function(sql, ...) {
  cn <- con()
  on.exit(db_release(cn), add = TRUE)
  a <- list(...)
  if (length(a)) dbGetQuery(cn, sql, params = a) else dbGetQuery(cn, sql)
}

read_app_defaults <- function() {
  x <- tryCatch(db_get("SELECT default_currency, default_country, default_submitted_by FROM settings WHERE id = 1"),
                error = function(e) NULL)
  if (is.null(x) || !nrow(x)) {
    return(list(submitted_by = "Betty T", country = "ETH", currency = "ETB"))
  }
  list(submitted_by = as.character(x$default_submitted_by[[1]]),
       country = as.character(x$default_country[[1]]),
       currency = as.character(x$default_currency[[1]]))
}

project_codes_db <- function() {
  z <- db_get("SELECT code AS project_code FROM project_budgets WHERE active ORDER BY lower(code)")
  as.character(z$project_code)
}

areas_catalog_db <- function() {
  db_get("SELECT code, name FROM areas WHERE active ORDER BY lower(code)")
}

people_names_db <- function() {
  z <- db_get("SELECT full_name FROM people WHERE active ORDER BY lower(full_name)")
  uniq(as.character(z$full_name))
}

# Department and active lines for one project. Not the full budget-line catalogue.
project_form_db <- function(project_code) {
  code <- trimws(as.character(project_code %||% ""))
  empty <- list(department = "", lines = character())
  if (!nzchar(code)) return(empty)
  rows <- db_get("SELECT p.department_code AS department, bl.code AS budget_line_code
                  FROM project_budgets p
                  LEFT JOIN budget_lines bl ON bl.project_budget_id = p.id AND bl.active
                  WHERE lower(p.code) = lower(?)
                  ORDER BY lower(bl.code)", code)
  if (!nrow(rows)) return(empty)
  department <- as.character(rows$department[[1]])
  if (!length(department) || is.na(department[[1]])) department <- "" else department <- department[[1]]
  lines <- as.character(rows$budget_line_code)
  lines <- unique(lines[!is.na(lines) & nzchar(lines)])
  list(department = department, lines = lines)
}

budget_catalog_db <- function(project_codes = character()) {
  z <- db_get("SELECT bl.id, p.code AS project_code, bl.code AS budget_line_code
               FROM budget_lines bl JOIN project_budgets p ON p.id = bl.project_budget_id
               WHERE bl.active ORDER BY lower(p.code), lower(bl.code)")
  if (length(project_codes)) {
    selected_codes <- tolower(trimws(as.character(project_codes)))
    z <- z[tolower(z$project_code) %in% selected_codes, , drop = FALSE]
  }
  z
}

supplier_catalog_db <- function(active_only = TRUE) {
  sql <- if (active_only) "SELECT supplier_name FROM suppliers WHERE active ORDER BY lower(supplier_name)"
  else "SELECT supplier_name FROM suppliers ORDER BY lower(supplier_name)"
  z <- db_get(sql)
  as.character(z$supplier_name)
}

ensure_department <- function(cn, code) {
  code <- toupper(trimws(as.character(code %||% "")))
  if (!nzchar(code)) code <- "ACTED"
  hit <- dbGetQuery(cn, "SELECT code FROM departments WHERE code = ?", params = list(code))
  if (!nrow(hit)) {
    dbExecute(cn, "INSERT INTO departments(code, name, active) VALUES(?,?,?)",
              params = list(code, code, db_bool(TRUE)))
  }
  code
}

ensure_project_record <- function(cn, project_code, department = "", location = "") {
  code <- toupper(trimws(as.character(project_code %||% "")))
  if (!nzchar(code)) return(NA_character_)
  dep <- ensure_department(cn, if (nzchar(trimws(as.character(department %||% "")))) department else "ACTED")
  stamp <- db_stamp()
  hit <- dbGetQuery(cn, "SELECT id FROM project_budgets WHERE code = ?", params = list(code))
  if (nrow(hit)) return(as.character(hit$id[[1]]))
  id <- new_uuid()
  dbExecute(cn, "INSERT INTO project_budgets(id, code, name, department_code, active, created_at, updated_at)
                 VALUES(?,?,?,?,?,?,?)",
            params = list(id, code, code, dep, db_bool(TRUE), stamp, stamp))
  id
}

require_project_record <- function(cn, project_code) {
  code <- toupper(trimws(as.character(project_code %||% "")))
  if (!nzchar(code)) stop("Choose a configured project code and budget line.")
  hit <- dbGetQuery(cn, "SELECT id FROM project_budgets WHERE code = ?", params = list(code))
  if (!nrow(hit)) stop("Choose a configured project code and budget line.")
  as.character(hit$id[[1]])
}

require_budget_line_record <- function(cn, project_id, budget_line_code) {
  code <- toupper(trimws(as.character(budget_line_code %||% "")))
  project_id <- as.character(project_id %||% "")
  if (!nzchar(project_id) || !nzchar(code)) stop("Choose a configured project code and budget line.")
  hit <- dbGetQuery(cn, "SELECT id FROM budget_lines WHERE project_budget_id = ? AND code = ?",
                    params = list(project_id, code))
  if (!nrow(hit)) stop("Choose a configured project code and budget line.")
  as.character(hit$id[[1]])
}

ensure_budget_line_record <- function(cn, project_id, budget_line_code) {
  code <- toupper(trimws(as.character(budget_line_code %||% "")))
  project_id <- as.character(project_id %||% "")
  if (!nzchar(project_id) || !nzchar(code)) return(NA_character_)
  hit <- dbGetQuery(cn, "SELECT id FROM budget_lines WHERE project_budget_id = ? AND code = ?",
                    params = list(project_id, code))
  if (nrow(hit)) return(as.character(hit$id[[1]]))
  id <- new_uuid()
  stamp <- db_stamp()
  dbExecute(cn, "INSERT INTO budget_lines(id, project_budget_id, code, description, active, created_at, updated_at)
                 VALUES(?,?,?,?,?,?,?)",
            params = list(id, project_id, code, code, db_bool(TRUE), stamp, stamp))
  id
}

ensure_area <- function(cn, area_text, country = "ETH") {
  raw <- trimws(as.character(area_text %||% ""))
  if (!nzchar(raw)) raw <- "ADD"
  code <- toupper(gsub("[^A-Za-z0-9]", "", raw))
  if (!nzchar(code)) code <- "ADD"
  code <- substr(code, 1, 40)
  hit <- dbGetQuery(cn, "SELECT id, code FROM areas WHERE code = ? OR lower(name) = lower(?)",
                    params = list(code, raw))
  if (nrow(hit)) return(as.character(hit$id[[1]]))
  id <- new_uuid()
  stamp <- db_stamp()
  country <- toupper(substr(trimws(as.character(country %||% "ETH")), 1, 3))
  if (!nzchar(country)) country <- "ETH"
  dbExecute(cn, "INSERT INTO areas(id, code, name, country_code, active, created_at, updated_at)
                 VALUES(?,?,?,?,?,?,?)",
            params = list(id, code, raw, country, db_bool(TRUE), stamp, stamp))
  id
}

ensure_person <- function(cn, full_name, department = "") {
  name <- trimws(as.character(full_name %||% ""))
  if (!nzchar(name)) stop("Choose a person.")
  hit <- dbGetQuery(cn, "SELECT id FROM people WHERE lower(full_name) = lower(?) AND active
                          ORDER BY created_at LIMIT 1",
                    params = list(name))
  if (nrow(hit)) return(as.character(hit$id[[1]]))
  dep <- trimws(as.character(department %||% ""))
  dep <- if (nzchar(dep)) ensure_department(cn, dep) else NA_character_
  id <- new_uuid()
  stamp <- db_stamp()
  dbExecute(cn, "INSERT INTO people(id, full_name, email, department_code, active, created_at, updated_at)
                 VALUES(?,?,?,?,?,?,?)",
            params = list(id, name, NA_character_, dep, db_bool(TRUE), stamp, stamp))
  id
}

ensure_supplier <- function(cn, supplier_name) {
  name <- trimws(as.character(supplier_name %||% ""))
  if (!nzchar(name)) return(NA_character_)
  hit <- dbGetQuery(cn, "SELECT id FROM suppliers WHERE lower(supplier_name) = lower(?)",
                    params = list(name))
  if (nrow(hit)) return(as.character(hit$id[[1]]))
  id <- new_uuid()
  stamp <- db_stamp()
  dbExecute(cn, "INSERT INTO suppliers(id, supplier_name, active, created_at, updated_at)
                 VALUES(?,?,?,?,?)",
            params = list(id, name, db_bool(TRUE), stamp, stamp))
  id
}

fx_month_row <- function(cn, month) {
  dbGetQuery(cn, "SELECT id, etb_per_eur, usd_per_eur, gbp_per_eur FROM fx_rate_months WHERE rate_month = ?",
             params = list(month))
}

convert_payment <- function(amount, currency, rates) {
  per_eur <- c(ETB = rates$etb_per_eur[[1]], USD = rates$usd_per_eur[[1]],
               EUR = 1, GBP = rates$gbp_per_eur[[1]])
  code <- toupper(currency)
  if (!code %in% names(per_eur) || is.na(per_eur[[code]]) || per_eur[[code]] <= 0) {
    stop("No exchange rate for ", code, ".")
  }
  eur <- amount / unname(per_eur[[code]])
  list(etb = eur * unname(per_eur[["ETB"]]), usd = eur * unname(per_eur[["USD"]]),
       eur = eur, gbp = eur * unname(per_eur[["GBP"]]))
}

round_half_away <- function(x, digits = 2) {
  scale <- 10^digits
  sign(x) * trunc(abs(x) * scale + 0.5 + 1e-10) / scale
}

next_reference <- function(cn, base, kind = c("request", "po")) {
  kind <- match.arg(kind)
  col <- if (kind == "request") "request_reference_base" else "po_reference_base"
  seq_col <- if (kind == "request") "request_reference_sequence" else "po_reference_sequence"
  row <- dbGetQuery(cn, sprintf("SELECT MAX(%s) AS n FROM requests WHERE %s = ?", seq_col, col),
                    params = list(base))
  n <- suppressWarnings(as.integer(row$n[[1]]))
  if (is.na(n)) 1L else n + 1L
}

issue_reference <- function(base, sequence) {
  if (sequence <= 1L) base else paste0(base, "/", sprintf("%03d", sequence))
}

record_state_event <- function(cn, request_id, from_status, to_status, occurred_at, actual_days = NA_integer_, delay_days = NA_integer_) {
  prev <- dbGetQuery(cn, "SELECT COALESCE(MAX(event_number), 0) AS n FROM request_state_events WHERE request_id = ?",
                     params = list(request_id))
  delayed <- if (is.na(delay_days)) NA_integer_ else as.integer(delay_days > 0)
  actual_value <- if (is.na(actual_days)) NA_integer_ else as.integer(actual_days)
  delay_value <- if (is.na(delay_days)) NA_integer_ else as.integer(delay_days)
  from_value <- if (is.na(from_status) || !nzchar(from_status)) NA_character_ else from_status
  common <- list(new_uuid(), request_id, as.integer(prev$n[[1]]) + 1L, new_uuid(),
                 from_value, to_status, occurred_at, actual_value, delay_value)
  if (db_is_postgres()) {
    dbExecute(cn, "INSERT INTO request_state_events(id, request_id, event_number, operation_key, from_status, to_status,
                   occurred_at, actual_days, delay_days)
                   VALUES(?,?,?,?,?,?,?,?,?)",
              params = common)
  } else {
    dbExecute(cn, "INSERT INTO request_state_events(id, request_id, event_number, operation_key, from_status, to_status,
                   occurred_at, actual_days, delay_days, is_delayed)
                   VALUES(?,?,?,?,?,?,?,?,?,?)",
              params = c(common, list(delayed)))
  }
}

load_request_rows <- function(cn) {
  rows <- dbGetQuery(cn, paste(
    "SELECT r.id, r.description, pb.code AS project_code, bl.code AS budget_line, bl.id AS budget_line_id,",
    "a.country_code AS country, a.code AS location, r.department_code AS department,",
    "r.po_reason AS reason, r.request_reference AS order_form, r.po_reference AS po,",
    "req.full_name AS requested_by, sub.full_name AS submitted_by, r.status,",
    "r.waiver_memo AS waiver, sup.supplier_name AS supplier, r.unit, r.quantity, r.amount, r.currency,",
    "pe.usd_value AS usd, pe.gbp_value, pe.etb_value, pe.eur_value,",
    "r.order_date, r.po_date, pe.payment_date, r.remarks, NULL AS rate_used,",
    "r.registered_at AS created, r.updated_at AS updated, r.scenario, r.expected_days,",
    "CASE WHEN fb.status = 'applied' AND bi.released_at IS NULL THEN fb.applied_at ELSE NULL END AS deducted_at",
    "FROM requests r",
    "JOIN budget_lines bl ON bl.id = r.budget_line_id",
    "JOIN project_budgets pb ON pb.id = bl.project_budget_id",
    "JOIN areas a ON a.id = r.area_id",
    "JOIN people req ON req.id = r.requester_id",
    "JOIN people sub ON sub.id = r.submitted_by_id",
    "LEFT JOIN suppliers sup ON sup.id = r.supplier_id",
    "LEFT JOIN payment_events pe ON pe.request_id = r.id",
    "LEFT JOIN batch_items bi ON bi.payment_event_id = pe.id AND bi.released_at IS NULL",
    "LEFT JOIN financial_batches fb ON fb.id = bi.batch_id",
    "WHERE r.deleted_at IS NULL",
    "ORDER BY r.registered_at DESC"))
  attr(rows, "database_revision") <- as.character(dbGetQuery(cn, "SELECT data_revision FROM settings WHERE id = 1")[[1]][[1]])
  rows
}

insert_expense_row <- function(cn, row) {
  create_request_record(cn, row)
}

create_request_record <- function(cn, row) {
  g <- function(name, default = "") {
    value <- row[[name]]
    if (is.null(value) || !length(value) || is.na(value[[1]])) default else as.character(value[[1]])
  }
  project_id <- require_project_record(cn, g("project_code"))
  line_id <- require_budget_line_record(cn, project_id, g("budget_line"))
  area_id <- ensure_area(cn, if (nzchar(g("location"))) g("location") else g("area"), g("country", "ETH"))
  area <- dbGetQuery(cn, "SELECT code, country_code FROM areas WHERE id = ?", params = list(area_id))
  dep <- ensure_department(cn, g("department", "ACTED"))
  requester <- ensure_person(cn, g("requested_by"), dep)
  submitted <- ensure_person(cn, if (nzchar(g("submitted_by"))) g("submitted_by") else DEFAULTS$submitted_by, dep)
  supplier_id <- if (nzchar(g("supplier"))) ensure_supplier(cn, g("supplier")) else NA_character_
  amount <- suppressWarnings(as.numeric(row$amount[[1]]))
  if (is.na(amount) || amount <= 0) stop("Amount must be positive.")
  currency <- toupper(g("currency", "ETB"))
  order_date <- substr(g("order_date"), 1, 10)
  rates <- fx_month_row(cn, substr(order_date, 1, 7))
  if (!nrow(rates)) rates <- fx_month_row(cn, format(Sys.Date(), "%Y-%m"))
  if (!nrow(rates)) stop("No exchange rate is stored for this month.")
  values <- convert_payment(amount, currency, rates)
  waiver <- g("waiver")
  scenario_code <- scenario(values$usd, waiver)
  expected <- SCENARIO_DAYS[[scenario_code]]
  if (is.null(expected) || is.na(expected)) expected <- 7L
  base <- paste0("OF/", toupper(g("project_code")), "/", toupper(g("budget_line")), "/",
                 area$country_code[[1]], "/", area$code[[1]], "/", dep, "/",
                 format(as.Date(order_date), "%d-%m-%Y"))
  sequence <- next_reference(cn, base, "request")
  reference <- issue_reference(base, sequence)
  status <- g("status", "under_review")
  if (!status %in% c("under_review", "pending", "paid", "cancelled")) status <- "under_review"
  stamp <- if (nzchar(g("created"))) g("created") else db_stamp()
  id <- new_uuid()
  quantity <- suppressWarnings(as.numeric(row$quantity[[1]]))
  if (!length(quantity) || is.na(quantity)) quantity <- NA_real_
  request_values <- list(id, new_uuid(), base, sequence, reference, line_id, area_id, dep, requester, submitted,
                         supplier_id, g("description"), quantity, if (nzchar(g("unit"))) g("unit") else NA_character_,
                         amount, currency, scenario_code, as.integer(expected),
                         if (nzchar(waiver)) waiver else NA_character_,
                         if (nzchar(g("remarks"))) g("remarks") else NA_character_,
                         order_date, if (nzchar(g("reason"))) g("reason") else NA_character_,
                         if (nzchar(g("po_date"))) substr(g("po_date"), 1, 10) else NA_character_,
                         if (nzchar(g("po"))) g("po") else NA_character_,
                         status, stamp, stamp, stamp)
  if (db_is_postgres()) {
    dbExecute(cn, "INSERT INTO requests(id, submission_key, request_reference_base, request_reference_sequence,
               request_reference, budget_line_id, area_id, department_code, requester_id, submitted_by_id, supplier_id,
               description, quantity, unit, amount, currency, scenario, expected_days, waiver_memo, remarks,
               order_date, po_reason, po_date, po_reference, status, registered_at, status_changed_at, updated_at)
               VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
              params = request_values)
  } else {
    dbExecute(cn, "INSERT INTO requests(id, submission_key, request_reference_base, request_reference_sequence,
               request_reference, budget_line_id, area_id, department_code, requester_id, submitted_by_id, supplier_id,
               description, quantity, unit, amount, currency, scenario, expected_days, waiver_memo, remarks,
               order_date, po_reason, po_date, po_reference, status, registered_at, status_changed_at, updated_at, due_date)
               VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
              params = c(request_values, list(NA_character_)))
  }
  record_state_event(cn, id, NA_character_, status, stamp)
  if (identical(status, "paid") && nzchar(g("payment_date"))) {
    record_payment(cn, id, substr(g("payment_date"), 1, 10), stamp)
  }
  if (nzchar(g("deducted_at"))) {
    mark_seed_deduction(cn, id, g("deducted_at"))
  }
  bump_revision(cn)
  id
}

record_payment <- function(cn, request_id, payment_date, stamp = db_stamp()) {
  req <- dbGetQuery(cn, "SELECT r.amount, r.currency, r.order_date, r.scenario, r.expected_days, bl.project_budget_id, r.budget_line_id
                          FROM requests r JOIN budget_lines bl ON bl.id = r.budget_line_id WHERE r.id = ?",
                    params = list(request_id))
  if (!nrow(req)) stop("Request was not found.")
  month <- substr(payment_date, 1, 7)
  rates <- fx_month_row(cn, month)
  if (!nrow(rates)) stop("No exchange rate is stored for ", month, ".")
  values <- convert_payment(as.numeric(req$amount[[1]]), req$currency[[1]], rates)
  order_date <- as.Date(substr(req$order_date[[1]], 1, 10))
  paid <- as.Date(payment_date)
  actual <- as.integer(paid - order_date)
  expected <- as.integer(req$expected_days[[1]])
  delay <- actual - expected
  dbExecute(cn, "INSERT INTO payment_events(id, request_id, project_budget_id, budget_line_id, original_amount,
             original_currency, payment_date, payment_method, recorded_at, fx_rate_month_id, fx_is_fallback,
             etb_value, usd_value, eur_value, gbp_value)
             VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
            params = list(new_uuid(), request_id, req$project_budget_id[[1]], req$budget_line_id[[1]],
                          as.numeric(req$amount[[1]]), req$currency[[1]], payment_date, "recorded", stamp,
                          rates$id[[1]], db_bool(FALSE), values$etb, values$usd, values$eur, values$gbp))
  if (db_is_postgres()) {
    dbExecute(cn, "UPDATE request_state_events SET actual_days = ?, delay_days = ?
                   WHERE request_id = ? AND to_status = 'paid'",
              params = list(actual, delay, request_id))
  } else {
    dbExecute(cn, "UPDATE request_state_events SET actual_days = ?, delay_days = ?, is_delayed = ?
                   WHERE request_id = ? AND to_status = 'paid'",
              params = list(actual, delay, as.integer(delay > 0), request_id))
  }
  invisible(values)
}

next_batch_code <- function(cn) {
  year <- format(Sys.Date(), "%Y")
  existing <- dbGetQuery(cn, "SELECT batch_code FROM financial_batches WHERE batch_code LIKE ?",
                         params = list(paste0("FB-", year, "-%")))
  nums <- suppressWarnings(as.integer(sub(paste0("^FB-", year, "-"), "", existing$batch_code)))
  nums <- nums[!is.na(nums)]
  sprintf("FB-%s-%03d", year, if (!length(nums)) 1L else max(nums) + 1L)
}

mark_seed_deduction <- function(cn, request_id, deducted_at) {
  pay <- dbGetQuery(cn, "SELECT id, gbp_value, project_budget_id FROM payment_events WHERE request_id = ?",
                    params = list(request_id))
  if (!nrow(pay)) return(invisible(FALSE))
  batch_id <- new_uuid()
  code <- next_batch_code(cn)
  amount <- round_half_away(as.numeric(pay$gbp_value[[1]]))
  dbExecute(cn, "INSERT INTO financial_batches(id, batch_code, batch_name, status, prepared_at, applied_at, total_gbp)
                 VALUES(?,?,?,?,?,?,?)",
            params = list(batch_id, code, "Seed deduction", "applied", deducted_at, deducted_at, amount))
  dbExecute(cn, "INSERT INTO batch_projects(id, batch_id, project_budget_id, draft_lock) VALUES(?,?,?,?)",
            params = list(new_uuid(), batch_id, pay$project_budget_id[[1]], db_bool(FALSE)))
  dbExecute(cn, "INSERT INTO batch_items(id, batch_id, payment_event_id, gbp_amount, released_at) VALUES(?,?,?,?,?)",
            params = list(new_uuid(), batch_id, pay$id[[1]], amount, NA_character_))
  dbExecute(cn, "UPDATE project_budgets SET last_checkpoint_at = ? WHERE id = ?",
            params = list(deducted_at, pay$project_budget_id[[1]]))
  invisible(TRUE)
}

update_expense_row <- function(cn, id, row) {
  save_request_record(cn, id, row)
}

save_request_record <- function(cn, id, row) {
  id <- as.character(id)
  current <- dbGetQuery(cn, "SELECT status, order_date, expected_days, status_changed_at FROM requests WHERE id = ?", params = list(id))
  if (!nrow(current)) stop("That request is no longer available.")
  g <- function(name, default = "") {
    value <- row[[name]]
    if (is.null(value) || !length(value) || is.na(value[[1]])) default else as.character(value[[1]])
  }
  status <- g("status", current$status[[1]])
  stamp <- db_stamp()
  project_id <- require_project_record(cn, g("project_code"))
  line_id <- require_budget_line_record(cn, project_id, g("budget_line"))
  area_id <- ensure_area(cn, if (nzchar(g("location"))) g("location") else "ADD", g("country", "ETH"))
  dep <- ensure_department(cn, g("department", "ACTED"))
  requester <- ensure_person(cn, g("requested_by"), dep)
  submitted <- ensure_person(cn, g("submitted_by"), dep)
  supplier_id <- if (nzchar(g("supplier"))) ensure_supplier(cn, g("supplier")) else NA_character_
  amount <- suppressWarnings(as.numeric(row$amount[[1]]))
  currency <- toupper(g("currency", "ETB"))
  rates <- fx_month_row(cn, substr(g("order_date", current$order_date[[1]]), 1, 7))
  if (!nrow(rates)) stop("No exchange rate is stored for this month.")
  values <- convert_payment(amount, currency, rates)
  scenario_code <- scenario(values$usd, g("waiver"))
  expected <- SCENARIO_DAYS[[scenario_code]]
  if (is.null(expected) || is.na(expected)) expected <- 7L
  status_changed <- if (identical(current$status[[1]], status)) current$status_changed_at[[1]] else stamp
  cancel_reason <- g("cancellation_reason")
  dbExecute(cn, "UPDATE requests SET budget_line_id=?, area_id=?, department_code=?, requester_id=?, submitted_by_id=?,
                 supplier_id=?, description=?, quantity=?, unit=?, amount=?, currency=?, scenario=?, expected_days=?,
                 waiver_memo=?, remarks=?, order_date=?, po_reason=?, po_date=?, po_reference=?, status=?,
                 updated_at=?, status_changed_at=?,
                 pending_entered_at=CASE WHEN ? = 'pending' AND pending_entered_at IS NULL THEN ? ELSE pending_entered_at END,
                 cancelled_at=CASE WHEN ? = 'cancelled' THEN ? ELSE cancelled_at END,
                 cancellation_reason=CASE WHEN ? = 'cancelled' THEN ? ELSE cancellation_reason END,
                 version=version+1
                 WHERE id=?",
            params = list(line_id, area_id, dep, requester, submitted, supplier_id, g("description"),
                          suppressWarnings(as.numeric(row$quantity[[1]])),
                          if (nzchar(g("unit"))) g("unit") else NA_character_,
                          amount, currency, scenario_code, as.integer(expected),
                          if (nzchar(g("waiver"))) g("waiver") else NA_character_,
                          if (nzchar(g("remarks"))) g("remarks") else NA_character_,
                          substr(g("order_date"), 1, 10),
                          if (nzchar(g("reason"))) g("reason") else NA_character_,
                          if (nzchar(g("po_date"))) substr(g("po_date"), 1, 10) else NA_character_,
                          if (nzchar(g("po"))) g("po") else NA_character_,
                          status, stamp, status_changed, status, stamp, status, stamp, status,
                          if (nzchar(cancel_reason)) cancel_reason else NA_character_, id))
  if (!identical(current$status[[1]], status)) {
    record_state_event(cn, id, current$status[[1]], status, stamp)
  }
  if (identical(status, "paid")) {
    existing <- dbGetQuery(cn, "SELECT id FROM payment_events WHERE request_id = ?", params = list(id))
    if (!nrow(existing) && nzchar(g("payment_date"))) record_payment(cn, id, substr(g("payment_date"), 1, 10), stamp)
  }
  bump_revision(cn)
  invisible(id)
}

load_batches <- function() {
  rows <- db_get("SELECT id, batch_code, batch_name, total_gbp AS total_amount, 'GBP' AS currency, status,
                         prepared_at AS created, applied_at AS confirmed_at
                  FROM financial_batches ORDER BY prepared_at DESC")
  if (!nrow(rows)) {
    rows$request_id_list <- list()
    rows$project_codes <- character()
    rows$bucket <- character()
    return(rows)
  }
  items <- db_get("SELECT bi.batch_id, pe.request_id FROM batch_items bi
                   JOIN payment_events pe ON pe.id = bi.payment_event_id
                   WHERE bi.released_at IS NULL")
  projects <- db_get("SELECT bp.batch_id, pb.code AS project_code
                      FROM batch_projects bp JOIN project_budgets pb ON pb.id = bp.project_budget_id")
  rows$request_id_list <- lapply(rows$id, function(id) {
    ids <- items$request_id[items$batch_id == id]
    as.character(ids[!is.na(ids)])
  })
  rows$project_codes <- vapply(rows$id, function(id) {
    codes <- projects$project_code[projects$batch_id == id]
    paste(codes[!is.na(codes)], collapse = "|")
  }, character(1))
  rows$bucket <- ifelse(rows$status == "active", "active",
                        ifelse(rows$status == "applied", "applied", "rejected"))
  rows
}

create_financial_batch <- function(cn, batch_name, project_codes, request_ids, total_gbp) {
  if (!nzchar(trimws(batch_name))) stop("Enter a name for the batch.")
  if (nchar(batch_name) > 80) stop("Batch name must be 80 characters or fewer.")
  request_ids <- unique(as.character(request_ids))
  request_ids <- request_ids[nzchar(request_ids)]
  if (!length(request_ids)) stop("Selected projects have no paid lines left to batch.")
  dbWithTransaction(cn, {
    pays <- dbGetQuery(cn, sprintf("SELECT id, request_id, project_budget_id, gbp_value FROM payment_events WHERE request_id IN (%s)",
                                   paste(rep("?", length(request_ids)), collapse = ",")),
                       params = as.list(request_ids))
    if (!nrow(pays)) stop("Selected projects have no paid lines left to batch.")
    code <- next_batch_code(cn)
    stamp <- db_stamp()
    batch_id <- new_uuid()
    amount <- round_half_away(sum(as.numeric(pays$gbp_value), na.rm = TRUE))
    dbExecute(cn, "INSERT INTO financial_batches(id, batch_code, batch_name, status, prepared_at, total_gbp)
                   VALUES(?,?,?,?,?,?)",
              params = list(batch_id, code, trimws(batch_name), "active", stamp, amount))
    for (project_id in unique(as.character(pays$project_budget_id))) {
      dbExecute(cn, "INSERT INTO batch_projects(id, batch_id, project_budget_id, draft_lock) VALUES(?,?,?,?)",
                params = list(new_uuid(), batch_id, project_id, db_bool(TRUE)))
    }
    for (i in seq_len(nrow(pays))) {
      dbExecute(cn, "INSERT INTO batch_items(id, batch_id, payment_event_id, gbp_amount) VALUES(?,?,?,?)",
                params = list(new_uuid(), batch_id, pays$id[[i]], round_half_away(as.numeric(pays$gbp_value[[i]]))))
    }
    bump_revision(cn)
    code
  })
}

apply_financial_batch <- function(cn, batch_code) {
  dbWithTransaction(cn, {
    batch <- dbGetQuery(cn, "SELECT id, status FROM financial_batches WHERE batch_code = ?", params = list(batch_code))
    if (!nrow(batch) || !identical(batch$status[[1]], "active")) stop("inactive")
    stamp <- db_stamp()
    dbExecute(cn, "UPDATE financial_batches SET status = 'applied', applied_at = ? WHERE id = ? AND status = 'active'",
              params = list(stamp, batch$id[[1]]))
    dbExecute(cn, "UPDATE batch_projects SET draft_lock = ? WHERE batch_id = ?",
              params = list(db_bool(FALSE), batch$id[[1]]))
    projects <- dbGetQuery(cn, "SELECT project_budget_id FROM batch_projects WHERE batch_id = ?",
                           params = list(batch$id[[1]]))
    for (project_id in projects$project_budget_id) {
      dbExecute(cn, "UPDATE project_budgets SET last_checkpoint_at = ?, updated_at = ? WHERE id = ?",
                params = list(stamp, stamp, project_id))
    }
    bump_revision(cn)
    TRUE
  })
}

reject_financial_batch <- function(cn, batch_code) {
  dbWithTransaction(cn, {
    batch <- dbGetQuery(cn, "SELECT id, status FROM financial_batches WHERE batch_code = ?", params = list(batch_code))
    if (!nrow(batch) || !identical(batch$status[[1]], "active")) stop("inactive")
    stamp <- db_stamp()
    dbExecute(cn, "UPDATE financial_batches SET status = 'rejected', rejected_at = ? WHERE id = ? AND status = 'active'",
              params = list(stamp, batch$id[[1]]))
    dbExecute(cn, "UPDATE batch_projects SET draft_lock = ? WHERE batch_id = ?",
              params = list(db_bool(FALSE), batch$id[[1]]))
    dbExecute(cn, "UPDATE batch_items SET released_at = ? WHERE batch_id = ? AND released_at IS NULL",
              params = list(stamp, batch$id[[1]]))
    bump_revision(cn)
    TRUE
  })
}
