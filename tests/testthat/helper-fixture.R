# Build an isolated SQLite database. Never opens tracker.db.

tracker_load_core <- function(db_path) {
  db_path <- normalizePath(db_path, winslash = "/", mustWork = TRUE)
  if (grepl("tracker\\.db$", db_path)) stop("Refusing to open the application database.", call. = FALSE)
  Sys.setenv(TRACKER_DB_PATH = db_path)
  root <- tracker_root()
  if (!isTRUE(get0(".tracker_core_loaded", envir = .GlobalEnv, ifnotfound = FALSE))) {
    assign("APP_DIR", root, envir = .GlobalEnv)
    sys.source(file.path(root, "R/core/config.R"), envir = .GlobalEnv)
    sys.source(file.path(root, "R/core/contracts.R"), envir = .GlobalEnv)
    sys.source(file.path(root, "R/core/db.R"), envir = .GlobalEnv)
    sys.source(file.path(root, "R/core/components.R"), envir = .GlobalEnv)
    sys.source(file.path(root, "R/features/fx_service.R"), envir = .GlobalEnv)
    sys.source(file.path(root, "R/features/finance_page.R"), envir = .GlobalEnv)
    assign(".tracker_core_loaded", TRUE, envir = .GlobalEnv)
  }
  assign("DB", db_path, envir = .GlobalEnv)
  init_db()
  invisible(db_path)
}

tracker_requests_server <- function(input, output, session) {
  sys.source(file.path(tracker_root(), "R/features/requests_service.R"), envir = environment())
}

tracker_finance_server <- function(input, output, session) {
  sys.source(file.path(tracker_root(), "R/features/fx_service.R"), envir = environment())
  sys.source(file.path(tracker_root(), "R/features/requests_service.R"), envir = environment())
  sys.source(file.path(tracker_root(), "R/features/finance_service.R"), envir = environment())
  sys.source(file.path(tracker_root(), "R/features/finance_page.R"), envir = environment())
}

tracker_manage_server <- function(input, output, session) {
  sys.source(file.path(tracker_root(), "R/features/requests_service.R"), envir = environment())
  sys.source(file.path(tracker_root(), "R/features/manage_service.R"), envir = environment())
}

tracker_insights_server <- function(input, output, session) {
  sys.source(file.path(tracker_root(), "R/features/requests_service.R"), envir = environment())
  sys.source(file.path(tracker_root(), "R/features/insights_service.R"), envir = environment())
}

tracker_insert <- function(cn, description, project, line, status,
                           amount = 1550, currency = "ETB",
                           order_date = "2026-11-03", payment_date = NA_character_,
                           created = "2026-10-15 09:00:00",
                           reason = NA_character_, po_date = NA_character_,
                           deducted_at = NA_character_,
                           requested_by = "Gedion", supplier = "Beacon Travel") {
  rate <- unname(RATES[[currency]])
  usd <- round(amount / rate, 2)
  row <- list(
    description = description, project_code = project, budget_line = line,
    country = "ETH", location = "Nekemte", department = "REACH",
    reason = reason, order_form = paste0("OF/", project, "/", description),
    po = if (status %in% c("pending", "paid")) paste0("PO/", project, "/", description) else NA_character_,
    requested_by = requested_by, submitted_by = "Aster H.", status = status,
    waiver = "No", supplier = supplier, unit = "Days", quantity = 10,
    amount = amount, currency = currency, usd = usd, rate_used = rate,
    order_date = order_date, po_date = po_date, payment_date = payment_date,
    remarks = "Seed remark", created = created, updated = created,
    deducted_at = deducted_at
  )
  insert_expense_row(cn, row)
  as.integer(dbGetQuery(cn, "SELECT last_insert_rowid() AS id")$id[[1]])
}

tracker_seed <- function(db_path) {
  cn <- con()
  on.exit(dbDisconnect(cn), add = TRUE)
  stamp <- "2026-10-01 08:00:00"
  dbExecute(cn, "INSERT INTO people(full_name, department, active, created, updated) VALUES(?,?,1,?,?)",
            params = list("Gedion", "REACH", stamp, stamp))
  dbExecute(cn, "INSERT INTO people(full_name, department, active, created, updated) VALUES(?,?,1,?,?)",
            params = list("Aster H.", "REACH", stamp, stamp))
  dbExecute(cn, "INSERT INTO suppliers(supplier_name, active, created, updated) VALUES(?,1,?,?)",
            params = list("Beacon Travel", stamp, stamp))
  ensure_project_record(cn, "ALPHA", "REACH", "Nekemte")
  alpha_id <- as.integer(dbGetQuery(cn, "SELECT id FROM projects WHERE project_code = 'ALPHA'")$id[[1]])
  ensure_budget_line_record(cn, alpha_id, "COMMS")

  ids <- list(
    review = tracker_insert(cn, "ANCHOR-REVIEW", "ALPHA", "COMMS", "under_review",
                            created = "2026-11-03 09:00:00"),
    pending = tracker_insert(cn, "ANCHOR-PENDING", "ALPHA", "COMMS", "pending",
                             reason = "Travel", po_date = "2026-11-04",
                             created = "2026-10-20 09:00:00"),
    paid_early = tracker_insert(cn, "ANCHOR-PAID-EARLY", "ALPHA", "COMMS", "paid",
                                order_date = "2026-10-01", payment_date = "2026-10-03",
                                reason = "Travel", po_date = "2026-10-02",
                                deducted_at = "2026-10-04 09:00:00"),
    paid_ontime = tracker_insert(cn, "ANCHOR-PAID-ONTIME", "ALPHA", "COMMS", "paid",
                                 order_date = "2026-10-01", payment_date = "2026-10-08",
                                 reason = "Travel", po_date = "2026-10-02",
                                 deducted_at = "2026-10-09 09:00:00"),
    paid_late = tracker_insert(cn, "ANCHOR-PAID-LATE", "ALPHA", "COMMS", "paid",
                               order_date = "2026-10-01", payment_date = "2026-10-20",
                               reason = "Travel", po_date = "2026-10-02",
                               deducted_at = "2026-10-21 09:00:00"),
    cancel = tracker_insert(cn, "ANCHOR-CANCEL", "ALPHA", "COMMS", "cancelled"),
    future = tracker_insert(cn, "ANCHOR-FUTURE", "ALPHA", "COMMS", "under_review",
                            order_date = "2026-12-20", created = "2026-11-04 09:00:00"),
    usd = tracker_insert(cn, "ANCHOR-USD", "ALPHA", "COMMS", "paid",
                         amount = 20, currency = "USD",
                         order_date = "2026-11-02", payment_date = "2026-11-05",
                         reason = "Travel", po_date = "2026-11-03",
                         deducted_at = "2026-11-05 12:00:00")
  )

  finance <- character()
  for (i in 1:12) {
    code <- sprintf("FIN%02d", i)
    ensure_project_record(cn, code, "PANDA", "Mekelle")
    project_id <- as.integer(dbGetQuery(cn, "SELECT id FROM projects WHERE project_code = ?", params = list(code))$id[[1]])
    ensure_budget_line_record(cn, project_id, "OPS")
    finance[[i]] <- code
    tracker_insert(cn, paste0("FIN-PAID-", code), code, "OPS", "paid",
                   amount = 1000, order_date = "2026-11-02", payment_date = "2026-11-05",
                   reason = "Supplies", po_date = "2026-11-03",
                   created = "2026-10-10 09:00:00")
  }
  ids$finance_same <- tracker_insert(cn, "FIN-PAID-FIN01-B", "FIN01", "OPS", "paid",
                                     amount = 2500, order_date = "2026-11-02", payment_date = "2026-11-05",
                                     reason = "Supplies", po_date = "2026-11-03",
                                     created = "2026-10-10 10:00:00")
  ids$finance_projects <- finance

  page_ids <- integer(100)
  for (i in seq_len(100)) {
    page_ids[[i]] <- tracker_insert(
      cn, sprintf("PAGE-%03d", i), "ALPHA", "COMMS", "under_review",
      requested_by = if (i %% 2L == 0L) "Aster H." else "Gedion"
    )
  }
  ids$page_ids <- page_ids

  dbExecute(cn, "CREATE TABLE IF NOT EXISTS fx (currency TEXT, etb REAL, rate_month TEXT, source TEXT)")
  rates <- c(USD = 155, EUR = 155 / 0.92, GBP = 155 / 0.79, ETB = 1)
  for (code in names(rates)) {
    dbExecute(cn, "INSERT INTO fx(currency, etb, rate_month, source) VALUES(?,?,?,?)",
              params = list(code, unname(rates[[code]]), "2026-11", "euroinfo"))
  }
  ids
}

local_tracker_db <- function(now = "2026-11-06 12:00:00", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  db <- file.path(dir, "tracker-test.sqlite")
  file.create(db)
  db <- normalizePath(db, winslash = "/", mustWork = TRUE)
  withr::local_envvar(TRACKER_NOW = now, TRACKER_DB_PATH = db, .local_envir = env)
  tracker_load_core(db)
  manifest <- tracker_seed(db)
  list(path = db, ids = manifest)
}
