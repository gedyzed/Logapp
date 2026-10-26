# Logistics Request Tracker
# R >= 4.1; use current CRAN packages.
#
# Three tabs: Dashboard, Financial report, Manage.
# One request -> one budget -> one budget line.
#
# DEMO:
# Sys.setenv(TRACKER_DEMO = "true")
# shiny::runApp()
#
# LIVE:
# Sys.setenv(TRACKER_DEMO = "false")
# shiny::runApp()
#
# No authentication. Restrict access at the hosting/network layer.
# No migration from the previous tracker.db is performed.

needed <- c(
  "shiny", "bslib", "DBI", "RSQLite",
  "DT", "httr2", "jsonlite", "writexl"
)

absent <- needed[
  !vapply(
    needed,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(absent)) {
  stop(
    "Install packages: ",
    paste(absent, collapse = ", ")
  )
}

library(shiny)
library(bslib)

options(scipen = 999)
# Handle empty dropdown choices without crashing the session.
setNames <- function(object, nm) {
  
  if (length(object) == 0L) {
    return(
      stats::setNames(
        object,
        character(0)
      )
    )
  }
  
  stats::setNames(
    object,
    nm
  )
}


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

DEMO <- TRUE

BUY_FIELD <- Sys.getenv(
  "TRACKER_CBE_BUY_FIELD",
  "transactionalBuying"
)

if (!BUY_FIELD %in% c("transactionalBuying", "cashBuying")) {
  stop("Unsupported CBE buying field.")
}

DB_PATH <- file.path(
  getwd(),
  "logistics_demo_v3.sqlite"
)


if (!dir.exists(dirname(DB_PATH))) {
  stop(
    "Database directory does not exist: ",
    dirname(DB_PATH)
  )
}

CBE_URL <- paste0(
  "https://combanketh.et/",
  "cbeapi/daily-exchange-rates/"
)

CUR <- c("ETB", "USD", "EUR", "GBP")

STAGES <- c(
  "Under review" = "under_review",
  "Pending procurement" = "pending",
  "Paid" = "paid",
  "Cancelled" = "cancelled"
)

DEPTS <- c("REACH", "PANDA", "ACTED")

SCENARIOS <- c(
  "Not recorded" = "",
  "A" = "A",
  "B" = "B",
  "C" = "C",
  "D" = "D",
  "E" = "E",
  "G" = "G"
)

# ---------------------------------------------------------------------------
# General helpers
# ---------------------------------------------------------------------------

s <- function(x) {
  if (
    is.null(x) ||
    length(x) != 1L ||
    is.na(x)
  ) {
    return("")
  }
  
  trimws(as.character(x))
}

num <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x)
}

now_utc <- function() {
  format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
}

today <- function() {
  as.Date(
    format(
      Sys.time(),
      "%Y-%m-%d",
      tz = "Africa/Addis_Ababa"
    )
  )
}

date_text <- function(x, required = FALSE) {
  x <- s(x)
  
  if (!nzchar(x)) {
    if (required) {
      stop("A date is required.")
    }
    
    return(NA_character_)
  }
  
  d <- tryCatch(
    as.Date(x, format = "%Y-%m-%d"),
    error = function(e) as.Date(NA)
  )
  
  if (
    is.na(d) ||
    format(d, "%Y-%m-%d") != x
  ) {
    stop("Invalid date: ", x)
  }
  
  as.character(d)
}

minor <- function(x) {
  if (
    !num(x) ||
    x <= 0 ||
    x > 1e10 ||
    abs(x * 100 - round(x * 100)) > 1e-5
  ) {
    stop(
      paste(
        "Amount must be positive, at most 10 billion,",
        "with at most two decimals."
      )
    )
  }
  
  round(x * 100)
}

fmt <- function(x) {
  x[!is.na(x) & x == 0] <- 0
  
  ifelse(
    is.na(x),
    "Unavailable",
    formatC(
      x / 100,
      format = "f",
      digits = 2,
      big.mark = ","
    )
  )
}

currency_value <- function(x) {
  if (s(x) %in% CUR) s(x) else "ETB"
}

label <- function(x) {
  names(STAGES)[match(x, unname(STAGES))]
}

code_ok <- function(x) {
  grepl(
    "^[A-Z0-9][A-Z0-9_-]{0,39}$",
    x
  )
}

ref_date <- function(x) {
  format(as.Date(x), "%d-%m-%Y")
}

read_date <- function(x) {
  if (!nzchar(s(x))) {
    as.Date(NA)
  } else {
    as.Date(x)
  }
}

scalar <- function(d, k, default = "") {
  if (nrow(d) && k %in% names(d)) {
    d[[k]][[1]]
  } else {
    default
  }
}

# ---------------------------------------------------------------------------
# Database helpers
# ---------------------------------------------------------------------------

con <- function() {
  cx <- DBI::dbConnect(
    RSQLite::SQLite(),
    DB_PATH,
    bigint = "numeric"
  )
  
  DBI::dbExecute(cx, "PRAGMA foreign_keys=ON")
  DBI::dbExecute(cx, "PRAGMA busy_timeout=8000")
  
  cx
}

query <- function(sql, params = NULL, cx = NULL) {
  own <- is.null(cx)
  
  if (own) {
    cx <- con()
    on.exit(DBI::dbDisconnect(cx))
  }
  
  if (is.null(params)) {
    DBI::dbGetQuery(cx, sql)
  } else {
    DBI::dbGetQuery(
      cx,
      sql,
      params = params
    )
  }
}

execute <- function(cx, sql, params = NULL) {
  if (is.null(params)) {
    DBI::dbExecute(cx, sql)
  } else {
    DBI::dbExecute(
      cx,
      sql,
      params = params
    )
  }
}

# Serialize writers before checking versions or reserving payment events.
write_tx <- function(fn) {
  cx <- con()
  committed <- FALSE
  
  on.exit({
    if (!committed) {
      try(
        execute(cx, "ROLLBACK"),
        silent = TRUE
      )
    }
    
    DBI::dbDisconnect(cx)
  })
  
  execute(cx, "BEGIN IMMEDIATE")
  
  result <- fn(cx)
  
  execute(cx, "COMMIT")
  committed <- TRUE
  
  result
}

bump <- function(cx) {
  execute(
    cx,
    "UPDATE revision SET n=n+1 WHERE id=1"
  )
}

audit <- function(
    cx,
    kind,
    id,
    action,
    note = ""
) {
  execute(
    cx,
    paste(
      "INSERT INTO audit",
      "(entity,entity_id,action,note,at)",
      "VALUES(?,?,?,?,?)"
    ),
    list(
      kind,
      as.integer(id),
      action,
      s(note),
      now_utc()
    )
  )
}

# ---------------------------------------------------------------------------
# Database schema
# ---------------------------------------------------------------------------

init_db <- function() {
  cx <- con()
  on.exit(DBI::dbDisconnect(cx))
  
  execute(cx, "PRAGMA journal_mode=WAL")
  
  if ("expenses" %in% DBI::dbListTables(cx)) {
    stop(
      paste(
        "Choose a new v3 database.",
        "This application does not migrate expenses."
      )
    )
  }
  
  statements <- c(
    'CREATE TABLE IF NOT EXISTS meta(
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL
    )',
    
    'CREATE TABLE IF NOT EXISTS revision(
      id INTEGER PRIMARY KEY CHECK(id=1),
      n INTEGER NOT NULL
    )',
    
    'INSERT OR IGNORE INTO revision VALUES(1,0)',
    
    'CREATE TABLE IF NOT EXISTS budgets(
      id INTEGER PRIMARY KEY,
      code TEXT NOT NULL UNIQUE,
      name TEXT NOT NULL,
      active INTEGER NOT NULL DEFAULT 1
        CHECK(active IN(0,1))
    )',
    
    'CREATE TABLE IF NOT EXISTS lines(
      id INTEGER PRIMARY KEY,
      budget_id INTEGER NOT NULL REFERENCES budgets(id),
      code TEXT NOT NULL,
      name TEXT NOT NULL,
      active INTEGER NOT NULL DEFAULT 1
        CHECK(active IN(0,1)),
      UNIQUE(budget_id,code),
      UNIQUE(id,budget_id)
    )',
    
    'CREATE TABLE IF NOT EXISTS areas(
      id INTEGER PRIMARY KEY,
      code TEXT NOT NULL UNIQUE,
      name TEXT NOT NULL,
      country TEXT NOT NULL,
      active INTEGER NOT NULL DEFAULT 1
        CHECK(active IN(0,1))
    )',
    
    'CREATE TABLE IF NOT EXISTS contacts(
      id INTEGER PRIMARY KEY,
      kind TEXT NOT NULL
        CHECK(kind IN("requester","focal","supplier")),
      name TEXT NOT NULL,
      active INTEGER NOT NULL DEFAULT 1
        CHECK(active IN(0,1)),
      UNIQUE(kind,name)
    )',
    
    'CREATE TABLE IF NOT EXISTS fx(
      rate_date TEXT NOT NULL,
      buy_field TEXT NOT NULL,
      currency TEXT NOT NULL,
      etb REAL NOT NULL CHECK(etb>0),
      source TEXT NOT NULL,
      fetched_at TEXT NOT NULL,
      PRIMARY KEY(rate_date,buy_field,currency)
    )',
    
    'CREATE TABLE IF NOT EXISTS requests(
      id INTEGER PRIMARY KEY AUTOINCREMENT,

      request_ref TEXT NOT NULL UNIQUE,
      po_ref TEXT UNIQUE,

      budget_id INTEGER NOT NULL REFERENCES budgets(id),
      line_id INTEGER NOT NULL,

      budget_code TEXT NOT NULL,
      line_code TEXT NOT NULL,

      area_id INTEGER NOT NULL REFERENCES areas(id),
      area_code TEXT NOT NULL,
      area_name TEXT NOT NULL,
      country TEXT NOT NULL,
      department TEXT NOT NULL,

      description TEXT NOT NULL,
      requester TEXT NOT NULL,
      focal TEXT NOT NULL,

      request_date TEXT NOT NULL,
      reference_date TEXT NOT NULL,
      processing_date TEXT,

      quantity REAL,
      unit TEXT,

      amount_minor INTEGER NOT NULL CHECK(amount_minor>0),
      currency TEXT NOT NULL
        CHECK(currency IN("ETB","USD","EUR","GBP")),

      status TEXT NOT NULL
        CHECK(status IN(
          "under_review","pending","paid","cancelled"
        )),

      scenario TEXT NOT NULL DEFAULT "",
      waiver TEXT NOT NULL DEFAULT "No",
      remarks TEXT NOT NULL DEFAULT "",
      program_remarks TEXT NOT NULL DEFAULT "",

      supplier TEXT,
      po_reason TEXT,
      po_date TEXT,
      finance_date TEXT,
      payment_date TEXT,
      cancel_reason TEXT,

      fx_date TEXT NOT NULL,
      fx_field TEXT NOT NULL,
      fx_source TEXT NOT NULL,
      fx_etb REAL NOT NULL,
      fx_usd REAL NOT NULL,
      fx_eur REAL NOT NULL,
      fx_gbp REAL NOT NULL,

      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      version INTEGER NOT NULL DEFAULT 1,

      FOREIGN KEY(line_id,budget_id)
        REFERENCES lines(id,budget_id)
    )',
    
    'CREATE TABLE IF NOT EXISTS payments(
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      request_id INTEGER NOT NULL UNIQUE
        REFERENCES requests(id),

      budget_id INTEGER NOT NULL REFERENCES budgets(id),
      line_id INTEGER NOT NULL REFERENCES lines(id),

      original_minor INTEGER NOT NULL,
      original_currency TEXT NOT NULL,
      payment_date TEXT NOT NULL,
      recorded_at TEXT NOT NULL,

      etb_minor INTEGER NOT NULL,
      usd_minor INTEGER NOT NULL,
      eur_minor INTEGER NOT NULL,
      gbp_minor INTEGER NOT NULL,

      fx_date TEXT NOT NULL,
      fx_field TEXT NOT NULL,
      fx_source TEXT NOT NULL
    )',
    
    'CREATE TABLE IF NOT EXISTS batches(
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      status TEXT NOT NULL
        CHECK(status IN("draft","applied","abandoned")),
      created_at TEXT NOT NULL,
      applied_at TEXT,
      note TEXT NOT NULL DEFAULT ""
    )',
    
    'CREATE TABLE IF NOT EXISTS batch_items(
      batch_id INTEGER NOT NULL REFERENCES batches(id),
      payment_id INTEGER NOT NULL UNIQUE REFERENCES payments(id),
      PRIMARY KEY(batch_id,payment_id)
    )',
    
    'CREATE TABLE IF NOT EXISTS audit(
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      entity TEXT NOT NULL,
      entity_id INTEGER NOT NULL,
      action TEXT NOT NULL,
      note TEXT NOT NULL,
      at TEXT NOT NULL
    )',
    
    'CREATE INDEX IF NOT EXISTS req_stage_date
      ON requests(status,request_date)',
    
    'CREATE INDEX IF NOT EXISTS payment_line
      ON payments(line_id,payment_date)',
    
    'CREATE TRIGGER IF NOT EXISTS paid_locked
      BEFORE UPDATE ON requests
      WHEN OLD.status="paid"
      BEGIN
        SELECT RAISE(ABORT,"Paid requests are locked");
      END',
    
    'CREATE TRIGGER IF NOT EXISTS cancelled_locked
      BEFORE UPDATE ON requests
      WHEN OLD.status="cancelled"
      BEGIN
        SELECT RAISE(ABORT,"Cancelled requests are locked");
      END',
    
    'CREATE TRIGGER IF NOT EXISTS request_no_delete
      BEFORE DELETE ON requests
      BEGIN
        SELECT RAISE(ABORT,"Requests cannot be deleted");
      END',
    
    'CREATE TRIGGER IF NOT EXISTS payment_no_update
      BEFORE UPDATE ON payments
      BEGIN
        SELECT RAISE(
          ABORT,"Payment snapshots are immutable"
        );
      END',
    
    'CREATE TRIGGER IF NOT EXISTS payment_no_delete
      BEFORE DELETE ON payments
      BEGIN
        SELECT RAISE(
          ABORT,"Payment snapshots cannot be deleted"
        );
      END',
    
    'CREATE TRIGGER IF NOT EXISTS applied_batch_locked
      BEFORE UPDATE ON batches
      WHEN OLD.status="applied"
      BEGIN
        SELECT RAISE(
          ABORT,"Applied checkpoints are immutable"
        );
      END',
    
    'CREATE TRIGGER IF NOT EXISTS applied_item_locked
      BEFORE DELETE ON batch_items
      WHEN (
        SELECT status FROM batches
        WHERE id=OLD.batch_id
      )="applied"
      BEGIN
        SELECT RAISE(
          ABORT,"Applied checkpoint items are immutable"
        );
      END',
    
    'CREATE TRIGGER IF NOT EXISTS batch_item_no_update
      BEFORE UPDATE ON batch_items
      BEGIN
        SELECT RAISE(
          ABORT,"Checkpoint items cannot be reassigned"
        );
      END',
    
    'CREATE TRIGGER IF NOT EXISTS batch_item_draft_only
      BEFORE INSERT ON batch_items
      WHEN (
        SELECT status FROM batches
        WHERE id=NEW.batch_id
      )!="draft"
      BEGIN
        SELECT RAISE(
          ABORT,"Only draft checkpoints accept items"
        );
      END'
  )
  
  DBI::dbWithTransaction(cx, {
    for (sql in statements) {
      execute(cx, sql)
    }
    
    m <- query(
      'SELECT value FROM meta WHERE key="mode"',
      cx = cx
    )
    
    expected <- if (DEMO) "demo" else "live"
    
    if (
      nrow(m) &&
      m$value[[1]] != expected
    ) {
      stop(
        paste(
          "Database mode mismatch.",
          "Demo and live must use separate database files."
        )
      )
    }
    
    execute(
      cx,
      'INSERT OR IGNORE INTO meta VALUES("mode",?)',
      list(expected)
    )
    
    execute(
      cx,
      'INSERT OR IGNORE INTO meta VALUES("schema","3")'
    )
    
    version <- query(
      'SELECT value FROM meta WHERE key="schema"',
      cx = cx
    )$value[[1]]
    
    if (version != "3") {
      stop("Unsupported database schema.")
    }
  })
}

init_db()

# ---------------------------------------------------------------------------
# CBE exchange rates
# ---------------------------------------------------------------------------

# Case-insensitive list lookup.
ci <- function(x, key) {
  if (!is.list(x) || is.null(names(x))) {
    return(NULL)
  }
  
  i <- match(
    tolower(key),
    tolower(names(x))
  )
  
  if (is.na(i)) NULL else x[[i]]
}

unwrap <- function(x) {
  if (!is.null(ci(x, "data"))) {
    x <- ci(x, "data")
  }
  
  if (
    is.list(x) &&
    is.null(names(x)) &&
    length(x)
  ) {
    x <- x[[1]]
  }
  
  if (!is.null(ci(x, "attributes"))) {
    x <- ci(x, "attributes")
  }
  
  x
}

parse_cbe <- function(
    body,
    requested,
    field = BUY_FIELD
) {
  x <- unwrap(body)
  
  if (!is.list(x)) {
    stop("CBE returned no usable rate record.")
  }
  
  dt <- substr(s(ci(x, "date")), 1, 10)
  dt <- date_text(dt, TRUE)
  
  if (as.Date(dt) > as.Date(requested)) {
    stop("CBE returned a future rate date.")
  }
  
  rows <- ci(x, "exchangeRate")
  
  if (is.null(rows)) {
    rows <- ci(x, "exchangeRates")
  }
  
  if (!is.null(ci(rows, "data"))) {
    rows <- ci(rows, "data")
  }
  
  if (is.null(rows) || !is.list(rows)) {
    stop("CBE exchangeRate array is missing.")
  }
  
  if (!is.null(ci(rows, "currency"))) {
    rows <- list(rows)
  }
  
  rates <- c(
    ETB = 1,
    USD = NA_real_,
    EUR = NA_real_,
    GBP = NA_real_
  )
  
  for (r in rows) {
    r <- unwrap(r)
    cc <- unwrap(ci(r, "currency"))
    
    code <- toupper(
      s(ci(cc, "CurrencyCode"))
    )
    
    val <- s(ci(r, field))
    
    value <- suppressWarnings(
      as.numeric(
        gsub(",", "", val, fixed = TRUE)
      )
    )
    
    if (
      code %in% names(rates) &&
      code != "ETB" &&
      length(value) == 1L &&
      is.finite(value) &&
      value > 0
    ) {
      rates[[code]] <- value
    }
  }
  
  if (
    any(!is.finite(rates)) ||
    any(rates <= 0)
  ) {
    stop(
      paste(
        "CBE response lacks positive USD,",
        "EUR or GBP buying rates."
      )
    )
  }
  
  list(
    date = dt,
    field = field,
    rates = rates,
    source = paste("CBE", field)
  )
}

cache_fx <- function(f) {
  write_tx(function(cx) {
    for (k in CUR) {
      execute(
        cx,
        "INSERT OR REPLACE INTO fx VALUES(?,?,?,?,?,?)",
        list(
          f$date,
          f$field,
          k,
          f$rates[[k]],
          f$source,
          now_utc()
        )
      )
    }
    
    bump(cx)
    invisible(f)
  })
}

read_fx <- function() {
  field <- if (DEMO) "DEMO" else BUY_FIELD
  
  d <- query(
    paste(
      "SELECT * FROM fx",
      "WHERE buy_field=? AND rate_date<=?",
      "ORDER BY rate_date DESC"
    ),
    list(
      field,
      as.character(today())
    )
  )
  
  for (dt in unique(d$rate_date)) {
    z <- d[d$rate_date == dt, , drop = FALSE]
    
    if (all(CUR %in% z$currency)) {
      rates <- setNames(
        z$etb,
        z$currency
      )[CUR]
      
      if (
        all(is.finite(rates)) &&
        all(rates > 0)
      ) {
        return(
          list(
            date = dt,
            field = field,
            rates = rates,
            source = z$source[[1]]
          )
        )
      }
    }
  }
  
  NULL
}

fetch_fx <- function() {
  if (DEMO) {
    return(read_fx())
  }
  
  errors <- character()
  started <- Sys.time()
  
  for (back in 0:7) {
    if (
      as.numeric(
        difftime(
          Sys.time(),
          started,
          units = "secs"
        )
      ) > 15
    ) {
      break
    }
    
    dt <- as.character(today() - back)
    
    response <- tryCatch({
      req <- httr2::request(CBE_URL)
      
      req <- httr2::req_url_query(
        req,
        `_limit` = 1,
        Date = dt
      )
      
      req <- httr2::req_timeout(req, 5)
      
      req <- httr2::req_user_agent(
        req,
        "LogisticsTracker/3.0"
      )
      
      resp <- httr2::req_perform(req)
      
      raw <- httr2::resp_body_json(
        resp,
        simplifyVector = FALSE
      )
      
      parse_cbe(raw, dt)
    }, error = function(e) e)
    
    if (!inherits(response, "error")) {
      cache_fx(response)
      return(response)
    }
    
    errors <- c(
      errors,
      conditionMessage(response)
    )
    
    if (
      grepl(
        "403|401|timed out|resolve|connect",
        tail(errors, 1),
        ignore.case = TRUE
      )
    ) {
      break
    }
  }
  
  stop(
    "CBE could not be refreshed. ",
    tail(errors, 1)
  )
}

fx_ready <- function(f) {
  if (is.null(f)) {
    stop(
      paste(
        "Fetch CBE rates in Manage before",
        "recording or paying a request."
      )
    )
  }
  
  if (
    !DEMO &&
    as.integer(today() - as.Date(f$date)) > 7
  ) {
    stop(
      paste(
        "Cached CBE rates are older than seven days.",
        "Refresh rates first."
      )
    )
  }
  
  f
}

converted <- function(
    amount,
    from,
    to,
    f
) {
  value <- amount *
    f$rates[[from]] /
    f$rates[[to]]
  
  if (
    !is.finite(value) ||
    value < 0 ||
    value > 9e14
  ) {
    stop(
      "Converted amount is outside the supported range."
    )
  }
  
  floor(value + 0.5)
}

request_fx <- function(r) {
  list(
    date = r$fx_date[[1]],
    field = r$fx_field[[1]],
    source = r$fx_source[[1]],
    
    rates = c(
      ETB = r$fx_etb[[1]],
      USD = r$fx_usd[[1]],
      EUR = r$fx_eur[[1]],
      GBP = r$fx_gbp[[1]]
    )
  )
}

# ---------------------------------------------------------------------------
# References
# ---------------------------------------------------------------------------

unique_ref <- function(
    cx,
    column,
    base
) {
  if (!column %in% c("request_ref", "po_ref")) {
    stop("Invalid reference column.")
  }
  
  candidate <- base
  n <- 1L
  
  while (
    nrow(
      query(
        paste0(
          "SELECT id FROM requests WHERE ",
          column,
          "=?"
        ),
        list(candidate),
        cx
      )
    )
  ) {
    n <- n + 1L
    
    candidate <- paste0(
      base,
      "/",
      sprintf("%03d", n)
    )
  }
  
  candidate
}

refs_base <- function(
    budget,
    line,
    country,
    area,
    dept,
    dt,
    reason = NULL
) {
  path <- paste(
    budget,
    line,
    country,
    area,
    dept,
    sep = "/"
  )
  
  if (is.null(reason)) {
    paste0(
      "OF/",
      path,
      "/",
      ref_date(dt)
    )
  } else {
    paste0(
      "PO/OF/",
      path,
      "/",
      reason,
      "/",
      ref_date(dt)
    )
  }
}

# ---------------------------------------------------------------------------
# Request creation and editing
# ---------------------------------------------------------------------------

create_request <- function(
    v,
    f,
    edit = NULL
) {
  for (
    k in c(
      "description",
      "requester",
      "focal",
      "department"
    )
  ) {
    if (!nzchar(s(v[[k]]))) {
      stop("Missing required request information.")
    }
  }
  
  if (
    !v$currency %in% CUR ||
    !v$department %in% DEPTS ||
    !v$scenario %in% unname(SCENARIOS)
  ) {
    stop("Invalid request classification.")
  }
  
  if (
    !num(v$amount_minor) ||
    v$amount_minor <= 0 ||
    v$amount_minor != round(v$amount_minor)
  ) {
    stop("Invalid amount in minor units.")
  }
  
  if (
    nchar(v$description) > 10000 ||
    nchar(v$remarks) > 20000 ||
    nchar(v$program_remarks) > 20000
  ) {
    stop("Text exceeds supported length.")
  }
  
  f <- fx_ready(f)
  
  write_tx(function(cx) {
    b <- query(
      "SELECT * FROM budgets WHERE id=? AND active=1",
      list(v$budget_id),
      cx
    )
    
    l <- query(
      paste(
        "SELECT * FROM lines",
        "WHERE id=? AND budget_id=? AND active=1"
      ),
      list(
        v$line_id,
        v$budget_id
      ),
      cx
    )
    
    a <- query(
      "SELECT * FROM areas WHERE id=? AND active=1",
      list(v$area_id),
      cx
    )
    
    if (
      !nrow(b) ||
      !nrow(l) ||
      !nrow(a)
    ) {
      stop(
        "Choose active budget, budget line and area."
      )
    }
    
    old <- NULL
    
    if (!is.null(edit)) {
      old <- query(
        "SELECT * FROM requests WHERE id=?",
        list(edit$id),
        cx
      )
      
      if (
        !nrow(old) ||
        old$version[[1]] != edit$version ||
        !old$status[[1]] %in% c(
          "under_review",
          "pending"
        )
      ) {
        stop(
          paste(
            "Request changed or is no longer editable.",
            "Reopen it."
          )
        )
      }
      
      if (
        old$amount_minor[[1]] == v$amount_minor &&
        old$currency[[1]] == v$currency
      ) {
        f <- request_fx(old)
      }
      
      if (
        old$budget_id[[1]] != v$budget_id ||
        old$line_id[[1]] != v$line_id ||
        old$area_id[[1]] != v$area_id ||
        old$department[[1]] != v$department ||
        old$reference_date[[1]] != v$reference_date
      ) {
        stop(
          paste(
            "Issued reference classification cannot change.",
            "Cancel and create a corrected request."
          )
        )
      }
    }
    
    row <- c(
      v,
      list(
        budget_code = b$code[[1]],
        line_code = l$code[[1]],
        area_code = a$code[[1]],
        area_name = a$name[[1]],
        country = a$country[[1]],
        
        fx_date = f$date,
        fx_field = f$field,
        fx_source = f$source,
        
        fx_etb = f$rates[["ETB"]],
        fx_usd = f$rates[["USD"]],
        fx_eur = f$rates[["EUR"]],
        fx_gbp = f$rates[["GBP"]],
        
        updated_at = now_utc()
      )
    )
    
    if (is.null(edit)) {
      row$request_ref <- unique_ref(
        cx,
        "request_ref",
        refs_base(
          b$code[[1]],
          l$code[[1]],
          a$country[[1]],
          a$code[[1]],
          v$department,
          v$reference_date
        )
      )
      
      row$status <- "under_review"
      row$created_at <- row$updated_at
      
      execute(
        cx,
        paste0(
          "INSERT INTO requests(",
          paste(names(row), collapse = ","),
          ") VALUES(",
          paste(
            rep("?", length(row)),
            collapse = ","
          ),
          ")"
        ),
        unname(row)
      )
      
      id <- query(
        "SELECT last_insert_rowid() AS id",
        cx = cx
      )$id[[1]]
      
      audit(
        cx,
        "request",
        id,
        "created",
        row$request_ref
      )
    } else {
      id <- edit$id
      
      execute(
        cx,
        paste0(
          "UPDATE requests SET ",
          paste(
            paste0(names(row), "=?"),
            collapse = ","
          ),
          ",version=version+1 WHERE id=?"
        ),
        c(
          unname(row),
          list(id)
        )
      )
      
      audit(
        cx,
        "request",
        id,
        "edited",
        "Open request details edited"
      )
    }
    
    for (kind in c("requester", "focal")) {
      execute(
        cx,
        paste(
          "INSERT OR IGNORE INTO contacts(kind,name)",
          "VALUES(?,?)"
        ),
        list(
          kind,
          v[[kind]]
        )
      )
    }
    
    if (
      "supplier" %in% names(v) &&
      nzchar(s(v$supplier))
    ) {
      execute(
        cx,
        paste(
          'INSERT OR IGNORE INTO contacts(kind,name)',
          'VALUES("supplier",?)'
        ),
        list(v$supplier)
      )
    }
    
    bump(cx)
    
    id
  })
}

# ---------------------------------------------------------------------------
# Stage transitions
# ---------------------------------------------------------------------------

transition <- function(
    id,
    version,
    action,
    values = list(),
    f = NULL
) {
  write_tx(function(cx) {
    r <- query(
      "SELECT * FROM requests WHERE id=?",
      list(id),
      cx
    )
    
    if (
      !nrow(r) ||
      r$version[[1]] != version
    ) {
      stop(
        paste(
          "This request changed.",
          "Reopen it before proceeding."
        )
      )
    }
    
    st <- r$status[[1]]
    
    row <- list(
      updated_at = now_utc()
    )
    
    if (action == "approve") {
      if (st != "under_review") {
        stop(
          "Only under-review requests can proceed to procurement."
        )
      }
      
      if (
        !nzchar(s(values$supplier)) ||
        !nzchar(s(values$po_reason))
      ) {
        stop("Supplier and PO reason are required.")
      }
      
      if (grepl("[/\\\\]", values$po_reason)) {
        stop("PO reason cannot contain slashes.")
      }
      
      dt <- date_text(
        values$po_date,
        TRUE
      )
      
      if (as.Date(dt) > today()) {
        stop("PO reference date cannot be in the future.")
      }
      
      row$status <- "pending"
      row$supplier <- s(values$supplier)
      row$po_reason <- s(values$po_reason)
      row$po_date <- dt
      
      row$po_ref <- unique_ref(
        cx,
        "po_ref",
        refs_base(
          r$budget_code[[1]],
          r$line_code[[1]],
          r$country[[1]],
          r$area_code[[1]],
          r$department[[1]],
          dt,
          row$po_reason
        )
      )
      
      execute(
        cx,
        paste(
          'INSERT OR IGNORE INTO contacts(kind,name)',
          'VALUES("supplier",?)'
        ),
        list(row$supplier)
      )
      
      row$processing_date <- if (
        nzchar(s(r$processing_date[[1]]))
      ) {
        r$processing_date[[1]]
      } else {
        as.character(today())
      }
    } else if (action == "cancel") {
      if (!st %in% c("under_review", "pending")) {
        stop("This request cannot be cancelled.")
      }
      
      if (!nzchar(s(values$reason))) {
        stop("Enter a cancellation reason.")
      }
      
      row$status <- "cancelled"
      row$cancel_reason <- s(values$reason)
    } else if (action == "paid") {
      if (st != "pending") {
        stop(
          "Only pending procurement can be marked paid."
        )
      }
      
      f <- fx_ready(f)
      
      dt <- date_text(
        values$payment_date,
        TRUE
      )
      
      if (as.Date(dt) > today()) {
        stop("Payment date cannot be in the future.")
      }
      
      row$status <- "paid"
      row$payment_date <- dt
      
      amounts <- vapply(
        CUR,
        function(k) {
          converted(
            r$amount_minor[[1]],
            r$currency[[1]],
            k,
            f
          )
        },
        numeric(1)
      )
      
      execute(
        cx,
        paste(
          "INSERT INTO payments(",
          "request_id,budget_id,line_id,",
          "original_minor,original_currency,",
          "payment_date,recorded_at,",
          "etb_minor,usd_minor,eur_minor,gbp_minor,",
          "fx_date,fx_field,fx_source",
          ") VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        ),
        list(
          id,
          r$budget_id[[1]],
          r$line_id[[1]],
          r$amount_minor[[1]],
          r$currency[[1]],
          dt,
          now_utc(),
          amounts[[1]],
          amounts[[2]],
          amounts[[3]],
          amounts[[4]],
          f$date,
          f$field,
          f$source
        )
      )
    } else if (action == "pending_edit") {
      if (st != "pending") {
        stop(
          "Only pending requests accept these edits."
        )
      }
      
      row$remarks <- s(values$remarks)
      row$program_remarks <- s(values$program_remarks)
      row$finance_date <- date_text(values$finance_date)
    } else {
      stop("Unsupported action.")
    }
    
    execute(
      cx,
      paste0(
        "UPDATE requests SET ",
        paste(
          paste0(names(row), "=?"),
          collapse = ","
        ),
        ",version=version+1 WHERE id=?"
      ),
      c(
        unname(row),
        list(id)
      )
    )
    
    audit(
      cx,
      "request",
      id,
      action
    )
    
    bump(cx)
    
    invisible(id)
  })
}

# ---------------------------------------------------------------------------
# Payments and checkpoint batches
# ---------------------------------------------------------------------------

payment_rows <- function(cx = NULL) {
  query(
    paste(
      "SELECT p.*,",
      "r.request_ref,r.po_ref,r.description,r.requester,",
      "b.code AS budget_code,b.name AS budget_name,",
      "l.code AS line_code,l.name AS line_name,",
      "bi.batch_id,ba.status AS batch_status,ba.applied_at",
      "FROM payments p",
      "JOIN requests r ON r.id=p.request_id",
      "JOIN budgets b ON b.id=p.budget_id",
      "JOIN lines l ON l.id=p.line_id",
      "LEFT JOIN batch_items bi ON bi.payment_id=p.id",
      "LEFT JOIN batches ba ON ba.id=bi.batch_id",
      "ORDER BY p.id"
    ),
    cx = cx
  )
}

make_batch <- function(
    ids,
    note = ""
) {
  write_tx(function(cx) {
    ids <- unique(as.integer(ids))
    
    if (
      !length(ids) ||
      anyNA(ids)
    ) {
      stop(
        "Select budget lines with new unreserved payments."
      )
    }
    
    p <- payment_rows(cx)
    
    p <- p[
      p$line_id %in% ids &
        is.na(p$batch_id),
      ,
      drop = FALSE
    ]
    
    if (!nrow(p)) {
      stop(
        paste(
          "No unreserved payment events remain in those lines.",
          "Refresh the report."
        )
      )
    }
    
    execute(
      cx,
      paste(
        "INSERT INTO batches(status,created_at,note)",
        'VALUES("draft",?,?)'
      ),
      list(
        now_utc(),
        s(note)
      )
    )
    
    id <- query(
      "SELECT last_insert_rowid() AS id",
      cx = cx
    )$id[[1]]
    
    for (pid in p$id) {
      execute(
        cx,
        "INSERT INTO batch_items VALUES(?,?)",
        list(id, pid)
      )
    }
    
    audit(
      cx,
      "batch",
      id,
      "prepared",
      paste(
        nrow(p),
        "exact payment events reserved"
      )
    )
    
    bump(cx)
    
    id
  })
}

apply_batch <- function(id) {
  write_tx(function(cx) {
    b <- query(
      "SELECT * FROM batches WHERE id=?",
      list(id),
      cx
    )
    
    if (
      !nrow(b) ||
      b$status[[1]] != "draft"
    ) {
      stop("This batch is no longer a draft.")
    }
    
    items <- query(
      "SELECT 1 FROM batch_items WHERE batch_id=?",
      list(id),
      cx
    )
    
    if (!nrow(items)) {
      stop("Empty batch cannot be applied.")
    }
    
    execute(
      cx,
      paste(
        'UPDATE batches SET status="applied",',
        "applied_at=? WHERE id=?"
      ),
      list(
        now_utc(),
        id
      )
    )
    
    audit(
      cx,
      "batch",
      id,
      "applied",
      "User confirmed external budgets were updated"
    )
    
    bump(cx)
  })
}

abandon_batch <- function(id) {
  write_tx(function(cx) {
    b <- query(
      "SELECT * FROM batches WHERE id=?",
      list(id),
      cx
    )
    
    if (
      !nrow(b) ||
      b$status[[1]] != "draft"
    ) {
      stop("Only a draft batch can be abandoned.")
    }
    
    execute(
      cx,
      "DELETE FROM batch_items WHERE batch_id=?",
      list(id)
    )
    
    execute(
      cx,
      'UPDATE batches SET status="abandoned" WHERE id=?',
      list(id)
    )
    
    audit(
      cx,
      "batch",
      id,
      "abandoned",
      "Reserved events released"
    )
    
    bump(cx)
  })
}

# ---------------------------------------------------------------------------
# Fictional demo data
# ---------------------------------------------------------------------------

seed_demo <- function() {
  if (!DEMO) {
    return(invisible(NULL))
  }
  
  f <- list(
    date = as.character(today()),
    field = "DEMO",
    
    rates = c(
      ETB = 1,
      USD = 150,
      EUR = 165,
      GBP = 190
    ),
    
    source = "SYNTHETIC DEMO RATES - NOT CBE"
  )
  
  cache_fx(f)
  
  if (
    nrow(
      query(
        'SELECT value FROM meta WHERE key="demo_seeded"'
      )
    )
  ) {
    return(invisible(NULL))
  }
  
  write_tx(function(cx) {
    for (i in 1:2) {
      execute(
        cx,
        paste(
          "INSERT OR IGNORE INTO budgets(id,code,name)",
          "VALUES(?,?,?)"
        ),
        list(
          i,
          paste0("DEMO0", i),
          paste("Demo budget", i)
        )
      )
      
      for (j in 1:2) {
        execute(
          cx,
          paste(
            "INSERT OR IGNORE INTO lines",
            "(id,budget_id,code,name)",
            "VALUES(?,?,?,?)"
          ),
          list(
            (i - 1) * 2 + j,
            i,
            paste0("LINE0", j),
            paste("Demo line", j)
          )
        )
      }
    }
    
    execute(
      cx,
      paste(
        "INSERT OR IGNORE INTO areas",
        "(id,code,name,country)",
        'VALUES(1,"TST","Demo area","ETH")'
      )
    )
    
    for (k in c("requester", "focal", "supplier")) {
      execute(
        cx,
        paste(
          "INSERT OR IGNORE INTO contacts(kind,name)",
          "VALUES(?,?)"
        ),
        list(
          k,
          paste("Demo", k)
        )
      )
    }
    
    bump(cx)
  })
  
  ids <- integer()
  
  for (i in 1:8) {
    b <- if (i %% 2) 1L else 2L
    l <- if (b == 1) 1L else 3L
    
    dt <- as.character(
      max(
        as.Date(format(today(), "%Y-%m-01")),
        today() - i
      )
    )
    
    v <- list(
      budget_id = b,
      line_id = l,
      area_id = 1L,
      department = "REACH",
      description = paste("DEMO ONLY - request", i),
      requester = "Demo requester",
      focal = "Demo focal",
      request_date = dt,
      reference_date = dt,
      processing_date = NA_character_,
      quantity = 10,
      unit = "Days",
      amount_minor = as.numeric(i * 10000),
      currency = if (i %% 2) "USD" else "ETB",
      scenario = "G",
      waiver = "Framework agreement",
      remarks = "Synthetic test record",
      program_remarks = ""
    )
    
    id <- create_request(v, f)
    ids <- c(ids, id)
    
    if (i >= 3) {
      transition(
        id,
        1L,
        "approve",
        list(
          supplier = "Demo supplier",
          po_reason = "Demo service",
          po_date = dt
        )
      )
    }
    
    if (i %in% c(5, 6, 7)) {
      transition(
        id,
        2L,
        "paid",
        list(
          payment_date = as.character(today())
        ),
        f
      )
    }
    
    if (i == 8) {
      transition(
        id,
        2L,
        "cancel",
        list(
          reason = "Demo cancellation"
        )
      )
    }
  }
  
  p <- payment_rows()
  
  first <- p[
    p$request_id == ids[[5]],
    ,
    drop = FALSE
  ]
  
  write_tx(function(cx) {
    execute(
      cx,
      paste(
        "INSERT INTO batches(status,created_at,note)",
        'VALUES("draft",?,"DEMO previously applied")'
      ),
      list(now_utc())
    )
    
    bid <- query(
      "SELECT last_insert_rowid() AS id",
      cx = cx
    )$id[[1]]
    
    execute(
      cx,
      "INSERT INTO batch_items VALUES(?,?)",
      list(
        bid,
        first$id[[1]]
      )
    )
    
    execute(
      cx,
      paste(
        'UPDATE batches SET status="applied",',
        "applied_at=? WHERE id=?"
      ),
      list(
        now_utc(),
        bid
      )
    )
    
    execute(
      cx,
      'INSERT INTO meta VALUES("demo_seeded","yes")'
    )
    
    bump(cx)
  })
}

seed_demo()

# ---------------------------------------------------------------------------
# UI helpers
# ---------------------------------------------------------------------------

choices <- function(
    d,
    label_col = "name"
) {
  if (!nrow(d)) {
    return(
      setNames(
        character(),
        character()
      )
    )
  }
  
  setNames(
    as.character(d$id),
    
    if ("code" %in% names(d)) {
      paste(
        d$code,
        d[[label_col]],
        sep = " - "
      )
    } else {
      d[[label_col]]
    }
  )
}

blank_date <- function(
    id,
    title,
    value = NA
) {
  dateInput(
    id,
    title,
    value = value,
    format = "dd MM yyyy"
  )
}

info <- function(
    title,
    value
) {
  div(
    class = "detail",
    div(
      class = "muted small",
      title
    ),
    div(
      class = "value",
      if (nzchar(s(value))) {
        s(value)
      } else {
        "Not recorded"
      }
    )
  )
}

box <- function(
    title,
    ...
) {
  div(
    class = "box",
    h3(title),
    ...
  )
}

stage_badge <- function(st) {
  span(
    class = paste("badge-state", st),
    label(st)
  )
}

# ---------------------------------------------------------------------------
# Styling
# ---------------------------------------------------------------------------

css <- "
:root{
  --blue:#005baa;
  --ink:#19324d;
  --muted:#61748a;
  --border:#dfe7ef;
}
body{
  background:#f5f7fa;
  color:var(--ink);
  font-family:system-ui,-apple-system,'Segoe UI',sans-serif;
}
.navbar{
  background:white!important;
  border-bottom:1px solid var(--border);
}
.navbar-brand{
  font-weight:750;
  color:var(--blue)!important;
}
.nav-link{font-weight:600}
.navbar .nav-link.active{color:var(--blue)!important}
.shell{
  max-width:1360px;
  margin:auto;
  padding:24px 16px 50px;
}
.heading{
  display:flex;
  justify-content:space-between;
  gap:16px;
  align-items:center;
  flex-wrap:wrap;
  margin-bottom:20px;
}
h1{
  font-size:1.65rem;
  font-weight:700;
  margin:0 0 6px;
}
h2{font-size:1.2rem;font-weight:650}
h3{
  font-size:1rem;
  font-weight:650;
  margin-bottom:16px;
}
.muted{color:var(--muted)}
.small{font-size:.83rem}
.box{
  background:white;
  border:1px solid var(--border);
  border-radius:10px;
  padding:20px;
  margin-bottom:18px;
}
.notice{
  background:#edf5ff;
  border-left:4px solid var(--blue);
  padding:12px 16px;
  border-radius:5px;
  margin-bottom:16px;
}
.demo{
  background:#fff2cc;
  border:1px solid #e9cb65;
  padding:12px 18px;
  font-weight:650;
}
.metrics{
  display:grid;
  grid-template-columns:repeat(4,minmax(0,1fr));
  gap:12px;
  margin-bottom:20px;
}
.metric{
  display:block;
  text-align:left;
  background:white;
  border:1px solid var(--border);
  border-radius:10px;
  padding:18px;
  color:var(--ink);
  width:100%;
}
.metric.active{
  border:2px solid var(--blue);
  background:#f0f7ff;
  padding:17px;
}
.metric strong{
  display:block;
  font-size:2rem;
  margin-top:4px;
}
.metric small{color:var(--muted)}
.metric:focus-visible,.po-card:focus-visible{
  outline:3px solid #58a6eb;
  outline-offset:2px;
}
.toolbar{
  display:flex;
  gap:12px;
  align-items:end;
  flex-wrap:wrap;
}
.toolbar .shiny-input-container{margin-bottom:8px}
.po-card{
  width:100%;
  background:white;
  border:1px solid var(--border);
  border-radius:9px;
  padding:18px;
  text-align:left;
  margin-bottom:10px;
  color:var(--ink);
  display:block;
}
.po-card:hover{
  border-color:#91b8da;
  background:#fbfdff;
}
.po-top{
  display:flex;
  justify-content:space-between;
  gap:16px;
  align-items:start;
}
.po-amount{
  font-weight:750;
  font-size:1.1rem;
  white-space:nowrap;
}
.po-desc{margin:8px 0;color:#3c526a}
.ref{
  font-size:.76rem;
  color:var(--muted);
  overflow-wrap:anywhere;
}
.po-person{font-weight:650}
.badge-state{
  font-size:.75rem;
  font-weight:650;
  padding:4px 9px;
  border-radius:5px;
  display:inline-block;
  margin-left:8px;
}
.under_review{background:#e7f1ff;color:#005baa}
.pending{background:#fff1d4;color:#805500}
.paid{background:#e6f5eb;color:#206b3d}
.cancelled{background:#edf0f3;color:#596675}
.grid2{
  display:grid;
  grid-template-columns:repeat(2,minmax(0,1fr));
  gap:0 18px;
}
.full{grid-column:1/-1}
.control-label{font-size:.87rem;font-weight:600}
.shiny-input-container{width:100%!important}
.form-control,.form-select,.selectize-input{
  border-color:#cbd8e5;
  border-radius:5px;
  box-shadow:none;
}
.btn{border-radius:6px;font-weight:600}
.btn-primary{
  background:var(--blue);
  border-color:var(--blue);
}
.actions{
  display:flex;
  gap:10px;
  flex-wrap:wrap;
  align-items:center;
}
.detail-grid{
  display:grid;
  grid-template-columns:repeat(2,minmax(0,1fr));
  gap:14px 22px;
}
.value{
  overflow-wrap:anywhere;
  white-space:pre-wrap;
}
.modal-content{border-radius:12px}
.modal-header{border-bottom:1px solid var(--border)}
.error-box{
  background:#fff0f0;
  color:#922c2c;
  padding:12px 16px;
  border-radius:6px;
  margin-bottom:16px;
}
.budget-group{
  border:1px solid var(--border);
  border-radius:8px;
  background:white;
  margin-bottom:12px;
}
.budget-group summary{
  padding:16px;
  cursor:pointer;
  display:flex;
  justify-content:space-between;
  gap:12px;
  font-weight:650;
}
.budget-group .inside{padding:0 16px 16px}
.table td,.table th{
  font-size:.86rem;
  vertical-align:middle;
}
.negative{color:#9b3030;font-weight:700}
.empty{
  padding:35px;
  text-align:center;
  color:var(--muted);
}
.shiny-notification{opacity:1}
.rate-line{
  font-size:.83rem;
  color:var(--muted);
  padding:8px 16px;
  border-bottom:1px solid var(--border);
  background:white;
}
@media(max-width:750px){
  .metrics{grid-template-columns:repeat(2,1fr)}
  .grid2,.detail-grid{grid-template-columns:1fr}
  .shell{padding:18px 10px}
  .po-top{flex-wrap:wrap}
  .po-amount{font-size:1rem}
  .box{padding:16px}
}
"
# ---------------------------------------------------------------------------
# Light/dark theme overrides
# Keep this AFTER the original css definition and BEFORE js.
# ---------------------------------------------------------------------------

css <- paste0(
  css,
  "
/* Light-mode defaults */
:root,
[data-bs-theme='light'] {
  --page-bg: #f5f7fa;
  --surface-bg: #ffffff;
  --surface-hover: #f8fbff;
  --field-bg: #ffffff;

  --ink: #19324d;
  --muted: #61748a;
  --border: #dfe7ef;

  --blue: #005baa;
  --selected-bg: #edf5ff;

  --negative: #a52d2d;
  --error-bg: #fff0f0;
  --error-text: #922c2c;
}

/* Dark-mode values */
[data-bs-theme='dark'] {
  --page-bg: #101820;
  --surface-bg: #18232e;
  --surface-hover: #203140;
  --field-bg: #111c26;

  --ink: #edf3f9;
  --muted: #b4c3d2;
  --border: #42576b;

  --blue: #75baff;
  --selected-bg: #19364f;

  --negative: #ffb1b1;
  --error-bg: #482329;
  --error-text: #ffd0d0;

  --bs-body-bg: #101820;
  --bs-body-color: #edf3f9;
  --bs-secondary-color: #b4c3d2;
  --bs-border-color: #42576b;
}

/* Page and navigation */
body {
  background: var(--page-bg) !important;
  color: var(--ink) !important;
}

.navbar {
  background: var(--surface-bg) !important;
  border-color: var(--border) !important;
}

.navbar-brand,
.navbar .nav-link {
  color: var(--ink) !important;
}

.navbar .nav-link.active {
  color: var(--blue) !important;
}

/* Main custom surfaces */
.box,
.metric,
.po-card,
.budget-group,
.rate-line,
.modal-content {
  background: var(--surface-bg) !important;
  color: var(--ink) !important;
  border-color: var(--border) !important;
}

.metric.active {
  background: var(--selected-bg) !important;
  border-color: var(--blue) !important;
}

.po-card:hover {
  background: var(--surface-hover) !important;
  border-color: var(--blue) !important;
}

.po-desc,
.control-label,
.value {
  color: var(--ink) !important;
}

.muted,
.ref,
.metric small,
.rate-line {
  color: var(--muted) !important;
}

.notice {
  background: var(--selected-bg) !important;
  color: var(--ink) !important;
  border-left-color: var(--blue) !important;
}

.negative {
  color: var(--negative) !important;
}

.error-box {
  background: var(--error-bg) !important;
  color: var(--error-text) !important;
}

.modal-header,
.modal-footer {
  border-color: var(--border) !important;
}

/* Inputs and selectize dropdowns */
.form-control,
.form-select,
.selectize-input,
.selectize-dropdown,
.input-group-text {
  background: var(--field-bg) !important;
  color: var(--ink) !important;
  border-color: var(--border) !important;
}

.form-control::placeholder,
.selectize-input input::placeholder {
  color: var(--muted) !important;
  opacity: 1;
}

.selectize-input input,
.selectize-input .item {
  color: var(--ink) !important;
}

.selectize-dropdown .option {
  background: var(--surface-bg) !important;
  color: var(--ink) !important;
}

.selectize-dropdown .active {
  background: var(--selected-bg) !important;
  color: var(--ink) !important;
}

.form-control:focus,
.form-select:focus,
.selectize-input.focus {
  border-color: var(--blue) !important;
  box-shadow: 0 0 0 3px rgba(75, 155, 225, .2) !important;
}

/* Keep the primary action readable in both modes */
.btn-primary {
  background: #005baa !important;
  border-color: #005baa !important;
  color: #ffffff !important;
}

.btn-primary:hover {
  background: #004780 !important;
  border-color: #004780 !important;
}

.btn-outline-primary {
  color: var(--blue);
  border-color: var(--blue);
}

.btn-outline-primary:hover {
  background: var(--selected-bg);
  color: var(--ink);
}

/* Tables, including DT tables */
.table,
table.dataTable {
  --bs-table-bg: var(--surface-bg);
  --bs-table-color: var(--ink);
  --bs-table-border-color: var(--border);
  --bs-table-striped-bg: var(--surface-hover);
  --bs-table-striped-color: var(--ink);
  --bs-table-hover-bg: var(--selected-bg);
  --bs-table-hover-color: var(--ink);

  color: var(--ink) !important;
}

table.dataTable thead th,
table.dataTable tbody td {
  background: var(--surface-bg) !important;
  color: var(--ink) !important;
  border-color: var(--border) !important;
}

table.dataTable tbody tr:hover td {
  background: var(--surface-hover) !important;
}

.dataTables_wrapper,
.dataTables_info,
.dataTables_length,
.dataTables_filter,
.dt-container {
  color: var(--ink) !important;
}

.dataTables_wrapper input,
.dataTables_wrapper select,
.dt-container input,
.dt-container select {
  background: var(--field-bg) !important;
  color: var(--ink) !important;
  border: 1px solid var(--border) !important;
}

/* Manage sub-tabs */
.nav-tabs {
  border-color: var(--border) !important;
}

.nav-tabs .nav-link {
  color: var(--muted) !important;
}

.nav-tabs .nav-link.active {
  background: var(--surface-bg) !important;
  color: var(--blue) !important;
  border-color: var(--border) !important;
}

/* Stage badges: dark-mode contrast */
[data-bs-theme='dark'] .badge-state.under_review {
  background: #193e60;
  color: #c4e4ff;
}

[data-bs-theme='dark'] .badge-state.pending {
  background: #4d3b18;
  color: #ffe2a1;
}

[data-bs-theme='dark'] .badge-state.paid {
  background: #173f2b;
  color: #b9f0cf;
}

[data-bs-theme='dark'] .badge-state.cancelled {
  background: #35414e;
  color: #e1e7ee;
}

/* Notifications */
[data-bs-theme='dark'] .shiny-notification {
  background: #253442;
  color: #edf3f9;
  border-color: #536b81;
}

[data-bs-theme='dark'] .shiny-notification-error {
  background: #482329;
  color: #ffd0d0;
}

[data-bs-theme='dark'] .shiny-notification-warning {
  background: #493a1c;
  color: #ffe3a3;
}

[data-bs-theme='dark'] .shiny-notification-message {
  background: #173f2b;
  color: #b9f0cf;
}

/* Preserve the warning's readability */
.demo {
  color: #4b3900 !important;
}
"
)


js <- "
$(document).on('click','[data-po]',function(){
  Shiny.setInputValue(
    'open_id',
    Number(this.dataset.po),
    {priority:'event'}
  );
});
$(document).on('click','[data-stage]',function(){
  Shiny.setInputValue(
    'stage_click',
    this.dataset.stage,
    {priority:'event'}
  );
});
"

# ---------------------------------------------------------------------------
# Main UI
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = "Logistics Tracker",
  id = "nav",
  selected = "dashboard",
  fillable = FALSE,
  
  theme = bs_theme(
    version = 5,
    primary = "#005baa",
    bg = "#f5f7fa",
    fg = "#19324d"
  ),
  
  header = tagList(
    tags$style(HTML(css)),
    tags$script(HTML(js)),
    
    if (DEMO) {
      #div(
       # class = "demo",
        #paste(
         # "DEMO"
        #)
      #)
    },
    
    uiOutput("rate_banner")
  ),
  
  nav_panel(
    "Dashboard",
    value = "dashboard",
    
    div(
      class = "shell",
      
      div(
        class = "heading",
        
        div(
          h1("Requests and purchase orders"),
          div(
            class = "muted",
            paste(
              "Review requests, progress procurement,",
              "and record paid status."
            )
          )
        ),
        
        actionButton(
          "new_request",
          "+ New request",
          class = "btn-primary btn-lg"
        )
      ),
      
      uiOutput("stage_cards"),
      
      box(
        "Find requests",
        
        div(
          class = "toolbar",
          
          selectInput(
            "d_period",
            "Request period",
            c(
              "This month" = "month",
              "This week" = "week",
              "All time" = "all",
              "Custom" = "custom"
            ),
            selected = "month",
            width = "180px"
          ),
          
          selectInput(
            "d_budget",
            "Budget",
            c("All budgets" = ""),
            width = "220px"
          ),
          
          selectInput(
            "d_line",
            "Budget line",
            c("All lines" = ""),
            width = "220px"
          ),
          
          selectInput(
            "d_currency",
            "Display currency",
            CUR,
            selected = "ETB",
            width = "130px"
          ),
          
          textInput(
            "d_search",
            "Search",
            placeholder = paste(
              "Reference, requester",
              "or description"
            ),
            width = "250px"
          )
        ),
        
        conditionalPanel(
          "input.d_period == 'custom'",
          
          dateRangeInput(
            "d_dates",
            "Request date range",
            start = today() - 30,
            end = today()
          )
        ),
        
        div(
          class = "actions",
          
          actionButton(
            "d_reset",
            "Reset filters"
          ),
          
          downloadButton(
            "dashboard_export",
            "Export filtered requests"
          )
        )
      ),
      
      div(
        class = "heading",
        
        textOutput("list_count"),
        
        div(
          class = "actions",
          
          actionButton(
            "prev",
            "Previous"
          ),
          
          textOutput(
            "page_label",
            inline = TRUE
          ),
          
          actionButton(
            "next_page",
            "Next"
          )
        )
      ),
      
      uiOutput("po_list"),
      
      box(
        "Workload at a glance",
        
        p(
          class = "muted small",
          paste(
            "Counts above show all stages in the selected",
            "request-date scope. The list follows the",
            "selected stage. Paid figures in Financial",
            "report use payment dates, not request dates."
          )
        ),
        
        uiOutput("workload"),
        
        plotOutput(
          "workload_chart",
          height = "180px"
        )
      )
    )
  ),
  
  nav_panel(
    "Financial report",
    value = "finance",
    
    div(
      class = "shell",
      
      div(
        class = "heading",
        
        div(
          h1("Financial report"),
          div(
            class = "muted",
            paste(
              "Paid-only changes for finance.",
              "No budget ceilings or balances are stored."
            )
          )
        )
      ),
      
      div(
        class = "notice",
        paste(
          "Checkpoint workflow: select lines, prepare",
          "an exact batch, export it, update the external",
          "budget workbook, then confirm that batch as",
          "applied. New payments never join an existing",
          "batch automatically."
        )
      ),
      
      div(
        class = "toolbar",
        
        selectInput(
          "f_currency",
          "Report currency",
          CUR,
          selected = "ETB",
          width = "140px"
        ),
        
        selectInput(
          "f_mode",
          "Report view",
          c(
            "Since checkpoint" = "pending",
            "Paid in a period" = "history"
          ),
          width = "220px"
        ),
        
        selectInput(
          "f_budget",
          "Budget",
          c("All budgets" = ""),
          width = "250px"
        )
      ),
      
      conditionalPanel(
        "input.f_mode == 'history'",
        
        dateRangeInput(
          "f_dates",
          "Actual payment dates",
          start = as.Date(
            format(today(), "%Y-%m-01")
          ),
          end = today()
        )
      ),
      
      uiOutput("finance_totals"),
      uiOutput("budget_groups"),
      
      conditionalPanel(
        "input.f_mode == 'pending'",
        
        box(
          "Prepare a checkpoint",
          
          p(
            class = "muted small",
            paste(
              "Select one or several budget lines, or",
              "select all available lines. This selection",
              "is independent of the historical date filter.",
              "Reserved draft items cannot enter another batch."
            )
          ),
          
          selectizeInput(
            "batch_lines",
            "Budget lines to include",
            choices = NULL,
            multiple = TRUE
          ),
          
          textInput(
            "batch_note",
            "Batch note (optional)"
          ),
          
          div(
            class = "actions",
            
            actionButton(
              "select_all_lines",
              "Select all available lines"
            ),
            
            actionButton(
              "prepare_batch",
              "Prepare checkpoint batch",
              class = "btn-primary"
            )
          )
        )
      ),
      
      box(
        "Checkpoint batches",
        
        selectInput(
          "batch_choice",
          "Open batch",
          choices = NULL
        ),
        
        uiOutput("batch_info"),
        DT::DTOutput("batch_table"),
        
        div(
          class = "actions",
          
          downloadButton(
            "batch_export",
            "Export this exact batch"
          ),
          
          actionButton(
            "ask_apply",
            "Confirm externally applied",
            class = "btn-success"
          ),
          
          actionButton(
            "ask_abandon",
            "Abandon draft",
            class = "btn-outline-danger"
          )
        )
      ),
      
      downloadButton(
        "finance_export",
        "Export current report"
      )
    )
  ),
  
  nav_panel(
    "Manage",
    value = "manage",
    
    div(
      class = "shell",
      
      div(
        class = "heading",
        
        div(
          h1("Manage reference lists"),
          
          div(
            class = "muted",
            paste(
              "Codes are stable. Deactivate finished",
              "items instead of deleting historical records."
            )
          )
        )
      ),
      
      box(
        "CBE buying rates",
        
        uiOutput("fx_details"),
        
        actionButton(
          "refresh_fx",
          "Fetch latest available CBE rates",
          class = "btn-primary"
        )
      ),
      
      navset_tab(
        nav_panel(
          "Budgets",
          
          box(
            "Add budget",
            
            div(
              class = "grid2",
              textInput("mb_code", "Budget code *"),
              textInput("mb_name", "Budget name *")
            ),
            
            actionButton(
              "add_budget",
              "Add budget",
              class = "btn-primary"
            )
          ),
          
          DT::DTOutput("budgets_table"),
          
          selectInput(
            "toggle_budget",
            "Budget to activate/deactivate",
            choices = NULL
          ),
          
          actionButton(
            "toggle_budget_btn",
            "Toggle active status"
          )
        ),
        
        nav_panel(
          "Budget lines",
          
          box(
            "Add budget line",
            
            selectInput(
              "ml_budget",
              "Parent budget *",
              choices = NULL
            ),
            
            div(
              class = "grid2",
              textInput("ml_code", "Budget-line code *"),
              textInput("ml_name", "Description *")
            ),
            
            actionButton(
              "add_line",
              "Add budget line",
              class = "btn-primary"
            )
          ),
          
          DT::DTOutput("lines_table"),
          
          selectInput(
            "toggle_line",
            "Line to activate/deactivate",
            choices = NULL
          ),
          
          actionButton(
            "toggle_line_btn",
            "Toggle active status"
          )
        ),
        
        nav_panel(
          "Areas",
          
          box(
            "Add area",
            
            div(
              class = "grid2",
              
              textInput(
                "ma_code",
                "Area code * (e.g. MEK)"
              ),
              
              textInput(
                "ma_name",
                "Area name *"
              ),
              
              textInput(
                "ma_country",
                "Country code *",
                "ETH"
              )
            ),
            
            actionButton(
              "add_area",
              "Add area",
              class = "btn-primary"
            )
          ),
          
          DT::DTOutput("areas_table"),
          
          selectInput(
            "toggle_area",
            "Area to activate/deactivate",
            choices = NULL
          ),
          
          actionButton(
            "toggle_area_btn",
            "Toggle active status"
          )
        ),
        
        nav_panel(
          "People and suppliers",
          
          box(
            "Add a name",
            
            selectInput(
              "mc_kind",
              "List",
              c(
                "Requester" = "requester",
                "Logistics focal point" = "focal",
                "Supplier" = "supplier"
              )
            ),
            
            textInput(
              "mc_name",
              "Name *"
            ),
            
            actionButton(
              "add_contact",
              "Add name",
              class = "btn-primary"
            )
          ),
          
          DT::DTOutput("contacts_table"),
          
          selectInput(
            "toggle_contact",
            "Name to activate/deactivate",
            choices = NULL
          ),
          
          actionButton(
            "toggle_contact_btn",
            "Toggle active status"
          )
        ),
        
        nav_panel(
          "Activity",
          
          p(
            class = "muted small",
            paste(
              "No login: timestamps and actions are",
              "recorded, but the actor is not authenticated."
            )
          ),
          
          DT::DTOutput("audit_table")
        )
      )
    )
  ),
  
  nav_spacer(),
  
  nav_item(
    bslib::input_dark_mode(
      id = "theme_mode",
      mode = "light"
    )
  )
)

# ---------------------------------------------------------------------------
# Shared state
# ---------------------------------------------------------------------------

load_state <- function() {
  cx <- con()
  on.exit(DBI::dbDisconnect(cx))
  
  DBI::dbWithTransaction(
    cx,
    
    list(
      budgets = query(
        "SELECT * FROM budgets ORDER BY code",
        cx = cx
      ),
      
      lines = query(
        paste(
          "SELECT l.*,",
          "b.code AS budget_code,",
          "b.name AS budget_name,",
          "b.active AS budget_active",
          "FROM lines l",
          "JOIN budgets b ON b.id=l.budget_id",
          "ORDER BY b.code,l.code"
        ),
        cx = cx
      ),
      
      areas = query(
        "SELECT * FROM areas ORDER BY code",
        cx = cx
      ),
      
      contacts = query(
        "SELECT * FROM contacts ORDER BY kind,name",
        cx = cx
      ),
      
      requests = query(
        paste(
          "SELECT r.*,",
          "b.name AS budget_name,",
          "l.name AS line_name",
          "FROM requests r",
          "JOIN budgets b ON b.id=r.budget_id",
          "JOIN lines l ON l.id=r.line_id",
          "ORDER BY r.id DESC"
        ),
        cx = cx
      ),
      
      payments = payment_rows(cx),
      
      batches = query(
        "SELECT * FROM batches ORDER BY id DESC",
        cx = cx
      ),
      
      audit = query(
        "SELECT * FROM audit ORDER BY id DESC LIMIT 500",
        cx = cx
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  local_rev <- reactiveVal(0L)
  stage <- reactiveVal("under_review")
  page <- reactiveVal(1L)
  
  form_ctx <- reactiveVal(NULL)
  action_ctx <- reactiveVal(NULL)
  detail_id <- reactiveVal(NULL)
  
  preferred_batch <- reactiveVal(NULL)
  fx_message <- reactiveVal("")
  
  refresh <- function() {
    local_rev(
      isolate(local_rev()) + 1L
    )
  }
  
  poll <- reactivePoll(
    1500,
    session,
    
    checkFunc = function() {
      query(
        "SELECT n FROM revision WHERE id=1"
      )$n[[1]]
    },
    
    valueFunc = load_state
  )
  
  state <- reactive({
    local_rev()
    poll()
    load_state()
  })
  
  rates <- reactive({
    local_rev()
    poll()
    read_fx()
  })
  
  attempt <- function(expr) {
    tryCatch(
      force(expr),
      
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error",
          duration = 12
        )
        
        NULL
      }
    )
  }
  
  contact_choices <- function(kind) {
    d <- isolate(state())$contacts
    
    d$name[
      d$kind == kind &
        d$active == 1
    ]
  }
  
  select_preserve <- function(
    id,
    ch,
    default = ""
  ) {
    old <- isolate(s(input[[id]]))
    
    if (!old %in% unname(ch)) {
      old <- default
    }
    
    updateSelectInput(
      session,
      id,
      choices = ch,
      selected = old
    )
  }
  
  # -------------------------------------------------------------------------
  # Reference-list dropdowns
  # -------------------------------------------------------------------------
  
  observe({
    z <- state()
    
    b <- z$budgets
    l <- z$lines
    a <- z$areas
    contacts <- z$contacts
    
    for (id in c("d_budget", "f_budget")) {
      select_preserve(
        id,
        c(
          "All budgets" = "",
          choices(b)
        )
      )
    }
    
    select_preserve(
      "ml_budget",
      choices(
        b[b$active == 1, , drop = FALSE]
      )
    )
    
    select_preserve(
      "toggle_budget",
      choices(b)
    )
    
    lc <- setNames(
      as.character(l$id),
      
      paste(
        l$budget_code,
        l$code,
        l$name,
        sep = " / "
      )
    )
    
    select_preserve("toggle_line", lc)
    select_preserve("toggle_area", choices(a))
    
    cc <- setNames(
      as.character(contacts$id),
      
      paste(
        contacts$kind,
        contacts$name,
        sep = " / "
      )
    )
    
    select_preserve(
      "toggle_contact",
      cc
    )
  })
  
  observe({
    l <- state()$lines
    b <- s(input$d_budget)
    
    if (nzchar(b)) {
      l <- l[
        l$budget_id == as.integer(b),
        ,
        drop = FALSE
      ]
    }
    
    ch <- setNames(
      as.character(l$id),
      
      paste(
        l$budget_code,
        l$code,
        l$name,
        sep = " / "
      )
    )
    
    select_preserve(
      "d_line",
      c(
        "All lines" = "",
        ch
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # CBE refresh and rate information
  # -------------------------------------------------------------------------
  
  last_attempt <- as.POSIXct(
    "1970-01-01",
    tz = "UTC"
  )
  
  observe({
    invalidateLater(60000, session)
    
    f <- isolate(read_fx())
    
    needs_refresh <- is.null(f) ||
      f$date != as.character(today())
    
    elapsed <- as.numeric(
      difftime(
        Sys.time(),
        last_attempt,
        units = "mins"
      )
    )
    
    if (
      !DEMO &&
      needs_refresh &&
      elapsed >= 30
    ) {
      last_attempt <<- Sys.time()
      
      result <- tryCatch(
        fetch_fx(),
        error = function(e) e
      )
      
      if (inherits(result, "error")) {
        fx_message(
          conditionMessage(result)
        )
      } else {
        fx_message("")
        refresh()
      }
    }
  })
  
  observeEvent(input$refresh_fx, {
    attempt({
      f <- fetch_fx()
      
      fx_message("")
      refresh()
      
      showNotification(
        if (DEMO) {
          "Demo rates reloaded."
        } else {
          paste(
            "CBE rates loaded:",
            f$date
          )
        },
        type = "message"
      )
    })
  })
  
  output$rate_banner <- renderUI({
    f <- rates()
    
    if (is.null(f)) {
      return(
        div(
          class = "rate-line",
          paste(
            "No CBE rates available. Open Manage to refresh.",
            "New requests and paid transitions require",
            "a valid rate set."
          )
        )
      )
    }
    
    div(
      class = "rate-line",
      
      paste(
        if (DEMO) {
          "DEMO rates"
        } else {
          "CBE buying rates"
        },
        
        f$date,
        "|",
        f$field,
        
        if (f$date != as.character(today())) {
          "| Latest available, not today"
        } else {
          "| Today"
        }
      )
    )
  })
  
  output$fx_details <- renderUI({
    f <- rates()
    
    tagList(
      if (nzchar(fx_message())) {
        div(
          class = "error-box",
          fx_message()
        )
      },
      
      if (is.null(f)) {
        p("No complete rate set is cached.")
      } else {
        tagList(
          p(
            paste(
              "Effective date:",
              f$date,
              "| Source:",
              f$source
            )
          ),
          
          tags$table(
            class = "table table-sm",
            
            tags$thead(
              tags$tr(
                tags$th("Currency"),
                tags$th("ETB per one unit")
              )
            ),
            
            tags$tbody(
              lapply(CUR, function(k) {
                tags$tr(
                  tags$td(k),
                  
                  tags$td(
                    format(
                      f$rates[[k]],
                      digits = 10
                    )
                  )
                )
              })
            )
          ),
          
          p(
            class = "muted small",
            paste(
              "Conversions use amount × ETB-per-source",
              "÷ ETB-per-target. Requests save their entry",
              "rates; paid events save their own rate snapshot",
              "when marked paid. Checkpoint exports never",
              "revalue old events."
            )
          )
        )
      }
    )
  })
  
  # -------------------------------------------------------------------------
  # Dashboard filters
  # -------------------------------------------------------------------------
  
  dashboard_base <- reactive({
    d <- state()$requests
    
    period <- s(input$d_period)
    from <- as.Date(NA)
    to <- today()
    
    if (period == "month") {
      from <- as.Date(
        format(today(), "%Y-%m-01")
      )
    }
    
    if (period == "week") {
      from <- today() -
        (
          as.integer(
            format(today(), "%u")
          ) - 1L
        )
    }
    
    if (period == "custom") {
      dr <- input$d_dates
      
      if (
        length(dr) != 2L ||
        anyNA(dr) ||
        dr[[1]] > dr[[2]]
      ) {
        return(
          d[FALSE, , drop = FALSE]
        )
      }
      
      from <- dr[[1]]
      to <- dr[[2]]
    }
    
    if (
      period != "all" &&
      !is.na(from)
    ) {
      d <- d[
        d$request_date >= as.character(from) &
          d$request_date <= as.character(to),
        ,
        drop = FALSE
      ]
    }
    
    if (nzchar(s(input$d_budget))) {
      d <- d[
        d$budget_id == as.integer(input$d_budget),
        ,
        drop = FALSE
      ]
    }
    
    if (nzchar(s(input$d_line))) {
      d <- d[
        d$line_id == as.integer(input$d_line),
        ,
        drop = FALSE
      ]
    }
    
    q <- tolower(s(input$d_search))
    
    if (nzchar(q) && nrow(d)) {
      hay <- apply(
        d[
          ,
          c(
            "request_ref",
            "po_ref",
            "description",
            "requester",
            "focal",
            "budget_code",
            "line_code"
          ),
          drop = FALSE
        ],
        1,
        paste,
        collapse = " "
      )
      
      d <- d[
        grepl(
          q,
          tolower(hay),
          fixed = TRUE
        ),
        ,
        drop = FALSE
      ]
    }
    
    d
  })
  
  listed <- reactive({
    d <- dashboard_base()
    
    d[
      d$status == stage(),
      ,
      drop = FALSE
    ]
  })
  
  observeEvent(input$stage_click, {
    if (
      s(input$stage_click) %in% unname(STAGES)
    ) {
      stage(input$stage_click)
      page(1L)
    }
  })
  
  observeEvent(
    list(
      input$d_period,
      input$d_dates,
      input$d_budget,
      input$d_line,
      input$d_search
    ),
    
    page(1L),
    
    ignoreInit = TRUE
  )
  
  observeEvent(input$prev, {
    page(
      max(
        1L,
        page() - 1L
      )
    )
  })
  
  observeEvent(input$next_page, {
    page(
      min(
        max(
          1L,
          ceiling(nrow(listed()) / 10)
        ),
        page() + 1L
      )
    )
  })
  
  observeEvent(input$d_reset, {
    updateSelectInput(
      session,
      "d_period",
      selected = "month"
    )
    
    updateSelectInput(
      session,
      "d_budget",
      selected = ""
    )
    
    updateSelectInput(
      session,
      "d_line",
      selected = ""
    )
    
    updateSelectInput(
      session,
      "d_currency",
      selected = "ETB"
    )
    
    updateTextInput(
      session,
      "d_search",
      value = ""
    )
    
    stage("under_review")
    page(1L)
  })
  
  # -------------------------------------------------------------------------
  # Dashboard cards and list
  # -------------------------------------------------------------------------
  
  output$stage_cards <- renderUI({
    d <- dashboard_base()
    
    div(
      class = "metrics",
      
      lapply(
        unname(STAGES),
        
        function(st) {
          tags$button(
            type = "button",
            
            class = paste(
              "metric",
              if (stage() == st) {
                "active"
              } else {
                ""
              }
            ),
            
            `data-stage` = st,
            
            `aria-pressed` = if (stage() == st) {
              "true"
            } else {
              "false"
            },
            
            span(label(st)),
            strong(sum(d$status == st)),
            shiny::tags$small("requests")
          )
        }
      )
    )
  })
  
  output$list_count <- renderText({
    paste(
      label(stage()),
      "-",
      nrow(listed()),
      "matching records. Newest first."
    )
  })
  
  output$page_label <- renderText({
    pages <- max(
      1L,
      ceiling(nrow(listed()) / 10)
    )
    
    paste(
      "Page",
      min(page(), pages),
      "of",
      pages
    )
  })
  
  display_amount <- function(
    r,
    cur
  ) {
    if (r$status[[1]] == "paid") {
      p <- state()$payments
      
      p <- p[
        p$request_id == r$id[[1]],
        ,
        drop = FALSE
      ]
      
      if (nrow(p)) {
        return(p[[paste0(tolower(cur), "_minor")]][[1]])
      }
    }
    
    converted(
      r$amount_minor[[1]],
      r$currency[[1]],
      cur,
      request_fx(r)
    )
  }
  
  output$po_list <- renderUI({
    d <- listed()
    cur <- currency_value(input$d_currency)
    
    if (!nrow(d)) {
      return(
        div(
          class = "box empty",
          paste(
            "No requests in this stage and period.",
            "Try All time or another stage."
          )
        )
      )
    }
    
    pg <- min(
      page(),
      max(
        1L,
        ceiling(nrow(d) / 10)
      )
    )
    
    d <- d[
      seq.int(
        (pg - 1) * 10 + 1,
        min(pg * 10, nrow(d))
      ),
      ,
      drop = FALSE
    ]
    
    tagList(
      lapply(
        seq_len(nrow(d)),
        
        function(i) {
          r <- d[i, , drop = FALSE]
          
          tags$button(
            type = "button",
            class = "po-card",
            `data-po` = r$id[[1]],
            
            div(
              class = "po-top",
              
              div(
                span(
                  class = "po-person",
                  r$requester
                ),
                
                stage_badge(r$status)
              ),
              
              span(
                class = "po-amount",
                
                paste(
                  cur,
                  fmt(
                    display_amount(r, cur)
                  )
                )
              )
            ),
            
            div(
              class = "po-desc",
              r$description
            ),
            
            div(
              class = "muted small",
              
              paste(
                r$budget_code,
                r$budget_name,
                "/",
                r$line_code,
                "|",
                r$area_name,
                "|",
                r$request_date
              )
            ),
            
            div(
              class = "ref",
              
              if (nzchar(s(r$po_ref))) {
                r$po_ref
              } else {
                r$request_ref
              }
            )
          )
        }
      )
    )
  })
  
  output$workload <- renderUI({
    d <- dashboard_base()
    
    open <- d[
      d$status %in% c(
        "under_review",
        "pending"
      ),
      ,
      drop = FALSE
    ]
    
    if (!nrow(open)) {
      return(
        p(
          class = "muted",
          "No open requests in this scope."
        )
      )
    }
    
    oldest <- min(
      as.Date(open$request_date)
    )
    
    tagList(
      p(
        paste(
          nrow(open),
          "open requests. Oldest request:",
          format(oldest, "%d %b %Y"),
          
          paste0(
            "(",
            as.integer(today() - oldest),
            " days since request)."
          )
        )
      ),
      
      p(
        class = "muted small",
        "No arbitrary overdue thresholds are applied."
      )
    )
  })
  
  output$workload_chart <- renderPlot({
    
    dark <- identical(
      input$theme_mode,
      "dark"
    )
    
    background <- if (dark) {
      "#18232e"
    } else {
      "#ffffff"
    }
    
    foreground <- if (dark) {
      "#edf3f9"
    } else {
      "#19324d"
    }
    
    bar_color <- if (dark) {
      "#75baff"
    } else {
      "#005baa"
    }
    
    old_par <- par(
      bg = background,
      fg = foreground,
      col.axis = foreground,
      col.lab = foreground,
      col.main = foreground,
      mar = c(3, 7, 1, 1)
    )
    
    on.exit(
      par(old_par),
      add = TRUE
    )
    
    d <- dashboard_base()
    
    d <- d[
      d$status %in% c(
        "under_review",
        "pending"
      ),
      ,
      drop = FALSE
    ]
    
    if (!nrow(d)) {
      
      plot.new()
      
      text(
        0.5,
        0.5,
        "No open workload in this scope",
        col = foreground
      )
      
      return()
    }
    
    counts <- sort(
      table(d$budget_code),
      decreasing = TRUE
    )
    
    counts <- head(
      counts,
      8
    )
    
    barplot(
      rev(counts),
      horiz = TRUE,
      col = bar_color,
      border = NA,
      las = 1,
      xlab = "Open requests by budget (top 8)",
      cex.names = 0.85,
      col.axis = foreground,
      col.lab = foreground
    )
    
  }, res = 96)
  
  # -------------------------------------------------------------------------
  # New/edit request modal
  # -------------------------------------------------------------------------
  
  form_error <- reactiveVal("")
  
  show_form <- function(r = NULL) {
    z <- isolate(state())
    
    b <- z$budgets
    b <- b[b$active == 1, , drop = FALSE]
    
    a <- z$areas
    a <- a[a$active == 1, , drop = FALSE]
    
    has_lines <- any(
      z$lines$active == 1 &
        z$lines$budget_active == 1
    )
    
    if (
      !nrow(b) ||
      !nrow(a) ||
      !has_lines
    ) {
      stop(
        paste(
          "Add an active budget, budget line",
          "and area in Manage first."
        )
      )
    }
    
    f <- fx_ready(
      isolate(rates())
    )
    
    get <- function(
    k,
    default = ""
    ) {
      if (is.null(r)) {
        default
      } else {
        scalar(r, k, default)
      }
    }
    
    edit <- !is.null(r)
    
    if (
      edit &&
      !r$status[[1]] %in% c(
        "under_review",
        "pending"
      )
    ) {
      stop("This request is locked.")
    }
    
    ctx <- list(
      edit = if (edit) {
        list(
          id = r$id[[1]],
          version = r$version[[1]]
        )
      } else {
        NULL
      },
      
      original = r,
      fx = f
    )
    
    form_ctx(ctx)
    action_ctx(NULL)
    
    l <- z$lines
    
    l <- l[
      l$active == 1 &
        l$budget_active == 1,
      ,
      drop = FALSE
    ]
    
    bid <- if (edit) {
      as.character(r$budget_id[[1]])
    } else {
      ""
    }
    
    lc <- l[
      l$budget_id %in% as.integer(bid),
      ,
      drop = FALSE
    ]
    
    reqs <- contact_choices("requester")
    focals <- contact_choices("focal")
    
    showModal(
      modalDialog(
        title = if (edit) {
          paste(
            "Edit request #",
            r$id[[1]]
          )
        } else {
          "New request"
        },
        
        size = "l",
        easyClose = FALSE,
        
        p(
          class = "muted small",
          paste(
            "* Required. One request belongs to one budget",
            "line. Amount is the total, not a unit price.",
            "New requests start Under review."
          )
        ),
        
        uiOutput("request_errors"),
        
        box(
          "1. Request details",
          
          div(
            class = "grid2",
            
            div(
              class = "full",
              
              textAreaInput(
                "r_description",
                "Description *",
                get("description"),
                rows = 3
              )
            ),
            
            selectizeInput(
              "r_requester",
              "Requester *",
              
              choices = unique(
                c(
                  "",
                  reqs,
                  get("requester")
                )
              ),
              
              selected = get("requester"),
              options = list(create = TRUE)
            ),
            
            selectizeInput(
              "r_focal",
              "Logistics focal point *",
              
              choices = unique(
                c(
                  "",
                  focals,
                  get("focal")
                )
              ),
              
              selected = get("focal"),
              options = list(create = TRUE)
            ),
            
            selectInput(
              "r_budget",
              "Budget *",
              c(
                "Select" = "",
                choices(b)
              ),
              selected = bid
            ),
            
            selectInput(
              "r_line",
              "Budget line *",
              c(
                "Select" = "",
                choices(lc)
              ),
              selected = as.character(
                get("line_id", "")
              )
            ),
            
            selectInput(
              "r_area",
              "Area *",
              c(
                "Select" = "",
                choices(a)
              ),
              selected = as.character(
                get("area_id", "")
              )
            ),
            
            selectInput(
              "r_department",
              "Department *",
              c(
                "Select" = "",
                DEPTS
              ),
              selected = get("department")
            ),
            
            blank_date(
              "r_date",
              "Request submission date *",
              if (edit) {
                read_date(r$request_date)
              } else {
                today()
              }
            ),
            
            blank_date(
              "r_refdate",
              "Date used in request reference *",
              if (edit) {
                read_date(r$reference_date)
              } else {
                today()
              }
            ),
            
            blank_date(
              "r_processing",
              "Processing date",
              if (edit) {
                read_date(r$processing_date)
              } else {
                NA
              }
            ),
            
            blank_date(
              "r_finance",
              "Submitted to finance date (optional)",
              if (edit) {
                read_date(r$finance_date)
              } else {
                NA
              }
            ),
            
            numericInput(
              "r_quantity",
              "Quantity (optional)",
              if (edit) {
                r$quantity[[1]]
              } else {
                NA
              },
              min = 0
            ),
            
            textInput(
              "r_unit",
              "Unit (e.g. Days)",
              get("unit")
            )
          )
        ),
        
        box(
          "2. Amount and procurement information",
          
          div(
            class = "grid2",
            
            numericInput(
              "r_amount",
              "Total request amount *",
              if (edit) {
                r$amount_minor[[1]] / 100
              } else {
                NA
              },
              min = 0,
              step = .01
            ),
            
            selectInput(
              "r_currency",
              "Currency *",
              CUR,
              selected = get("currency", "ETB")
            ),
            
            selectInput(
              "r_scenario",
              "Procurement scenario (manual)",
              SCENARIOS,
              selected = get("scenario")
            ),
            
            textInput(
              "r_waiver",
              "Waiver / memo",
              get("waiver", "No")
            )
          )
        ),
        
        if (
          edit &&
          r$status[[1]] == "pending"
        ) {
          box(
            "Supplier",
            
            selectizeInput(
              "r_supplier",
              "Supplier *",
              
              choices = unique(
                c(
                  "",
                  contact_choices("supplier"),
                  s(r$supplier)
                )
              ),
              
              selected = s(r$supplier),
              options = list(create = TRUE)
            ),
            
            p(
              class = "ref",
              r$po_ref
            )
          )
        },
        
        box(
          "3. Remarks",
          
          div(
            class = "grid2",
            
            textAreaInput(
              "r_remarks",
              "Logistics remarks",
              get("remarks"),
              rows = 2
            ),
            
            textAreaInput(
              "r_program",
              "Program / other staff remarks",
              get("program_remarks"),
              rows = 2
            )
          )
        ),
        
        if (edit) {
          div(
            class = "notice",
            paste(
              "Issued reference and its budget/line/area/",
              "department/date classification are fixed.",
              "To correct that classification, cancel and",
              "create a replacement. Amount and other",
              "details can change before Paid."
            )
          )
        },
        
        div(
          class = "notice",
          
          paste(
            if (edit) {
              paste(
                "Available rates (unchanged amounts retain",
                "their saved entry rates):"
              )
            } else {
              "Rate snapshot:"
            },
            
            f$date,
            "|",
            f$source
          )
        ),
        
        if (
          f$date != as.character(today())
        ) {
          checkboxInput(
            "r_accept_old",
            paste(
              "Use this explicitly dated",
              "latest-available rate set"
            ),
            FALSE
          )
        },
        
        footer = tagList(
          modalButton("Discard / close"),
          
          actionButton(
            "save_request",
            "Save request",
            class = "btn-primary"
          )
        )
      )
    )
  }
  
  output$request_errors <- renderUI({
    if (nzchar(form_error())) {
      div(
        class = "error-box",
        form_error()
      )
    }
  })
  
  observeEvent(input$new_request, {
    attempt({
      form_error("")
      show_form()
    })
  })
  
  observeEvent(input$r_budget, {
    ctx <- form_ctx()
    req(!is.null(ctx))
    
    l <- isolate(state())$lines
    
    l <- l[
      l$active == 1 &
        l$budget_active == 1 &
        l$budget_id %in% as.integer(input$r_budget),
      ,
      drop = FALSE
    ]
    
    old <- isolate(s(input$r_line))
    orig <- ctx$original
    
    if (!old %in% as.character(l$id)) {
      old <- if (
        !is.null(orig) &&
        orig$line_id[[1]] %in% l$id
      ) {
        as.character(orig$line_id[[1]])
      } else {
        ""
      }
    }
    
    updateSelectInput(
      session,
      "r_line",
      
      choices = c(
        "Select" = "",
        choices(l)
      ),
      
      selected = old
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$save_request, {
    ctx <- form_ctx()
    
    if (is.null(ctx)) {
      return()
    }
    
    result <- tryCatch({
      if (
        ctx$fx$date != as.character(today()) &&
        !isTRUE(input$r_accept_old)
      ) {
        stop(
          "Acknowledge the dated rate set before saving."
        )
      }
      
      required <- c(
        "r_description",
        "r_requester",
        "r_focal",
        "r_budget",
        "r_line",
        "r_area",
        "r_department"
      )
      
      for (k in required) {
        if (!nzchar(s(input[[k]]))) {
          stop(
            "Complete all required request fields."
          )
        }
      }
      
      if (
        !s(input$r_department) %in% DEPTS ||
        !s(input$r_currency) %in% CUR ||
        !s(input$r_scenario) %in% unname(SCENARIOS)
      ) {
        stop(
          "Invalid department, currency or scenario."
        )
      }
      
      q <- if (num(input$r_quantity)) {
        input$r_quantity
      } else {
        NA_real_
      }
      
      if (!is.na(q) && q <= 0) {
        stop("Quantity must be positive.")
      }
      
      if (
        xor(
          !is.na(q),
          nzchar(s(input$r_unit))
        )
      ) {
        stop(
          paste(
            "Enter quantity and unit together,",
            "or leave both blank."
          )
        )
      }
      
      dt <- date_text(
        input$r_date,
        TRUE
      )
      
      rd <- date_text(
        input$r_refdate,
        TRUE
      )
      
      if (
        as.Date(dt) > today() ||
        as.Date(rd) > today()
      ) {
        stop(
          "Request/reference dates cannot be in the future."
        )
      }
      
      v <- list(
        budget_id = as.integer(input$r_budget),
        line_id = as.integer(input$r_line),
        area_id = as.integer(input$r_area),
        department = s(input$r_department),
        description = s(input$r_description),
        requester = s(input$r_requester),
        focal = s(input$r_focal),
        request_date = dt,
        reference_date = rd,
        
        processing_date = date_text(
          input$r_processing
        ),
        
        finance_date = date_text(
          input$r_finance
        ),
        
        quantity = q,
        unit = s(input$r_unit),
        amount_minor = minor(input$r_amount),
        currency = s(input$r_currency),
        scenario = s(input$r_scenario),
        waiver = s(input$r_waiver),
        remarks = s(input$r_remarks),
        program_remarks = s(input$r_program)
      )
      
      if (
        !is.null(ctx$original) &&
        ctx$original$status[[1]] == "pending"
      ) {
        if (!nzchar(s(input$r_supplier))) {
          stop(
            "Supplier is required for pending procurement."
          )
        }
        
        v$supplier <- s(input$r_supplier)
      }
      
      create_request(
        v,
        ctx$fx,
        ctx$edit
      )
    }, error = function(e) e)
    
    if (inherits(result, "error")) {
      form_error(
        conditionMessage(result)
      )
      
      return()
    }
    
    form_ctx(NULL)
    form_error("")
    
    removeModal()
    refresh()
    
    stage(
      if (is.null(ctx$original)) {
        "under_review"
      } else {
        ctx$original$status[[1]]
      }
    )
    
    page(1L)
    
    showNotification(
      paste(
        "Request #",
        result,
        "saved."
      ),
      type = "message"
    )
  })
  
  # -------------------------------------------------------------------------
  # Request detail modal
  # -------------------------------------------------------------------------
  
  show_details <- function(id) {
    r <- query(
      "SELECT * FROM requests WHERE id=?",
      list(as.integer(id))
    )
    
    if (!nrow(r)) {
      stop("Request no longer exists.")
    }
    
    detail_id(r$id[[1]])
    form_ctx(NULL)
    action_ctx(NULL)
    
    payments <- state()$payments
    
    payments <- payments[
      payments$request_id == id,
      ,
      drop = FALSE
    ]
    
    showModal(
      modalDialog(
        title = paste("Request #", id),
        size = "l",
        easyClose = TRUE,
        
        stage_badge(r$status),
        
        p(
          class = "ref",
          r$request_ref
        ),
        
        if (nzchar(s(r$po_ref))) {
          p(
            class = "ref",
            r$po_ref
          )
        },
        
        div(
          class = "detail-grid",
          
          info("Description", r$description),
          
          info(
            "Original total",
            paste(
              r$currency,
              fmt(r$amount_minor)
            )
          ),
          
          info("Requester", r$requester),
          info("Focal point", r$focal),
          
          info(
            "Budget / line",
            paste(
              r$budget_code,
              r$line_code,
              sep = " / "
            )
          ),
          
          info(
            "Area",
            paste(
              r$area_name,
              r$area_code,
              r$country,
              sep = " / "
            )
          ),
          
          info("Department", r$department),
          info("Request date", r$request_date),
          info("Reference date", r$reference_date),
          info("Processing date", r$processing_date),
          
          info(
            "Quantity / unit",
            if (is.na(r$quantity[[1]])) {
              ""
            } else {
              paste(
                r$quantity,
                r$unit
              )
            }
          ),
          
          info("Scenario (entered)", r$scenario),
          info("Waiver / memo", r$waiver),
          info("Supplier", r$supplier),
          info("PO reason", r$po_reason),
          info("PO date", r$po_date),
          info("Submitted to finance", r$finance_date),
          info("Payment date", r$payment_date),
          info("Logistics remarks", r$remarks),
          info("Program remarks", r$program_remarks),
          info("Cancellation reason", r$cancel_reason),
          info("Entry-rate date", r$fx_date),
          info("Entry-rate source", r$fx_source)
        ),
        
        if (nrow(payments)) {
          div(
            class = "notice",
            
            paste(
              "Paid snapshot:",
              payments$fx_date,
              "|",
              payments$fx_source,
              "| Record locked."
            )
          )
        },
        
        tags$hr(),
        
        div(
          class = "actions",
          
          if (
            r$status[[1]] %in% c(
              "under_review",
              "pending"
            )
          ) {
            actionButton(
              "edit_details",
              "Edit details",
              class = "btn-outline-primary"
            )
          },
          
          if (
            r$status[[1]] == "under_review"
          ) {
            actionButton(
              "ask_approve",
              "Proceed with purchase order",
              class = "btn-success btn-lg"
            )
          },
          
          if (
            r$status[[1]] == "pending"
          ) {
            actionButton(
              "ask_paid",
              "Mark paid",
              class = "btn-success btn-lg"
            )
          },
          
          if (
            r$status[[1]] %in% c(
              "under_review",
              "pending"
            )
          ) {
            actionButton(
              "ask_cancel",
              "Cancel request",
              class = "btn-danger btn-lg"
            )
          },
          
          if (
            r$status[[1]] %in% c(
              "paid",
              "cancelled"
            )
          ) {
            span(
              class = "muted",
              "This record is locked."
            )
          }
        ),
        
        footer = modalButton("Close")
      )
    )
  }
  
  observeEvent(input$open_id, {
    attempt(
      show_details(
        as.integer(input$open_id)
      )
    )
  })
  
  observeEvent(input$edit_details, {
    attempt({
      id <- detail_id()
      req(!is.null(id))
      
      r <- query(
        "SELECT * FROM requests WHERE id=?",
        list(id)
      )
      
      form_error("")
      show_form(r)
    })
  })
  
  # -------------------------------------------------------------------------
  # Confirmation modals
  # -------------------------------------------------------------------------
  
  open_action <- function(action) {
    id <- detail_id()
    req(!is.null(id))
    
    r <- query(
      "SELECT * FROM requests WHERE id=?",
      list(id)
    )
    
    if (!nrow(r)) {
      stop("Request not found.")
    }
    
    if (
      action == "approve" &&
      r$status[[1]] != "under_review"
    ) {
      stop("Request is no longer under review.")
    }
    
    if (
      action == "paid" &&
      r$status[[1]] != "pending"
    ) {
      stop("Request is no longer pending.")
    }
    
    if (
      action == "cancel" &&
      !r$status[[1]] %in% c(
        "under_review",
        "pending"
      )
    ) {
      stop("Request is locked.")
    }
    
    f <- if (action == "paid") {
      fx_ready(
        isolate(rates())
      )
    } else {
      NULL
    }
    
    action_ctx(
      list(
        id = id,
        version = r$version[[1]],
        action = action,
        fx = f
      )
    )
    
    suppliers <- contact_choices("supplier")
    
    showModal(
      modalDialog(
        title = switch(
          action,
          approve = "Confirm procurement approval",
          paid = "Confirm marked paid",
          cancel = "Confirm cancellation"
        ),
        
        size = "m",
        easyClose = FALSE,
        
        p(
          class = "ref",
          r$request_ref
        ),
        
        if (action == "approve") {
          tagList(
            selectizeInput(
              "a_supplier",
              "Supplier *",
              choices = c("", suppliers),
              options = list(create = TRUE)
            ),
            
            textInput(
              "a_reason",
              "PO reason * (e.g. Vehicle Rent)"
            ),
            
            blank_date(
              "a_po_date",
              "PO reference date *",
              today()
            ),
            
            p(
              class = "muted small",
              paste(
                "A stable PO/OF/.../reason/date reference",
                "is generated when you confirm."
              )
            )
          )
        },
        
        if (action == "paid") {
          tagList(
            p(
              strong(
                paste(
                  "Original amount:",
                  r$currency,
                  fmt(r$amount_minor)
                )
              )
            ),
            
            blank_date(
              "a_payment_date",
              "Payment date *",
              today()
            ),
            
            p(
              paste(
                "This only marks the request Paid in this",
                "tracker. It does not execute a payment or",
                "require external finance confirmation."
              )
            ),
            
            p(
              strong(
                paste(
                  "The record will be locked and included",
                  "in the next finance checkpoint."
                )
              )
            ),
            
            div(
              class = "notice",
              
              paste(
                "Conversion snapshot:",
                f$date,
                "|",
                f$source
              )
            ),
            
            if (
              f$date != as.character(today())
            ) {
              checkboxInput(
                "a_old_rates",
                paste(
                  "Use this explicitly dated",
                  "latest-available rate set"
                ),
                FALSE
              )
            }
          )
        },
        
        if (action == "cancel") {
          textAreaInput(
            "a_cancel_reason",
            "Cancellation reason *",
            rows = 2
          )
        },
        
        checkboxInput(
          "a_confirm",
          "I confirm this action",
          FALSE
        ),
        
        footer = tagList(
          modalButton("Back / close"),
          
          actionButton(
            "confirm_action",
            "Confirm",
            
            class = if (action == "cancel") {
              "btn-danger"
            } else {
              "btn-success"
            }
          )
        )
      )
    )
  }
  
  observeEvent(input$ask_approve, {
    attempt(open_action("approve"))
  })
  
  observeEvent(input$ask_paid, {
    attempt(open_action("paid"))
  })
  
  observeEvent(input$ask_cancel, {
    attempt(open_action("cancel"))
  })
  
  observeEvent(input$confirm_action, {
    ctx <- action_ctx()
    
    if (is.null(ctx)) {
      return()
    }
    
    attempt({
      if (!isTRUE(input$a_confirm)) {
        stop("Tick the confirmation box.")
      }
      
      if (
        ctx$action == "paid" &&
        ctx$fx$date != as.character(today()) &&
        !isTRUE(input$a_old_rates)
      ) {
        stop("Acknowledge the dated rate set.")
      }
      
      vals <- switch(
        ctx$action,
        
        approve = list(
          supplier = s(input$a_supplier),
          po_reason = s(input$a_reason),
          po_date = input$a_po_date
        ),
        
        paid = list(
          payment_date = input$a_payment_date
        ),
        
        cancel = list(
          reason = s(input$a_cancel_reason)
        )
      )
      
      transition(
        ctx$id,
        ctx$version,
        ctx$action,
        vals,
        ctx$fx
      )
      
      action_ctx(NULL)
      removeModal()
      refresh()
      
      stage(
        switch(
          ctx$action,
          approve = "pending",
          paid = "paid",
          cancel = "cancelled"
        )
      )
      
      page(1L)
      
      showNotification(
        "Request updated.",
        type = "message"
      )
    })
  })
  
  # -------------------------------------------------------------------------
  # Finance reporting
  # -------------------------------------------------------------------------
  
  finance_payments <- reactive({
    p <- state()$payments
    
    if (nzchar(s(input$f_budget))) {
      p <- p[
        p$budget_id == as.integer(input$f_budget),
        ,
        drop = FALSE
      ]
    }
    
    if (
      identical(
        input$f_mode,
        "history"
      )
    ) {
      dr <- input$f_dates
      
      if (
        length(dr) != 2L ||
        anyNA(dr) ||
        dr[[1]] > dr[[2]]
      ) {
        return(
          p[FALSE, , drop = FALSE]
        )
      }
      
      p <- p[
        p$payment_date >= as.character(dr[[1]]) &
          p$payment_date <= as.character(dr[[2]]),
        ,
        drop = FALSE
      ]
    } else {
      p <- p[
        is.na(p$batch_status) |
          p$batch_status != "applied",
        ,
        drop = FALSE
      ]
    }
    
    p
  })
  
  report_col <- reactive({
    k <- tolower(
      currency_value(input$f_currency)
    )
    
    paste0(k, "_minor")
  })
  
  last_checkpoint <- function(
    line_ids,
    p
  ) {
    z <- p[
      p$line_id %in% line_ids &
        !is.na(p$batch_status) &
        p$batch_status == "applied",
      ,
      drop = FALSE
    ]
    
    if (!nrow(z)) {
      "Never"
    } else {
      max(z$applied_at)
    }
  }
  
  output$finance_totals <- renderUI({
    p <- finance_payments()
    col <- report_col()
    
    reserved <- p[
      !is.na(p$batch_status) &
        p$batch_status == "draft",
      ,
      drop = FALSE
    ]
    
    div(
      class = "box",
      
      h2(
        paste(
          if (
            identical(
              input$f_mode,
              "history"
            )
          ) {
            "Paid in selected period:"
          } else {
            "Unapplied change:"
          },
          
          currency_value(input$f_currency),
          fmt(-sum(p[[col]]))
        )
      ),
      
      p(
        class = "muted small",
        
        paste(
          nrow(p),
          "paid records.",
          
          if (
            !identical(
              input$f_mode,
              "history"
            )
          ) {
            paste(
              nrow(reserved),
              "already reserved in draft batches;",
              "drafts remain unapplied until confirmed."
            )
          } else {
            paste(
              "Historical totals include both",
              "checkpointed and uncheckpointed payments."
            )
          }
        )
      )
    )
  })
  
  output$budget_groups <- renderUI({
    z <- state()
    
    b <- z$budgets
    l <- z$lines
    
    p <- finance_payments()
    allp <- z$payments
    
    col <- report_col()
    cur <- currency_value(input$f_currency)
    
    if (nzchar(s(input$f_budget))) {
      b <- b[
        b$id == as.integer(input$f_budget),
        ,
        drop = FALSE
      ]
    }
    
    if (!nrow(b)) {
      return(
        div(
          class = "empty",
          "Add budgets and budget lines in Manage."
        )
      )
    }
    
    tagList(
      lapply(
        seq_len(nrow(b)),
        
        function(i) {
          bid <- b$id[[i]]
          
          ls <- l[
            l$budget_id == bid,
            ,
            drop = FALSE
          ]
          
          bp <- p[
            p$budget_id == bid,
            ,
            drop = FALSE
          ]
          
          tags$details(
            class = "budget-group",
            
            tags$summary(
              span(
                paste(
                  b$code[[i]],
                  b$name[[i]]
                )
              ),
              
              span(
                class = "negative",
                
                paste(
                  cur,
                  fmt(-sum(bp[[col]]))
                )
              )
            ),
            
            div(
              class = "inside",
              
              p(
                class = "muted small",
                
                paste(
                  "Latest checkpoint affecting any line:",
                  last_checkpoint(ls$id, allp),
                  "| Individual line checkpoints may differ.",
                  "All checkpoint times are UTC."
                )
              ),
              
              if (!nrow(ls)) {
                p("No budget lines yet.")
              } else {
                tagList(
                  lapply(
                    seq_len(nrow(ls)),
                    
                    function(j) {
                      lp <- p[
                        p$line_id == ls$id[[j]],
                        ,
                        drop = FALSE
                      ]
                      
                      tags$details(
                        class = "box",
                        
                        tags$summary(
                          span(
                            paste(
                              ls$code[[j]],
                              ls$name[[j]]
                            )
                          ),
                          
                          span(
                            class = "negative",
                            
                            paste(
                              cur,
                              fmt(-sum(lp[[col]]))
                            )
                          )
                        ),
                        
                        p(
                          class = "muted small",
                          
                          paste(
                            "Last checkpoint:",
                            last_checkpoint(
                              ls$id[[j]],
                              allp
                            )
                          )
                        ),
                        
                        if (!nrow(lp)) {
                          p(
                            class = "muted",
                            "No paid changes in this view."
                          )
                        } else {
                          tags$table(
                            class = "table table-sm",
                            
                            tags$thead(
                              tags$tr(
                                tags$th("PO / request"),
                                tags$th("Payment date"),
                                tags$th("Change"),
                                tags$th("Checkpoint")
                              )
                            ),
                            
                            tags$tbody(
                              lapply(
                                seq_len(nrow(lp)),
                                
                                function(k) {
                                  tags$tr(
                                    tags$td(
                                      lp$po_ref[[k]]
                                    ),
                                    
                                    tags$td(
                                      lp$payment_date[[k]]
                                    ),
                                    
                                    tags$td(
                                      fmt(
                                        -lp[[col]][[k]]
                                      )
                                    ),
                                    
                                    tags$td(
                                      if (
                                        is.na(
                                          lp$batch_id[[k]]
                                        )
                                      ) {
                                        "Not batched"
                                      } else {
                                        paste(
                                          "Batch",
                                          lp$batch_id[[k]],
                                          lp$batch_status[[k]]
                                        )
                                      }
                                    )
                                  )
                                }
                              )
                            )
                          )
                        }
                      )
                    }
                  )
                )
              }
            )
          )
        }
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # Preparing and applying checkpoint batches
  # -------------------------------------------------------------------------
  
  available_lines <- reactive({
    p <- state()$payments
    
    p <- p[
      is.na(p$batch_id),
      ,
      drop = FALSE
    ]
    
    if (nzchar(s(input$f_budget))) {
      p <- p[
        p$budget_id == as.integer(input$f_budget),
        ,
        drop = FALSE
      ]
    }
    
    l <- state()$lines
    
    l[
      l$id %in% p$line_id,
      ,
      drop = FALSE
    ]
  })
  
  observe({
    l <- available_lines()
    
    ch <- setNames(
      as.character(l$id),
      
      paste(
        l$budget_code,
        l$code,
        l$name,
        sep = " / "
      )
    )
    
    old <- isolate(input$batch_lines)
    old <- old[old %in% unname(ch)]
    
    updateSelectizeInput(
      session,
      "batch_lines",
      choices = ch,
      selected = old
    )
  })
  
  observeEvent(input$select_all_lines, {
    updateSelectizeInput(
      session,
      "batch_lines",
      selected = as.character(
        available_lines()$id
      )
    )
  })
  
  observeEvent(input$prepare_batch, {
    attempt({
      if (
        !identical(
          input$f_mode,
          "pending"
        )
      ) {
        stop(
          paste(
            "Switch to Since checkpoint",
            "before preparing a batch."
          )
        )
      }
      
      id <- make_batch(
        input$batch_lines,
        s(input$batch_note)
      )
      
      preferred_batch(
        as.character(id)
      )
      
      refresh()
      
      showNotification(
        paste(
          "Batch",
          id,
          "prepared. Export it, update external budgets,",
          "then confirm applied."
        ),
        type = "message",
        duration = 12
      )
    })
  })
  
  observe({
    b <- state()$batches
    
    preferred <- isolate(
      preferred_batch()
    )
    
    old <- isolate(
      s(input$batch_choice)
    )
    
    ch <- setNames(
      as.character(b$id),
      
      paste(
        "Batch",
        b$id,
        "-",
        b$status,
        "-",
        b$created_at
      )
    )
    
    selected <- if (
      !is.null(preferred) &&
      preferred %in% unname(ch)
    ) {
      preferred
    } else if (
      old %in% unname(ch)
    ) {
      old
    } else if (
      length(ch)
    ) {
      unname(ch[[1]])
    } else {
      ""
    }
    
    updateSelectInput(
      session,
      "batch_choice",
      choices = ch,
      selected = selected
    )
    
    if (!is.null(preferred)) {
      preferred_batch(NULL)
    }
  })
  
  selected_batch <- reactive({
    b <- state()$batches
    id <- s(input$batch_choice)
    
    b[
      b$id %in% suppressWarnings(
        as.integer(id)
      ),
      ,
      drop = FALSE
    ]
  })
  
  batch_payments <- reactive({
    b <- selected_batch()
    p <- state()$payments
    
    if (!nrow(b)) {
      return(
        p[FALSE, , drop = FALSE]
      )
    }
    
    p[
      !is.na(p$batch_id) &
        p$batch_id == b$id[[1]],
      ,
      drop = FALSE
    ]
  })
  
  output$batch_info <- renderUI({
    b <- selected_batch()
    payments <- batch_payments()
    
    if (!nrow(b)) {
      return(
        p(
          class = "muted",
          "No checkpoint batches yet."
        )
      )
    }
    
    tagList(
      p(
        strong(
          paste(
            "Batch",
            b$id[[1]],
            "-",
            b$status[[1]]
          )
        )
      ),
      
      p(
        class = "muted small",
        
        paste(
          "Prepared UTC:",
          b$created_at,
          "| Applied UTC:",
          
          if (nzchar(s(b$applied_at))) {
            b$applied_at
          } else {
            "Not applied"
          }
        )
      ),
      
      p(
        paste(
          nrow(payments),
          "exact payment events. Note:",
          b$note
        )
      ),
      
      if (b$status[[1]] == "draft") {
        div(
          class = "notice",
          paste(
            "Do not confirm until these exact figures",
            "have been applied to the external budgets.",
            "New payments are excluded from this batch."
          )
        )
      }
    )
  })
  
  payment_display <- function(
    p,
    cur = "ETB"
  ) {
    cur <- currency_value(cur)
    
    col <- paste0(
      tolower(cur),
      "_minor"
    )
    
    data.frame(
      Payment_ID = p$id,
      Request_ID = p$request_id,
      Budget = p$budget_code,
      Budget_line = p$line_code,
      PO = p$po_ref,
      Payment_date = p$payment_date,
      Original_currency = p$original_currency,
      Original_amount = p$original_minor / 100,
      Report_currency = rep(cur, nrow(p)),
      Change = -p[[col]] / 100,
      Rate_date = p$fx_date,
      Rate_source = p$fx_source,
      stringsAsFactors = FALSE
    )
  }
  
  output$batch_table <- DT::renderDT({
    DT::datatable(
      payment_display(
        batch_payments(),
        currency_value(input$f_currency)
      ),
      
      rownames = FALSE,
      escape = TRUE,
      
      options = list(
        scrollX = TRUE,
        pageLength = 8
      )
    ) |>
      DT::formatRound(
        c(
          "Original_amount",
          "Change"
        ),
        2
      )
  })
  
  batch_action <- reactiveVal(NULL)
  
  observeEvent(input$ask_apply, {
    attempt({
      b <- selected_batch()
      
      if (
        !nrow(b) ||
        b$status[[1]] != "draft"
      ) {
        stop("Select a draft batch.")
      }
      
      batch_action(
        list(
          id = b$id[[1]],
          action = "apply"
        )
      )
      
      showModal(
        modalDialog(
          title = paste(
            "Apply checkpoint batch",
            b$id[[1]]
          ),
          
          easyClose = FALSE,
          
          p(
            paste(
              "Confirm only after updating the external",
              "budgets using this batch. This does not",
              "modify any external workbook."
            )
          ),
          
          checkboxInput(
            "batch_confirm",
            "I have applied this exact batch externally",
            FALSE
          ),
          
          footer = tagList(
            modalButton("Not yet"),
            
            actionButton(
              "batch_confirm_action",
              "Confirm checkpoint",
              class = "btn-success"
            )
          )
        )
      )
    })
  })
  
  observeEvent(input$ask_abandon, {
    attempt({
      b <- selected_batch()
      
      if (
        !nrow(b) ||
        b$status[[1]] != "draft"
      ) {
        stop("Select a draft batch.")
      }
      
      batch_action(
        list(
          id = b$id[[1]],
          action = "abandon"
        )
      )
      
      showModal(
        modalDialog(
          title = "Abandon draft?",
          
          p(
            paste(
              "Its payment events will become available",
              "for another batch. Do not abandon a batch",
              "already applied externally."
            )
          ),
          
          checkboxInput(
            "batch_confirm",
            paste(
              "I confirm this draft was",
              "not applied externally"
            ),
            FALSE
          ),
          
          footer = tagList(
            modalButton("Keep draft"),
            
            actionButton(
              "batch_confirm_action",
              "Abandon draft",
              class = "btn-danger"
            )
          )
        )
      )
    })
  })
  
  observeEvent(input$batch_confirm_action, {
    ctx <- batch_action()
    
    if (is.null(ctx)) {
      return()
    }
    
    attempt({
      if (!isTRUE(input$batch_confirm)) {
        stop("Tick the confirmation box.")
      }
      
      if (ctx$action == "apply") {
        apply_batch(ctx$id)
      } else {
        abandon_batch(ctx$id)
      }
      
      batch_action(NULL)
      removeModal()
      refresh()
      
      showNotification(
        "Checkpoint updated.",
        type = "message"
      )
    })
  })
  
  # -------------------------------------------------------------------------
  # Manage: create reference records
  # -------------------------------------------------------------------------
  
  observeEvent(input$add_budget, {
    attempt({
      code <- toupper(s(input$mb_code))
      name <- s(input$mb_name)
      
      if (
        !code_ok(code) ||
        !nzchar(name)
      ) {
        stop(
          paste(
            "Enter a code (letters/digits/_/-)",
            "and budget name."
          )
        )
      }
      
      write_tx(function(cx) {
        execute(
          cx,
          "INSERT INTO budgets(code,name) VALUES(?,?)",
          list(code, name)
        )
        
        id <- query(
          "SELECT last_insert_rowid() AS id",
          cx = cx
        )$id[[1]]
        
        audit(
          cx,
          "budget",
          id,
          "created",
          code
        )
        
        bump(cx)
      })
      
      refresh()
      
      updateTextInput(
        session,
        "mb_code",
        value = ""
      )
      
      updateTextInput(
        session,
        "mb_name",
        value = ""
      )
    })
  })
  
  observeEvent(input$add_line, {
    attempt({
      code <- toupper(s(input$ml_code))
      name <- s(input$ml_name)
      
      bid <- suppressWarnings(
        as.integer(input$ml_budget)
      )
      
      if (
        !code_ok(code) ||
        !nzchar(name) ||
        length(bid) != 1L ||
        is.na(bid)
      ) {
        stop(
          paste(
            "Choose a budget and enter",
            "line code/description."
          )
        )
      }
      
      write_tx(function(cx) {
        active <- query(
          "SELECT 1 FROM budgets WHERE id=? AND active=1",
          list(bid),
          cx
        )
        
        if (!nrow(active)) {
          stop("Budget is inactive.")
        }
        
        execute(
          cx,
          paste(
            "INSERT INTO lines(budget_id,code,name)",
            "VALUES(?,?,?)"
          ),
          list(
            bid,
            code,
            name
          )
        )
        
        id <- query(
          "SELECT last_insert_rowid() AS id",
          cx = cx
        )$id[[1]]
        
        audit(
          cx,
          "line",
          id,
          "created",
          code
        )
        
        bump(cx)
      })
      
      refresh()
      
      updateTextInput(
        session,
        "ml_code",
        value = ""
      )
      
      updateTextInput(
        session,
        "ml_name",
        value = ""
      )
    })
  })
  
  observeEvent(input$add_area, {
    attempt({
      code <- toupper(s(input$ma_code))
      name <- s(input$ma_name)
      country <- toupper(s(input$ma_country))
      
      if (
        !code_ok(code) ||
        !code_ok(country) ||
        !nzchar(name)
      ) {
        stop(
          paste(
            "Enter valid area/country codes",
            "and an area name."
          )
        )
      }
      
      write_tx(function(cx) {
        execute(
          cx,
          paste(
            "INSERT INTO areas(code,name,country)",
            "VALUES(?,?,?)"
          ),
          list(
            code,
            name,
            country
          )
        )
        
        id <- query(
          "SELECT last_insert_rowid() AS id",
          cx = cx
        )$id[[1]]
        
        audit(
          cx,
          "area",
          id,
          "created",
          code
        )
        
        bump(cx)
      })
      
      refresh()
      
      updateTextInput(
        session,
        "ma_code",
        value = ""
      )
      
      updateTextInput(
        session,
        "ma_name",
        value = ""
      )
    })
  })
  
  observeEvent(input$add_contact, {
    attempt({
      kind <- s(input$mc_kind)
      name <- s(input$mc_name)
      
      if (
        !kind %in% c(
          "requester",
          "focal",
          "supplier"
        ) ||
        !nzchar(name)
      ) {
        stop(
          "Choose a list and enter a name."
        )
      }
      
      write_tx(function(cx) {
        execute(
          cx,
          "INSERT INTO contacts(kind,name) VALUES(?,?)",
          list(kind, name)
        )
        
        id <- query(
          "SELECT last_insert_rowid() AS id",
          cx = cx
        )$id[[1]]
        
        audit(
          cx,
          "contact",
          id,
          "created",
          name
        )
        
        bump(cx)
      })
      
      refresh()
      
      updateTextInput(
        session,
        "mc_name",
        value = ""
      )
    })
  })
  
  # -------------------------------------------------------------------------
  # Manage: activate/deactivate
  # -------------------------------------------------------------------------
  
  manage_action <- reactiveVal(NULL)
  
  for (
    kind in c(
      "budget",
      "line",
      "area",
      "contact"
    )
  ) {
    local({
      k <- kind
      
      observeEvent(
        input[[paste0("toggle_", k, "_btn")]],
        attempt({
          id <- suppressWarnings(
            as.integer(input[[paste0("toggle_", k)]])
          )
          
          
          
          if (
            length(id) != 1L ||
            is.na(id)
          ) {
            stop("Select an item first.")
          }
          
          table <- switch(
            k,
            budget = "budgets",
            line = "lines",
            area = "areas",
            contact = "contacts"
          )
          
          row <- query(
            paste0(
              "SELECT * FROM ",
              table,
              " WHERE id=?"
            ),
            list(id)
          )
          
          if (!nrow(row)) {
            stop("Item not found.")
          }
          
          manage_action(
            list(
              table = table,
              kind = k,
              id = id,
              old = row$active[[1]]
            )
          )
          
          showModal(
            modalDialog(
              title = if (
                row$active[[1]] == 1
              ) {
                "Deactivate item?"
              } else {
                "Reactivate item?"
              },
              
              p(
                paste(
                  "Historical records remain unchanged.",
                  "Items used by open requests cannot",
                  "be deactivated."
                )
              ),
              
              footer = tagList(
                modalButton("Cancel"),
                
                actionButton(
                  "confirm_manage",
                  "Confirm",
                  class = "btn-primary"
                )
              )
            )
          )
        })
      )
    })
  }
  
  observeEvent(input$confirm_manage, {
    ctx <- manage_action()
    
    if (is.null(ctx)) {
      return()
    }
    
    attempt({
      write_tx(function(cx) {
        if (
          ctx$old == 1 &&
          ctx$kind != "contact"
        ) {
          field <- switch(
            ctx$kind,
            budget = "budget_id",
            line = "line_id",
            area = "area_id"
          )
          
          open <- query(
            paste0(
              "SELECT 1 FROM requests WHERE ",
              field,
              '=? AND status IN("under_review","pending")'
            ),
            list(ctx$id),
            cx
          )
          
          if (nrow(open)) {
            stop(
              paste(
                "This item has open requests.",
                "Finish/cancel them before deactivation."
              )
            )
          }
        }
        
        n <- execute(
          cx,
          paste0(
            "UPDATE ",
            ctx$table,
            " SET active=? WHERE id=? AND active=?"
          ),
          list(
            1L - ctx$old,
            ctx$id,
            ctx$old
          )
        )
        
        if (n != 1L) {
          stop(
            paste(
              "Item changed in another session.",
              "Reopen it."
            )
          )
        }
        
        audit(
          cx,
          ctx$kind,
          ctx$id,
          "active_changed",
          as.character(1L - ctx$old)
        )
        
        bump(cx)
      })
      
      manage_action(NULL)
      removeModal()
      refresh()
    })
  })
  
  # -------------------------------------------------------------------------
  # Manage tables
  # -------------------------------------------------------------------------
  
  output$budgets_table <- DT::renderDT({
    DT::datatable(
      state()$budgets,
      rownames = FALSE,
      escape = TRUE,
      options = list(pageLength = 8)
    )
  })
  
  output$lines_table <- DT::renderDT({
    DT::datatable(
      state()$lines,
      rownames = FALSE,
      escape = TRUE,
      
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
  })
  
  output$areas_table <- DT::renderDT({
    DT::datatable(
      state()$areas,
      rownames = FALSE,
      escape = TRUE,
      options = list(pageLength = 8)
    )
  })
  
  output$contacts_table <- DT::renderDT({
    DT::datatable(
      state()$contacts,
      rownames = FALSE,
      escape = TRUE,
      options = list(pageLength = 8)
    )
  })
  
  output$audit_table <- DT::renderDT({
    DT::datatable(
      state()$audit,
      rownames = FALSE,
      escape = TRUE,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # Export helpers
  # -------------------------------------------------------------------------
  
  export_requests <- function(
    d,
    cur
  ) {
    d$original_amount <- d$amount_minor / 100
    
    d$display_currency <- rep(
      cur,
      nrow(d)
    )
    
    d$display_amount <- vapply(
      seq_len(nrow(d)),
      
      function(i) {
        display_amount(
          d[i, , drop = FALSE],
          cur
        ) / 100
      },
      
      numeric(1)
    )
    
    d$stage_label <- label(d$status)
    
    p <- state()$payments
    
    d$display_rate_date <- vapply(
      seq_len(nrow(d)),
      
      function(i) {
        z <- p[
          p$request_id == d$id[[i]],
          ,
          drop = FALSE
        ]
        
        if (nrow(z)) {
          z$fx_date[[1]]
        } else {
          d$fx_date[[i]]
        }
      },
      
      character(1)
    )
    
    d$display_rate_source <- vapply(
      seq_len(nrow(d)),
      
      function(i) {
        z <- p[
          p$request_id == d$id[[i]],
          ,
          drop = FALSE
        ]
        
        if (nrow(z)) {
          z$fx_source[[1]]
        } else {
          d$fx_source[[i]]
        }
      },
      
      character(1)
    )
    
    d
  }
  
  export_payments <- function(p) {
    out <- p
    
    for (
      k in c(
        "original",
        "etb",
        "usd",
        "eur",
        "gbp"
      )
    ) {
      out[[paste0(k, "_amount")]] <-
        out[[paste0(k, "_minor")]] / 100
    }
    
    out
  }
  
  notes <- function(
    kind,
    extra = character()
  ) {
    data.frame(
      note = c(
        paste("Report:", kind),
        
        paste(
          "Generated UTC:",
          now_utc()
        ),
        
        paste(
          "Mode:",
          if (DEMO) {
            "SYNTHETIC DEMO"
          } else {
            "LIVE"
          }
        ),
        
        paste(
          "Negative change means an amount for finance",
          "to deduct externally. This app never updates",
          "actual budgets."
        ),
        
        paste(
          "Original amount is the request total.",
          "One request, one budget line, one payment.",
          "Paid records are locked."
        ),
        
        paste(
          "Currency conversions use saved buying-rate",
          "snapshots; rates may differ between records."
        ),
        
        extra
      )
    )
  }
  
  finance_summary <- function(
    p,
    cur
  ) {
    if (!nrow(p)) {
      return(
        data.frame(
          Budget = character(),
          Budget_line = character(),
          Currency = character(),
          Change = numeric(),
          Payments = integer()
        )
      )
    }
    
    col <- paste0(
      tolower(cur),
      "_minor"
    )
    
    keys <- unique(
      p[
        ,
        c(
          "budget_code",
          "line_code"
        ),
        drop = FALSE
      ]
    )
    
    do.call(
      rbind,
      
      lapply(
        seq_len(nrow(keys)),
        
        function(i) {
          z <- p[
            p$budget_code == keys$budget_code[[i]] &
              p$line_code == keys$line_code[[i]],
            ,
            drop = FALSE
          ]
          
          data.frame(
            Budget = keys$budget_code[[i]],
            Budget_line = keys$line_code[[i]],
            Currency = cur,
            Change = -sum(z[[col]]) / 100,
            Payments = nrow(z)
          )
        }
      )
    )
  }
  
  # -------------------------------------------------------------------------
  # Dashboard export
  # -------------------------------------------------------------------------
  
  output$dashboard_export <- downloadHandler(
    filename = function() {
      paste0(
        "Requests_",
        format(today(), "%Y-%m-%d"),
        "_",
        format(Sys.time(), "%H%M%S"),
        ".xlsx"
      )
    },
    
    content = function(file) {
      d <- listed()
      cur <- currency_value(input$d_currency)
      
      writexl::write_xlsx(
        list(
          Requests = export_requests(d, cur),
          
          Notes = notes(
            "Dashboard selected stage",
            
            c(
              paste(
                "Stage:",
                label(stage())
              ),
              
              paste(
                "Period:",
                s(input$d_period)
              ),
              
              paste(
                "Custom request dates:",
                paste(
                  input$d_dates,
                  collapse = " to "
                )
              ),
              
              paste(
                "Budget ID:",
                s(input$d_budget)
              ),
              
              paste(
                "Line ID:",
                s(input$d_line)
              ),
              
              paste(
                "Search:",
                s(input$d_search)
              )
            )
          )
        ),
        file
      )
    }
  )
  
  # -------------------------------------------------------------------------
  # Finance export
  # -------------------------------------------------------------------------
  
  output$finance_export <- downloadHandler(
    filename = function() {
      paste0(
        "Finance_",
        s(input$f_mode),
        "_",
        today(),
        "_",
        format(Sys.time(), "%H%M%S"),
        ".xlsx"
      )
    },
    
    content = function(file) {
      p <- finance_payments()
      cur <- currency_value(input$f_currency)
      
      writexl::write_xlsx(
        list(
          Summary = finance_summary(p, cur),
          
          Payments = payment_display(p, cur),
          
          Rate_snapshots = export_payments(p),
          
          Notes = notes(
            "Financial report",
            
            c(
              paste(
                "View:",
                s(input$f_mode)
              ),
              
              paste(
                "Budget ID:",
                s(input$f_budget)
              ),
              
              paste(
                "Payment dates:",
                paste(
                  input$f_dates,
                  collapse = " to "
                )
              ),
              
              paste(
                "Since-checkpoint includes draft-reserved",
                "items until the draft is applied.",
                "Use an exact batch export for acknowledgment."
              )
            )
          )
        ),
        file
      )
    }
  )
  
  # -------------------------------------------------------------------------
  # Exact checkpoint export
  # -------------------------------------------------------------------------
  
  output$batch_export <- downloadHandler(
    filename = function() {
      b <- selected_batch()
      
      paste0(
        "Checkpoint_batch_",
        
        if (nrow(b)) {
          b$id[[1]]
        } else {
          "none"
        },
        
        "_",
        today(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      b <- selected_batch()
      
      if (!nrow(b)) {
        stop("Select a checkpoint batch.")
      }
      
      p <- batch_payments()
      cur <- currency_value(input$f_currency)
      
      writexl::write_xlsx(
        list(
          Batch = b,
          
          Summary = finance_summary(p, cur),
          
          Exact_payment_events = payment_display(
            p,
            cur
          ),
          
          All_currencies = export_payments(p),
          
          Notes = notes(
            "Exact checkpoint batch",
            
            c(
              paste(
                "Only these exact event IDs are acknowledged.",
                "Later payments are not part of this batch."
              ),
              
              paste(
                "Update external budgets first;",
                "then confirm this batch as applied",
                "in the application."
              )
            )
          )
        ),
        file
      )
    }
  )
}

if (
  !identical(
    Sys.getenv("TRACKER_TEST"),
    "true"
  )
) {
  shinyApp(ui, server)
}