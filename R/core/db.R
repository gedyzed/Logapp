# ---- number formatting -----------------------------------------------------
abbr <- function(x) {   # 1,250,000 -> 1.25M
  tr <- function(y, suf) paste0(sub("\\.?0+$", "", formatC(y, format = "f", digits = 2)), suf)
  ifelse(abs(x) >= 1e9, tr(x / 1e9, "B"),
         ifelse(abs(x) >= 1e6, tr(x / 1e6, "M"),
                ifelse(abs(x) >= 1e3, tr(x / 1e3, "K"), tr(x, ""))))
}
money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

# ---- normalized SQLite database ---------------------------------------------
.tracker_pg_con <- NULL

connect_postgres <- function(url) {
  raw <- trimws(as.character(url))
  body <- sub("^postgres(ql)?://", "", raw, ignore.case = TRUE)
  userinfo <- if (grepl("@", body, fixed = TRUE)) sub("@.*", "", body) else ""
  hostpart <- if (nzchar(userinfo)) sub("^[^@]+@", "", body) else body
  user <- utils::URLdecode(sub(":.*", "", userinfo))
  password <- utils::URLdecode(sub("^[^:]*:", "", userinfo))
  hostport <- sub("[/?].*", "", hostpart)
  database <- sub("^[^/]*/", "", hostpart)
  database <- sub("\\?.*", "", database)
  if (grepl(":", hostport, fixed = TRUE)) {
    host <- sub(":[^:]*$", "", hostport)
    port <- as.integer(sub("^.*:", "", hostport))
  } else {
    host <- hostport
    port <- 5432L
  }
  query <- if (grepl("?", hostpart, fixed = TRUE)) sub("^[^?]*\\?", "", hostpart) else ""
  sslmode <- if (grepl("(^|&)sslmode=", query)) sub(".*(?:^|&)sslmode=([^&]*).*", "\\1", query) else "require"
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = utils::URLdecode(database),
    user = user,
    password = password,
    sslmode = sslmode
  )
}

con <- function() {
  if (db_is_postgres()) {
    if (is.null(.tracker_pg_con) || !DBI::dbIsValid(.tracker_pg_con)) {
      if (!requireNamespace("RPostgres", quietly = TRUE)) {
        stop("Install RPostgres to connect to Neon.", call. = FALSE)
      }
      .tracker_pg_con <<- connect_postgres(DB)
    }
    return(.tracker_pg_con)
  }
  cn <- dbConnect(SQLite(), DB)
  dbExecute(cn, "PRAGMA busy_timeout = 5000")
  dbExecute(cn, "PRAGMA foreign_keys = ON")
  cn
}

sql_for_driver <- function(sql) {
  if (!db_is_postgres()) return(sql)
  n <- 0L
  chars <- strsplit(sql, "", fixed = TRUE)[[1]]
  out <- character(length(chars))
  for (i in seq_along(chars)) {
    if (identical(chars[[i]], "?")) {
      n <- n + 1L
      out[[i]] <- paste0("$", n)
    } else {
      out[[i]] <- chars[[i]]
    }
  }
  paste(out, collapse = "")
}

dbGetQuery <- function(conn, statement, ..., params = NULL) {
  statement <- sql_for_driver(statement)
  if (is.null(params)) DBI::dbGetQuery(conn, statement, ...) else DBI::dbGetQuery(conn, statement, ..., params = params)
}

dbExecute <- function(conn, statement, ..., params = NULL) {
  statement <- sql_for_driver(statement)
  if (is.null(params)) DBI::dbExecute(conn, statement, ...) else DBI::dbExecute(conn, statement, ..., params = params)
}

db_release <- function(cn) {
  if (db_is_postgres()) return(invisible(FALSE))
  if (!is.null(cn) && DBI::dbIsValid(cn)) DBI::dbDisconnect(cn)
  invisible(TRUE)
}

strip_sql_comments <- function(sql) {
  lines <- strsplit(sql, "\n", fixed = TRUE)[[1]]
  kept <- lines[!grepl("^\\s*--", lines)]
  trimws(paste(kept, collapse = "\n"))
}

split_sql_script <- function(text) {
  parts <- strsplit(text, "\n-- >>>\n", fixed = TRUE)[[1]]
  parts <- vapply(parts, strip_sql_comments, character(1))
  parts[nzchar(parts)]
}

apply_sql_file <- function(cn, path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (grepl("\n-- >>>\n", text, fixed = TRUE)) {
    parts <- split_sql_script(text)
  } else {
    parts <- strsplit(text, ";\n", fixed = FALSE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
  }
  for (sql in parts) {
    sql <- trimws(sql)
    sql <- sub(";\\s*$", "", sql)
    sql <- strip_sql_comments(sql)
    if (!nzchar(sql)) next
    dbExecute(cn, sql)
  }
  invisible(TRUE)
}

db_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")

source_persist <- function() {
  path <- file.path(configured_app_dir, "R", "core", "persist.R")
  sys.source(path, envir = environment(con))
}
source_persist()

now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")

fmt <- function(d) format(d, "%d-%m-%Y")
v   <- function(x) if (is.null(x) || !length(x) || is.na(x)) "" else trimws(x)
nz  <- function(x) if (nzchar(v(x))) v(x) else NA_character_
has_date <- function(d) !is.null(d) && length(d) == 1 && !is.na(d) && nzchar(trimws(as.character(d)))
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

tryCatch(
  init_db(),
  error = function(e) {
    detail <- conditionMessage(e)
    if (grepl("://|password|pwd=", detail, ignore.case = TRUE)) detail <- "the connection was refused"
    stop("Expense Tracker could not open its database. ", detail, call. = FALSE)
  }
)
DEFAULTS <- read_app_defaults()

