# Apply the Neon schema with the direct connection, then seed reference data.
# The Shiny app uses db_url_pooled and does not create tables.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
setwd(root)
if (file.exists(".Renviron")) readRenviron(".Renviron")

env_first <- function(...) {
  for (name in list(...)) {
    value <- trimws(Sys.getenv(name, unset = ""))
    if (nzchar(value)) return(value)
  }
  ""
}
direct <- env_first("Db_url", "DATABASE_URL")
if (!nzchar(direct)) {
  stop("Set DATABASE_URL to the Neon direct connection string before running scripts/migrate.R.", call. = FALSE)
}
if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RPostgres", quietly = TRUE)) {
  stop("Install DBI and RPostgres before running scripts/migrate.R.", call. = FALSE)
}

sanitize_db_error <- function(text) {
  text <- gsub("://[^@\\s]+@", "://***@", text)
  text <- gsub("password[=:][^\\s&]+", "password=***", text, ignore.case = TRUE)
  text
}
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
cn <- tryCatch(
  connect_postgres(direct),
  error = function(e) {
    stop("Could not connect to Neon with the direct URL. ", sanitize_db_error(conditionMessage(e)), call. = FALSE)
  }
)
on.exit(try(DBI::dbDisconnect(cn), silent = TRUE), add = TRUE)

files <- c(
  "001_schema.sql",
  "002_integrity.sql",
  "003_request_operations.sql",
  "004_finance_operations.sql",
  "005_notification_operations.sql"
)

split_sql <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  parts <- strsplit(text, "\n-- >>>\n", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(sql) {
    lines <- strsplit(sql, "\n", fixed = TRUE)[[1]]
    trimws(paste(lines[!grepl("^\\s*--", lines)], collapse = "\n"))
  }, character(1))
  parts[nzchar(parts)]
}

applied <- if ("schema_migrations" %in% DBI::dbListTables(cn)) {
  DBI::dbGetQuery(cn, "SELECT version FROM schema_migrations")$version
} else {
  integer()
}

for (i in seq_along(files)) {
  if (i %in% applied) next
  path <- file.path(root, "sql", files[[i]])
  parts <- split_sql(path)
  tryCatch(
    DBI::dbWithTransaction(cn, {
      for (sql in parts) DBI::dbExecute(cn, sql)
      DBI::dbExecute(
        cn,
        "INSERT INTO schema_migrations(version, filename) VALUES($1, $2)",
        params = list(i, files[[i]])
      )
    }),
    error = function(e) stop("Migration ", files[[i]], " failed: ", conditionMessage(e), call. = FALSE)
  )
  message("Applied ", files[[i]])
}

Sys.unsetenv("TRACKER_DB_PATH")
assign("APP_DIR", root, envir = .GlobalEnv)
sys.source(file.path(root, "R/core/config.R"), envir = .GlobalEnv)
sys.source(file.path(root, "R/core/contracts.R"), envir = .GlobalEnv)
sys.source(file.path(root, "R/core/db.R"), envir = .GlobalEnv)
seed_reference_data(cn)
message("Neon schema is current and reference data is seeded.")
