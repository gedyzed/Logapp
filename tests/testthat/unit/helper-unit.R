# Load parent test helpers and application core with isolated temp SQLite DB
helpers_dir <- normalizePath(file.path(testthat::test_path("..")), winslash = "/", mustWork = TRUE)
for (h in list.files(helpers_dir, pattern = "^helper-.*\\.R$", full.names = TRUE)) {
  source(h)
}

# Ensure isolated database is used; never open tracker.db
if (!exists("RATES", envir = .GlobalEnv)) {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  root <- tracker_root()
  sys.source(file.path(root, "R/features/fx_service.R"), envir = .GlobalEnv)

  # Safely load finance pure helpers into .GlobalEnv without registering dangling observers
  fin_env <- new.env(parent = .GlobalEnv)
  fin_env$observeEvent <- function(...) invisible(NULL)
  fin_env$observe <- function(...) invisible(NULL)
  fin_env$reactiveVal <- function(x = NULL) function(new) x
  fin_env$reactive <- function(expr) function(...) NULL
  sys.source(file.path(root, "R/features/finance_service.R"), envir = fin_env)
  fin_to_display_minor <<- fin_env$fin_to_display_minor
  fin_amount_minor <<- fin_env$fin_amount_minor
  fin_as_minor <<- fin_env$fin_as_minor
}
