# Compatibility loader for tools that source R/load.R directly.
#
# The normal entry point is app.R. This file keeps older VS Code/RStudio run
# commands working without depending on APP_DIR being a global variable.

load_expense_tracker_app <- function(app_dir = NULL, envir = .GlobalEnv) {
  if (is.null(app_dir) || !length(app_dir) || !nzchar(app_dir[[1]])) {
    ofile <- vapply(
      rev(sys.frames()),
      function(frame) if (!is.null(frame$ofile)) as.character(frame$ofile) else "",
      character(1)
    )
    ofile <- ofile[nzchar(ofile)]
    app_dir <- if (length(ofile)) {
      dirname(dirname(normalizePath(ofile[[1]], winslash = "/", mustWork = TRUE)))
    } else {
      normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    }
  }

  source_module <- function(relative_path) {
    module_path <- file.path(app_dir, relative_path)
    if (!file.exists(module_path)) {
      stop("Expense Tracker module is missing: ", module_path, call. = FALSE)
    }
    sys.source(module_path, envir = envir)
  }

  source_module("R/core/config.R")
  source_module("R/core/contracts.R")
  source_module("R/core/db.R")
  tryCatch(
    align_request_statuses(),
    error = function(e) warning("Could not align request statuses: ", conditionMessage(e), call. = FALSE)
  )
  source_module("R/core/components.R")
  source_module("R/features/fx_service.R")
  source_module("R/features/finance_page.R")
  source_module("R/features/requests_page.R")
  invisible(app_dir)
}

if (identical(environment(), globalenv())) {
  load_expense_tracker_app()
}
