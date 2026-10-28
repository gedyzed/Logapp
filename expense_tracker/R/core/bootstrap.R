# Load the core runtime in dependency order. Feature modules are deliberately
# excluded so command-line workers can reuse the database and shared helpers
# without initializing Shiny page behavior.
load_expense_tracker_core <- function(app_dir, envir = .GlobalEnv) {
  core_modules <- c(
    "R/core/config.R",
    "R/core/db.R",
    "R/core/components.R"
  )

  for (relative_path in core_modules) {
    module_path <- file.path(app_dir, relative_path)
    if (!file.exists(module_path)) {
      stop("Expense Tracker core module is missing: ", module_path, call. = FALSE)
    }
    sys.source(module_path, envir = envir)
  }

  invisible(core_modules)
}
