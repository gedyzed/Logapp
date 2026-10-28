<<<<<<< HEAD
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
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/core/bootstrap.R
# Owner: Integrator
# Purpose: Load definitions explicitly and build dependency bundles for app, worker and command-line scripts.
# Expected code: Explicit ordered source manifest and factory functions; definitions load without side effects. Separate app/worker/test dependency builders.
# Connections: Loads core, integrations and feature definitions; returns config, connection provider, repositories, services and clock. Used by app.R and scripts.
# Boundary: Never recursively source arbitrary files or run migrations, seeds, network calls or email on source. Worker initialization must not require CBE or Shiny UI initialization.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
