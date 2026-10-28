<<<<<<< HEAD
# Expense Tracker application entry point.
#
# The application is intentionally composed through an explicit manifest. Core
# modules define shared state and helpers; feature server modules are sourced
# inside each Shiny session so their observers use that session's environment.

APP_DIR <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  candidates <- c(
    if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else character(),
    vapply(
      rev(sys.frames()),
      function(frame) if (!is.null(frame$ofile)) as.character(frame$ofile) else "",
      character(1)
    )
  )
  existing <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(existing)) {
    dirname(normalizePath(existing[[1]], winslash = "/", mustWork = TRUE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
})

source_module <- function(relative_path, envir = .GlobalEnv) {
  module_path <- file.path(APP_DIR, relative_path)
  if (!file.exists(module_path)) {
    stop("Expense Tracker module is missing: ", module_path, call. = FALSE)
  }
  sys.source(module_path, envir = envir)
}

# Core setup loads packages, resolves the database, initializes the schema, and
# exposes shared formatting, UI, and persistence helpers.
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

# The page module defines the complete navigation UI.
source_module("R/features/requests_page.R")

server <- function(input, output, session) {
  # Each of these files contains server registrations for the active session.
  source_module("R/features/requests_service.R", environment())
  source_module("R/features/manage_service.R", environment())
  source_module("R/features/requests_modals.R", environment())
  source_module("R/features/insights_service.R", environment())
  source_module("R/features/finance_service.R", environment())
  source_module("R/features/finance_page.R", environment())
}

shinyApp(ui, server)
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: app.R
# Owner: Integrator
# Purpose: Compose the four-tab Shiny application and session wiring.
# Expected code: Thin entry point: call the loader, initialize app dependencies, assemble UI and call page module servers.
# Connections: Calls bootstrap.R; wires requests_page, finance_page, manage_page and insights_page with explicit dependencies and refresh callbacks.
# Boundary: No SQL, business transitions, sample data, SMTP, large CSS strings or automatic migrations. Target roughly 100-150 lines, not a hard correctness limit.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
