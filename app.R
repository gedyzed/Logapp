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

# --- Custom login page matching reference design ---
login_style <- "
  /* Light gray-blue background */
  body, html {
    margin: 0;
    padding: 0;
    width: 100%;
    height: 100%;
    background-color: #F5F9FE;
  }
  
  /* Center the login container */
  .shinymanager-container {
    background-color: #F5F9FE !important;
    min-height: 100vh !important;
    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
    flex-direction: column !important;
    padding: 2rem !important;
  }
  
  /* Center wrapper */
  .shinymanager-container > div {
    display: flex;
    flex-direction: column;
    align-items: center;
    width: 100%;
    max-width: 420px;
  }
  
  /* White login card */
  .panel-auth {
    background: #ffffff !important;
    border: 1px solid #e5e7eb !important;
    border-radius: 12px !important;
    box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important;
    overflow: visible !important;
    width: 100% !important;
    margin: 0 !important;
  }
  
  /* Remove default header */
  .panel-auth .panel-heading {
    display: none !important;
  }
  
  /* Card body styling */
  .panel-auth .panel-body {
    background: #ffffff !important;
    padding: 2.5rem 2rem !important;
  }
  
  /* Form groups */
  .panel-auth .form-group {
    margin-bottom: 1.5rem !important;
  }
  
  /* Labels - dark navy */
  .panel-auth .form-group label {
    font-weight: 600 !important;
    color: #1e3a8a !important;
    font-size: 0.875rem !important;
    margin-bottom: 0.5rem !important;
    display: block !important;
  }
  
  /* Input fields with icons */
  .panel-auth .form-control {
    border: 1px solid #e5e7eb !important;
    border-radius: 8px !important;
    padding: 0.75rem 1rem 0.75rem 2.5rem !important;
    font-size: 0.95rem !important;
    height: 3rem !important;
    transition: all 0.2s ease !important;
    background-color: #ffffff !important;
    color: #374151 !important;
  }
  
  .panel-auth .form-control:focus {
    border-color: #3b82f6 !important;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1) !important;
    outline: none !important;
  }
  
  .panel-auth .form-control::placeholder {
    color: #9ca3af !important;
  }
  
  /* Blue login button */
  .panel-auth .btn-primary {
    background: #3b82f6 !important;
    border: none !important;
    color: #ffffff !important;
    font-weight: 600 !important;
    border-radius: 8px !important;
    padding: 0.875rem 1.5rem !important;
    width: 100% !important;
    min-height: 3rem !important;
    font-size: 1rem !important;
    transition: all 0.2s ease !important;
    margin-top: 0.5rem !important;
  }
  
  .panel-auth .btn-primary:hover {
    background: #2563eb !important;
  }
  
  .panel-auth .btn-primary:active {
    background: #1d4ed8 !important;
  }
  
  /* Error message */
  .panel-auth .help-block {
    color: #dc2626 !important;
    font-size: 0.875rem !important;
    margin-top: 0.5rem !important;
  }
  
  /* Add icons before input fields */
  .panel-auth .form-group:nth-of-type(1) .form-control {
    background-image: url('data:image/svg+xml;utf8,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"%236b7280\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2\"></path><circle cx=\"12\" cy=\"7\" r=\"4\"></circle></svg>') !important;
    background-repeat: no-repeat !important;
    background-position: 0.75rem center !important;
    background-size: 1rem !important;
  }
  
  .panel-auth .form-group:nth-of-type(2) .form-control {
    background-image: url('data:image/svg+xml;utf8,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"%236b7280\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\" ry=\"2\"></rect><path d=\"M7 11V7a5 5 0 0 1 10 0v4\"></path></svg>') !important;
    background-repeat: no-repeat !important;
    background-position: 0.75rem center !important;
    background-size: 1rem !important;
  }
  
  /* Logout button hover effect */
  #logout_btn:hover {
    background-color: rgba(220, 38, 38, 0.2) !important;
    transform: translateY(-1px);
  }
  
  #logout_btn:active {
    transform: translateY(0);
  }
"

# JavaScript to move welcome text inside card
login_js <- "
$(document).ready(function() {
  setTimeout(function() {
    var welcomeText = $('#login-welcome-text');
    var panelBody = $('.panel-auth .panel-body');
    if (welcomeText.length && panelBody.length) {
      panelBody.prepend(welcomeText);
    }
  }, 100);
});
"

secured_ui <- shinymanager::secure_app(
  ui,
  enable_admin = FALSE,
  language = "en",
  choose_language = FALSE,
  fab_position = "none",
  tags_top = tags$div(
    tags$head(
      tags$style(HTML(login_style)),
      tags$script(HTML(login_js))
    ),
    # Logo above the card
    tags$div(
      style = "text-align: center; margin-bottom: 2.5rem;",
      # Logo icon and text
      tags$div(
        style = "display: flex; align-items: center; justify-content: center; gap: 0.75rem; margin-bottom: 0.5rem;",
        # Blue rounded square with white L
        tags$div(
          style = "width: 3.5rem; height: 3.5rem; border-radius: 10px; background: #3b82f6; display: flex; align-items: center; justify-content: center;",
          tags$span(
            style = "color: #ffffff; font-weight: 700; font-size: 1.75rem; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
            "L"
          )
        ),
        # Logapp text
        tags$div(
          style = "text-align: left;",
          tags$div(
            style = "color: #1e3a8a; font-weight: 700; font-size: 1.5rem; line-height: 1.2; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
            "Logapp"
          ),
          tags$div(
            style = "color: #3b82f6; font-size: 0.75rem; font-weight: 600; letter-spacing: 0.05em; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
            "EXPENSE TRACKER"
          )
        )
      )
    ),
    # Welcome text (will be moved inside the card by JavaScript)
    tags$div(
      id = "login-welcome-text",
      style = "margin-bottom: 2rem;",
      tags$h1(
        style = "color: #1e3a8a; font-weight: 700; font-size: 1.75rem; margin: 0 0 0.5rem 0; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Welcome Back"
      ),
      tags$p(
        style = "color: #6b7280; font-size: 0.95rem; margin: 0; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Please enter your details to continue."
      )
    )
  ),
  tags_bottom = NULL
)

server <- function(input, output, session) {
  
  # --- shinymanager authentication ---
  auth_result <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(AUTH_CREDENTIALS)
  )
  
  # Log successful authentication
  observe({
    if (!is.null(auth_result$user) && nzchar(auth_result$user)) {
      message("User authenticated: ", auth_result$user, " at ", Sys.time())
    }
  })
  
  # Handle logout - reload session to show login page
  observeEvent(input$logout_btn, {
    # Clear authentication and reload
    session$reload()
  })
  
  # Load feature modules
  source_module("R/features/requests_service.R", environment())
  source_module("R/features/manage_service.R", environment())
  source_module("R/features/requests_modals.R", environment())
  source_module("R/features/insights_service.R", environment())
  source_module("R/features/finance_service.R", environment())
  source_module("R/features/finance_page.R", environment())
}

shinyApp(secured_ui, server)