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
if (file.exists(".Renviron")) {
  try(readRenviron(".Renviron"), silent = TRUE)
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

# The page module defines the complete navigation UI.
source_module("R/features/requests_page.R")

# --- Custom login page matching reference design ---
login_style <- "
  /* Center the login container & background */
  html, body {
    margin: 0 !important;
    padding: 0 !important;
    width: 100% !important;
    height: 100% !important;
    background-color: #f4f4f5 !important;
    overflow-x: hidden !important;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
  }
  
  /* Make shinymanager panel-auth the fixed centered flex viewport */
  .panel-auth {
    position: fixed !important;
    top: 0 !important;
    left: 0 !important;
    right: 0 !important;
    bottom: 0 !important;
    width: 100vw !important;
    height: 100vh !important;
    background-color: #f4f4f5 !important;
    z-index: 99997 !important;
    overflow-y: auto !important;
    overflow-x: hidden !important;
    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
    padding: 2rem 1rem !important;
    box-sizing: border-box !important;
    margin: 0 !important;
    border: none !important;
    box-shadow: none !important;
  }
  
  /* Remove default top spacer elements from shinymanager */
  .panel-auth > br,
  .panel-auth > div[style*='height'] {
    display: none !important;
  }
  
  .panel-auth > .row {
    margin: 0 !important;
    width: 100% !important;
    max-width: 400px !important;
    display: flex !important;
    justify-content: center !important;
  }
  
  .panel-auth > .row > div {
    width: 100% !important;
    max-width: 400px !important;
    margin: 0 !important;
    padding: 0 !important;
    float: none !important;
    flex: 0 0 100% !important;
  }
  
  /* White card styling */
  .panel-auth .panel-primary {
    background: #ffffff !important;
    border: 1px solid #e4e4e7 !important;
    border-radius: 20px !important;
    box-shadow: 0 4px 20px -2px rgba(0, 0, 0, 0.05), 0 2px 6px -1px rgba(0, 0, 0, 0.02) !important;
    overflow: hidden !important;
    margin: 0 !important;
    width: 100% !important;
    max-width: 400px !important;
  }
  
  .panel-auth .panel-heading {
    display: none !important;
  }
  
  .panel-auth .panel-body {
    background: #ffffff !important;
    padding: 2.6rem 2.2rem 2.8rem 2.2rem !important;
  }
  
  .panel-auth .panel-body > br {
    display: none !important;
  }
  
  /* Auth heading */
  #auth-shinymanager-auth-head {
    color: #0f172a !important;
    font-size: 1.7rem !important;
    font-weight: 600 !important;
    letter-spacing: -0.02em !important;
    margin: 1.25rem 0 1.6rem 0 !important;
    text-align: center !important;
    font-family: inherit !important;
  }
  
  /* Form groups */
  #auth-user_input {
    display: flex;
    flex-direction: column;
    gap: 1.25rem;
    margin-bottom: 0.25rem;
  }
  
  .panel-auth .form-group {
    margin-bottom: 0 !important;
  }
  
  /* Labels: Deep navy blue matching reference */
  .panel-auth .control-label {
    font-weight: 650 !important;
    color: #1e3a8a !important;
    font-size: 0.875rem !important;
    margin-bottom: 0.45rem !important;
    display: block !important;
    text-align: left !important;
  }
  
  /* Clean inputs with soft blue background */
  .panel-auth .form-control {
    background-color: #eff6ff !important;
    border: 1px solid #bfdbfe !important;
    border-radius: 10px !important;
    padding: 0.75rem 1rem !important;
    font-size: 0.95rem !important;
    height: 3rem !important;
    color: #1e293b !important;
    background-image: none !important;
    box-shadow: none !important;
    transition: background-color 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease !important;
  }
  
  .panel-auth .form-control:focus {
    background-color: #ffffff !important;
    border-color: #2563eb !important;
    box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.15) !important;
    outline: none !important;
  }
  
  /* Login button */
  #auth-container-btn-ok {
    margin-top: 1.5rem !important;
  }
  
  #auth-container-btn-ok > br {
    display: none !important;
  }
  
  .panel-auth .btn-primary {
    background: #2563eb !important;
    border: none !important;
    color: #ffffff !important;
    font-weight: 600 !important;
    border-radius: 10px !important;
    padding: 0.75rem 1.5rem !important;
    width: 100% !important;
    height: 3rem !important;
    font-size: 1rem !important;
    transition: background-color 0.15s ease, transform 0.1s ease !important;
    box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05) !important;
  }
  
  .panel-auth .btn-primary:hover {
    background: #1d4ed8 !important;
  }
  
  .panel-auth .btn-primary:active {
    background: #1e40af !important;
    transform: translateY(1px);
  }
  
  /* Error / alert messages */
  #auth-result_auth .alert,
  .panel-auth .help-block {
    border-radius: 8px !important;
    font-size: 0.85rem !important;
    margin-top: 0.75rem !important;
    margin-bottom: 0 !important;
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

login_js <- "
$(document).ready(function() {
  if ($('.panel-auth').length) {
    $('body').css('background-color', '#f4f4f5');
    $('.panel-auth').closest('.container-fluid').addClass('login-viewport-active');
  }
});
"

secured_ui <- shinymanager::secure_app(
  ui,
  enable_admin = FALSE,
  language = "en",
  choose_language = FALSE,
  fab_position = "none",
  tags_top = tagList(
    tags$head(
      tags$style(HTML(login_style)),
      tags$script(HTML(login_js))
    ),
    # Top-left Welcome Back
    tags$div(
      class = "login-welcome-box",
      style = "text-align: left; margin-bottom: 1.85rem;",
      tags$div(
        style = "color: #2563eb; font-weight: 700; font-size: 1.4rem; line-height: 1.25; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Welcome Back"
      ),
      tags$div(
        style = "color: #6b7280; font-size: 0.875rem; margin-top: 0.35rem; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Please enter your details to continue."
      )
    ),
    # Centered Logo and Brand Lockup
    tags$div(
      class = "login-brand-lockup",
      style = "display: flex; align-items: center; justify-content: center; gap: 0.75rem; margin-bottom: 1.5rem;",
      # Blue rounded square with white L
      tags$div(
        style = "width: 2.6rem; height: 2.6rem; border-radius: 10px; background: #2563eb; display: flex; align-items: center; justify-content: center; flex-shrink: 0;",
        tags$span(
          style = "color: #ffffff; font-weight: 700; font-size: 1.4rem; line-height: 1; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
          "L"
        )
      ),
      # Logapp & Expense Tracker text
      tags$div(
        style = "text-align: left;",
        tags$div(
          style = "color: #2563eb; font-weight: 700; font-size: 1.25rem; line-height: 1.15; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
          "Logapp"
        ),
        tags$div(
          style = "color: #6b7280; font-size: 0.85rem; font-weight: 500; line-height: 1.15; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
          "Expense Tracker"
        )
      )
    )
  ),
  tags_bottom = NULL
)

server <- function(input, output, session) {
  
  # --- shinymanager authentication with session persistence on refresh ---
  auth_result <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(AUTH_CREDENTIALS),
    keep_token = TRUE
  )
  
  # Log successful authentication
  observe({
    if (!is.null(auth_result$user) && nzchar(auth_result$user)) {
      message("User authenticated: ", auth_result$user, " at ", Sys.time())
    }
  })
  
  # Handle logout - invalidate shinymanager token, clear query string, and reload session
  observeEvent(input$logout_btn, {
    token <- tryCatch(shinymanager:::getToken(session = session), error = function(e) NULL)
    if (!is.null(token)) {
      try(shinymanager:::.tok$remove(token), silent = TRUE)
    }
    try(shinymanager:::clearQueryString(session = session), silent = TRUE)
    shiny::updateQueryString("?", mode = "replace", session = session)
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