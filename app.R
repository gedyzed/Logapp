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
    max-width: min(50rem, 100%) !important;
    display: flex !important;
    justify-content: center !important;
  }
  
  .panel-auth > .row > div {
    width: 100% !important;
    max-width: min(45.0rem, 100%) !important;
    margin: 0 !important;
    padding: 0 !important;
    float: none !important;
    flex: 0 0 100% !important;
  }
  
  /* White card styling */
  .panel-auth .panel-primary {
    background: #ffffff !important;
    border: 0.0625rem solid #e4e4e7 !important;
    border-radius: 1.375rem !important;
    box-shadow: 0 0.65rem 1.9rem -0.5rem rgba(15, 23, 42, 0.12), 0 0.125rem 0.5rem -0.125rem rgba(15, 23, 42, 0.06) !important;
    overflow: hidden !important;
    margin: 0 !important;
    width: 100% !important;
    max-width: min(45.0rem, 100%) !important;
  }
  
  .panel-auth .panel-heading {
    display: none !important;
  }
  
  .panel-auth .panel-body {
    background: #f4f4f5 !important;
    padding: 3.46rem 3.15rem 3.78rem 3.15rem !important;
  }
  
  .panel-auth .panel-body > br {
    display: none !important;
  }
  
  /* Auth heading */
  #auth-shinymanager-auth-head {
    color: #0f172a !important;
    font-size: 2.58rem !important;
    font-weight: 700 !important;
    letter-spacing: -0.03em !important;
    margin: 1.35rem 0 1.75rem 0 !important;
    text-align: center !important;
    font-family: inherit !important;
    line-height: 1.2 !important;
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
    color: #1e293b !important;
    font-size: 1.26rem !important;
    margin-bottom: 0.5rem !important;
    display: block !important;
    text-align: left !important;
  }
  
  /* Clean inputs with soft blue background */
  .panel-auth .form-control {
    background-color: #eff6ff !important;
    border: 0.0625rem solid #bfdbfe !important;
    border-radius: 0.75rem !important;
    padding: 1.02rem 1.26rem !important;
    font-size: 1.3rem !important;
    height: 4.24rem !important;
    color: #1e293b !important;
    background-image: none !important;
    box-shadow: none !important;
    transition: background-color 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease !important;
  }

  .login-password-wrap {
    position: relative !important;
  }

  .login-password-wrap .form-control {
    padding-right: 3.25rem !important;
  }

  .login-password-toggle {
    position: absolute !important;
    right: 0.7rem !important;
    top: 50% !important;
    transform: translateY(-50%) !important;
    width: 2.25rem !important;
    height: 2.25rem !important;
    border: none !important;
    background: transparent !important;
    color: #64748b !important;
    padding: 0 !important;
    display: flex !important;
    align-items: center !important;
    justify-content: center !important;
    cursor: pointer !important;
    font-size: 1.62rem !important;
  }

  .login-password-toggle:hover {
    color: #1d4ed8 !important;
  }
  
  .panel-auth .form-control:focus {
    background-color: #ffffff !important;
    border-color: #2563eb !important;
    box-shadow: 0 0 0 0.2rem rgba(37, 99, 235, 0.15) !important;
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
    border-radius: 0.75rem !important;
    padding: 1.02rem 1.8rem !important;
    width: 100% !important;
    height: 4.24rem !important;
    font-size: 1.3rem !important;
    transition: background-color 0.15s ease, transform 0.1s ease !important;
    box-shadow: 0 0.0625rem 0.125rem 0 rgba(0, 0, 0, 0.05) !important;
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
    border-radius: 0.5rem !important;
    font-size: 1.02rem !important;
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
  var eye = '<svg viewBox=\"0 0 24 24\" width=\"1em\" height=\"1em\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><path d=\"M2 12s3.5-7 10-7 10 7 10 7-3.5 7-10 7S2 12 2 12z\"></path><circle cx=\"12\" cy=\"12\" r=\"3\"></circle></svg>';
  var eyeOff = '<svg viewBox=\"0 0 24 24\" width=\"1em\" height=\"1em\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" aria-hidden=\"true\"><path d=\"M3 3l18 18\"></path><path d=\"M10.6 6.2A10.9 10.9 0 0 1 12 6c6.5 0 10 6 10 6a18.5 18.5 0 0 1-4.2 4.7\"></path><path d=\"M6.1 6.1C3.7 7.8 2 12 2 12s3.5 7 10 7c1.5 0 2.9-.3 4.2-.9\"></path><path d=\"M9.9 9.9a3 3 0 0 0 4.2 4.2\"></path></svg>';
  var pwd = document.getElementById('auth-user_pwd');
  if (!pwd || pwd.dataset.eyeReady) return;
  pwd.dataset.eyeReady = '1';
  var wrap = document.createElement('div');
  wrap.className = 'login-password-wrap';
  pwd.parentNode.insertBefore(wrap, pwd);
  wrap.appendChild(pwd);
  var btn = document.createElement('button');
  btn.type = 'button';
  btn.className = 'login-password-toggle';
  btn.setAttribute('aria-label', 'Show password');
  btn.innerHTML = eye;
  btn.addEventListener('click', function() {
    var show = pwd.type === 'password';
    pwd.type = show ? 'text' : 'password';
    btn.setAttribute('aria-label', show ? 'Hide password' : 'Show password');
    btn.innerHTML = show ? eyeOff : eye;
  });
  wrap.appendChild(btn);
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
        style = "color: #2563eb; font-weight: 700; font-size: 2.04rem; line-height: 1.25; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Welcome Back"
      ),
      tags$div(
        style = "color: #6b7280; font-size: 1.26rem; margin-top: 0.48rem; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
        "Please enter your details to continue."
      )
    ),
    # Centered Logo and Brand Lockup
    tags$div(
      class = "login-brand-lockup",
      style = "display: flex; align-items: center; justify-content: center; gap: 0.75rem; margin-bottom: 1.5rem;",
      # Blue rounded square with white L
      tags$div(
        style = "width: 3.78rem; height: 3.78rem; border-radius: 0.9rem; background: #2563eb; display: flex; align-items: center; justify-content: center; flex-shrink: 0;",
        tags$span(
          style = "color: #ffffff; font-weight: 700; font-size: 2.04rem; line-height: 1; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
          "L"
        )
      ),
      # Logapp & Expense Tracker text
      tags$div(
        style = "text-align: left;",
        tags$div(
          style = "color: #2563eb; font-weight: 700; font-size: 1.8rem; line-height: 1.15; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
          "Logapp"
        ),
        tags$div(
          style = "color: #6b7280; font-size: 1.2rem; font-weight: 500; line-height: 1.2; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
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