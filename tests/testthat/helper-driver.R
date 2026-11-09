tracker_browser_path <- function() {
  configured <- Sys.getenv("CHROMOTE_CHROME", unset = "")
  if (nzchar(configured) && file.exists(configured)) return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) return(normalizePath(hit[[1]], winslash = "/", mustWork = TRUE))
  found <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (is.character(found) && length(found) && nzchar(found[[1]]) && file.exists(found[[1]])) {
    return(normalizePath(found[[1]], winslash = "/", mustWork = TRUE))
  }
  ""
}

tracker_browser_problem <- function() {
  if (!requireNamespace("shinytest2", quietly = TRUE)) return("shinytest2 is not installed")
  if (!requireNamespace("chromote", quietly = TRUE)) return("chromote is not installed")
  if (!nzchar(tracker_browser_path())) return("Chrome or Edge was not found, so shinytest2 cannot start")
  ""
}

tracker_drive <- function(db_path, now) {
  problem <- tracker_browser_problem()
  if (nzchar(problem)) stop(problem, call. = FALSE)
  Sys.setenv(
    TRACKER_DB_PATH = normalizePath(db_path, winslash = "/", mustWork = TRUE),
    TRACKER_NOW = now,
    CHROMOTE_CHROME = tracker_browser_path(),
    NOT_CRAN = "true"
  )
  shinytest2::AppDriver$new(
    tracker_root(),
    name = "expense-tracker",
    load_timeout = 120000,
    timeout = 30000,
    seed = 1,
    options = list(shiny.testmode = TRUE)
  )
}

tracker_login <- function(app) {
  app$set_inputs(`auth-user_id` = "acted", `auth-user_pwd` = "logapp")
  app$click("auth-go_auth")
  # shinymanager reloads the page after a successful login. The reload makes
  # the old idle watch fail, so poll for the requests page instead.
  ready <- FALSE
  for (i in seq_len(30)) {
    ready <- isTRUE(tryCatch(
      identical(app$get_js("document.querySelector('.requests-page') ? 'yes' : 'no'"), "yes"),
      error = function(e) FALSE
    ))
    if (ready) break
    Sys.sleep(1)
  }
  if (!ready) stop("Login did not reach the requests page.", call. = FALSE)
  app
}
