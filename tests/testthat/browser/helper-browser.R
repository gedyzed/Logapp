# Browser helpers. This directory is not part of run_tests.R.
# Run it with: Rscript scripts/test_browser.R

parent_helpers <- normalizePath(file.path(testthat::test_path(), ".."), winslash = "/", mustWork = TRUE)
source(file.path(parent_helpers, "helper-context.R"))
source(file.path(parent_helpers, "helper-fixture.R"))
source(file.path(parent_helpers, "helper-driver.R"))

tracker_wait_js <- function(app, expr, seconds = 20) {
  for (i in seq_len(seconds)) {
    ready <- isTRUE(tryCatch(
      identical(app$get_js(expr), "yes"),
      error = function(e) FALSE
    ))
    if (ready) return(invisible(TRUE))
    Sys.sleep(1)
  }
  stop("Timed out waiting for the page.", call. = FALSE)
}

tracker_table_text <- function(app) {
  app$get_js("document.querySelector('.request-table-card') ? document.querySelector('.request-table-card').innerText : ''")
}

tracker_open_tab <- function(app, value) {
  app$run_js(sprintf(
    "(function() {
      var link = document.querySelector(\"a[data-value='%s']\");
      if (link) link.click();
    })()",
    value
  ))
}

tracker_finance_has <- function(needle) {
  sprintf(
    "var root = document.querySelector('#fin_project_cards'); root && root.innerText.indexOf('%s') >= 0 ? 'yes' : 'no'",
    needle
  )
}

# The catalogue is alphabetical and shows 10 rows. Search so a new code on a later page still counts.
tracker_catalog_search <- function(app, needle) {
  app$run_js(sprintf(
    "(function() {
      var input = document.querySelector('#settings_catalog_table .dataTables_filter input');
      if (!input) return;
      input.value = '%s';
      input.dispatchEvent(new Event('input', {bubbles: true}));
      if (window.jQuery) $(input).trigger('input').trigger('keyup');
    })()",
    needle
  ))
}

tracker_catalog_shows <- function(needle) {
  sprintf(
    "var box = document.querySelector('#settings_catalog_table'); box && box.innerText.indexOf('%s') >= 0 ? 'yes' : 'no'",
    needle
  )
}

tracker_wait_catalog <- function(app, needle, seconds = 25) {
  for (i in seq_len(seconds)) {
    tracker_catalog_search(app, needle)
    ready <- isTRUE(tryCatch(
      identical(app$get_js(tracker_catalog_shows(needle)), "yes"),
      error = function(e) FALSE
    ))
    if (ready) return(invisible(TRUE))
    Sys.sleep(1)
  }
  stop("Timed out waiting for the catalogue.", call. = FALSE)
}

# The requests session polls and pushes inputs often enough that Shiny
# may never sit idle for the 500ms shinytest2 requires. Sample the flag directly.
tracker_install_busy_probe <- function(app) {
  app$run_js(
    "if (!window.__busyProbe) {
      window.__busyProbe = {busy: 0, idle: 0, started: performance.now()};
      $(document).on('shiny:busy.probe', function() { window.__busyProbe.busy += 1; });
      $(document).on('shiny:idle.probe', function() { window.__busyProbe.idle += 1; });
    }"
  )
}

tracker_busy_probe <- function(app) {
  raw <- app$get_js(
    "(function() {
      var probe = window.__busyProbe || {busy: 0, idle: 0, started: performance.now()};
      return JSON.stringify({
        busy: probe.busy,
        idle: probe.idle,
        ms: Math.round(performance.now() - probe.started),
        now_busy: document.documentElement.classList.contains('shiny-busy')
      });
    })()"
  )
  if (is.list(raw)) return(raw)
  jsonlite::fromJSON(raw)
}
