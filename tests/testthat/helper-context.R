# Shared paths for the expense-tracker suite. Do not source the app here.

tracker_root <- function() {
  p <- normalizePath(testthat::test_path(), winslash = "/", mustWork = FALSE)
  for (i in 1:6) {
    if (file.exists(file.path(p, "app.R")) || file.exists(file.path(p, "R", "core", "config.R"))) {
      return(p)
    }
    p <- dirname(p)
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

tracker_requirement_log <- function() {
  override <- Sys.getenv("TRACKER_REQUIREMENT_LOG", unset = "")
  if (nzchar(override)) return(override)
  file.path(tracker_root(), "tests", "requirement-log.tsv")
}
