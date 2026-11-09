# Requirement results are appended as id, status, note. The report keeps the worst status.

record_requirement <- function(id, status, note = "") {
  status <- match.arg(status, c("PASS", "FAIL", "KNOWN BUG", "NOT TESTABLE", "PARTIAL (manual)"))
  note <- gsub("[\r\n\t]+", " ", paste(note, collapse = " "))
  dir.create(dirname(tracker_requirement_log()), recursive = TRUE, showWarnings = FALSE)
  cat(paste(id, status, note, sep = "\t"), "\n", file = tracker_requirement_log(), append = TRUE, sep = "")
  invisible(status)
}

record_partial <- function(id, note = "") {
  record_requirement(id, "PARTIAL (manual)", note)
}

expect_req <- function(id, code) {
  force(code)
  record_requirement(id, "PASS")
}

expect_partial <- function(id, code, note = "") {
  force(code)
  record_partial(id, note)
}

expect_known_bug <- function(id, code, note) {
  passed <- tryCatch({ force(code); TRUE }, error = function(e) FALSE)
  if (isTRUE(passed)) {
    record_requirement(id, "PASS")
    testthat::succeed()
  } else {
    record_requirement(id, "KNOWN BUG", note)
    testthat::expect_failure(force(code))
  }
}

expect_not_testable <- function(id, note) {
  record_requirement(id, "NOT TESTABLE", note)
  testthat::skip(note)
}
