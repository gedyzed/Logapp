# Run the expense-tracker suite via run_tests.R. Exit 1 when any test fails.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(root)
log_path <- file.path(root, "tests", "requirement-log.tsv")
if (file.exists(log_path)) file.remove(log_path)
Sys.setenv(TRACKER_REQUIREMENT_LOG = log_path, NOT_CRAN = "true")

source(file.path(root, "run_tests.R"))
