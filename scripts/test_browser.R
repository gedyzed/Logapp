# Browser suite only. Does not run unit or integration tests.
# Usage, from the project folder:
#   Rscript scripts/test_browser.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(root)
Sys.setenv(NOT_CRAN = "true")

source(file.path(root, "tests", "testthat", "helper-context.R"))
source(file.path(root, "tests", "testthat", "helper-driver.R"))
problem <- tracker_browser_problem()
if (nzchar(problem)) {
  cat(problem, "\n", sep = "")
  cat("Browser tests were not run.\n")
  quit(status = 1)
}

cat("====================================================\n")
cat("      EXPENSE TRACKER BROWSER TESTS                \n")
cat("====================================================\n\n")

started <- proc.time()
res <- testthat::test_dir(
  file.path(root, "tests", "testthat", "browser"),
  stop_on_failure = FALSE,
  reporter = "summary"
)
elapsed <- (proc.time() - started)[["elapsed"]]
df <- as.data.frame(res)
failed <- sum(df$failed > 0 | df$error > 0)
passed <- sum(df$passed)

cat("\n====================================================\n")
cat(sprintf("Browser assertions passed: %d\n", passed))
cat(sprintf("Browser test failures:     %d\n", failed))
cat(sprintf("Duration:                  %.2fs\n", elapsed))
cat("====================================================\n\n")

if (failed > 0) {
  cat("[FAILED] Browser tests failed.\n")
  quit(status = 1)
}
cat("[SUCCESS] Browser tests passed.\n")
quit(status = 0)
