# Expense Tracker Test Suite Runner
# Runs both unit and integration test layers without browser/e2e dependencies.

cat("====================================================\n")
cat("      EXPENSE TRACKER TEST SUITE RUNNER             \n")
cat("====================================================\n\n")

start_all <- proc.time()

# 1. Run Unit Tests (pure functions & calculation logic)
cat(">>> LAYER 1: Running Unit Tests (tests/testthat/unit)...\n")
unit_start <- proc.time()
unit_res <- testthat::test_dir("tests/testthat/unit", stop_on_failure = FALSE)
unit_elapsed <- (proc.time() - unit_start)[["elapsed"]]
cat(sprintf("Layer 1 completed in %.2f seconds.\n\n", unit_elapsed))

# 2. Run Integration Tests (shiny::testServer with isolated DB)
cat(">>> LAYER 2: Running Integration Tests (tests/testthat/integration)...\n")
int_start <- proc.time()
int_res <- testthat::test_dir("tests/testthat/integration", stop_on_failure = FALSE)
int_elapsed <- (proc.time() - int_start)[["elapsed"]]
cat(sprintf("Layer 2 completed in %.2f seconds.\n\n", int_elapsed))

total_elapsed <- (proc.time() - start_all)[["elapsed"]]

# Summary
unit_df <- as.data.frame(unit_res)
int_df <- as.data.frame(int_res)

unit_failed <- sum(unit_df$failed > 0 | unit_df$error > 0)
int_failed <- sum(int_df$failed > 0 | int_df$error > 0)
total_passed <- sum(unit_df$passed) + sum(int_df$passed)
total_failed <- unit_failed + int_failed

cat("====================================================\n")
cat("                  TEST SUMMARY                      \n")
cat("====================================================\n")
cat(sprintf("Unit tests passed:        %d (in %.2fs)\n", sum(unit_df$passed), unit_elapsed))
cat(sprintf("Integration tests passed: %d (in %.2fs)\n", sum(int_df$passed), int_elapsed))
cat(sprintf("Total assertions passed:  %d\n", total_passed))
cat(sprintf("Total test failures:      %d\n", total_failed))
cat(sprintf("Total duration:           %.2fs\n", total_elapsed))
cat("====================================================\n\n")

if (total_failed > 0) {
  cat("[FAILED] Test suite encountered failures.\n")
  quit(status = 1)
} else {
  cat("[SUCCESS] All unit and integration tests passed cleanly!\n")
  quit(status = 0)
}
