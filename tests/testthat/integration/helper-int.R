# Load parent test helpers and application core for integration tests
helpers_dir <- normalizePath(file.path(testthat::test_path("..")), winslash = "/", mustWork = TRUE)
for (h in list.files(helpers_dir, pattern = "^helper-.*\\.R$", full.names = TRUE)) {
  source(h)
}

# Ensure isolated database fixture is ready
if (!exists("RATES", envir = .GlobalEnv)) {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
}
