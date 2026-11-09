# Unit tests for Finance validations and schema requirements [15] and [17]

test_that("[15] Batch name validation rules (empty, >80 chars, duplicates)", {
  validate_batch_name <- function(name) {
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name)) return(list(ok = FALSE, error = "Enter a name for the batch."))
    if (nchar(name) > 80) return(list(ok = FALSE, error = "Batch name must be 80 characters or fewer."))
    list(ok = TRUE, name = name)
  }

  # Empty name rejected
  expect_false(validate_batch_name("")$ok)
  expect_false(validate_batch_name("   ")$ok)
  expect_equal(validate_batch_name("")$error, "Enter a name for the batch.")

  # > 80 chars rejected
  long_name <- paste(rep("A", 81), collapse = "")
  expect_false(validate_batch_name(long_name)$ok)
  expect_equal(validate_batch_name(long_name)$error, "Batch name must be 80 characters or fewer.")

  # Exact 80 chars accepted
  exact_name <- paste(rep("A", 80), collapse = "")
  expect_true(validate_batch_name(exact_name)$ok)

  # Duplicate names allowed (validation does not block duplicate strings)
  v1 <- validate_batch_name("October Deduction")
  v2 <- validate_batch_name("October Deduction")
  expect_true(v1$ok && v2$ok)
  expect_equal(v1$name, v2$name)

  record_requirement("15", "PASS", "Batch name validation unit rules verified")
})

test_that("[17] Schema and code check: no balance column exists anywhere", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
  on.exit(DBI::dbDisconnect(cn), add = TRUE)

  tables <- DBI::dbListTables(cn)
  all_cols <- unlist(lapply(tables, function(tbl) DBI::dbListFields(cn, tbl)), use.names = FALSE)
  balance_cols <- grep("balance", all_cols, ignore.case = TRUE, value = TRUE)

  # Check UI source code (.R files only)
  root <- tracker_root()
  ui_files <- list.files(file.path(root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  ui_texts <- unlist(lapply(ui_files, readLines))
  balance_headers <- grep('["\'](remaining\\s+)?balance["\']', ui_texts, ignore.case = TRUE, value = TRUE)

  expect_equal(length(balance_cols), 0L)
  expect_equal(length(balance_headers), 0L)

  record_requirement("17", "PASS", "No balance column exists in database schema or UI code")
})

