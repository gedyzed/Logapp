# Integration tests for Manage Settings [21]

test_that("[21] Manage add/edit validation: empty and duplicates show type='error', valid add persists", {
  fixture <- local_tracker_db("2026-11-06 13:00:00")
  box <- new.env(parent = emptyenv())

  # 1. Source code check: verify all empty and duplicate checks are type = 'error'
  root <- tracker_root()
  manage_src <- readLines(file.path(root, "R/features/manage_service.R"))
  expect_true(any(grepl('Enter a project code first.*type = "error"', manage_src)))
  expect_true(any(grepl('project code already exists.*type = "error"', manage_src)))
  expect_true(any(grepl('Choose a project and enter a budget line.*type = "error"', manage_src)))
  expect_true(any(grepl('budget line is already configured.*type = "error"', manage_src)))
  expect_true(any(grepl('Enter a supplier name first.*type = "error"', manage_src)))
  expect_true(any(grepl('supplier already exists.*type = "error"', manage_src)))
  expect_true(any(grepl('Project code is required.*type = "error"', manage_src)))
  expect_true(any(grepl('project code is already in use.*type = "error"', manage_src)))
  expect_true(any(grepl('Supplier name is required.*type = "error"', manage_src)))
  expect_true(any(grepl('supplier name already exists.*type = "error"', manage_src)))

  # 2. Runtime execution with mock notification spy in .GlobalEnv
  notifs <- list()
  mock_notify <- function(ui, ...) {
    args <- list(...)
    msg <- if (is.character(ui)) ui else as.character(ui)
    notifs <<- c(notifs, list(list(msg = msg, type = args$type %||% "default")))
  }
  assign("showNotification", mock_notify, envir = .GlobalEnv)
  on.exit(rm("showNotification", envir = .GlobalEnv), add = TRUE)

  shiny::testServer(tracker_manage_server, {
    cn <- DBI::dbConnect(RSQLite::SQLite(), fixture$path)
    on.exit(DBI::dbDisconnect(cn), add = TRUE)

    # Project Add: Empty & Duplicate
    session$setInputs(cfg_project_code = "   ", cfg_project_department = "REACH")
    session$setInputs(cfg_add_project = 1)
    session$flushReact()

    session$setInputs(cfg_project_code = "alpha", cfg_project_department = "REACH")
    session$setInputs(cfg_add_project = 2)
    session$flushReact()

    box$empty_proj_count <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM project_budgets WHERE trim(code) = ''")$n[[1]]
    box$post_alpha_proj <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM project_budgets WHERE lower(code) = 'alpha'")$n[[1]]

    # Budget Line Add: Empty & Duplicate
    session$setInputs(cfg_budget_project = "ALPHA", cfg_budget_line = "   ")
    session$setInputs(cfg_add_budget_line = 1)
    session$flushReact()

    session$setInputs(cfg_budget_project = "ALPHA", cfg_budget_line = "comms")
    session$setInputs(cfg_add_budget_line = 2)
    session$flushReact()

    box$empty_bl_count <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM budget_lines WHERE trim(code) = ''")$n[[1]]
    box$post_alpha_lines <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM budget_lines bl JOIN project_budgets p ON p.id = bl.project_budget_id WHERE p.code = 'ALPHA'")$n[[1]]

    # Supplier Add: Empty & Duplicate
    session$setInputs(cfg_supplier_name = "   ")
    session$setInputs(cfg_add_supplier = 1)
    session$flushReact()

    session$setInputs(cfg_supplier_name = "beacon travel")
    session$setInputs(cfg_add_supplier = 2)
    session$flushReact()

    box$empty_sup_count <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM suppliers WHERE trim(supplier_name) = ''")$n[[1]]
    box$post_beacon_count <- DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM suppliers WHERE supplier_name = 'Beacon Travel' COLLATE NOCASE")$n[[1]]

    # Project Edit: Empty & Duplicate
    session$setInputs(settings_catalog_table_rows_selected = 1L)
    session$setInputs(edit_project_selected = 1)
    session$flushReact()

    session$setInputs(edit_project_code = "   ", edit_project_department = "REACH", edit_project_location = "ADD")
    session$setInputs(save_project_edit = 1)
    session$flushReact()

    session$setInputs(edit_project_code = "fin01", edit_project_department = "REACH", edit_project_location = "ADD")
    session$setInputs(save_project_edit = 2)
    session$flushReact()

    # Supplier Edit: Empty & Duplicate
    session$setInputs(settings_supplier_table_rows_selected = 1L)
    session$setInputs(edit_supplier_selected = 1)
    session$flushReact()

    session$setInputs(edit_supplier_name = "   ")
    session$setInputs(save_supplier_edit = 1)
    session$flushReact()

    box$notifs <- notifs

    # Valid Adds: Project, Budget Line, Supplier
    session$setInputs(cfg_project_code = "TESTPROJ", cfg_project_department = "LOG")
    session$setInputs(cfg_add_project = 3)
    session$flushReact()

    session$setInputs(cfg_budget_project = "TESTPROJ", cfg_budget_line = "BL-01")
    session$setInputs(cfg_add_budget_line = 3)
    session$flushReact()

    session$setInputs(cfg_supplier_name = "Global Cargo")
    session$setInputs(cfg_add_supplier = 3)
    session$flushReact()

    box$saved_project <- DBI::dbGetQuery(cn, "SELECT code AS project_code, department_code AS department FROM project_budgets WHERE code = 'TESTPROJ'")
    box$saved_bl <- DBI::dbGetQuery(cn, "SELECT bl.code AS budget_line_code FROM budget_lines bl JOIN project_budgets p ON p.id = bl.project_budget_id WHERE p.code = 'TESTPROJ'")
    box$saved_supplier <- DBI::dbGetQuery(cn, "SELECT * FROM suppliers WHERE supplier_name = 'Global Cargo'")

    box$proj_codes <- project_codes_db()
    box$testproj_lines <- budget_catalog_db("TESTPROJ")$budget_line_code
    box$supplier_choices <- supplier_catalog_db()
  })

  expect_req("21", {
    expect_equal(box$empty_proj_count, 0L)
    expect_equal(box$post_alpha_proj, 1L)
    expect_equal(box$empty_bl_count, 0L)
    expect_equal(box$post_alpha_lines, 1L)
    expect_equal(box$empty_sup_count, 0L)
    expect_equal(box$post_beacon_count, 1L)

    types <- vapply(box$notifs, function(n) n$type, character(1))
    error_notifs <- box$notifs[types == "error"]
    expect_true(length(error_notifs) >= 7L)

    expect_equal(nrow(box$saved_project), 1L)
    expect_equal(box$saved_project$department[[1]], "LOG")
    expect_equal(nrow(box$saved_bl), 1L)
    expect_equal(box$saved_bl$budget_line_code[[1]], "BL-01")
    expect_equal(nrow(box$saved_supplier), 1L)

    expect_true("TESTPROJ" %in% box$proj_codes)
    expect_true("BL-01" %in% box$testproj_lines)
    expect_true("Global Cargo" %in% box$supplier_choices)
  })
})

