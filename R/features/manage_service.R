<<<<<<< HEAD
  current_share_url <- function() {
    page <- trimws(as.character(isolate(input$nav) %||% "requests"))
    if (!nzchar(page)) page <- "requests"
    if (identical(page, "dashboards") && exists("share_dashboard_url", mode = "function")) return(share_dashboard_url())
    if (identical(page, "financial_update") && exists("fin_share_url", mode = "function")) return(fin_share_url())
    if (identical(page, "requests") && exists("share_url", mode = "function")) return(share_url())
    cd <- session$clientData
    host <- paste0(cd$url_hostname, if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?v=page&nav=", URLencode(page, reserved = TRUE))
  }
  show_share_modal <- function() {
    showModal(modalDialog(
      title = "Share this view",
      p(class = "chart-note", "Anyone with the link can open this view with the same filters. No request data is embedded in the URL."),
      div(class = "input-group",
          tags$input(id = "share_url", class = "form-control", readonly = NA,
                     value = current_share_url(), onclick = "this.select()"),
          tags$button(id = "copy_link", type = "button", class = "btn btn-primary", "Copy link")),
      footer = modalButton("Close"), easyClose = TRUE))
  }
  observeEvent(input$share, show_share_modal(), ignoreInit = TRUE)
  observeEvent(input$share_nav, show_share_modal(), ignoreInit = TRUE)
  
  # The gear icon now navigates to the full Manage workspace instead of opening a modal.
  observeEvent(input$settings_open, nav_select("nav", "manage", session = session))
  
  project_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT p.id, p.project_code, COALESCE(p.department,'') AS department,
      COALESCE(p.location,'') AS location,
      COALESCE(GROUP_CONCAT(bl.budget_line_code, ', '), '') AS budget_lines,
      COALESCE(p.last_deducted_at, '') AS last_deducted_at
      FROM projects p LEFT JOIN budget_lines bl ON bl.project_id = p.id
      GROUP BY p.id ORDER BY p.project_code COLLATE NOCASE")
  })
  
  supplier_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, supplier_name, COALESCE(email,'') AS email, COALESCE(phone,'') AS phone,
                   active FROM suppliers ORDER BY supplier_name COLLATE NOCASE")
  })
  
  people_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, full_name, COALESCE(email,'') AS email, COALESCE(department,'') AS department,
                   active FROM people ORDER BY full_name COLLATE NOCASE")
  })
  
  notification_catalog_view <- reactive({
    invisible(data())
    db_get("SELECT id, email, weekly_report, request_created, status_changed, active
            FROM notification_emails ORDER BY email COLLATE NOCASE")
  })
  
  output$settings_catalog_table <- renderDT({
    catalog <- project_catalog_view()
    catalog$last_deducted_at <- NULL
    datatable(catalog, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 8, lengthChange = FALSE, searching = TRUE,
                             scrollX = TRUE, dom = "ftip", columnDefs = list(
                               list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Project code", "Department", "Location", "Budget lines"))
  })
  
  output$settings_supplier_table <- renderDT({
    sup <- supplier_catalog_view() |>
      mutate(status = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, supplier_name, status)
    datatable(sup, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 8, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Supplier", "Status"))
  })
  
  output$settings_people_table <- renderDT({
    ppl <- people_catalog_view() |>
      mutate(status = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, full_name, department, status)
    datatable(ppl, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Full name", "Department", "Status"))
  })
  
  output$settings_email_table <- renderDT({
    em <- notification_catalog_view() |>
      mutate(weekly_label = ifelse(weekly_report == 1, "Yes", "No"),
             active_label = ifelse(active == 1, "Active", "Inactive")) |>
      select(id, email, weekly_label, active_label)
    datatable(em, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
                             columnDefs = list(list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE)),
                             language = dt_language),
              colnames = c("ID", "Email", "Weekly summary", "Status"))
  })
  
  # Edit forms are opened from a selected catalogue row. Records are updated in-place;
  # no request history is deleted when a catalogue label changes.
  observeEvent(input$edit_project_selected, {
    idx <- input$settings_catalog_table_rows_selected
    catalog <- project_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(catalog)) {
      showNotification("Select a project row first.", type = "warning"); return()
    }
    r <- catalog[idx[[1]], , drop = FALSE]
    showModal(modalDialog(title = paste("Edit project", r$project_code),
                          textInput("edit_project_code", "Project code", r$project_code, width = "100%"),
                          selectInput("edit_project_department", "Department", with_sel(c("Not specified" = "", DEPTS), r$department),
                                      selected = r$department, width = "100%"),
                          textInput("edit_project_location", "Default location", r$location, width = "100%"),
                          footer = tagList(modalButton("Cancel"), actionButton("save_project_edit", "Save changes", class = "btn-primary")),
                          easyClose = TRUE))
    project_edit_id(as.integer(r$id[[1]]))
  })
  
  observeEvent(input$save_project_edit, {
    id <- project_edit_id(); code <- trimws(v(input$edit_project_code))
    if (is.null(id) || !nzchar(code)) { showNotification("Project code is required.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE AND id <> ?", code, as.integer(id))) > 0) {
      showNotification("That project code is already in use.", type = "warning"); return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE projects SET project_code=?, department=?, location=?, updated=? WHERE id=?",
              params = list(code, if (nzchar(v(input$edit_project_department))) v(input$edit_project_department) else NA_character_,
                            if (nzchar(v(input$edit_project_location))) v(input$edit_project_location) else NA_character_, now(), as.integer(id)))
    project_edit_id(NULL); removeModal()
    showNotification("Project updated.", type = "message")
  })
  
  observeEvent(input$edit_supplier_selected, {
    idx <- input$settings_supplier_table_rows_selected; sup <- supplier_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(sup)) { showNotification("Select a supplier row first.", type = "warning"); return() }
    r <- sup[idx[[1]], , drop = FALSE]; supplier_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit supplier", r$supplier_name),
                          textInput("edit_supplier_name", "Supplier name", r$supplier_name, width = "100%"),
                          checkboxInput("edit_supplier_active", "Active supplier", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_supplier_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_supplier_edit, {
    id <- supplier_edit_id(); name <- trimws(v(input$edit_supplier_name))
    if (is.null(id) || !nzchar(name)) { showNotification("Supplier name is required.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM suppliers WHERE supplier_name = ? COLLATE NOCASE AND id <> ?", name, as.integer(id))) > 0) { showNotification("That supplier name already exists.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE suppliers SET supplier_name=?,active=?,updated=? WHERE id=?",
              params = list(name, as.integer(isTRUE(input$edit_supplier_active)), now(), as.integer(id)))
    supplier_edit_id(NULL); removeModal()
    updateSelectizeInput(session, "supplier", choices = c("", supplier_catalog_db()), selected = if (isTRUE(input$edit_supplier_active)) name else "",
                         options = list(create = TRUE, placeholder = "Type or choose a supplier"))
    showNotification("Supplier updated. Existing expense text was left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_person_selected, {
    idx <- input$settings_people_table_rows_selected; ppl <- people_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(ppl)) { showNotification("Select a person row first.", type = "warning"); return() }
    r <- ppl[idx[[1]], , drop = FALSE]; person_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit person", r$full_name),
                          textInput("edit_person_name", "Full name", r$full_name, width = "100%"),
                          selectInput("edit_person_department", "Department", with_sel(c("Not specified" = "", DEPTS), r$department), selected = r$department, width = "100%"),
                          checkboxInput("edit_person_active", "Active person", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_person_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_person_edit, {
    id <- person_edit_id(); name <- trimws(v(input$edit_person_name)); dept <- v(input$edit_person_department)
    if (is.null(id) || !nzchar(name)) { showNotification("Person name is required.", type = "warning"); return() }
    # email removed
    if (nrow(db_get("SELECT id FROM people WHERE full_name = ? COLLATE NOCASE AND id <> ?", name, as.integer(id))) > 0) { showNotification("That name already belongs to another person.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE people SET full_name=?,department=?,active=?,updated=? WHERE id=?",
              params = list(name, if (nzchar(dept)) dept else NA_character_, as.integer(isTRUE(input$edit_person_active)), now(), as.integer(id)))
    person_edit_id(NULL); removeModal(); showNotification("Person updated. Historical request names were left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_email_selected, {
    idx <- input$settings_email_table_rows_selected; em <- notification_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(em)) { showNotification("Select an email recipient row first.", type = "warning"); return() }
    r <- em[idx[[1]], , drop = FALSE]; email_edit_id(as.integer(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit recipient", r$email),
                          textInput("edit_notify_email", "Email address", r$email, width = "100%"),
                          checkboxInput("edit_notify_weekly", "Weekly request summary", value = as.integer(r$weekly_report) == 1),
                          checkboxInput("edit_notify_active", "Active recipient", value = as.integer(r$active) == 1),
                          footer = tagList(modalButton("Cancel"), actionButton("save_email_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_email_edit, {
    id <- email_edit_id(); email <- tolower(trimws(v(input$edit_notify_email)))
    if (is.null(id) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) { showNotification("Enter a valid email address.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM notification_emails WHERE email = ? COLLATE NOCASE AND id <> ?", email, as.integer(id))) > 0) { showNotification("That notification email already exists.", type = "warning"); return() }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "UPDATE notification_emails SET email=?,weekly_report=?,request_created=0,status_changed=0,active=?,updated=? WHERE id=?",
              params = list(email, as.integer(isTRUE(input$edit_notify_weekly)), as.integer(isTRUE(input$edit_notify_active)), now(), as.integer(id)))
    email_edit_id(NULL); removeModal(); showNotification("Notification recipient updated.", type = "message")
  })
  
  observeEvent(input$cfg_add_project, {
    code <- trimws(v(input$cfg_project_code))
    if (!nzchar(code)) {
      showNotification("Enter a project code first.", type = "warning")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE", code)) > 0
    if (exists) {
      showNotification("That project code already exists. Choose it in the budget-line section instead.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    ensure_project_record(cn, code, v(input$cfg_project_department), "")
    updateTextInput(session, "cfg_project_code", value = "")
    codes <- project_codes_db()
    updateSelectInput(session, "cfg_budget_project",
                      choices = c("Select project" = "", setNames(codes, codes)), selected = code)
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(codes, codes)),
                         selected = isolate(input$project_code) %||% "")
    # Refresh the dashboard filter from the catalogue immediately, not after the next poll.
    selected_projects <- isolate(input$f_project) %||% character()
    updateSelectizeInput(session, "f_project", choices = codes, selected = selected_projects)
    bchoices <- budget_filter_choices(selected_projects)
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = intersect(isolate(input$f_budget_line) %||% character(), unname(bchoices)),
                         options = list(placeholder = if (length(selected_projects))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    showNotification(paste("Project", code, "added."), type = "message")
  })
  
  observeEvent(input$cfg_add_budget_line, {
    code <- trimws(v(input$cfg_budget_project))
    line <- trimws(v(input$cfg_budget_line))
    if (!nzchar(code) || !nzchar(line)) {
      showNotification("Choose a project and enter a budget line.", type = "warning")
      return()
    }
    p_row <- db_get("SELECT id FROM projects WHERE project_code = ? COLLATE NOCASE", code)
    if (!nrow(p_row)) {
      showNotification("The selected project code is no longer available.", type = "error")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM budget_lines WHERE project_id = ? AND budget_line_code = ? COLLATE NOCASE",
                          as.integer(p_row$id[[1]]), line)) > 0
    if (exists) {
      showNotification("That budget line is already configured for this project.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    ensure_budget_line_record(cn, as.integer(p_row$id[[1]]), line)
    updateTextInput(session, "cfg_budget_line", value = "")
    if (identical(tolower(v(isolate(input$project_code))), tolower(code))) {
      lines <- budget_catalog_db(code)$budget_line_code
      updateSelectizeInput(session, "budget_line", choices = c("Select budget line" = "", setNames(lines, lines)),
                           selected = "", options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
    }
    # Refresh the dependent dashboard budget-line filter directly after the insert.
    selected_projects <- isolate(input$f_project) %||% character()
    bchoices <- budget_filter_choices(selected_projects)
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = intersect(isolate(input$f_budget_line) %||% character(), unname(bchoices)),
                         options = list(placeholder = if (length(selected_projects))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    showNotification(paste("Budget line added to", code, "."), type = "message")
  })
  
  observeEvent(input$cfg_add_supplier, {
    supplier_name <- trimws(v(input$cfg_supplier_name))
    if (!nzchar(supplier_name)) {
      showNotification("Enter a supplier name first.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM suppliers WHERE supplier_name = ? COLLATE NOCASE", params = list(supplier_name))) > 0
    if (exists) {
      showNotification("That supplier already exists.", type = "warning")
      return()
    }
    dbExecute(cn, "INSERT INTO suppliers(supplier_name,email,phone,active,created,updated) VALUES(?,?,?,?,?,?)",
              params = list(supplier_name, NA_character_, NA_character_, 1L, now(), now()))
    updateTextInput(session, "cfg_supplier_name", value = "")
    updateSelectizeInput(session, "supplier", choices = c("", supplier_catalog_db()), selected = supplier_name,
                         options = list(create = TRUE, placeholder = "Type or pick"))
    showNotification(paste("Supplier", supplier_name, "added."), type = "message")
  })
  
  observeEvent(input$cfg_add_person, {
    full_name <- trimws(v(input$cfg_person_name))
    department <- v(input$cfg_person_department)
    if (!nzchar(full_name)) {
      showNotification("Enter the person's name first.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM people WHERE full_name = ? COLLATE NOCASE",
                              params = list(full_name))) > 0
    if (exists) {
      showNotification("That person already exists.", type = "warning")
      return()
    }
    dbExecute(cn, "INSERT INTO people(full_name,department,active,created,updated) VALUES(?,?,?,?,?)",
              params = list(full_name, if (nzchar(department)) department else NA_character_, 1L, now(), now()))
    updateTextInput(session, "cfg_person_name", value = "")
    updateSelectInput(session, "cfg_person_department", selected = "")
    showNotification(paste(full_name, "added to People."), type = "message")
  })
  
  observeEvent(input$cfg_add_notification_email, {
    email <- tolower(trimws(v(input$cfg_notify_email)))
    if (!nzchar(email) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
      showNotification("Enter a valid notification email address.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, "INSERT INTO notification_emails(email,weekly_report,request_created,status_changed,active,created,updated)
      VALUES(?,?,0,0,1,?,?)
      ON CONFLICT(email) DO UPDATE SET weekly_report=excluded.weekly_report,
        request_created=0,status_changed=0,
        active=excluded.active,updated=excluded.updated",
              params = list(email, as.integer(isTRUE(input$cfg_notify_weekly)), now(), now()))
    updateTextInput(session, "cfg_notify_email", value = "")
    updateCheckboxInput(session, "cfg_notify_weekly", value = TRUE)
    showNotification("Notification recipient saved.", type = "message")
  })
  
  observeEvent(input$cfg_save_defaults, {
    focal <- trimws(v(input$cfg_default_submitter))
    country <- trimws(v(input$cfg_default_country))
    currency <- v(input$cfg_default_currency)
    if (!nzchar(focal) || !currency %in% CUR) {
      showNotification("Enter a logistics focal point and choose an available default currency.", type = "warning")
      return()
    }
    cn <- con(); on.exit(dbDisconnect(cn))
    dbBegin(cn)
    saved_defaults <- tryCatch({
      values <- list(submitted_by = focal, country = country, currency = currency)
      for (key in names(values)) {
        dbExecute(cn, "INSERT OR REPLACE INTO app_defaults(setting_key, setting_value, updated) VALUES(?,?,?)",
                  params = list(key, values[[key]], now()))
      }
      dbCommit(cn)
      TRUE
    }, error = function(e) {
      try(dbRollback(cn), silent = TRUE)
      showNotification(paste("Could not save defaults:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved_defaults) return()
    DEFAULTS <<- read_app_defaults()
    showNotification("Request defaults saved to the database.", type = "message")
    # Apply the new defaults immediately to this session; future sessions also read them from SQLite.
    updateSelectizeInput(session, "submitted_by", selected = focal,
                         choices = with_sel(uniq(c(data()$submitted_by, focal)), focal))
    updateSelectizeInput(session, "country", selected = country,
                         choices = with_sel(uniq(c(data()$country, country)), country))
    updateSelectInput(session, "currency", selected = currency)
    updateRadioButtons(session, "cur", selected = currency)
  })
  
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/manage_service.R
# Owner: Friend
# Purpose: Own reference-data and recipient validation and management rules.
# Expected code: List-choice readers, create/update/deactivate/reactivate operations, normalization and conflict messages.
# Connections: Calls manage_repo; exposes read contracts to Requests/Insights; FX refresh delegated to fx_service.
# Boundary: No SQL in page; do not deactivate budgets with open/unapplied/reserved work. Contact identity is not authentication.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
