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
  
  manage_revision <- reactiveVal(0L)
  observe({
    if (exists("db_poll", mode = "function")) db_poll()
    manage_revision(isolate(manage_revision()) + 1L)
  })

  project_catalog_view <- reactive({
    manage_revision()
    invisible(data())
    rows <- db_get("SELECT id, code AS project_code, COALESCE(department_code, '') AS department,
                           '' AS location, COALESCE(last_checkpoint_at, '') AS last_deducted_at
                    FROM project_budgets ORDER BY lower(code)")
    lines <- db_get("SELECT project_budget_id, code FROM budget_lines")
    rows$budget_lines <- vapply(rows$id, function(id) {
      paste(lines$code[lines$project_budget_id == id], collapse = ", ")
    }, character(1))
    rows
  })

  supplier_catalog_view <- reactive({
    manage_revision()
    invisible(data())
    db_get("SELECT id, supplier_name, COALESCE(email, '') AS email, COALESCE(phone, '') AS phone, active
            FROM suppliers ORDER BY lower(supplier_name)")
  })

  people_catalog_view <- reactive({
    manage_revision()
    invisible(data())
    db_get("SELECT id, full_name, COALESCE(email, '') AS email, COALESCE(department_code, '') AS department, active
            FROM people ORDER BY lower(full_name)")
  })

  notification_catalog_view <- reactive({
    manage_revision()
    invisible(data())
    db_get("SELECT id, email, active AS weekly_report, 0 AS request_created, 0 AS status_changed, active
            FROM email_recipients ORDER BY lower(email)")
  })
  
  output$settings_catalog_table <- renderDT({
    catalog <- project_catalog_view()
    catalog$last_deducted_at <- NULL
    datatable(catalog, rownames = FALSE, selection = "single", filter = "none",
              options = list(pageLength = 10, lengthChange = FALSE, searching = TRUE,
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
              options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, dom = "ftip",
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
                          footer = tagList(modal_cancel_button("Cancel"), actionButton("save_project_edit", "Save changes", class = "btn-primary")),
                          easyClose = TRUE))
    project_edit_id(as.character(r$id[[1]]))
  })
  
  observeEvent(input$save_project_edit, {
    id <- project_edit_id(); code <- trimws(v(input$edit_project_code))
    if (is.null(id) || !nzchar(code)) { showNotification("Project code is required.", type = "error"); return() }
    if (nrow(db_get("SELECT id FROM project_budgets WHERE lower(code) = lower(?) AND id <> ?", code, id)) > 0) {
      showNotification("That project code is already in use.", type = "error"); return()
    }
    cn <- con(); on.exit(db_release(cn))
    dep <- ensure_department(cn, if (nzchar(v(input$edit_project_department))) v(input$edit_project_department) else "ACTED")
    dbExecute(cn, "UPDATE project_budgets SET code = ?, department_code = ?, updated_at = ? WHERE id = ?",
              params = list(toupper(code), dep, now(), id))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
    project_edit_id(NULL); removeModal()
    showNotification("Project updated.", type = "message")
  })
  
  observeEvent(input$edit_supplier_selected, {
    idx <- input$settings_supplier_table_rows_selected; sup <- supplier_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(sup)) { showNotification("Select a supplier row first.", type = "warning"); return() }
    r <- sup[idx[[1]], , drop = FALSE]; supplier_edit_id(as.character(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit supplier", r$supplier_name),
                          textInput("edit_supplier_name", "Supplier name", r$supplier_name, width = "100%"),
                          checkboxInput("edit_supplier_active", "Active supplier", value = as.integer(r$active) == 1),
                          footer = tagList(modal_cancel_button("Cancel"), actionButton("save_supplier_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_supplier_edit, {
    id <- supplier_edit_id(); name <- trimws(v(input$edit_supplier_name))
    if (is.null(id) || !nzchar(name)) { showNotification("Supplier name is required.", type = "error"); return() }
    if (nrow(db_get("SELECT id FROM suppliers WHERE lower(supplier_name) = lower(?) AND id <> ?", name, id)) > 0) { showNotification("That supplier name already exists.", type = "error"); return() }
    cn <- con(); on.exit(db_release(cn))
    dbExecute(cn, "UPDATE suppliers SET supplier_name=?,active=?,updated_at=? WHERE id=?",
              params = list(name, db_bool(isTRUE(input$edit_supplier_active)), now(), id))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
    supplier_edit_id(NULL); removeModal()
    updateSelectizeInput(session, "supplier", choices = c("", supplier_catalog_db()), selected = if (isTRUE(input$edit_supplier_active)) name else "",
                         options = list(create = TRUE, placeholder = "Type or choose a supplier"))
    showNotification("Supplier updated. Existing expense text was left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_person_selected, {
    idx <- input$settings_people_table_rows_selected; ppl <- people_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(ppl)) { showNotification("Select a person row first.", type = "warning"); return() }
    r <- ppl[idx[[1]], , drop = FALSE]; person_edit_id(as.character(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit person", r$full_name),
                          textInput("edit_person_name", "Full name", r$full_name, width = "100%"),
                          selectInput("edit_person_department", "Department", with_sel(c("Not specified" = "", DEPTS), r$department), selected = r$department, width = "100%"),
                          checkboxInput("edit_person_active", "Active person", value = as.integer(r$active) == 1),
                          footer = tagList(modal_cancel_button("Cancel"), actionButton("save_person_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_person_edit, {
    id <- person_edit_id(); name <- trimws(v(input$edit_person_name)); dept <- v(input$edit_person_department)
    if (is.null(id) || !nzchar(name)) { showNotification("Person name is required.", type = "warning"); return() }
    # email removed
    if (nrow(db_get("SELECT id FROM people WHERE lower(full_name) = lower(?) AND id <> ?", name, id)) > 0) { showNotification("That name already belongs to another person.", type = "warning"); return() }
    cn <- con(); on.exit(db_release(cn))
    dep <- if (nzchar(dept)) ensure_department(cn, dept) else NA_character_
    dbExecute(cn, "UPDATE people SET full_name=?,department_code=?,active=?,updated_at=? WHERE id=?",
              params = list(name, dep, db_bool(isTRUE(input$edit_person_active)), now(), id))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
    person_edit_id(NULL); removeModal(); showNotification("Person updated. Historical request names were left unchanged.", type = "message")
  })
  
  observeEvent(input$edit_email_selected, {
    idx <- input$settings_email_table_rows_selected; em <- notification_catalog_view()
    if (!length(idx) || idx[[1]] > nrow(em)) { showNotification("Select an email recipient row first.", type = "warning"); return() }
    r <- em[idx[[1]], , drop = FALSE]; email_edit_id(as.character(r$id[[1]]))
    showModal(modalDialog(title = paste("Edit recipient", r$email),
                          textInput("edit_notify_email", "Email address", r$email, width = "100%"),
                          checkboxInput("edit_notify_weekly", "Weekly request summary", value = as.integer(r$weekly_report) == 1),
                          checkboxInput("edit_notify_active", "Active recipient", value = as.integer(r$active) == 1),
                          footer = tagList(modal_cancel_button("Cancel"), actionButton("save_email_edit", "Save changes", class = "btn-primary")), easyClose = TRUE))
  })
  
  observeEvent(input$save_email_edit, {
    id <- email_edit_id(); email <- tolower(trimws(v(input$edit_notify_email)))
    if (is.null(id) || !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) { showNotification("Enter a valid email address.", type = "warning"); return() }
    if (nrow(db_get("SELECT id FROM email_recipients WHERE lower(email) = lower(?) AND id <> ?", email, id)) > 0) { showNotification("That notification email already exists.", type = "warning"); return() }
    cn <- con(); on.exit(db_release(cn))
    dbExecute(cn, "UPDATE email_recipients SET email=?, display_name=?, active=?, updated_at=? WHERE id=?",
              params = list(email, email, db_bool(isTRUE(input$edit_notify_active)), now(), id))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
    email_edit_id(NULL); removeModal(); showNotification("Notification recipient updated.", type = "message")
  })
  
  observeEvent(input$cfg_add_project, {
    code <- trimws(v(input$cfg_project_code))
    if (!nzchar(code)) {
      showNotification("Enter a project code first.", type = "error")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM project_budgets WHERE lower(code) = lower(?)", code)) > 0
    if (exists) {
      showNotification("That project code already exists. Choose it in the budget-line section instead.", type = "error")
      return()
    }
    cn <- con(); on.exit(db_release(cn))
    ensure_project_record(cn, code, v(input$cfg_project_department), "")
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
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
      showNotification("Choose a project and enter a budget line.", type = "error")
      return()
    }
    p_row <- db_get("SELECT id FROM project_budgets WHERE lower(code) = lower(?)", code)
    if (!nrow(p_row)) {
      showNotification("The selected project code is no longer available.", type = "error")
      return()
    }
    exists <- nrow(db_get("SELECT id FROM budget_lines WHERE project_budget_id = ? AND lower(code) = lower(?)",
                          p_row$id[[1]], line)) > 0
    if (exists) {
      showNotification("That budget line is already configured for this project.", type = "error")
      return()
    }
    cn <- con(); on.exit(db_release(cn))
    ensure_budget_line_record(cn, p_row$id[[1]], line)
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
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
      showNotification("Enter a supplier name first.", type = "error")
      return()
    }
    cn <- con(); on.exit(db_release(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM suppliers WHERE lower(supplier_name) = lower(?)", params = list(supplier_name))) > 0
    if (exists) {
      showNotification("That supplier already exists.", type = "error")
      return()
    }
    dbExecute(cn, "INSERT INTO suppliers(id, supplier_name, active, created_at, updated_at) VALUES(?,?,?,?,?)",
              params = list(new_uuid(), supplier_name, db_bool(TRUE), now(), now()))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
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
    cn <- con(); on.exit(db_release(cn))
    exists <- nrow(dbGetQuery(cn, "SELECT id FROM people WHERE lower(full_name) = lower(?)",
                              params = list(full_name))) > 0
    if (exists) {
      showNotification("That person already exists.", type = "warning")
      return()
    }
    dep <- if (nzchar(department)) ensure_department(cn, department) else NA_character_
    dbExecute(cn, "INSERT INTO people(id, full_name, department_code, active, created_at, updated_at) VALUES(?,?,?,?,?,?)",
              params = list(new_uuid(), full_name, dep, db_bool(TRUE), now(), now()))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
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
    cn <- con(); on.exit(db_release(cn))
    active_flag <- db_bool(isTRUE(input$cfg_notify_weekly))
    dbExecute(cn, "INSERT INTO email_recipients(id, display_name, email, active, created_at, updated_at)
      VALUES(?,?,?,?,?,?)
      ON CONFLICT(email) DO UPDATE SET display_name=excluded.display_name,
        active=excluded.active, updated_at=excluded.updated_at",
              params = list(new_uuid(), email, email, active_flag, now(), now()))
    bump_revision(cn)
    manage_revision(manage_revision() + 1L)
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
    cn <- con(); on.exit(db_release(cn))
    saved_defaults <- tryCatch({
      dbWithTransaction(cn, {
        dbExecute(cn, "UPDATE settings SET default_submitted_by = ?, default_country = ?, default_currency = ?, updated_at = ? WHERE id = 1",
                  params = list(focal, if (nzchar(country)) toupper(substr(country, 1, 3)) else "ETH", currency, now()))
        bump_revision(cn)
      })
      TRUE
    }, error = function(e) {
      showNotification(paste("Could not save defaults:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved_defaults) return()
    manage_revision(manage_revision() + 1L)
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
  
