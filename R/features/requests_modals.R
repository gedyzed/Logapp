  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q) || is.null(q[["v"]])) return()
    vals <- function(k) if (is.null(q[[k]]) || !nzchar(q[[k]])) character()
    else strsplit(q[[k]], "|", fixed = TRUE)[[1]]

    if (identical(q[["v"]], "page")) {
      if (!is.null(q[["nav"]]) && nzchar(q[["nav"]])) nav_select("nav", q[["nav"]], session = session)
      return()
    }

    if (identical(q[["v"]], "fin")) {
      nav_select("nav", "financial_update", session = session)
      if (!is.null(q[["cur"]])) updateSelectInput(session, "global_cur", selected = q[["cur"]])
      if (!is.null(q[["tab"]]) && exists("fin_history_tab", mode = "function")) fin_history_tab(q[["tab"]])
      return()
    }

    if (identical(q[["v"]], "dash")) {
      nav_select("nav", "dashboards", session = session)
      if (!is.null(q[["cur"]])) updateSelectInput(session, "global_cur", selected = q[["cur"]])
      if (!is.null(q[["period"]])) updateSelectInput(session, "dash_period_type", selected = q[["period"]])
      if (!is.null(q[["yr"]])) updateSelectInput(session, "dash_year", selected = q[["yr"]])
      if (!is.null(q[["m"]])) updateSelectInput(session, "dash_month", selected = q[["m"]])
      if (!is.null(q[["q"]])) updateSelectInput(session, "dash_quarter", selected = q[["q"]])
      if (!is.null(q[["p"]])) {
        psel <- vals("p")
        updateSelectizeInput(session, "dash_project", selected = psel)
      }
      if (!is.null(q[["d"]])) {
        dsel <- vals("d")
        updateSelectizeInput(session, "dash_department", selected = dsel)
      }
      if (!is.null(q[["chart"]])) updateSelectInput(session, "dash_chart_select", selected = q[["chart"]])
      if (!is.null(q[["bstat"]])) updateSelectInput(session, "breakdown_status", selected = q[["bstat"]])
      if (!is.null(q[["bdim"]])) updateSelectInput(session, "dim", selected = q[["bdim"]])
      if (!is.null(q[["bp"]])) updateSelectInput(session, "breakdown_project", selected = q[["bp"]])
      return()
    }

    if (!(q[["v"]] %in% c("r", "r2"))) return()
    if (!is.null(q[["cur"]])) updateSelectInput(session, "global_cur", selected = q[["cur"]])
    if (!is.null(q[["find"]])) updateTextInput(session, "f_query", value = q[["find"]])
    if (!is.null(q[["st"]])) set_request_status(q[["st"]])
    d <- isolate(data())
    if (!is.null(q[["m"]]) && !is.na(q[["m"]])) {
      updateSelectInput(session, "f_month", selected = q[["m"]])
    } else {
      # A shared view without an explicit month represents an unfiltered
      # date view; do not reapply the fresh-session current-month default.
      updateSelectInput(session, "f_month", selected = "")
    }
    set <- function(id, key, choices) {
      selected <- vals(key)
      if (length(selected)) updateSelectizeInput(session, id,
                                                 choices = with_sel(choices, selected), selected = selected)
    }
    set("f_project", "p", project_codes_db())
    set("f_person", "r", uniq(d$requested_by))
    set("f_submitter", "s", uniq(d$submitted_by))
    set("f_week", "w", week_choices(d, if (is.null(q[["m"]])) "" else q[["m"]]))
    bchoices <- budget_filter_choices(vals("p"))
    bsel <- intersect(vals("b"), unname(bchoices))
    updateSelectizeInput(session, "f_budget_line", choices = bchoices, selected = bsel)
  }, once = TRUE)
  
  # ---- New request is a full page, not a modal ----
  observeEvent(input$new_request, {
    DEFAULTS <<- read_app_defaults()
    touched(character()); show_all(FALSE)
    updateSelectizeInput(session, "submitted_by", selected = DEFAULTS$submitted_by,
                         choices = with_sel(uniq(c(isolate(data())$submitted_by, DEFAULTS$submitted_by)), DEFAULTS$submitted_by))
    updateSelectizeInput(session, "country", selected = DEFAULTS$country,
                         choices = with_sel(uniq(c(isolate(data())$country, DEFAULTS$country)), DEFAULTS$country))
    updateSelectInput(session, "currency", selected = DEFAULTS$currency)
    nav_select("nav", "new", session = session)
  })
  observeEvent(input$cancel_new, nav_select("nav", "requests", session = session))
  observeEvent(input$cancel_new_bottom, nav_select("nav", "requests", session = session))
  
  output$new_form_ui <- renderUI({
    codes <- project_codes_db()
    div(class = "nr",
        div(class = "form-section-title", "Request details"),
        div(class = "fg",
            col(12, textAreaInput("description", R("Description"), rows = 2, width = "100%")),
            col(4, cached("requested_by", "Requested by")),
            col(4, selectizeInput("project_code", R("Project code"),
                                  choices = c("Select project" = "", setNames(codes, codes)),
                                  selected = "", width = "100%",
                                  options = list(create = FALSE, placeholder = "Choose a configured project"))),
            col(4, selectizeInput("budget_line", R("Budget line"), choices = character(),
                                  selected = "", width = "100%",
                                  options = list(create = FALSE, placeholder = "Choose project first"))),
            col(4, pick("department", "Department", DEPTS)),
            col(4, cached("location", "Location")),
            col(4, selectizeInput("supplier", R("Supplier"),
                                  choices = c("" , setNames(supplier_catalog_db(), supplier_catalog_db())),
                                  selected = "", width = "100%",
                                  options = list(create = TRUE, placeholder = "Type or choose a supplier"))),
            col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
            col(4, pick("currency", "Currency", CUR))
        ),
        div(class = "form-section-rule"),
        div(class = "form-section-title", "Optional details"),
        div(class = "fg",
            col(4, cached("country", "Country", req = FALSE)),
            col(4, numericInput("quantity", "Quantity", NA, min = 0, width = "100%")),
            col(4, cached("unit", "Unit", req = FALSE)),
            col(4, div(style = "padding-top:1.9rem", checkboxInput("framework", "Framework agreement"))),
            col(6, cached("submitted_by", "Submitted by (focal point)", req = FALSE)),
            col(6, date_in("of_date", "Order date", Sys.Date(), width = "100%")),
            col(12, textAreaInput("remarks", "Remarks", rows = 2, width = "100%"))
        )
    )
  })
  
  # ---- new request: required fields (TRUE = filled) ----
  valid <- reactive(c(
    description  = nzchar(v(input$description)),
    requested_by = nzchar(v(input$requested_by)),
    supplier     = nzchar(v(input$supplier)),
    project_code = nzchar(v(input$project_code)),
    budget_line  = nzchar(v(input$budget_line)),
    department   = nzchar(v(input$department)),
    location     = nzchar(v(input$location)),
    amount       = has_num(input$amount) && input$amount > 0,
    currency     = nzchar(v(input$currency))
  ))
  
  observeEvent(input$touched, touched(union(touched(), input$touched)))
  
  # red = required but empty (after leaving the field, or after pressing Add)
  observe({
    bad  <- names(valid())[!valid()]
    show <- if (show_all()) bad else intersect(bad, touched())
    session$sendCustomMessage("mark", list(ids = as.list(show)))
  })
  
  waiver_val <- reactive(if (isTRUE(input$framework)) "Framework agreement" else "No")
  odate <- reactive(if (has_date(input$of_date)) input$of_date else Sys.Date())
  
  form_rec <- reactive(list(
    project_code = v(input$project_code), budget_line = v(input$budget_line),
    country = v(input$country), location = v(input$location),
    department = v(input$department), order_date = as.character(odate())))
  
  # picking a project suggests its department and location (still editable)
  observeEvent(input$project_code, {
    pc <- v(input$project_code); req(nzchar(pc))
    p <- db_get("SELECT department, location FROM projects WHERE project_code = ?", pc)
    if (!nrow(p)) return()
    if (nzchar(v(p$department)))
      updateSelectInput(session, "department", selected = p$department)
    if (nzchar(v(p$location)))
      updateSelectizeInput(session, "location",
                           choices = c("", union(cache_choices(isolate(data()), "location"), p$location)),
                           selected = p$location,
                           options = list(create = TRUE, placeholder = "Type or pick"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$project_code, {
    pc <- v(input$project_code)
    lines <- if (nzchar(pc)) budget_catalog_db(pc)$budget_line_code else character()
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = "", setNames(lines, lines)),
                         selected = character(0),
                         options = list(create = FALSE, placeholder = if (nzchar(pc)) "Choose a mapped budget line" else "Choose project first"))
  }, ignoreInit = TRUE)
  
  observe({
    invisible(data())
    codes <- project_codes_db()
    current_project <- isolate(input$project_code) %||% ""
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(codes, codes)), selected = current_project,
                         options = list(create = FALSE, placeholder = "Choose a configured project"))
    lines <- if (nzchar(v(current_project))) budget_catalog_db(current_project)$budget_line_code else character()
    current_line <- isolate(input$budget_line) %||% ""
    selected_line <- if (current_line %in% lines) current_line else ""
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = "", setNames(lines, lines)), selected = selected_line,
                         options = list(create = FALSE, placeholder = if (nzchar(v(current_project))) "Choose a mapped budget line" else "Choose project first"))
  })
  
  output$preview <- renderUI({
    of  <- mk_ref(form_rec(), "OF")
    cur <- v(input$currency); amt <- input$amount
    shown <- function(x) if (is.na(x)) "\u2014" else x
    top <- if (!nzchar(cur) || !has_num(amt)) {
      p(class = "muted", "Enter amount and currency to see the scenario.")
    } else {
      usd <- amt / RATES[[cur]]
      s   <- scenario(usd, waiver_val())
      tagList(
        p(span(paste("Scenario", s), class = "badge badge-scen"), " ",
          span(SCEN_NAME[[s]], class = "muted")),
        p(class = "mb-2", tags$b("Expected time: "), LEAD[[s]]),
        tags$table(class = "table table-sm",
                   lapply(CUR, function(k)
                     tags$tr(tags$td(k, class = "muted"),
                             tags$td(money(usd * RATES[[k]]), style = "text-align:right"))))
      )
    }
    div(top,
        p(class = "muted mb-0", "Order form"), p(shown(of), class = "ref"),
        p(class = "muted mb-0", "Purchase order"),
        p("Generated when status moves from Under review to Pending", class = "ref muted"))
  })
  
  # back to the starting values (defaults prefilled, the rest empty)
  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    DEFAULTS <<- read_app_defaults()
    d <- isolate(data())
    for (id in names(CACHE)) {
      val <- DEFAULTS[[id]] %||% ""
      ch <- cache_choices(d, id)
      if (identical(id, "supplier")) ch <- uniq(c(ch, supplier_catalog_db()))
      ch <- union(ch, val[nzchar(val)])
      updateSelectizeInput(session, id, choices = c("", ch), selected = val,
                           options = list(create = TRUE, placeholder = "Type or pick"))
    }
    updateSelectizeInput(session, "project_code",
                         choices = c("Select project" = "", setNames(project_codes_db(), project_codes_db())), selected = "")
    updateSelectizeInput(session, "budget_line", choices = character(), selected = "",
                         options = list(create = FALSE, placeholder = "Choose project first"))
    for (id in c("department", "currency"))
      updateSelectInput(session, id, selected = DEFAULTS[[id]] %||% "")
    updateCheckboxInput(session, "framework", value = FALSE)
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    suppressWarnings(updateDateInput(session, "of_date", value = Sys.Date()))
    updateTextAreaInput(session, "remarks", value = "")
    touched(character()); show_all(FALSE)
  }
  observeEvent(input$clear, reset_form())
  
  observeEvent(input$add, {
    if (!all(valid())) {      # button stays active: show what is missing instead
      show_all(TRUE)
      session$sendCustomMessage("scrollbad", list())
      showNotification("Some required fields are empty.", type = "warning")
      return()
    }
    cur  <- input$currency
    rate <- RATES[[cur]]
    rec  <- form_rec()
    row <- list(
      description = v(input$description), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$country),
      location = rec$location, department = rec$department,
      reason = NA_character_, order_form = mk_ref(rec, "OF"), po = NA_character_,
      requested_by = v(input$requested_by),
      submitted_by = if (nzchar(v(input$submitted_by))) v(input$submitted_by)
      else DEFAULTS$submitted_by,
      status = "under_review", waiver = waiver_val(), supplier = v(input$supplier),
      unit = nz(input$unit),
      quantity = if (has_num(input$quantity)) input$quantity else NA_real_,
      amount = input$amount, currency = cur,
      usd = input$amount / rate, rate_used = rate,
      order_date = rec$order_date, po_date = NA_character_,
      payment_date = NA_character_, remarks = nz(input$remarks),
      created = now(), updated = now())
    cn <- con(); on.exit(dbDisconnect(cn))
    supplier_name <- trimws(v(row$supplier))
    if (nzchar(supplier_name)) {
      dbExecute(cn, "INSERT OR IGNORE INTO suppliers(supplier_name,active,created,updated) VALUES(?,1,?,?)",
                params = list(supplier_name, now(), now()))
    }
    saved <- tryCatch({ insert_expense_row(cn, row); TRUE }, error = function(e) {
      showNotification(paste("Request was not saved:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved) return()
    reset_form()
    nav_select("nav", "requests", session = session)
    showNotification("Request added.", type = "message")
  })
  
  # ---- Open an individual request when its table row is clicked ----
  render_request_modal <- function(id, edit_mode = FALSE) {
    r <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(r)) {
      showNotification("That request is no longer available. Refresh the table and try again.", type = "warning")
      return()
    }
    r <- r[1, , drop = FALSE]
    
    reference_record <- list(
      project_code = v(r$project_code), budget_line = v(r$budget_line),
      country = v(r$country), location = v(r$location), department = v(r$department),
      order_date = v(r$order_date), reason = v(r$reason), po_date = v(r$po_date)
    )
    order_ref <- v(r$order_form)
    if (!nzchar(order_ref)) order_ref <- v(mk_ref(reference_record, "OF"))
    if (!nzchar(order_ref)) order_ref <- "Unavailable — check project, budget line, location, department and order date"
    
    po_ref <- v(r$po)
    po_is_preview <- FALSE
    if (!nzchar(po_ref) && v(r$status) %in% c("pending", "paid")) {
      candidate_po <- v(mk_ref(reference_record, "PO"))
      if (nzchar(candidate_po)) {
        po_ref <- candidate_po
        po_is_preview <- TRUE
      }
    }
    if (!nzchar(po_ref)) {
      po_ref <- if (v(r$status) %in% c("pending", "paid")) {
        "Not generated — provide a PO reason and PO date, then save."
      } else {
        "Generated when the request moves from Under review to Pending."
      }
    }
    
    if (!edit_mode) {
      showModal(modalDialog(
        title = div(class = "d-flex justify-content-between align-items-center w-100 pe-3",
                    span(paste0("Request #", r$id)),
                    span(class = paste0("badge status-pill status-", gsub(" ", "-", v(r$status))), status_label(v(r$status)))),
        size = "l", easyClose = TRUE,
        div(class = "request-view-container",
            div(class = "mb-2 pb-2 border-bottom",
                div(class = "text-muted small", "Amount"),
                div(class = "fs-4 fw-bold", paste(r$currency, money(r$amount)))
            ),
            div(class = "form-section-title", "Request details"),
            div(class = "request-details-grid",
                div(class = "detail-item wide", tags$label("Description"), div(class = "detail-val", v(r$description))),
                div(class = "detail-item", tags$label("Requested by"), div(class = "detail-val", v(r$requested_by))),
                div(class = "detail-item", tags$label("Submitted by"), div(class = "detail-val", if (nzchar(v(r$submitted_by))) v(r$submitted_by) else "-")),
                div(class = "detail-item", tags$label("Project code"), div(class = "detail-val", v(r$project_code))),
                div(class = "detail-item", tags$label("Budget line"), div(class = "detail-val", v(r$budget_line))),
                div(class = "detail-item", tags$label("Department"), div(class = "detail-val", v(r$department))),
                div(class = "detail-item", tags$label("Location"), div(class = "detail-val", v(r$location))),
                div(class = "detail-item", tags$label("Country"), div(class = "detail-val", if (nzchar(v(r$country))) v(r$country) else "-")),
                div(class = "detail-item", tags$label("Supplier"), div(class = "detail-val", v(r$supplier))),
                div(class = "detail-item", tags$label("Quantity & Unit"), div(class = "detail-val", paste(if (!is.na(r$quantity)) r$quantity else "-", if (nzchar(v(r$unit))) v(r$unit) else ""))),
                div(class = "detail-item", tags$label("Framework agreement"), div(class = "detail-val", if (identical(v(r$waiver), "Framework agreement")) "Yes" else "No")),
                div(class = "detail-item", tags$label("Order date"), div(class = "detail-val", if (!is.na(r$order_date)) format(as.Date(r$order_date), "%d %b %Y") else "-")),
                if (v(r$status) %in% c("pending", "paid"))
                  tagList(
                    div(class = "detail-item", tags$label("PO reason"), div(class = "detail-val", if (nzchar(v(r$reason))) v(r$reason) else "-")),
                    div(class = "detail-item", tags$label("PO date"), div(class = "detail-val", if (!is.na(r$po_date)) format(as.Date(r$po_date), "%d %b %Y") else "-"))
                  ),
                if (identical(v(r$status), "paid"))
                  div(class = "detail-item", tags$label("Payment date"), div(class = "detail-val", if (!is.na(r$payment_date)) format(as.Date(r$payment_date), "%d %b %Y") else "-")),
                if (nzchar(v(r$remarks)))
                  div(class = "detail-item wide", tags$label("Remarks"), div(class = "detail-val", v(r$remarks)))
            ),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item",
                        div(class = "reference-label", "Order Form (OF)"),
                        tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item",
                        div(class = "reference-label", "Purchase Order (PO)"),
                        tags$code(class = "reference-value", po_ref))
                )
            )
        ),
        footer = tagList(
          modalButton("Close"),
          actionButton("start_edit_modal", tagList(icon("pen-to-square"), "Edit request"), class = "btn-primary")
        )
      ))
    } else {
      showModal(modalDialog(
        title = paste0("Edit Request #", r$id), size = "l", easyClose = TRUE,
        div(class = "form-intro", "Fields marked * are required. Edit values below and click Save changes."),
        div(class = "nr",
            div(class = "form-section-title", "Request details"),
            div(class = "fg",
                col(12, textAreaInput("m_desc", "Description *", v(r$description), rows = 2, width = "100%")),
                col(6, textInput("m_requested_by", "Requested by *", v(r$requested_by), width = "100%")),
                col(6, textInput("m_submitted_by", "Submitted by", v(r$submitted_by), width = "100%")),
                col(4, selectizeInput("m_project", "Project code *",
                                      choices = setNames(project_codes_db(), project_codes_db()), selected = v(r$project_code), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a configured project"))),
                col(4, selectizeInput("m_budget", "Budget line *",
                                      choices = setNames(budget_catalog_db(v(r$project_code))$budget_line_code,
                                                         budget_catalog_db(v(r$project_code))$budget_line_code),
                                      selected = v(r$budget_line), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a mapped budget line"))),
                col(4, selectInput("m_dept", "Department *", unique(c(DEPTS, v(r$department))), selected = v(r$department), width = "100%")),
                col(4, textInput("m_loc", "Location *", v(r$location), width = "100%")),
                col(4, textInput("m_country", "Country", v(r$country), width = "100%")),
                col(4, textInput("m_supplier", "Supplier *", v(r$supplier), width = "100%")),
                col(4, numericInput("m_amount", "Amount *", r$amount, min = 0, width = "100%")),
                col(4, selectInput("m_cur", "Currency *", CUR, selected = r$currency, width = "100%")),
                col(4, numericInput("m_quantity", "Quantity", if (is.na(r$quantity)) NA else r$quantity, min = 0, width = "100%")),
                col(4, textInput("m_unit", "Unit", v(r$unit), width = "100%")),
                col(4, div(style = "padding-top:1.9rem", checkboxInput("m_framework", "Framework agreement", value = identical(v(r$waiver), "Framework agreement")))),
                col(4, selectInput("m_status", "Status *", choices = c(
                  "Under review" = "under_review", "Pending" = "pending", "Paid" = "paid", "Cancelled" = "cancelled"
                ), selected = v(r$status), width = "100%")),
                col(8, textAreaInput("m_remarks", "Remarks", v(r$remarks), rows = 2, width = "100%")),
                col(6, date_in("m_order_date", "Order date", if (is.na(r$order_date)) Sys.Date() else as.Date(r$order_date), width = "100%")),
                col(6, conditionalPanel(
                  "input.m_status == 'pending' || input.m_status == 'paid'",
                  div(class = "fg",
                      col(6, selectizeInput("m_reason", "PO reason *",
                                            choices = c("", uniq(c(data()$reason, r$reason))),
                                            selected = v(r$reason), width = "100%",
                                            options = list(create = TRUE, placeholder = "Type or pick"))),
                      col(6, date_in("m_po_date", "PO date *",
                                     if (is.na(r$po_date)) Sys.Date() else as.Date(r$po_date), width = "100%"))))),
                col(6, conditionalPanel(
                  "input.m_status == 'paid'",
                  date_in("m_date", "Payment date *",
                          if (is.na(r$payment_date)) Sys.Date() else as.Date(r$payment_date), width = "100%")))
            ),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item",
                        div(class = "reference-label", "Order Form (OF)"),
                        tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item",
                        div(class = "reference-label", "Purchase Order (PO)"),
                        tags$code(class = "reference-value", po_ref))),
                p(class = "reference-hint",
                  if (po_is_preview) {
                    "Preview only: save the request to persist this generated PO reference."
                  } else if (v(r$status) %in% c("pending", "paid") && !nzchar(v(r$po))) {
                    "The PO reference is generated from the project, budget line, country, location, department, PO reason and PO date."
                  } else {
                    "The OF and PO references are stored with the request and included in exports. A PO is created when the request moves from Under review to Pending."
                  }))
        ),
        footer = tagList(
          actionButton("cancel_edit_modal", "Back to view", class = "btn-outline-secondary"),
          actionButton("m_save", tagList(icon("check"), "Save changes"), class = "btn-primary")
        )
      ))
    }
  }

  observeEvent(input$open_request_row, {
    id <- suppressWarnings(as.integer(input$open_request_row$id))
    if (length(id) != 1L || is.na(id)) {
      showNotification("Could not identify that request. Refresh the table and try again.", type = "warning")
      return()
    }
    edit_id(id)
    render_request_modal(id, edit_mode = FALSE)
  })
  
  observeEvent(input$start_edit_modal, {
    id <- edit_id()
    if (!is.null(id)) render_request_modal(id, edit_mode = TRUE)
  })
  
  observeEvent(input$cancel_edit_modal, {
    id <- edit_id()
    if (!is.null(id)) render_request_modal(id, edit_mode = FALSE)
  })
  
  observeEvent(input$m_project, {
    pc <- v(input$m_project)
    lines <- if (nzchar(pc)) budget_catalog_db(pc)$budget_line_code else character()
    # Keep the saved budget line when the modal first opens; only clear it if it no
    # longer belongs to the selected project.
    current <- isolate(input$m_budget)
    keep_line <- if (!is.null(current) && current %in% lines) current else character(0)
    updateSelectizeInput(session, "m_budget", choices = setNames(lines, lines), selected = keep_line,
                         options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$m_save, {
    id <- edit_id()
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old)) { removeModal(); return() }
    old <- old[1, , drop = FALSE]
    need_po <- input$m_status %in% c("pending", "paid")
    ok <- all(nzchar(c(v(input$m_desc), v(input$m_requested_by), v(input$m_project),
                       v(input$m_budget), v(input$m_dept), v(input$m_loc), v(input$m_supplier)))) &&
      has_num(input$m_amount) && input$m_amount > 0 && nzchar(v(input$m_cur)) &&
      (!need_po || (nzchar(v(input$m_reason)) && has_date(input$m_po_date))) &&
      (input$m_status != "paid" || has_date(input$m_date))
    if (!ok) {
      showNotification("Please complete the required fields before saving.", type = "warning")
      return()
    }
    
    changed <- !isTRUE(all.equal(input$m_amount, old$amount)) || input$m_cur != old$currency
    rate <- if (changed || is.na(old$rate_used)) RATES[[input$m_cur]] else old$rate_used
    usd <- if (changed || is.na(old$usd)) input$m_amount / rate else old$usd
    order_date <- if (has_date(input$m_order_date)) as.character(input$m_order_date) else as.character(old$order_date)
    reason <- if (need_po) v(input$m_reason) else v(old$reason)
    po_date <- if (need_po) as.character(input$m_po_date) else as.character(old$po_date)
    rec <- list(project_code = v(input$m_project), budget_line = v(input$m_budget),
                country = v(input$m_country), location = v(input$m_loc),
                department = v(input$m_dept), order_date = order_date,
                reason = reason, po_date = po_date)
    
    # Generate the PO while a request is Under validation or Paid. This covers
    # the transition into Under validation and also repairs a missing reference
    # on an older row. If the request later leaves these statuses, retain an
    # already-issued PO reference rather than erasing it.
    order_ref <- mk_ref(rec, "OF")
    if (is.na(order_ref) || !nzchar(v(order_ref))) {
      showNotification("Could not generate the Order Form reference. Check project, budget line, location, department and order date.", type = "error")
      return()
    }
    generated_po <- if (need_po) mk_ref(rec, "PO") else NA_character_
    if (need_po && (is.na(generated_po) || !nzchar(v(generated_po)))) {
      showNotification("Could not generate the Purchase Order reference. Check the PO reason and PO date.", type = "error")
      return()
    }
    saved_po <- if (need_po) generated_po else if (nzchar(v(old$po))) v(old$po) else NA_character_
    
    row <- list(
      description = v(input$m_desc), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$m_country),
      location = rec$location, department = rec$department,
      supplier = v(input$m_supplier), requested_by = v(input$m_requested_by),
      submitted_by = if (nzchar(v(input$m_submitted_by))) v(input$m_submitted_by) else DEFAULTS$submitted_by,
      quantity = if (has_num(input$m_quantity)) input$m_quantity else NA_real_,
      unit = nz(input$m_unit), remarks = nz(input$m_remarks),
      waiver = if (isTRUE(input$m_framework)) "Framework agreement" else "No",
      amount = input$m_amount, currency = input$m_cur, usd = usd, rate_used = rate,
      status = input$m_status,
      reason = if (nzchar(reason)) reason else NA_character_,
      order_date = order_date,
      po_date = if (need_po) as.character(input$m_po_date) else as.character(old$po_date),
      order_form = order_ref,
      po = saved_po,
      payment_date = if (input$m_status == "paid") as.character(input$m_date) else NA_character_,
      updated = now()
    )
    cn <- con(); on.exit(dbDisconnect(cn))
    supplier_name <- trimws(v(row$supplier))
    if (nzchar(supplier_name)) {
      dbExecute(cn, "INSERT OR IGNORE INTO suppliers(supplier_name,active,created,updated) VALUES(?,1,?,?)",
                params = list(supplier_name, now(), now()))
    }
    saved <- tryCatch({ update_expense_row(cn, id, row); TRUE }, error = function(e) {
      showNotification(paste("Request was not updated:", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!saved) return()
    removeModal()
    status_notice <- switch(input$m_status,
                            under_review = list(text = "Request moved to Under review.", type = "default"),
                            pending = list(text = "Request moved to Pending; the PO reference was generated.", type = "warning"),
                            paid = list(text = "Request marked as Paid.", type = "message"),
                            cancelled = list(text = "Request cancelled.", type = "error"),
                            list(text = "Request updated.", type = "message"))
    showNotification(status_notice$text, type = status_notice$type)
  })
  
  # popup with the full description / remarks
  observeEvent(input$see_more, {
    showModal(modalDialog(
      title = input$see_more$title,
      div(style = "white-space:pre-wrap", input$see_more$text),
      footer = modalButton("Close"), easyClose = TRUE))
  })
  
  # ---- request status cards (counts only; monetary totals live in Dashboards) ----

