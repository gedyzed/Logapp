  SCENARIO_CHOICES <- c(
    "Select scenario"              = "",
    "A \u2014 Direct purchase"     = "A",
    "B \u2014 Quotations"           = "B",
    "C \u2014 Contract"             = "C",
    "D \u2014 National tender"      = "D",
    "E \u2014 International tender" = "E",
    "G \u2014 Framework agreement"  = "G"
  )

  area_rows <- reactiveVal(areas_catalog_db())
  people_names <- reactiveVal(people_names_db())
  observe({
    if (!exists("manage_revision", mode = "function")) return()
    manage_revision()
    area_rows(areas_catalog_db())
    people_names(people_names_db())
  }, ignoreInit = TRUE)

  area_choice_list <- function(rows) {
    if (is.null(rows) || !nrow(rows)) return(c("Select area" = ""))
    code <- as.character(rows$code)
    label <- as.character(rows$name)
    missing <- is.na(label) | !nzchar(label)
    label[missing] <- code[missing]
    c("Select area" = "", setNames(code, label))
  }

  # ---- New request: modal (Fix 4) ----
  observeEvent(input$new_request, {
    touched(character()); show_all(FALSE)
    codes <- project_codes_db()
    names <- people_names()
    showModal(modalDialog(
      title = tagList(icon("plus"), " New Request"),
      size = "l", easyClose = FALSE,
      div(class = "nr",
          div(class = "form-section-title", "Request details"),
          div(class = "fg",
              col(12, textAreaInput("description", R("Description"), rows = 2, width = "100%")),
              col(4, selectizeInput("requested_by", R("Requested by"),
                                    choices = c("", names), selected = "", width = "100%",
                                    options = list(create = TRUE, placeholder = "Full name of requester"))),
              col(4, selectizeInput("project_code", R("Project code"),
                                    choices = c("Select project" = "", setNames(codes, codes)),
                                    selected = "", width = "100%",
                                    options = list(create = FALSE, placeholder = "Choose a configured project"))),
              col(4, selectizeInput("budget_line", R("Budget line"), choices = character(),
                                    selected = "", width = "100%",
                                    options = list(create = FALSE, placeholder = "Choose project first"))),
              col(6, pick("department", "Department", DEPTS)),
              col(6, selectizeInput("location", R("Area"),
                                    choices = area_choice_list(area_rows()),
                                    selected = "", width = "100%",
                                    options = list(create = FALSE, placeholder = "Choose an area"))),
              col(6, numericInput("quantity", R("Quantity"), NA, min = 0, width = "100%")),
              col(6, selectizeInput("unit", R("Unit"),
                                    choices = c("", "Days", "Months", "Units", "Trips", "Kg", "Litres"),
                                    selected = "", width = "100%",
                                    options = list(create = TRUE, placeholder = "e.g. Days, Units"))),
              col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
              col(4, pick("currency", "Currency", CUR)),
              col(4, selectInput("procurement_scenario", R("Scenario"),
                                 choices = SCENARIO_CHOICES,
                                 selected = "", width = "100%"))
          ),
          div(class = "form-section-rule"),
          div(class = "form-section-title", "Optional details"),
          div(class = "fg",
              col(4, div(style = "padding-top:1.9rem",
                         checkboxInput("framework", "Framework agreement"))),
              col(4, selectizeInput("submitted_by", "Submitted by (focal point)",
                                    choices = c("", names),
                                    selected = DEFAULTS$submitted_by %||% "", width = "100%",
                                    options = list(create = TRUE, placeholder = "Full name of focal point"))),
              col(4, selectizeInput("country", "Country",
                                    choices = c("", uniq(c(isolate(data())$country, DEFAULTS$country))),
                                    selected = DEFAULTS$country %||% "", width = "100%",
                                    options = list(create = TRUE, placeholder = "e.g. ETH"))),
              col(6, date_in("of_date", "Order date", app_today(), width = "100%")),
              col(12, textAreaInput("remarks", "Remarks", rows = 2, width = "100%"))
          )
      ),
      footer = div(
        class = "d-flex justify-content-between align-items-center w-100",
        actionButton("cancel_new", tagList(icon("arrow-left"), "Back"), class = "btn-outline-secondary"),
        div(class = "d-flex align-items-center gap-2",
            actionButton("clear", "Clear form", class = "btn-outline-secondary"),
            actionButton("add", tagList(icon("plus"), "Add request"), class = "btn-primary"))
      )
    ))
  })
  observeEvent(input$cancel_new, removeModal())
  observeEvent(input$cancel_new_bottom, removeModal())

  # Auto-select procurement scenario: Amount drives scenario; Framework agreement overrides to G
  observe({
    is_fw <- isTRUE(input$framework)
    if (is_fw) {
      updateSelectInput(session, "procurement_scenario", selected = "G")
      return()
    }
    amt <- suppressWarnings(as.numeric(input$amount))
    cur <- input$currency %||% "ETB"
    if (has_num(amt) && amt > 0 && nzchar(cur) && cur %in% names(RATES)) {
      usd <- amt / RATES[[cur]]
      scen <- scenario(usd, "No")
      if (scen %in% c("A", "B", "C", "D", "E")) {
        updateSelectInput(session, "procurement_scenario", selected = scen)
      }
    }
  })

  valid <- reactive(c(
    description          = nzchar(v(input$description)),
    requested_by         = nzchar(v(input$requested_by)),
    project_code         = nzchar(v(input$project_code)),
    budget_line          = nzchar(v(input$budget_line)),
    department           = nzchar(v(input$department)),
    location             = nzchar(v(input$location)),
    procurement_scenario = nzchar(v(input$procurement_scenario)),
    amount               = has_num(input$amount) && input$amount > 0,
    currency             = nzchar(v(input$currency)),
    quantity             = has_num(input$quantity) && input$quantity > 0,
    unit                 = nzchar(v(input$unit))
  ))

  observeEvent(input$touched, touched(union(touched(), input$touched)))

  observe({
    bad  <- names(valid())[!valid()]
    show <- if (show_all()) bad else intersect(bad, touched())
    session$sendCustomMessage("mark", list(ids = as.list(show)))
  })

  waiver_val <- reactive(if (isTRUE(input$framework)) "Framework agreement" else "No")
  odate      <- reactive(if (has_date(input$of_date)) input$of_date else app_today())

  form_rec <- reactive(list(
    project_code = v(input$project_code), budget_line = v(input$budget_line),
    country = v(input$country), location = v(input$location),
    department = v(input$department), order_date = as.character(odate())))

  observeEvent(input$project_code, {
    pc <- v(input$project_code)
    if (!nzchar(pc)) {
      updateSelectizeInput(session, "budget_line",
                           choices = c("Select budget line" = ""),
                           selected = "",
                           options = list(create = FALSE, placeholder = "Choose project first"))
      return()
    }
    form <- project_form_db(pc)
    if (nzchar(form$department)) updateSelectInput(session, "department", selected = form$department)
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = "", setNames(form$lines, form$lines)),
                         selected = "",
                         options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
  }, ignoreInit = TRUE)

  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    names <- people_names()
    updateSelectizeInput(session, "requested_by",
                         choices = c("", names),
                         selected = "", options = list(create = TRUE, placeholder = "Full name of requester"))
    updateSelectizeInput(session, "submitted_by",
                         choices = c("", names),
                         selected = DEFAULTS$submitted_by %||% "",
                         options = list(create = TRUE, placeholder = "Full name of focal point"))
    updateSelectizeInput(session, "country",
                         choices = c("", uniq(c(isolate(data())$country, DEFAULTS$country))),
                         selected = DEFAULTS$country %||% "",
                         options = list(create = TRUE, placeholder = "e.g. ETH"))
    updateSelectizeInput(session, "project_code", selected = "")
    updateSelectizeInput(session, "budget_line",
                         choices = c("Select budget line" = ""), selected = "",
                         options = list(create = FALSE, placeholder = "Choose project first"))
    updateSelectizeInput(session, "location",
                         choices = area_choice_list(area_rows()),
                         selected = "", options = list(create = FALSE, placeholder = "Choose an area"))
    updateSelectizeInput(session, "unit",
                         choices = c("", "Days", "Months", "Units", "Trips", "Kg", "Litres"),
                         selected = "", options = list(create = TRUE, placeholder = "e.g. Days, Units"))
    for (id in c("department", "currency", "procurement_scenario"))
      updateSelectInput(session, id, selected = "")
    updateCheckboxInput(session, "framework", value = FALSE)
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    suppressWarnings(updateDateInput(session, "of_date", value = app_today()))
    updateTextAreaInput(session, "remarks", value = "")
    touched(character()); show_all(FALSE)
  }
  observeEvent(input$clear, reset_form())

  observeEvent(input$add, {
    if (!all(valid())) {
      show_all(TRUE)
      session$sendCustomMessage("scrollbad", list())
      showNotification("Some required fields are empty.", type = "warning")
      return()
    }
    cur  <- input$currency
    rate <- RATES[[cur]]
    rec  <- form_rec()
    row  <- list(
      description  = v(input$description), project_code = rec$project_code,
      budget_line  = rec$budget_line, country = nz(input$country),
      location     = rec$location, department = rec$department,
      reason       = NA_character_, order_form = mk_ref(rec, "OF"), po = NA_character_,
      requested_by = v(input$requested_by),
      submitted_by = if (nzchar(v(input$submitted_by))) v(input$submitted_by) else DEFAULTS$submitted_by,
      status       = "under_review", waiver = waiver_val(), supplier = NA_character_,
      unit         = nz(input$unit),
      quantity     = if (has_num(input$quantity)) input$quantity else NA_real_,
      amount       = input$amount, currency = cur,
      usd          = input$amount / rate, rate_used = rate,
      order_date   = rec$order_date, po_date = NA_character_,
      payment_date = NA_character_, remarks = nz(input$remarks),
      created      = now(), updated = now()
    )
    cn <- con(); on.exit(db_release(cn))
    saved <- tryCatch({
      withProgress(message = "Saving request...", value = 0.4, insert_expense_row(cn, row))
      TRUE
    }, error = function(e) {
      showNotification(paste("Request was not saved:", conditionMessage(e)), type = "error"); FALSE
    })
    if (!saved) return()
    reload_expenses()
    removeModal()
    session$sendCustomMessage("cleanupModal", list())
    showNotification("Request added.", type = "message")
  })

  observeEvent(session$clientData$url_search, {
    q    <- parseQueryString(session$clientData$url_search)
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
      chart <- q[["chart"]]
      if (!is.null(chart) && chart %in% c("year_compare", "quarter_compare")) {
        updateRadioButtons(session, "insights_compare_mode",
                           selected = if (identical(chart, "quarter_compare")) "quarter" else "month")
        chart <- "comparison"
      }
      if (!is.null(chart) && nzchar(chart)) updateSelectInput(session, "dash_chart_select", selected = chart)
      if (!is.null(q[["smode"]])) updateRadioButtons(session, "insights_spend_mode", selected = q[["smode"]])
      if (!is.null(q[["cmode"]])) updateRadioButtons(session, "insights_compare_mode", selected = q[["cmode"]])
      if (!is.null(q[["bstat"]])) updateSelectInput(session, "breakdown_status", selected = q[["bstat"]])
      month <- if (!is.null(q[["mo"]]) && nzchar(q[["mo"]])) q[["mo"]] else NULL
      projects <- if (!is.null(q[["p"]])) vals("p") else NULL
      dept <- if (!is.null(q[["d"]]) && !grepl("|", q[["d"]], fixed = TRUE)) q[["d"]] else NULL
      scen <- if (!is.null(q[["sc"]])) q[["sc"]] else NULL
      if (!is.null(month)) updateSelectizeInput(session, "insights_month", selected = month)
      if (!is.null(projects)) {
        one <- if (exists("insights_project_codes", mode = "function")) insights_project_codes(projects) else projects
        updateSelectizeInput(session, "insights_project", selected = if (length(one)) one[[1]] else "all")
      }
      if (!is.null(dept)) updateSelectizeInput(session, "insights_department", selected = dept)
      if (!is.null(scen)) updateSelectizeInput(session, "insights_scenario", selected = scen)
      if (!is.null(month) && exists("insights_commit", mode = "function")) {
        insights_commit(month, if (is.null(projects)) character() else projects,
                        if (is.null(dept)) "" else dept,
                        if (is.null(scen)) "" else scen)
      }
      return()
    }
    if (!(q[["v"]] %in% c("r", "r2"))) return()
    if (!is.null(q[["cur"]])) updateSelectInput(session, "global_cur", selected = q[["cur"]])
    if (!is.null(q[["find"]])) updateTextInput(session, "f_query", value = q[["find"]])
    if (!is.null(q[["st"]])) set_request_status(q[["st"]])
    d <- isolate(data())
    if (!is.null(q[["m"]]) && !is.na(q[["m"]])) updateSelectizeInput(session, "f_month", selected = q[["m"]])
    else updateSelectizeInput(session, "f_month", selected = "")
    set <- function(id, key, choices) {
      sel <- vals(key)
      if (length(sel)) updateSelectizeInput(session, id, choices = with_sel(choices, sel), selected = sel)
    }
    set("f_project",   "p", project_codes_db())
    set("f_person",    "r", uniq(d$requested_by))
    set("f_submitter", "s", uniq(d$submitted_by))
    set("f_week",      "w", week_choices(d, if (is.null(q[["m"]])) "" else q[["m"]]))
    bchoices <- budget_filter_choices(vals("p"))
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = intersect(vals("b"), unname(bchoices)))
  }, once = TRUE)

  render_request_modal <- function(id, edit_mode = FALSE, read_only = FALSE) {
    r <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(r)) { showNotification("That request is no longer available.", type = "warning"); return() }
    r      <- r[1, , drop = FALSE]
    status <- v(r$status)
    ref_rec <- list(
      project_code = v(r$project_code), budget_line = v(r$budget_line),
      country = v(r$country), location = v(r$location),
      department = v(r$department), order_date = v(r$order_date),
      reason = v(r$reason), po_date = v(r$po_date))
    order_ref <- v(r$order_form)
    if (!nzchar(order_ref)) order_ref <- v(mk_ref(ref_rec, "OF"))
    if (!nzchar(order_ref)) order_ref <- "Unavailable — check project, budget line, area, department and order date"
    po_ref <- v(r$po); po_is_preview <- FALSE
    if (!nzchar(po_ref) && status %in% c("pending", "paid")) {
      cand <- v(mk_ref(ref_rec, "PO"))
      if (nzchar(cand)) { po_ref <- cand; po_is_preview <- TRUE }
    }
    if (!nzchar(po_ref))
      po_ref <- if (status %in% c("pending", "paid"))
        "Not generated — provide a PO reason and PO date, then save."
      else "Generated when the request moves from Under review to Pending."

    if (!edit_mode) {
      footer_btns <- if (isTRUE(read_only)) {
        div(class = "d-flex justify-content-start align-items-center w-100",
            modalButton(tagList(icon("arrow-left"), "Back")))
      } else if (status == "under_review") {
        div(class = "d-flex justify-content-between align-items-center w-100",
            div(class = "d-flex align-items-center gap-2",
                modalButton(tagList(icon("arrow-left"), "Back")),
                actionButton("start_edit_modal", tagList(icon("pen-to-square"), "Edit"), class = "btn-primary")),
            div(class = "d-flex align-items-center gap-2",
                actionButton("btn_cancel_request", tagList(icon("xmark"), "Cancel request"), class = "btn-outline-danger"),
                actionButton("btn_proceed", tagList(icon("arrow-right"), "Proceed"), class = "btn-primary")))
      } else if (status == "pending") {
        div(class = "d-flex justify-content-between align-items-center w-100",
            div(class = "d-flex align-items-center gap-2",
                modalButton(tagList(icon("arrow-left"), "Back")),
                actionButton("start_edit_modal", tagList(icon("pen-to-square"), "Edit"), class = "btn-primary")),
            div(class = "d-flex align-items-center gap-2",
                actionButton("btn_cancel_request", tagList(icon("xmark"), "Cancel request"), class = "btn-outline-danger"),
                actionButton("btn_pay", tagList(icon("circle-check"), "Mark Paid"), class = "btn-primary")))
      } else {
        div(class = "d-flex justify-content-start align-items-center w-100",
            modalButton(tagList(icon("arrow-left"), "Back")))
      }
      showModal(modalDialog(
        title = div(class = "d-flex justify-content-between align-items-center w-100 pe-3",
                    span(paste0("Request #", r$id)),
                    span(class = paste0("badge status-pill status-", status), status_label(status))),
        size = "l", easyClose = TRUE,
        div(class = "request-view-container",
            div(class = "mb-2 pb-2 border-bottom",
                div(class = "text-muted small", "Amount"),
                div(class = "fs-4 fw-bold", paste(r$currency, money(r$amount)))),
            div(class = "form-section-title", "Request details"),
            div(class = "request-details-grid",
                div(class = "detail-item wide", tags$label("Description"), div(class = "detail-val", v(r$description))),
                div(class = "detail-item", tags$label("Requested by"), div(class = "detail-val", v(r$requested_by))),
                div(class = "detail-item", tags$label("Submitted by"), div(class = "detail-val", if (nzchar(v(r$submitted_by))) v(r$submitted_by) else "-")),
                div(class = "detail-item", tags$label("Project code"), div(class = "detail-val", v(r$project_code))),
                div(class = "detail-item", tags$label("Budget line"), div(class = "detail-val", v(r$budget_line))),
                div(class = "detail-item", tags$label("Department"), div(class = "detail-val", v(r$department))),
                div(class = "detail-item", tags$label("Area"), div(class = "detail-val", v(r$location))),
                div(class = "detail-item", tags$label("Country"), div(class = "detail-val", if (nzchar(v(r$country))) v(r$country) else "-")),
                div(class = "detail-item", tags$label("Supplier"), div(class = "detail-val", if (nzchar(v(r$supplier))) v(r$supplier) else "-")),
                div(class = "detail-item", tags$label("Quantity & Unit"), div(class = "detail-val", paste(if (!is.na(r$quantity)) r$quantity else "-", if (nzchar(v(r$unit))) v(r$unit) else ""))),
                div(class = "detail-item", tags$label("Framework agreement"), div(class = "detail-val", if (identical(v(r$waiver), "Framework agreement")) "Yes" else "No")),
                div(class = "detail-item", tags$label("Order date"), div(class = "detail-val", if (!is.na(r$order_date)) format(as.Date(r$order_date), "%d %b %Y") else "-")),
                if (status %in% c("pending", "paid"))
                  tagList(
                    div(class = "detail-item", tags$label("PO reason"), div(class = "detail-val", if (nzchar(v(r$reason))) v(r$reason) else "-")),
                    div(class = "detail-item", tags$label("PO date"), div(class = "detail-val", if (!is.na(r$po_date)) format(as.Date(r$po_date), "%d %b %Y") else "-"))),
                if (status == "paid")
                  div(class = "detail-item", tags$label("Payment date"), div(class = "detail-val", if (!is.na(r$payment_date)) format(as.Date(r$payment_date), "%d %b %Y") else "-")),
                if (nzchar(v(r$remarks)))
                  div(class = "detail-item wide", tags$label("Remarks"), div(class = "detail-val", v(r$remarks)))),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item", div(class = "reference-label", "Order Form (OF)"), tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item", div(class = "reference-label", "Purchase Order (PO)"), tags$code(class = "reference-value", po_ref))))),
        footer = footer_btns))
    } else {
      edit_lines <- project_form_db(v(r$project_code))$lines
      showModal(modalDialog(
        title = paste0("Edit Request #", r$id), size = "l", easyClose = TRUE,
        div(class = "form-intro", "Fields marked * are required. Use Proceed / Pay / Cancel buttons to change status."),
        div(class = "nr",
            div(class = "form-section-title", "Request details"),
            div(class = "fg",
                col(12, textAreaInput("m_desc", "Description *", v(r$description), rows = 2, width = "100%")),
                col(6, textInput("m_requested_by", "Requested by *", v(r$requested_by), width = "100%")),
                col(6, textInput("m_submitted_by", "Submitted by", v(r$submitted_by), width = "100%")),
                col(4, selectizeInput("m_project", "Project code *",
                                      choices = setNames(project_codes_db(), project_codes_db()),
                                      selected = v(r$project_code), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a configured project"))),
                col(4, selectizeInput("m_budget", "Budget line *",
                                      choices = setNames(edit_lines, edit_lines),
                                      selected = v(r$budget_line), width = "100%",
                                      options = list(create = FALSE, placeholder = "Choose a mapped budget line"))),
                col(4, selectInput("m_dept", "Department *", unique(c(DEPTS, v(r$department))), selected = v(r$department), width = "100%")),
                col(4, textInput("m_loc", "Area *", v(r$location), width = "100%")),
                col(4, textInput("m_country", "Country", v(r$country), width = "100%")),
                col(4, textInput("m_supplier", "Supplier", v(r$supplier), width = "100%")),
                col(4, numericInput("m_amount", "Amount *", r$amount, min = 0, width = "100%")),
                col(4, selectInput("m_cur", "Currency *", CUR, selected = r$currency, width = "100%")),
                col(4, numericInput("m_quantity", "Quantity", if (is.na(r$quantity)) NA else r$quantity, min = 0, width = "100%")),
                col(4, textInput("m_unit", "Unit", v(r$unit), width = "100%")),
                col(4, div(style = "padding-top:1.9rem", checkboxInput("m_framework", "Framework agreement", value = identical(v(r$waiver), "Framework agreement")))),
                col(8, textAreaInput("m_remarks", "Remarks", v(r$remarks), rows = 2, width = "100%")),
                col(6, date_in("m_order_date", "Order date", if (is.na(r$order_date)) app_today() else as.Date(r$order_date), width = "100%"))),
            div(class = "reference-panel",
                div(class = "form-section-title", "Generated references"),
                div(class = "reference-grid",
                    div(class = "reference-item", div(class = "reference-label", "Order Form (OF)"), tags$code(class = "reference-value", order_ref)),
                    div(class = "reference-item", div(class = "reference-label", "Purchase Order (PO)"), tags$code(class = "reference-value", po_ref))),
                p(class = "reference-hint", if (po_is_preview) "Preview only: save to persist this PO reference."
                  else "OF and PO references are stored with the request and included in exports."))),
        footer = div(
          class = "d-flex justify-content-between align-items-center w-100",
          actionButton("cancel_edit_modal", tagList(icon("arrow-left"), "Back to view"), class = "btn-outline-secondary"),
          actionButton("m_save", tagList(icon("check"), "Save changes"), class = "btn-primary")
        )))
    }
  }

  observeEvent(input$open_request_row, {
    id <- as.character(input$open_request_row$id)
    id <- id[!is.na(id) & nzchar(id)]
    if (length(id) != 1L) { showNotification("Could not identify that request.", type = "warning"); return() }
    edit_id(id)
    render_request_modal(id, edit_mode = FALSE)
  })
  observeEvent(input$start_edit_modal, {
    id <- edit_id()
    if (is.null(id)) return()
    row <- data()[data()$id == id, , drop = FALSE]
    if (nrow(row) && v(row$status[1]) %in% c("paid", "cancelled")) {
      showNotification("Paid and cancelled requests are locked.", type = "warning")
      return()
    }
    render_request_modal(id, edit_mode = TRUE)
  })
  observeEvent(input$cancel_edit_modal, { id <- edit_id(); if (!is.null(id)) render_request_modal(id, edit_mode = FALSE) })

  observeEvent(input$m_project, {
    pc    <- v(input$m_project)
    lines <- if (nzchar(pc)) project_form_db(pc)$lines else character()
    cur   <- isolate(input$m_budget)
    keep  <- if (!is.null(cur) && cur %in% lines) cur else character(0)
    updateSelectizeInput(session, "m_budget", choices = setNames(lines, lines), selected = keep,
                         options = list(create = FALSE, placeholder = "Choose a mapped budget line"))
  }, ignoreInit = TRUE)

  observeEvent(input$m_save, {
    id  <- edit_id()
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old)) { removeModal(); return() }
    old <- old[1, , drop = FALSE]
    if (v(old$status) %in% c("paid", "cancelled")) {
      showNotification("Paid and cancelled requests are locked.", type = "warning")
      removeModal()
      return()
    }
    ok  <- all(nzchar(c(v(input$m_desc), v(input$m_requested_by), v(input$m_project),
                        v(input$m_budget), v(input$m_dept), v(input$m_loc)))) &&
      has_num(input$m_amount) && input$m_amount > 0 && nzchar(v(input$m_cur))
    if (!ok) { showNotification("Please complete the required fields.", type = "warning"); return() }
    changed    <- !isTRUE(all.equal(input$m_amount, old$amount)) || input$m_cur != old$currency
    rate       <- if (changed || is.na(old$rate_used)) RATES[[input$m_cur]] else old$rate_used
    usd        <- if (changed || is.na(old$usd)) input$m_amount / rate else old$usd
    order_date <- if (has_date(input$m_order_date)) as.character(input$m_order_date) else as.character(old$order_date)
    rec <- list(project_code = v(input$m_project), budget_line = v(input$m_budget),
                country = v(input$m_country), location = v(input$m_loc),
                department = v(input$m_dept), order_date = order_date,
                reason = v(old$reason), po_date = as.character(old$po_date))
    order_ref <- mk_ref(rec, "OF")
    if (is.na(order_ref) || !nzchar(v(order_ref))) { showNotification("Could not generate the Order Form reference.", type = "error"); return() }
    row <- list(
      description  = v(input$m_desc), project_code = rec$project_code, budget_line = rec$budget_line,
      country      = nz(input$m_country), location = rec$location, department = rec$department,
      supplier     = if (nzchar(v(input$m_supplier))) v(input$m_supplier) else NA_character_,
      requested_by = v(input$m_requested_by),
      submitted_by = if (nzchar(v(input$m_submitted_by))) v(input$m_submitted_by) else DEFAULTS$submitted_by,
      quantity     = if (has_num(input$m_quantity)) input$m_quantity else NA_real_,
      unit         = nz(input$m_unit), remarks = nz(input$m_remarks),
      waiver       = if (isTRUE(input$m_framework)) "Framework agreement" else "No",
      amount       = input$m_amount, currency = input$m_cur, usd = usd, rate_used = rate,
      status       = v(old$status),
      reason       = if (nzchar(v(old$reason))) v(old$reason) else NA_character_,
      order_date   = order_date, po_date = as.character(old$po_date),
      order_form   = order_ref, po = if (nzchar(v(old$po))) v(old$po) else NA_character_,
      payment_date = as.character(old$payment_date), updated = now())
    cn <- con(); on.exit(db_release(cn))
    saved <- tryCatch({
      withProgress(message = "Saving request...", value = 0.4, update_expense_row(cn, id, row))
      TRUE
    }, error = function(e) {
      showNotification(paste("Request was not updated:", conditionMessage(e)), type = "error"); FALSE })
    if (!saved) return()
    reload_expenses()
    removeModal(); showNotification("Request updated.", type = "message")
  })

  # ---- Fix 1: Proceed ----
  observeEvent(input$btn_proceed, {
    id <- edit_id(); if (is.null(id)) return()
    showModal(modalDialog(
      title = tagList(icon("arrow-right"), " Proceed with Purchase Order"),
      size = "m", easyClose = TRUE,
      div(class = "nr",
          p(class = "form-hint", "Complete the purchase order details to move this request to Pending."),
          selectizeInput("proc_supplier", "Supplier *", choices = c("", supplier_catalog_db()),
                         selected = "", width = "100%",
                         options = list(create = TRUE, placeholder = "Type or choose a supplier")),
          textInput("proc_reason", "PO reason *", width = "100%", placeholder = "e.g. Vehicle Rent"),
          date_in("proc_po_date", "PO date *", app_today(), width = "100%")),
      footer = div(
        class = "d-flex justify-content-between align-items-center w-100",
        modalButton(tagList(icon("arrow-left"), "Back")),
        actionButton("btn_proceed_confirm", tagList(icon("check"), "Confirm Proceed"), class = "btn-primary")
      )))
  })

  observeEvent(input$btn_proceed_confirm, {
    id       <- edit_id(); if (is.null(id)) return()
    supplier <- trimws(v(input$proc_supplier))
    reason   <- trimws(v(input$proc_reason))
    po_date  <- input$proc_po_date
    if (!nzchar(supplier) || !nzchar(reason) || !has_date(po_date)) {
      showNotification("Supplier, PO reason and PO date are all required.", type = "warning"); return() }
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old) || v(old$status[1]) != "under_review") {
      showNotification("This request is no longer Under review.", type = "warning"); removeModal(); return() }
    old <- old[1, , drop = FALSE]
    rec <- list(project_code = v(old$project_code), budget_line = v(old$budget_line),
                country = v(old$country), location = v(old$location), department = v(old$department),
                order_date = v(old$order_date), reason = reason, po_date = as.character(po_date))
    po_ref <- mk_ref(rec, "PO")
    if (is.na(po_ref) || !nzchar(v(po_ref))) {
      showNotification("Could not generate PO reference. Check project, area, department, reason and date.", type = "error"); return() }
    cn <- con(); on.exit(db_release(cn))
    row <- list(
      description  = v(old$description), project_code = v(old$project_code), budget_line = v(old$budget_line),
      country      = nz(old$country), location = v(old$location), department = v(old$department),
      supplier     = supplier, requested_by = v(old$requested_by), submitted_by = v(old$submitted_by),
      quantity     = if (!is.na(old$quantity)) old$quantity else NA_real_,
      unit         = nz(old$unit), remarks = nz(old$remarks), waiver = v(old$waiver),
      amount       = old$amount, currency = v(old$currency), usd = old$usd, rate_used = old$rate_used,
      status       = "pending", reason = reason, order_date = v(old$order_date),
      po_date      = as.character(po_date), order_form = v(old$order_form), po = po_ref,
      payment_date = NA_character_, updated = now())
    saved <- tryCatch({
      withProgress(message = "Saving request...", value = 0.4, update_expense_row(cn, id, row))
      TRUE
    }, error = function(e) {
      showNotification(paste("Could not proceed:", conditionMessage(e)), type = "error"); FALSE })
    if (!saved) return()
    reload_expenses()
    removeModal()
    session$sendCustomMessage("cleanupModal", list())
    showNotification("Request moved to Pending. PO reference generated.", type = "message")
  })

  # ---- Fix 1: Mark Paid ----
  observeEvent(input$btn_pay, {
    id <- edit_id(); if (is.null(id)) return()
    showModal(modalDialog(
      title = tagList(icon("circle-check"), " Mark as Paid"),
      size = "m", easyClose = TRUE,
      div(class = "nr",
          p(class = "form-hint", "This records a payment assertion inside the tracker. No external payment is executed."),
          date_in("pay_date", "Payment date *", app_today(), width = "100%"),
          checkboxInput("pay_confirm", "I confirm this request has been paid.", value = FALSE)),
      footer = div(
        class = "d-flex justify-content-between align-items-center w-100",
        modalButton(tagList(icon("arrow-left"), "Back")),
        actionButton("btn_pay_confirm", tagList(icon("check"), "Confirm Paid"), class = "btn-primary", disabled = TRUE)
      )))
  })

  observeEvent(input$btn_pay_confirm, {
    id <- edit_id(); if (is.null(id)) return()
    if (!isTRUE(input$pay_confirm)) { showNotification("Please check the confirmation box.", type = "warning"); return() }
    if (!has_date(input$pay_date)) { showNotification("A payment date is required.", type = "warning"); return() }
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old) || v(old$status[1]) != "pending") {
      showNotification("This request is no longer Pending.", type = "warning"); removeModal(); return() }
    old <- old[1, , drop = FALSE]
    cn  <- con(); on.exit(db_release(cn))
    row <- list(
      description  = v(old$description), project_code = v(old$project_code), budget_line = v(old$budget_line),
      country      = nz(old$country), location = v(old$location), department = v(old$department),
      supplier     = v(old$supplier), requested_by = v(old$requested_by), submitted_by = v(old$submitted_by),
      quantity     = if (!is.na(old$quantity)) old$quantity else NA_real_,
      unit         = nz(old$unit), remarks = nz(old$remarks), waiver = v(old$waiver),
      amount       = old$amount, currency = v(old$currency), usd = old$usd, rate_used = old$rate_used,
      status       = "paid", reason = v(old$reason), order_date = v(old$order_date),
      po_date      = as.character(old$po_date), order_form = v(old$order_form), po = v(old$po),
      payment_date = as.character(input$pay_date), updated = now())
    saved <- tryCatch({
      withProgress(message = "Saving request...", value = 0.4, update_expense_row(cn, id, row))
      TRUE
    }, error = function(e) {
      showNotification(paste("Could not mark paid:", conditionMessage(e)), type = "error"); FALSE })
    if (!saved) return()
    reload_expenses()
    removeModal()
    session$sendCustomMessage("cleanupModal", list())
    showNotification("Request marked as Paid.", type = "message")
  })

  # ---- Fix 1: Cancel ----
  observeEvent(input$btn_cancel_request, {
    id <- edit_id(); if (is.null(id)) return()
    showModal(modalDialog(
      title = tagList(icon("xmark"), " Cancel Request"),
      size = "m", easyClose = TRUE,
      div(class = "nr",
          p(class = "form-hint", "This action cannot be undone. The request will be marked Cancelled."),
          textAreaInput("cancel_reason", "Reason *", rows = 3, width = "100%",
                        placeholder = "Briefly explain why this request is being cancelled.")),
      footer = div(
        class = "d-flex justify-content-between align-items-center w-100",
        modalButton(tagList(icon("arrow-left"), "Back")),
        actionButton("btn_cancel_confirm", tagList(icon("xmark"), "Confirm Cancel"), class = "btn-danger")
      )))
  })

  observeEvent(input$btn_cancel_confirm, {
    id     <- edit_id(); if (is.null(id)) return()
    reason <- trimws(v(input$cancel_reason))
    if (!nzchar(reason)) { showNotification("A cancellation reason is required.", type = "warning"); return() }
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old) || !v(old$status[1]) %in% c("under_review", "pending")) {
      showNotification("This request cannot be cancelled from its current status.", type = "warning")
      removeModal(); return() }
    old <- old[1, , drop = FALSE]
    cn  <- con(); on.exit(db_release(cn))
    row <- list(
      description  = v(old$description), project_code = v(old$project_code), budget_line = v(old$budget_line),
      country      = nz(old$country), location = v(old$location), department = v(old$department),
      supplier     = v(old$supplier), requested_by = v(old$requested_by), submitted_by = v(old$submitted_by),
      quantity     = if (!is.na(old$quantity)) old$quantity else NA_real_,
      unit         = nz(old$unit), remarks = nz(old$remarks), waiver = v(old$waiver),
      amount       = old$amount, currency = v(old$currency), usd = old$usd, rate_used = old$rate_used,
      status       = "cancelled", reason = v(old$reason), cancellation_reason = reason, order_date = v(old$order_date),
      po_date      = as.character(old$po_date), order_form = v(old$order_form), po = v(old$po),
      payment_date = NA_character_, updated = now())
    saved <- tryCatch({
      withProgress(message = "Saving request...", value = 0.4, update_expense_row(cn, id, row))
      TRUE
    }, error = function(e) {
      showNotification(paste("Could not cancel:", conditionMessage(e)), type = "error"); FALSE })
    if (!saved) return()
    reload_expenses()
    removeModal()
    session$sendCustomMessage("cleanupModal", list())
    showNotification("Request cancelled.", type = "error")
  })

  observeEvent(input$see_more, {
    showModal(modalDialog(title = input$see_more$title,
                          div(style = "white-space:pre-wrap", input$see_more$text),
                          footer = modalButton(tagList(icon("arrow-left"), "Back")), easyClose = TRUE))
  })

  # Weekly report row click: opens strictly read-only request modal (Fix Option B)
  observeEvent(input$open_weekly_row, {
    id <- as.character(input$open_weekly_row$id)
    id <- id[!is.na(id) & nzchar(id)]
    if (length(id) != 1L) { showNotification("Could not identify that request.", type = "warning"); return() }
    edit_id(id); render_request_modal(id, edit_mode = FALSE, read_only = TRUE)
  })

  # ---- Fix 3: Weekly report banner and table ----
  output$weekly_report_banner <- renderUI({
    if (!weekly_window_active()) return(NULL)
    wd     <- weekly_data()
    today  <- app_today()
    mon    <- today - (as.integer(format(today, "%u")) - 1L)
    period <- paste0(format(mon, "%d %b"), " \u2013 ", format(today, "%d %b %Y"))
    n_req  <- nrow(wd)
    tags$details(
      open = NA, class = "weekly-report-card",
      tags$summary(
        style = "cursor:pointer;font-weight:650;font-size:.95rem;display:flex;align-items:center;gap:.5rem;padding:.6rem .2rem;list-style:none;",
        icon("calendar-week"),
        span(paste0("Weekly Report \u2014 ", period)),
        span(class = "fin-count ms-2", paste0(n_req, " request", if (n_req != 1) "s" else ""))
      ),
      div(class = "card mt-2",
          style = "border:1px solid var(--app-border);border-radius:var(--app-radius);",
          div(class = "card-body", style = "padding:.75rem 1rem;",
              if (n_req == 0)
                p(class = "text-muted small mb-0", "No requests registered this week.")
              else
                DTOutput("table_weekly", fill = FALSE)))
    )
  })

  output$table_weekly <- renderDT({
    wd <- weekly_data()
    if (!nrow(wd)) return(datatable(data.frame(), options = list(dom = "t", language = dt_language)))
    blank <- function(x) ifelse(is.na(x) | !nzchar(as.character(x)), "", as.character(x))
    ref   <- if ("order_form" %in% names(wd)) blank(wd$order_form) else rep("", nrow(wd))
    ref[!nzchar(ref)] <- paste0("REQ-", wd$id[!nzchar(ref)])
    display <- data.frame(
      id           = wd$id,
      reference    = ref,
      description  = blank(wd$description),
      requested_by = blank(wd$requested_by),
      project      = blank(wd$project_code),
      budget_line  = blank(wd$budget_line),
      amount       = selected_amount_label(wd),
      status       = blank(wd$status),
      registered   = blank(substr(as.character(wd$created), 1, 10)),
      check.names  = FALSE
    )
    names(display) <- c("id", "Reference", "Description", "Requester",
                        "Project", "Budget line", "Amount", "Status", "Registered")
    datatable(display, rownames = FALSE, selection = "none", filter = "none",
              fillContainer = FALSE, escape = TRUE,
              options = list(
                scrollX = TRUE, autoWidth = FALSE,
                pageLength = 10, dom = "rt<'dt-foot'ip>",
                language = dt_language,
                initComplete = weekly_table_init(),
                rowCallback = JS("function(row,data){$(row).attr('data-id',data[0]);}"),
                columnDefs = list(
                  list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE),
                  list(targets = 7, orderable = FALSE, render = JS(r"---(
function(data,type){
  if(type!=='display') return data;
  var l={under_review:'Under review',pending:'Pending',paid:'Paid',cancelled:'Cancelled'};
  var k=String(data||'');
  return '<span class="req-pill req-pill-'+k+'">'+(l[k]||k)+'</span>';
}
)---")))))
  }, server = TRUE)
