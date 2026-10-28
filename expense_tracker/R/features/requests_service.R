  touched  <- reactiveVal(character())
  show_all <- reactiveVal(FALSE)
  edit_id  <- reactiveVal(NULL)
  request_table_status <- reactiveVal("")
  project_edit_id <- reactiveVal(NULL)
  supplier_edit_id <- reactiveVal(NULL)
  person_edit_id <- reactiveVal(NULL)
  email_edit_id <- reactiveVal(NULL)
  
  observeEvent(input$table_status_filter, {
    request_table_status(v(input$table_status_filter))
  }, ignoreInit = FALSE)
  set_request_status <- function(status = "") {
    status <- if (status %in% STATUS) status else ""
    request_table_status(status)
    session$sendCustomMessage("setTableStatus", list(status = status))
  }
  observeEvent(input$card_status_all, set_request_status(""), ignoreInit = TRUE)
  observeEvent(input$card_status_waiting, set_request_status("under_review"), ignoreInit = TRUE)
  observeEvent(input$card_status_validation, set_request_status("pending"), ignoreInit = TRUE)
  observeEvent(input$card_status_paid, set_request_status("paid"), ignoreInit = TRUE)
  observeEvent(input$card_status_rejected, set_request_status("cancelled"), ignoreInit = TRUE)
  observeEvent(input$chip_status_all, set_request_status(""), ignoreInit = TRUE)
  observeEvent(input$chip_status_waiting, set_request_status("under_review"), ignoreInit = TRUE)
  observeEvent(input$chip_status_validation, set_request_status("pending"), ignoreInit = TRUE)
  observeEvent(input$chip_status_paid, set_request_status("paid"), ignoreInit = TRUE)
  observeEvent(input$chip_status_rejected, set_request_status("cancelled"), ignoreInit = TRUE)
  
  
  # ---- read live rows from the shared SQLite database ----
  # Keep this connection open for PRAGMA data_version, which changes after commits
  # by other connections (including other Shiny sessions and external writers).
  poll_cn <- con()
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(poll_cn)) DBI::dbDisconnect(poll_cn)
  })
  db_poll <- reactivePoll(
    1000, session,
    checkFunc = function() as.character(dbGetQuery(poll_cn, "PRAGMA data_version")[[1]][[1]]),
    valueFunc = function() {
      rows <- dbGetQuery(poll_cn,
                         "SELECT e.*, p.project_code, bl.budget_line_code AS budget_line
         FROM expenses e
         LEFT JOIN budget_lines bl ON bl.id = e.budget_line_id
         LEFT JOIN projects p ON p.id = bl.project_id
         ORDER BY e.id DESC")
      # Include the database revision so catalogue/default changes invalidate dependants
      # even when the expense rows themselves are unchanged.
      attr(rows, "database_revision") <- as.character(dbGetQuery(poll_cn, "PRAGMA data_version")[[1]][[1]])
      rows
    }
  )
  dashboard_last_refresh <- reactiveVal(Sys.time())
  observeEvent(db_poll(), dashboard_last_refresh(Sys.time()), ignoreInit = FALSE)
  
  data <- reactive({
    d <- db_poll()
    if ("status" %in% names(d)) {
      old <- !is.na(d$status)
      d$status[old & d$status == "waiting"] <- "under_review"
      d$status[old & d$status == "under validation"] <- "pending"
      d$status[old & d$status == "rejected"] <- "cancelled"
    }
    # USD is fixed at entry, so scenario and totals don't move when RATES change.
    # as.numeric() keeps this working when the table is still empty.
    usd <- as.numeric(ifelse(is.na(d$usd),
                             as.numeric(d$amount) / unname(RATES[d$currency]),
                             d$usd))
    for (k in CUR)
      d[[k]] <- as.numeric(ifelse(d$currency %in% k, d$amount,
                                  round(usd * RATES[[k]], 2)))
    d$scenario      <- scenario(usd, d$waiver)
    date_value <- suppressWarnings(as.Date(d$order_date))
    d$week          <- format(date_value, "%G-W%V")
    d$month         <- format(date_value, "%Y-%m")
    d$year          <- format(date_value, "%Y")
    month_number    <- suppressWarnings(as.integer(format(date_value, "%m")))
    d$quarter       <- ifelse(!is.na(month_number), paste0("Q", ceiling(month_number / 3)), NA_character_)
    d
  })
  
  week_choices <- function(d, selected_month = "") {
    d <- d[!is.na(d$week), , drop = FALSE]
    if (nzchar(v(selected_month))) d <- d[d$month == selected_month, , drop = FALSE]
    if (!nrow(d)) return(character())
    summary <- d |> group_by(week) |>
      summarise(first = min(as.Date(order_date)), .groups = "drop") |>
      arrange(desc(week))
    monday <- summary$first - (as.integer(format(summary$first, "%u")) - 1L)
    setNames(summary$week, paste0(summary$week, " (from ", format(monday, "%d %b"), ")"))
  }
  
  budget_filter_choices <- function(project_values = character()) {
    # Budget lines are only selectable after a project is selected, and always come
    # from the configured project/budget-line catalogue in tracker.db.
    if (is.null(project_values) || !length(project_values)) return(character())
    catalog <- budget_catalog_db(project_values)
    if (!nrow(catalog)) return(character())
    setNames(as.character(catalog$id), paste0(catalog$project_code, " — ", catalog$budget_line_code))
  }
  
  # Populate filters from the database. Month constrains Week; selecting a project
  # constrains Budget line to that project's configured mappings.
  observe({
    d <- data()
    keep <- function(id) {
      x <- isolate(input[[id]])
      if (is.null(x)) character() else as.character(x)
    }
    current_month <- format(Sys.Date(), "%Y-%m")
    available_months <- unique(c(d$month, current_month))
    month_selected <- isolate(input$f_month) %||% current_month
    updateSelectInput(session, "f_month",
                      choices = c("All months" = "", month_choices(available_months)),
                      selected = month_selected)
    # Project choices come from the projects table, including newly configured projects
    # that do not yet have an expense record.
    project_choices <- project_codes_db()
    project_selected <- intersect(keep("f_project"), project_choices)
    updateSelectizeInput(session, "f_project", choices = project_choices,
                         selected = project_selected)
    updateSelectInput(session, "cfg_budget_project",
                      choices = c("Select project" = "", setNames(project_choices, project_choices)),
                      selected = v(isolate(input$cfg_budget_project)))
    department_choices <- uniq(c(DEPTS, d$department))
    updateSelectizeInput(session, "dash_department", choices = department_choices,
                         selected = intersect(keep("dash_department"), department_choices))

    # Dynamic cascading: if department(s) are selected in dashboard, restrict dash_project choices
    selected_dash_dept <- if (is.null(input$dash_department)) character() else as.character(input$dash_department)
    if (length(selected_dash_dept)) {
      db_projs <- tryCatch(
        db_get("SELECT project_code, department FROM projects"),
        error = function(e) data.frame(project_code = character(), department = character())
      )
      matched_db <- db_projs$project_code[db_projs$department %in% selected_dash_dept]
      matched_data <- d$project_code[d$department %in% selected_dash_dept]
      dash_project_choices <- sort(unique(c(matched_db, matched_data)))
      dash_project_choices <- dash_project_choices[!is.na(dash_project_choices) & nzchar(dash_project_choices)]
    } else {
      dash_project_choices <- project_choices
    }

    dash_project_selected <- intersect(keep("dash_project"), dash_project_choices)
    updateSelectizeInput(session, "dash_project", choices = dash_project_choices,
                         selected = dash_project_selected)
    years <- sort(unique(c(d$year,
                           format(suppressWarnings(as.Date(d$payment_date)), "%Y"),
                           format(Sys.Date(), "%Y"))), decreasing = TRUE)
    years <- years[!is.na(years) & nzchar(years)]
    updateSelectInput(session, "dash_year", choices = setNames(years, years),
                      selected = isolate(input$dash_year) %||% format(Sys.Date(), "%Y"))
    updateSelectizeInput(session, "f_person", choices = uniq(d$requested_by),
                         selected = keep("f_person"))
    updateSelectizeInput(session, "f_submitter", choices = uniq(d$submitted_by),
                         selected = keep("f_submitter"))
    
    # Keep these as reactive reads so the dependent choices refresh when the user changes them.
    project_selection <- if (is.null(input$f_project)) character() else as.character(input$f_project)
    selected_month <- input$f_month %||% ""
    bchoices <- budget_filter_choices(project_selection)
    budget_selected <- intersect(keep("f_budget_line"), unname(bchoices))
    updateSelectizeInput(session, "f_budget_line", choices = bchoices,
                         selected = budget_selected,
                         options = list(placeholder = if (length(project_selection))
                           "Choose budget line(s) for selected project(s)" else "Select project code(s) first"))
    
    # Request breakdown uses a separate, database-backed project selector when
    # Group by = Project. Without a selected project, show project totals only;
    # selecting one project reveals its total followed by its budget lines.
    current_dim <- isolate(input$dim) %||% "project_code"
    if (!(current_dim %in% unname(DIMS))) current_dim <- "project_code"
    updateSelectInput(session, "dim", choices = DIMS, selected = current_dim)
    
    breakdown_projects <- if (length(selected_dash_dept)) dash_project_choices else project_codes_db()
    if (length(dash_project_selected)) {
      breakdown_projects <- intersect(breakdown_projects, dash_project_selected)
    }
    breakdown_project_selected <- keep("breakdown_project")
    if (!length(breakdown_project_selected) ||
        !breakdown_project_selected[[1]] %in% breakdown_projects) {
      breakdown_project_selected <- ""
    } else {
      breakdown_project_selected <- breakdown_project_selected[[1]]
    }
    updateSelectInput(session, "breakdown_project",
                      choices = c("Select a project" = "", breakdown_projects),
                      selected = breakdown_project_selected)
    
    wchoices <- week_choices(d, selected_month)
    week_selected <- intersect(keep("f_week"), unname(wchoices))
    updateSelectizeInput(session, "f_week", choices = wchoices, selected = week_selected,
                         options = list(placeholder = "All weeks in selected month"))
  })

  active_currency <- function() {
    c_val <- v(input$global_cur)
    if (is.null(c_val) || !nzchar(c_val) || !c_val %in% CUR) "ETB" else c_val
  }

  observeEvent(input$reset_dash_filter, {
    updateSelectInput(session, "dash_period_type", selected = "month")
    updateSelectInput(session, "dash_year", selected = format(Sys.Date(), "%Y"))
    updateSelectInput(session, "dash_month", selected = format(Sys.Date(), "%m"))
    updateSelectInput(session, "dash_quarter", selected = paste0("Q", ceiling(as.integer(format(Sys.Date(), "%m")) / 3)))
    updateSelectizeInput(session, "dash_project", selected = character(0))
    updateSelectizeInput(session, "dash_department", selected = character(0))
    updateSelectInput(session, "breakdown_status", selected = "paid")
    updateSelectInput(session, "dim", selected = "project_code")
    updateSelectInput(session, "breakdown_project", selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_breakdown_project, {
    updateSelectInput(session, "breakdown_project", selected = "")
    updateSelectizeInput(session, "dash_project", selected = character(0))
  }, ignoreInit = TRUE)
  
  # ---- filters: month + week work together ----
  base <- reactive({
    d <- data()
      if (nzchar(v(input$f_month))) d <- filter(d, month == input$f_month)
      if (length(input$f_week)) d <- filter(d, week %in% input$f_week)
    if (length(input$f_project)) d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_budget_line)) d <- filter(d, budget_line_id %in% as.integer(input$f_budget_line))
    if (length(input$f_person)) d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_submitter)) d <- filter(d, submitted_by %in% input$f_submitter)
    query <- trimws(v(input$f_query))
    if (nzchar(query) && nrow(d)) {
      fields <- intersect(c("description", "requested_by", "submitted_by", "project_code", "budget_line", "order_form", "po"), names(d))
      pieces <- lapply(d[fields], function(col) ifelse(is.na(col), "", as.character(col)))
      blob <- if (length(pieces)) do.call(paste, c(pieces, list(sep = " "))) else rep("", nrow(d))
      blob <- paste(blob, d$id)
      d <- d[grepl(query, blob, ignore.case = TRUE), , drop = FALSE]
    }
    d
  })
  filtered <- reactive(base())
  
  # Dashboard period helpers. Request values/status counts follow order_date; actual paid spend follows payment_date.
  dashboard_period_bounds <- function(offset = 0L) {
    period_type <- v(input$dash_period_type)
    if (!period_type %in% c("month", "quarter", "year")) period_type <- "month"
    selected_year <- suppressWarnings(as.integer(v(input$dash_year)))
    if (is.na(selected_year)) selected_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (period_type == "month") {
      month_num <- suppressWarnings(as.integer(v(input$dash_month)))
      if (is.na(month_num) || month_num < 1 || month_num > 12) {
        month_num <- as.integer(format(Sys.Date(), "%m"))
      }
      selected_start <- as.Date(sprintf("%04d-%02d-01", selected_year, month_num))
      shift_month_start <- function(date, month_offset) {
        month_index <- as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m")) - 1L + as.integer(month_offset)
        new_year <- month_index %/% 12L
        new_month <- month_index %% 12L + 1L
        as.Date(sprintf("%04d-%02d-01", new_year, new_month))
      }
      start_date <- shift_month_start(selected_start, offset)
      next_start <- shift_month_start(start_date, 1L)
    } else if (period_type == "quarter") {
      quarter_num <- suppressWarnings(as.integer(gsub("[^1-4]", "", v(input$dash_quarter))))
      if (is.na(quarter_num) || quarter_num < 1 || quarter_num > 4) {
        quarter_num <- ceiling(as.integer(format(Sys.Date(), "%m")) / 3)
      }
      start_month <- (quarter_num - 1L) * 3L + 1L
      selected_start <- as.Date(sprintf("%04d-%02d-01", selected_year, start_month))
      month_index <- as.integer(format(selected_start, "%Y")) * 12L + as.integer(format(selected_start, "%m")) - 1L + (3L * as.integer(offset))
      shifted_year <- month_index %/% 12L
      shifted_month <- month_index %% 12L + 1L
      start_date <- as.Date(sprintf("%04d-%02d-01", shifted_year, shifted_month))
      next_month_index <- month_index + 3L
      next_year <- next_month_index %/% 12L
      next_month <- next_month_index %% 12L + 1L
      next_start <- as.Date(sprintf("%04d-%02d-01", next_year, next_month))
    } else {
      start_date <- as.Date(sprintf("%04d-01-01", selected_year + offset))
      next_start <- as.Date(sprintf("%04d-01-01", selected_year + offset + 1L))
    }
    list(start = start_date, end = next_start - 1)
  }
  
  dashboard_period_label <- function(offset = 0L) {
    bounds <- dashboard_period_bounds(offset)
    period_type <- v(input$dash_period_type)
    if (period_type == "quarter") {
      paste0("Q", ceiling(as.integer(format(bounds$start, "%m")) / 3), " ", format(bounds$start, "%Y"))
    } else if (period_type == "year") {
      format(bounds$start, "%Y")
    } else {
      format(bounds$start, "%B %Y")
    }
  }
  
  dashboard_scope <- function(d, date_column = "order_date", offset = 0L) {
    bounds <- dashboard_period_bounds(offset)
    dates <- suppressWarnings(as.Date(d[[date_column]]))
    keep <- !is.na(dates) & dates >= bounds$start & dates <= bounds$end
    d <- d[keep, , drop = FALSE]
    if (length(input$dash_project)) d <- d[d$project_code %in% input$dash_project, , drop = FALSE]
    if (length(input$dash_department)) d <- d[d$department %in% input$dash_department, , drop = FALSE]
    d
  }
  
  # Dashboard filters are independent from the Requests sidebar filters.
  dashboard_filtered <- reactive(dashboard_scope(data(), "order_date"))
  dashboard_paid_filtered <- reactive({
    dashboard_scope(data(), "payment_date") |>
      filter(status == "paid")
  })
  dashboard_previous_paid <- reactive({
    dashboard_scope(data(), "payment_date", offset = -1L) |>
      filter(status == "paid")
  })
  
  output$dashboard_meta <- renderUI({
    invisible(data())
    div(class = "dashboard-meta",
        span(icon("calendar-days"), paste("Period:", dashboard_period_label())),
        span(icon("coins"), paste("Currency:", active_currency()))
    )
  })
  
  observeEvent(input$reset, {
    # Clear means no active filters (All months), unlike the fresh-session default,
    # which intentionally starts on the current month.
    updateSelectInput(session, "f_month", selected = "")
    updateSelectizeInput(session, "f_week", selected = character(0))
    for (id in c("f_project", "f_budget_line", "f_person", "f_submitter"))
      updateSelectizeInput(session, id, selected = character(0))
    updateTextInput(session, "f_query", value = "")
    set_request_status("")
    # Also clear the project selector within Request breakdown so no secondary
    # selection continues to narrow the breakdown after dashboard filters reset.
    updateSelectInput(session, "breakdown_project", selected = "")
  }, ignoreInit = TRUE)
  
  observe({
    n <- sum(nzchar(v(input$f_month)), nzchar(v(input$f_query)), length(input$f_week) > 0,
             length(input$f_project) > 0, length(input$f_budget_line) > 0,
             length(input$f_person) > 0, length(input$f_submitter) > 0)
    session$sendCustomMessage("badge", list(n = n))
  })
  
  # Short, shareable URL containing only active filters.
  share_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    q <- list(
      v = "r2",
      cur = if (nzchar(active_currency())) active_currency() else NULL,
      m = if (nzchar(v(input$f_month))) input$f_month else NULL,
      w = if (length(input$f_week)) enc(input$f_week) else NULL,
      p = if (length(input$f_project)) enc(input$f_project) else NULL,
      b = if (length(input$f_budget_line)) enc(as.character(input$f_budget_line)) else NULL,
      r = if (length(input$f_person)) enc(input$f_person) else NULL,
      s = if (length(input$f_submitter)) enc(input$f_submitter) else NULL,
      find = if (nzchar(v(input$f_query))) URLencode(v(input$f_query), reserved = TRUE) else NULL,
      st = if (nzchar(request_table_status())) request_table_status() else NULL
    )
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname,
                   if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }
  
