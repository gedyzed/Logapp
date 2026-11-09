
  touched  <- reactiveVal(character())
  show_all <- reactiveVal(FALSE)
  edit_id  <- reactiveVal(NULL)
  request_table_status <- reactiveVal("")
  project_edit_id <- reactiveVal(NULL)
  supplier_edit_id <- reactiveVal(NULL)
  person_edit_id <- reactiveVal(NULL)
  email_edit_id <- reactiveVal(NULL)
  
  set_request_status <- function(status = "") {
    status <- if (status %in% STATUS) status else ""
    request_table_status(status)
    session$sendCustomMessage("setTableStatus", list(status = status))
  }
  toggle_request_status <- function(target_status) {
    new_status <- if (identical(request_table_status(), target_status)) "" else target_status
    set_request_status(new_status)
  }
  observeEvent(input$card_status_all, set_request_status(""), ignoreInit = TRUE)
  observeEvent(input$card_status_waiting, toggle_request_status("under_review"), ignoreInit = TRUE)
  observeEvent(input$card_status_validation, toggle_request_status("pending"), ignoreInit = TRUE)
  observeEvent(input$card_status_paid, toggle_request_status("paid"), ignoreInit = TRUE)
  observeEvent(input$card_status_rejected, toggle_request_status("cancelled"), ignoreInit = TRUE)
  observeEvent(input$chip_status_all, set_request_status(""), ignoreInit = TRUE)
  observeEvent(input$chip_status_waiting, toggle_request_status("under_review"), ignoreInit = TRUE)
  observeEvent(input$chip_status_validation, toggle_request_status("pending"), ignoreInit = TRUE)
  observeEvent(input$chip_status_paid, toggle_request_status("paid"), ignoreInit = TRUE)
  observeEvent(input$chip_status_rejected, toggle_request_status("cancelled"), ignoreInit = TRUE)
  
  
  # ---- read live rows from the shared SQLite database ----
  # Keep this connection open for PRAGMA data_version, which changes after commits
  # by other connections (including other Shiny sessions and external writers).
  poll_cn <- con()
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(poll_cn)) DBI::dbDisconnect(poll_cn)
  })
  load_expense_rows <- function(cn) {
    rows <- dbGetQuery(cn,
                       "SELECT e.*, p.project_code, bl.budget_line_code AS budget_line
         FROM expenses e
         LEFT JOIN budget_lines bl ON bl.id = e.budget_line_id
         LEFT JOIN projects p ON p.id = bl.project_id
         ORDER BY e.id DESC")
    # Include the database revision so catalogue/default changes invalidate dependants
    # even when the expense rows themselves are unchanged.
    attr(rows, "database_revision") <- as.character(dbGetQuery(cn, "PRAGMA data_version")[[1]][[1]])
    rows
  }
  db_poll <- reactivePoll(
    1000, session,
    checkFunc = function() as.character(dbGetQuery(poll_cn, "PRAGMA data_version")[[1]][[1]]),
    valueFunc = function() load_expense_rows(poll_cn)
  )
  # Same-session writes do not change data_version on poll_cn. Mark Paid stores a
  # fresh read here so Financial Updates does not wait for the next poll.
  expense_rows <- reactiveVal(NULL)
  observeEvent(db_poll(), {
    expense_rows(db_poll())
  }, ignoreInit = FALSE)
  reload_expenses <- function() {
    cn <- con()
    on.exit(if (DBI::dbIsValid(cn)) DBI::dbDisconnect(cn), add = TRUE)
    expense_rows(load_expense_rows(cn))
  }
  dashboard_last_refresh <- reactiveVal(Sys.time())
  observeEvent(db_poll(), dashboard_last_refresh(Sys.time()), ignoreInit = FALSE)
  
  data <- reactive({
    d <- expense_rows()
    req(is.data.frame(d))
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
    date_value <- safe_date(d$order_date)
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
      summarise(first = {
        parsed <- safe_date(order_date)
        if (!any(!is.na(parsed))) as.Date(NA) else min(parsed, na.rm = TRUE)
      }, .groups = "drop") |>
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

  # Skip an input update when choices and selection are already what we last sent.
  # Clear all otherwise echoes "" / character(0) back into this observer forever.
  filter_sent <- new.env(parent = emptyenv())
  push_input <- function(id, choices, selected, selectize = TRUE, server = NULL, options = NULL) {
    choice_vals <- as.character(unname(choices %||% character()))
    choice_labs <- names(choices)
    if (is.null(choice_labs)) choice_labs <- choice_vals
    sel <- as.character(selected %||% character())
    sel <- sel[!is.na(sel)]
    placeholder <- if (is.list(options) && !is.null(options$placeholder)) as.character(options$placeholder) else ""
    sig <- paste(
      paste(choice_vals, as.character(choice_labs), sep = "=", collapse = "\u001f"),
      paste(sel, collapse = "\u001f"),
      placeholder,
      sep = "\u001e"
    )
    if (identical(filter_sent[[id]], sig)) return(invisible(FALSE))
    filter_sent[[id]] <- sig
    if (isTRUE(selectize)) {
      args <- list(session = session, inputId = id, choices = choices, selected = selected)
      if (!is.null(server)) args$server <- server
      if (!is.null(options)) args$options <- options
      do.call(updateSelectizeInput, args)
    } else {
      updateSelectInput(session, id, choices = choices, selected = selected)
    }
    invisible(TRUE)
  }
  
  # Populate filters from the database. Month constrains Week; selecting a project
  # constrains Budget line to that project's configured mappings.
  observe({
    d <- data()
    keep <- function(id) {
      x <- isolate(input[[id]])
      if (is.null(x)) character() else as.character(x)
    }
    current_month <- format(app_today(), "%Y-%m")
    available_months <- unique(c(d$month, current_month))
    month_selected <- isolate(input$f_month) %||% current_month
    if (identical(month_selected, "__none__")) month_selected <- ""
    month_opts <- month_choices(available_months)
    push_input("f_month",
               choices = filter_named(unname(month_opts), names(month_opts), "All months"),
               selected = month_selected, server = FALSE,
               options = list(dropdownParent = "body", allowEmptyOption = TRUE))
    # Project choices come from the projects table, including newly configured projects
    # that do not yet have an expense record.
    project_choices <- project_codes_db()
    project_selected <- intersect(keep("f_project"), project_choices)
    project_selected <- project_selected[project_selected != "__none__"]
    push_input("f_project",
               choices = if (length(project_choices)) project_choices else c("No options found" = "__none__"),
               selected = project_selected, server = FALSE,
               options = list(placeholder = if (length(project_choices)) "All projects" else "No options found",
                              dropdownParent = "body"))
    push_input("cfg_budget_project",
               choices = c("Select project" = "", setNames(project_choices, project_choices)),
               selected = v(isolate(input$cfg_budget_project)), selectize = FALSE)
    department_choices <- uniq(c(DEPTS, d$department))
    push_input("dash_department", choices = department_choices,
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
    push_input("dash_project", choices = dash_project_choices, selected = dash_project_selected)
    years <- sort(unique(c(d$year,
                           format(suppressWarnings(as.Date(d$payment_date)), "%Y"),
                           format(app_today(), "%Y"))), decreasing = TRUE)
    years <- years[!is.na(years) & nzchar(years)]
    push_input("dash_year", choices = setNames(years, years),
               selected = isolate(input$dash_year) %||% format(app_today(), "%Y"),
               selectize = FALSE)
    person_choices <- uniq(d$requested_by)
    submitter_choices <- uniq(d$submitted_by)
    push_input("f_person",
               choices = if (length(person_choices)) person_choices else c("No options found" = "__none__"),
               selected = intersect(keep("f_person"), person_choices), server = FALSE,
               options = list(placeholder = if (length(person_choices)) "All requesters" else "No options found",
                              dropdownParent = "body"))
    push_input("f_submitter",
               choices = if (length(submitter_choices)) submitter_choices else c("No options found" = "__none__"),
               selected = intersect(keep("f_submitter"), submitter_choices), server = FALSE,
               options = list(placeholder = if (length(submitter_choices)) "All focal points" else "No options found",
                              dropdownParent = "body"))
    
    # Keep these as reactive reads so the dependent choices refresh when the user changes them.
    project_selection <- if (is.null(input$f_project)) character() else as.character(input$f_project)
    project_selection <- project_selection[!is.na(project_selection) & nzchar(project_selection) & project_selection != "__none__"]
    selected_month <- input$f_month %||% ""
    if (identical(selected_month, "__none__")) selected_month <- ""
    bchoices <- budget_filter_choices(project_selection)
    budget_selected <- intersect(keep("f_budget_line"), unname(bchoices))
    budget_locked <- !length(project_selection)
    budget_placeholder <- if (budget_locked) {
      "Select project code(s) first"
    } else if (!length(bchoices)) {
      "No options found"
    } else {
      "All lines"
    }
    push_input("f_budget_line",
               choices = if (length(bchoices)) bchoices else if (!budget_locked) c("No options found" = "__none__") else character(),
               selected = budget_selected, server = FALSE,
               options = list(placeholder = budget_placeholder, dropdownParent = "body"))
    lock_sig <- paste(budget_locked, budget_placeholder, sep = "\u001e")
    if (!identical(filter_sent[["filterLock"]], lock_sig)) {
      filter_sent[["filterLock"]] <- lock_sig
      session$sendCustomMessage("filterLock", list(
        id = "f_budget_line", locked = budget_locked, placeholder = budget_placeholder
      ))
    }
    
    # Request breakdown uses a separate, database-backed project selector when
    # Group by = Project. Without a selected project, show project totals only;
    # selecting one project reveals its total followed by its budget lines.
    current_dim <- isolate(input$dim) %||% "project_code"
    if (!(current_dim %in% unname(DIMS))) current_dim <- "project_code"
    push_input("dim", choices = DIMS, selected = current_dim, selectize = FALSE)
    
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
    push_input("breakdown_project",
               choices = c("Select a project" = "", breakdown_projects),
               selected = breakdown_project_selected, selectize = FALSE)
    
    wchoices <- week_choices(d, selected_month)
    week_selected <- intersect(keep("f_week"), unname(wchoices))
    push_input("f_week",
               choices = if (length(wchoices)) wchoices else c("No options found" = "__none__"),
               selected = week_selected, server = FALSE,
               options = list(placeholder = if (length(wchoices)) "All weeks" else "No options found",
                              dropdownParent = "body"))
  })

  active_currency <- function() {
    c_val <- v(input$global_cur)
    if (is.null(c_val) || !nzchar(c_val) || !c_val %in% CUR) "ETB" else c_val
  }

  observeEvent(input$reset_dash_filter, {
    updateSelectInput(session, "dash_period_type", selected = "month")
    updateSelectInput(session, "dash_year", selected = format(app_today(), "%Y"))
    updateSelectInput(session, "dash_month", selected = format(app_today(), "%m"))
    updateSelectInput(session, "dash_quarter", selected = paste0("Q", ceiling(as.integer(format(app_today(), "%m")) / 3)))
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
  request_filters <- debounce(reactive({
    list(
      month = input$f_month %||% "",
      week = input$f_week %||% character(),
      project = input$f_project %||% character(),
      budget = input$f_budget_line %||% character(),
      person = input$f_person %||% character(),
      submitter = input$f_submitter %||% character(),
      query = input$f_query %||% ""
    )
  }), 300)

  base <- reactive({
    d <- data()
    f <- request_filters()
    picked <- function(x) {
      x <- as.character(x %||% character())
      x[!is.na(x) & nzchar(x) & x != "__none__"]
    }
    if (nzchar(v(f$month)) && !identical(v(f$month), "__none__")) d <- filter(d, month == f$month)
    if (length(picked(f$week))) d <- filter(d, week %in% picked(f$week))
    if (length(picked(f$project))) d <- filter(d, project_code %in% picked(f$project))
    if (length(picked(f$budget))) d <- filter(d, budget_line_id %in% as.integer(picked(f$budget)))
    if (length(picked(f$person))) d <- filter(d, requested_by %in% picked(f$person))
    if (length(picked(f$submitter))) d <- filter(d, submitted_by %in% picked(f$submitter))
    query <- trimws(v(f$query))
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

  # Weekly report: all requests registered from Monday of current week through today.
  # The display window is today only (for testing; production: Friday afternoon).
  weekly_data <- reactive({
    d <- data()
    today <- app_today()
    # Monday of current week
    mon <- today - (as.integer(format(today, "%u")) - 1L)
    reg_dates <- suppressWarnings(as.Date(substr(as.character(d$created), 1, 10)))
    d[!is.na(reg_dates) & reg_dates >= mon & reg_dates <= today, , drop = FALSE]
  })

  weekly_window_active <- reactive({
    if (is_dev_mode()) return(TRUE)
    time <- as.POSIXct(app_now(), tz = "Africa/Addis_Ababa")
    local <- as.POSIXlt(time, tz = "Africa/Addis_Ababa")
    weekday <- local$wday
    minutes <- local$hour * 60L + local$min
    (weekday == 5L && minutes >= 13L * 60L) || (weekday == 6L && minutes < 17L * 60L)
  })
  
  # Dashboard period helpers. Request values/status counts follow order_date; actual paid spend follows payment_date.
  dashboard_period_bounds <- function(offset = 0L) {
    period_type <- v(input$dash_period_type)
    if (!period_type %in% c("month", "quarter", "year")) period_type <- "month"
    selected_year <- suppressWarnings(as.integer(v(input$dash_year)))
    if (is.na(selected_year)) selected_year <- as.integer(format(app_today(), "%Y"))
    
    if (period_type == "month") {
      month_num <- suppressWarnings(as.integer(v(input$dash_month)))
      if (is.na(month_num) || month_num < 1 || month_num > 12) {
        month_num <- as.integer(format(app_today(), "%m"))
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
        quarter_num <- ceiling(as.integer(format(app_today(), "%m")) / 3)
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
    updateSelectizeInput(session, "f_month", selected = "")
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
      m = if (nzchar(v(input$f_month)) && !identical(v(input$f_month), "__none__")) input$f_month else NULL,
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

