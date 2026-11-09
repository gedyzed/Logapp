  calc_overdue_flags <- function(df) {
    if (!is.data.frame(df) || !nrow(df)) return(logical(0))
    vapply(seq_len(nrow(df)), function(i) {
      r <- df[i, , drop = FALSE]
      st <- if ("status" %in% names(r)) as.character(r$status[[1]]) else ""
      if (!identical(st, "pending")) return(FALSE)
      waiver_txt <- if ("waiver" %in% names(r)) as.character(r$waiver[[1]]) else "No"
      usd_val <- if ("usd" %in% names(r)) suppressWarnings(as.numeric(r$usd[[1]])) else NA_real_
      scen <- if ("procurement_scenario" %in% names(r) && nzchar(trimws(as.character(r$procurement_scenario[[1]] %||% "")))) {
        as.character(r$procurement_scenario[[1]])
      } else {
        scenario(usd_val, waiver_txt)
      }
      days_limit <- if (exists("SCENARIO_DAYS") && scen %in% names(SCENARIO_DAYS)) SCENARIO_DAYS[[scen]] else 14L
      pdate <- if ("po_date" %in% names(r)) suppressWarnings(as.Date(r$po_date[[1]])) else as.Date(NA)
      if (is.na(pdate)) {
        pdate <- if ("order_date" %in% names(r)) suppressWarnings(as.Date(r$order_date[[1]])) else as.Date(NA)
      }
      if (is.na(pdate) && "created" %in% names(r)) {
        pdate <- suppressWarnings(as.Date(substr(as.character(r$created[[1]]), 1, 10)))
      }
      if (is.na(pdate)) return(FALSE)
      as.numeric(app_today() - pdate) > days_limit
    }, logical(1))
  }

  output$request_status_cards <- renderUI({
    d <- filtered()
    selected <- request_table_status()
    cur <- active_currency()
    amount_of <- function(part) {
      if (!nrow(part) || !cur %in% names(part)) return(0)
      sum(suppressWarnings(as.numeric(part[[cur]])), na.rm = TRUE)
    }
    paid_week <- 0L
    if (nrow(d) && "payment_date" %in% names(d)) {
      paid <- d[d$status == "paid", , drop = FALSE]
      weeks <- format(suppressWarnings(as.Date(paid$payment_date)), "%G-W%V")
      paid_week <- sum(!is.na(weeks) & weeks == format(app_today(), "%G-W%V"))
    }
    pending_part <- if (nrow(d)) d[d$status == "pending" & !is.na(d$status), , drop = FALSE] else d[0, , drop = FALSE]
    pending_overdue_count <- sum(calc_overdue_flags(pending_part))
    pending_badge <- if (pending_overdue_count > 0) {
      div(class = "status-overdue-pill", icon("circle-exclamation"), paste("Overdue:", pending_overdue_count))
    } else {
      NULL
    }

    make_card <- function(id, label, status, tone, icon_name, note, extra_bottom = NULL) {
      part <- if (nzchar(status)) d[d$status == status & !is.na(d$status), , drop = FALSE] else d
      card <- KpiCard(
        id = id,
        label = label,
        value = format(nrow(part), big.mark = ","),
        secondaryValue = paste(cur, money(amount_of(part))),
        subtext = if (!is.null(extra_bottom)) extra_bottom else note,
        icon = icon(icon_name),
        tone = tone
      )
      if (identical(selected, status)) tagAppendAttributes(card, class = "is-active") else card
    }
    kpi_grid(
        make_card("card_status_waiting", "Under review", "under_review", "under_review", "magnifying-glass", "Awaiting review"),
        make_card("card_status_validation", "Pending", "pending", "pending", "clock", "In progress", extra_bottom = pending_badge),
        make_card("card_status_paid", "Paid", "paid", "paid", "circle-check",
                  if (paid_week > 0) paste0("+", paid_week, " this week") else "Paid requests"),
        make_card("card_status_rejected", "Cancelled", "cancelled", "cancelled", "circle-xmark", "Closed requests"))
  })
  
  # ---- Insights snapshot: the drawer edits a draft; Show results commits it ----
  insights_query <- reactiveVal(list(
    month = format(app_today(), "%Y-%m"),
    projects = character(),
    department = "",
    scenario = ""
  ))
  insights_days_all <- reactiveVal(FALSE)
  insights_lines_all <- reactiveVal(FALSE)

  insights_chart_notes <- list(
    spending_daily = "Total of requests marked Paid in the selected month, grouped by week. Cumulative shows the running total.",
    spending_cumulative = "Running total of requests marked Paid in the selected month. Each point is a date with a payment, and the last point is the month total.",
    breakdown = "Amounts for the selected status in the chosen month, grouped by project. Open a project to see its budget lines.",
    days = "Days from the order date to the payment date for paid requests in the selected month. On time means the request finished within its scenario allowance.",
    comparison_month = "Paid spend by month for up to five years, through the selected month. Each year is its own series.",
    comparison_quarter = "Paid spend by quarter for up to five years, through the quarter of the selected month. Each year is its own series."
  )

  insights_chart_note <- function(chart) {
    key <- chart
    if (identical(chart, "spending")) {
      key <- if (identical(v(input$insights_spend_mode), "cumulative")) "spending_cumulative" else "spending_daily"
    } else if (identical(chart, "comparison")) {
      key <- if (identical(v(input$insights_compare_mode), "quarter")) "comparison_quarter" else "comparison_month"
    }
    note <- insights_chart_notes[[key]]
    if (is.null(note)) "" else note
  }

  insights_info_icon <- function(chart) {
    info_tooltip(insights_chart_note(chart), "About this chart")
  }

  insights_day_stats <- function(on_time, actual, delay) {
    div(class = "insights-day-stats",
        div(class = "insights-day-stat", span("On time"), tags$b(on_time)),
        div(class = "insights-day-stat", span("Avg actual"), tags$b(actual)),
        div(class = "insights-day-stat", span("Avg delay"), tags$b(delay)))
  }

  insights_list_block <- function(items, expanded, input_id, class_name) {
    n <- length(items)
    show_n <- if (isTRUE(expanded) || n <= 10L) n else 10L
    shown <- if (show_n > 0L) items[seq_len(show_n)] else list()
    control <- if (n > 10L) {
      tags$button(
        type = "button",
        class = "insights-show-all",
        onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority:'event'})", input_id),
        if (isTRUE(expanded)) "Show less" else sprintf("Show all (%d)", as.integer(n))
      )
    }
    tagList(div(class = class_name, shown), control)
  }

  insights_commit <- function(month, projects, department, scenario) {
    if (is.null(month)) month <- ""
    projects <- insights_project_codes(projects)
    insights_query(list(
      month = as.character(month),
      projects = projects,
      department = if (is.null(department)) "" else as.character(department),
      scenario = if (is.null(scenario)) "" else as.character(scenario)
    ))
  }

  insights_project_codes <- function(raw) {
    raw <- as.character(raw %||% "")
    raw <- raw[!is.na(raw) & nzchar(raw) & !raw %in% c("all", "__none__")]
    unique(raw)
  }

  configured_expected_days <- function() {
    days <- as.numeric(SCENARIO_DAYS)
    days <- days[is.finite(days)]
    if (!length(days)) 0 else mean(days)
  }

  month_label <- function(month) {
    if (!nzchar(month %||% "")) return("All months")
    start <- safe_date(paste0(month, "-01"))
    if (length(start) != 1L || is.na(start)) as.character(month) else format(start, "%B %Y")
  }

  shift_month <- function(month, n) {
    start <- safe_date(paste0(month %||% "", "-01"))
    if (length(start) != 1L || is.na(start)) return("")
    format(seq(start, by = paste0(n, " months"), length.out = 2)[2], "%Y-%m")
  }

  render_quiet <- function(expr) {
    tryCatch(expr, error = function(e) div(class = "insights-empty insights-empty-fill", "No data"))
  }

  row_scenario <- function(d) {
    if (!is.data.frame(d) || !nrow(d)) return(character())
    if ("scenario" %in% names(d)) {
      base <- as.character(d$scenario)
    } else {
      usd <- if ("usd" %in% names(d)) d$usd else NA_real_
      waiver <- if ("waiver" %in% names(d)) d$waiver else "No"
      base <- as.character(scenario(usd, waiver))
    }
    if ("procurement_scenario" %in% names(d)) {
      stored <- trimws(as.character(d$procurement_scenario))
      use <- !is.na(stored) & nzchar(stored)
      base[use] <- stored[use]
    }
    base
  }

  insights_scope <- function(d, q) {
    if (!is.data.frame(d) || !nrow(d)) return(d)
    if (length(q$projects)) d <- d[d$project_code %in% q$projects, , drop = FALSE]
    if (nrow(d) && nzchar(q$department %||% "")) d <- d[d$department == q$department, , drop = FALSE]
    if (nrow(d) && nzchar(q$scenario %||% "")) d <- d[row_scenario(d) == q$scenario, , drop = FALSE]
    d
  }

  insights_in_month <- function(d, date_col, month) {
    empty <- if (is.data.frame(d)) d[0, , drop = FALSE] else d
    month <- as.character(month %||% "")
    if (!length(month) || !nzchar(month[[1]]) || month[[1]] %in% c("All", "All months")) return(d)
    if (!is.data.frame(d) || !nrow(d) || !date_col %in% names(d)) return(empty)
    dates <- safe_date(d[[date_col]])
    start <- safe_date(paste0(month[[1]], "-01"))
    if (length(start) != 1L || is.na(start)) return(empty)
    end <- seq(start, by = "1 month", length.out = 2)[2] - 1
    d[!is.na(dates) & dates >= start & dates <= end, , drop = FALSE]
  }

  sum_cur <- function(d) {
    cur <- active_currency()
    if (!is.data.frame(d) || !nrow(d) || !cur %in% names(d)) return(0)
    sum(suppressWarnings(as.numeric(d[[cur]])), na.rm = TRUE)
  }

  insights_paid_month <- function(month) {
    q <- insights_query()
    d <- data()
    d <- d[!is.na(d$status) & d$status == "paid", , drop = FALSE]
    insights_in_month(insights_scope(d, q), "payment_date", month)
  }

  insights_open_month <- function() {
    q <- insights_query()
    d <- data()
    d <- d[!is.na(d$status) & d$status %in% c("under_review", "pending"), , drop = FALSE]
    insights_in_month(insights_scope(d, q), "order_date", q$month)
  }

  kpi_amount <- function(x) {
    if (!is.finite(x)) x <- 0
    money(x)
  }

  delta_ui <- function(current, previous) kpi_delta(current, previous)

  fg_col <- function() if (identical(input$mode, "dark")) "#e4e4e7" else "#27272a"
  bar_col <- function() if (identical(input$mode, "dark")) "#38bdf8" else "#0284C7"

  chart_theme <- function(fg)
    theme_minimal(base_size = 12, base_family = "Inter") +
    theme(text = element_text(colour = fg), axis.text = element_text(colour = fg),
          axis.title = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = scales::alpha(fg, 0.16)),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "bottom", legend.title = element_blank(),
          plot.margin = margin(10, 18, 5, 5))

  vertical_grid <- function(fg)
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.16)))

  insights_choice_sig <- reactiveVal("")
  insights_inputs_ready <- reactiveVal(FALSE)
  observeEvent(input$insights_month, insights_inputs_ready(TRUE), once = TRUE)
  observe({
    d <- data()
    codes <- sort(unique(c(project_codes_db(), uniq(d$project_code))))
    codes <- codes[!is.na(codes) & nzchar(codes)]
    dept_db <- tryCatch(
      db_get("SELECT DISTINCT TRIM(department) AS department FROM projects
              WHERE department IS NOT NULL AND TRIM(department) <> ''"),
      error = function(e) data.frame(department = character())
    )
    deps <- sort(unique(c(as.character(dept_db$department), uniq(d$department))))
    deps <- deps[!is.na(deps) & nzchar(deps)]
    scen_vals <- sort(unique(row_scenario(d)))
    scen_vals <- scen_vals[!is.na(scen_vals) & nzchar(scen_vals)]
    scen_labels <- if (length(scen_vals)) {
      ifelse(scen_vals %in% names(SCEN_NAME), unname(SCEN_NAME[scen_vals]), scen_vals)
    } else {
      character()
    }
    months <- unique(c(format(app_today(), "%Y-%m"), d$month,
                       format(suppressWarnings(as.Date(d$payment_date)), "%Y-%m"),
                       format(suppressWarnings(as.Date(d$order_date)), "%Y-%m")))
    months <- months[!is.na(months) & nzchar(months)]
    client_ready <- isTRUE(isolate(insights_inputs_ready()))
    if (!client_ready) {
      sel <- "all"
      dep_sel <- isolate(insights_query()$department %||% "")
      month_sel <- isolate(insights_query()$month)
      scen_sel <- isolate(insights_query()$scenario %||% "")
    } else {
      raw_project <- as.character(isolate(input$insights_project) %||% "")
      if (any(raw_project == "all")) {
        sel <- "all"
      } else {
        sel <- insights_project_codes(raw_project)
        sel <- if (length(sel) && sel[[1]] %in% codes) sel[[1]] else ""
      }
      dep_sel <- isolate(v(input$insights_department))
      month_sel <- isolate(v(input$insights_month))
      scen_sel <- isolate(v(input$insights_scenario))
    }
    project_choices <- if (length(codes)) {
      c("All projects" = "all", setNames(codes, codes))
    } else {
      c("All projects" = "all", "No options found" = "__none__")
    }
    if (!nzchar(dep_sel) || identical(dep_sel, "__none__") || !dep_sel %in% deps) dep_sel <- ""
    if (!nzchar(scen_sel) || identical(scen_sel, "__none__") || !scen_sel %in% scen_vals) scen_sel <- ""
    month_opts <- month_choices(months)
    if (!length(month_opts)) month_opts <- month_choices(format(app_today(), "%Y-%m"))
    month_opts <- c(c("All months" = ""), month_opts)
    if (nzchar(month_sel) && !month_sel %in% as.character(month_opts)) month_sel <- format(app_today(), "%Y-%m")
    sig <- paste(paste(codes, collapse = "\n"), sel,
                 paste(deps, collapse = "\n"), dep_sel,
                 paste(scen_vals, collapse = "\n"), scen_sel,
                 paste(sort(months), collapse = "\n"), month_sel, sep = "\f")
    if (identical(sig, isolate(insights_choice_sig()))) return()
    insights_choice_sig(sig)
    updateSelectizeInput(session, "insights_project", choices = project_choices, selected = sel,
                         server = FALSE, options = list(placeholder = "All projects", dropdownParent = "body", allowEmptyOption = TRUE))
    updateSelectizeInput(session, "insights_department",
                         choices = filter_named(deps, all_label = "All departments"),
                         selected = dep_sel, server = FALSE,
                         options = list(placeholder = "All departments", dropdownParent = "body", allowEmptyOption = TRUE))
    updateSelectizeInput(session, "insights_scenario",
                         choices = filter_named(scen_vals, scen_labels, "All scenarios"),
                         selected = scen_sel, server = FALSE,
                         options = list(placeholder = "All scenarios", dropdownParent = "body", allowEmptyOption = TRUE))
    updateSelectizeInput(session, "insights_month",
                         choices = month_opts, selected = month_sel, server = FALSE,
                         options = list(dropdownParent = "body", allowEmptyOption = TRUE))
  })

  insights_filter_state <- debounce(reactive({
    list(
      month = input$insights_month %||% "",
      project = input$insights_project %||% "",
      department = input$insights_department %||% "",
      scenario = input$insights_scenario %||% ""
    )
  }), 300)

  observe({
    f <- insights_filter_state()
    mo <- v(f$month)
    if (!nzchar(mo) && !isTRUE(insights_inputs_ready())) mo <- isolate(insights_query()$month)
    projects <- insights_project_codes(f$project)
    if (identical(v(f$project), "__none__")) {
      updateSelectizeInput(session, "insights_project", selected = "")
    }
    dept <- v(f$department) %||% ""
    scen <- v(f$scenario) %||% ""
    if (dept %in% c("__none__", "all")) dept <- ""
    if (scen %in% c("__none__", "all")) scen <- ""

    current <- isolate(insights_query())
    if (!identical(current$month, mo) ||
        !identical(sort(current$projects), sort(as.character(projects))) ||
        !identical(current$department, as.character(dept)) ||
        !identical(current$scenario, as.character(scen))) {
      insights_commit(mo, projects, dept, scen)
      insights_drill_code("")
      insights_days_all(FALSE)
      insights_lines_all(FALSE)
    }
  })

  observeEvent(insights_query(), {
    insights_days_all(FALSE)
    insights_lines_all(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(insights_drill_code(), {
    insights_lines_all(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$insights_clear, {
    insights_inputs_ready(TRUE)
    updateSelectizeInput(session, "insights_month", selected = "")
    updateSelectizeInput(session, "insights_project", selected = "")
    updateSelectizeInput(session, "insights_department", selected = "")
    updateSelectizeInput(session, "insights_scenario", selected = "")
  }, ignoreInit = TRUE)

  output$insights_filter_badge <- renderUI({
    q <- insights_query()
    n <- (nzchar(q$department %||% "")) + (nzchar(q$scenario %||% ""))
    if (!n) return(NULL)
    span(class = "insights-filter-badge", n)
  })

  insights_status_filter <- reactiveVal("")

  observeEvent(input$insights_kpi_paid, {
    st <- insights_status_filter()
    current_chart <- v(input$dash_chart_select)
    if (identical(st, "paid") && !identical(current_chart, "days")) {
      insights_status_filter("")
    } else {
      insights_status_filter("paid")
      updateSelectInput(session, "breakdown_status", selected = "paid")
      if (identical(current_chart, "days")) {
        updateSelectInput(session, "dash_chart_select", selected = "spending")
      }
    }
  })

  observeEvent(input$insights_kpi_open, {
    st <- insights_status_filter()
    current_chart <- v(input$dash_chart_select)
    if ((identical(st, "pending") || identical(st, "under_review")) && !identical(current_chart, "days")) {
      insights_status_filter("")
    } else {
      insights_status_filter("pending")
      updateSelectInput(session, "breakdown_status", selected = "pending")
      updateSelectInput(session, "dash_chart_select", selected = "breakdown")
    }
  })

  observeEvent(input$insights_kpi_days, {
    current_chart <- v(input$dash_chart_select)
    if (identical(current_chart, "days")) {
      updateSelectInput(session, "dash_chart_select", selected = "spending")
    } else {
      insights_status_filter("")
      updateSelectInput(session, "dash_chart_select", selected = "days")
    }
  })

  output$insights_footer <- renderUI({
    q <- insights_query()
    dept <- if (nzchar(q$department %||% "")) q$department else "All departments"
    proj <- if (length(q$projects)) paste(q$projects, collapse = ", ") else "All projects"
    chart <- v(input$dash_chart_select)
    status_code <- if (identical(chart, "breakdown")) {
      picked <- if (nzchar(insights_status_filter())) insights_status_filter() else v(input$breakdown_status)
      if (picked %in% c("paid", "pending", "under_review", "cancelled")) picked else "paid"
    } else {
      "paid"
    }
    status_word <- switch(status_code,
                          pending = "Pending",
                          under_review = "Under review",
                          cancelled = "Cancelled",
                          "Paid")
    parts <- c(paste0("Status: ", status_word), month_label(q$month), dept, proj)
    if (nzchar(q$scenario %||% "")) {
      scen <- if (q$scenario %in% names(SCEN_NAME)) unname(SCEN_NAME[[q$scenario]]) else q$scenario
      parts <- c(parts, scen)
    }
    div(class = "insights-footer",
        icon("code-branch", class = "insights-footer-icon"),
        span(paste(parts, collapse = " \u00b7 ")))
  })

  output$totals <- renderUI({
    render_quiet({
    q <- insights_query()
    paid <- insights_paid_month(q$month)
    prev_key <- shift_month(q$month, -1)
    prev <- if (!nzchar(q$month %||% "") || !nzchar(prev_key)) paid else insights_paid_month(prev_key)
    open_rows <- insights_open_month()
    paid_amt <- sum_cur(paid)
    prev_amt <- sum_cur(prev)
    open_amt <- sum_cur(open_rows)
    n_review <- if (!nrow(open_rows)) 0L else sum(open_rows$status == "under_review", na.rm = TRUE)
    n_pending <- if (!nrow(open_rows)) 0L else sum(open_rows$status == "pending", na.rm = TRUE)
    cur <- active_currency()

    order_d <- safe_date(paid$order_date)
    pay_d <- safe_date(paid$payment_date)
    ok <- !is.na(order_d) & !is.na(pay_d)
    actual <- as.numeric(pay_d[ok] - order_d[ok])
    expected <- vapply(row_scenario(paid)[ok], function(s) {
      if (s %in% names(SCENARIO_DAYS)) as.numeric(SCENARIO_DAYS[[s]]) else NA_real_
    }, numeric(1))
    mean_actual <- if (length(actual)) mean(actual) else NA_real_
    mean_expected <- if (length(expected)) mean(expected, na.rm = TRUE) else NA_real_
    if (!is.finite(mean_expected)) mean_expected <- configured_expected_days()
    paid_delta <- delta_ui(paid_amt, prev_amt)
    paid_display <- paste(cur, kpi_amount(paid_amt))
    open_display <- paste(cur, kpi_amount(open_amt))
    days_display <- if (is.finite(mean_actual)) sprintf("%.1f days", mean_actual) else "0 days"
    days_note <- sprintf("vs %.0f expected", if (is.finite(mean_expected)) mean_expected else 0)
    status_on <- insights_status_filter()
    chart_on <- v(input$dash_chart_select)
    days_on <- identical(chart_on, "days")
    mark_active <- function(card, on) {
      if (isTRUE(on)) tagAppendAttributes(card, class = "is-active") else card
    }

    kpi_grid(
        mark_active(
          KpiCard(id = "insights_kpi_paid", label = "Paid spend",
                  value = format(nrow(paid), big.mark = ","),
                  secondaryValue = paid_display,
                  subtext = "Paid requests",
                  delta = paid_delta, icon = icon("circle-check"), tone = "paid"),
          !days_on && identical(status_on, "paid")),
        mark_active(
          KpiCard(id = "insights_kpi_open", label = "Open requests",
                  value = format(nrow(open_rows), big.mark = ","),
                  secondaryValue = open_display,
                  subtext = sprintf("%s under review \u00b7 %s pending",
                                    format(n_review, big.mark = ","),
                                    format(n_pending, big.mark = ",")),
                  icon = icon("clock"), tone = "pending"),
          !days_on && status_on %in% c("pending", "under_review")),
        mark_active(
          KpiCard(id = "insights_kpi_days", label = "Avg days to pay", value = days_display,
                  subtext = days_note, icon = icon("calendar-check"), tone = "days"),
          days_on)
    )
    })
  })

  output$insights_spend_title <- renderUI({
    q <- insights_query()
    div(class = "insights-plot-title",
        paste0("Paid spend by day \u00b7 ", month_label(q$month)),
        insights_info_icon("spending"))
  })

  output$plot_trend <- renderPlot({
    tryCatch({
    q <- insights_query()
    d <- insights_paid_month(q$month)
    cur <- active_currency()
    fg <- fg_col()

    amt_total <- if (nrow(d) && cur %in% names(d)) sum(suppressWarnings(as.numeric(d[[cur]])), na.rm = TRUE) else 0
    if (amt_total <= 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data", colour = fg, size = 5.5, fontface = "bold") +
               theme_void() +
               theme(plot.background = element_rect(fill = "transparent", colour = NA)))
    }

    weeks <- paste("Week", 1:5)
    totals <- setNames(rep(0, 5), weeks)
    if (nrow(d) && "payment_date" %in% names(d)) {
      pay <- safe_date(d$payment_date)
      day <- as.integer(format(pay, "%d"))
      bucket <- pmin(5L, ((day - 1L) %/% 7L) + 1L)
      amt <- if (cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else rep(0, nrow(d))
      ok <- !is.na(pay) & !is.na(bucket) & !is.na(amt)
      if (any(ok)) {
        summed <- tapply(amt[ok], bucket[ok], sum)
        totals[as.integer(names(summed))] <- as.numeric(summed)
      }
    }
    df <- data.frame(week = factor(weeks, levels = weeks), amount = as.numeric(totals))
    ggplot(df, aes(week, amount)) +
      geom_col(fill = bar_col(), width = 0.62) +
      scale_y_continuous(labels = function(x) abbr(x), expand = expansion(mult = c(0, 0.06))) +
      chart_theme(fg) + vertical_grid(fg) +
      theme(plot.margin = margin(2, 8, 0, 0))
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No data", size = 5.5, fontface = "bold") +
        theme_void()
    })
  }, bg = "transparent")

  output$insights_spend_cumulative <- renderUI({
    render_quiet({
    q <- insights_query()
    d <- insights_paid_month(q$month)
    cur <- active_currency()
    pay <- if (nrow(d) && "payment_date" %in% names(d)) safe_date(d$payment_date) else as.Date(character())
    amt <- if (nrow(d) && cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else numeric()
    if (length(amt) < length(pay)) amt <- c(amt, rep(NA_real_, length(pay) - length(amt)))
    ok <- !is.na(pay) & !is.na(amt) & is.finite(amt)
    if (!any(ok)) {
      return(div(class = "insights-empty insights-empty-fill insights-spend-empty", "No data"))
    }
    daily <- tapply(amt[ok], pay[ok], sum)
    counts <- tapply(amt[ok], pay[ok], length)
    dates <- sort(safe_date(names(daily)))
    dates <- dates[!is.na(dates)]
    daily_amt <- as.numeric(daily[as.character(dates)])
    counts <- as.integer(counts[as.character(dates)])
    daily_amt[!is.finite(daily_amt)] <- 0
    cum <- cumsum(daily_amt)
    start <- safe_date(paste0(q$month %||% "", "-01"))
    if (length(start) != 1L || is.na(start)) start <- min(dates)
    month_end <- seq(start, by = "1 month", length.out = 2)[2] - 1
    today <- app_today()
    axis_end <- month_end
    if (today >= start && today <= month_end) axis_end <- min(month_end, max(today, max(dates)))
    if (axis_end < max(dates)) axis_end <- max(dates)
    span <- as.numeric(axis_end - start)
    if (!is.finite(span) || span < 1) span <- 1
    xpos <- function(date) 6 + as.numeric(date - start) / span * 88
    peak <- comparison_axis_max(cum)
    ybottom <- function(value) {
      value <- as.numeric(value)
      if (!is.finite(peak) || peak <= 0) return(rep(0, length(value)))
      pmax(0, pmin(100, value / peak * 100))
    }
    ticks <- seq(0, peak, length.out = 5)
    colour <- bar_col()
    vx <- c(xpos(start), xpos(dates), xpos(axis_end))
    vy <- 100 - c(0, ybottom(cum), ybottom(cum[length(cum)]))
    points <- paste(sprintf("%.2f,%.2f", vx, vy), collapse = " ")
    date_label <- sprintf("%s %d, %s", format(dates, "%b"), as.integer(format(dates, "%d")), format(dates, "%Y"))
    markers <- lapply(seq_along(dates), function(i) {
      paid_txt <- paste0("+ ", cur, " ", money(daily_amt[[i]]))
      cum_txt <- paste(cur, money(cum[[i]]))
      count_txt <- if (counts[[i]] == 1L) "1 request" else paste(counts[[i]], "requests")
      tags$button(type = "button", class = "insights-point",
                  style = sprintf("left:%.2f%%;bottom:%.2f%%;--dot:%s", xpos(dates[[i]]), ybottom(cum[[i]]), colour),
                  `aria-label` = paste(date_label[[i]], cum_txt, paid_txt, count_txt, sep = ", "),
                  span(class = "insights-point-tip",
                       span(class = "insights-point-date", date_label[[i]]),
                       span(class = "insights-point-cum", cum_txt),
                       span(class = "insights-point-day", paid_txt),
                       span(class = "insights-point-n", count_txt)))
    })
    month_days <- as.integer(axis_end - start) + 1L
    n_weeks <- max(1L, min(5L, ((max(0L, month_days) - 1L) %/% 7L) + 1L))
    week_labels <- lapply(seq_len(n_weeks), function(w) {
      mid <- start + min(as.numeric(axis_end - start), (w - 1) * 7 + 3)
      span(class = "insights-compare-xlabel", style = sprintf("left:%.2f%%", xpos(mid)), paste("Week", w))
    })
    div(class = "insights-spend-line",
        div(class = "insights-compare-plot",
            div(class = "insights-compare-y", lapply(rev(ticks), function(t) span(abbr(t)))),
            div(class = "insights-compare-main",
                div(class = "insights-compare-area",
                    div(class = "insights-compare-grid", lapply(ticks, function(t) span())),
                    div(class = "insights-spend-guide"),
                    HTML(sprintf("<svg class=\"insights-spend-svg\" viewBox=\"0 0 100 100\" preserveAspectRatio=\"none\" aria-hidden=\"true\"><polyline points=\"%s\" fill=\"none\" stroke=\"%s\" stroke-width=\"2\" vector-effect=\"non-scaling-stroke\" stroke-linejoin=\"round\" stroke-linecap=\"round\"></polyline></svg>", points, colour)),
                    div(class = "insights-spend-points", markers)),
                div(class = "insights-compare-x insights-spend-x", week_labels))))
    })
  })

  breakdown_base <- reactive({
    q <- insights_query()
    status <- if (nzchar(insights_status_filter())) insights_status_filter() else v(input$breakdown_status)
    if (!status %in% STATUS) status <- "paid"
    d <- data()
    d <- d[!is.na(d$status) & d$status == status, , drop = FALSE]
    date_col <- if (identical(status, "paid")) "payment_date" else "order_date"
    insights_in_month(insights_scope(d, q), date_col, q$month)
  })

  insights_drill_code <- reactiveVal("")

  breakdown_plot_df <- reactive({
    d <- breakdown_base()
    cur <- active_currency()
    drill <- insights_drill_code()
    empty <- data.frame(label = character(), total = numeric(), share = numeric(), stringsAsFactors = FALSE)
    if (!nrow(d)) return(empty)
    amt <- if (cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else rep(0, nrow(d))
    amt[is.na(amt)] <- 0
    if (nzchar(drill)) {
      part <- d[d$project_code == drill, , drop = FALSE]
      if (!nrow(part)) return(empty)
      part_amt <- amt[d$project_code == drill]
      lines <- as.character(part$budget_line)
      lines[!nzchar(lines) | is.na(lines)] <- "(No budget line)"
      totals <- tapply(part_amt, lines, sum)
    } else {
      codes <- as.character(d$project_code)
      codes[!nzchar(codes) | is.na(codes)] <- "(No project)"
      totals <- sort(tapply(amt, codes, sum), decreasing = TRUE)
      if (length(totals) > 4) {
        top <- totals[1:4]
        totals <- c(top, stats::setNames(sum(totals[-(1:4)]), "Other"))
      }
    }
    if (!length(totals)) return(empty)
    grand <- sum(totals)
    data.frame(
      label = names(totals),
      total = as.numeric(totals),
      share = if (grand > 0) as.numeric(totals) / grand * 100 else rep(0, length(totals)),
      stringsAsFactors = FALSE
    )
  })

  observeEvent(input$insights_drill_back, {
    insights_drill_code("")
  }, ignoreInit = TRUE)

  observeEvent(input$insights_line_pick, {
    picked <- input$insights_line_pick
    code <- if (is.list(picked)) picked$code else picked
    code <- as.character(code %||% "")
    if (!nzchar(code) || code %in% c("Other", "(No project)")) return()
    if (nzchar(isolate(insights_drill_code()))) return()
    insights_drill_code(code)
  }, ignoreInit = TRUE)

  output$insights_breakdown <- renderUI({
    render_quiet({
    df <- breakdown_plot_df()
    q <- insights_query()
    status <- if (nzchar(insights_status_filter())) insights_status_filter() else v(input$breakdown_status)
    if (!status %in% STATUS) status <- "paid"
    drill <- insights_drill_code()
    status_word <- switch(status,
                          pending = "Pending",
                          under_review = "Under review",
                          cancelled = "Cancelled",
                          "Paid")
    fill_class <- switch(status,
                         pending = "is-pending",
                         under_review = "is-review",
                         cancelled = "is-cancelled",
                         "is-paid")
    subject <- if (nzchar(drill)) "budget line" else "project"
    
    if (!nrow(df) || sum(df$total, na.rm = TRUE) == 0) {
      return(tagList(
        if (nzchar(drill))
          actionButton("insights_drill_back", "Back to projects",
                       icon = icon("arrow-left"),
                       class = "btn btn-outline-secondary btn-sm insights-back"),
        div(class = "insights-line-head",
            div(class = "insights-plot-title",
                paste0(status_word, " by ", subject, " \u00b7 ", month_label(q$month)),
                insights_info_icon("breakdown"))
        ),
        div(class = "insights-empty insights-empty-fill", "No data")
      ))
    }

    named <- df[df$label != "Other", , drop = FALSE]
    total_amt <- sum(df$total)
    count_label <- if (nzchar(drill)) sprintf("%d budget lines", as.integer(nrow(df))) else sprintf("%d projects", as.integer(nrow(named)))
    peak <- max(df$total)

    rows <- lapply(seq_len(nrow(df)), function(i) {
      lab <- df$label[[i]]
      clickable <- !nzchar(drill) && !lab %in% c("Other", "(No project)")
      safe <- gsub("'", "\\'", lab, fixed = TRUE)
      width <- if (!is.finite(peak) || peak <= 0) 0 else round(100 * df$total[[i]] / peak)
      is_other <- grepl("^Other", lab)
      inner <- tagList(
        span(class = "insights-line-name", lab),
        span(class = "insights-line-track",
             span(class = paste("insights-line-fill", fill_class),
                  style = sprintf("width:%.0f%%", if (is_other) 0 else max(0, min(100, as.numeric(width)))))),
        span(class = "insights-line-meta",
             paste(abbr(df$total[[i]]), sprintf("%.0f%%", df$share[[i]]), sep = " \u00b7 "),
             if (clickable) span(class = "insights-line-chevron", "\u203a"))
      )
      if (!clickable) {
        div(class = "insights-line", inner)
      } else {
        tags$button(type = "button", class = "insights-line is-button",
                    onclick = sprintf("Shiny.setInputValue('insights_line_pick', {code:'%s', nonce:Math.random()}, {priority:'event'})", safe),
                    inner)
      }
    })

    tagList(
      if (nzchar(drill))
        actionButton("insights_drill_back", "Back to projects",
                     icon = icon("arrow-left"),
                     class = "btn btn-outline-secondary btn-sm insights-back"),
      div(class = "insights-line-head",
          div(class = "insights-plot-title",
              paste0(status_word, " by ", subject, " \u00b7 ", month_label(q$month)),
              insights_info_icon("breakdown")),
          span(class = "insights-line-summary",
               paste(paste(active_currency(), abbr(total_amt)), count_label, sep = " \u00b7 "))
      ),
      if (nzchar(drill)) {
        insights_list_block(rows, insights_lines_all(), "insights_lines_all_btn", "insights-lines")
      } else {
        div(class = "insights-lines", rows)
      }
    )
    })
  })

  output$insights_days <- renderUI({
    render_quiet({
    q <- insights_query()
    d <- insights_paid_month(q$month)
    order_d <- if (nrow(d)) safe_date(d$order_date) else as.Date(character())
    pay_d <- if (nrow(d)) safe_date(d$payment_date) else as.Date(character())
    ok <- !is.na(order_d) & !is.na(pay_d)
    
    if (!any(ok)) {
      return(tagList(
        div(class = "insights-line-head",
            div(class = "insights-plot-title",
                "Completion time by request",
                insights_info_icon("days"))
        ),
        insights_day_stats("0%", "0 days", "0 days"),
        div(class = "insights-empty insights-empty-fill", "No data")
      ))
    }

    part <- d[ok, , drop = FALSE]
    actual <- as.numeric(pay_d[ok] - order_d[ok])
    scen <- row_scenario(part)
    expected <- vapply(scen, function(s) {
      if (s %in% names(SCENARIO_DAYS)) as.numeric(SCENARIO_DAYS[[s]]) else NA_real_
    }, numeric(1))
    on_time <- !is.na(expected) & actual <= expected
    n <- length(actual)
    n_on <- sum(on_time)
    delay <- actual - expected
    avg_actual <- mean(actual)
    avg_delay <- mean(delay, na.rm = TRUE)
    delay_span <- suppressWarnings(max(abs(delay), na.rm = TRUE))
    if (!is.finite(delay_span) || delay_span <= 0) delay_span <- 1
    ref <- as.character(part$project_code)
    missing_code <- is.na(ref) | !nzchar(ref)
    ref[missing_code] <- paste0("REQ-", part$id[missing_code])
    order_idx <- order(pay_d[ok], part$id, decreasing = TRUE)
    rows <- lapply(order_idx, function(i) {
      late <- is.na(expected[i]) || expected[i] <= 0 || actual[i] > expected[i]
      width <- if (is.na(expected[i])) 4 else round(48 * abs(actual[i] - expected[i]) / delay_span)
      delta_txt <- if (is.na(expected[i])) "\u2014" else sprintf("%+.0f days", actual[i] - expected[i])
      div(class = "insights-day-row",
          div(class = "insights-day-ref",
              span(title = ref[i], ref[i]),
              span(class = "insights-day-ref-sep", "|")),
          div(class = "insights-day-track",
              div(class = paste("insights-day-fill", if (late) "is-late" else "is-ok"),
                  style = sprintf("width:%.0f%%", max(4, min(48, as.numeric(width)))))),
          span(class = paste("insights-day-delta", if (late) "is-late" else "is-ok"), delta_txt))
    })

    pct_on <- if (n > 0) round(100 * n_on / n) else 0
    actual_txt <- if (is.finite(avg_actual)) sprintf("%.1f days", avg_actual) else "0 days"
    delay_txt <- if (is.finite(avg_delay)) sprintf("%+.1f days", avg_delay) else "0 days"

    tagList(
      div(class = "insights-line-head",
          div(class = "insights-plot-title",
              "Completion time by request",
              insights_info_icon("days")),
          span(class = "insights-line-summary", sprintf("%d requests", as.integer(n)))
      ),
      insights_day_stats(
        sprintf("%.0f%%", as.numeric(pct_on)),
        actual_txt,
        delay_txt
      ),
      div(class = "insights-day-center-label", span("On schedule")),
      insights_list_block(rows, insights_days_all(), "insights_days_all_btn", "insights-day-rows")
    )
    })
  })

  observeEvent(input$insights_days_all_btn, {
    insights_days_all(!isTRUE(insights_days_all()))
  }, ignoreInit = TRUE)

  observeEvent(input$insights_lines_all_btn, {
    insights_lines_all(!isTRUE(insights_lines_all()))
  }, ignoreInit = TRUE)

  comparison_year_label <- function(years) {
    years <- sort(unique(as.character(years)))
    years <- years[!is.na(years) & nzchar(years)]
    if (!length(years)) return("")
    if (length(years) == 1) return(years)
    nums <- suppressWarnings(as.integer(years))
    if (!anyNA(nums) && all(diff(nums) == 1)) {
      paste(years[[1]], years[[length(years)]], sep = "\u2013")
    } else {
      paste(rev(years), collapse = ", ")
    }
  }

  comparison_axis_max <- function(values) {
    peak <- suppressWarnings(max(values, na.rm = TRUE))
    if (!is.finite(peak) || peak <= 0) return(1)
    raw <- peak / 4
    exp <- 10 ^ floor(log10(raw))
    marks <- c(1, 2, 2.5, 5, 10) * exp
    step <- marks[which(marks >= raw)[1]]
    if (!is.finite(step)) step <- 10 * exp
    step * 4
  }

  comparison_frame <- reactive({
    q <- insights_query()
    d <- data()
    d <- d[!is.na(d$status) & d$status == "paid", , drop = FALSE]
    d <- insights_scope(d, q)
    dates <- if (nrow(d)) suppressWarnings(as.Date(d$payment_date)) else as.Date(character())
    cur <- active_currency()
    amt <- if (nrow(d) && cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else numeric()
    ok <- !is.na(dates) & !is.na(amt)
    dates <- dates[ok]
    amt <- amt[ok]
    amt[!is.finite(amt)] <- 0
    present <- sort(unique(format(dates, "%Y")))
    present <- present[!is.na(present) & nzchar(present)]
    anchor <- substr(q$month %||% format(app_today(), "%Y-%m"), 1, 4)
    if (!nzchar(anchor)) anchor <- format(app_today(), "%Y")
    pool <- sort(unique(c(present, anchor)))
    pool <- pool[pool <= anchor]
    if (!length(pool)) pool <- anchor
    years <- tail(pool, 5)
    cutoff <- suppressWarnings(as.integer(substr(q$month %||% "", 6, 7)))
    if (is.na(cutoff) || cutoff < 1 || cutoff > 12) cutoff <- as.integer(format(app_today(), "%m"))
    list(dates = dates, amount = amt, years = years, month = q$month,
         cutoff = cutoff, currency = cur, has_rows = length(dates) > 0 && any(amt > 0))
  })

  output$insights_compare_title <- renderUI({
    by_month <- !identical(v(input$insights_compare_mode), "quarter")
    frame <- comparison_frame()
    base <- if (by_month) "Paid spend by month" else "Paid spend by quarter"
    label <- comparison_year_label(frame$years)
    title_text <- if (nzchar(label)) paste(base, label, sep = " \u00b7 ") else base
    span(class = "insights-plot-title",
         title_text,
         insights_info_icon("comparison"))
  })

  output$insights_compare_delta <- renderUI({
    frame <- comparison_frame()
    years <- frame$years
    if (length(years) < 2) return(NULL)
    newest <- years[length(years)]
    prior <- years[length(years) - 1]
    cutoff <- frame$cutoff
    if (is.na(cutoff)) cutoff <- as.integer(format(app_today(), "%m"))
    month_num <- as.integer(format(frame$dates, "%m"))
    year_num <- format(frame$dates, "%Y")
    current <- sum(frame$amount[year_num == newest & month_num <= cutoff], na.rm = TRUE)
    previous <- sum(frame$amount[year_num == prior & month_num <= cutoff], na.rm = TRUE)
    parts <- kpi_delta_parts(current, previous)
    if (is.null(parts)) return(NULL)
    shown <- round(abs(parts$pct))
    if (!is.finite(shown) || shown == 0) return(NULL)
    up <- isTRUE(parts$up)
    span(class = paste("insights-compare-delta", if (up) "is-up" else "is-down"),
         sprintf("%s %.0f%% vs %s (to date)", if (up) "\u25b2" else "\u25bc", shown, prior))
  })

  output$insights_compare_plot <- renderUI({
    render_quiet({
    frame <- comparison_frame()
    if (!isTRUE(frame$has_rows)) {
      return(div(class = "insights-empty insights-empty-fill", "No data"))
    }
    by_month <- !identical(v(input$insights_compare_mode), "quarter")
    years <- frame$years
    cutoff <- frame$cutoff
    if (by_month) {
      periods <- seq_len(cutoff)
      period_labels <- month.abb[periods]
      period_names <- month.name[periods]
    } else {
      last_q <- ((cutoff - 1L) %/% 3L) + 1L
      periods <- seq_len(last_q)
      period_labels <- paste0("Q", periods)
      period_names <- period_labels
    }
    amounts <- lapply(years, function(yr) {
      in_year <- format(frame$dates, "%Y") == yr
      month_num <- as.integer(format(frame$dates[in_year], "%m"))
      bucket <- if (by_month) month_num else ((month_num - 1L) %/% 3L) + 1L
      out <- rep(0, length(periods))
      keep <- !is.na(bucket) & bucket %in% periods
      if (any(keep)) {
        summed <- tapply(frame$amount[in_year][keep], bucket[keep], sum)
        idx <- as.integer(names(summed))
        ok_idx <- !is.na(idx) & idx >= 1 & idx <= length(out)
        out[idx[ok_idx]] <- as.numeric(summed)[ok_idx]
      }
      out[!is.finite(out)] <- 0
      out
    })
    peak <- comparison_axis_max(unlist(amounts))
    ticks <- seq(0, peak, length.out = 5)
    is_dark <- identical(input$mode, "dark")
    palette <- if (is_dark) {
      c("#38BDF8", "#2DD4BF", "#FBBF24", "#C4B5FD", "#FB7185")
    } else {
      c("#0284C7", "#0F766E", "#D97706", "#7C3AED", "#BE123C")
    }
    newest_first <- rev(years)
    fills <- setNames(palette[seq_along(newest_first)], newest_first)
    cur <- frame$currency
    columns <- lapply(seq_along(periods), function(i) {
      bars <- lapply(seq_along(years), function(yi) {
        value <- amounts[[yi]][[i]]
        if (!is.finite(value)) value <- 0
        pct <- if (!is.finite(peak) || peak <= 0) 0 else 100 * value / peak
        tip <- paste(paste(cur, money(value)), period_names[[i]], years[[yi]], sep = " \u00b7 ")
        tags$button(type = "button", class = "insights-bar",
                    style = sprintf("height:%.4f%%;background:%s", max(0, min(100, pct)), unname(fills[[years[[yi]]]])),
                    `data-tip` = tip, `aria-label` = tip)
      })
      div(class = "insights-compare-col", div(class = "insights-compare-group", bars))
    })
    labels <- lapply(period_labels, function(lab) span(class = "insights-compare-xlabel", lab))
    legend <- lapply(years, function(yr) {
      span(class = "insights-compare-key",
           span(class = "insights-compare-swatch", style = sprintf("background:%s", unname(fills[[yr]]))),
           yr)
    })
    div(class = "insights-compare",
        div(class = "insights-compare-plot",
            div(class = "insights-compare-y", lapply(rev(ticks), function(t) span(abbr(t)))),
            div(class = "insights-compare-main",
                div(class = "insights-compare-area",
                    div(class = "insights-compare-grid", lapply(ticks, function(t) span())),
                    div(class = "insights-compare-cols", columns)),
                div(class = "insights-compare-x", labels))),
        div(class = "insights-compare-legend", legend))
    })
  })

  share_dashboard_url <- function() {
    q <- insights_query()
    cd <- session$clientData
    host <- paste0(cd$url_hostname, if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
    parts <- c(
      "v=dash",
      paste0("cur=", enc(active_currency())),
      paste0("mo=", enc(q$month %||% "")),
      paste0("chart=", enc(v(input$dash_chart_select))),
      paste0("smode=", enc(if (nzchar(v(input$insights_spend_mode))) v(input$insights_spend_mode) else "daily")),
      paste0("cmode=", enc(if (nzchar(v(input$insights_compare_mode))) v(input$insights_compare_mode) else "month")),
      paste0("bstat=", enc(if (nzchar(v(input$breakdown_status))) v(input$breakdown_status) else "paid"))
    )
    if (length(q$projects)) parts <- c(parts, paste0("p=", paste(vapply(q$projects, enc, character(1)), collapse = "|")))
    if (nzchar(q$department %||% "")) parts <- c(parts, paste0("d=", enc(q$department)))
    if (nzchar(q$scenario %||% "")) parts <- c(parts, paste0("sc=", enc(q$scenario)))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?", paste(parts, collapse = "&"))
  }

  output$request_table_title <- renderUI({
    d <- filtered()
    selected_status <- request_table_status()
    if (nzchar(selected_status) && selected_status %in% STATUS) {
      d <- d[d$status == selected_status, , drop = FALSE]
    }
    span(class = "fw-semibold", sprintf("All requests (%s)", format(nrow(d), big.mark = ",")))
  })

  # ---- request register: click a row to open that request ----
  output$table_requests <- renderDT({
    d <- filtered()
    selected_status <- request_table_status()
    if (nzchar(selected_status) && selected_status %in% STATUS) {
      d <- d[d$status == selected_status, , drop = FALSE]
    }
    
    blank <- function(x) ifelse(is.na(x) | !nzchar(as.character(x)), "", as.character(x))
    reference <- if ("order_form" %in% names(d)) blank(d$order_form) else rep("", nrow(d))
    reference[!nzchar(reference)] <- paste0("REQ-", d$id[!nzchar(reference)])
    overdue_flags <- calc_overdue_flags(d)
    raw_status <- blank(d$status)
    encoded_status <- ifelse(overdue_flags, paste0(raw_status, ":overdue"), raw_status)
    display <- data.frame(
      id = d$id,
      requested_by = blank(d$requested_by),
      reference = reference,
      description = blank(d$description),
      project_code = blank(d$project_code),
      budget_line = blank(d$budget_line),
      amount = ifelse(is.na(d$amount), "", paste0(d$currency, " ", vapply(d$amount, money, character(1)))),
      status = encoded_status,
      check.names = FALSE
    )
    names(display) <- c("id", "Requester", "Reference", "Description", "Project", "Budget line", "Amount", "Status")
    
    datatable(display, rownames = FALSE, selection = "none", filter = "none",
              fillContainer = FALSE, escape = TRUE,
              options = list(
                scrollX = FALSE, autoWidth = FALSE, pageLength = 10,
                dom = "rt<'dt-foot'ip>",
                language = dt_language, initComplete = request_table_init(),
                rowCallback = JS("function(row, data) { $(row).attr('data-id', data[0]); }"),
                columnDefs = list(
                  list(targets = 0, visible = FALSE, searchable = FALSE, orderable = FALSE, width = "0px"),
                  list(targets = 1, width = "11%"),
                  list(targets = 2, width = "17%", render = JS("function(data, type) { if (type !== 'display') return data; return '<span class=\"req-ref\">' + data + '</span>'; }")),
                  list(targets = 3, width = "16%", render = more_render("Description")),
                  list(targets = 4, width = "11%"),
                  list(targets = 5, width = "13%"),
                  list(targets = 6, width = "18%"),
                  list(targets = 7, width = "14%", orderable = FALSE, render = JS(r"---(
function(data, type) {
  if (type !== 'display') return data;
  var raw = String(data || '');
  var isOverdue = raw.indexOf(':overdue') !== -1;
  var key = raw.replace(':overdue', '');
  var labels = {under_review: 'Under review', pending: 'Pending', paid: 'Paid', cancelled: 'Cancelled'};
  var html = '<div class="req-status-cell"><span class="req-pill req-pill-' + key + '">&#8226; ' + (labels[key] || key) + '</span>';
  if (isOverdue) {
    html += '<div class="req-overdue-flag"><i class="far fa-clock"></i> Overdue</div>';
  }
  html += '</div>';
  return html;
}
)---")),
                  list(targets = c(1, 4, 5, 6), className = "dt-body-left")
                )
              ))
  }, server = TRUE)
  
  output$export <- downloadHandler(
    filename = "expenses.xlsx",
    content  = function(file) {
      d <- filtered()
      selected_status <- request_table_status()
      if (nzchar(selected_status) && selected_status %in% STATUS)
        d <- d[d$status == selected_status, , drop = FALSE]
      write_xlsx(select(d, -any_of(c("week", "month", "budget_line_id"))), file)
    }
  )

