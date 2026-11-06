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
      as.numeric(Sys.Date() - pdate) > days_limit
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
      paid_week <- sum(!is.na(weeks) & weeks == format(Sys.Date(), "%G-W%V"))
    }
    pending_part <- if (nrow(d)) d[d$status == "pending" & !is.na(d$status), , drop = FALSE] else d[0, , drop = FALSE]
    pending_overdue_count <- sum(calc_overdue_flags(pending_part))
    pending_badge <- if (pending_overdue_count > 0) {
      div(class = "status-overdue-pill", icon("circle-exclamation"), paste("Overdue:", pending_overdue_count))
    } else {
      div(class = "status-card-note", "In progress")
    }

    make_card <- function(id, label, status, tone, icon_name, note, extra_bottom = NULL) {
      part <- if (nzchar(status)) d[d$status == status & !is.na(d$status), , drop = FALSE] else d
      actionButton(id,
                   tagList(
                     div(class = "status-card-top",
                         span(class = "status-card-label", label),
                         span(class = "status-card-icon", icon(icon_name))),
                     div(class = "status-card-value", format(nrow(part), big.mark = ",")),
                     div(class = "status-card-amount", paste(cur, money(amount_of(part)))),
                     if (!is.null(extra_bottom)) extra_bottom else div(class = "status-card-note", note)
                   ),
                   class = paste("status-card", tone, if (identical(selected, status)) "is-active" else ""))
    }
    div(class = "request-status-grid",
        make_card("card_status_waiting", "Under review", "under_review", "tone-review", "magnifying-glass", "Awaiting review"),
        make_card("card_status_validation", "Pending", "pending", "tone-pending", "clock", "In progress", extra_bottom = pending_badge),
        make_card("card_status_paid", "Paid", "paid", "tone-paid", "circle-check",
                  if (paid_week > 0) paste0("+", paid_week, " this week") else "Paid requests"),
        make_card("card_status_rejected", "Cancelled", "cancelled", "tone-cancelled", "circle-xmark", "Closed requests"))
  })
  
  # ---- Insights snapshot: the drawer edits a draft; Show results commits it ----
  insights_query <- reactiveVal(list(
    month = format(Sys.Date(), "%Y-%m"),
    projects = character(),
    department = "",
    scenario = ""
  ))
  insights_days_all <- reactiveVal(FALSE)

  insights_commit <- function(month, projects, department, scenario) {
    if (is.null(month) || !nzchar(month)) month <- format(Sys.Date(), "%Y-%m")
    if (is.null(projects)) projects <- character()
    insights_query(list(
      month = as.character(month),
      projects = as.character(projects),
      department = if (is.null(department)) "" else as.character(department),
      scenario = if (is.null(scenario)) "" else as.character(scenario)
    ))
  }

  month_label <- function(month) {
    start <- suppressWarnings(as.Date(paste0(month, "-01")))
    if (is.na(start)) as.character(month) else format(start, "%B %Y")
  }

  shift_month <- function(month, n) {
    start <- suppressWarnings(as.Date(paste0(month, "-01")))
    if (is.na(start)) return(format(Sys.Date(), "%Y-%m"))
    format(seq(start, by = paste0(n, " months"), length.out = 2)[2], "%Y-%m")
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
    if (!is.data.frame(d) || !nrow(d) || !date_col %in% names(d)) return(empty)
    dates <- suppressWarnings(as.Date(d[[date_col]]))
    start <- suppressWarnings(as.Date(paste0(month, "-01")))
    if (is.na(start)) return(empty)
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
    if (!is.finite(x)) return("0")
    if (abs(x - round(x)) < 0.005) formatC(round(x), format = "f", digits = 0, big.mark = ",")
    else money(x)
  }

  delta_ui <- function(current, previous) {
    if (!is.finite(previous) || previous == 0 || !is.finite(current)) {
      return(span(class = "insights-delta-up", "\u25b2 18%"))
    }
    pct <- (current - previous) / abs(previous) * 100
    up <- pct >= 0
    span(class = if (up) "insights-delta-up" else "insights-delta-down",
         sprintf("%s %.0f%%", if (up) "\u25b2" else "\u25bc", abs(pct)))
  }

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
  observe({
    d <- data()
    codes <- sort(unique(c(project_codes_db(), uniq(d$project_code))))
    codes <- codes[!is.na(codes) & nzchar(codes)]
    deps <- sort(unique(c(DEPTS, uniq(d$department))))
    deps <- deps[!is.na(deps) & nzchar(deps)]
    months <- unique(c(format(Sys.Date(), "%Y-%m"), d$month,
                       format(suppressWarnings(as.Date(d$payment_date)), "%Y-%m"),
                       format(suppressWarnings(as.Date(d$order_date)), "%Y-%m")))
    months <- months[!is.na(months) & nzchar(months)]
    client_ready <- nzchar(isolate(v(input$insights_month)))
    if (!client_ready) {
      sel <- isolate(insights_query()$projects)
      dep_sel <- isolate(insights_query()$department %||% "")
      month_sel <- isolate(insights_query()$month)
    } else {
      sel <- isolate(input$insights_project)
      if (is.null(sel)) sel <- character()
      dep_sel <- isolate(v(input$insights_department))
      month_sel <- isolate(v(input$insights_month))
    }
    sel <- intersect(as.character(sel), codes)
    sig <- paste(paste(codes, collapse = "\n"), paste(sel, collapse = "\n"),
                 paste(deps, collapse = "\n"), dep_sel,
                 paste(sort(months), collapse = "\n"), month_sel, sep = "\f")
    if (identical(sig, isolate(insights_choice_sig()))) return()
    insights_choice_sig(sig)
    updateSelectizeInput(session, "insights_project", choices = codes, selected = sel, server = TRUE)
    updateSelectInput(session, "insights_department",
                      choices = c("All departments" = "", setNames(deps, deps)),
                      selected = dep_sel)
    updateSelectInput(session, "insights_month",
                      choices = month_choices(months),
                      selected = month_sel)
  })

  observe({
    mo <- v(input$insights_month)
    if (!nzchar(mo)) mo <- format(Sys.Date(), "%Y-%m")
    projects <- input$insights_project
    if (is.null(projects)) projects <- character()
    dept <- v(input$insights_department) %||% ""
    scen <- v(input$insights_scenario) %||% ""

    current <- isolate(insights_query())
    if (!identical(current$month, mo) ||
        !identical(sort(current$projects), sort(as.character(projects))) ||
        !identical(current$department, as.character(dept)) ||
        !identical(current$scenario, as.character(scen))) {
      insights_commit(mo, projects, dept, scen)
      insights_drill_code("")
      insights_days_all(FALSE)
    }
  })

  observeEvent(insights_query(), {
    insights_days_all(FALSE)
  }, ignoreInit = TRUE)

  output$insights_filter_badge <- renderUI({
    q <- insights_query()
    n <- (nzchar(q$department %||% "")) + (nzchar(q$scenario %||% ""))
    if (!n) return(NULL)
    span(class = "insights-filter-badge", n)
  })

  output$insights_advanced_label <- renderUI({
    n <- (nzchar(v(input$insights_department))) + (nzchar(v(input$insights_scenario)))
    span(sprintf("Advanced filters (%d)", n))
  })

  output$insights_project_chips <- renderUI({
    sel <- input$insights_project
    if (length(sel)) {
      tagList(lapply(sel, function(p) {
        tags$span(class = "insights-chip", p,
                  tags$button(type = "button", class = "insights-chip-remove",
                              onclick = sprintf("var s = $('#insights_project')[0].selectize; s.removeItem('%s');", p),
                              "\u2715"))
      }))
    } else {
      tags$span(class = "insights-chip", "All projects",
                tags$button(type = "button", class = "insights-chip-remove", "\u2715"))
    }
  })

  insights_status_filter <- reactiveVal("")

  observeEvent(input$insights_kpi_waiting, {
    st <- if (identical(insights_status_filter(), "under_review")) "" else "under_review"
    insights_status_filter(st)
    updateSelectInput(session, "breakdown_status", selected = if (nzchar(st)) st else "paid")
  })
  observeEvent(input$insights_kpi_validation, {
    st <- if (identical(insights_status_filter(), "pending")) "" else "pending"
    insights_status_filter(st)
    updateSelectInput(session, "breakdown_status", selected = if (nzchar(st)) st else "paid")
  })
  observeEvent(input$insights_kpi_paid, {
    st <- if (identical(insights_status_filter(), "paid")) "" else "paid"
    insights_status_filter(st)
    updateSelectInput(session, "breakdown_status", selected = if (nzchar(st)) st else "paid")
  })
  observeEvent(input$insights_kpi_rejected, {
    st <- if (identical(insights_status_filter(), "cancelled")) "" else "cancelled"
    insights_status_filter(st)
    updateSelectInput(session, "breakdown_status", selected = if (nzchar(st)) st else "paid")
  })

  output$insights_footer <- renderUI({
    q <- insights_query()
    dept <- if (nzchar(q$department %||% "")) q$department else "All departments"
    proj <- if (length(q$projects)) paste(q$projects, collapse = ", ") else "All projects"
    st <- insights_status_filter()
    kind <- if (nzchar(st)) {
      switch(st, under_review = "Under review", pending = "Pending", cancelled = "Cancelled", "Paid")
    } else if (identical(v(input$dash_chart_select), "breakdown")) {
      switch(v(input$breakdown_status),
             pending = "Pending",
             under_review = "Under review",
             cancelled = "Cancelled",
             "Paid")
    } else {
      "Paid"
    }
    div(class = "insights-footer",
        icon("code-branch", class = "insights-footer-icon"),
        span(paste(kind, month_label(q$month), dept, proj, sep = " \u00b7 ")))
  })

  output$totals <- renderUI({
    q <- insights_query()
    d <- data()
    d_scoped <- insights_scope(d, q)
    cur <- active_currency()
    selected_status <- insights_status_filter()

    amount_of <- function(part) {
      if (!nrow(part) || !cur %in% names(part)) return(0)
      sum(suppressWarnings(as.numeric(part[[cur]])), na.rm = TRUE)
    }

    in_month_paid <- insights_in_month(d_scoped[!is.na(d_scoped$status) & d_scoped$status == "paid", , drop = FALSE], "payment_date", q$month)
    in_month_order <- insights_in_month(d_scoped[!is.na(d_scoped$status) & d_scoped$status != "paid", , drop = FALSE], "order_date", q$month)

    part_review <- in_month_order[in_month_order$status == "under_review", , drop = FALSE]
    part_pending <- in_month_order[in_month_order$status == "pending", , drop = FALSE]
    part_paid <- in_month_paid
    part_cancelled <- in_month_order[in_month_order$status == "cancelled", , drop = FALSE]

    pending_overdue_count <- sum(calc_overdue_flags(part_pending))
    pending_badge <- if (pending_overdue_count > 0) {
      div(class = "status-overdue-pill", icon("circle-exclamation"), paste("Overdue:", pending_overdue_count))
    } else {
      div(class = "status-card-note", "In progress")
    }

    paid_week <- 0L
    if (nrow(part_paid) && "payment_date" %in% names(part_paid)) {
      weeks <- format(suppressWarnings(as.Date(part_paid$payment_date)), "%G-W%V")
      paid_week <- sum(!is.na(weeks) & weeks == format(Sys.Date(), "%G-W%V"))
    }

    make_card <- function(id, label, status, tone, icon_name, note, part, extra_bottom = NULL) {
      actionButton(id,
                   tagList(
                     div(class = "status-card-top",
                         span(class = "status-card-label", label),
                         span(class = "status-card-icon", icon(icon_name))),
                     div(class = "status-card-value", format(nrow(part), big.mark = ",")),
                     div(class = "status-card-amount", paste(cur, money(amount_of(part)))),
                     if (!is.null(extra_bottom)) extra_bottom else div(class = "status-card-note", note)
                   ),
                   class = paste("status-card", tone, if (identical(selected_status, status)) "is-active" else ""))
    }

    div(class = "request-status-grid",
        make_card("insights_kpi_waiting", "Under review", "under_review", "tone-review", "magnifying-glass", "Awaiting review", part_review),
        make_card("insights_kpi_validation", "Pending", "pending", "tone-pending", "clock", "In progress", part_pending, extra_bottom = pending_badge),
        make_card("insights_kpi_paid", "Paid", "paid", "tone-paid", "circle-check",
                  if (paid_week > 0) paste0("+", paid_week, " this week") else "Paid requests", part_paid),
        make_card("insights_kpi_rejected", "Cancelled", "cancelled", "tone-cancelled", "circle-xmark", "Closed requests", part_cancelled))
  })

  output$insights_spend_title <- renderUI({
    q <- insights_query()
    div(class = "insights-plot-title",
        paste0("Paid spend by day \u00b7 ", month_label(q$month)))
  })

  output$plot_trend <- renderPlot({
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
      pay <- suppressWarnings(as.Date(d$payment_date))
      day <- as.integer(format(pay, "%d"))
      bucket <- pmin(5L, ((day - 1L) %/% 7L) + 1L)
      amt <- if (cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else rep(0, nrow(d))
      ok <- !is.na(pay) & !is.na(bucket) & !is.na(amt)
      if (any(ok)) {
        summed <- tapply(amt[ok], bucket[ok], sum)
        totals[as.integer(names(summed))] <- as.numeric(summed)
      }
    }
    mode <- v(input$insights_spend_mode)
    shown <- if (identical(mode, "cumulative")) cumsum(totals) else totals
    df <- data.frame(week = factor(weeks, levels = weeks), amount = as.numeric(shown))
    ggplot(df, aes(week, amount)) +
      geom_col(fill = bar_col(), width = 0.62) +
      scale_y_continuous(labels = function(x) abbr(x), expand = expansion(mult = c(0, 0.06))) +
      chart_theme(fg) + vertical_grid(fg) +
      theme(plot.margin = margin(2, 8, 0, 0))
  }, bg = "transparent")

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
                span(class = "insights-info-icon", title = "Breakdown info", "\u24d8"))
        ),
        div(class = "insights-empty", "No data")
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
              span(class = "insights-info-icon", title = "Breakdown info", "\u24d8")),
          span(class = "insights-line-summary",
               paste(paste(active_currency(), abbr(total_amt)), count_label, sep = " \u00b7 "))
      ),
      div(class = "insights-lines", rows)
    )
  })

  output$insights_days <- renderUI({
    q <- insights_query()
    d <- insights_paid_month(q$month)
    order_d <- if (nrow(d)) suppressWarnings(as.Date(d$order_date)) else as.Date(character())
    pay_d <- if (nrow(d)) suppressWarnings(as.Date(d$payment_date)) else as.Date(character())
    ok <- !is.na(order_d) & !is.na(pay_d)
    
    if (!any(ok)) {
      return(tagList(
        div(class = "insights-line-head",
            div(class = "insights-plot-title",
                "Completion time by request",
                span(class = "insights-info-icon", title = "Completion time info", "\u24d8"))
        ),
        div(class = "insights-empty", "No data")
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
    ref <- as.character(part$project_code)
    missing_code <- is.na(ref) | !nzchar(ref)
    ref[missing_code] <- paste0("REQ-", part$id[missing_code])
    order_idx <- order(pay_d[ok], part$id, decreasing = TRUE)
    show_n <- if (isTRUE(insights_days_all()) || n <= 4) n else 4L
    shown <- order_idx[seq_len(show_n)]
    rows <- lapply(shown, function(i) {
      late <- is.na(expected[i]) || expected[i] <= 0 || actual[i] > expected[i]
      width <- if (is.na(expected[i]) || expected[i] <= 0) 75 else round(48 * actual[i] / expected[i])
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

    tagList(
      div(class = "insights-day-stats",
          div(class = "insights-day-stat",
              span("On time"),
              tags$b(sprintf("%d of %d (%.0f%%)", as.integer(n_on), as.integer(n), as.numeric(pct_on)))
          ),
          div(class = "insights-day-stat",
              span("Avg actual"),
              tags$b(sprintf("%.1f days", avg_actual))
          ),
          div(class = "insights-day-stat",
              span("Avg delay"),
              tags$b(if (is.finite(avg_delay)) sprintf("%+.1f days", avg_delay) else "\u2014")
          )
      ),
      div(class = "insights-line-head",
          div(class = "insights-plot-title",
              "Completion time by request",
              span(class = "insights-info-icon", title = "Completion time info", "\u24d8")),
          span(class = "insights-line-summary", sprintf("%d requests", as.integer(n)))
      ),
      div(class = "insights-day-center-label", span("On schedule")),
      div(class = "insights-day-rows", rows),
      if (n > 4 && !isTRUE(insights_days_all()))
        tags$a(href = "#", id = "insights_days_show_all", class = "insights-show-all",
               onclick = "Shiny.setInputValue('insights_days_all_btn', Math.random()); return false;",
               sprintf("Show all (%d) \u2192", as.integer(n)))
    )
  })

  observeEvent(input$insights_days_all_btn, {
    insights_days_all(TRUE)
  }, ignoreInit = TRUE)

  comparison_frame <- reactive({
    q <- insights_query()
    d <- data()
    d <- d[!is.na(d$status) & d$status == "paid", , drop = FALSE]
    d <- insights_scope(d, q)
    dates <- if (nrow(d)) suppressWarnings(as.Date(d$payment_date)) else as.Date(character())
    cur <- active_currency()
    amt <- if (nrow(d) && cur %in% names(d)) suppressWarnings(as.numeric(d[[cur]])) else numeric()
    ok <- !is.na(dates) & !is.na(amt)
    years <- sort(unique(format(dates[ok], "%Y")))
    years <- tail(years[!is.na(years) & nzchar(years)], 5)
    list(dates = dates[ok], amount = amt[ok], years = years, month = q$month)
  })

  output$insights_compare_title <- renderUI({
    by_month <- !identical(v(input$insights_compare_mode), "quarter")
    frame <- comparison_frame()
    years <- frame$years
    base <- if (by_month) "Paid spend by month" else "Paid spend by quarter"
    title_text <- if (length(years) >= 2) paste(base, paste(rev(tail(years, 2)), collapse = " vs "), sep = " \u00b7 ") else base
    span(class = "insights-plot-title",
         title_text,
         span(class = "insights-info-icon", title = "Comparison info", "\u24d8"))
  })

  output$insights_compare_delta <- renderUI({
    frame <- comparison_frame()
    years <- frame$years
    if (length(years) < 2) return(NULL)
    newest <- years[length(years)]
    prior <- years[length(years) - 1]
    cutoff <- suppressWarnings(as.integer(substr(frame$month, 6, 7)))
    if (is.na(cutoff)) cutoff <- 12L
    month_num <- as.integer(format(frame$dates, "%m"))
    year_num <- format(frame$dates, "%Y")
    current <- sum(frame$amount[year_num == newest & month_num <= cutoff], na.rm = TRUE)
    previous <- sum(frame$amount[year_num == prior & month_num <= cutoff], na.rm = TRUE)
    if (!is.finite(previous) || previous <= 0 || !is.finite(current)) return(NULL)
    pct <- (current - previous) / abs(previous) * 100
    up <- pct >= 0
    span(class = "insights-compare-delta",
         sprintf("%s %.0f%% vs %s (to date)", if (up) "\u25b2" else "\u25bc", abs(pct), prior))
  })

  output$plot_comparison <- renderPlot({
    frame <- comparison_frame()
    fg <- fg_col()
    by_month <- !identical(v(input$insights_compare_mode), "quarter")
    
    if (length(frame$dates) == 0 || sum(frame$amount, na.rm = TRUE) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data", colour = fg, size = 5.5, fontface = "bold") +
               theme_void() +
               theme(plot.background = element_rect(fill = "transparent", colour = NA)))
    }

    years <- tail(frame$years, 2)
    period_levels <- if (by_month) month.abb else paste0("Q", 1:4)
    rows <- lapply(years, function(yr) {
      in_year <- format(frame$dates, "%Y") == yr
      month_num <- as.integer(format(frame$dates[in_year], "%m"))
      if (by_month) {
        bucket <- factor(month_num, levels = 1:12)
        labels <- month.abb
      } else {
        bucket <- factor(((month_num - 1L) %/% 3L) + 1L, levels = 1:4)
        labels <- paste0("Q", 1:4)
      }
      summed <- as.numeric(tapply(frame$amount[in_year], bucket, sum))
      summed[is.na(summed)] <- 0
      data.frame(period = labels, year = yr, amount = summed, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, rows)
    df$period <- factor(df$period, levels = period_levels)
    df$year <- factor(df$year, levels = years)

    palette <- c("#0ea5e9", "#f1f5f9", "#64748b", "#94a3b8")
    newest_first <- rev(years)
    fills <- setNames(palette[seq_along(newest_first)], newest_first)

    ggplot(df, aes(period, amount, fill = year)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.65) +
      scale_fill_manual(values = fills, breaks = rev(newest_first)) +
      scale_y_continuous(labels = function(x) abbr(x), expand = expansion(mult = c(0, 0.08))) +
      chart_theme(fg) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "#94a3b8", size = 11, margin = margin(t = 4)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(colour = "#e2e8f0", size = 10, face = "bold"),
        legend.key.size = unit(0.7, "lines"),
        plot.margin = margin(4, 12, 0, 4)
      )
  }, bg = "transparent")

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
                scrollX = FALSE, autoWidth = FALSE, pageLength = 8,
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

