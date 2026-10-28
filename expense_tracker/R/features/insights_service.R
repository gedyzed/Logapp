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
    make_card <- function(id, label, status, tone, icon_name, note) {
      part <- if (nzchar(status)) d[d$status == status & !is.na(d$status), , drop = FALSE] else d
      actionButton(id,
                   tagList(
                     div(class = "status-card-top",
                         span(class = "status-card-label", label),
                         span(class = "status-card-icon", icon(icon_name))),
                     div(class = "status-card-value", format(nrow(part), big.mark = ",")),
                     div(class = "status-card-amount", paste(cur, money(amount_of(part)))),
                     div(class = "status-card-note", note)
                   ),
                   class = paste("status-card", tone, if (identical(selected, status)) "is-active" else ""))
    }
    div(class = "request-status-grid",
        make_card("card_status_waiting", "Under review", "under_review", "tone-review", "magnifying-glass", "Awaiting review"),
        make_card("card_status_validation", "Pending", "pending", "tone-pending", "clock", "In progress"),
        make_card("card_status_paid", "Paid", "paid", "tone-paid", "circle-check",
                  if (paid_week > 0) paste0("+", paid_week, " this week") else "Paid requests"),
        make_card("card_status_rejected", "Cancelled", "cancelled", "tone-cancelled", "circle-xmark", "Closed requests"))
  })
  
  # ---- dashboard KPIs ----
  output$totals <- renderUI({
    requests_period <- dashboard_filtered()
    paid_period <- dashboard_paid_filtered()
    paid_previous <- dashboard_previous_paid()
    currency <- active_currency()
    
    request_value <- if (nrow(requests_period)) sum(requests_period[[currency]], na.rm = TRUE) else 0
    paid_value <- if (nrow(paid_period)) sum(paid_period[[currency]], na.rm = TRUE) else 0
    previous_paid_value <- if (nrow(paid_previous)) sum(paid_previous[[currency]], na.rm = TRUE) else 0
    waiting_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "under_review"], na.rm = TRUE) else 0
    validation_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "pending"], na.rm = TRUE) else 0
    rejected_value <- if (nrow(requests_period)) sum(requests_period[[currency]][requests_period$status == "cancelled"], na.rm = TRUE) else 0
    
    if (previous_paid_value > 0) {
      delta <- paid_value - previous_paid_value
      pct <- (delta / previous_paid_value) * 100
      delta_class <- if (delta >= 0) "positive" else "negative"
      delta_sign <- if (delta >= 0) "+" else "−"
      paid_comparison <- div(class = paste("metric-comparison", delta_class),
                             paste0(delta_sign, currency, " ", abbr(abs(delta)), " · ", delta_sign,
                                    formatC(abs(pct), format = "f", digits = 1), "% vs ", dashboard_period_label(-1L)))
    } else if (paid_value > 0) {
      paid_comparison <- span(class = "metric-note", paste("No paid spend in", dashboard_period_label(-1L)))
    } else {
      paid_comparison <- span(class = "metric-note", paste("No paid spend in", dashboard_period_label(), "or the prior period"))
    }
    
    metric <- function(label, value, note, extra_class = "") {
      div(class = paste("metric-card", extra_class),
          p(class = "metric-label", label),
          p(class = "metric-value", paste(currency, abbr(value))),
          p(class = "metric-note", note)
      )
    }
    div(class = "metrics-grid dashboard-metrics-grid",
        div(class = "metric-card metric-paid",
            p(class = "metric-label", "Paid spend"),
            p(class = "metric-value", paste(currency, abbr(paid_value))),
            p(class = "metric-note", paste(nrow(paid_period), "paid requests by payment date")),
            # paid comparison removed
        ),
        metric("Under review", waiting_value,
               paste(sum(requests_period$status == "under_review", na.rm = TRUE), "requests awaiting initial review"), "metric-waiting"),
        metric("Pending", validation_value,
               paste(sum(requests_period$status == "pending", na.rm = TRUE), "requests awaiting procurement completion"), "metric-validation"),
        metric("Cancelled", rejected_value,
               paste(sum(requests_period$status == "cancelled", na.rm = TRUE), "cancelled requests"), "metric-rejected")
    )
  })
  
  # Status palette is consistent across charts, notifications and status changes.
  fg_col <- function() if (identical(input$mode, "dark")) "#e4e4e7" else "#27272a"
  status_label <- function(status) switch(status,
                                          under_review = "Under review",
                                          pending = "Pending",
                                          paid = "Paid",
                                          cancelled = "Cancelled",
                                          waiting = "Under review",
                                          `under validation` = "Pending",
                                          rejected = "Cancelled",
                                          status)
  
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
  
  # Main report selector: request-stage reports use request/order date; spend reports use payment date.
  output$plot_dashboard_main <- renderPlot({
    currency <- active_currency()
    report <- v(input$dash_report)
    fg <- fg_col()
    
    if (report %in% c("request_value_status", "status_count")) {
      d <- dashboard_filtered()
      validate(need(nrow(d) > 0, "No requests in the selected period and scope."))
      g <- d |>
        group_by(status) |>
        summarise(total = if (report == "status_count") n() else sum(.data[[currency]], na.rm = TRUE),
                  .groups = "drop")
      g <- data.frame(status = STATUS, stringsAsFactors = FALSE) |>
        left_join(g, by = "status")
      g$total[is.na(g$total)] <- 0
      g$status <- factor(g$status, levels = STATUS)
      g$label <- vapply(as.character(g$status), status_label, character(1))
      plot <- ggplot(g, aes(x = reorder(label, total), y = total, fill = status)) +
        geom_col(width = .66) +
        geom_text(aes(label = if (report == "status_count") as.character(round(total)) else abbr(total)),
                  hjust = -.15, colour = fg, size = 3.5) +
        coord_flip(clip = "off") +
        scale_fill_manual(values = STATUS_COL, breaks = STATUS,
                          labels = vapply(STATUS, status_label, character(1)), drop = FALSE) +
        scale_y_continuous(labels = if (report == "status_count") scales::label_number(big.mark = ",") else abbr,
                           expand = expansion(mult = c(0, .27))) +
        labs(x = NULL, y = NULL,
             subtitle = if (report == "status_count") {
               paste("Request count · request/order date ·", dashboard_period_label())
             } else {
               paste("Request value by status · request/order date ·", currency, "·", dashboard_period_label())
             }) + chart_theme(fg)
      return(plot)
    }
    
    # Department and project spending must be assigned to the period in which payment occurred.
    d <- dashboard_paid_filtered()
    validate(need(nrow(d) > 0, "No paid requests with a payment date in this period and scope."))
    
    if (identical(report, "department_spend")) {
      g <- d |>
        group_by(label = coalesce(as.character(department), "Not specified")) |>
        summarise(total = sum(.data[[currency]], na.rm = TRUE), .groups = "drop") |>
        filter(total > 0) |>
        arrange(desc(total))
      validate(need(nrow(g) > 0, paste("No department paid spend in", currency, "for this period.")))
      g$share <- g$total / sum(g$total)
      g$pie_label <- paste0(abbr(g$total), "\n", round(100 * g$share), "%")
      pie_cols <- c("#0284c7", "#0369a1", "#075985", "#0ea5e9", "#38bdf8", "#7dd3fc")
      pie_values <- setNames(rep(pie_cols, length.out = nrow(g)), g$label)
      for (department in names(pie_values)) {
        key <- toupper(trimws(department))
        if (key %in% names(DEPT_PIE_COL)) pie_values[[department]] <- unname(DEPT_PIE_COL[[key]])
      }
      return(ggplot(g, aes(x = "", y = total, fill = label)) +
               geom_col(width = 1, colour = if (identical(input$mode, "dark")) "#18181b" else "#ffffff", linewidth = .8) +
               coord_polar(theta = "y") +
               geom_text(aes(label = pie_label), position = position_stack(vjust = .5),
                         colour = "white", fontface = "bold", size = 3.5) +
               scale_fill_manual(values = pie_values, name = "Department") +
               labs(subtitle = paste("Paid spend by department · payment date ·", currency, "·", dashboard_period_label()), x = NULL, y = NULL) +
               chart_theme(fg) + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                                       panel.grid = element_blank(), axis.title = element_blank(), legend.position = "right"))
    }
    
    g <- d |>
      group_by(label = coalesce(as.character(project_code), "Not specified")) |>
      summarise(total = sum(.data[[currency]], na.rm = TRUE), requests = n(), .groups = "drop") |>
      filter(total > 0) |>
      arrange(desc(total)) |>
      head(12)
    validate(need(nrow(g) > 0, paste("No project paid spend in", currency, "for this period.")))
    g$label <- factor(g$label, levels = rev(g$label))
    ggplot(g, aes(x = label, y = total)) +
      geom_col(fill = "#0284c7", width = .64) +
      geom_text(aes(label = paste0(abbr(total), " · ", requests, " req.")), hjust = -.15, colour = fg, size = 3.2) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .32))) +
      labs(subtitle = paste("Top projects by paid spend · payment date ·", currency, "·", dashboard_period_label()),
           x = NULL, y = NULL) + chart_theme(fg)
  }, bg = "transparent", res = 96)
  
  # Paid spending trend is bucketed by the selected reporting period and payment_date.
  output$plot_trend <- renderPlot({
    d <- dashboard_paid_filtered()
    currency <- active_currency()
    period_type <- v(input$dash_period_type)
    bounds <- dashboard_period_bounds()
    fg <- fg_col()
    validate(need(nrow(d) > 0, "No paid spending with a payment date in this period."))
    
    paid_dates <- suppressWarnings(as.Date(d$payment_date))
    if (period_type == "month") {
      agg <- data.frame(bucket = paid_dates, amount = as.numeric(d[[currency]])) |>
        group_by(bucket) |>
        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
      buckets <- seq(bounds$start, bounds$end, by = "day")
      date_breaks <- "1 week"
      date_labels <- "%d %b"
    } else {
      month_starts <- as.Date(paste0(format(paid_dates, "%Y-%m"), "-01"))
      agg <- data.frame(bucket = month_starts, amount = as.numeric(d[[currency]])) |>
        group_by(bucket) |>
        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
      buckets <- seq(as.Date(format(bounds$start, "%Y-%m-01")),
                     as.Date(format(bounds$end, "%Y-%m-01")), by = "month")
      date_breaks <- if (period_type == "quarter") "1 month" else "2 months"
      date_labels <- "%b"
    }
    g <- data.frame(bucket = buckets) |>
      left_join(agg, by = "bucket") |>
      arrange(bucket)
    g$total[is.na(g$total)] <- 0
    
    ggplot(g, aes(x = bucket, y = total)) +
      geom_line(colour = "#0284c7", linewidth = 1.15, lineend = "round") +
      geom_point(colour = "#0284c7", size = 2.2) +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels,
                   limits = c(min(buckets), max(buckets))) +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .14))) +
      labs(subtitle = paste("Paid spend by payment date ·", dashboard_period_label(), "·", currency),
           x = NULL, y = NULL) + chart_theme(fg) +
      theme(axis.text.x = element_text(angle = if (period_type == "month") 0 else 0, hjust = .5))
  }, bg = "transparent", res = 96)
  
  # ---- comparison & days turnaround charts (strictly paid spend) ----
  # Scope for comparison charts respects currency, project, and department filters.
  # When Period is set to Month or Quarter, it filters to that month or quarter across years.
  compare_data_scope <- function() {
    d <- data()
    if (length(input$dash_project)) d <- d[d$project_code %in% input$dash_project, , drop = FALSE]
    if (length(input$dash_department)) d <- d[d$department %in% input$dash_department, , drop = FALSE]
    d <- d[d$status == "paid" & !is.na(d$payment_date) & nzchar(trimws(d$payment_date)), , drop = FALSE]
    d$plot_date <- suppressWarnings(as.Date(d$payment_date))
    d <- d[!is.na(d$plot_date), , drop = FALSE]
    d
  }

  share_dashboard_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    q <- list(
      v = "dash",
      cur = if (nzchar(active_currency())) active_currency() else NULL,
      period = if (nzchar(v(input$dash_period_type))) input$dash_period_type else NULL,
      yr = if (nzchar(v(input$dash_year))) input$dash_year else NULL,
      m = if (identical(v(input$dash_period_type), "month") && nzchar(v(input$dash_month))) input$dash_month else NULL,
      q = if (identical(v(input$dash_period_type), "quarter") && nzchar(v(input$dash_quarter))) input$dash_quarter else NULL,
      p = if (length(input$dash_project)) enc(input$dash_project) else NULL,
      d = if (length(input$dash_department)) enc(input$dash_department) else NULL,
      chart = if (nzchar(v(input$dash_chart_select))) input$dash_chart_select else NULL,
      bstat = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$breakdown_status))) input$breakdown_status else NULL,
      bdim = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$dim))) input$dim else NULL,
      bp = if (identical(v(input$dash_chart_select), "breakdown") && nzchar(v(input$breakdown_project))) input$breakdown_project else NULL
    )
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname,
                   if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }

  observeEvent(input$share_dashboard, {
    showModal(modalDialog(
      title = "Share this view",
      p(class = "chart-note", "Anyone with the link can open this view with the same filters. No request data is embedded in the URL."),
      div(class = "input-group",
          tags$input(id = "share_url", class = "form-control", readonly = NA,
                     value = share_dashboard_url(), onclick = "this.select()"),
          tags$button(id = "copy_link", type = "button", class = "btn btn-primary", "Copy link")),
      footer = modalButton("Close"), easyClose = TRUE))
  })

  output$plot_year_compare <- renderPlot({
    currency <- active_currency()
    fg <- fg_col()

    df <- compare_data_scope()
    validate(need(nrow(df) > 0, paste("No paid data available in", currency, "for the selected filters.")))

    df$year <- format(df$plot_date, "%Y")
    df$month <- factor(format(df$plot_date, "%b"), levels = month.abb)
    df$amount <- as.numeric(df[[currency]])

    years <- sort(unique(df$year))
    validate(need(length(years) > 0, "No records with valid payment dates found."))

    target_months <- month.abb

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    current_m <- as.integer(format(Sys.Date(), "%m"))

    full_grid <- expand.grid(month = factor(target_months, levels = month.abb),
                             year = factor(years, levels = years))
    full_grid$m_num <- as.integer(full_grid$month)
    full_grid$yr_num <- as.integer(as.character(full_grid$year))
    # Exclude months that are in the future relative to today
    full_grid <- full_grid[!(full_grid$yr_num > current_year |
                             (full_grid$yr_num == current_year & full_grid$m_num > current_m)), ]
    full_grid$m_num <- NULL
    full_grid$yr_num <- NULL

    agg <- df |>
      group_by(month, year) |>
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

    g <- merge(full_grid, agg, by = c("month", "year"), all.x = TRUE)
    g$total[is.na(g$total)] <- 0

    yr_totals <- g |>
      group_by(year) |>
      summarise(yr_sum = sum(total, na.rm = TRUE), .groups = "drop")
    yr_summary <- paste(paste0(yr_totals$year, ": ", currency, " ", abbr(yr_totals$yr_sum)), collapse = "   |   ")

    pal_base <- c("#0284c7", "#f59e0b", "#10b981", "#8b5cf6", "#ec4899", "#06b6d4")
    if (length(years) <= length(pal_base)) {
      pal <- setNames(pal_base[seq_along(years)], years)
    } else {
      pal <- setNames(scales::hue_pal()(length(years)), years)
    }

    ggplot(g, aes(x = month, y = total, fill = year)) +
      geom_col(position = position_dodge(width = 0.78), width = 0.7) +
      geom_text(aes(label = ifelse(total > 0, abbr(total), "")),
                position = position_dodge(width = 0.78), vjust = -0.4, size = 3, colour = fg) +
      scale_fill_manual(values = pal, name = "Year") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, 0.16))) +
      labs(subtitle = paste0("Paid spend by month · Jan–Dec · ", yr_summary),
           x = NULL, y = NULL) +
      chart_theme(fg) +
      theme(legend.position = "top", legend.justification = "right",
            axis.text.x = element_text(face = "bold"),
            panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
            panel.grid.major.x = element_blank())
  }, bg = "transparent", res = 96)

  output$plot_quarter_compare <- renderPlot({
    currency <- active_currency()
    fg <- fg_col()

    df <- compare_data_scope()
    validate(need(nrow(df) > 0, paste("No paid data available in", currency, "for the selected filters.")))

    df$year <- format(df$plot_date, "%Y")
    q_nums <- ceiling(as.integer(format(df$plot_date, "%m")) / 3)
    q_labels <- c("Q1 (Jan–Mar)", "Q2 (Apr–Jun)", "Q3 (Jul–Sep)", "Q4 (Oct–Dec)")
    df$quarter <- factor(q_labels[q_nums], levels = q_labels)
    df$amount <- as.numeric(df[[currency]])

    years <- sort(unique(df$year))
    validate(need(length(years) > 0, "No records with valid payment dates found."))

    target_quarters <- q_labels

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    current_q <- ceiling(as.integer(format(Sys.Date(), "%m")) / 3)

    full_grid <- expand.grid(quarter = factor(target_quarters, levels = q_labels),
                             year = factor(years, levels = years))
    full_grid$q_num <- as.integer(full_grid$quarter)
    full_grid$yr_num <- as.integer(as.character(full_grid$year))
    # Exclude quarters that are in the future relative to today
    full_grid <- full_grid[!(full_grid$yr_num > current_year |
                             (full_grid$yr_num == current_year & full_grid$q_num > current_q)), ]
    full_grid$q_num <- NULL
    full_grid$yr_num <- NULL

    agg <- df |>
      group_by(quarter, year) |>
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")

    g <- merge(full_grid, agg, by = c("quarter", "year"), all.x = TRUE)
    g$total[is.na(g$total)] <- 0

    yr_totals <- g |>
      group_by(year) |>
      summarise(yr_sum = sum(total, na.rm = TRUE), .groups = "drop")
    yr_summary <- paste(paste0(yr_totals$year, ": ", currency, " ", abbr(yr_totals$yr_sum)), collapse = "   |   ")

    pal_base <- c("#0284c7", "#f59e0b", "#10b981", "#8b5cf6", "#ec4899", "#06b6d4")
    if (length(years) <= length(pal_base)) {
      pal <- setNames(pal_base[seq_along(years)], years)
    } else {
      pal <- setNames(scales::hue_pal()(length(years)), years)
    }

    ggplot(g, aes(x = quarter, y = total, fill = year)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_text(aes(label = ifelse(total > 0, abbr(total), "")),
                position = position_dodge(width = 0.75), vjust = -0.4, size = 3.3, colour = fg) +
      scale_fill_manual(values = pal, name = "Year") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, 0.16))) +
      labs(subtitle = paste0("Paid spend by quarter · payment date · ", yr_summary),
           x = NULL, y = NULL) +
      chart_theme(fg) +
      theme(legend.position = "top", legend.justification = "right",
            axis.text.x = element_text(face = "bold"),
            panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
            panel.grid.major.x = element_blank())
  }, bg = "transparent", res = 96)

  output$plot_days <- renderPlot({
    fg <- fg_col()
    # Fully filtered using the active reporting filters (Period, Project, Department)
    d <- dashboard_paid_filtered()
    validate(need(nrow(d) > 0, paste("No paid requests found for", dashboard_period_label(), "with the selected filters.")))

    d <- d[!is.na(d$payment_date) & !is.na(d$order_date) &
             nzchar(trimws(d$payment_date)) & nzchar(trimws(d$order_date)), , drop = FALSE]
    validate(need(nrow(d) > 0, "No paid requests with valid order and payment dates found in this reporting period."))

    d$order_dt <- suppressWarnings(as.Date(d$order_date))
    d$pay_dt <- suppressWarnings(as.Date(d$payment_date))
    d <- d[!is.na(d$order_dt) & !is.na(d$pay_dt), , drop = FALSE]
    validate(need(nrow(d) > 0, "No records with parseable order and payment dates found in this reporting period."))

    d$actual_days <- pmax(0, as.numeric(d$pay_dt - d$order_dt))
    d$usd_amt <- suppressWarnings(as.numeric(d$usd))
    d$scen <- mapply(scenario, ifelse(is.na(d$usd_amt), 0, d$usd_amt), d$waiver)

    scen_lead_days <- c(A = 7, B = 18, C = 21, D = 45, E = 60, G = 60)
    d$expected_days <- unname(scen_lead_days[d$scen])
    d$expected_days[is.na(d$expected_days)] <- 21

    d <- d[order(d$order_dt, d$id), , drop = FALSE]
    d$idx <- seq_len(nrow(d))
    d$is_ontime <- d$actual_days <= d$expected_days
    d$perf_status <- ifelse(d$is_ontime, "On-time (≤ Expected)", "Delayed (> Expected)")

    ontime_count <- sum(d$is_ontime)
    total_count <- nrow(d)
    ontime_pct <- round((ontime_count / total_count) * 100, 1)
    avg_actual <- round(mean(d$actual_days), 1)
    avg_expected <- round(mean(d$expected_days), 1)

    sub_txt <- paste0(
      dashboard_period_label(), " · ",
      ontime_count, "/", total_count, " on-time (", ontime_pct, "%) · ",
      "Avg actual: ", avg_actual, " days · Avg standard: ", avg_expected, " days"
    )

    ggplot(d) +
      geom_segment(aes(x = idx, xend = idx, y = expected_days, yend = actual_days),
                   linetype = "dotted", colour = scales::alpha(fg, 0.45), linewidth = 0.6) +
      geom_point(aes(x = idx, y = expected_days, shape = "Expected standard"),
                 colour = "#0284c7", size = 3) +
      geom_point(aes(x = idx, y = actual_days, colour = perf_status),
                 size = 3) +
      scale_colour_manual(
        name = "Actual turnaround",
        values = c("On-time (≤ Expected)" = "#10b981", "Delayed (> Expected)" = "#ef4444")
      ) +
      scale_shape_manual(
        name = "Scenario standard",
        values = c("Expected standard" = 18)
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = min(12, total_count))) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      labs(
        subtitle = sub_txt,
        x = "Paid requests in period (chronological by order date)",
        y = "Days"
      ) +
      chart_theme(fg) +
      theme(
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "right",
        axis.title.x = element_text(size = 9, colour = scales::alpha(fg, 0.8), margin = margin(t = 8)),
        axis.title.y = element_text(size = 9, colour = scales::alpha(fg, 0.8), margin = margin(r = 8)),
        panel.grid.major.y = element_line(colour = scales::alpha(fg, 0.12)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }, bg = "transparent", res = 96)
  
  # Paid breakdown follows payment date; all non-paid workflow stages follow request/order date.
  breakdown_filtered <- reactive({
    status_value <- v(input$breakdown_status)
    if (identical(status_value, "paid")) dashboard_paid_filtered() else dashboard_filtered()
  })
  
  # Aggregate one row per label for non-project views.
  totals_by <- function(key, status_value = input$breakdown_status %||% "paid") {
    if (is.null(status_value) || !status_value %in% STATUS) status_value <- "paid"
    breakdown_filtered() |>
      filter(.data$status == status_value) |>
      group_by(label = coalesce(as.character(.data[[key]]), "(none)")) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }
  
  # When Group by = Project, show only non-zero project totals in the overview.
  # After a project is selected, switch to its non-zero budget-line amounts only;
  # do not repeat the project total in the budget-line view.
  project_breakdown <- function(status_value = input$breakdown_status %||% "paid") {
    scope <- breakdown_filtered()
    if (is.null(status_value) || !status_value %in% STATUS) status_value <- "paid"
    selected_currency <- active_currency()
    
    breakdown_project <- v(input$breakdown_project)
    if (nzchar(breakdown_project)) {
      projects <- breakdown_project
    } else if (length(input$dash_project)) {
      projects <- unique(as.character(input$dash_project))
    } else {
      projects <- project_codes_db()
    }
    projects <- projects[!is.na(projects) & nzchar(projects)]
    if (!length(projects)) return(data.frame())
    
    selected <- scope |>
      filter(.data$status == status_value, .data$project_code %in% projects)
    
    totals <- selected |>
      group_by(project_code) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    totals <- data.frame(project_code = projects, stringsAsFactors = FALSE) |>
      left_join(totals, by = "project_code")
    if (!"requests" %in% names(totals)) totals$requests <- 0L
    totals$requests[is.na(totals$requests)] <- 0L
    for (cc in CUR) {
      if (!cc %in% names(totals)) totals[[cc]] <- 0
      totals[[cc]][is.na(totals[[cc]])] <- 0
    }
    total_rows <- totals |>
      mutate(row_type = "Project total", budget_line = "",
             plot_label = project_code, row_order = 0L)
    
    # Without a selected project, show only projects with a non-zero value in the
    # selected currency/status/filter scope (matching the compact project overview).
    if (!nzchar(breakdown_project)) {
      return(total_rows |>
               filter(abs(.data[[selected_currency]]) > 0) |>
               arrange(desc(.data[[selected_currency]]), project_code) |>
               mutate(plot_label = factor(plot_label, levels = rev(unique(plot_label)))))
    }
    
    # When a project is selected, show only its configured budget lines with a
    # non-zero value. Never add a project-total row to this budget-line view.
    catalogue <- budget_catalog_db(projects)
    selected_budget_ids <- suppressWarnings(as.integer(input$f_budget_line %||% character()))
    selected_budget_ids <- selected_budget_ids[!is.na(selected_budget_ids)]
    if (length(selected_budget_ids)) {
      catalogue <- catalogue[catalogue$id %in% selected_budget_ids, , drop = FALSE]
    }
    
    line_spend <- selected |>
      mutate(budget_line = coalesce(as.character(budget_line), "")) |>
      group_by(project_code, budget_line) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    lines <- catalogue |>
      transmute(project_code = as.character(project_code),
                budget_line = as.character(budget_line_code)) |>
      distinct() |>
      left_join(line_spend, by = c("project_code", "budget_line"))
    if (!"requests" %in% names(lines)) lines$requests <- 0L
    lines$requests[is.na(lines$requests)] <- 0L
    for (cc in CUR) {
      if (!cc %in% names(lines)) lines[[cc]] <- 0
      lines[[cc]][is.na(lines[[cc]])] <- 0
    }
    
    lines |>
      filter(abs(.data[[selected_currency]]) > 0) |>
      mutate(row_type = "Budget line",
             plot_label = paste0(project_code, " · ", budget_line),
             row_order = 1L) |>
      arrange(desc(.data[[selected_currency]]), budget_line) |>
      mutate(plot_label = factor(plot_label, levels = rev(unique(plot_label))))
  }
  
  output$plot_dim <- renderPlot({
    status_value <- input$breakdown_status %||% "paid"
    group_by_value <- input$dim %||% "project_code"
    currency <- active_currency()
    fg <- fg_col()
    
    # Project overview shows only non-zero project totals. Selecting a project
    # switches the chart to that project's non-zero budget lines, without a total row.
    if (identical(group_by_value, "project_code")) {
      g <- project_breakdown(status_value)
      empty_msg <- if (nzchar(v(input$breakdown_project))) {
        paste("No non-zero budget lines for", v(input$breakdown_project),
              "with", tolower(status_label(status_value)), "in", currency)
      } else {
        paste("No non-zero project amounts for", tolower(status_label(status_value)),
              "in", currency)
      }
      validate(need(nrow(g) > 0, empty_msg))
      g$total <- as.numeric(g[[currency]])
      g$total[is.na(g$total)] <- 0
      g$plot_label <- factor(as.character(g$plot_label), levels = levels(g$plot_label))
      # Use the same status color for project totals and budget-line bars. The
      # department pie chart is the intentional exception and uses department colors.
      status_color <- unname(STATUS_COL[[status_value]])
      return(
        ggplot(g, aes(x = plot_label, y = total)) +
          geom_col(fill = status_color, width = .68) +
          geom_text(aes(label = abbr(total)), hjust = -.15, colour = fg, size = 3.2) +
          coord_flip(clip = "off") +
          scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .3))) +
          labs(subtitle = if (nzchar(v(input$breakdown_project))) {
            paste("Non-zero budget lines for", v(input$breakdown_project),
                  "—", tolower(status_label(status_value)), "in", currency)
          } else {
            paste("Projects with non-zero", tolower(status_label(status_value)),
                  "amounts in", currency, "· select a project to see its budget lines")
          }, x = NULL, y = NULL) +
          chart_theme(fg) +
          theme(legend.position = "bottom", axis.text.y = element_text(size = 8.5),
                plot.margin = margin(8, 28, 8, 8))
      )
    }
    
    g <- totals_by(group_by_value, status_value)
    validate(need(nrow(g) > 0, paste("No", status_label(status_value), "requests in this view")))
    g$total <- as.numeric(g[[currency]])
    g$total[is.na(g$total)] <- 0
    
    # Department remains a pie chart with stable department colors.
    if (identical(group_by_value, "department")) {
      g <- g |> filter(total > 0) |> arrange(desc(total))
      validate(need(nrow(g) > 0, paste("No positive", tolower(status_label(status_value)), "spend by department in", currency)))
      g$share <- g$total / sum(g$total)
      g$label <- ifelse(is.na(g$label) | !nzchar(g$label), "(none)", as.character(g$label))
      g$pie_label <- paste0(abbr(g$total), "\n", round(g$share * 100), "%")
      pie_cols <- c("#0284c7", "#0369a1", "#075985", "#0ea5e9", "#38bdf8", "#7dd3fc")
      pie_values <- setNames(rep(pie_cols, length.out = nrow(g)), g$label)
      for (department in names(pie_values)) {
        key <- toupper(trimws(department))
        if (key %in% names(DEPT_PIE_COL)) pie_values[[department]] <- unname(DEPT_PIE_COL[[key]])
      }
      return(
        ggplot(g, aes(x = "", y = total, fill = label)) +
          geom_col(width = 1, colour = if (identical(input$mode, "dark")) "#18181b" else "#ffffff",
                   linewidth = .8) +
          coord_polar(theta = "y") +
          geom_text(aes(label = pie_label), position = position_stack(vjust = .5),
                    colour = "white", fontface = "bold", size = 3.6, lineheight = .95) +
          scale_fill_manual(values = pie_values, name = "Department") +
          labs(subtitle = paste("Department share of", tolower(status_label(status_value)),
                                "spend in", currency), x = NULL, y = NULL) +
          chart_theme(fg) +
          theme(axis.text = element_blank(), axis.ticks = element_blank(),
                panel.grid = element_blank(), axis.title = element_blank(),
                legend.position = "right", plot.margin = margin(8, 12, 8, 8))
      )
    }
    
    g <- head(arrange(g, desc(total)), 10)
    g$label <- factor(g$label, levels = rev(g$label))
    ggplot(g, aes(x = label, y = total)) +
      geom_col(fill = STATUS_COL[[status_value]], width = .62) +
      geom_text(aes(label = abbr(total)), hjust = -.15, colour = fg, size = 3.4) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .3))) +
      labs(subtitle = paste("Top 10", tolower(status_label(status_value)), "requests in", currency),
           x = NULL, y = NULL) + chart_theme(fg)
  }, bg = "transparent", res = 96,
  height = function() {
    if (identical(isolate(input$dim) %||% "project_code", "project_code")) {
      rows <- tryCatch(nrow(project_breakdown(isolate(input$breakdown_status) %||% "paid")), error = function(e) 0L)
      max(380, rows * 28 + 130)
    } else 380
  })
  
  output$by_dim <- renderDT({
    status_value <- input$breakdown_status %||% "paid"
    group_by_value <- input$dim %||% "project_code"
    if (identical(group_by_value, "project_code")) {
      g <- project_breakdown(status_value)
      if (!nrow(g)) return(datatable(data.frame(), options = list(dom = "t", language = dt_language)))
      display <- g |>
        mutate(`Row type` = row_type) |>
        select(`Row type`, project_code, budget_line, requests, all_of(CUR))
      names(display) <- c("Type", "Project", "Budget line", "Requests", CUR)
      datatable(display, rownames = FALSE, selection = "none", fillContainer = FALSE,
                options = list(dom = "t", language = dt_language, scrollX = TRUE)) |>
        formatRound(CUR, 2)
    } else {
      lab <- switch(group_by_value,
                    requested_by = "Requester", department = "Department", "Group")
      datatable(totals_by(group_by_value, status_value), rownames = FALSE, selection = "none",
                fillContainer = FALSE, colnames = c(lab, "Requests", CUR),
                options = list(dom = "t", language = dt_language, scrollX = TRUE)) |>
        formatRound(CUR, 2)
    }
  })
  
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
    display <- data.frame(
      id = d$id,
      requested_by = blank(d$requested_by),
      reference = reference,
      description = blank(d$description),
      project_code = blank(d$project_code),
      budget_line = blank(d$budget_line),
      amount = ifelse(is.na(d$amount), "", paste0(d$currency, " ", vapply(d$amount, money, character(1)))),
      status = blank(d$status),
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
  var labels = {under_review: 'Under review', pending: 'Pending', paid: 'Paid', cancelled: 'Cancelled'};
  var key = String(data || '');
  return '<span class="req-pill req-pill-' + key + '">' + (labels[key] || key) + '</span>';
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

