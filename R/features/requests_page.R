# =============================================================================
ui <- function(request) {
  page_navbar(
    id = "nav", selected = "requests", fillable = FALSE,
    title = tags$a(href = ".", class = "brand brand-lockup",
                   span(class = "brand-mark", "L"),
                   span(class = "brand-copy",
                        span(class = "brand-name", "Logapp"),
                        span(class = "brand-sub", "Expense tracker"))),
    window_title = "Logapp", theme = app_theme,
    header = tagList(tags$style(css), tags$script(HTML(js))),
    footer = tags$footer(class = "app-footer",
                         tags$img(src = "acted-logo.png", class = "footer-logo", alt = "ACTED"),
                         span(class = "footer-copy", paste0("\u00a9 ", format(app_today(), "%Y"), ". Log app. Expense tracker"))),
    
    # Requests: status cards span the page. Filters sit beside the register, as in the reference.
    nav_panel(
      title = "Requests", value = "requests",
      div(class = "requests-page",
          uiOutput("weekly_report_banner"),
          div(class = "page-heading",
              div(h1("Requests"), p("Track, review and approve all organizational expenses in one place.")),
              div(class = "d-flex align-items-center gap-2",
                  actionButton("toggle_requests_filter", tagList(icon("sliders"), "Filters"), class = "btn-outline-secondary requests-filter-toggle"),
                  actionButton("new_request", tagList(icon("plus"), "New Request"), class = "btn-primary")
              )
          ),
          uiOutput("request_status_cards"),
          div(class = "requests-layout",
              tags$main(class = "requests-main",
                        card(class = "request-table-card", fill = FALSE,
                             card_header(div(class = "table-head w-100",
                                             uiOutput("request_table_title"),
                                             div(class = "buttons ms-auto",
                                                 downloadButton("export", "Export", class = "btn-outline-secondary")
                                             )
                             )),
                             div(class = "card-body d-flex flex-column", DTOutput("table_requests", fill = FALSE))
                        )
              ),
              FilterPanel(
                class = "requests-sidebar filter-panel",
                clear_id = "reset",
                fields = list(
                  list(key = "f_query", type = "search", placeholder = "Search requests...", default = ""),
                  list(key = "status", type = "chips", label = "Status", default = "",
                       options = list(
                         list(id = "chip_status_all", label = "All", value = "", on = TRUE),
                         list(id = "chip_status_waiting", label = "Review", value = "under_review"),
                         list(id = "chip_status_validation", label = "Pending", value = "pending"),
                         list(id = "chip_status_paid", label = "Paid", value = "paid"),
                         list(id = "chip_status_rejected", label = "Cancelled", value = "cancelled")
                       )),
                  list(key = "f_project", label = "Project code", type = "searchable-select", multiple = TRUE,
                       placeholder = "All projects", default = ""),
                  list(key = "f_budget_line", label = "Budget line", type = "searchable-select", multiple = TRUE,
                       placeholder = "Select project code(s) first", default = "", depends = "f_project"),
                  list(key = "f_month", label = "Request period", type = "month",
                       choices = c("All months" = "", month_choices(c(last_month(), format(app_today(), "%Y-%m")))),
                       selected = last_month(), default = ""),
                  list(key = "f_week", label = "Week", type = "multi-select", advanced = TRUE,
                       placeholder = "All weeks", default = ""),
                  list(key = "f_person", label = "Requested by", type = "multi-select", advanced = TRUE,
                       placeholder = "All requesters", default = ""),
                  list(key = "f_submitter", label = "Submitted by", type = "multi-select", advanced = TRUE,
                       placeholder = "All focal points", default = "")
                )
              )
          )
      )
    ),
    
    finance_nav_panel(),
    
    # Management dashboards are separate from the request register and update workflow.
    nav_panel(
      title = "Insights", value = "dashboards",
      div(class = "insights-page",
          div(class = "insights-top-actions d-md-none mb-2 text-end",
              actionButton("toggle_dash_filter",
                           tagList(icon("sliders"), "Filters"),
                           class = "btn-outline-secondary btn-sm")
          ),
          uiOutput("totals"),
          div(class = "dashboard-layout insights-split",
          tags$main(class = "insights-main",
              card(class = "dashboard-breakdown-card insights-chart-card", fill = FALSE,
                   card_header(
                     div(class = "insights-chart-head",
                         div(class = "insights-chart-head-left d-flex align-items-center gap-2",
                             tags$span(class = "insights-view-label", "Chart view:"),
                             selectInput("dash_chart_select", NULL,
                                         choices = c(
                                           "Spending" = "spending",
                                           "Breakdown" = "breakdown",
                                           "Days to complete" = "days",
                                           "Comparison" = "comparison"
                                         ),
                                         selected = "comparison", width = "190px")
                         ),
                         div(class = "insights-chart-head-right",
                             conditionalPanel("input.dash_chart_select === 'spending'",
                                              div(class = "insights-seg",
                                                  radioButtons("insights_spend_mode", NULL,
                                                               choices = c("Daily" = "daily", "Cumulative" = "cumulative"),
                                                               selected = "daily", inline = TRUE))
                             ),
                             conditionalPanel("input.dash_chart_select === 'comparison'",
                                              div(class = "insights-seg",
                                                  radioButtons("insights_compare_mode", NULL,
                                                               choices = c("by quarter" = "quarter", "By month" = "month"),
                                                               selected = "month", inline = TRUE))
                             ),
                             conditionalPanel("input.dash_chart_select === 'breakdown'",
                                              div(class = "insights-breakdown-controls d-flex align-items-center gap-2",
                                                  tags$span(class = "insights-status-label", "Status"),
                                                  selectInput("breakdown_status", NULL,
                                                              choices = c(
                                                                "Paid" = "paid",
                                                                "Pending" = "pending",
                                                                "Under review" = "under_review",
                                                                "Cancelled" = "cancelled"
                                                              ),
                                                              selected = "paid")
                                              )
                             )
                         )
                     )
                   ),
                   div(class = "card-body",
                       div(class = "insights-chart-stage",
                           conditionalPanel("input.dash_chart_select === 'spending'",
                                            div(class = "insights-chart-panel",
                                                uiOutput("insights_spend_title"),
                                                conditionalPanel("input.insights_spend_mode !== 'cumulative'",
                                                                 plotOutput("plot_trend", height = "300px")),
                                                conditionalPanel("input.insights_spend_mode === 'cumulative'",
                                                                 uiOutput("insights_spend_cumulative")))
                           ),
                           conditionalPanel("input.dash_chart_select === 'breakdown'",
                                            div(class = "insights-chart-panel", uiOutput("insights_breakdown"))
                           ),
                           conditionalPanel("input.dash_chart_select === 'days'",
                                            div(class = "insights-chart-panel", uiOutput("insights_days"))
                           ),
                           conditionalPanel("input.dash_chart_select === 'comparison'",
                                            div(class = "insights-chart-panel",
                                                div(class = "insights-plot-head",
                                                    uiOutput("insights_compare_title"),
                                                    uiOutput("insights_compare_delta")),
                                                uiOutput("insights_compare_plot"))
                           )
                       ),
                       uiOutput("insights_footer")
                   )
              )
          ),
          FilterPanel(
            class = "dashboard-sidebar insights-sidebar filter-panel",
            clear_id = "insights_clear",
            badge = uiOutput("insights_filter_badge", inline = TRUE),
            close_id = "close_dash_filter",
            fields = list(
              list(key = "insights_month", label = "Month", type = "month",
                   choices = c("All months" = "", month_choices()),
                   selected = last_month(),
                   default = ""),
              list(key = "insights_project", label = "Project code", type = "searchable-select",
                   choices = c("All projects" = "all"), selected = "all", default = "",
                   placeholder = "All projects"),
              list(key = "insights_department", label = "Department", type = "select", advanced = TRUE,
                   choices = c("All departments" = ""), selected = "", default = "",
                   placeholder = "All departments"),
              list(key = "insights_scenario", label = "Scenario", type = "select", advanced = TRUE,
                   choices = c("All scenarios" = ""), selected = "", default = "",
                   placeholder = "All scenarios")
            )
          )
          )
      )
    ),
    # Manage keeps the project catalogue, suppliers, email preferences, people, and defaults together.
    nav_panel(
      title = "Manage", value = "manage",
      div(class = "manage-layout",
          div(class = "page-heading",
              div(h1("Manage"), p("Maintain project catalogues, suppliers, contacts, notification recipients, and request defaults."))
          ),
          navset_tab(
            nav_panel("Project & Supplier", value = "manage_catalog",
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("folder-open"), " Projects & budget lines")),
                               div(class = "card-body nr",
                                   div(class = "form-section-title", "Add a project code"),
                                   div(class = "fg",
                                       col(12, textInput("cfg_project_code", "Project code *", "", width = "100%", placeholder = "e.g. EDUAMH2026")),
                                       col(12, selectInput("cfg_project_department", "Department", c("Not specified" = "", DEPTS), selected = "", width = "100%")),
                                       col(12, p(class = "form-hint", "Letters-and-numbers examples are guidance only; existing naming formats are allowed.")),
                                       col(12, actionButton("cfg_add_project", tagList(icon("plus"), "Add project"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Add a budget line"),
                                   div(class = "fg",
                                       col(12, selectInput("cfg_budget_project", "Project code *", choices = c("Select project" = ""), selected = "", width = "100%")),
                                       col(12, textInput("cfg_budget_line", "Budget line *", "", width = "100%", placeholder = "e.g. TRAVEL01")),
                                       col(12, actionButton("cfg_add_budget_line", tagList(icon("plus"), "Add budget line"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Configured projects"),
                                   DTOutput("settings_catalog_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_project_selected", tagList(icon("edit"), "Edit selected project"), class = "btn-outline-secondary btn-sm"))
                               )
                          ),
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("truck"), " Suppliers")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Supplier names added here are available in the New request form. Historical expense supplier text is preserved."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_supplier_name", "Supplier name *", "", width = "100%", placeholder = "Supplier or vendor name")),
                                       col(12, actionButton("cfg_add_supplier", tagList(icon("plus"), "Add supplier"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Supplier register"),
                                   DTOutput("settings_supplier_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_supplier_selected", tagList(icon("edit"), "Edit selected supplier"), class = "btn-outline-secondary btn-sm"))
                               )
                          )
                      )
            ),
            nav_panel("Emails, People & other", value = "manage_people",
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("envelope"), " Email notifications")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Store recipients and the events they want to receive. Actual sending still needs an SMTP/mail provider and a scheduled job."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_notify_email", "Email address *", "", width = "100%", placeholder = "team@example.org")),
                                       col(12, checkboxInput("cfg_notify_weekly", "Weekly request summary", value = TRUE)),
                                       col(12, actionButton("cfg_add_notification_email", tagList(icon("plus"), "Save recipient"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "Recipients and subscriptions"),
                                   DTOutput("settings_email_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_email_selected", tagList(icon("edit"), "Edit selected recipient"), class = "btn-outline-secondary btn-sm"))
                               )
                          ),
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("users"), " People")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Maintain the staff/contact list separately from request records. Existing requester names remain available for historical data."),
                                   div(class = "fg",
                                       col(12, textInput("cfg_person_name", "Full name *", "", width = "100%", placeholder = "Staff member name")),
                                       col(12, selectInput("cfg_person_department", "Department", c("Not specified" = "", DEPTS), selected = "", width = "100%")),
                                       col(12, actionButton("cfg_add_person", tagList(icon("plus"), "Add person"), class = "btn-primary"))
                                   ),
                                   div(class = "form-section-rule"),
                                   div(class = "form-section-title", "People register"),
                                   DTOutput("settings_people_table"),
                                   div(class = "table-actions mt-2", actionButton("edit_person_selected", tagList(icon("edit"), "Edit selected person"), class = "btn-outline-secondary btn-sm"))
                               )
                          )
                      ),
                      div(class = "manage-defaults-row mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("cog"), " Defaults")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Defaults prefill future requests. Changing them does not rewrite existing records."),
                                   div(class = "fg",
                                       col(4, selectizeInput("cfg_default_submitter", "Logistics focal point", choices = DEFAULTS$submitted_by,
                                                             selected = DEFAULTS$submitted_by, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a name"))),
                                       col(4, selectizeInput("cfg_default_country", "Default country", choices = DEFAULTS$country,
                                                             selected = DEFAULTS$country, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a country"))),
                                       col(4, selectInput("cfg_default_currency", "Default currency", choices = CUR,
                                                           selected = DEFAULTS$currency, width = "100%")),
                                       col(12, div(class = "action-bar mt-2", actionButton("cfg_save_defaults", tagList(icon("save"), "Save defaults"), class = "btn-primary")))
                                   )
                               )
                          )
                      )
            )
          )
      )
    ),
    
    nav_spacer(),
    nav_item(
      div(class = "d-flex align-items-center global-currency-wrap",
          selectInput("global_cur", NULL,
                      choices = c("ETB (Br)" = "ETB", "USD ($)" = "USD", "EUR (\u20ac)" = "EUR", "GBP (\u00a3)" = "GBP"),
                      selected = DEFAULTS$currency, width = "8.6rem")
      )
    ),
    nav_item(input_dark_mode(id = "mode", mode = "light")),
    nav_item(actionButton("share_nav", NULL, icon = icon("share-square"),
                          class = "nav-icon-btn", title = "Share this view", `aria-label` = "Share this view")),
    nav_item(actionButton("settings_open", NULL, icon = icon("gear"),
                          class = "nav-icon-btn", title = "Manage", `aria-label` = "Manage")),
    nav_item(actionButton("logout_btn", NULL, icon = icon("arrow-right-from-bracket"),
                          class = "nav-icon-btn", title = "Logout", `aria-label` = "Logout"))
  )
}
# =============================================================================
# SERVER

