<<<<<<< HEAD
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
                         span("Logapp · Expense tracker")),
    
    # Requests: status cards span the page. Filters sit beside the register, as in the reference.
    nav_panel(
      title = "Requests", value = "requests",
      div(class = "requests-page",
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
                             div(class = "card-body", DTOutput("table_requests", fill = FALSE))
                        )
              ),
              tags$aside(class = "requests-sidebar",
                         div(class = "requests-sidebar-head d-flex align-items-center justify-content-between",
                             div(class = "d-flex align-items-center gap-2", icon("sliders"), span("Filters")),
                             actionButton("reset", "Clear all", class = "btn btn-link sidebar-clear")
                         ),
                         div(class = "requests-sidebar-body",
                             div(class = "filter-search",
                                 icon("magnifying-glass"),
                                 textInput("f_query", NULL, placeholder = "Search requests...", width = "100%")),
                             tags$label(class = "control-label", "Status"),
                             div(class = "status-chips", role = "group", "aria-label" = "Status",
                                 actionButton("chip_status_all", "All", class = "status-chip is-on", `data-status` = ""),
                                 actionButton("chip_status_waiting", "Review", class = "status-chip", `data-status` = "under_review"),
                                 actionButton("chip_status_validation", "Pending", class = "status-chip", `data-status` = "pending"),
                                 actionButton("chip_status_paid", "Paid", class = "status-chip", `data-status` = "paid"),
                                 actionButton("chip_status_rejected", "Cancelled", class = "status-chip", `data-status` = "cancelled")),
                             selectizeInput("f_project", "Project code", NULL, multiple = TRUE,
                                            options = list(placeholder = "All projects")),
                             selectizeInput("f_budget_line", "Budget line", NULL, multiple = TRUE,
                                            options = list(placeholder = "All lines")),
                             selectInput("f_month", "Request period",
                                         choices = c("All months" = "", month_choices(format(Sys.Date(), "%Y-%m"))),
                                         selected = format(Sys.Date(), "%Y-%m"), width = "100%"),
                             tags$details(class = "advanced-filters",
                                          tags$summary(tagList(icon("sliders"), " Advanced filters")),
                                          selectizeInput("f_week", "Week", NULL, multiple = TRUE,
                                                         options = list(placeholder = "All weeks")),
                                          selectizeInput("f_person", "Requested by", NULL, multiple = TRUE,
                                                         options = list(placeholder = "All requesters")),
                                          selectizeInput("f_submitter", "Submitted by", NULL, multiple = TRUE,
                                                         options = list(placeholder = "All focal points")))
                         )
              )
          )
      )
    ),
    
    finance_nav_panel(),
    
    # Management dashboards are separate from the request register and update workflow.
    nav_panel(
      title = "Insights", value = "dashboards",
      div(class = "dashboard-layout",
          div(class = "dashboard-main",
              div(class = "page-heading dashboard-heading",
                  div(h1("Insights"), p("A clear view of paid expenditure, request value, and procurement activity.")),
                  div(class = "d-flex flex-column align-items-end gap-1",
                      actionButton("toggle_dash_filter", tagList(icon("sliders"), "Filters"), class = "btn-outline-secondary btn-sm filter-toggle-btn is-active"),
                      uiOutput("dashboard_meta")
                  )
              ),
              uiOutput("totals"),
              card(class = "dashboard-breakdown-card mt-3", fill = FALSE,
                   card_header(div(class = "dashboard-chart-title dashboard-breakdown-title d-flex align-items-center justify-content-between flex-wrap gap-2",
                                   div(class = "d-flex align-items-center gap-2",
                                       span(class = "fw-semibold", "Chart view:"),
                                       selectInput("dash_chart_select", NULL,
                                                   choices = c(
                                                     "Spending" = "spending",
                                                     "Breakdown" = "breakdown",
                                                     "Days to complete" = "days",
                                                     "Year comparison" = "year_compare",
                                                     "Quarter comparison" = "quarter_compare"
                                                   ),
                                                   selected = "spending", width = "220px")
                                   )
                   )),
                   div(class = "card-body",
                       conditionalPanel("input.dash_chart_select === 'spending'",
                                        p(class = "chart-note mb-3",
                                          "Spending: Tracks total paid expenditure over time by payment date. Displays daily spending across a selected month, and monthly spending across a quarter or year in the chosen currency."),
                                        plotOutput("plot_trend", height = "380px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'breakdown'",
                                        p(class = "chart-note mb-3",
                                          "Breakdown: Visualizes the distribution of requests across project codes, budget lines, requesters, or departments across procurement workflow stages."),
                                        plotOutput("plot_dim", height = "auto")
                       ),
                       conditionalPanel("input.dash_chart_select === 'days'",
                                        p(class = "chart-note mb-3",
                                          "Days to complete: Compares actual processing days taken against expected lead time based on ACTED procurement scenario standards for the selected reporting period and filters."),
                                        plotOutput("plot_days", height = "410px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'year_compare'",
                                        p(class = "chart-note mb-3",
                                          "Year comparison: Compares monthly spending (Jan–Dec) year-over-year side-by-side in the chosen currency."),
                                        plotOutput("plot_year_compare", height = "390px")
                       ),
                       conditionalPanel("input.dash_chart_select === 'quarter_compare'",
                                        p(class = "chart-note mb-3",
                                          "Quarter comparison: Compares paid spending across quarters (Q1–Q4) side-by-side across years in the chosen currency."),
                                        plotOutput("plot_quarter_compare", height = "390px")
                       )
                   )
              )
          ),
          div(class = "dashboard-sidebar",
              card(class = "dashboard-filter-card", fill = FALSE,
                   card_header(div(class = "dashboard-filter-heading d-flex align-items-center justify-content-between w-100",
                                   div(class = "d-flex align-items-center gap-2",
                                       icon("sliders"), span("Reporting filters")
                                   ),
                                   tags$button(type = "button", id = "close_dash_filter", class = "sidebar-close-btn",
                                               title = "Collapse filters", icon("xmark"))
                   )),
                   div(class = "card-body",
                       p(class = "chart-note mb-3", "Independent from Requests filters; request value uses order date, paid spend uses payment date."),
                       
                       # Reporting period filters for single-period charts (Spending, Breakdown, Days)
                       conditionalPanel("['spending', 'breakdown', 'days'].indexOf(input.dash_chart_select) !== -1",
                                        div(class = "dashboard-filter-section mb-2",
                                            div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Reporting Period"),
                                            selectInput("dash_period_type", "Period", choices = c(
                                              "Month" = "month", "Quarter" = "quarter", "Year" = "year"
                                            ), selected = "month", width = "100%"),
                                            selectInput("dash_year", "Year", choices = setNames(format(Sys.Date(), "%Y"), format(Sys.Date(), "%Y")),
                                                        selected = format(Sys.Date(), "%Y"), width = "100%"),
                                            conditionalPanel("input.dash_period_type === 'month'",
                                                             selectInput("dash_month", "Month", choices = setNames(sprintf("%02d", 1:12), month.name),
                                                                         selected = format(Sys.Date(), "%m"), width = "100%")
                                            ),
                                            conditionalPanel("input.dash_period_type === 'quarter'",
                                                             selectInput("dash_quarter", "Quarter", choices = c(
                                                               "Q1 · Jan–Mar" = "Q1", "Q2 · Apr–Jun" = "Q2",
                                                               "Q3 · Jul–Sep" = "Q3", "Q4 · Oct–Dec" = "Q4"
                                                             ), selected = paste0("Q", ceiling(as.integer(format(Sys.Date(), "%m")) / 3)), width = "100%")
                                            )
                                        )
                       ),
                       
                       # Comparison charts notice
                       conditionalPanel("['year_compare', 'quarter_compare'].indexOf(input.dash_chart_select) !== -1",
                                        div(class = "alert alert-info py-2 px-3 small mb-3",
                                            icon("circle-info"), " ",
                                            span("Compares paid spend across all years side-by-side.")
                                        )
                       ),
                       
                       # Breakdown-specific filters: moved together to filter section
                       conditionalPanel("input.dash_chart_select === 'breakdown'",
                                        div(class = "dashboard-filter-section pt-2 border-top mb-2",
                                            div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Breakdown Settings"),
                                            selectInput("breakdown_status", "Status", choices = c(
                                              "Paid" = "paid", "Under review" = "under_review", "Pending" = "pending", "Cancelled" = "cancelled"
                                            ), selected = "paid", width = "100%"),
                                            selectInput("dim", "Group by", DIMS, selected = "project_code", width = "100%"),
                                            conditionalPanel("input.dim === 'project_code'",
                                                             div(class = "mb-2",
                                                                 tags$label(class = "control-label", "Project drill-down"),
                                                                 div(class = "d-flex align-items-center gap-1",
                                                                     div(class = "flex-grow-1",
                                                                         selectInput("breakdown_project", NULL, choices = c("Select a project" = ""),
                                                                                     selected = "", width = "100%")
                                                                     ),
                                                                     actionButton("clear_breakdown_project", "Clear", class = "btn btn-outline-secondary btn-sm mb-3", title = "Clear project drill-down")
                                                                 ),
                                                                 p(class = "form-hint small text-muted mt-n2 mb-2", "Select a project to inspect its budget lines.")
                                                             )
                                            )
                                        )
                       ),
                       
                       # Scope filters (Department & Project) apply across charts
                       div(class = "dashboard-filter-section pt-2 border-top",
                           div(class = "text-muted small fw-bold text-uppercase mb-2 tracking-wide", "Scope Filters"),
                           selectizeInput("dash_department", "Department", NULL, multiple = TRUE,
                                          options = list(placeholder = "All departments")),
                           selectizeInput("dash_project", "Project code", NULL, multiple = TRUE,
                                          options = list(placeholder = "All projects"))
                       ),
                       
                       # Reset button for reporting filters
                       div(class = "pt-2 mt-2 border-top",
                           actionButton("reset_dash_filter", tagList(icon("rotate-left"), "Reset filters"),
                                        class = "btn btn-outline-secondary btn-sm w-100")
                       )
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
                      div(class = "manage-two-col mt-3",
                          card(class = "manage-card", fill = FALSE,
                               card_header(tagList(icon("cog"), " Defaults")),
                               div(class = "card-body nr",
                                   p(class = "manage-note", "Defaults prefill future requests. Changing them does not rewrite existing records."),
                                   div(class = "fg",
                                       col(12, selectizeInput("cfg_default_submitter", "Logistics focal point", choices = DEFAULTS$submitted_by,
                                                              selected = DEFAULTS$submitted_by, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a name"))),
                                       col(12, selectizeInput("cfg_default_country", "Default country", choices = DEFAULTS$country,
                                                              selected = DEFAULTS$country, width = "100%", options = list(create = TRUE, placeholder = "Type or choose a country"))),
                                       col(12, selectInput("cfg_default_currency", "Default currency", choices = CUR,
                                                           selected = DEFAULTS$currency, width = "100%")),
                                       col(12, div(class = "action-bar", actionButton("cfg_save_defaults", tagList(icon("save"), "Save defaults"), class = "btn-primary")))
                                   )
                               )
                          )
                      )
            )
          )
      )
    ),
    
    # Full-page request form; kept as a programmatically opened page.
    nav_panel(
      title = "New request", value = "new",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("New request"), p("Enter a procurement request. Fields without an asterisk are optional.")),
              actionButton("cancel_new", tagList(icon("arrow-left"), "Back to requests"), class = "btn-outline-secondary")
          ),
          layout_columns(fill = FALSE, col_widths = c(8, 4),
                         card(fill = FALSE,
                              div(class = "card-body",
                                  uiOutput("new_form_ui"),
                                  div(class = "action-bar",
                                      actionButton("clear", "Clear form", class = "btn-outline-secondary"),
                                      actionButton("cancel_new_bottom", "Cancel", class = "btn-outline-secondary"),
                                      actionButton("add", "Add request", class = "btn-primary")
                                  )
                              )
                         ),
                         card(card_header("Request preview"), div(class = "card-body", uiOutput("preview")))
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
    nav_item(input_dark_mode(id = "mode")),
    nav_item(actionButton("share_nav", NULL, icon = icon("share-square"),
                          class = "nav-icon-btn", title = "Share this view", `aria-label` = "Share this view")),
    nav_item(actionButton("settings_open", NULL, icon = icon("gear"),
                          class = "nav-icon-btn", title = "Manage", `aria-label` = "Manage"))
  )
}
# =============================================================================
# SERVER
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/requests_page.R
# Owner: Dagim
# Purpose: Implement Requests tab layout and reactive behavior.
# Expected code: Namespaced UI/server module: state cards, compact filters, newest-first table, pagination, export handler and modal launch.
# Connections: Calls requests_service, requests_modals, weekly_report, requests_export and shared components; returns refresh/action signals to app shell.
# Boundary: No direct SQL or lifecycle writes. State-card counts exclude selected-state filter. Table remains immediately below cards.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
