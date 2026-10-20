# Logistics PO Tracker - single-file Shiny application
# Run from this file's folder: shiny::runApp()
# Install once: install.packages(c("shiny", "bslib", "DBI", "RSQLite", "DT", "dplyr", "writexl"))
# No confidential budget ceilings/balances, authentication, or payment execution.
# Original amounts are USD. Other currencies use per-PO, manually supplied rates.
# One PO may have several allocations, each with one project and budget line.
# One payment per PO. Only paid POs appear in the finance paid total.

required_packages <- c("shiny", "bslib", "DBI", "RSQLite", "DT", "dplyr", "writexl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages)) {
  stop("Install these R packages first: ", paste(missing_packages, collapse = ", "))
}
library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
options(scipen = 999)

# A new database is intentional: no migration from the old prototype.
DB_FILE <- Sys.getenv("PO_TRACKER_DB", unset = file.path(getwd(), "po_tracker_v1.sqlite"))
BATCH <- "Main batch"
CURRENCIES <- c("USD", "ETB", "EUR", "GBP")
STAGES <- c("waiting", "under validation", "paid", "cancelled")
STAGE_LABELS <- c("Waiting" = "waiting", "Under validation" = "under validation",
                  "Paid" = "paid", "Cancelled" = "cancelled")
DEPARTMENTS <- c("REACH", "PANDA", "ACTED")
SCENARIOS <- c("Not specified" = "", "A - Direct purchase" = "A",
               "B - Quotations" = "B", "C - Contract" = "C",
               "D - National/local tender" = "D", "E - International tender" = "E",
               "G - Framework agreement" = "G")

text <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("")
  trimws(as.character(x))
}
number_ok <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
date_ok <- function(x) inherits(x, "Date") && length(x) == 1L && !is.na(x)
iso_date <- function(x) if (date_ok(x)) as.character(x) else NA_character_
num_or_na <- function(x) if (number_ok(x)) as.numeric(x) else NA_real_
stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
money <- function(x) ifelse(is.na(x), "Rate missing", formatC(x, digits = 2,
                                                              format = "f", big.mark = ","))
stage_label <- function(x) unname(setNames(names(STAGE_LABELS), STAGE_LABELS)[x])

connect_db <- function() {
  cx <- DBI::dbConnect(RSQLite::SQLite(), DB_FILE)
  DBI::dbExecute(cx, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(cx, "PRAGMA busy_timeout = 5000")
  cx
}
initialize_db <- function() {
  cx <- connect_db(); on.exit(DBI::dbDisconnect(cx))
  DBI::dbExecute(cx, "CREATE TABLE IF NOT EXISTS po_requests (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    batch TEXT NOT NULL, description TEXT NOT NULL,
    requested_by TEXT NOT NULL, submitted_by TEXT NOT NULL,
    department TEXT NOT NULL, country TEXT NOT NULL, location TEXT NOT NULL,
    order_date TEXT NOT NULL, processing_date TEXT,
    order_form TEXT, po_reference TEXT, po_reason TEXT, po_date TEXT,
    supplier TEXT, unit TEXT, quantity REAL,
    amount_usd REAL NOT NULL CHECK(amount_usd > 0),
    status TEXT NOT NULL CHECK(status IN ('waiting','under validation','paid','cancelled')),
    scenario TEXT, waiver TEXT, finance_date TEXT, payment_date TEXT,
    remarks TEXT, program_remarks TEXT, cancellation_reason TEXT,
    rate_etb REAL, rate_eur REAL, rate_gbp REAL, rate_date TEXT,
    created TEXT NOT NULL, updated TEXT NOT NULL,
    version INTEGER NOT NULL DEFAULT 1)")
  DBI::dbExecute(cx, "CREATE TABLE IF NOT EXISTS po_allocations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    po_id INTEGER NOT NULL REFERENCES po_requests(id) ON DELETE CASCADE,
    project_code TEXT NOT NULL, budget_line TEXT NOT NULL,
    amount_usd REAL NOT NULL CHECK(amount_usd > 0))")
  DBI::dbExecute(cx, "CREATE INDEX IF NOT EXISTS idx_alloc_po ON po_allocations(po_id)")
  DBI::dbExecute(cx, "CREATE TABLE IF NOT EXISTS app_revision (
    id INTEGER PRIMARY KEY CHECK(id = 1), revision INTEGER NOT NULL)")
  DBI::dbExecute(cx, "INSERT OR IGNORE INTO app_revision VALUES (1, 0)")
}
initialize_db()
read_requests <- function() {
  cx <- connect_db(); on.exit(DBI::dbDisconnect(cx))
  DBI::dbGetQuery(cx, "SELECT * FROM po_requests ORDER BY id DESC")
}
read_allocations <- function() {
  cx <- connect_db(); on.exit(DBI::dbDisconnect(cx))
  DBI::dbGetQuery(cx, "SELECT * FROM po_allocations ORDER BY id")
}


read_dataset <- function() {
  cx <- connect_db(); on.exit(DBI::dbDisconnect(cx))
  DBI::dbWithTransaction(cx, {
    list(requests = DBI::dbGetQuery(cx, "SELECT * FROM po_requests ORDER BY id DESC"),
         allocations = DBI::dbGetQuery(cx, "SELECT * FROM po_allocations ORDER BY id"))
  })
}

# White/blue application styling; replace the accent with an approved brand value if supplied.
css <- "
:root{--accent:#005baa;--ink:#172b4d;--line:#e3e9f0}
body{background:#f5f7fa;color:var(--ink);font-family:system-ui,-apple-system,'Segoe UI',sans-serif;font-size:15px}
.navbar{background:white!important;border-bottom:1px solid var(--line);box-shadow:none}
.navbar-brand{font-weight:700;color:var(--accent)!important;font-size:1.15rem}
.navbar .nav-link{font-weight:500;padding:.9rem 1rem!important}
.navbar .nav-link.active{color:var(--accent)!important;background:#f0f6fc;border-radius:5px}
.wrap{max-width:1440px;margin:0 auto;padding:26px 18px 50px}
.heading{display:flex;justify-content:space-between;align-items:center;gap:16px;margin-bottom:22px;flex-wrap:wrap}
h2{font-size:1.6rem;font-weight:650;margin:0 0 5px}h3{font-size:1.05rem;font-weight:650}
.subtle{color:#607087;font-size:.9rem}.panel{background:white;border:1px solid var(--line);border-radius:8px;padding:22px;margin-bottom:18px}
.metrics{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:12px;margin-bottom:20px}
.metric{background:white;border:1px solid var(--line);border-radius:8px;padding:17px}.metric strong{display:block;font-size:1.7rem;margin-top:5px;color:var(--accent)}
.filters{display:grid;grid-template-columns:2fr repeat(4,minmax(120px,1fr));gap:12px;align-items:end}
.form-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:0 22px}.full{grid-column:1/-1}
.shiny-input-container{width:100%!important}.control-label{font-size:.88rem;font-weight:550;margin-bottom:6px}
.form-control,.form-select,.selectize-input{border-color:#d6dfe9;border-radius:5px;box-shadow:none}
.form-control:focus,.form-select:focus{border-color:var(--accent);box-shadow:0 0 0 3px #005baa18}
.btn{border-radius:5px;font-weight:550}.btn-primary{background:var(--accent);border-color:var(--accent)}
.actions{display:flex;gap:10px;align-items:center;flex-wrap:wrap}.section-title{border-bottom:1px solid var(--line);padding-bottom:12px;margin-bottom:18px}
.alloc-row{display:grid;grid-template-columns:1fr 1fr 1fr;gap:14px;border-bottom:1px solid #edf0f4;padding-top:8px}
.notice{background:#f0f6fc;border-left:3px solid var(--accent);padding:12px 15px;margin-bottom:16px;font-size:.9rem}
.error-list{background:#fff4f3;color:#a32b25;border:1px solid #f0ccc8;border-radius:6px;padding:14px 20px;margin-bottom:16px}
.badge-stage{display:inline-block;padding:4px 9px;border-radius:4px;font-size:.82rem;font-weight:600;white-space:nowrap}
.stage-waiting{background:#fff3d6;color:#805500}.stage-under-validation{background:#e9f2ff;color:#005baa}
.stage-paid{background:#e9f6ed;color:#216b3b}.stage-cancelled{background:#f0f1f3;color:#59616d}
table.dataTable tbody td{vertical-align:middle;font-size:.9rem}table.dataTable thead th{font-size:.82rem;background:#f7f9fc;color:#52647b}
.detail-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px 24px}.detail-label{font-size:.78rem;color:#607087;margin-bottom:3px}.detail-value{white-space:pre-wrap;overflow-wrap:anywhere}
@media(max-width:1000px){.filters{grid-template-columns:repeat(3,minmax(0,1fr))}.metrics{grid-template-columns:repeat(3,1fr)}}
@media(max-width:650px){.wrap{padding:18px 10px}.panel{padding:16px}.form-grid,.detail-grid,.alloc-row{grid-template-columns:1fr}.metrics{grid-template-columns:repeat(2,1fr)}.filters{grid-template-columns:1fr 1fr}}
"

field <- function(label, value) {
  if (!length(value) || is.na(value) || !nzchar(as.character(value))) value <- "Not recorded"
  div(div(class = "detail-label", label), div(class = "detail-value", as.character(value)))
}
section <- function(title, ...) div(class = "panel", h3(class = "section-title", title), ...)
blank_date <- function(id, label) dateInput(id, label, value = NA, format = "dd M yyyy")

ui <- page_navbar(
  title = "Logistics PO Tracker", id = "nav", selected = "requests", fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#005baa", bg = "#f5f7fa", fg = "#172b4d"),
  header = tags$head(tags$style(HTML(css))),
  nav_panel("Requests", value = "requests", div(class = "wrap",
                                                div(class = "heading", div(h2("Purchase orders"),
                                                                           div(class = "subtle", "Record, review and track logistics requests.")),
                                                    actionButton("new_po", "Record new PO", class = "btn-primary")),
                                                uiOutput("metrics"),
                                                div(class = "panel",
                                                    div(class = "filters",
                                                        textInput("search", "Search", placeholder = "ID, description, reference or person"),
                                                        selectInput("f_status", "Stage", c("All stages" = "", STAGE_LABELS)),
                                                        selectInput("f_project", "Project", c("All projects" = "")),
                                                        selectInput("f_budget", "Budget line", c("All budget lines" = "")),
                                                        selectInput("f_location", "Location", c("All locations" = ""))),
                                                    div(class = "form-grid",
                                                        blank_date("f_from", "Request date from"), blank_date("f_to", "Request date to")),
                                                    div(class = "actions", actionButton("clear_filters", "Clear filters"),
                                                        downloadButton("export", "Export filtered workbook"))),
                                                div(class = "panel", div(class = "heading", div(h3("Request register"),
                                                                                                textOutput("row_count")), div(class = "actions",
                                                                                                                              actionButton("open_po", "Open selected"), actionButton("edit_po", "Edit selected"))),
                                                    p(class = "subtle", "Select a row, then open or edit it. All amounts below are original USD amounts."),
                                                    DTOutput("register"))
  )),
  nav_panel("Finance summary", value = "finance", div(class = "wrap",
                                                      div(class = "heading", div(h2("Amounts by budget line"),
                                                                                 div(class = "subtle", "Current-stage snapshot for the filtered requests.")),
                                                          downloadButton("finance_export", "Export finance workbook")),
                                                      div(class = "notice", "No confidential budget ceilings or available balances are stored. Only the Paid column is included in the finance paid total. This is a cumulative snapshot, not an instruction to deduct the same records again."),
                                                      div(class = "panel", selectInput("report_currency", "Display currency", CURRENCIES,
                                                                                       selected = "USD", width = "220px"),
                                                          p(class = "subtle", "Uses each PO's saved units-per-USD rate. USD remains the original amount. Filters on Requests apply here too."),
                                                          uiOutput("conversion_warning"), DTOutput("finance_table"))
  )),
  nav_panel_hidden(value = "form", div(class = "wrap",
                                       div(class = "heading", div(h2(textOutput("form_title", inline = TRUE)),
                                                                  div(class = "subtle", "Complete the record before saving. * Required; other fields are optional.")),
                                           actionButton("back", "Back to requests")),
                                       uiOutput("form_errors"),
                                       section("1. Request details", div(class = "form-grid",
                                                                         div(class = "full", textAreaInput("description", "Description *", rows = 3)),
                                                                         textInput("requested_by", "Requested by *"), textInput("submitted_by", "Logistics focal point *"),
                                                                         selectInput("department", "Department *", c("Select" = "", DEPARTMENTS)),
                                                                         textInput("country", "Country *", "ETH"), textInput("location", "Location / area *"),
                                                                         dateInput("order_date", "Order form / request date *", Sys.Date(), format = "dd M yyyy"),
                                                                         blank_date("processing_date", "Processing date"), textInput("order_form", "Order form reference"),
                                                                         numericInput("quantity", "Quantity", NA, min = 0), textInput("unit", "Unit"))),
                                       section("2. Amount and project / budget allocation",
                                               div(class = "notice", "Enter the original PO total in USD. For multiple budgets, add allocation rows. Their USD amounts must equal the PO total. Each row contains one project and one budget line."),
                                               numericInput("amount_usd", "Total PO amount (USD) *", NA, min = 0, step = 0.01),
                                               uiOutput("allocation_inputs"),
                                               div(class = "actions", actionButton("add_allocation", "Add another budget allocation"),
                                                   actionButton("remove_allocation", "Remove last allocation")),
                                               uiOutput("allocation_total")),
                                       section("3. Procurement and payment",
                                               div(class = "actions", actionButton("suggest_scenario", "Suggest scenario from USD total"),
                                                   actionButton("draft_refs", "Generate draft references")),
                                               p(class = "subtle", "Suggestions use the prototype rules, not a policy check. Existing references are never overwritten. Confirm drafts before saving."),
                                               uiOutput("scenario_hint"), div(class = "form-grid",
                                                                              textInput("supplier", "Supplier"), selectInput("scenario", "Procurement scenario", SCENARIOS),
                                                                              selectInput("waiver", "Memo / waiver", c("No", "Framework agreement", "Other")),
                                                                              textInput("po_reason", "PO reason"), textInput("po_reference", "Purchase order / contract reference"),
                                                                              blank_date("po_date", "Purchase order date"), selectInput("status", "Stage *", STAGE_LABELS),
                                                                              blank_date("finance_date", "Submitted to finance date"),
                                                                              conditionalPanel("input.status == 'paid'", blank_date("payment_date", "Payment date *")),
                                                                              conditionalPanel("input.status == 'cancelled'", textInput("cancellation_reason", "Cancellation reason")))),
                                       section("4. Currency conversions",
                                               p(class = "subtle", "Optional: enter approved units per 1 USD, not the inverse. No live or example rates are supplied. These rates are saved with this PO; editing them does not change other POs."),
                                               div(class = "form-grid", numericInput("rate_etb", "ETB per 1 USD", NA, min = 0),
                                                   numericInput("rate_eur", "EUR per 1 USD", NA, min = 0),
                                                   numericInput("rate_gbp", "GBP per 1 USD", NA, min = 0), blank_date("rate_date", "Exchange-rate date"),
                                                   selectInput("preview_currency", "Preview currency", CURRENCIES, selected = "USD"),
                                                   uiOutput("currency_preview"))),
                                       section("5. Remarks", div(class = "form-grid",
                                                                 textAreaInput("remarks", "Logistics remarks", rows = 3),
                                                                 textAreaInput("program_remarks", "Program / other staff remarks", rows = 3))),
                                       div(class = "panel actions", actionButton("save_po", "Save PO", class = "btn-primary"),
                                           actionButton("cancel_form", "Cancel"), span(class = "subtle", "One batch: Main batch. One payment per PO."))
  ))
)

server <- function(input, output, session) {
  editing <- reactiveVal(NULL)
  errors <- reactiveVal(character())
  alloc_keys <- reactiveVal(1L)
  next_key <- reactiveVal(1L)
  alloc_seed <- reactiveVal(data.frame(key = 1L, project_code = "", budget_line = "", amount_usd = NA_real_))
  local_refresh <- reactiveVal(0L)
  saving <- FALSE
  form_saved <- FALSE
  
  snapshot <- reactivePoll(1500, session,
                           checkFunc = function() {
                             cx <- connect_db(); on.exit(DBI::dbDisconnect(cx))
                             DBI::dbGetQuery(cx, "SELECT revision FROM app_revision WHERE id=1")$revision[[1]]
                           },
                           valueFunc = read_dataset)
  # Local refresh provides immediate feedback after this session saves.
  dataset <- reactive({
    local_refresh()
    snapshot()
    read_dataset()
  })
  requests <- reactive(dataset()$requests)
  allocations <- reactive(dataset()$allocations)
  
  observe({
    d <- requests(); a <- allocations()
    updateSelectInput(session, "f_project", choices = c("All projects" = "", sort(unique(a$project_code))),
                      selected = isolate(input$f_project))
    updateSelectInput(session, "f_location", choices = c("All locations" = "", sort(unique(d$location))),
                      selected = isolate(input$f_location))
  })
  observe({
    a <- allocations()
    if (nzchar(text(input$f_project))) a <- a[a$project_code == input$f_project, , drop = FALSE]
    choices <- sort(unique(a$budget_line))
    selected <- isolate(text(input$f_budget))
    if (!selected %in% choices) selected <- ""
    updateSelectInput(session, "f_budget", choices = c("All budget lines" = "", choices), selected = selected)
  })
  
  filtered <- reactive({
    d <- requests(); a <- allocations()
    if (nzchar(text(input$f_status))) d <- d[d$status == input$f_status, , drop = FALSE]
    if (nzchar(text(input$f_location))) d <- d[d$location == input$f_location, , drop = FALSE]
    if (date_ok(input$f_from)) d <- d[d$order_date >= as.character(input$f_from), , drop = FALSE]
    if (date_ok(input$f_to)) d <- d[d$order_date <= as.character(input$f_to), , drop = FALSE]
    if (nzchar(text(input$f_project))) a <- a[a$project_code == input$f_project, , drop = FALSE]
    if (nzchar(text(input$f_budget))) a <- a[a$budget_line == input$f_budget, , drop = FALSE]
    if (nzchar(text(input$f_project)) || nzchar(text(input$f_budget))) d <- d[d$id %in% a$po_id, , drop = FALSE]
    q <- tolower(text(input$search))
    if (nzchar(q) && nrow(d)) {
      cols <- c("id", "description", "requested_by", "submitted_by", "order_form", "po_reference", "supplier", "location")
      hay <- apply(d[, cols, drop = FALSE], 1, function(x) paste(x, collapse = " "))
      d <- d[grepl(q, tolower(hay), fixed = TRUE), , drop = FALSE]
    }
    d
  })
  scoped_allocations <- reactive({
    a <- allocations(); a <- a[a$po_id %in% filtered()$id, , drop = FALSE]
    if (nzchar(text(input$f_project))) a <- a[a$project_code == input$f_project, , drop = FALSE]
    if (nzchar(text(input$f_budget))) a <- a[a$budget_line == input$f_budget, , drop = FALSE]
    a
  })
  observeEvent(input$clear_filters, {
    updateTextInput(session, "search", value = "")
    for (id in c("f_status", "f_project", "f_budget", "f_location")) updateSelectInput(session, id, selected = "")
    updateDateInput(session, "f_from", value = NA)
    updateDateInput(session, "f_to", value = NA)
  })
  
  output$metrics <- renderUI({
    d <- filtered()
    counts <- c("All POs" = nrow(d), "Waiting" = sum(d$status == "waiting"),
                "Under validation" = sum(d$status == "under validation"),
                "Paid" = sum(d$status == "paid"), "Cancelled" = sum(d$status == "cancelled"))
    div(class = "metrics", lapply(names(counts), function(nm)
      div(class = "metric", span(nm), strong(counts[[nm]]))))
  })
  output$row_count <- renderText(paste(nrow(filtered()), "matching POs. Amounts in the register are whole-PO totals; finance uses only matching allocation rows."))
  output$register <- renderDT({
    d <- filtered(); a <- allocations()
    labels <- vapply(d$id, function(id) {
      z <- a[a$po_id == id, , drop = FALSE]
      paste(paste(z$project_code, z$budget_line, sep = " / "), collapse = "; ")
    }, character(1))
    stage <- vapply(d$status, function(s) {
      cls <- paste0("stage-", gsub(" ", "-", s))
      paste0('<span class="badge-stage ', cls, '">', stage_label(s), '</span>')
    }, character(1))
    view <- data.frame(ID = d$id, Description = d$description, Allocation = labels,
                       USD = d$amount_usd, Stage = stage, Location = d$location,
                       Focal = d$submitted_by, Date = d$order_date, check.names = FALSE)
    datatable(view, rownames = FALSE, escape = setdiff(seq_len(ncol(view)), 5L),
              selection = "single", options = list(dom = "tip", pageLength = 10,
                                                   scrollX = TRUE, order = list(list(0, "desc")),
                                                   language = list(emptyTable = "No POs match these filters."))) |>
      formatRound("USD", 2)
  }, server = FALSE)
  
  selected_record <- function() {
    idx <- input$register_rows_selected
    d <- isolate(filtered())
    if (length(idx) != 1L || idx < 1L || idx > nrow(d)) {
      showNotification("Select one PO in the register first.", type = "warning")
      return(NULL)
    }
    d[idx, , drop = FALSE]
  }
  
  # Allocation inputs preserve unsaved values when rows are added/removed.
  collect_allocations <- function() {
    keys <- isolate(alloc_keys()); seed <- isolate(alloc_seed())
    rows <- lapply(keys, function(k) {
      fallback <- seed[seed$key == k, , drop = FALSE]
      get_value <- function(prefix, col, default) {
        val <- isolate(input[[paste0(prefix, k)]])
        if (!is.null(val)) return(val)
        if (nrow(fallback)) return(fallback[[col]][[1]])
        default
      }
      data.frame(key = k,
                 project_code = text(get_value("ap_", "project_code", "")),
                 budget_line = text(get_value("ab_", "budget_line", "")),
                 amount_usd = num_or_na(get_value("aa_", "amount_usd", NA_real_)))
    })
    do.call(rbind, rows)
  }
  output$allocation_inputs <- renderUI({
    keys <- alloc_keys(); seed <- alloc_seed()
    tagList(lapply(keys, function(k) {
      row <- seed[seed$key == k, , drop = FALSE]
      if (!nrow(row)) row <- data.frame(project_code = "", budget_line = "", amount_usd = NA_real_)
      div(class = "alloc-row",
          textInput(paste0("ap_", k), "Project code *", row$project_code[[1]]),
          textInput(paste0("ab_", k), "Budget line *", row$budget_line[[1]]),
          numericInput(paste0("aa_", k), "Allocation amount (USD) *", row$amount_usd[[1]], min = 0, step = 0.01))
    }))
  })
  observeEvent(input$add_allocation, {
    a <- collect_allocations(); k <- next_key() + 1L; next_key(k)
    alloc_seed(rbind(a, data.frame(key = k, project_code = "", budget_line = "", amount_usd = NA_real_)))
    alloc_keys(c(alloc_keys(), k))
  })
  observeEvent(input$remove_allocation, {
    if (length(alloc_keys()) <= 1L) return()
    a <- collect_allocations(); keys <- head(alloc_keys(), -1L)
    alloc_seed(a[a$key %in% keys, , drop = FALSE]); alloc_keys(keys)
  })
  output$allocation_total <- renderUI({
    vals <- lapply(alloc_keys(), function(k) input[[paste0("aa_", k)]])
    amounts <- vapply(vals, num_or_na, numeric(1))
    if (anyNA(amounts)) return(p(class = "subtle", "Enter an amount for every allocation row."))
    p(class = "subtle", paste("Allocated:", money(sum(amounts)), "USD | PO total:",
                              if (number_ok(input$amount_usd)) money(input$amount_usd) else "not entered"))
  })
  
  text_fields <- c("description", "requested_by", "submitted_by", "country", "location",
                   "order_form", "po_reason", "po_reference", "supplier", "unit",
                   "remarks", "program_remarks", "cancellation_reason")
  date_fields <- c("order_date", "processing_date", "po_date", "finance_date", "payment_date", "rate_date")
  numeric_fields <- c("quantity", "amount_usd", "rate_etb", "rate_eur", "rate_gbp")
  select_fields <- c("department", "scenario", "waiver", "status")
  
  load_form <- function(row = NULL) {
    form_saved <<- FALSE
    errors(character())
    editing(if (is.null(row)) NULL else list(id = row$id[[1]], version = row$version[[1]]))
    for (id in text_fields) {
      val <- if (is.null(row)) { if (id == "country") "ETH" else "" } else text(row[[id]][[1]])
      if (id %in% c("description", "remarks", "program_remarks")) updateTextAreaInput(session, id, value = val)
      else updateTextInput(session, id, value = val)
    }
    for (id in numeric_fields) updateNumericInput(session, id,
                                                  value = if (is.null(row)) NA_real_ else row[[id]][[1]])
    for (id in date_fields) {
      val <- if (is.null(row)) { if (id == "order_date") Sys.Date() else as.Date(NA) }
      else if (is.na(row[[id]][[1]]) || !nzchar(row[[id]][[1]])) as.Date(NA)
      else as.Date(row[[id]][[1]])
      updateDateInput(session, id, value = val)
    }
    defaults <- list(department = "", scenario = "", waiver = "No", status = "waiting")
    for (id in select_fields) updateSelectInput(session, id,
                                                selected = if (is.null(row)) defaults[[id]] else row[[id]][[1]])
    updateSelectInput(session, "preview_currency", selected = "USD")
    a <- if (is.null(row)) data.frame(project_code = "", budget_line = "", amount_usd = NA_real_)
    else isolate(allocations())[isolate(allocations())$po_id == row$id[[1]], c("project_code", "budget_line", "amount_usd"), drop = FALSE]
    if (!nrow(a)) a <- data.frame(project_code = "", budget_line = "", amount_usd = NA_real_)
    # Fresh keys prevent stale browser input values being reused between POs.
    keys <- seq.int(next_key() + 1L, length.out = nrow(a))
    next_key(max(keys)); a$key <- keys
    alloc_seed(a[, c("key", "project_code", "budget_line", "amount_usd")]); alloc_keys(keys)
    nav_select("nav", "form", session = session)
  }
  observeEvent(input$new_po, load_form())
  observeEvent(input$edit_po, { row <- selected_record(); if (!is.null(row)) load_form(row) })
  observeEvent(input$back, nav_select("nav", "requests", session = session))
  observeEvent(input$cancel_form, { errors(character()); nav_select("nav", "requests", session = session) })
  output$form_title <- renderText(if (is.null(editing())) "Record new PO" else paste("Edit PO #", editing()$id))
  output$form_errors <- renderUI({
    if (!length(errors())) return(NULL)
    div(class = "error-list", strong("Please correct the following:"), tags$ul(lapply(errors(), tags$li)))
  })
  output$currency_preview <- renderUI({
    cur <- text(input$preview_currency)
    if (!number_ok(input$amount_usd)) return(p(class = "subtle", "Enter the USD total first."))
    rate <- if (cur == "USD") 1 else input[[paste0("rate_", tolower(cur))]]
    if (!number_ok(rate) || rate <= 0) return(p(class = "subtle", paste("Enter", cur, "per USD to preview this conversion.")))
    div(class = "notice", strong(paste(money(input$amount_usd * rate), cur)),
        div(class = "subtle", paste("Original:", money(input$amount_usd), "USD")))
  })
  
  observeEvent(input$suggest_scenario, {
    if (!number_ok(input$amount_usd) || input$amount_usd <= 0) {
      showNotification("Enter a positive USD total first.", type = "warning"); return()
    }
    code <- if (identical(input$waiver, "Framework agreement")) "G" else
      as.character(cut(input$amount_usd, breaks = c(-Inf, 500, 5000, 30000, 150000, Inf),
                       labels = c("A", "B", "C", "D", "E"), right = FALSE))
    updateSelectInput(session, "scenario", selected = code)
  })
  output$scenario_hint <- renderUI({
    lead <- c(A = "less than 1 week", B = "about 2-3 weeks", C = "about 2-3 weeks",
              D = "about 1.5 months", E = "about 2 months", G = "about 2 months")
    code <- text(input$scenario)
    if (!code %in% names(lead)) return(NULL)
    p(class = "subtle", paste("Indicative lead time from the prototype:", lead[[code]],
                              "(not a deadline or guarantee)."))
  })
  observeEvent(input$draft_refs, {
    a <- collect_allocations()
    if (any(!nzchar(a$project_code)) || any(!nzchar(a$budget_line)) ||
        !date_ok(input$order_date) || !nzchar(text(input$country)) ||
        !nzchar(text(input$location)) || !nzchar(text(input$department))) {
      showNotification("Complete project, budget line, country, location, department and request date first.", type = "warning")
      return()
    }
    # Multi-allocation references need explicit review rather than silently choosing one budget.
    if (nrow(a) > 1L) {
      showNotification("For a multi-budget PO, enter the actual external references manually.", type = "warning")
      return()
    }
    parts <- c(toupper(a$project_code[[1]]), toupper(a$budget_line[[1]]),
               text(input$country), text(input$location), text(input$department))
    base <- paste(parts, collapse = "/")
    if (!nzchar(text(input$order_form))) updateTextInput(session, "order_form",
                                                         value = paste0("OF/", base, "/", format(input$order_date, "%d-%m-%Y")))
    if (!nzchar(text(input$po_reference)) && nzchar(text(input$po_reason)) && date_ok(input$po_date))
      updateTextInput(session, "po_reference", value = paste0("PO/OF/", base, "/",
                                                              text(input$po_reason), "/", format(input$po_date, "%d-%m-%Y")))
    showNotification("Drafted blank reference fields only. Check them against the actual documents.", type = "message")
  })
  
  observeEvent(input$save_po, {
    if (saving) return()
    if (form_saved) {
      showNotification("This form was already saved. Use Record new PO or Edit selected to start again.", type = "warning")
      return()
    }
    saving <<- TRUE
    on.exit({ saving <<- FALSE }, add = TRUE)
    a <- collect_allocations(); issues <- character()
    required_text <- c(description = "Description", requested_by = "Requested by",
                       submitted_by = "Logistics focal point", country = "Country", location = "Location")
    for (id in names(required_text)) if (!nzchar(text(input[[id]]))) issues <- c(issues, paste(required_text[[id]], "is required."))
    if (!text(input$department) %in% DEPARTMENTS) issues <- c(issues, "Select a department.")
    if (!text(input$status) %in% STAGES) issues <- c(issues, "Select a valid stage.")
    if (!date_ok(input$order_date)) issues <- c(issues, "Request date is required.")
    if (!number_ok(input$amount_usd) || input$amount_usd <= 0) issues <- c(issues, "PO amount must be greater than zero.")
    if (number_ok(input$amount_usd) && abs(input$amount_usd - round(input$amount_usd, 2)) > 1e-7)
      issues <- c(issues, "PO amount must have at most two decimal places.")
    if (any(abs(a$amount_usd - round(a$amount_usd, 2)) > 1e-7, na.rm = TRUE))
      issues <- c(issues, "Allocation amounts must have at most two decimal places.")
    if (any(!nzchar(a$project_code)) || any(!nzchar(a$budget_line))) issues <- c(issues, "Every allocation needs a project code and budget line.")
    if (anyNA(a$amount_usd) || any(!is.finite(a$amount_usd)) || any(a$amount_usd <= 0, na.rm = TRUE)) issues <- c(issues, "Every allocation amount must be greater than zero.")
    if (number_ok(input$amount_usd) && !anyNA(a$amount_usd) &&
        abs(sum(a$amount_usd) - input$amount_usd) > 0.005) issues <- c(issues, "Allocation amounts must equal the USD PO total.")
    if (anyDuplicated(paste(toupper(a$project_code), toupper(a$budget_line), sep = "|"))) issues <- c(issues, "Combine duplicate project/budget-line rows into one allocation.")
    if (number_ok(input$quantity) && input$quantity <= 0) issues <- c(issues, "Quantity must be positive when entered.")
    for (id in c("rate_etb", "rate_eur", "rate_gbp")) {
      val <- input[[id]]
      if (!is.null(val) && length(val) && !is.na(val) && (!number_ok(val) || val <= 0)) issues <- c(issues, paste(id, "must be positive."))
    }
    any_rate <- any(vapply(c("rate_etb", "rate_eur", "rate_gbp"), function(id) number_ok(input[[id]]), logical(1)))
    if (any_rate && !date_ok(input$rate_date)) issues <- c(issues, "Enter the exchange-rate date when supplying rates.")
    if (identical(input$status, "paid") && !date_ok(input$payment_date)) issues <- c(issues, "Payment date is required for a paid PO.")
    if (length(issues)) { errors(unique(issues)); showNotification("Please review the validation messages above the form.", type = "warning"); return() }
    row <- lapply(text_fields, function(id) text(input[[id]])); names(row) <- text_fields
    for (id in date_fields) row[[id]] <- iso_date(input[[id]])
    for (id in numeric_fields) row[[id]] <- num_or_na(input[[id]])
    for (id in select_fields) row[[id]] <- text(input[[id]])
    row$payment_date <- if (input$status == "paid") iso_date(input$payment_date) else NA_character_
    row$cancellation_reason <- if (input$status == "cancelled") text(input$cancellation_reason) else ""
    row$batch <- BATCH; row$updated <- stamp()
    current <- isolate(editing())
    result <- tryCatch({
      cx <- connect_db(); on.exit(DBI::dbDisconnect(cx), add = TRUE)
      DBI::dbWithTransaction(cx, {
        if (is.null(current)) {
          row$created <- row$updated
          DBI::dbExecute(cx, paste0("INSERT INTO po_requests (", paste(names(row), collapse = ","),
                                    ") VALUES (", paste(rep("?", length(row)), collapse = ","), ")"), params = unname(row))
          id <- DBI::dbGetQuery(cx, "SELECT last_insert_rowid() AS id")$id[[1]]
        } else {
          id <- current$id
          changed <- DBI::dbExecute(cx, paste0("UPDATE po_requests SET ",
                                               paste(paste0(names(row), "=?"), collapse = ","), ",version=version+1 WHERE id=? AND version=?"),
                                    params = c(unname(row), list(id, current$version)))
          if (changed != 1L) stop("This PO changed in another session. Reopen it from Requests before editing again.")
          DBI::dbExecute(cx, "DELETE FROM po_allocations WHERE po_id=?", params = list(id))
        }
        for (i in seq_len(nrow(a))) DBI::dbExecute(cx,
                                                   "INSERT INTO po_allocations (po_id,project_code,budget_line,amount_usd) VALUES (?,?,?,?)",
                                                   params = list(id, toupper(a$project_code[[i]]), toupper(a$budget_line[[i]]), a$amount_usd[[i]]))
        DBI::dbExecute(cx, "UPDATE app_revision SET revision=revision+1 WHERE id=1")
        id
      })
    }, error = function(e) e)
    if (inherits(result, "error")) {
      errors(paste("Could not save:", conditionMessage(result)))
      showNotification("PO was not saved. Your form is still available.", type = "error")
      return()
    }
    form_saved <<- TRUE
    errors(character()); editing(NULL); local_refresh(local_refresh() + 1L)
    nav_select("nav", "requests", session = session)
    showNotification(paste("PO #", result, "saved."), type = "message")
  })
  
  observeEvent(input$open_po, {
    row <- selected_record(); if (is.null(row)) return()
    a <- allocations(); a <- a[a$po_id == row$id[[1]], , drop = FALSE]
    showModal(modalDialog(title = paste("PO #", row$id[[1]], "-", stage_label(row$status[[1]])), size = "l",
                          div(class = "detail-grid",
                              field("Description", row$description), field("Original amount (USD)", money(row$amount_usd)),
                              field("Requester", row$requested_by), field("Logistics focal point", row$submitted_by),
                              field("Department", row$department), field("Country / location", paste(row$country, row$location, sep = " / ")),
                              field("Request date", row$order_date), field("Processing date", row$processing_date),
                              field("Order form reference", row$order_form), field("PO / contract reference", row$po_reference),
                              field("Supplier", row$supplier), field("Quantity / unit", paste(ifelse(is.na(row$quantity), "", row$quantity), row$unit)),
                              field("Scenario", row$scenario), field("Memo / waiver", row$waiver),
                              field("PO reason", row$po_reason), field("PO date", row$po_date),
                              field("Submitted to finance", row$finance_date), field("Payment date", row$payment_date),
                              field("Cancellation reason", row$cancellation_reason), field("Batch", row$batch),
                              field("Logistics remarks", row$remarks), field("Program remarks", row$program_remarks),
                              field("ETB per USD", row$rate_etb), field("EUR per USD", row$rate_eur),
                              field("GBP per USD", row$rate_gbp), field("Rate date", row$rate_date),
                              field("Created (UTC)", row$created), field("Updated (UTC)", row$updated)),
                          tags$hr(), h3("Budget allocations (USD)"),
                          tags$table(class = "table table-sm",
                                     tags$thead(tags$tr(tags$th("Project"), tags$th("Budget line"), tags$th("USD"))),
                                     tags$tbody(lapply(seq_len(nrow(a)), function(i) tags$tr(tags$td(a$project_code[i]),
                                                                                             tags$td(a$budget_line[i]), tags$td(money(a$amount_usd[i])))))),
                          footer = modalButton("Close"), easyClose = TRUE))
  })
  
  finance_rows <- reactive({
    a <- scoped_allocations(); d <- filtered()
    if (!nrow(a)) return(data.frame(project_code = character(), budget_line = character(),
                                    currency = character(), waiting = numeric(), under_validation = numeric(), paid = numeric(),
                                    cancelled = numeric(), missing_rates = integer()))
    z <- merge(a, d[, c("id", "status", "rate_etb", "rate_eur", "rate_gbp"), drop = FALSE],
               by.x = "po_id", by.y = "id", all.x = TRUE)
    cur <- text(input$report_currency); if (!cur %in% CURRENCIES) cur <- "USD"
    rate <- if (cur == "USD") rep(1, nrow(z)) else z[[paste0("rate_", tolower(cur))]]
    z$value <- z$amount_usd * rate
    z$missing <- is.na(z$value)
    # A missing conversion makes the affected stage total NA, never a misleading partial sum.
    z |>
      group_by(project_code, budget_line) |>
      summarise(currency = cur,
                waiting = sum(value[status == "waiting"]),
                under_validation = sum(value[status == "under validation"]),
                paid = sum(value[status == "paid"]),
                cancelled = sum(value[status == "cancelled"]),
                missing_rates = sum(missing), .groups = "drop") |>
      arrange(project_code, budget_line)
  })
  output$conversion_warning <- renderUI({
    f <- finance_rows()
    if (nrow(f) && any(f$missing_rates > 0)) div(class = "notice",
                                                 "Some matching allocations have no saved rate for this currency. Affected totals are blank, not zero. Use USD or edit those POs to supply the rate.")
  })
  output$finance_table <- renderDT({
    datatable(finance_rows(), rownames = FALSE, selection = "none",
              colnames = c("Project", "Budget line", "Currency", "Waiting", "Under validation",
                           "Paid - for finance", "Cancelled - excluded", "Allocations missing rates"),
              options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
      formatRound(c("waiting", "under_validation", "paid", "cancelled"), 2)
  }, server = FALSE)
  
  workbook <- function(file) {
    d <- filtered(); a <- scoped_allocations(); f <- finance_rows()
    # Separate allocation sheet avoids duplicating whole-PO totals across budget lines.
    paid <- a[a$po_id %in% d$id[d$status == "paid"], , drop = FALSE]
    paid <- merge(paid, d[, c("id", "payment_date"), drop = FALSE], by.x = "po_id", by.y = "id", all.x = TRUE)
    meta <- data.frame(item = c("Generated UTC", "Batch", "Report type", "Request-date from", "Request-date to",
                                "Project filter", "Budget-line filter", "Stage filter", "Location filter", "Search", "Summary currency", "Important"),
                       value = c(stamp(), BATCH, "Current-stage snapshot; not incremental deductions",
                                 if (date_ok(input$f_from)) as.character(input$f_from) else "All",
                                 if (date_ok(input$f_to)) as.character(input$f_to) else "All",
                                 text(input$f_project), text(input$f_budget), text(input$f_status), text(input$f_location),
                                 text(input$search), text(input$report_currency),
                                 "PO sheet contains whole-PO USD totals. Allocations and paid-for-finance sheets contain matching allocation USD amounts. No budget balances are calculated."))
    writexl::write_xlsx(list(POs = d, Allocations_USD = a, Summary = f,
                             Paid_for_finance_USD = paid, Report_notes = meta), path = file)
  }
  output$export <- downloadHandler(
    filename = function() paste0("PO_tracker_", Sys.Date(), ".xlsx"), content = workbook)
  output$finance_export <- downloadHandler(
    filename = function() paste0("Finance_snapshot_", Sys.Date(), ".xlsx"), content = workbook)
}

shinyApp(ui, server)
