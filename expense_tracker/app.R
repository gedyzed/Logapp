library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(writexl)

enableBookmarking("url")

DB     <- "tracker.db"
CUR    <- c("ETB", "USD", "EUR", "GBP")
STATUS <- c("waiting", "under validation", "paid")
WAIVER <- c("No", "Framework agreement")
SCEN_NAME <- c(
  A = "Direct purchase", B = "Quotations", C = "Contract",
  D = "National/Local call for tender", E = "International call for tender",
  G = "Framework agreement"
)

# Units per 1 USD. Edit here when rates change (placeholders for now).
RATES <- c(ETB = 155, USD = 1, EUR = 0.92, GBP = 0.79)

# A <500 | B 500-<5k | C 5k-<30k | D 30k-<150k | E >=150k | G if framework
scenario <- function(usd, waiver) {
  base <- as.character(cut(usd, c(-Inf, 500, 5000, 30000, 150000, Inf),
                           labels = c("A", "B", "C", "D", "E"), right = FALSE))
  ifelse(waiver == "Framework agreement", "G", base)
}

con <- function() {
  c <- dbConnect(SQLite(), DB)
  dbExecute(c, "CREATE TABLE IF NOT EXISTS expenses(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    description TEXT, project_code TEXT, budget_line TEXT,
    country TEXT, location TEXT, department TEXT, reason TEXT,
    order_form TEXT, po TEXT, requested_by TEXT, submitted_by TEXT,
    status TEXT, waiver TEXT, supplier TEXT, unit TEXT, quantity REAL,
    amount REAL, currency TEXT, payment_date TEXT, remarks TEXT,
    created TEXT)")
  for (col in c("submitted_by", "reason"))   # migrate older databases
    if (!col %in% dbListFields(c, "expenses"))
      dbExecute(c, paste("ALTER TABLE expenses ADD COLUMN", col, "TEXT"))
  c
}

fmt <- function(d) format(d, "%d-%m-%Y")
v   <- function(x) if (is.null(x) || !length(x) || is.na(x)) "" else trimws(x)
nz  <- function(x) if (nzchar(v(x))) v(x) else NA_character_
has_date <- function(d) !is.null(d) && length(d) == 1 && !is.na(d)

# short fields remembered from previous requests (input id -> db columns)
CACHE <- list(
  requested_by = c("requested_by", "submitted_by"),
  submitted_by = c("requested_by", "submitted_by"),
  project_code = "project_code", budget_line = "budget_line",
  country = "country", location = "location", department = "department",
  supplier = "supplier", unit = "unit", reason = "reason"
)

R   <- function(x) tagList(x, span("required", class = "tag tag-req"))
OPT <- function(x) tagList(x, span("optional", class = "tag"))
col <- function(n, ...) div(class = paste0("c", n), ...)

cached <- function(id, label, required = TRUE)
  selectizeInput(id, if (required) R(label) else OPT(label), NULL, width = "100%",
                 options = list(create = TRUE, placeholder = "Type or pick"))
pick <- function(id, label, choices, required = TRUE)
  selectInput(id, if (required) R(label) else OPT(label),
              c("Select" = "", choices), width = "100%")

theme <- bs_theme(version = 5, primary = "#1d4ed8", "border-radius" = "0.6rem")

css <- "
body{font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}
.navbar{border-bottom:2px solid var(--bs-primary)}
.bslib-grid>*{min-width:0}
.muted{color:var(--bs-secondary-color)}
.ref{font-family:ui-monospace,Menlo,monospace;font-size:.8rem;word-break:break-all}
.badge-scen{font-size:1rem;background:var(--bs-primary)}
.sticky{position:sticky;top:1rem}

/* ---- new request form ---- */
.nr{max-width:880px;padding:.5rem .25rem 0}
.nr-title{font-size:1.7rem;font-weight:600;margin:0}
.nr-sub{color:var(--bs-secondary-color);margin:.15rem 0 1.5rem}
.fg{display:grid;grid-template-columns:repeat(12,minmax(0,1fr));gap:.1rem 1.1rem}
.c3{grid-column:span 3}.c4{grid-column:span 4}.c5{grid-column:span 5}
.c6{grid-column:span 6}.c8{grid-column:span 8}.c12{grid-column:span 12}
@media(max-width:900px){.fg>.c3,.fg>.c4,.fg>.c5{grid-column:span 6}}
@media(max-width:560px){.fg>*{grid-column:span 12!important}}

.nr .control-label{display:block;font-size:.72rem;font-weight:600;
  letter-spacing:.02em;color:var(--bs-secondary-color);margin-bottom:.3rem}
.nr .shiny-input-container{margin-bottom:1rem;width:100%}
.nr .form-control,.nr .form-select,.nr .selectize-input{
  background:var(--bs-tertiary-bg)!important;border:1px solid transparent!important;
  border-radius:.5rem;padding:.55rem .75rem;min-height:2.6rem;box-shadow:none;
  transition:border-color .15s,box-shadow .15s}
.nr textarea.form-control{resize:vertical}
.nr .form-control:focus,.nr .form-select:focus,.nr .selectize-input.focus{
  border-color:var(--bs-primary)!important;background:var(--bs-body-bg)!important;
  box-shadow:0 0 0 3px rgba(var(--bs-primary-rgb),.18)!important}
.tag{font-size:.6rem;text-transform:uppercase;letter-spacing:.05em;
  margin-left:.45rem;font-weight:500;color:var(--bs-secondary-color);opacity:.8}
.tag-req{color:#dc2626;opacity:1}
summary.more{cursor:pointer;color:var(--bs-primary);font-weight:600;
  font-size:.85rem;margin:.1rem 0 1rem;list-style:none}
summary.more::-webkit-details-marker{display:none}
summary.more::before{content:'+ '}
details[open]>summary.more::before{content:'\\2212  '}

.action-bar{position:sticky;bottom:0;z-index:5;display:flex;align-items:center;
  justify-content:flex-end;gap:1.25rem;padding:.85rem 0;margin-top:.5rem;
  background:var(--bs-body-bg);border-top:1px solid var(--bs-border-color)}
.action-bar .btn-primary{text-transform:uppercase;letter-spacing:.06em;
  font-size:.78rem;font-weight:600;padding:.55rem 1.6rem}
.action-bar a.clear{text-transform:uppercase;letter-spacing:.06em;font-size:.78rem;
  font-weight:600;color:var(--bs-secondary-color);text-decoration:none}

/* ---- table: compact rows, long text behind 'see more' ---- */
table.dataTable th,table.dataTable td{white-space:nowrap}
.dtxt{display:inline-block;white-space:normal;max-width:320px}
a.dmore{color:var(--bs-primary);font-weight:600;font-size:.8rem;text-decoration:none}
"

js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('enable', function(m) {
    document.getElementById(m.id).disabled = m.disabled;
  });
});
"

# first 5 words in the row, full text on 'see more'
more_render <- JS(r"---(
function(data, type, row) {
  if (type !== 'display' || data === null || data === undefined) return data;
  var esc = function(s) {
    return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;')
                    .replace(/>/g,'&gt;').replace(/"/g,'&quot;');
  };
  var words = String(data).trim().split(/\s+/);
  if (words.length <= 5) return esc(data);
  return '<span class="dtxt"><span class="dshort">' + esc(words.slice(0,5).join(' ')) +
         '\u2026 </span><span class="dfull" style="display:none">' + esc(data) +
         ' </span><a href="#" class="dmore">see more</a></span>';
}
)---")

more_init <- JS(r"---(
function(settings) {
  $(this.api().table().node()).on('click', 'a.dmore', function(e) {
    e.preventDefault(); e.stopPropagation();
    var s = $(this).closest('.dtxt');
    var open = s.find('.dfull').is(':visible');
    s.find('.dshort').toggle(open);
    s.find('.dfull').toggle(!open);
    $(this).text(open ? 'see more' : 'see less');
  });
}
)---")

ui <- function(request) {
  page_navbar(
    title = "Expense Tracker", theme = theme, fillable = FALSE,
    header = tagList(tags$style(css), tags$script(HTML(js))),
    
    nav_panel("Dashboard",
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                selectizeInput("f_person",  "Requester", NULL, multiple = TRUE,
                               options = list(placeholder = "All")),
                selectizeInput("f_project", "Project",   NULL, multiple = TRUE,
                               options = list(placeholder = "All")),
                selectizeInput("f_status",  "Status", STATUS, multiple = TRUE,
                               options = list(placeholder = "All")),
                div(style = "margin-top:1.5rem",
                    actionButton("edit", "Update status"),
                    downloadButton("export", "Export", class = "btn-outline-primary"))
              ),
              uiOutput("totals"),
              card(card_header("Requests"), DTOutput("table")),
              layout_columns(
                col_widths = c(6, 6),
                card(card_header("Per requester"), DTOutput("by_person")),
                card(card_header("Per project"),   DTOutput("by_project"))
              )
    ),
    
    nav_panel("New request",
              layout_columns(
                col_widths = c(8, 4),
                div(class = "nr",
                    h2("New request", class = "nr-title"),
                    p("Fields marked required must be filled before you can add it.",
                      class = "nr-sub"),
                    div(class = "fg",
                        col(12, textAreaInput("description", R("Description"), rows = 3,
                                              width = "100%")),
                        col(6, cached("requested_by", "Requested by")),
                        col(6, cached("submitted_by", "Submitted by (focal point)")),
                        col(4, cached("project_code", "Project code")),
                        col(4, cached("budget_line",  "Budget line")),
                        col(4, cached("department",   "Department")),
                        col(4, cached("country",      "Country")),
                        col(4, cached("location",     "Location")),
                        col(4, dateInput("of_date", R("Order form date"), value = NA,
                                         format = "dd-mm-yyyy", width = "100%")),
                        col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
                        col(4, pick("currency", "Currency", CUR)),
                        col(4, pick("waiver",   "Memo / waiver", WAIVER)),
                        col(4, pick("status", "Status", STATUS)),
                        uiOutput("pay_ui", class = "c4"),
                        col(12,
                            tags$details(
                              tags$summary("See more", class = "more"),
                              div(class = "fg",
                                  col(6, cached("supplier", "Supplier", FALSE)),
                                  col(3, numericInput("quantity", OPT("Quantity"), NA, min = 0,
                                                      width = "100%")),
                                  col(3, cached("unit", "Unit", FALSE)),
                                  col(6, cached("reason", "PO reason", FALSE)),
                                  col(6, dateInput("po_date", OPT("PO date"), value = NA,
                                                   format = "dd-mm-yyyy", width = "100%")),
                                  col(12, textAreaInput("remarks", OPT("Remarks"), rows = 2,
                                                        width = "100%"))
                              )
                            )
                        )
                    ),
                    div(class = "action-bar",
                        uiOutput("need", inline = TRUE),
                        actionLink("clear", "Clear", class = "clear"),
                        actionButton("add", "Add request", class = "btn-primary",
                                     disabled = NA))
                ),
                div(class = "sticky",
                    card(card_header("Summary"), uiOutput("preview"))
                )
              )
    ),
    
    nav_spacer(),
    nav_item(bookmarkButton(label = "Share link")),
    nav_item(input_dark_mode())
  )
}

server <- function(input, output, session) {
  tick <- reactiveVal(0)
  
  setBookmarkExclude(c("description", names(CACHE), "of_date", "amount",
                       "currency", "waiver", "status", "payment_date",
                       "quantity", "po_date", "remarks", "m_status", "m_date"))
  
  onBookmarked(function(url) {
    updateQueryString(url)
    showModal(modalDialog(div(url, class = "ref"),
                          title = "Share this view", easyClose = TRUE))
  })
  
  data <- reactive({
    tick()
    c <- con(); on.exit(dbDisconnect(c))
    d <- dbGetQuery(c, "SELECT * FROM expenses ORDER BY id DESC")
    if (!nrow(d)) return(d)
    usd <- d$amount / unname(RATES[d$currency])
    for (k in CUR) d[[k]] <- round(usd * RATES[[k]], 2)
    d$scenario <- scenario(usd, d$waiver)
    d
  })
  
  # remembered values for the short fields + dashboard filters
  observe({
    d <- data()
    for (id in names(CACHE)) {
      ch <- unlist(d[CACHE[[id]]], use.names = FALSE)
      ch <- sort(unique(ch[!is.na(ch) & nzchar(ch)]))
      updateSelectizeInput(session, id, choices = c("", ch), selected = "",
                           options = list(create = TRUE,
                                          placeholder = "Type or pick"))
    }
    updateSelectizeInput(session, "f_person",
                         choices = sort(unique(d$requested_by)),
                         selected = isolate(input$f_person))
    updateSelectizeInput(session, "f_project",
                         choices = sort(unique(d$project_code)),
                         selected = isolate(input$f_project))
  })
  
  filtered <- reactive({
    d <- data()
    if (!nrow(d)) return(d)
    if (length(input$f_person))  d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_project)) d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_status))  d <- filter(d, status %in% input$f_status)
    d
  })
  
  output$pay_ui <- renderUI({
    if (!identical(input$status, "paid")) return(NULL)
    dateInput("payment_date", R("Date of payment"), value = NA,
              format = "dd-mm-yyyy", width = "100%")
  })
  
  # ---- required fields: drives the button state and the hint beside it
  required <- reactive(c(
    "Description"     = nzchar(v(input$description)),
    "Requested by"    = nzchar(v(input$requested_by)),
    "Submitted by"    = nzchar(v(input$submitted_by)),
    "Project code"    = nzchar(v(input$project_code)),
    "Budget line"     = nzchar(v(input$budget_line)),
    "Department"      = nzchar(v(input$department)),
    "Country"         = nzchar(v(input$country)),
    "Location"        = nzchar(v(input$location)),
    "Order form date" = has_date(input$of_date),
    "Amount"          = !is.null(input$amount) && !is.na(input$amount),
    "Currency"        = nzchar(v(input$currency)),
    "Memo / waiver"   = nzchar(v(input$waiver)),
    "Status"          = nzchar(v(input$status)),
    "Date of payment" = !identical(input$status, "paid") || has_date(input$payment_date)
  ))
  
  observe({
    session$sendCustomMessage("enable",
                              list(id = "add", disabled = !all(required())))
  })
  
  output$need <- renderUI({
    n <- sum(!required())
    span(class = "muted small",
         if (n == 0) "All required fields filled"
         else paste(n, if (n == 1) "required field left" else "required fields left"))
  })
  
  refs <- reactive({
    p <- vapply(list(input$project_code, input$budget_line, input$country,
                     input$location, input$department), v, "")
    base_ok <- all(nzchar(p))
    of <- if (base_ok && has_date(input$of_date))
      paste0("OF/", paste(p, collapse = "/"), "/", fmt(input$of_date))
    else NA_character_
    po <- if (base_ok && nzchar(v(input$reason)) && has_date(input$po_date))
      paste0("PO/OF/", paste(p, collapse = "/"), "/", v(input$reason),
             "/", fmt(input$po_date))
    else NA_character_
    list(of = of, po = po)
  })
  
  output$preview <- renderUI({
    r   <- refs()
    cur <- v(input$currency)
    amt <- input$amount
    shown <- function(x) if (is.na(x)) "—" else x
    top <- if (!nzchar(cur) || is.null(amt) || is.na(amt)) {
      p(class = "muted", "Enter amount and currency to see the scenario.")
    } else {
      usd <- amt / RATES[[cur]]
      s   <- scenario(usd, v(input$waiver))
      tagList(
        p(span(paste("Scenario", s), class = "badge badge-scen"), " ",
          span(SCEN_NAME[[s]], class = "muted")),
        tags$table(class = "table table-sm",
                   lapply(CUR, function(k)
                     tags$tr(tags$td(k, class = "muted"),
                             tags$td(format(round(usd * RATES[[k]], 2), big.mark = ",",
                                            nsmall = 2), style = "text-align:right"))))
      )
    }
    div(top,
        p(class = "muted mb-0", "Order form"), p(shown(r$of), class = "ref"),
        p(class = "muted mb-0", "Purchase order"), p(shown(r$po), class = "ref"))
  })
  
  clear_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    for (id in names(CACHE)) updateSelectizeInput(session, id, selected = "")
    for (id in c("currency", "waiver", "status"))
      updateSelectInput(session, id, selected = "")
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    for (id in c("of_date", "po_date", "payment_date"))
      updateDateInput(session, id, value = NA)
    updateTextAreaInput(session, "remarks", value = "")
  }
  observeEvent(input$clear, clear_form())
  
  observeEvent(input$add, {
    if (!all(required())) return()   # button is disabled, this is a safety net
    r   <- refs()
    pay <- if (input$status == "paid") as.character(input$payment_date)
    else NA_character_
    qty <- if (is.null(input$quantity) || is.na(input$quantity)) NA_real_
    else input$quantity
    c <- con(); on.exit(dbDisconnect(c))
    dbExecute(c, "INSERT INTO expenses(description,project_code,budget_line,
      country,location,department,reason,order_form,po,requested_by,
      submitted_by,status,waiver,supplier,unit,quantity,amount,currency,
      payment_date,remarks,created)
      VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
              params = list(v(input$description), v(input$project_code),
                            v(input$budget_line), v(input$country), v(input$location),
                            v(input$department), nz(input$reason), r$of, r$po,
                            v(input$requested_by), v(input$submitted_by), input$status,
                            input$waiver, nz(input$supplier), nz(input$unit), qty, input$amount,
                            input$currency, pay, nz(input$remarks), as.character(Sys.time())))
    tick(tick() + 1)
    clear_form()
    showNotification("Request added.")
  })
  
  # status update: select a row, then "Update status"
  observeEvent(input$edit, {
    i <- input$table_rows_selected
    if (!length(i)) {
      showNotification("Select a row in the table first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = paste("Update request #", filtered()$id[i]),
      selectInput("m_status", "Status", STATUS, selected = filtered()$status[i]),
      conditionalPanel("input.m_status == 'paid'",
                       dateInput("m_date", "Date of payment", value = Sys.Date(),
                                 format = "dd-mm-yyyy")),
      footer = tagList(modalButton("Cancel"),
                       actionButton("m_save", "Save", class = "btn-primary")),
      easyClose = TRUE, size = "s"))
  })
  
  observeEvent(input$m_save, {
    id  <- filtered()$id[input$table_rows_selected]
    pay <- if (input$m_status == "paid") as.character(input$m_date)
    else NA_character_
    c <- con(); on.exit(dbDisconnect(c))
    dbExecute(c, "UPDATE expenses SET status=?, payment_date=? WHERE id=?",
              params = list(input$m_status, pay, as.integer(id)))
    removeModal(); tick(tick() + 1)
  })
  
  output$totals <- renderUI({
    d <- filtered()
    boxes <- lapply(CUR, function(k)
      value_box(k, format(sum(d[[k]]), big.mark = ",", nsmall = 2),
                theme = "primary"))
    do.call(layout_columns, c(list(col_widths = c(3, 3, 3, 3)), boxes))
  })
  
  SHOW <- c("id", "description", "project_code", "budget_line", "requested_by",
            "submitted_by", "status", "scenario", "waiver", "supplier",
            "amount", "currency", "payment_date", "order_form", "po", "remarks")
  
  output$table <- renderDT({
    d <- filtered(); req(nrow(d))
    datatable(d[, SHOW], rownames = FALSE, filter = "top", selection = "single",
              colnames = gsub("_", " ", SHOW),
              options = list(
                scrollX = TRUE, pageLength = 10, initComplete = more_init,
                columnDefs = list(list(
                  targets = c(match("description", SHOW) - 1,
                              match("remarks", SHOW) - 1),
                  render = more_render))))
  })
  
  summarise_by <- function(key) {
    d <- filtered(); req(nrow(d))
    d |> group_by(.data[[key]]) |>
      summarise(across(all_of(CUR), ~ round(sum(.x), 2)), .groups = "drop") |>
      datatable(rownames = FALSE, selection = "none",
                colnames = c(gsub("_", " ", key), CUR),
                options = list(dom = "t"))
  }
  output$by_person  <- renderDT(summarise_by("requested_by"))
  output$by_project <- renderDT(summarise_by("project_code"))
  
  output$export <- downloadHandler(
    filename = "expenses.xlsx",
    content  = function(file) write_xlsx(filtered(), file)
  )
}

shinyApp(ui, server)