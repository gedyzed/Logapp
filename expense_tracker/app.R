library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(writexl)
library(ggplot2)

options(scipen = 999)          # never show scientific notation

DB     <- "tracker.db"
CUR    <- c("ETB", "USD", "EUR", "GBP")
STATUS <- c("waiting", "under validation", "paid")
WAIVER <- c("No", "Framework agreement")
DEPTS  <- c("REACH", "PANDA", "ACTED")

# values the new-request form starts with
DEFAULTS <- list(submitted_by = "Betty T", status = "waiting",
                 currency = "ETB", country = "ETH")

SCEN_NAME <- c(
  A = "Direct purchase", B = "Quotations", C = "Contract",
  D = "National/Local call for tender", E = "International call for tender",
  G = "Framework agreement"
)
LEAD <- c(A = "< 1 week", B = "~2-3 weeks", C = "~2-3 weeks",
          D = "~1.5 months", E = "~2 months", G = "~2 months")

# Units per 1 USD. Edit here when rates change (placeholders for now).
RATES <- c(ETB = 155, USD = 1, EUR = 0.92, GBP = 0.79)

`%||%` <- function(a, b) if (is.null(a)) b else a

# A <500 | B 500-<5k | C 5k-<30k | D 30k-<150k | E >=150k | G if framework
scenario <- function(usd, waiver) {
  base <- as.character(cut(usd, c(-Inf, 500, 5000, 30000, 150000, Inf),
                           labels = c("A", "B", "C", "D", "E"), right = FALSE))
  as.character(ifelse(waiver == "Framework agreement", "G", base))
}

# ---- number formatting -----------------------------------------------------
abbr <- function(x) {   # 1,250,000 -> 1.25M
  tr <- function(y, suf) paste0(sub("\\.?0+$", "", formatC(y, format = "f", digits = 2)), suf)
  ifelse(abs(x) >= 1e9, tr(x / 1e9, "B"),
  ifelse(abs(x) >= 1e6, tr(x / 1e6, "M"),
  ifelse(abs(x) >= 1e3, tr(x / 1e3, "K"), tr(x, ""))))
}
money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

# ---- database ---------------------------------------------------------------
con <- function() {
  c <- dbConnect(SQLite(), DB)
  dbExecute(c, "CREATE TABLE IF NOT EXISTS expenses(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    description TEXT, project_code TEXT, budget_line TEXT,
    country TEXT, location TEXT, department TEXT, reason TEXT,
    order_form TEXT, po TEXT, requested_by TEXT, submitted_by TEXT,
    status TEXT, waiver TEXT, supplier TEXT, unit TEXT, quantity REAL,
    amount REAL, currency TEXT, order_date TEXT, po_date TEXT,
    payment_date TEXT, remarks TEXT, created TEXT, updated TEXT)")
  c
}
now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")

fmt <- function(d) format(d, "%d-%m-%Y")
v   <- function(x) if (is.null(x) || !length(x) || is.na(x)) "" else trimws(x)
nz  <- function(x) if (nzchar(v(x))) v(x) else NA_character_
has_date <- function(d) !is.null(d) && length(d) == 1 && !is.na(d)
has_num  <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
uniq <- function(x) { x <- as.character(x); sort(unique(x[!is.na(x) & nzchar(x)])) }
with_sel <- function(ch, sel) { miss <- setdiff(sel, ch); c(ch, setNames(miss, miss)) }

date_in <- function(id, label, value = NA, ...)
  suppressWarnings(dateInput(id, label, value = value, format = "dd-mm-yyyy", ...))

# short fields whose dropdowns are filled from the shared database
CACHE <- list(
  requested_by = c("requested_by", "submitted_by"),
  submitted_by = c("requested_by", "submitted_by"),
  project_code = "project_code", budget_line = "budget_line",
  country = "country", location = "location",
  supplier = "supplier", unit = "unit", reason = "reason"
)
cache_choices <- function(d, id) uniq(unlist(d[CACHE[[id]]], use.names = FALSE))

# ---- UI helpers ---------------------------------------------------------------
R   <- function(x) tagList(x, span("required", class = "tag tag-req"))
OPT <- function(x) tagList(x, span("optional", class = "tag"))
col <- function(n, ...) div(class = paste0("c", n), ...)

cached <- function(id, label) {
  d <- DEFAULTS[[id]]
  selectizeInput(id, R(label), choices = d, selected = d, width = "100%",
                 options = list(create = TRUE, placeholder = "Type or pick"))
}
pick <- function(id, label, choices)
  selectInput(id, R(label), c("Select" = "", choices),
              selected = DEFAULTS[[id]] %||% "", width = "100%")

theme <- bs_theme(version = 5, primary = "#1d4ed8", "border-radius" = "0.6rem")

css <- "
body{font-family:system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}
.bslib-grid>*{min-width:0}
.muted{color:var(--bs-secondary-color)}
.ref{font-family:ui-monospace,Menlo,monospace;font-size:.8rem;word-break:break-all}
.badge-scen{font-size:1rem;background:var(--bs-primary)}
.sticky{position:sticky;top:1rem}

/* ---- brand + main tabs ---- */
.navbar{border-bottom:2px solid var(--bs-primary)}
a.brand{font-size:1.4rem;font-weight:800;letter-spacing:-.02em;
  color:var(--bs-primary)!important;text-decoration:none}
.navbar .nav-link[data-value='new'],.navbar .nav-link[data-value='dash']{
  display:inline-flex;align-items:center;gap:.4rem;line-height:1.4;
  min-height:2.2rem;font-weight:700;border:1.5px solid var(--bs-primary);
  border-radius:999px;padding:.3rem 1.05rem!important;
  margin:.2rem .5rem .2rem 0;color:var(--bs-primary)!important}
.navbar .nav-link[data-value='new']{background:rgba(var(--bs-primary-rgb),.14)}
.navbar .nav-link.active[data-value='new'],.navbar .nav-link.active[data-value='dash']{
  background:var(--bs-primary);color:#fff!important}
@media(max-width:991.98px){
  .navbar .navbar-nav{gap:.35rem;padding:.4rem 0}
  .navbar .nav-link[data-value='new'],.navbar .nav-link[data-value='dash']{
    margin:0;width:100%}
}

/* ---- buttons: secondary ones stay clearly visible ---- */
.btn-outline-secondary,.btn-outline-primary{border-width:1.5px;font-weight:600}
.btn-outline-secondary{
  color:var(--bs-body-color);border-color:var(--bs-secondary-color);background:transparent}
.btn-outline-secondary:hover,.btn-outline-secondary:focus{
  background:var(--bs-secondary-bg);color:var(--bs-body-color);
  border-color:var(--bs-body-color)}

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
  border-radius:.5rem;padding:.5rem .75rem;box-shadow:none;
  transition:border-color .15s,box-shadow .15s}
.nr input.form-control,.nr .form-select{height:2.7rem}
.nr .selectize-input{min-height:2.7rem;display:flex;align-items:center;flex-wrap:wrap}
.nr textarea.form-control{height:auto;resize:vertical}
.nr .form-control:focus,.nr .form-select:focus,.nr .selectize-input.focus{
  border-color:var(--bs-primary)!important;background:var(--bs-body-bg)!important;
  box-shadow:0 0 0 3px rgba(var(--bs-primary-rgb),.18)!important}
.nr .req-bad .control-label{color:#dc2626}
.nr .req-bad .form-control,.nr .req-bad .form-select,.nr .req-bad .selectize-input{
  border-color:#dc2626!important;background:rgba(220,38,38,.07)!important;
  box-shadow:0 0 0 3px rgba(220,38,38,.14)!important}
.tag{font-size:.6rem;text-transform:uppercase;letter-spacing:.05em;
  margin-left:.45rem;font-weight:500;color:var(--bs-secondary-color);opacity:.8}
.tag-req{color:#dc2626;opacity:1}

.action-bar{position:sticky;bottom:0;z-index:5;display:flex;align-items:center;
  justify-content:flex-end;gap:.75rem;padding:.85rem 0;margin-top:.5rem;
  background:var(--bs-body-bg);border-top:1px solid var(--bs-border-color)}
.action-bar .btn{text-transform:uppercase;letter-spacing:.06em;
  font-size:.78rem;padding:.55rem 1.5rem}

/* ---- dashboard toolbar + filter dropdown ---- */
.toolbar{display:flex;align-items:center;flex-wrap:wrap;gap:.6rem;margin-bottom:1rem}
.fmenu{width:min(92vw,540px)}
.fmenu .fgrid{display:grid;grid-template-columns:1fr 1fr;gap:0 .9rem}
@media(max-width:520px){.fmenu .fgrid{grid-template-columns:1fr}}
.fmenu .shiny-input-container{margin-bottom:.65rem;width:100%}
.fmenu .control-label{font-size:.72rem;font-weight:600;color:var(--bs-secondary-color)}
.chart-head{display:flex;align-items:center;gap:1rem;margin:.25rem 0 .5rem}
.chart-head .shiny-input-container{margin:0}
.chart-head label.radio-inline{margin-right:.9rem}
.card-head{display:flex;align-items:center;justify-content:space-between;
  gap:1rem;flex-wrap:wrap;width:100%}
.card-head .btns{display:flex;gap:.5rem}

/* ---- tables: compact rows, visible search + paging ---- */
table.dataTable th,table.dataTable td{white-space:nowrap}
a.dmore{color:var(--bs-primary);font-weight:600;font-size:.8rem;text-decoration:none}
.dt-top{display:flex;justify-content:flex-end;margin-bottom:.6rem}
.dt-foot{display:flex;justify-content:space-between;align-items:center;
  flex-wrap:wrap;gap:.75rem;margin-top:.9rem}
div.dataTables_wrapper div.dataTables_filter{margin:0}
div.dataTables_wrapper div.dataTables_filter input{
  min-width:280px;margin-left:0!important;padding:.5rem .85rem;border-radius:.5rem;
  border:1.5px solid var(--bs-primary)!important;background:var(--bs-body-bg)!important;
  color:var(--bs-body-color);outline:none;box-shadow:none}
div.dataTables_wrapper div.dataTables_filter input:focus{
  box-shadow:0 0 0 3px rgba(var(--bs-primary-rgb),.22)!important}
.dataTables_wrapper thead input,.dataTables_wrapper thead .selectize-input{
  border:1px solid var(--bs-border-color);border-radius:.4rem;
  background:var(--bs-body-bg);color:var(--bs-body-color)}
/* paging: one clean bordered button per page, no nested boxes */
.dataTables_wrapper .dataTables_paginate{margin:0}
.dataTables_wrapper .dataTables_paginate ul.pagination{gap:.35rem;margin:0}
.dataTables_wrapper .dataTables_paginate .paginate_button{
  border:0!important;padding:0!important;margin:0!important;background:transparent!important}
.dataTables_wrapper .dataTables_paginate .page-link{
  border:1.5px solid var(--bs-border-color)!important;border-radius:.5rem!important;
  font-weight:700;padding:.3rem .9rem;color:var(--bs-body-color)!important;
  background:transparent!important}
.dataTables_wrapper .dataTables_paginate .page-link:hover{
  border-color:var(--bs-primary)!important;color:var(--bs-primary)!important}
.dataTables_wrapper .dataTables_paginate .active .page-link,
.dataTables_wrapper .dataTables_paginate .paginate_button.current{
  background:var(--bs-primary)!important;border-color:var(--bs-primary)!important;
  color:#fff!important}
.dataTables_wrapper .dataTables_paginate .disabled .page-link{opacity:.4}

/* ---- notifications: success = green, warning = amber, error = red ---- */
.shiny-notification{opacity:1;border:0;border-radius:.6rem;font-weight:600;
  box-shadow:0 6px 20px rgba(0,0,0,.25)}
.shiny-notification-message{background:#16a34a!important;color:#fff!important}
.shiny-notification-warning{background:#f59e0b!important;color:#1f2937!important}
.shiny-notification-error{background:#dc2626!important;color:#fff!important}
.shiny-notification-close{color:inherit!important}
"

js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('mark', function(m) {
    $('.req-bad').removeClass('req-bad');
    (m.ids || []).forEach(function(id) {
      $('#' + id).closest('.shiny-input-container').addClass('req-bad');
    });
  });
  Shiny.addCustomMessageHandler('scrollbad', function(m) {
    setTimeout(function() {
      var el = $('.req-bad').first();
      if (el.length) el[0].scrollIntoView({behavior: 'smooth', block: 'center'});
    }, 80);
  });
  Shiny.addCustomMessageHandler('badge', function(m) {
    $('#fcount').text(m.n).css('display', m.n > 0 ? 'inline-block' : 'none');
  });
});
// a field counts as 'touched' once the user leaves it
$(document).on('focusout', '.nr .shiny-input-container', function() {
  var id = this.id || $(this).find('select,input,textarea').first().attr('id');
  if (!id) return;
  setTimeout(function() {
    Shiny.setInputValue('touched', id, {priority: 'event'});
  }, 300);
});
// keep the filter dropdown open while picking dates / options
$(document).on('hide.bs.dropdown', '.filter-dd', function(e) {
  var ce = e.clickEvent;
  if (ce && $(ce.target).closest('.datepicker, .datepicker-dropdown, .selectize-dropdown').length)
    e.preventDefault();
});
// copy button in the share dialog
$(document).on('click', '#copy_link', function() {
  var t = document.getElementById('share_url');
  t.select();
  if (navigator.clipboard) navigator.clipboard.writeText(t.value);
  else document.execCommand('copy');
  $(this).text('Copied');
});
"

# first 5 words in the row; 'see more' opens the full text in a popup
more_render <- function(title) JS(sub("TITLE", title, fixed = TRUE, r"---(
function(data, type, row) {
  if (type !== 'display' || data === null || data === undefined) return data;
  var esc = function(s) {
    return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;')
                    .replace(/>/g,'&gt;').replace(/"/g,'&quot;');
  };
  var words = String(data).trim().split(/\s+/);
  if (words.length <= 5) return esc(data);
  return esc(words.slice(0,5).join(' ')) + '\u2026 <a href="#" class="dmore" ' +
         'data-title="TITLE #' + row[0] + '" data-full="' + esc(data) +
         '">see more</a>';
}
)---"))

more_init <- JS(r"---(
function(settings) {
  $(this.api().table().node()).on('click', 'a.dmore', function(e) {
    e.preventDefault(); e.stopPropagation();
    Shiny.setInputValue('see_more',
      {title: $(this).attr('data-title'), text: $(this).attr('data-full'),
       nonce: Math.random()}, {priority: 'event'});
  });
}
)---")

dt_language <- list(
  search = "", searchPlaceholder = "Search all columns...",
  paginate = list(previous = "\u2190", `next` = "\u2192"),
  emptyTable = "No requests yet", zeroRecords = "No matching requests")

ui <- function(request) {
  page_navbar(
    id = "nav", selected = "new", fillable = FALSE,
    title = tags$a(href = ".", class = "brand", "Expense Tracker"),
    window_title = "Expense Tracker",
    theme = theme,
    header = tagList(tags$style(css), tags$script(HTML(js))),

    # ------------------------------------------------------------ New request
    nav_panel(
      title = tagList(icon("plus"), "New request"), value = "new",
      layout_columns(
        fill = FALSE, col_widths = c(8, 4),
        div(class = "nr",
          h2("New request", class = "nr-title"),
          p("Every field is required except remarks. Missing fields turn red.",
            class = "nr-sub"),
          div(class = "fg",
            col(12, textAreaInput("description", R("Description"), rows = 3,
                                  width = "100%")),
            col(6, cached("requested_by", "Requested by")),
            col(6, cached("submitted_by", "Submitted by (focal point)")),
            col(4, cached("project_code", "Project code")),
            col(4, cached("budget_line",  "Budget line")),
            col(4, pick("department", "Department", DEPTS)),
            col(4, cached("country",  "Country")),
            col(4, cached("location", "Location")),
            col(4, date_in("of_date", R("Order form date"), Sys.Date(), width = "100%")),
            col(6, cached("supplier", "Supplier")),
            col(3, numericInput("quantity", R("Quantity"), NA, min = 0,
                                width = "100%")),
            col(3, cached("unit", "Unit")),
            col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
            col(4, pick("currency", "Currency", CUR)),
            col(4, pick("waiver",   "Memo / waiver", WAIVER)),
            col(4, pick("status", "Status", STATUS)),
            tagAppendAttributes(
              conditionalPanel("input.status == 'paid'",
                date_in("payment_date", R("Date of payment"), Sys.Date(),
                        width = "100%")),
              class = "c4"),
            col(4, cached("reason", "PO reason")),
            col(4, date_in("po_date", R("PO date"), Sys.Date(), width = "100%")),
            col(12, textAreaInput("remarks", OPT("Remarks"), rows = 2,
                                  width = "100%"))
          ),
          div(class = "action-bar",
              actionButton("clear", "Clear", class = "btn-outline-secondary"),
              actionButton("add", "Add request", class = "btn-primary"))
        ),
        div(class = "sticky",
          card(fill = FALSE, card_header("Summary"), uiOutput("preview")))
      )
    ),

    # -------------------------------------------------------------- Dashboard
    nav_panel(
      title = tagList(icon("chart-bar"), "Dashboard"), value = "dash",
      div(class = "toolbar",
        div(class = "dropdown filter-dd",
          tags$button(class = "btn btn-outline-secondary", type = "button",
                      `data-bs-toggle` = "dropdown",
                      `data-bs-auto-close` = "outside", `aria-expanded` = "false",
                      icon("filter"), " Filters",
                      span(id = "fcount", class = "badge text-bg-primary ms-2",
                           style = "display:none")),
          div(class = "dropdown-menu p-3 fmenu",
            div(class = "fgrid",
              date_in("f_from", "From"),
              date_in("f_to",   "To"),
              selectizeInput("f_week", "Week", NULL, multiple = TRUE,
                             options = list(placeholder = "All")),
              selectInput("f_status", "Status", c("All" = "", STATUS)),
              selectizeInput("f_project", "Project", NULL, multiple = TRUE,
                             options = list(placeholder = "All")),
              selectizeInput("f_person", "Requested by", NULL, multiple = TRUE,
                             options = list(placeholder = "All")),
              selectizeInput("f_submitter", "Submitted by", NULL, multiple = TRUE,
                             options = list(placeholder = "All"))
            )
          )
        ),
        actionButton("reset", "Clear filters", class = "btn-outline-secondary"),
        div(class = "ms-auto",
            actionButton("share", "Share link", icon = icon("link"),
                         class = "btn-outline-secondary"))
      ),
      uiOutput("totals"),
      card(fill = FALSE, class = "mb-3",
        card_header(div(class = "card-head",
          span("Requests"),
          div(class = "btns",
              actionButton("edit", "Update status", icon = icon("pen"),
                           class = "btn-outline-secondary btn-sm"),
              downloadButton("export", "Export",
                             class = "btn-outline-primary btn-sm")))),
        DTOutput("table", fill = FALSE)
      ),
      div(class = "chart-head",
          strong("Totals in"),
          radioButtons("chart_cur", NULL, CUR, selected = "USD", inline = TRUE)),
      layout_columns(
        fill = FALSE, col_widths = c(6, 6),
        navset_card_tab(title = "By requester",
          nav_panel("Chart", plotOutput("plot_person", height = "280px")),
          nav_panel("Table", DTOutput("by_person", fill = FALSE))),
        navset_card_tab(title = "By project",
          nav_panel("Chart", plotOutput("plot_project", height = "280px")),
          nav_panel("Table", DTOutput("by_project", fill = FALSE)))
      )
    ),

    nav_spacer(),
    nav_item(input_dark_mode(id = "mode"))
  )
}

server <- function(input, output, session) {
  touched  <- reactiveVal(character())
  show_all <- reactiveVal(FALSE)

  # ---- shared data: re-read from SQLite whenever any user changes it ----
  db_poll <- reactivePoll(
    1000, session,
    checkFunc = function() {
      c <- con(); on.exit(dbDisconnect(c))
      paste(unlist(dbGetQuery(c, "SELECT COUNT(*), COALESCE(MAX(id),0),
                                  COALESCE(MAX(updated),'') FROM expenses")),
            collapse = "|")
    },
    valueFunc = function() {
      c <- con(); on.exit(dbDisconnect(c))
      dbGetQuery(c, "SELECT * FROM expenses ORDER BY id DESC")
    })

  data <- reactive({
    d <- db_poll()
    usd <- d$amount / unname(RATES[d$currency])
    for (k in CUR) d[[k]] <- round(usd * RATES[[k]], 2)
    d$scenario      <- scenario(usd, d$waiver)
    d$expected_time <- unname(LEAD[d$scenario])
    d$week          <- format(as.Date(d$order_date), "%G-W%V")
    d
  })

  week_choices <- function(d) {
    d <- d[!is.na(d$week), , drop = FALSE]
    if (!nrow(d)) return(character())
    s <- d |> group_by(week) |>
      summarise(first = min(as.Date(order_date)), .groups = "drop") |>
      arrange(desc(week))
    mon <- s$first - (as.integer(format(s$first, "%u")) - 1L)
    setNames(s$week, paste0(s$week, " (from ", format(mon, "%d %b"), ")"))
  }

  # dropdown values come from the database, not from this user's history
  observe({
    d <- data()
    keep <- function(id) { x <- isolate(input[[id]]); if (is.null(x)) "" else x }
    for (id in names(CACHE)) {
      cur <- keep(id)
      ch  <- union(cache_choices(d, id), cur[nzchar(cur)])
      updateSelectizeInput(session, id, choices = c("", ch), selected = cur,
                           options = list(create = TRUE,
                                          placeholder = "Type or pick"))
    }
    updateSelectizeInput(session, "f_project", choices = uniq(d$project_code),
                         selected = isolate(input$f_project))
    updateSelectizeInput(session, "f_person", choices = uniq(d$requested_by),
                         selected = isolate(input$f_person))
    updateSelectizeInput(session, "f_submitter", choices = uniq(d$submitted_by),
                         selected = isolate(input$f_submitter))
    updateSelectizeInput(session, "f_week", choices = week_choices(d),
                         selected = isolate(input$f_week))
  })

  # ---- filters ----
  filtered <- reactive({
    d <- data()
    if (has_date(input$f_from))
      d <- filter(d, !is.na(order_date), order_date >= as.character(input$f_from))
    if (has_date(input$f_to))
      d <- filter(d, !is.na(order_date), order_date <= as.character(input$f_to))
    if (length(input$f_week))      d <- filter(d, week %in% input$f_week)
    if (length(input$f_project))   d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_person))    d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_submitter)) d <- filter(d, submitted_by %in% input$f_submitter)
    if (nzchar(v(input$f_status))) d <- filter(d, status == input$f_status)
    d
  })

  clear_date <- function(id) suppressWarnings(updateDateInput(session, id, value = NA))

  observeEvent(input$reset, {
    clear_date("f_from"); clear_date("f_to")
    for (id in c("f_week", "f_project", "f_person", "f_submitter"))
      updateSelectizeInput(session, id, selected = character(0))
    updateSelectInput(session, "f_status", selected = "")
  })

  # number of active filters, shown on the Filters button
  observe({
    n <- sum(has_date(input$f_from), has_date(input$f_to),
             length(input$f_week) > 0, length(input$f_project) > 0,
             length(input$f_person) > 0, length(input$f_submitter) > 0,
             nzchar(v(input$f_status)))
    session$sendCustomMessage("badge", list(n = n))
  })

  # ---- short share link: only the selected filters ----
  share_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    q <- list(
      v  = "d",
      f  = if (has_date(input$f_from)) as.character(input$f_from),
      t  = if (has_date(input$f_to))   as.character(input$f_to),
      w  = if (length(input$f_week))      enc(input$f_week),
      p  = if (length(input$f_project))   enc(input$f_project),
      r  = if (length(input$f_person))    enc(input$f_person),
      s  = if (length(input$f_submitter)) enc(input$f_submitter),
      st = if (nzchar(v(input$f_status))) enc(input$f_status))
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname,
                   if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }

  observeEvent(input$share, {
    showModal(modalDialog(
      title = "Share this view",
      p(class = "muted",
        "Anyone who opens this link sees the dashboard with the filters ",
        "selected right now."),
      div(class = "input-group",
        tags$input(id = "share_url", class = "form-control", readonly = NA,
                   value = share_url(), onclick = "this.select()"),
        tags$button(id = "copy_link", type = "button", class = "btn btn-primary",
                    "Copy")),
      footer = modalButton("Close"), easyClose = TRUE))
  })

  # opening a shared link: apply its filters and jump to the dashboard
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q)) return()
    d <- isolate(data())
    vals <- function(k) if (is.null(q[[k]]) || !nzchar(q[[k]])) character()
                        else strsplit(q[[k]], "|", fixed = TRUE)[[1]]
    as_d <- function(x) tryCatch(as.Date(x), error = function(e) NA)
    if (!is.null(q$f) && !is.na(as_d(q$f)))
      suppressWarnings(updateDateInput(session, "f_from", value = as_d(q$f)))
    if (!is.null(q$t) && !is.na(as_d(q$t)))
      suppressWarnings(updateDateInput(session, "f_to", value = as_d(q$t)))
    set <- function(id, key, ch) {
      s <- vals(key)
      if (length(s)) updateSelectizeInput(session, id, choices = with_sel(ch, s),
                                          selected = s)
    }
    set("f_week",      "w", week_choices(d))
    set("f_project",   "p", uniq(d$project_code))
    set("f_person",    "r", uniq(d$requested_by))
    set("f_submitter", "s", uniq(d$submitted_by))
    if (!is.null(q$st) && q$st %in% STATUS)
      updateSelectInput(session, "f_status", selected = q$st)
    nav_select("nav", "dash", session = session)
  }, once = TRUE)

  # ---- required fields, by input id (TRUE = filled) ----
  valid <- reactive(c(
    description  = nzchar(v(input$description)),
    requested_by = nzchar(v(input$requested_by)),
    submitted_by = nzchar(v(input$submitted_by)),
    project_code = nzchar(v(input$project_code)),
    budget_line  = nzchar(v(input$budget_line)),
    department   = nzchar(v(input$department)),
    country      = nzchar(v(input$country)),
    location     = nzchar(v(input$location)),
    of_date      = has_date(input$of_date),
    supplier     = nzchar(v(input$supplier)),
    quantity     = has_num(input$quantity),
    unit         = nzchar(v(input$unit)),
    amount       = has_num(input$amount),
    currency     = nzchar(v(input$currency)),
    waiver       = nzchar(v(input$waiver)),
    status       = nzchar(v(input$status)),
    payment_date = !identical(input$status, "paid") || has_date(input$payment_date),
    reason       = nzchar(v(input$reason)),
    po_date      = has_date(input$po_date)
  ))

  observeEvent(input$touched, touched(union(touched(), input$touched)))

  # red = required but empty (after leaving the field, or after pressing Add)
  observe({
    bad  <- names(valid())[!valid()]
    show <- if (show_all()) bad else intersect(bad, touched())
    session$sendCustomMessage("mark", list(ids = as.list(show)))
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
    top <- if (!nzchar(cur) || !has_num(amt)) {
      p(class = "muted", "Enter amount and currency to see the scenario.")
    } else {
      usd <- amt / RATES[[cur]]
      s   <- scenario(usd, v(input$waiver))
      tagList(
        p(span(paste("Scenario", s), class = "badge badge-scen"), " ",
          span(SCEN_NAME[[s]], class = "muted")),
        p(class = "mb-2", tags$b("Expected time: "), LEAD[[s]]),
        tags$table(class = "table table-sm",
          lapply(CUR, function(k)
            tags$tr(tags$td(k, class = "muted"),
                    tags$td(money(usd * RATES[[k]]), style = "text-align:right"))))
      )
    }
    div(top,
        p(class = "muted mb-0", "Order form"), p(shown(r$of), class = "ref"),
        p(class = "muted mb-0", "Purchase order"), p(shown(r$po), class = "ref"))
  })

  # back to the starting values (defaults prefilled, the rest empty)
  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    d <- isolate(data())
    for (id in names(CACHE)) {
      val <- DEFAULTS[[id]] %||% ""
      ch  <- union(cache_choices(d, id), val[nzchar(val)])
      updateSelectizeInput(session, id, choices = c("", ch), selected = val,
                           options = list(create = TRUE,
                                          placeholder = "Type or pick"))
    }
    for (id in c("department", "currency", "waiver", "status"))
      updateSelectInput(session, id, selected = DEFAULTS[[id]] %||% "")
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    for (id in c("of_date", "po_date", "payment_date"))
      suppressWarnings(updateDateInput(session, id, value = Sys.Date()))
    updateTextAreaInput(session, "remarks", value = "")
    touched(character()); show_all(FALSE)
  }
  observeEvent(input$clear, reset_form())

  observeEvent(input$add, {
    if (!all(valid())) {      # button stays active: show what is missing instead
      show_all(TRUE)
      session$sendCustomMessage("scrollbad", list())
      showNotification("Some required fields are empty.", type = "warning")
      return()
    }
    r   <- refs()
    row <- list(
      description = v(input$description), project_code = v(input$project_code),
      budget_line = v(input$budget_line), country = v(input$country),
      location = v(input$location), department = v(input$department),
      reason = v(input$reason), order_form = r$of, po = r$po,
      requested_by = v(input$requested_by), submitted_by = v(input$submitted_by),
      status = input$status, waiver = input$waiver, supplier = v(input$supplier),
      unit = v(input$unit), quantity = input$quantity, amount = input$amount,
      currency = input$currency, order_date = as.character(input$of_date),
      po_date = as.character(input$po_date),
      payment_date = if (input$status == "paid") as.character(input$payment_date)
                     else NA_character_,
      remarks = nz(input$remarks), created = now(), updated = now())
    c <- con(); on.exit(dbDisconnect(c))
    dbExecute(c, sprintf("INSERT INTO expenses(%s) VALUES(%s)",
                         paste(names(row), collapse = ","),
                         paste(rep("?", length(row)), collapse = ",")),
              params = unname(row))
    reset_form()
    showNotification("Request added.", type = "message")
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
        date_in("m_date", "Date of payment", Sys.Date())),
      footer = tagList(modalButton("Cancel"),
                       actionButton("m_save", "Save", class = "btn-primary")),
      easyClose = TRUE, size = "s"))
  })

  observeEvent(input$m_save, {
    id  <- filtered()$id[input$table_rows_selected]
    pay <- if (input$m_status == "paid") as.character(input$m_date)
           else NA_character_
    c <- con(); on.exit(dbDisconnect(c))
    dbExecute(c, "UPDATE expenses SET status=?, payment_date=?, updated=? WHERE id=?",
              params = list(input$m_status, pay, now(), as.integer(id)))
    removeModal()
    showNotification("Status updated.", type = "message")
  })

  # popup with the full description / remarks
  observeEvent(input$see_more, {
    showModal(modalDialog(
      title = input$see_more$title,
      div(style = "white-space:pre-wrap", input$see_more$text),
      footer = modalButton("Close"), easyClose = TRUE))
  })

  # ---- dashboard ----
  output$totals <- renderUI({
    d <- filtered()
    boxes <- lapply(CUR, function(k)
      value_box(k, abbr(sum(d[[k]])), theme = "primary", fill = FALSE,
                height = "110px"))
    do.call(layout_columns,
            c(list(fill = FALSE, class = "mb-3", col_widths = c(3, 3, 3, 3)), boxes))
  })

  SHOW <- c("id", "description", "project_code", "budget_line", "requested_by",
            "submitted_by", "department", "status", "scenario", "expected_time",
            "supplier", "quantity", "unit", "amount", "currency", "waiver",
            "order_date", "payment_date", "order_form", "po", "remarks")

  output$table <- renderDT({
    d <- filtered()
    datatable(d[, SHOW, drop = FALSE], rownames = FALSE, filter = "top",
              selection = "single", fillContainer = FALSE,
              colnames = gsub("_", " ", SHOW),
              options = list(
                scrollX = TRUE, pageLength = 10,
                dom = "<'dt-top'f>rt<'dt-foot'ip>",
                language = dt_language, initComplete = more_init,
                columnDefs = list(
                  list(targets = match("description", SHOW) - 1,
                       render = more_render("Description")),
                  list(targets = match("remarks", SHOW) - 1,
                       render = more_render("Remarks"))))) |>
      formatRound("amount", 2)
  })

  totals_by <- function(key) {
    d <- filtered()
    d |> group_by(label = .data[[key]]) |>
      summarise(across(all_of(CUR), ~ sum(.x)), .groups = "drop")
  }

  summary_table <- function(key, label)
    datatable(totals_by(key), rownames = FALSE, selection = "none",
              fillContainer = FALSE, colnames = c(label, CUR),
              options = list(dom = "t", language = dt_language)) |>
      formatRound(CUR, 2)

  output$by_person  <- renderDT(summary_table("requested_by", "requested by"))
  output$by_project <- renderDT(summary_table("project_code", "project"))

  bar_chart <- function(key) {
    g <- totals_by(key)
    validate(need(nrow(g) > 0, "No data yet"))
    cur  <- input$chart_cur
    g$total <- g[[cur]]
    g <- head(arrange(g, desc(total)), 10)
    fg <- if (identical(input$mode, "dark")) "#e5e7eb" else "#1f2937"
    ggplot(g, aes(x = reorder(label, total), y = total)) +
      geom_col(fill = "#1d4ed8", width = 0.6) +
      geom_text(aes(label = abbr(total)), hjust = -0.15, colour = fg, size = 3.8) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = abbr) +
      labs(x = NULL, y = NULL, subtitle = paste("Total in", cur)) +
      theme_minimal(base_size = 12) +
      theme(text = element_text(colour = fg),
            axis.text = element_text(colour = fg),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = alpha(fg, 0.12)),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA))
  }
  output$plot_person  <- renderPlot(bar_chart("requested_by"), bg = "transparent", res = 96)
  output$plot_project <- renderPlot(bar_chart("project_code"), bg = "transparent", res = 96)

  output$export <- downloadHandler(
    filename = "expenses.xlsx",
    content  = function(file) write_xlsx(select(filtered(), -week), file)
  )
}

shinyApp(ui, server)