library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(writexl)
library(ggplot2)

options(scipen = 999)          # never show scientific notation

# Resolve the database next to this application by default. Set TRACKER_DB_PATH to use a shared database stored elsewhere.
script_file <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else {
    frames <- sys.frames()
    ofiles <- vapply(frames, function(fr) if (!is.null(fr$ofile)) as.character(fr$ofile) else "", character(1))
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles)) ofiles[[length(ofiles)]] else ""
  }
})
APP_DIR <- if (nzchar(script_file) && file.exists(script_file)) dirname(normalizePath(script_file, mustWork = TRUE)) else normalizePath(getwd(), mustWork = FALSE)
db_override <- Sys.getenv("TRACKER_DB_PATH", unset = "")
if (nzchar(db_override)) {
  override_path <- path.expand(db_override)
  if (!file.exists(override_path)) {
    stop("TRACKER_DB_PATH points to a database file that does not exist: ", override_path,
         ". Check the path instead of silently starting with an empty database.")
  }
  DB <- normalizePath(override_path, mustWork = TRUE)
} else {
  # Prefer an existing production/local database with actual requests. If no populated
  # tracker.db exists, use the separate synthetic demo database shipped with the app.
  # This keeps demo rows out of an existing database and makes a fresh install visual.
  db_candidates <- unique(c(file.path(APP_DIR, "tracker.db"), file.path(getwd(), "tracker.db")))
  existing_db <- db_candidates[file.exists(db_candidates)]
  has_rows <- function(path) {
    tryCatch({
      cn <- DBI::dbConnect(RSQLite::SQLite(), path)
      on.exit(DBI::dbDisconnect(cn), add = TRUE)
      "expenses" %in% DBI::dbListTables(cn) &&
        DBI::dbGetQuery(cn, "SELECT COUNT(*) AS n FROM expenses")$n[[1]] > 0
    }, error = function(e) FALSE)
  }
  populated_db <- existing_db[vapply(existing_db, has_rows, logical(1))]
  demo_db <- file.path(APP_DIR, "tracker_demo.db")
  if (length(populated_db)) {
    DB <- populated_db[[1]]
  } else if (file.exists(demo_db) && file.info(demo_db)$size > 0) {
    DB <- demo_db
    message("No populated tracker.db found; displaying the bundled synthetic demo dataset.")
  } else if (length(existing_db)) {
    DB <- existing_db[[1]]
  } else {
    DB <- file.path(APP_DIR, "tracker.db")
  }
  DB <- normalizePath(DB, mustWork = FALSE)
}
message("Expense Tracker SQLite database: ", DB)
CUR    <- c("ETB", "USD", "EUR", "GBP")
STATUS <- c("waiting", "under validation", "paid", "rejected")
DEPTS  <- c("REACH", "PANDA", "ACTED")

# values the new-request form starts with
DEFAULTS <- list(submitted_by = "Betty T", currency = "ETB", country = "ETH")

SCEN_NAME <- c(
  A = "Direct purchase", B = "Quotations", C = "Contract",
  D = "National/Local call for tender", E = "International call for tender",
  G = "Framework agreement"
)
LEAD <- c(A = "< 1 week", B = "~2-3 weeks", C = "~2-3 weeks",
          D = "~1.5 months", E = "~2 months", G = "~2 months")
# Units per 1 USD. VERIFY these before real use. Each request stores its own rate.
RATES <- c(ETB = 155, USD = 1, EUR = 0.92, GBP = 0.79)

STATUS_COL <- c(waiting = "#94a3b8", `under validation` = "#f59e0b",
                paid = "#16a34a", rejected = "#dc2626")
DIMS <- c(Project = "project_code", Requester = "requested_by",
          Department = "department")

`%||%` <- function(a, b) if (is.null(a)) b else a

month_choices <- function() {
  m <- seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 24)
  setNames(format(m, "%Y-%m"), format(m, "%B %Y"))
}

# A <500 | B 500-<5k | C 5k-<30k | D 30k-<150k | E >=150k | G if framework
scenario <- function(usd, waiver) {
  usd    <- as.numeric(usd)
  waiver <- as.character(waiver)
  base <- as.character(cut(usd, c(-Inf, 500, 5000, 30000, 150000, Inf),
                           labels = c("A", "B", "C", "D", "E"), right = FALSE))
  as.character(ifelse(waiver %in% "Framework agreement", "G", base))
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
  cn <- dbConnect(SQLite(), DB)
  dbGetQuery(cn, "PRAGMA busy_timeout = 5000")
  cn
}

init_db <- function() {
  cn <- con(); on.exit(dbDisconnect(cn))
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS expenses(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    description TEXT, project_code TEXT, budget_line TEXT,
    country TEXT, location TEXT, department TEXT, reason TEXT,
    order_form TEXT, po TEXT, requested_by TEXT, submitted_by TEXT,
    status TEXT, waiver TEXT, supplier TEXT, unit TEXT, quantity REAL,
    amount REAL, currency TEXT, usd REAL, rate_used REAL,
    order_date TEXT, po_date TEXT, payment_date TEXT, remarks TEXT,
    created TEXT, updated TEXT)")
  # upgrade an older database that lacks the two new columns
  for (k in setdiff(c("usd", "rate_used"), dbListFields(cn, "expenses")))
    dbExecute(cn, sprintf("ALTER TABLE expenses ADD COLUMN %s REAL", k))
  dbExecute(cn, "CREATE TABLE IF NOT EXISTS projects(
    project_code TEXT PRIMARY KEY, department TEXT, location TEXT)")
}
init_db()

db_get <- function(sql, ...) {
  cn <- con(); on.exit(dbDisconnect(cn)); a <- list(...)
  dbGetQuery(cn, sql, params = if (length(a)) a)
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

# Order form / PO references. Country is optional and skipped when empty.
mk_ref <- function(r, kind = c("OF", "PO")) {
  kind <- match.arg(kind)
  g <- function(k) v(r[[k]])
  if (!all(nzchar(c(g("project_code"), g("budget_line"), g("location"), g("department")))))
    return(NA_character_)
  parts <- c(g("project_code"), g("budget_line"), g("country"), g("location"), g("department"))
  parts <- paste(parts[nzchar(parts)], collapse = "/")
  if (kind == "OF") {
    d <- suppressWarnings(as.Date(g("order_date")))
    if (is.na(d)) return(NA_character_)
    paste0("OF/", parts, "/", fmt(d))
  } else {
    d <- suppressWarnings(as.Date(g("po_date")))
    if (is.na(d) || !nzchar(g("reason"))) return(NA_character_)
    paste0("PO/OF/", parts, "/", g("reason"), "/", fmt(d))
  }
}

# short fields whose dropdowns are filled from the shared database
CACHE <- list(
  requested_by = c("requested_by", "submitted_by"),
  submitted_by = c("requested_by", "submitted_by"),
  project_code = "project_code", budget_line = "budget_line",
  country = "country", location = "location",
  supplier = "supplier", unit = "unit"
)
cache_choices <- function(d, id) uniq(unlist(d[CACHE[[id]]], use.names = FALSE))

# ---- UI helpers ---------------------------------------------------------------
R   <- function(x) paste0(x, " *")
OPT <- function(x) x
col <- function(n, ...) div(class = paste0("c", n), ...)

cached <- function(id, label, req = TRUE) {
  d <- DEFAULTS[[id]]
  selectizeInput(id, if (req) R(label) else OPT(label), choices = d, selected = d,
                 width = "100%",
                 options = list(create = TRUE, placeholder = "Type or pick"))
}
pick <- function(id, label, choices)
  selectInput(id, R(label), c("Select" = "", choices),
              selected = DEFAULTS[[id]] %||% "", width = "100%")

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#09090b",
  primary = "#0284c7", # shadcn Sky
  secondary = "#f4f4f5", # shadcn Zinc
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius" = "0.6rem"
)

css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
:root{
  --app-bg:#fafafa; --app-card-bg:#ffffff; --app-input-bg:#ffffff;
  --app-border:#d4d4d8; --app-muted:#f4f4f5; --app-muted-fg:#52525b;
  --app-accent:#0284c7; --app-accent-soft:#e0f2fe; --app-radius:.6rem;
}
[data-bs-theme='dark']{
  --app-bg:#09090b; --app-card-bg:#18181b; --app-input-bg:#18181b;
  --app-border:#52525b; --app-muted:#27272a; --app-muted-fg:#d4d4d8;
  --app-accent:#38bdf8; --app-accent-soft:#082f49;
  --bs-body-bg:#09090b; --bs-body-color:#fafafa; --bs-secondary-color:#d4d4d8;
  --bs-tertiary-bg:#27272a; --bs-border-color:#52525b; --bs-card-bg:#18181b;
}
html,body,.navbar,.btn,.form-control,.form-select,.selectize-input,.selectize-dropdown{
  font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
}
body{background:var(--bs-body-bg);color:var(--bs-body-color);font-size:.94rem}
.bslib-grid>*{min-width:0}
.navbar{border-bottom:1px solid var(--app-border);box-shadow:none;background:var(--bs-body-bg)}
.navbar .container-fluid{max-width:1800px}
a.brand{font-weight:700;letter-spacing:-.035em;color:var(--bs-body-color)!important;text-decoration:none;font-size:1.05rem}
.navbar .nav-link[data-value='requests']{font-weight:600;color:var(--bs-body-color)!important}
.navbar .nav-link[data-value='new']{display:none!important}
.navbar .nav-link.active[data-value='requests']{color:var(--app-accent)!important;background:transparent}
.page-shell{width:100%;max-width:1760px;margin:0 auto;padding:1.3rem clamp(1rem,2.2vw,2.5rem) 2.5rem}
.page-heading{display:flex;align-items:center;justify-content:space-between;gap:1rem;flex-wrap:wrap;margin-bottom:1.15rem}
.page-heading h1{font-size:1.55rem;font-weight:650;letter-spacing:-.04em;margin:0}
.page-heading p{color:var(--app-muted-fg);margin:.3rem 0 0;font-size:.9rem}
.toolbar{display:flex;align-items:center;gap:.65rem;flex-wrap:wrap;margin:1.15rem 0 1.2rem}
.toolbar .shiny-input-container{margin:0}
.toolbar .btn{min-height:2.45rem}
.filter-dd .dropdown-menu{width:min(94vw,600px);max-height:75vh;overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);box-shadow:0 14px 35px rgba(0,0,0,.12)}
.fgrid{display:grid;grid-template-columns:1fr 1fr;gap:.15rem .85rem}
.fgrid .wide{grid-column:1/-1}
.fgrid .shiny-input-container{width:100%;margin:.15rem 0 .65rem}
.fgrid .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
@media(max-width:620px){.fgrid{grid-template-columns:1fr}.fgrid .wide{grid-column:auto}}
.btn-primary{--bs-btn-bg:#0284c7;--bs-btn-border-color:#0284c7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1;--bs-btn-active-bg:#075985;font-weight:600;color:#fff!important}
.btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:disabled,[data-bs-theme='dark'] .btn-primary{color:#fff!important}
[data-bs-theme='dark'] .btn-primary{--bs-btn-bg:#0284c7;--bs-btn-border-color:#0284c7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1}
.btn-outline-secondary{border-color:var(--app-border);color:var(--bs-body-color);font-weight:500}
.btn-outline-secondary:hover{background:var(--app-muted);border-color:var(--app-border);color:var(--bs-body-color)}
.card{border:1px solid var(--app-border);border-radius:var(--app-radius);box-shadow:none;background:var(--bs-card-bg,var(--bs-body-bg));min-width:0}
.card-header{padding:1rem 1.15rem;background:transparent;border-bottom:1px solid var(--app-border);font-weight:600}
.card-body{padding:1.1rem 1.15rem}
.metrics-grid{display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:.8rem;margin-bottom:1rem}
@media(max-width:1150px){.metrics-grid{grid-template-columns:repeat(3,minmax(0,1fr))}}
@media(max-width:720px){.metrics-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:440px){.metrics-grid{grid-template-columns:1fr}}
.metric-card{height:100%;padding:1rem 1.1rem;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--bs-body-bg);min-width:0}
.metric-label{font-size:.82rem;font-weight:500;color:var(--app-muted-fg);margin:0 0 .45rem}
.metric-value{font-size:clamp(1.25rem,1.65vw,1.75rem);font-weight:650;letter-spacing:-.045em;line-height:1.15;overflow-wrap:anywhere;margin:0}
.metric-note{font-size:.78rem;color:var(--app-muted-fg);margin:.4rem 0 0}
.chart-card .card-body{padding:.75rem 1rem 1rem}
.chart-note{color:var(--app-muted-fg);font-size:.82rem;margin:-.25rem 0 .5rem}
.table-head{display:flex;align-items:center;justify-content:space-between;gap:.8rem;flex-wrap:wrap}
.table-head .buttons{display:flex;align-items:center;gap:.5rem;flex-wrap:wrap}
.nav-tabs{border-bottom:1px solid var(--app-border);gap:.2rem;flex-wrap:wrap}
.nav-tabs .nav-link{border:0;border-radius:.45rem .45rem 0 0;color:var(--app-muted-fg);font-weight:500;padding:.65rem .9rem}
.nav-tabs .nav-link.active{color:var(--bs-body-color);background:var(--app-muted);border-bottom:2px solid var(--app-accent)}
.tab-content{padding-top:.9rem}
.dataTables_wrapper{width:100%;min-width:0}
table.dataTable{width:100%!important;border-collapse:separate!important;border-spacing:0}
table.dataTable thead th{font-size:.78rem;color:var(--app-muted-fg);font-weight:600;border-bottom:1px solid var(--app-border)!important;background:var(--bs-body-bg)}
table.dataTable tbody td{font-size:.86rem;vertical-align:middle;border-bottom:1px solid var(--app-border)!important;max-width:260px}
table.dataTable tbody tr:hover{background:var(--app-muted)!important}
table.dataTable tbody tr.selected>*{box-shadow:inset 0 0 0 9999px var(--app-accent-soft)!important;color:var(--bs-body-color)!important}
a.dmore{color:var(--app-accent);font-weight:600;font-size:.8rem;text-decoration:none;white-space:nowrap}
.dt-top{display:flex;justify-content:flex-end;margin-bottom:.8rem}
.dt-foot{display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:.75rem;margin-top:.8rem;color:var(--app-muted-fg);font-size:.82rem}
div.dataTables_wrapper div.dataTables_filter{margin:0}
div.dataTables_wrapper div.dataTables_filter input{min-width:min(280px,55vw);margin-left:0!important;padding:.48rem .7rem;border-radius:.5rem;border:1px solid var(--app-border)!important;background:var(--bs-body-bg)!important;color:var(--bs-body-color);outline:none;box-shadow:none}
div.dataTables_wrapper div.dataTables_filter input:focus{border-color:var(--app-accent)!important;box-shadow:0 0 0 3px rgba(2,132,199,.14)!important}
.dataTables_wrapper .dataTables_paginate ul.pagination{gap:.3rem;margin:0}
.dataTables_wrapper .dataTables_paginate .paginate_button{border:0!important;padding:0!important;margin:0!important;background:transparent!important}
.dataTables_wrapper .dataTables_paginate .page-link{border:1px solid var(--app-border)!important;border-radius:.45rem!important;padding:.28rem .7rem;color:var(--bs-body-color)!important;background:transparent!important}
.dataTables_wrapper .dataTables_paginate .active .page-link{background:var(--app-accent)!important;border-color:var(--app-accent)!important;color:#fff!important}
.fg{display:grid;grid-template-columns:repeat(12,minmax(0,1fr));gap:.1rem 1rem}
.c3{grid-column:span 3}.c4{grid-column:span 4}.c5{grid-column:span 5}.c6{grid-column:span 6}.c8{grid-column:span 8}.c12{grid-column:span 12}
@media(max-width:900px){.fg>.c3,.fg>.c4,.fg>.c5{grid-column:span 6}}
@media(max-width:560px){.fg>*{grid-column:span 12!important}}
.nr .control-label{display:block;font-size:.78rem;font-weight:600;margin-bottom:.35rem;color:var(--bs-body-color)}
.nr .shiny-input-container{margin-bottom:.8rem;width:100%}
.nr .form-control,.nr .form-select,.nr .selectize-input{border:1px solid var(--app-border)!important;border-radius:.5rem;background:var(--bs-body-bg)!important;color:var(--bs-body-color);box-shadow:none}
.nr input.form-control,.nr .form-select{min-height:2.65rem}
.nr .selectize-input{min-height:2.65rem;display:flex;align-items:center;flex-wrap:wrap}
.nr textarea.form-control{height:auto;resize:vertical}
.nr .form-control:focus,.nr .form-select:focus,.nr .selectize-input.focus{border-color:var(--app-accent)!important;box-shadow:0 0 0 3px rgba(2,132,199,.15)!important}
.nr .req-bad .control-label{color:#dc2626}
.nr .req-bad .form-control,.nr .req-bad .form-select,.nr .req-bad .selectize-input{border-color:#dc2626!important;background:rgba(220,38,38,.05)!important;box-shadow:0 0 0 3px rgba(220,38,38,.12)!important}
.form-intro{font-size:.85rem;color:var(--app-muted-fg);margin:-.2rem 0 1rem}
.form-section-title{font-weight:650;font-size:.95rem;margin:.65rem 0 .8rem}
.form-section-rule{border-top:1px solid var(--app-border);margin:.5rem 0 1rem}
.action-bar{position:sticky;bottom:0;z-index:5;display:flex;align-items:center;justify-content:flex-end;gap:.65rem;padding:.9rem 0 .1rem;margin-top:.5rem;background:var(--bs-body-bg);border-top:1px solid var(--app-border)}
.modal-content{border:1px solid var(--app-border);border-radius:.8rem;background:var(--bs-body-bg)}
.modal-header,.modal-footer{border-color:var(--app-border)}
.ref{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.8rem;word-break:break-word;color:var(--app-muted-fg)}
.badge-scen{font-size:.9rem;background:var(--app-accent);color:#fff;padding:.2rem .5rem;border-radius:.4rem}
.shiny-notification{opacity:1;border:1px solid #d4d4d8;border-radius:.6rem;font-weight:600;box-shadow:0 8px 24px rgba(0,0,0,.16);background:#f4f4f5!important;color:#27272a!important}
.shiny-notification-message{background:#ecfdf5!important;color:#166534!important;border-color:#86efac!important}
.shiny-notification-warning{background:#fffbeb!important;color:#92400e!important;border-color:#fcd34d!important}
.shiny-notification-error{background:#fef2f2!important;color:#991b1b!important;border-color:#fca5a5!important}
[data-bs-theme='dark'] .shiny-notification{background:#27272a!important;color:#f4f4f5!important;border-color:#52525b!important}
[data-bs-theme='dark'] .shiny-notification-message{background:#052e16!important;color:#bbf7d0!important;border-color:#166534!important}
[data-bs-theme='dark'] .shiny-notification-warning{background:#451a03!important;color:#fde68a!important;border-color:#92400e!important}
[data-bs-theme='dark'] .shiny-notification-error{background:#450a0a!important;color:#fecaca!important;border-color:#991b1b!important}
.shiny-notification-close{color:inherit!important}
@media(min-width:1900px){.page-shell{max-width:1840px;padding-left:2.75rem;padding-right:2.75rem}.card-body{padding:1.3rem 1.4rem}}

/* Final contrast/accessibility pass inspired by the reference dashboard. */
body,.navbar{background:var(--app-bg)!important;color:var(--bs-body-color)!important}
.page-heading p,.chart-note,.metric-label,.metric-note,.dt-foot,.fgrid .control-label{color:var(--app-muted-fg)!important}
.card,.metric-card{background:var(--app-card-bg)!important;border:1px solid var(--app-border)!important;box-shadow:0 1px 2px rgba(0,0,0,.07)}
.card-header{background:var(--app-card-bg)!important;border-bottom:1px solid var(--app-border)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .card,[data-bs-theme='dark'] .metric-card{box-shadow:0 0 0 1px rgba(212,212,216,.08),0 8px 22px rgba(0,0,0,.22)}
.btn-outline-secondary{border-color:var(--app-border)!important;color:var(--bs-body-color)!important}
.btn-outline-secondary:hover,.btn-outline-secondary:focus{border-color:var(--app-muted-fg)!important;background:var(--app-muted)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .btn-outline-secondary{border-color:#71717a!important;color:#fafafa!important}
.form-control,.form-select,.selectize-input,.selectize-dropdown,.input-group-text{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border-color:var(--app-border)!important}
.form-control::placeholder,.selectize-input input::placeholder{color:var(--app-muted-fg)!important;opacity:1}
.selectize-dropdown{border:1px solid var(--app-border)!important;box-shadow:0 8px 20px rgba(0,0,0,.14)!important}
.selectize-dropdown .option{background:var(--app-card-bg)!important;color:var(--bs-body-color)!important}
.selectize-dropdown .active{background:var(--app-muted)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .selectize-input input{color:#fafafa!important}
[data-bs-theme='dark'] .form-control:focus,[data-bs-theme='dark'] .form-select:focus,[data-bs-theme='dark'] .selectize-input.focus{border-color:#38bdf8!important;box-shadow:0 0 0 3px rgba(56,189,248,.2)!important}
.nav-tabs{border-bottom:1px solid var(--app-border)!important}
.nav-tabs .nav-link{color:var(--app-muted-fg)!important;border:1px solid transparent!important}
.nav-tabs .nav-link.active{color:var(--bs-body-color)!important;background:var(--app-muted)!important;border-color:var(--app-border) var(--app-border) var(--app-accent)!important}
table.dataTable thead th{color:var(--bs-body-color)!important;border-bottom:2px solid var(--app-border)!important}
table.dataTable tbody td{color:var(--bs-body-color)!important;border-bottom:1px solid var(--app-border)!important}
table.dataTable tbody tr:nth-child(even)>td{background:var(--app-card-bg)!important}
table.dataTable tbody tr:nth-child(odd)>td{background:var(--app-bg)!important}
table.dataTable tbody tr:hover>td{background:var(--app-muted)!important}
[data-bs-theme='dark'] table.dataTable tbody tr.selected>td{background:#164e63!important;color:#f0f9ff!important}
[data-bs-theme='dark'] .dropdown-menu{background:#18181b!important;color:#fafafa!important;border-color:#52525b!important}
[data-bs-theme='dark'] .dropdown-item{color:#f4f4f5!important}
[data-bs-theme='dark'] .dropdown-item:hover{background:#27272a!important;color:#ffffff!important}
[data-bs-theme='dark'] .text-muted,[data-bs-theme='dark'] .muted{color:#d4d4d8!important}
.btn-primary,.btn-primary:hover,.btn-primary:focus,.btn-primary:active{color:#ffffff!important}

"

js <- "
function setRangePlaceholders(){
  $('#f_dates-start').attr('placeholder','Start date');
  $('#f_dates-end').attr('placeholder','End date');
}
$(document).on('shiny:connected', setRangePlaceholders);
$(document).on('shown.bs.dropdown', '.filter-dd', setRangePlaceholders);
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

# =============================================================================
# UI
# =============================================================================
ui <- function(request) {
  page_navbar(
    id = "nav", selected = "requests", fillable = FALSE,
    title = tags$a(href = ".", class = "brand", "Expense Tracker"),
    window_title = "Expense Tracker", theme = app_theme,
    header = tagList(tags$style(css), tags$script(HTML(js))),
    
    nav_panel(
      title = "Requests", value = "requests",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("Requests"), p("Track procurement requests and understand spending over time.")),
              actionButton("new_request", tagList(icon("plus"), "New request"), class = "btn-primary")),
          
          div(class = "toolbar",
              div(class = "dropdown filter-dd",
                  tags$button(class = "btn btn-outline-secondary", type = "button",
                              `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside",
                              `aria-expanded` = "false", icon("filter"), " Filters",
                              span(id = "fcount", class = "badge rounded-pill text-bg-primary ms-2",
                                   style = "display:none")),
                  div(class = "dropdown-menu p-3 fmenu",
                      div(class = "fgrid",
                          div(class = "wide", dateRangeInput(
                            "f_dates", "Date range", start = NULL, end = NULL,
                            separator = " to ", format = "dd-mm-yyyy", width = "100%")),
                          selectizeInput("f_week", "Week", NULL, multiple = TRUE,
                                         options = list(placeholder = "All weeks")),
                          selectizeInput("f_project", "Project code", NULL, multiple = TRUE,
                                         options = list(placeholder = "All projects")),
                          selectizeInput("f_person", "Requested by", NULL, multiple = TRUE,
                                         options = list(placeholder = "All requesters")),
                          selectizeInput("f_submitter", "Submitted by", NULL, multiple = TRUE,
                                         options = list(placeholder = "All focal points")),
                          div(class = "wide d-flex justify-content-end gap-2 mt-1",
                              actionButton("reset", "Clear filters", class = "btn-outline-secondary btn-sm"))))),
              div(class = "ms-auto d-flex align-items-center gap-2 flex-wrap",
                  selectInput("cur", NULL, CUR, selected = "ETB", width = "110px"),
                  actionButton("share", "Share view", icon = icon("link"), class = "btn-outline-secondary"))),
          
          uiOutput("totals"),
          
          card(class = "mt-3", card_header(div(class = "card-head",
                                               span("Request breakdown"),
                                               div(class = "d-flex align-items-end flex-wrap gap-2",
                                                   selectInput("breakdown_status", "Status",
                                                               choices = c("Paid" = "paid", "Waiting" = "waiting",
                                                                           "Under validation" = "under validation", "Rejected" = "rejected"),
                                                               selected = "paid", width = "190px"),
                                                   selectInput("dim", "Group by", DIMS, selected = "project_code", width = "190px")))),
               navset_tab(
                 nav_panel("Chart", plotOutput("plot_dim", height = "300px")),
                 nav_panel("Table", DTOutput("by_dim", fill = FALSE)))),
          
          card(class = "chart-card mt-3", card_header("Monthly request amounts by status"),
               div(class = "card-body",
                   p(class = "chart-note", "Compare paid, waiting and under-validation request values month by month."),
                   plotOutput("plot_trend", height = "330px"))),
          
          
          
          card(class = "mt-3", fill = FALSE,
               card_header(div(class = "table-head",
                               div(span("Request register"), p(class = "chart-note mb-0", "Select a row, then open it to review or update its details.")),
                               div(class = "buttons",
                                   actionButton("open_request", "Open", icon = icon("folder-open"), class = "btn-outline-secondary"),
                                   downloadButton("export", "Export", class = "btn-outline-secondary")))),
               navset_tab(
                 id = "request_status", selected = "all",
                 nav_panel("All requests", value = "all", DTOutput("table_all", fill = FALSE)),
                 nav_panel("Waiting", value = "waiting", DTOutput("table_waiting", fill = FALSE)),
                 nav_panel("Under validation", value = "under validation", DTOutput("table_validation", fill = FALSE)),
                 nav_panel("Paid", value = "paid", DTOutput("table_paid", fill = FALSE)),
                 nav_panel("Rejected", value = "rejected", DTOutput("table_rejected", fill = FALSE))))
      )
    ),
    # Full-page new request; the navbar link is hidden and navigation is controlled by buttons.
    nav_panel(
      title = "New request", value = "new",
      div(class = "page-shell",
          div(class = "page-heading",
              div(h1("New request"),
                  p("Enter a procurement request. Fields without an asterisk are optional.")),
              actionButton("cancel_new", tagList(icon("arrow-left"), "Back to requests"),
                           class = "btn-outline-secondary")),
          layout_columns(
            fill = FALSE, col_widths = c(8, 4),
            card(fill = FALSE,
                 div(class = "card-body",
                     uiOutput("new_form_ui"),
                     div(class = "action-bar",
                         actionButton("clear", "Clear form", class = "btn-outline-secondary"),
                         actionButton("cancel_new_bottom", "Cancel", class = "btn-outline-secondary"),
                         actionButton("add", "Add request", class = "btn-primary")))),
            card(card_header("Request preview"),
                 div(class = "card-body", uiOutput("preview")))
          ))
    ),
    nav_spacer(),
    nav_item(input_dark_mode(id = "mode"))
  )
}

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  touched  <- reactiveVal(character())
  show_all <- reactiveVal(FALSE)
  edit_id  <- reactiveVal(NULL)
  
  # Clear Shiny's default date-range value on first render so the dashboard opens
  # with the full database history rather than only the current date/month.
  session$onFlushed(function() {
    suppressWarnings(updateDateRangeInput(session, "f_dates", start = NA, end = NA))
  }, once = TRUE)
  
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
    valueFunc = function() dbGetQuery(poll_cn, "SELECT * FROM expenses ORDER BY id DESC")
  )
  
  data <- reactive({
    d <- db_poll()
    # Backward compatibility for older rows saved with the status "cancelled".
    if ("status" %in% names(d)) d$status[!is.na(d$status) & d$status == "cancelled"] <- "rejected"
    # USD is fixed at entry, so scenario and totals don't move when RATES change.
    # as.numeric() keeps this working when the table is still empty.
    usd <- as.numeric(ifelse(is.na(d$usd),
                             as.numeric(d$amount) / unname(RATES[d$currency]),
                             d$usd))
    for (k in CUR)
      d[[k]] <- as.numeric(ifelse(d$currency %in% k, d$amount,
                                  round(usd * RATES[[k]], 2)))
    d$scenario      <- scenario(usd, d$waiver)
    d$week          <- format(as.Date(d$order_date), "%G-W%V")
    d$month         <- format(as.Date(d$order_date), "%Y-%m")
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
                           options = list(create = TRUE, placeholder = "Type or pick"))
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
  
  # ---- filters: date range, week, project, requester and submitter ----
  date_bound <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]]))))
      return(as.Date(NA))
    suppressWarnings(as.Date(as.character(x[[1]])))
  }
  base <- reactive({
    d <- data()
    dr <- input$f_dates
    from <- if (length(dr) >= 1) date_bound(dr[[1]]) else as.Date(NA)
    to <- if (length(dr) >= 2) date_bound(dr[[2]]) else as.Date(NA)
    if (!is.na(from)) d <- filter(d, !is.na(order_date), as.Date(order_date) >= from)
    if (!is.na(to)) d <- filter(d, !is.na(order_date), as.Date(order_date) <= to)
    if (length(input$f_week))      d <- filter(d, week %in% input$f_week)
    if (length(input$f_project))   d <- filter(d, project_code %in% input$f_project)
    if (length(input$f_person))    d <- filter(d, requested_by %in% input$f_person)
    if (length(input$f_submitter)) d <- filter(d, submitted_by %in% input$f_submitter)
    d
  })
  filtered <- reactive(base())
  
  observeEvent(input$reset, {
    suppressWarnings(updateDateRangeInput(session, "f_dates", start = NA, end = NA))
    for (id in c("f_week", "f_project", "f_person", "f_submitter"))
      updateSelectizeInput(session, id, selected = character(0))
  })
  
  # Filter count shown on the dropdown button.
  observe({
    dr <- input$f_dates
    has_one_date <- length(dr) >= 1 && !is.na(date_bound(dr[[1]]))
    has_two_dates <- length(dr) >= 2 && !is.na(date_bound(dr[[2]]))
    n <- sum(has_one_date, has_two_dates, length(input$f_week) > 0,
             length(input$f_project) > 0, length(input$f_person) > 0,
             length(input$f_submitter) > 0)
    session$sendCustomMessage("badge", list(n = n))
  })
  
  # Short, shareable URL containing only the active filters.
  share_url <- function() {
    enc <- function(x) URLencode(paste(x, collapse = "|"), reserved = TRUE)
    dr <- input$f_dates
    q <- list(
      v = "r",
      f = if (length(dr) >= 1 && !is.na(date_bound(dr[[1]]))) as.character(date_bound(dr[[1]])) else NULL,
      t = if (length(dr) >= 2 && !is.na(date_bound(dr[[2]]))) as.character(date_bound(dr[[2]])) else NULL,
      w = if (length(input$f_week)) enc(input$f_week) else NULL,
      p = if (length(input$f_project)) enc(input$f_project) else NULL,
      r = if (length(input$f_person)) enc(input$f_person) else NULL,
      s = if (length(input$f_submitter)) enc(input$f_submitter) else NULL
    )
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
      p(class = "chart-note", "Anyone with the link can open this view with the same filters. No request data is embedded in the URL."),
      div(class = "input-group",
          tags$input(id = "share_url", class = "form-control", readonly = NA,
                     value = share_url(), onclick = "this.select()"),
          tags$button(id = "copy_link", type = "button", class = "btn btn-primary", "Copy link")),
      footer = modalButton("Close"), easyClose = TRUE))
  })
  
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q) || !identical(q[["v"]], "r")) return()
    d <- isolate(data())
    vals <- function(k) if (is.null(q[[k]]) || !nzchar(q[[k]])) character()
    else strsplit(q[[k]], "|", fixed = TRUE)[[1]]
    as_d <- function(x) tryCatch(as.Date(x), error = function(e) NA)
    from <- if (!is.null(q[["f"]])) as_d(q[["f"]]) else NA
    to <- if (!is.null(q[["t"]])) as_d(q[["t"]]) else NA
    if (!is.na(from) || !is.na(to))
      suppressWarnings(updateDateRangeInput(session, "f_dates",
                                            start = if (is.na(from)) NA else from, end = if (is.na(to)) NA else to))
    set <- function(id, key, choices) {
      selected <- vals(key)
      if (length(selected)) updateSelectizeInput(session, id,
                                                 choices = with_sel(choices, selected), selected = selected)
    }
    set("f_week", "w", week_choices(d))
    set("f_project", "p", uniq(d$project_code))
    set("f_person", "r", uniq(d$requested_by))
    set("f_submitter", "s", uniq(d$submitted_by))
  }, once = TRUE)
  
  # ---- New request is a full page, not a modal ----
  observeEvent(input$new_request, {
    touched(character()); show_all(FALSE)
    nav_select("nav", "new", session = session)
  })
  observeEvent(input$cancel_new, nav_select("nav", "requests", session = session))
  observeEvent(input$cancel_new_bottom, nav_select("nav", "requests", session = session))
  
  output$new_form_ui <- renderUI({
    div(class = "nr",
        div(class = "form-section-title", "Request details"),
        div(class = "fg",
            col(12, textAreaInput("description", R("Description"), rows = 2, width = "100%")),
            col(4, cached("requested_by", "Requested by")),
            col(4, cached("project_code", "Project code")),
            col(4, cached("budget_line", "Budget line")),
            col(4, pick("department", "Department", DEPTS)),
            col(4, cached("location", "Location")),
            col(4, cached("supplier", "Supplier")),
            col(4, numericInput("amount", R("Amount"), NA, min = 0, width = "100%")),
            col(4, pick("currency", "Currency", CUR))
        ),
        div(class = "form-section-rule"),
        div(class = "form-section-title", "Optional details"),
        div(class = "fg",
            col(4, cached("country", "Country", req = FALSE)),
            col(4, numericInput("quantity", "Quantity", NA, min = 0, width = "100%")),
            col(4, cached("unit", "Unit", req = FALSE)),
            col(4, div(style = "padding-top:1.9rem", checkboxInput("framework", "Framework agreement"))),
            col(6, cached("submitted_by", "Submitted by (focal point)", req = FALSE)),
            col(6, date_in("of_date", "Order date", Sys.Date(), width = "100%")),
            col(12, textAreaInput("remarks", "Remarks", rows = 2, width = "100%"))
        )
    )
  })
  
  # ---- new request: required fields (TRUE = filled) ----
  valid <- reactive(c(
    description  = nzchar(v(input$description)),
    requested_by = nzchar(v(input$requested_by)),
    supplier     = nzchar(v(input$supplier)),
    project_code = nzchar(v(input$project_code)),
    budget_line  = nzchar(v(input$budget_line)),
    department   = nzchar(v(input$department)),
    location     = nzchar(v(input$location)),
    amount       = has_num(input$amount) && input$amount > 0,
    currency     = nzchar(v(input$currency))
  ))
  
  observeEvent(input$touched, touched(union(touched(), input$touched)))
  
  # red = required but empty (after leaving the field, or after pressing Add)
  observe({
    bad  <- names(valid())[!valid()]
    show <- if (show_all()) bad else intersect(bad, touched())
    session$sendCustomMessage("mark", list(ids = as.list(show)))
  })
  
  waiver_val <- reactive(if (isTRUE(input$framework)) "Framework agreement" else "No")
  odate <- reactive(if (has_date(input$of_date)) input$of_date else Sys.Date())
  
  form_rec <- reactive(list(
    project_code = v(input$project_code), budget_line = v(input$budget_line),
    country = v(input$country), location = v(input$location),
    department = v(input$department), order_date = as.character(odate())))
  
  # picking a project suggests its department and location (still editable)
  observeEvent(input$project_code, {
    pc <- v(input$project_code); req(nzchar(pc))
    p <- db_get("SELECT department, location FROM projects WHERE project_code = ?", pc)
    if (!nrow(p)) return()
    if (nzchar(v(p$department)))
      updateSelectInput(session, "department", selected = p$department)
    if (nzchar(v(p$location)))
      updateSelectizeInput(session, "location",
                           choices = c("", union(cache_choices(isolate(data()), "location"), p$location)),
                           selected = p$location,
                           options = list(create = TRUE, placeholder = "Type or pick"))
  }, ignoreInit = TRUE)
  
  output$preview <- renderUI({
    of  <- mk_ref(form_rec(), "OF")
    cur <- v(input$currency); amt <- input$amount
    shown <- function(x) if (is.na(x)) "\u2014" else x
    top <- if (!nzchar(cur) || !has_num(amt)) {
      p(class = "muted", "Enter amount and currency to see the scenario.")
    } else {
      usd <- amt / RATES[[cur]]
      s   <- scenario(usd, waiver_val())
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
        p(class = "muted mb-0", "Order form"), p(shown(of), class = "ref"),
        p(class = "muted mb-0", "Purchase order"),
        p("Created when status moves to under validation", class = "ref muted"))
  })
  
  # back to the starting values (defaults prefilled, the rest empty)
  reset_form <- function() {
    updateTextAreaInput(session, "description", value = "")
    d <- isolate(data())
    for (id in names(CACHE)) {
      val <- DEFAULTS[[id]] %||% ""
      ch  <- union(cache_choices(d, id), val[nzchar(val)])
      updateSelectizeInput(session, id, choices = c("", ch), selected = val,
                           options = list(create = TRUE, placeholder = "Type or pick"))
    }
    for (id in c("department", "currency"))
      updateSelectInput(session, id, selected = DEFAULTS[[id]] %||% "")
    updateCheckboxInput(session, "framework", value = FALSE)
    for (id in c("amount", "quantity")) updateNumericInput(session, id, value = NA)
    suppressWarnings(updateDateInput(session, "of_date", value = Sys.Date()))
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
    cur  <- input$currency
    rate <- RATES[[cur]]
    rec  <- form_rec()
    row <- list(
      description = v(input$description), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$country),
      location = rec$location, department = rec$department,
      reason = NA_character_, order_form = mk_ref(rec, "OF"), po = NA_character_,
      requested_by = v(input$requested_by),
      submitted_by = if (nzchar(v(input$submitted_by))) v(input$submitted_by)
      else DEFAULTS$submitted_by,
      status = "waiting", waiver = waiver_val(), supplier = v(input$supplier),
      unit = nz(input$unit),
      quantity = if (has_num(input$quantity)) input$quantity else NA_real_,
      amount = input$amount, currency = cur,
      usd = input$amount / rate, rate_used = rate,
      order_date = rec$order_date, po_date = NA_character_,
      payment_date = NA_character_, remarks = nz(input$remarks),
      created = now(), updated = now())
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, sprintf("INSERT INTO expenses(%s) VALUES(%s)",
                          paste(names(row), collapse = ","),
                          paste(rep("?", length(row)), collapse = ",")),
              params = unname(row))
    # remember this project's department and location for next time
    dbExecute(cn, "INSERT OR REPLACE INTO projects(project_code, department, location)
                   VALUES(?,?,?)",
              params = list(row$project_code, row$department, row$location))
    reset_form()
    nav_select("nav", "requests", session = session)
    showNotification("Request added.", type = "message")
  })
  
  # ---- Open a selected request in a detailed, editable modal ----
  requests_for_tab <- function(tab = input$request_status %||% "all") {
    d <- filtered()
    if (!identical(tab, "all")) d <- d[d$status == tab, , drop = FALSE]
    d
  }
  selected_request_id <- function() {
    tab <- input$request_status %||% "all"
    selected_input <- switch(tab,
                             all = input$table_all_rows_selected,
                             waiting = input$table_waiting_rows_selected,
                             `under validation` = input$table_validation_rows_selected,
                             paid = input$table_paid_rows_selected,
                             rejected = input$table_rejected_rows_selected,
                             NULL
    )
    d <- requests_for_tab(tab)
    if (!length(selected_input) || selected_input[[1]] < 1 || selected_input[[1]] > nrow(d))
      return(NULL)
    as.integer(d$id[selected_input[[1]]])
  }
  
  observeEvent(input$open_request, {
    id <- selected_request_id()
    if (is.null(id)) {
      showNotification("Select a request row first.", type = "warning")
      return()
    }
    r <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(r)) {
      showNotification("That request is no longer available. Refresh the table and try again.", type = "warning")
      return()
    }
    r <- r[1, , drop = FALSE]
    edit_id(id)
    showModal(modalDialog(
      title = paste0("Request #", r$id), size = "l", easyClose = TRUE,
      div(class = "form-intro", "Fields marked * are required. Country, quantity, unit, framework agreement, order date, submitted by and remarks are optional."),
      div(class = "nr",
          div(class = "form-section-title", "Request details"),
          div(class = "fg",
              col(12, textAreaInput("m_desc", "Description *", v(r$description), rows = 2, width = "100%")),
              col(6, textInput("m_requested_by", "Requested by *", v(r$requested_by), width = "100%")),
              col(6, textInput("m_submitted_by", "Submitted by", v(r$submitted_by), width = "100%")),
              col(4, textInput("m_project", "Project code *", v(r$project_code), width = "100%")),
              col(4, textInput("m_budget", "Budget line *", v(r$budget_line), width = "100%")),
              col(4, selectInput("m_dept", "Department *", unique(c(DEPTS, v(r$department))), selected = v(r$department), width = "100%")),
              col(4, textInput("m_loc", "Location *", v(r$location), width = "100%")),
              col(4, textInput("m_country", "Country", v(r$country), width = "100%")),
              col(4, textInput("m_supplier", "Supplier *", v(r$supplier), width = "100%")),
              col(4, numericInput("m_amount", "Amount *", r$amount, min = 0, width = "100%")),
              col(4, selectInput("m_cur", "Currency *", CUR, selected = r$currency, width = "100%")),
              col(4, numericInput("m_quantity", "Quantity", if (is.na(r$quantity)) NA else r$quantity, min = 0, width = "100%")),
              col(4, textInput("m_unit", "Unit", v(r$unit), width = "100%")),
              col(4, div(style = "padding-top:1.9rem", checkboxInput("m_framework", "Framework agreement", value = identical(v(r$waiver), "Framework agreement")))),
              col(4, selectInput("m_status", "Status *", STATUS, selected = v(r$status), width = "100%")),
              col(8, textAreaInput("m_remarks", "Remarks", v(r$remarks), rows = 2, width = "100%")),
              col(6, date_in("m_order_date", "Order date", if (is.na(r$order_date)) Sys.Date() else as.Date(r$order_date), width = "100%")),
              col(6, conditionalPanel(
                "input.m_status == 'under validation' || input.m_status == 'paid'",
                div(class = "fg",
                    col(6, selectizeInput("m_reason", "PO reason *",
                                          choices = c("", uniq(c(data()$reason, r$reason))),
                                          selected = v(r$reason), width = "100%",
                                          options = list(create = TRUE, placeholder = "Type or pick"))),
                    col(6, date_in("m_po_date", "PO date *",
                                   if (is.na(r$po_date)) Sys.Date() else as.Date(r$po_date), width = "100%"))))),
              col(6, conditionalPanel(
                "input.m_status == 'paid'",
                date_in("m_date", "Payment date *",
                        if (is.na(r$payment_date)) Sys.Date() else as.Date(r$payment_date), width = "100%")))
          )
      ),
      footer = tagList(modalButton("Cancel"), actionButton("m_save", "Save changes", class = "btn-primary"))
    ))
  })
  
  observeEvent(input$m_save, {
    id <- edit_id()
    old <- data()[data()$id == id, , drop = FALSE]
    if (!nrow(old)) { removeModal(); return() }
    old <- old[1, , drop = FALSE]
    need_po <- input$m_status %in% c("under validation", "paid")
    ok <- all(nzchar(c(v(input$m_desc), v(input$m_requested_by), v(input$m_project),
                       v(input$m_budget), v(input$m_dept), v(input$m_loc), v(input$m_supplier)))) &&
      has_num(input$m_amount) && input$m_amount > 0 && nzchar(v(input$m_cur)) &&
      (!need_po || (nzchar(v(input$m_reason)) && has_date(input$m_po_date))) &&
      (input$m_status != "paid" || has_date(input$m_date))
    if (!ok) {
      showNotification("Please complete the required fields before saving.", type = "warning")
      return()
    }
    
    changed <- !isTRUE(all.equal(input$m_amount, old$amount)) || input$m_cur != old$currency
    rate <- if (changed || is.na(old$rate_used)) RATES[[input$m_cur]] else old$rate_used
    usd <- if (changed || is.na(old$usd)) input$m_amount / rate else old$usd
    order_date <- if (has_date(input$m_order_date)) as.character(input$m_order_date) else as.character(old$order_date)
    reason <- if (need_po) v(input$m_reason) else v(old$reason)
    po_date <- if (need_po) as.character(input$m_po_date) else as.character(old$po_date)
    rec <- list(project_code = v(input$m_project), budget_line = v(input$m_budget),
                country = v(input$m_country), location = v(input$m_loc),
                department = v(input$m_dept), order_date = order_date,
                reason = reason, po_date = po_date)
    row <- list(
      description = v(input$m_desc), project_code = rec$project_code,
      budget_line = rec$budget_line, country = nz(input$m_country),
      location = rec$location, department = rec$department,
      supplier = v(input$m_supplier), requested_by = v(input$m_requested_by),
      submitted_by = if (nzchar(v(input$m_submitted_by))) v(input$m_submitted_by) else DEFAULTS$submitted_by,
      quantity = if (has_num(input$m_quantity)) input$m_quantity else NA_real_,
      unit = nz(input$m_unit), remarks = nz(input$m_remarks),
      waiver = if (isTRUE(input$m_framework)) "Framework agreement" else "No",
      amount = input$m_amount, currency = input$m_cur, usd = usd, rate_used = rate,
      status = input$m_status,
      reason = if (nzchar(reason)) reason else NA_character_,
      order_date = order_date,
      po_date = if (need_po) as.character(input$m_po_date) else NA_character_,
      order_form = mk_ref(rec, "OF"),
      po = if (need_po) mk_ref(rec, "PO") else NA_character_,
      payment_date = if (input$m_status == "paid") as.character(input$m_date) else NA_character_,
      updated = now()
    )
    cn <- con(); on.exit(dbDisconnect(cn))
    dbExecute(cn, sprintf("UPDATE expenses SET %s WHERE id=?",
                          paste0(names(row), "=?", collapse = ",")),
              params = c(unname(row), list(as.integer(id))))
    dbExecute(cn, "INSERT OR REPLACE INTO projects(project_code, department, location) VALUES(?,?,?)",
              params = list(row$project_code, row$department, row$location))
    removeModal()
    status_notice <- switch(input$m_status,
                            waiting = list(text = "Request status set to Waiting.", type = "default"),
                            `under validation` = list(text = "Request moved to Under validation.", type = "warning"),
                            paid = list(text = "Request marked as Paid.", type = "message"),
                            rejected = list(text = "Request rejected.", type = "error"),
                            list(text = "Request updated.", type = "message"))
    showNotification(status_notice$text, type = status_notice$type)
  })
  
  # popup with the full description / remarks
  observeEvent(input$see_more, {
    showModal(modalDialog(
      title = input$see_more$title,
      div(style = "white-space:pre-wrap", input$see_more$text),
      footer = modalButton("Close"), easyClose = TRUE))
  })
  
  # ---- dashboard summary ----
  output$totals <- renderUI({
    d <- filtered(); k <- input$cur %||% "ETB"
    total <- sum(d[[k]], na.rm = TRUE)
    paid <- sum(d[[k]][d$status == "paid"], na.rm = TRUE)
    waiting <- sum(d[[k]][d$status == "waiting"], na.rm = TRUE)
    validation <- sum(d[[k]][d$status == "under validation"], na.rm = TRUE)
    metric <- function(label, value, note) div(class = "metric-card",
                                               p(class = "metric-label", label),
                                               p(class = "metric-value", paste(k, abbr(value))),
                                               p(class = "metric-note", note))
    rejected <- sum(d[[k]][d$status == "rejected"], na.rm = TRUE)
    div(class = "metrics-grid",
        metric("Total requested", total, paste(nrow(d), "requests in the selected view")),
        metric("Paid spend", paid, paste(sum(d$status == "paid", na.rm = TRUE), "paid requests")),
        metric("Waiting", waiting, paste(sum(d$status == "waiting", na.rm = TRUE), "requests awaiting review")),
        metric("Under validation", validation, paste(sum(d$status == "under validation", na.rm = TRUE), "requests in validation")),
        metric("Rejected", rejected, paste(sum(d$status == "rejected", na.rm = TRUE), "rejected requests")))
  })
  
  
  # Status palette is consistent across charts, notifications and status changes.
  fg_col <- function() if (identical(input$mode, "dark")) "#e4e4e7" else "#27272a"
  status_label <- function(status) switch(status,
                                          waiting = "Waiting",
                                          `under validation` = "Under validation",
                                          paid = "Paid",
                                          rejected = "Rejected",
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
  
  # One monthly stacked chart compares paid, waiting and under-validation amounts.
  output$plot_trend <- renderPlot({
    chart_statuses <- c("waiting", "under validation", "paid")
    d <- filtered() |> filter(status %in% chart_statuses, !is.na(month))
    k <- input$cur %||% "ETB"
    fg <- fg_col()
    validate(need(nrow(d) > 0, "No paid, waiting or under-validation requests in this view"))
    
    g <- d |>
      group_by(month, status) |>
      summarise(total = sum(.data[[k]], na.rm = TRUE), .groups = "drop")
    month_levels <- tail(sort(unique(g$month)), 12)
    g <- filter(g, month %in% month_levels)
    g$month <- factor(g$month, levels = month_levels)
    g$status <- factor(g$status, levels = chart_statuses)
    label_data <- g |> group_by(month) |> summarise(total = sum(total), .groups = "drop")
    
    ggplot(g, aes(x = month, y = total, fill = status)) +
      geom_col(width = .68, position = "stack") +
      geom_text(data = label_data, aes(x = month, y = total, label = abbr(total)),
                inherit.aes = FALSE, vjust = -.45, colour = fg, size = 3.5) +
      scale_fill_manual(values = STATUS_COL[chart_statuses],
                        breaks = chart_statuses,
                        labels = c("Waiting", "Under validation", "Paid"),
                        drop = FALSE) +
      scale_y_continuous(labels = abbr, expand = expansion(mult = c(0, .16))) +
      labs(subtitle = paste("Monthly request values in", k), x = NULL, y = NULL) +
      chart_theme(fg)
  }, bg = "transparent", res = 96)
  
  # Breakdown can switch between all four statuses and group by project, requester or department.
  totals_by <- function(key, status_value = input$breakdown_status %||% "paid") {
    if (is.null(status_value) || !status_value %in% STATUS) status_value <- "paid"
    filtered() |>
      filter(.data$status == status_value) |>
      group_by(label = coalesce(as.character(.data[[key]]), "(none)")) |>
      summarise(requests = n(), across(all_of(CUR), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }
  
  output$plot_dim <- renderPlot({
    status_value <- input$breakdown_status %||% "paid"
    group_by_value <- input$dim %||% "project_code"
    g <- totals_by(group_by_value, status_value)
    validate(need(nrow(g) > 0, paste("No", status_label(status_value), "requests in this view")))
    currency <- input$cur %||% "ETB"
    g$total <- as.numeric(g[[currency]])
    g$total[is.na(g$total)] <- 0
    fg <- fg_col()
    
    # Department is more readable as a share-of-spend pie than a ranked bar chart.
    if (identical(group_by_value, "department")) {
      g <- g |> filter(total > 0) |> arrange(desc(total))
      validate(need(nrow(g) > 0, paste("No positive", tolower(status_label(status_value)), "spend by department in", currency)))
      g$share <- g$total / sum(g$total)
      g$label <- ifelse(is.na(g$label) | !nzchar(g$label), "(none)", as.character(g$label))
      g$pie_label <- paste0(abbr(g$total), "\n", round(g$share * 100), "%")
      pie_cols <- c("#0284c7", "#0369a1", "#075985", "#0ea5e9", "#38bdf8", "#7dd3fc")
      pie_values <- setNames(rep(pie_cols, length.out = nrow(g)), g$label)
      
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
  }, bg = "transparent", res = 96)
  
  output$by_dim <- renderDT({
    status_value <- input$breakdown_status %||% "paid"
    lab <- names(DIMS)[DIMS == input$dim]
    datatable(totals_by(input$dim, status_value), rownames = FALSE, selection = "none",
              fillContainer = FALSE, colnames = c(lab, "Requests", CUR),
              options = list(dom = "t", language = dt_language, scrollX = TRUE)) |>
      formatRound(CUR, 2)
  })
  
  # ---- request list: exactly the six requested columns, no column filters ----
  SHOW <- c("description", "project_code", "budget_line", "requested_by", "amount", "status")
  render_request_table <- function(tab = "all") {
    renderDT({
      d <- requests_for_tab(tab)
      display <- data.frame(
        id = d$id,
        description = ifelse(is.na(d$description), "", as.character(d$description)),
        project_code = ifelse(is.na(d$project_code), "", as.character(d$project_code)),
        budget_line = ifelse(is.na(d$budget_line), "", as.character(d$budget_line)),
        requested_by = ifelse(is.na(d$requested_by), "", as.character(d$requested_by)),
        amount = ifelse(is.na(d$amount), "", paste0(d$currency, " ", vapply(d$amount, money, character(1)))),
        status = ifelse(is.na(d$status), "", as.character(d$status)),
        check.names = FALSE
      )
      names(display) <- c("id", "Description", "Project code", "Budget line", "Requested by", "Amount", "Status")
      datatable(display, rownames = FALSE, selection = "single", filter = "none",
                fillContainer = FALSE, escape = TRUE,
                options = list(
                  scrollX = TRUE, autoWidth = TRUE, pageLength = 8,
                  dom = "<'dt-top'f>rt<'dt-foot'ip>",
                  language = dt_language, initComplete = more_init,
                  columnDefs = list(
                    list(targets = 0, visible = FALSE, searchable = FALSE),
                    list(targets = 1, width = "32%", render = more_render("Description")),
                    list(targets = c(2, 3, 4, 5, 6), className = "dt-body-left")
                  )
                ))
    }, server = TRUE)
  }
  output$table_all <- render_request_table("all")
  output$table_waiting <- render_request_table("waiting")
  output$table_validation <- render_request_table("under validation")
  output$table_paid <- render_request_table("paid")
  output$table_rejected <- render_request_table("rejected")
  
  output$export <- downloadHandler(
    filename = "expenses.xlsx",
    content  = function(file) write_xlsx(select(filtered(), -any_of(c("week", "month"))), file)
  )
}

shinyApp(ui, server)