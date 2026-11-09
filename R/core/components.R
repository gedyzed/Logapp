cache_choices <- function(d, id) {
  vals <- uniq(unlist(d[CACHE[[id]]], use.names = FALSE))
  if (identical(id, "supplier")) vals <- supplier_catalog_db(active_only = TRUE)
  if (id %in% c("requested_by", "submitted_by")) {
    people <- db_get("SELECT full_name FROM people WHERE active=1 ORDER BY full_name COLLATE NOCASE")$full_name
    vals <- uniq(c(vals, people))
  }
  vals
}

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

# Dismiss buttons. Back and Close stay neutral. Cancel is the same weight, in red.
# Shiny modalButton() always uses btn-default, which the footer treats as Back.
modal_cancel_button <- function(label = "Cancel") {
  tags$button(
    type = "button",
    class = "btn btn-outline-danger",
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    label
  )
}

# Percent change against a previous period. Hidden when there is nothing real
# to compare, including an empty current period (that case is -100%).
kpi_delta_parts <- function(current, previous) {
  current <- suppressWarnings(as.numeric(current)[1])
  previous <- suppressWarnings(as.numeric(previous)[1])
  if (!is.finite(current) || !is.finite(previous)) return(NULL)
  if (previous == 0 || current == 0) return(NULL)
  pct <- (current - previous) / abs(previous) * 100
  if (!is.finite(pct)) return(NULL)
  list(pct = pct, up = pct >= 0)
}

kpi_delta <- function(current, previous, lower_is_better = FALSE) {
  parts <- kpi_delta_parts(current, previous)
  if (is.null(parts)) return(NULL)
  shown <- round(abs(parts$pct))
  if (!is.finite(shown) || shown == 0) return(NULL)
  up <- isTRUE(parts$up)
  improved <- if (isTRUE(lower_is_better)) !up else up
  span(
    class = paste("kpi-delta", if (improved) "kpi-delta-up" else "kpi-delta-down"),
    sprintf("%s %.0f%%", if (up) "\u25b2" else "\u25bc", shown)
  )
}

kpi_tone_class <- function(tone = NULL, status = NULL) {
  key <- tolower(as.character(if (!is.null(tone) && nzchar(as.character(tone))) tone else status %||% ""))
  if (!nzchar(key)) return("")
  if (grepl("^tone-", key)) return(key)
  switch(key,
         under_review = "tone-review", review = "tone-review",
         pending = "tone-pending",
         paid = "tone-paid",
         cancelled = "tone-cancelled", rejected = "tone-cancelled",
         days = "tone-days",
         "")
}

kpi_blank <- function(x) {
  if (is.null(x)) return(TRUE)
  if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) return(FALSE)
  if (is.list(x)) return(!length(x))
  !nzchar(trimws(paste(as.character(x), collapse = "")))
}

# Shared status/metric card. Optional rows are omitted so the card closes up.
# `id` makes the whole card the click target (Shiny input id).
KpiCard <- function(label, value, secondaryValue = NULL, subtext = NULL,
                    icon = NULL, onIconClick = NULL, action = NULL, delta = NULL,
                    tone = NULL, status = NULL, loading = FALSE, error = NULL,
                    onClick = NULL, id = NULL) {
  failed <- isTRUE(loading) || isTRUE(error) || (is.character(error) && length(error) && nzchar(error[[1]]))
  if (failed) {
    value <- "No data"
    secondaryValue <- NULL
    subtext <- NULL
    delta <- NULL
  }
  tone_class <- kpi_tone_class(tone, status)
  click_id <- id %||% onClick %||% action
  if (!is.null(click_id) && !nzchar(as.character(click_id))) click_id <- NULL
  icon_id <- onIconClick
  if (!is.null(icon_id) && (is.null(click_id) || identical(as.character(icon_id), as.character(click_id)))) {
    icon_id <- NULL
  }
  icon_el <- NULL
  if (!is.null(icon)) {
    if (!is.null(icon_id)) {
      icon_el <- actionButton(as.character(icon_id), icon, class = "kpi-card-icon")
    } else {
      icon_el <- span(class = "kpi-card-icon", icon)
    }
  }
  sub_bits <- list()
  if (!kpi_blank(subtext)) sub_bits <- c(sub_bits, list(subtext))
  if (!kpi_blank(delta)) {
    if (length(sub_bits)) sub_bits <- c(sub_bits, list(" \u00b7 "))
    sub_bits <- c(sub_bits, list(delta))
  }
  body <- tagList(
    div(class = "kpi-card-top",
        span(class = "kpi-card-label", label),
        icon_el),
    div(class = "kpi-card-value", value),
    if (!kpi_blank(secondaryValue)) div(class = "kpi-card-secondary", secondaryValue),
    if (length(sub_bits)) div(class = "kpi-card-sub", sub_bits)
  )
  cls <- paste("kpi-card", tone_class, if (!is.null(click_id) || !is.null(icon_id)) "is-interactive")
  if (!is.null(click_id)) {
    actionButton(as.character(click_id), body, class = cls)
  } else {
    div(class = cls, body)
  }
}

kpi_grid <- function(...) {
  div(class = "kpi-grid", ...)
}

# Tooltip content is supplied by the caller. Positioning is done in a portal.
info_tooltip <- function(text, label = "About this chart") {
  text <- paste(as.character(text %||% ""), collapse = " ")
  tags$button(
    type = "button",
    class = "info-tip",
    `aria-label` = label,
    `data-tip` = text,
    span(`aria-hidden` = "true", "\u24d8")
  )
}

filter_named <- function(values, labels = NULL, all_label = NULL) {
  values <- as.character(values %||% character())
  if (is.null(labels)) labels <- values
  labels <- as.character(labels)
  if (length(labels) != length(values)) labels <- values
  keep <- !is.na(values) & nzchar(trimws(values)) & values != "__none__"
  values <- values[keep]
  labels <- labels[keep]
  if (length(values)) {
    first <- !duplicated(values)
    values <- values[first]
    labels <- labels[first]
  }
  if (!length(values)) {
    empty <- c("No options found" = "__none__")
    if (is.null(all_label) || !nzchar(all_label)) return(empty)
    return(c(stats::setNames("", all_label), empty))
  }
  named <- stats::setNames(values, labels)
  if (is.null(all_label) || !nzchar(all_label)) return(named)
  c(stats::setNames("", all_label), named)
}

filter_field_ui <- function(field) {
  key <- field$key
  type <- field$type
  label <- field$label %||% NULL
  placeholder <- field$placeholder %||% ""
  default <- as.character(field$default %||% "")
  if (!length(default)) default <- ""
  default <- default[[1]]
  ui <- if (identical(type, "search")) {
    div(class = "filter-search",
        icon("magnifying-glass"),
        textInput(key, NULL, placeholder = placeholder, width = "100%"))
  } else if (identical(type, "chips")) {
    buttons <- lapply(field$options %||% list(), function(opt) {
      actionButton(
        opt$id, opt$label,
        class = paste("status-chip", if (isTRUE(opt$on)) "is-on"),
        `data-status` = opt$value %||% ""
      )
    })
    tagList(
      if (!is.null(label)) tags$label(class = "control-label", label),
      div(class = "status-chips", role = "group", `aria-label` = label %||% "Options", buttons)
    )
  } else if (type %in% c("select", "searchable-select", "multi-select", "month")) {
    multiple <- identical(type, "multi-select") || isTRUE(field$multiple)
    choices <- field$choices %||% character()
    selected <- field$selected %||% if (multiple) character() else default
    selectizeInput(
      key, label, choices = choices, selected = selected, multiple = multiple, width = "100%",
      options = list(
        placeholder = placeholder,
        dropdownParent = "body",
        allowEmptyOption = TRUE
      )
    )
  } else {
    NULL
  }
  div(
    class = "filter-field",
    `data-filter-key` = key,
    `data-filter-type` = type,
    `data-default` = default,
    `data-depends` = if (!is.null(field$depends) && nzchar(field$depends)) field$depends else NULL,
    ui
  )
}

# One filter panel. Each page passes its own field list.
FilterPanel <- function(fields, clear_id, class = "filter-panel", badge = NULL, close_id = NULL) {
  primary <- list()
  advanced <- list()
  for (field in fields) {
    ui <- filter_field_ui(field)
    if (isTRUE(field$advanced)) advanced[[length(advanced) + 1L]] <- ui else primary[[length(primary) + 1L]] <- ui
  }
  at_default <- TRUE
  for (field in fields) {
    selected <- field$selected
    default <- field$default %||% ""
    if (identical(field$type, "chips")) next
    if (identical(field$type, "multi-select") || isTRUE(field$multiple)) {
      if (length(selected) && any(nzchar(as.character(selected)))) at_default <- FALSE
    } else if (!is.null(selected) && !identical(as.character(selected)[1], as.character(default)[1])) {
      at_default <- FALSE
    }
  }
  advanced_ui <- if (length(advanced)) {
    tags$details(
      class = "advanced-filters",
      tags$summary(tagList(icon("sliders"), " Advanced filters")),
      advanced
    )
  }
  tags$aside(
    class = class,
    div(class = "filter-panel-head",
        div(class = "d-flex align-items-center gap-2 min-w-0",
            icon("sliders"), span("Filters"), badge),
        div(class = "d-flex align-items-center gap-1",
            actionButton(
              clear_id, "Clear all",
              class = paste("btn btn-link sidebar-clear filter-clear", if (at_default) "is-muted"),
              `aria-disabled` = if (at_default) "true" else "false"
            ),
            if (!is.null(close_id)) {
              tags$button(type = "button", id = close_id, class = "sidebar-close-btn",
                          title = "Close filters", icon("xmark"))
            })
    ),
    div(class = "filter-panel-body", primary, advanced_ui)
  )
}

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#09090b",
  primary = "#0284c7",
  secondary = "#f4f4f5", # shadcn Zinc
  base_font = font_google("Inter", wght = c(400, 500, 600, 700)),
  heading_font = font_google("Inter", wght = c(600, 700)),
  "border-radius" = "0.6rem"
)

css <- "
:root{
  --app-bg:#f3f5f8; --app-card-bg:#ffffff; --app-input-bg:#ffffff;
  --app-border:#e4e4e7; --app-muted:#f4f4f5; --app-muted-fg:#52525b;
  --app-accent:#0284C7; --app-accent-soft:#e0f2fe; --app-radius:.7rem;
  --nav-bg:#23313F; --nav-fg:#ffffff; --nav-muted:rgba(255,255,255,.78);
  --nav-active:#7f97ff;
  --status-review:#475569; --status-pending:#b45309; --status-paid:#15803d; --status-rejected:#b91c1c;
}
[data-bs-theme='dark']{
  --app-bg:#0b1220; --app-card-bg:#121a2b; --app-input-bg:#121a2b;
  --app-border:#243044; --app-muted:#1a2436; --app-muted-fg:#cbd5e1;
  --app-accent:#38bdf8; --app-accent-soft:#0c4a6e;
  --bs-body-bg:#0b1220; --bs-body-color:#f8fafc; --bs-secondary-color:#cbd5e1;
  --bs-tertiary-bg:#1a2436; --bs-border-color:#243044; --bs-card-bg:#121a2b;
  --nav-bg:#23313F; --nav-fg:#ffffff; --nav-muted:rgba(255,255,255,.78);
  --nav-active:#9aabff;
  --status-review:#cbd5e1; --status-pending:#fbbf24; --status-paid:#4ade80; --status-rejected:#f87171;
}
html,body,.navbar,.navbar-brand,.nav-link,.btn,.form-control,.form-select,.selectize-input,.selectize-dropdown,table.dataTable,.dataTables_wrapper,.modal,.card,h1,h2,h3,h4,.brand-name,.brand-sub,.kpi-card{
  font-family:Inter,ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif!important;
}
body{background:var(--app-bg);color:var(--bs-body-color);font-size:.92rem;font-weight:400;letter-spacing:-.011em}
.bslib-grid>*{min-width:0}
.navbar{border-bottom:0;box-shadow:none;background:var(--nav-bg)!important;color:var(--nav-fg)!important}
.navbar .container-fluid{max-width:1800px}
.navbar .navbar-brand,.navbar .nav-link,.navbar .navbar-toggler{color:#fff!important}
.navbar .nav-link{font-weight:400!important;padding:.45rem .7rem!important;color:#fff!important}
.navbar .nav-link.active{color:#fff!important;font-weight:400!important;background:transparent!important;box-shadow:inset 0 -2px 0 var(--nav-active)}
.navbar .nav-link[data-value='new'],.navbar .nav-link[data-value='manage']{display:none!important}
.navbar .navbar-brand,a.brand{display:inline-flex!important;flex-direction:row!important;align-items:center!important;gap:.55rem!important;white-space:nowrap;color:var(--nav-fg)!important;text-decoration:none;line-height:1}
.brand-mark{width:2.15rem;height:2.15rem;flex:0 0 auto;border-radius:.5rem;background:#0284C7;color:#fff;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:.95rem;letter-spacing:-.04em}
.brand-copy{display:flex!important;flex-direction:column!important;align-items:flex-start;justify-content:center;line-height:1;width:auto}
.brand-name{display:block;font-weight:700;letter-spacing:-.03em;font-size:1rem;line-height:1.05;color:#fff}
.brand-sub{display:block;margin-top:.14rem;font-size:.48rem;font-weight:600;letter-spacing:.08em;line-height:1;text-transform:uppercase;color:rgba(255,255,255,.72);white-space:nowrap}
.nav-icon-btn{width:2.3rem;height:2.3rem;padding:0!important;display:inline-flex!important;align-items:center;justify-content:center;border-radius:.5rem!important;border:1px solid rgba(255,255,255,.22)!important;background:transparent!important;color:#fff!important}
.nav-icon-btn:hover,.nav-icon-btn:focus{background:rgba(255,255,255,.08)!important;color:#fff!important}
.navbar .bslib-input-dark-mode, .navbar .bslib-input-dark-mode .btn{color:#fff!important;border:1px solid rgba(255,255,255,.32)!important;background:transparent!important;border-radius:.55rem!important;width:2.3rem;height:2.3rem;display:inline-flex!important;align-items:center;justify-content:center;padding:0!important}
.navbar .bslib-input-dark-mode .btn svg,.navbar .bslib-input-dark-mode .btn svg *,.navbar .bslib-input-dark-mode .btn i{color:#fff!important;fill:#fff!important;stroke:#fff!important;opacity:1!important}
.page-shell{width:100%;max-width:1760px;margin:0 auto;padding:1.3rem clamp(1rem,2.2vw,2.5rem) 1.5rem;display:flex;flex-direction:column}
.page-heading{display:flex;align-items:center;justify-content:space-between;gap:1rem;flex-wrap:wrap;margin-bottom:1.15rem}
.page-heading h1{font-size:1.85rem;font-weight:700;letter-spacing:-.045em;margin:0}
.page-heading p{color:var(--app-muted-fg);margin:.28rem 0 0;font-size:.9rem;font-weight:400;letter-spacing:-.011em}
.toolbar{display:flex;align-items:center;gap:.65rem;flex-wrap:wrap;margin:1.15rem 0 1.2rem}
.toolbar .shiny-input-container{margin:0}
.toolbar .btn{min-height:2.45rem}
.filter-dd .dropdown-menu{width:min(94vw,600px);max-height:75vh;overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);box-shadow:0 14px 35px rgba(0,0,0,.12)}
.fgrid{display:grid;grid-template-columns:1fr 1fr;gap:.15rem .85rem}
.fgrid .wide{grid-column:1/-1}
.fgrid .shiny-input-container{width:100%;margin:.15rem 0 .65rem}
.fgrid .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
@media(max-width:620px){.fgrid{grid-template-columns:1fr}.fgrid .wide{grid-column:auto}}
.btn-primary{--bs-btn-bg:#0284C7;--bs-btn-border-color:#0284C7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1;--bs-btn-active-bg:#0284C7;background-color:#0284C7!important;border-color:#0284C7!important;font-weight:600;color:#fff!important;border-radius:.55rem}
.btn-primary:hover{background-color:#0369a1!important;border-color:#0369a1!important;color:#fff!important}
.btn-primary:focus,.btn-primary:active,.btn-primary:disabled,[data-bs-theme='dark'] .btn-primary{background-color:#0284C7!important;border-color:#0284C7!important;color:#fff!important}
[data-bs-theme='dark'] .btn-primary{--bs-btn-bg:#0284C7;--bs-btn-border-color:#0284C7;--bs-btn-hover-bg:#0369a1;--bs-btn-hover-border-color:#0369a1}
.btn-outline-secondary{border:1px solid var(--app-border)!important;color:var(--bs-body-color)!important;font-weight:500;border-radius:.55rem;background:transparent!important}
.btn-outline-secondary:hover,.btn-outline-secondary:focus{background:var(--app-muted)!important;border-color:var(--app-muted-fg)!important;color:var(--bs-body-color)!important}
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
.table-head{width:100%;display:flex;align-items:center;justify-content:space-between;gap:.8rem;flex-wrap:wrap}
.table-head .buttons{margin-left:auto;display:flex;align-items:center;gap:.5rem;flex-wrap:wrap}
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
.dt-top{display:flex;justify-content:flex-end;align-items:center;gap:.65rem;flex-wrap:wrap;margin-bottom:.8rem}
.dt-top .dataTables_filter{order:2}
.dt-status-control{display:flex;align-items:center;gap:.45rem;order:1}
.dt-status-control label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg);margin:0;white-space:nowrap}
.dt-status-control select{min-width:175px;border:1px solid var(--app-border);border-radius:.5rem;padding:.48rem .7rem;background:var(--bs-body-bg);color:var(--bs-body-color)}
.dt-status-control select:focus{outline:0;border-color:var(--app-accent);box-shadow:0 0 0 3px rgba(2,132,199,.14)}
@media(max-width:640px){.dt-top{align-items:stretch;justify-content:stretch}.dt-status-control{width:100%;order:1}.dt-status-control select{flex:1;min-width:0}.dt-top .dataTables_filter{width:100%;order:2}.dt-top .dataTables_filter input{width:100%;min-width:0}}
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
.form-hint{font-size:.78rem;color:var(--app-muted-fg);margin:-.2rem 0 .65rem}
.form-section-title{font-weight:650;font-size:.95rem;margin:.65rem 0 .8rem}
.form-section-rule{border-top:1px solid var(--app-border);margin:.5rem 0 1rem}
.action-bar{position:sticky;bottom:0;z-index:5;display:flex;align-items:center;justify-content:flex-end;gap:.65rem;padding:.9rem 0 .1rem;margin-top:.5rem;background:var(--bs-body-bg);border-top:1px solid var(--app-border)}
.modal-content{border:1px solid var(--app-border);border-radius:.8rem;background:var(--bs-body-bg);height:auto;overflow:hidden}
.modal-header{background:#23313F!important;color:#ffffff!important;border-color:rgba(255,255,255,.12)!important;padding:.75rem 1.25rem;border-top-left-radius:.8rem;border-top-right-radius:.8rem}
.modal-header .modal-title,.modal-header h4,.modal-header h5,.modal-header span,.modal-header div{color:#ffffff!important}
.modal-header .btn-close{filter:invert(1) grayscale(100%) brightness(200%)!important}
.modal-footer{border-color:var(--app-border);padding:.45rem .85rem;gap:.5rem}
.modal-footer .btn{margin:0}
.modal-footer .btn-default:not(.btn-primary):not(.btn-danger):not(.btn-outline-danger):not(.btn-outline-secondary){background:transparent!important;border:1px solid var(--app-border)!important;color:var(--bs-body-color)!important;font-weight:500!important;padding:.38rem .85rem!important;border-radius:.5rem!important;display:inline-flex!important;align-items:center!important;gap:.4rem!important}
.modal-footer .btn-default:not(.btn-primary):not(.btn-danger):not(.btn-outline-danger):not(.btn-outline-secondary):hover,.modal-footer .btn-default:not(.btn-primary):not(.btn-danger):not(.btn-outline-danger):not(.btn-outline-secondary):focus{background:var(--app-muted)!important;border-color:var(--app-muted-fg)!important;color:var(--bs-body-color)!important}
[data-bs-theme='dark'] .modal-footer .btn-default:not(.btn-primary):not(.btn-danger):not(.btn-outline-danger):not(.btn-outline-secondary){border-color:#71717a!important;color:#fafafa!important}
[data-bs-theme='dark'] .modal-footer .btn-default:not(.btn-primary):not(.btn-danger):not(.btn-outline-danger):not(.btn-outline-secondary):hover{background:#27272a!important;color:#ffffff!important}
.modal-footer .btn.btn-primary{background-color:#0284C7!important;border:1px solid #0284C7!important;color:#fff!important;font-weight:600!important;padding:.38rem .85rem!important;border-radius:.5rem!important;display:inline-flex!important;align-items:center!important;gap:.4rem!important}
.modal-footer .btn.btn-primary:hover,.modal-footer .btn.btn-primary:focus{background-color:#0369a1!important;border-color:#0369a1!important;color:#fff!important}
.modal-footer .btn.btn-outline-secondary{background:transparent!important;border:1px solid var(--app-border)!important;color:var(--bs-body-color)!important;font-weight:500!important;padding:.38rem .85rem!important;border-radius:.5rem!important;display:inline-flex!important;align-items:center!important;gap:.4rem!important}
.modal-footer .btn.btn-outline-secondary:hover,.modal-footer .btn.btn-outline-secondary:focus{background:var(--app-muted)!important;border-color:var(--app-muted-fg)!important;color:var(--bs-body-color)!important}
.modal-footer .btn.btn-outline-danger{background:transparent!important;border:1px solid #dc2626!important;color:#dc2626!important;font-weight:500!important;padding:.38rem .85rem!important;border-radius:.5rem!important;display:inline-flex!important;align-items:center!important;gap:.4rem!important}
.modal-footer .btn.btn-outline-danger:hover,.modal-footer .btn.btn-outline-danger:focus{background:rgba(220,38,38,.08)!important;border-color:#b91c1c!important;color:#b91c1c!important}
.modal-footer .btn.btn-danger{background-color:#dc2626!important;border:1px solid #dc2626!important;color:#fff!important;font-weight:600!important;padding:.38rem .85rem!important;border-radius:.5rem!important;display:inline-flex!important;align-items:center!important;gap:.4rem!important}
.modal-footer .btn.btn-danger:hover,.modal-footer .btn.btn-danger:focus{background-color:#b91c1c!important;border-color:#b91c1c!important;color:#fff!important}
[data-bs-theme='dark'] .modal-footer .btn.btn-outline-secondary{border-color:#71717a!important;color:#fafafa!important}
[data-bs-theme='dark'] .modal-footer .btn.btn-outline-secondary:hover{background:#27272a!important;color:#ffffff!important}
[data-bs-theme='dark'] .modal-footer .btn.btn-outline-danger{border-color:#f87171!important;color:#f87171!important}
[data-bs-theme='dark'] .modal-footer .btn.btn-outline-danger:hover{background:rgba(220,38,38,.16)!important;color:#fecaca!important}
.modal-dialog{width:auto!important;max-width:34rem!important;display:flex!important;align-items:center!important;justify-content:center!important;min-height:calc(100% - 3.5rem)!important;margin:1.75rem auto!important}
.modal-dialog.modal-lg{width:min(1280px,calc(100vw - 1rem))!important;max-width:min(1280px,calc(100vw - 1rem))!important;margin:1.75rem auto!important}
body.modal-open:has(.modal.show) .requests-page,body.modal-open:has(.modal.show) .dashboard-shell,body.modal-open:has(.modal.show) .fin-page,body.modal-open:has(.modal.show) .manage-layout,body.modal-open:has(.modal.show) .navbar,body.modal-open:has(.modal.show) .app-footer{filter:blur(4px) grayscale(15%)!important;pointer-events:none!important;user-select:none!important;transition:filter .2s ease}
.modal-backdrop.show{opacity:.65!important;backdrop-filter:blur(5px)!important;-webkit-backdrop-filter:blur(5px)!important;background-color:rgba(15,23,42,.65)!important}
#btn_pay_confirm:disabled{opacity:.45!important;cursor:not-allowed!important;pointer-events:none!important}
.modal-body{overflow:visible;padding:.8rem 1rem .45rem}
.modal-dialog.modal-lg .modal-header{padding:.5rem 1rem}
.modal-dialog.modal-lg .modal-body{padding:.65rem 1rem .2rem}
.modal-dialog.modal-lg .modal-footer{padding:.35rem .85rem}
.ref{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.8rem;word-break:break-word;color:var(--app-muted-fg)}
.reference-panel{margin-top:1rem;padding:1rem;border:1px solid var(--app-border);border-radius:.65rem;background:var(--app-muted)}
.reference-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:.8rem}
.request-view-container .form-section-title{margin:.15rem 0 .4rem}
.request-view-container .reference-panel{margin-top:.5rem;padding:.55rem .7rem}
.request-view-container .reference-grid{gap:.5rem}
.request-view-container .reference-item{padding:.45rem .6rem}
.request-details-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:.45rem .65rem;margin-top:.3rem}
.detail-item{background:var(--app-muted);padding:.42rem .65rem;border-radius:.5rem;border:1px solid var(--app-border)}
.detail-item.wide{grid-column:1/-1}
.detail-item label{display:block;font-size:.72rem;font-weight:600;text-transform:uppercase;letter-spacing:.03em;color:var(--app-muted-fg);margin-bottom:.2rem}
.detail-val{font-size:.88rem;font-weight:550;color:var(--bs-body-color);word-break:break-word}
@media(max-width:768px){.request-details-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:480px){.request-details-grid{grid-template-columns:1fr}}
@media(min-width:900px){.request-details-grid{grid-template-columns:repeat(4,minmax(0,1fr))}}
.status-pill{font-size:.78rem;font-weight:600;padding:.35rem .65rem;border-radius:1rem}
.status-waiting,.status-under_review{background:color-mix(in srgb, var(--status-review) 16%, transparent);color:var(--status-review)}
.status-under-validation,.status-pending{background:color-mix(in srgb, var(--status-pending) 16%, transparent);color:var(--status-pending)}
.status-paid{background:color-mix(in srgb, var(--status-paid) 16%, transparent);color:var(--status-paid)}
.status-rejected,.status-cancelled{background:color-mix(in srgb, var(--status-rejected) 16%, transparent);color:var(--status-rejected)}
.reference-item{min-width:0;padding:.8rem;border:1px solid var(--app-border);border-radius:.55rem;background:var(--bs-body-bg)}
.reference-label{font-size:.76rem;font-weight:650;color:var(--app-muted-fg);margin-bottom:.35rem}
.reference-value{display:block;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:.78rem;line-height:1.5;overflow-wrap:anywhere;color:var(--bs-body-color);white-space:normal}
.reference-hint{font-size:.76rem;color:var(--app-muted-fg);margin:.7rem 0 0}
@media(max-width:560px){.reference-grid{grid-template-columns:1fr}}
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
body{background:var(--app-bg)!important;color:var(--bs-body-color)!important}
.navbar{background:var(--nav-bg)!important;color:var(--nav-fg)!important}
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
.selectize-dropdown.insights-portal{position:fixed!important;z-index:4000!important;overflow-y:auto}
.selectize-dropdown.insights-portal .selectize-dropdown-content{max-height:none!important}
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
.btn-danger{--bs-btn-bg:#dc2626;--bs-btn-border-color:#dc2626;--bs-btn-hover-bg:#b91c1c;--bs-btn-hover-border-color:#b91c1c;font-weight:600;color:#fff!important}
.btn-danger:hover,.btn-danger:focus,.btn-danger:active{color:#fff!important}
.weekly-report-card{margin-bottom:1rem;max-width:100%;overflow:hidden}
.weekly-report-card summary{list-style:none}
.weekly-report-card summary::-webkit-details-marker{display:none}
.weekly-report-card .card-body{overflow-x:auto}
.weekly-report-card table.dataTable{width:100%!important;table-layout:fixed!important}
.weekly-report-card table.dataTable th,.weekly-report-card table.dataTable td{white-space:nowrap!important;overflow:hidden!important;text-overflow:ellipsis!important}

/* Redesigned information architecture: persistent request filters, dashboard workspace, management pages. */
.navbar .nav-link[data-value='requests'],.navbar .nav-link[data-value='dashboards'],.navbar .nav-link[data-value='financial_update'],.navbar .nav-link[data-value='manage']{font-weight:400!important;color:#fff!important}
.navbar .nav-link.active[data-value='requests'],.navbar .nav-link.active[data-value='dashboards'],.navbar .nav-link.active[data-value='financial_update'],.navbar .nav-link.active[data-value='manage']{color:#fff!important;font-weight:400!important;background:transparent!important;box-shadow:inset 0 -2px 0 var(--nav-active)}
.navbar .nav-link[data-value='new'],.navbar .nav-link[data-value='manage']{display:none!important}
.global-currency-wrap .shiny-input-container{margin:0!important;width:8.6rem!important}
.navbar .global-currency-wrap .form-select{appearance:none!important;-webkit-appearance:none!important;width:100%;height:2.3rem;min-height:2.3rem;padding:.15rem 1.7rem .15rem .7rem!important;font-size:.84rem!important;font-weight:600!important;line-height:1.2;border-radius:.55rem!important;border:1px solid rgba(255,255,255,.32)!important;background-color:transparent!important;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 16 16'%3E%3Cpath fill='none' stroke='%23ffffff' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.8' d='M3.5 6.5 8 11l4.5-4.5'/%3E%3C/svg%3E\")!important;background-repeat:no-repeat!important;background-position:right .6rem center!important;background-size:12px 12px!important;color:#fff!important;box-shadow:none!important}
.navbar .global-currency-wrap .form-select option{color:#111;background:#fff}
.dashboard-filter-section{margin-bottom:.5rem}
.tracking-wide{letter-spacing:.04em}
.requests-page{max-width:1900px;width:100%;margin:0 auto;padding:1.2rem clamp(.8rem,1.6vw,1.8rem) 1.25rem;flex:1 1 auto;display:flex;flex-direction:column}
.requests-layout{display:grid;grid-template-columns:minmax(0,1fr) minmax(248px,268px);gap:1rem;align-items:stretch;margin:0;padding:0;min-height:0;flex:1 1 auto;transition:grid-template-columns .2s ease}
.requests-layout.sidebar-collapsed{grid-template-columns:minmax(0,1fr) 0;gap:0}
.requests-layout.sidebar-collapsed .requests-sidebar{display:none!important}
.requests-sidebar{position:sticky;top:1rem;align-self:start;height:auto;max-height:calc(100vh - 6rem);overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg)}
.requests-sidebar-head{display:flex;align-items:center;gap:.55rem;padding:1rem 1rem .85rem;border-bottom:1px solid var(--app-border);font-size:.96rem;font-weight:650}
.requests-sidebar-body{padding:1rem}
.requests-sidebar .shiny-input-container{width:100%;margin:0 0 .8rem}
.requests-sidebar .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
.requests-sidebar .form-control,.requests-sidebar .form-select,.requests-sidebar .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.requests-sidebar .sidebar-divider{height:1px;background:var(--app-border);margin:.9rem 0}
.requests-main{min-width:0;grid-column:1;grid-row:1}
.requests-sidebar{grid-column:2;grid-row:1}
.sidebar-close-btn{border:none!important;background:transparent!important;color:var(--app-muted-fg)!important;padding:.2rem .4rem;font-size:1.05rem;line-height:1;border-radius:.35rem;cursor:pointer;display:inline-flex;align-items:center;justify-content:center}
.sidebar-close-btn:hover{color:var(--bs-body-color)!important;background:var(--app-muted)!important}
.filter-toggle-btn.is-active{background:var(--app-muted)!important;color:var(--app-accent)!important;border-color:var(--app-accent)!important}
.kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,15.5rem),1fr));gap:.75rem;align-items:stretch;margin:0 0 1rem}
.kpi-card{display:flex;flex-direction:column;align-items:stretch;width:100%;height:100%;min-width:0;text-align:left;padding:.9rem 1rem .85rem;border:1px solid var(--app-border)!important;border-radius:.75rem;background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;white-space:normal;box-shadow:0 1px 2px rgba(15,23,42,.04)!important;outline:none}
button.btn.kpi-card,button.btn.kpi-card:hover,button.btn.kpi-card:focus,button.btn.kpi-card:active{background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;font-weight:inherit;line-height:inherit;text-align:left;border-right-color:var(--app-border)!important;border-bottom-color:var(--app-border)!important;border-left-color:var(--app-border)!important;box-shadow:0 1px 2px rgba(15,23,42,.04)!important;outline:none!important}
.kpi-card.is-interactive:focus-visible{outline:2px solid var(--app-accent)!important;outline-offset:2px}
.kpi-card-top{display:flex;align-items:flex-start;justify-content:space-between;gap:.5rem}
.kpi-card-icon,.kpi-card .kpi-card-icon.btn{width:1.85rem;height:1.85rem;margin:0;padding:0;border:0!important;border-radius:.5rem;display:inline-flex;align-items:center;justify-content:center;background:var(--app-muted)!important;color:var(--app-muted-fg)!important;flex:0 0 auto;box-shadow:none!important;line-height:1}
.kpi-card.tone-review,
.kpi-card.tone-pending,
.kpi-card.tone-paid,
.kpi-card.tone-cancelled,
.kpi-card.tone-days,
button.btn.kpi-card.tone-review,
button.btn.kpi-card.tone-pending,
button.btn.kpi-card.tone-paid,
button.btn.kpi-card.tone-cancelled,
button.btn.kpi-card.tone-days,
button.btn.kpi-card.tone-review:hover,
button.btn.kpi-card.tone-pending:hover,
button.btn.kpi-card.tone-paid:hover,
button.btn.kpi-card.tone-cancelled:hover,
button.btn.kpi-card.tone-days:hover,
button.btn.kpi-card.tone-review:focus,
button.btn.kpi-card.tone-pending:focus,
button.btn.kpi-card.tone-paid:focus,
button.btn.kpi-card.tone-cancelled:focus,
button.btn.kpi-card.tone-days:focus,
button.btn.kpi-card.tone-review:active,
button.btn.kpi-card.tone-pending:active,
button.btn.kpi-card.tone-paid:active,
button.btn.kpi-card.tone-cancelled:active,
button.btn.kpi-card.tone-days:active{border-top:1px solid var(--app-border)!important;border-bottom:1px solid var(--app-border)!important}
.kpi-card.tone-review.is-active,
button.btn.kpi-card.tone-review.is-active,
button.btn.kpi-card.tone-review.is-active:hover,
button.btn.kpi-card.tone-review.is-active:focus,
button.btn.kpi-card.tone-review.is-active:active{border-bottom:3px solid var(--status-review)!important}
.kpi-card.tone-pending.is-active,
button.btn.kpi-card.tone-pending.is-active,
button.btn.kpi-card.tone-pending.is-active:hover,
button.btn.kpi-card.tone-pending.is-active:focus,
button.btn.kpi-card.tone-pending.is-active:active{border-bottom:3px solid var(--status-pending)!important}
.kpi-card.tone-paid.is-active,
button.btn.kpi-card.tone-paid.is-active,
button.btn.kpi-card.tone-paid.is-active:hover,
button.btn.kpi-card.tone-paid.is-active:focus,
button.btn.kpi-card.tone-paid.is-active:active{border-bottom:3px solid var(--status-paid)!important}
.kpi-card.tone-cancelled.is-active,
button.btn.kpi-card.tone-cancelled.is-active,
button.btn.kpi-card.tone-cancelled.is-active:hover,
button.btn.kpi-card.tone-cancelled.is-active:focus,
button.btn.kpi-card.tone-cancelled.is-active:active{border-bottom:3px solid var(--status-rejected)!important}
.kpi-card.tone-days.is-active,
button.btn.kpi-card.tone-days.is-active,
button.btn.kpi-card.tone-days.is-active:hover,
button.btn.kpi-card.tone-days.is-active:focus,
button.btn.kpi-card.tone-days.is-active:active{border-bottom:3px solid #38bdf8!important}
.insights-page .kpi-card{min-height:calc(1.85rem + .45rem + 1.9rem * 1.05 + .35rem + .95rem * 1.25 + .15rem + .75rem * 1.35 + .9rem + .85rem + 4px)}
.kpi-card-label{display:flex;align-items:center;gap:.45rem;color:var(--app-muted-fg);font-size:.78rem;font-weight:600;margin:0;line-height:1.3}
.kpi-card-value{margin-top:.45rem;font-size:1.9rem;line-height:1.05;letter-spacing:-.05em;font-weight:700;overflow-wrap:anywhere}
.kpi-card-secondary{margin-top:.35rem;font-weight:650;font-size:.95rem;letter-spacing:-.02em;line-height:1.25}
.kpi-card-sub{margin-top:.15rem;font-size:.75rem;color:var(--app-muted-fg);font-weight:600;line-height:1.35}
.kpi-delta{font-weight:700}
.kpi-delta-up{color:#10b981}
.kpi-delta-down{color:#ef4444}
.status-overdue-pill{display:inline-flex;align-items:center;gap:.35rem;padding:.2rem .65rem;border-radius:1rem;font-size:.74rem;font-weight:650;background:rgba(239,68,68,.12);border:1px solid rgba(239,68,68,.35);color:#dc2626;margin-top:.4rem;line-height:1.2}
.status-overdue-pill i,.status-overdue-pill svg{font-size:.76rem}
[data-bs-theme='dark'] .status-overdue-pill{background:rgba(239,68,68,.18);border-color:rgba(239,68,68,.45);color:#f87171}
.req-status-cell{display:inline-flex;flex-direction:column;align-items:center;justify-content:center;gap:.25rem;vertical-align:middle}
.req-overdue-flag{display:inline-flex;align-items:center;justify-content:center;gap:.28rem;font-size:.74rem;font-weight:600;color:#dc2626;line-height:1}
.req-overdue-flag i,.req-overdue-flag svg{font-size:.72rem}
[data-bs-theme='dark'] .req-overdue-flag{color:#f87171}
.req-pill{display:inline-flex;align-items:center;justify-content:center;min-width:6.4rem;font-size:.75rem;font-weight:650;padding:.32rem .7rem;border-radius:999px;white-space:nowrap}
.req-pill-under_review{background:color-mix(in srgb, var(--status-review) 16%, transparent);color:var(--status-review)}
.req-pill-pending{background:color-mix(in srgb, var(--status-pending) 16%, transparent);color:var(--status-pending)}
.req-pill-paid{background:color-mix(in srgb, var(--status-paid) 16%, transparent);color:var(--status-paid)}
.req-pill-cancelled{background:color-mix(in srgb, var(--status-rejected) 16%, transparent);color:var(--status-rejected)}
.req-ref{color:var(--app-accent);font-weight:650;font-size:.8rem}
.status-chips{display:flex;flex-wrap:wrap;gap:.35rem;margin:0 0 .85rem}
.status-chip.btn{border:1px solid var(--app-border)!important;background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;border-radius:999px!important;padding:.2rem .7rem!important;font-size:.78rem;font-weight:650;line-height:1.4}
.status-chip.btn.is-on{background:var(--app-accent)!important;border-color:var(--app-accent)!important;color:#fff!important}
.status-chip.btn[data-status='under_review']::before,.status-chip.btn[data-status='pending']::before,.status-chip.btn[data-status='paid']::before,.status-chip.btn[data-status='cancelled']::before{content:'';display:inline-block;width:.5rem;height:.5rem;margin-right:.35rem;border-radius:100%;vertical-align:-.05rem}
.status-chip.btn[data-status='under_review']::before{background:var(--status-review)}
.status-chip.btn[data-status='pending']::before{background:var(--status-pending)}
.status-chip.btn[data-status='paid']::before{background:var(--status-paid)}
.status-chip.btn[data-status='cancelled']::before{background:var(--status-rejected)}
.status-chip.btn[data-status='under_review'].is-on{background:color-mix(in srgb, var(--status-review) 18%, transparent)!important;border-color:var(--status-review)!important;color:var(--status-review)!important}
.status-chip.btn[data-status='pending'].is-on{background:color-mix(in srgb, var(--status-pending) 18%, transparent)!important;border-color:var(--status-pending)!important;color:var(--status-pending)!important}
.status-chip.btn[data-status='paid'].is-on{background:color-mix(in srgb, var(--status-paid) 18%, transparent)!important;border-color:var(--status-paid)!important;color:var(--status-paid)!important}
.status-chip.btn[data-status='cancelled'].is-on{background:color-mix(in srgb, var(--status-rejected) 18%, transparent)!important;border-color:var(--status-rejected)!important;color:var(--status-rejected)!important}
.sidebar-clear.btn{border:0!important;background:transparent!important;color:var(--app-accent)!important;font-weight:650;padding:.1rem .2rem!important;font-size:.82rem;text-decoration:none}
.filter-clear.is-muted,.filter-clear[aria-disabled='true']{color:var(--app-muted-fg)!important;opacity:.55;pointer-events:none;cursor:default}
.advanced-filters{margin-top:.35rem;border-top:1px solid var(--app-border);padding-top:.7rem}
.advanced-filters summary{cursor:pointer;font-weight:650;font-size:.84rem;color:var(--app-accent);margin-bottom:0;user-select:none;outline:none}
.advanced-filters[open] summary{margin-bottom:.7rem}
.advanced-filters summary i,.advanced-filters summary svg{margin-left:.35rem;margin-right:.25rem}
.filter-search{position:relative}
.filter-search>.fa,.filter-search>svg,.filter-search .fa-magnifying-glass{position:absolute;left:.75rem;top:50%;transform:translateY(-50%);z-index:2;pointer-events:none;color:var(--app-muted-fg);font-size:.8rem;margin:0}
.filter-search .shiny-input-container{position:relative;margin:0}
.filter-search label.shiny-label-null,.filter-search label.control-label:empty{display:none!important}
.filter-search .form-control{padding-left:2.15rem;min-height:2.45rem}
.requests-filter-toggle{display:none!important}
.status-dot{display:inline-block;width:.55rem;height:.55rem;border-radius:100%;flex:0 0 auto}
.status-dot-all{background:var(--app-accent)}.status-dot-waiting{background:var(--status-review)}.status-dot-validation{background:var(--status-pending)}.status-dot-paid{background:var(--status-paid)}.status-dot-rejected{background:var(--status-rejected)}
.request-table-card{min-height:480px;height:100%;display:flex;flex-direction:column}
.request-table-card>.card-header{flex:0 0 auto}
.request-table-card>.card-body,.request-table-card .card-body{padding:.65rem 1rem .85rem;flex:1 1 auto;display:flex;flex-direction:column;min-height:0}
.request-table-card .dataTables_wrapper{flex:1 1 auto;display:flex;flex-direction:column;min-height:100%}
.request-table-card .dt-foot{margin-top:auto}
.dashboard-layout{display:grid;grid-template-columns:minmax(0,1fr) minmax(260px,310px);gap:1.15rem;align-items:start;max-width:1900px;margin:0 auto;padding:1.25rem clamp(1rem,2.1vw,2.4rem) 2rem;transition:grid-template-columns .2s ease}
.dashboard-layout.sidebar-collapsed{grid-template-columns:minmax(0,1fr) 0;gap:0}
.dashboard-layout.sidebar-collapsed .dashboard-sidebar{display:none!important}
@media(max-width:860px){
  .requests-filter-toggle{display:inline-flex!important}
  .requests-page{position:relative}
  .requests-layout{display:block}
  .dashboard-layout{display:block;position:relative}
  .requests-sidebar{display:none;position:absolute;top:4.6rem;right:clamp(.5rem,1.8vw,1.5rem);left:auto;z-index:1050;width:min(94vw,360px);max-height:80vh;overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg)!important;box-shadow:0 16px 40px rgba(0,0,0,.35)}
  .requests-sidebar.is-open{display:block!important;animation:popoverFadeIn .18s ease-out}
  .requests-sidebar-head{display:none!important}
  .requests-sidebar-body{display:grid;grid-template-columns:1fr 1fr;gap:.2rem .85rem;padding:1rem 1.15rem 1.15rem}
  .requests-sidebar-body .wide{grid-column:1/-1}
  .requests-sidebar-body .sidebar-divider{display:none}
  .requests-sidebar-body .clear-filter-row{grid-column:1/-1;display:flex;justify-content:flex-end;margin-top:.4rem}
  .requests-sidebar-body #reset{width:auto!important;padding:.35rem .85rem;font-size:.85rem}
  .dashboard-sidebar{display:none;position:absolute;top:5.5rem;right:clamp(.5rem,2vw,1.5rem);z-index:1050;width:min(94vw,540px);max-height:80vh;overflow-y:auto;border-radius:var(--app-radius);box-shadow:0 16px 40px rgba(0,0,0,.35)}
  .dashboard-sidebar.is-open{display:block!important;animation:popoverFadeIn .18s ease-out}
  .dashboard-sidebar .dashboard-filter-card{border:1px solid var(--app-border)!important}
  .dashboard-sidebar .card-header{display:none!important}
  .dashboard-sidebar .card-body{display:grid;grid-template-columns:1fr 1fr;gap:.25rem .85rem;padding:1.15rem}
  .dashboard-sidebar .card-body .chart-note{grid-column:1/-1}
}
@media(max-width:600px){
  .requests-sidebar-body,.dashboard-sidebar .card-body{grid-template-columns:1fr}
  .requests-sidebar-body .clear-filter-row #reset{width:100%!important}
}
@keyframes popoverFadeIn{from{opacity:0;transform:translateY(-6px)}to{opacity:1;transform:translateY(0)}}
.dashboard-main{min-width:0}
.dashboard-sidebar{position:sticky;top:1rem;max-height:calc(100vh - 4rem);overflow-y:auto}
.dashboard-filter-card{border:1px solid var(--app-border)!important;border-radius:var(--app-radius);background:var(--app-card-bg)!important}
.dashboard-filter-card .card-body{padding:1rem}
.dashboard-filter-card .shiny-input-container{width:100%;margin:0 0 .75rem}
.dashboard-filter-card .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg);margin-bottom:.25rem}
.dashboard-filter-card .form-control,.dashboard-filter-card .form-select,.dashboard-filter-card .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.dashboard-filter-heading{display:flex;align-items:center;gap:.5rem;font-size:.95rem;font-weight:650}
.dashboard-chart-title{display:flex;align-items:center;justify-content:space-between;gap:.75rem;flex-wrap:wrap}
.dashboard-chart-title .shiny-input-container{margin:0;min-width:230px}
.dashboard-main-card{margin-top:1rem}
.dashboard-main-chart-body{padding:.7rem 1.1rem 1.1rem}
.currency-btn-group .shiny-options-group{display:inline-flex;align-items:center;background:var(--app-muted);border:1px solid var(--app-border);border-radius:.5rem;padding:3px;gap:3px}
.currency-btn-group .radio-inline{position:relative;margin:0!important;padding:0!important;cursor:pointer}
.currency-btn-group .radio-inline input[type='radio']{position:absolute;opacity:0;width:0;height:0;pointer-events:none}
.currency-btn-group .radio-inline span{display:inline-block;padding:.32rem .85rem;font-size:.8rem;font-weight:600;color:var(--app-muted-fg);border-radius:.4rem;transition:all .15s ease-in-out;user-select:none}
.currency-btn-group .radio-inline input[type='radio']:checked+span{background:var(--app-accent);color:#fff!important;box-shadow:0 1px 3px rgba(0,0,0,.15)}
.currency-btn-group .radio-inline:hover span{color:var(--bs-body-color)}
.currency-btn-group .radio-inline input[type='radio']:checked:hover span{color:#fff!important}
.currency-btn-group .shiny-input-container{margin-bottom:0!important}
.currency-btn-group .control-label{display:none!important}
.dashboard-lower-grid{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1.3fr);gap:1rem;margin-top:1rem}
.dashboard-heading{align-items:flex-start}
.dashboard-meta{display:flex;align-items:center;gap:.5rem 1rem;justify-content:flex-end;flex-wrap:wrap;color:var(--app-muted-fg);font-size:.78rem;padding-top:.35rem}
.dashboard-meta span{display:inline-flex;align-items:center;gap:.35rem}
.metric-card{position:relative}
.metric-card::before{content:'';position:absolute;left:0;top:.75rem;bottom:.75rem;width:3px;border-radius:3px;background:var(--app-accent)}
.metric-card.metric-paid::before{background:#16a34a}.metric-card.metric-waiting::before{background:#94a3b8}.metric-card.metric-validation::before{background:#f59e0b}.metric-card.metric-rejected::before{background:#dc2626}
button.btn.metric-card{display:block;width:100%;text-align:left;padding:1rem 1.1rem;border-radius:var(--app-radius);background:var(--app-card-bg)!important;border:1px solid var(--app-border)!important;color:var(--bs-body-color)!important;font-weight:inherit;line-height:inherit;box-shadow:0 1px 2px rgba(0,0,0,.07)}
button.btn.metric-card:hover,button.btn.metric-card:focus{border-color:var(--app-accent)!important}
button.btn.metric-card.is-selected{box-shadow:inset 0 0 0 2px var(--app-accent)!important}
.metric-comparison{display:inline-flex;align-items:center;gap:.3rem;font-weight:600;font-size:.76rem;margin-top:.3rem}
/* Spending changes are neutral analytics, not a good/bad indicator. */
.metric-comparison.positive,.metric-comparison.negative{color:var(--app-muted-fg)}
.dashboard-metrics-grid{grid-template-columns:repeat(4,minmax(0,1fr))!important}
.manage-layout{max-width:1700px;margin:0 auto;padding:1.3rem clamp(1rem,2.2vw,2.5rem) 2rem}
.manage-two-col{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem;align-items:start}
.manage-defaults-row{display:grid;grid-template-columns:1fr;gap:1rem;align-items:start}
.manage-card .card-body{padding:1rem}
.defaults-card{position:sticky;top:1rem}
.defaults-card .action-bar{position:static;margin-top:.8rem}
.manage-card .shiny-input-container{width:100%;margin-bottom:.75rem}
.manage-card .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg)}
.manage-card .form-control,.manage-card .form-select,.manage-card .selectize-input{background:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem}
.manage-note{font-size:.82rem;line-height:1.55;color:var(--app-muted-fg)}
html,body{min-height:100vh}
body.bslib-page-navbar{min-height:100vh;display:flex;flex-direction:column}
body.bslib-page-navbar>.container-fluid{flex:1 1 auto;display:flex;flex-direction:column;padding-left:0!important;padding-right:0!important}
body.bslib-page-navbar .tab-content{flex:1 1 auto;display:flex;flex-direction:column}
body.bslib-page-navbar .tab-pane.active{flex:1 1 auto;display:flex;flex-direction:column}
.page-shell,.dashboard-layout,.manage-layout{flex:1 1 auto}
.app-footer{margin-top:auto;flex:0 0 auto;padding:.85rem 1.5rem;border-top:1px solid var(--app-border);background:#e2e8f0!important;color:var(--app-muted-fg)!important;font-size:.82rem;display:flex;align-items:center;justify-content:center;gap:1.5rem}
.footer-logo{height:28px;width:auto;display:block}
.footer-copy{font-weight:500;letter-spacing:.01em}
[data-bs-theme='dark'] .app-footer{border-top:1px solid var(--app-border)!important;background:#070c16!important;color:var(--app-muted-fg)!important}
@media(max-width:1150px){.metrics-grid{grid-template-columns:repeat(3,minmax(0,1fr))}.dashboard-metrics-grid{grid-template-columns:repeat(2,minmax(0,1fr))!important}.dashboard-lower-grid{grid-template-columns:1fr}}
@media(max-width:992px){.dashboard-layout{grid-template-columns:1fr}.dashboard-sidebar{position:static;max-height:none;order:-1}}
@media(max-width:920px){.requests-layout{grid-template-columns:1fr}.requests-sidebar{position:static;max-height:none;grid-column:auto;grid-row:auto;order:-1}.requests-main{grid-column:auto;grid-row:auto}.manage-two-col{grid-template-columns:1fr}}
@media(max-width:720px){.dashboard-breakdown-title{align-items:flex-start}}
@media(max-width:600px){.requests-page,.dashboard-layout{padding:.75rem}.dashboard-shell,.manage-layout{padding:.8rem .75rem 1.2rem}.metrics-grid{grid-template-columns:1fr}.dashboard-metrics-grid{grid-template-columns:1fr!important}.dashboard-chart-title .shiny-input-container{min-width:100%}}
.navbar .global-currency-wrap select.form-select{appearance:none!important;-webkit-appearance:none!important;background-color:transparent!important;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 16 16'%3E%3Cpath fill='none' stroke='%23ffffff' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.8' d='M3.5 6.5 8 11l4.5-4.5'/%3E%3C/svg%3E\")!important;background-repeat:no-repeat!important;background-position:right .6rem center!important;background-size:12px 12px!important;color:#fff!important;border:1px solid rgba(255,255,255,.32)!important;border-radius:.55rem!important;box-shadow:none!important}
.navbar .global-currency-wrap select.form-select option{color:#111!important;background:#fff!important}
.request-table-card table.dataTable{width:100%!important;table-layout:fixed!important}
.request-table-card table.dataTable thead th{font-size:.68rem;font-weight:650;letter-spacing:.01em;text-transform:uppercase;color:var(--app-muted-fg)!important;padding:.55rem .3rem!important}
.request-table-card table.dataTable th,.request-table-card table.dataTable td{white-space:nowrap!important;vertical-align:middle;overflow:hidden!important;text-overflow:ellipsis}
.request-table-card table.dataTable tbody td{padding:.85rem .4rem!important}
.request-table-card table.dataTable tbody td:last-child{overflow:visible!important;text-align:right}
.request-table-card table.dataTable tbody tr{cursor:pointer}
.request-table-card table.dataTable th:nth-child(1),.request-table-card table.dataTable td:nth-child(1){width:14%}
.request-table-card table.dataTable th:nth-child(2),.request-table-card table.dataTable td:nth-child(2){width:16%}
.request-table-card table.dataTable th:nth-child(3),.request-table-card table.dataTable td:nth-child(3){width:15%}
.request-table-card table.dataTable th:nth-child(4),.request-table-card table.dataTable td:nth-child(4){width:12%}
.request-table-card table.dataTable th:nth-child(5),.request-table-card table.dataTable td:nth-child(5){width:12%}
.request-table-card table.dataTable th:nth-child(6),.request-table-card table.dataTable td:nth-child(6){width:17%}
.request-table-card table.dataTable th:nth-child(7),.request-table-card table.dataTable td:nth-child(7){width:14%}
.navbar .global-currency-wrap .selectize-control{width:100%;margin:0}
.navbar .global-currency-wrap .selectize-input{background:transparent!important;background-color:transparent!important;color:#fff!important;border:1px solid rgba(255,255,255,.32)!important;border-radius:.55rem!important;box-shadow:none!important;min-height:2.3rem;height:2.3rem;padding:.25rem 1.6rem .25rem .7rem!important;display:flex!important;align-items:center}
.navbar .global-currency-wrap .selectize-input .item,.navbar .global-currency-wrap .selectize-input input{color:#fff!important}
.navbar .global-currency-wrap .selectize-input::after{border-color:#fff transparent transparent transparent!important}
.navbar .global-currency-wrap .selectize-input.dropdown-active::after{border-color:transparent transparent #fff transparent!important}
.navbar .global-currency-wrap .selectize-dropdown{background:#0c2340!important;color:#fff!important;border:1px solid rgba(255,255,255,.32)!important}
.navbar .global-currency-wrap .selectize-dropdown .option{background:transparent!important;color:#fff!important}
.navbar .global-currency-wrap .selectize-dropdown .active{background:rgba(255,255,255,.14)!important;color:#fff!important}
.navbar bslib-input-dark-mode{--text-1:#fff!important;--text-2:#fff!important;--text-1-light:#fff!important;--text-2-light:#fff!important;--text-1-dark:#fff!important;--text-2-dark:#fff!important;color:#fff!important;display:inline-flex;align-items:center;justify-content:center;width:2.3rem;height:2.3rem;border:1px solid rgba(255,255,255,.32);border-radius:.55rem;background:transparent}
.request-table-card .card-body{display:flex!important;flex-direction:column!important}
.request-table-card .html-widget-output,.request-table-card .datatables{flex:1 1 auto!important;display:flex!important;flex-direction:column!important;min-height:0}
.request-table-card .dataTables_wrapper{flex:1 1 auto!important;min-height:100%}
.card .dt-foot{margin-top:auto}
body:not(.modal-open) .modal-backdrop{display:none!important;pointer-events:none!important}
.insights-kpi{background:var(--app-card-bg);border:1px solid var(--app-border);border-left:3.5px solid var(--app-accent);border-radius:var(--app-radius);padding:1rem 1.15rem 1.05rem}
.insights-kpi-paid{border-left-color:#10b981}
.insights-kpi-open{border-left-color:#f59e0b}
.insights-kpi-days{border-left-color:#38bdf8}
.insights-kpi-label{color:var(--app-muted-fg);font-size:.78rem;font-weight:650}
.insights-kpi-value{font-size:clamp(1.2rem,1.5vw,1.65rem);line-height:1.15;font-weight:700;letter-spacing:-.03em;margin-top:.35rem;white-space:nowrap}
.insights-kpi-sub{display:flex;align-items:center;gap:.35rem;color:var(--app-muted-fg);font-size:.8rem;margin-top:.45rem}
.insights-delta-up{color:#10b981;font-weight:700}
.insights-delta-down{color:#ef4444;font-weight:700}
.insights-delta-flat{color:var(--app-muted-fg);font-weight:650}
.insights-filter-badge{display:inline-flex;align-items:center;justify-content:center;min-width:1.15rem;height:1.15rem;margin-left:.4rem;padding:0 .32rem;border-radius:999px;background:var(--app-accent);color:#fff;font-size:.68rem;font-weight:700}
.insights-chart-head{display:flex;align-items:center;justify-content:space-between;gap:.75rem;flex-wrap:wrap;width:100%}
.insights-chart-head .shiny-input-container{margin:0}
.insights-page .dashboard-chart-title .shiny-input-container,.insights-chart-head .shiny-input-container{min-width:0}
.insights-view-label{color:var(--app-muted-fg);font-size:.84rem;font-weight:500;white-space:nowrap}
.insights-breakdown-controls{display:flex;align-items:center;justify-content:flex-end;gap:.5rem;flex-wrap:nowrap;min-width:0;max-width:100%}
.insights-status-label{white-space:nowrap;flex:0 1 auto;min-width:0;overflow:hidden;text-overflow:ellipsis}
.insights-breakdown-controls .shiny-input-container{width:11.5rem!important;min-width:11.5rem;max-width:100%;flex:0 0 auto;margin:0}
#breakdown_status + .selectize-control{width:11.5rem!important;min-width:11.5rem}
#breakdown_status + .selectize-control .selectize-input,#breakdown_status + .selectize-control .selectize-input .item{white-space:nowrap}
.insights-seg .shiny-options-group{display:inline-flex;align-items:center;background:var(--app-muted);border:1px solid var(--app-border);border-radius:.45rem;padding:2px;gap:2px}
.insights-seg .shiny-input-container{margin:0}
.insights-seg .radio,.insights-seg .radio-inline,.insights-seg .form-check{position:relative;margin:0!important;padding:0!important;min-height:0}
.insights-seg input[type=radio]{position:absolute;opacity:0;width:0;height:0;pointer-events:none}
.insights-seg label{margin:0;cursor:pointer}
.insights-seg label span,.insights-seg .form-check-label{display:inline-block;padding:.3rem .75rem;font-size:.78rem;font-weight:600;color:var(--app-muted-fg);border-radius:.35rem;transition:all .15s ease}
.insights-seg input[type=radio]:checked+span,.insights-seg input[type=radio]:checked+.form-check-label{background:var(--app-accent);color:#ffffff;font-weight:650}
[data-bs-theme='dark'] .insights-seg .shiny-options-group{background:#0f172a;border-color:#1e293b}
[data-bs-theme='dark'] .insights-seg label span,[data-bs-theme='dark'] .insights-seg .form-check-label{color:#94a3b8}
[data-bs-theme='dark'] .insights-seg input[type=radio]:checked+span,[data-bs-theme='dark'] .insights-seg input[type=radio]:checked+.form-check-label{background:#0ea5e9;color:#ffffff}
.insights-plot-title{font-weight:650;margin:0;display:inline-flex;align-items:center;gap:.35rem}
.info-tip{position:relative;display:inline-flex;align-items:center;justify-content:center;width:1.15rem;height:1.15rem;margin:0;padding:0;border:0;background:transparent;color:#64748b;font-size:.82rem;line-height:1;font-weight:normal;cursor:pointer;vertical-align:middle;flex:0 0 auto}
.info-tip:hover,.info-tip:focus,.info-tip:focus-visible{color:var(--bs-body-color)}
.info-tip-portal{position:fixed;z-index:4000;display:none;width:max-content;max-width:320px;box-sizing:border-box;padding:.45rem .55rem;border-radius:.35rem;background:#0f172a;color:#fff;font-size:.72rem;line-height:1.4;font-weight:550;text-align:left;white-space:normal;overflow-wrap:anywhere;box-shadow:0 8px 20px rgba(0,0,0,.18);pointer-events:none}
.info-tip-portal.is-open{display:block}
[data-bs-theme='dark'] .info-tip-portal{background:#1e293b;color:#f8fafc;border:1px solid #475569}
.insights-plot-head,.insights-line-head{display:flex;align-items:baseline;justify-content:space-between;gap:.75rem;margin:0 0 .45rem}
.insights-line-summary{color:var(--app-muted-fg);font-size:.78rem;font-weight:650;white-space:nowrap}
.insights-lines{display:flex;flex-direction:column;gap:.25rem}
.insights-line{display:grid;grid-template-columns:minmax(6.5rem,10.5rem) minmax(0,1fr) auto;gap:.7rem;align-items:center;width:100%;margin:0;padding:.35rem 0;border:0;background:transparent;color:inherit;text-align:left;font:inherit}
.insights-line.is-button{cursor:pointer}
.insights-line.is-button:hover .insights-line-name{color:var(--app-accent)}
.insights-line-name{font-size:.84rem;font-weight:650;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
.insights-line-track{display:block;height:.55rem;background:var(--app-muted);border-radius:999px;overflow:hidden}
[data-bs-theme='dark'] .insights-line-track{background:#1e293b}
.insights-line-fill{display:block;height:100%;border-radius:999px;background:var(--status-paid)}
.insights-line-fill.is-paid{background:var(--status-paid)}
.insights-line-fill.is-pending{background:var(--status-pending)}
.insights-line-fill.is-review{background:var(--status-review)}
.insights-line-fill.is-cancelled{background:var(--status-rejected)}
.insights-line-meta{font-size:.78rem;font-weight:650;color:var(--app-muted-fg);white-space:nowrap;display:inline-flex;align-items:center}
.insights-line-chevron{color:#64748b;font-size:1rem;margin-left:.4rem;font-weight:600;line-height:1}
#insights_footer{margin-top:auto;flex:0 0 auto;width:100%}
.insights-footer{margin-top:auto;padding-top:.65rem;border-top:1px solid var(--app-border);color:var(--app-muted-fg);font-size:.78rem;display:flex;align-items:center;gap:.45rem;width:100%}
.insights-empty{color:var(--app-muted-fg);text-align:center;padding:1.1rem .5rem}
.insights-chart-stage{flex:1 1 auto;display:flex;flex-direction:column;min-height:340px}
.insights-chart-stage>.shiny-panel-conditional:not([style*='display: none']){flex:1 1 auto;display:flex!important;flex-direction:column;min-height:340px}
.insights-chart-panel{display:flex;flex-direction:column;flex:1 1 auto;min-height:320px}
.insights-empty-fill{flex:1 1 auto;display:flex;align-items:center;justify-content:center;min-height:240px;margin:0}
.insights-page{max-width:1900px;width:100%;margin:0 auto;padding:1.2rem clamp(.8rem,1.6vw,1.8rem) 1.25rem;display:flex;flex-direction:column}
.insights-page .page-heading{margin-bottom:.75rem}
.insights-page .kpi-grid{margin-bottom:.85rem}
.insights-page .dashboard-layout{max-width:none;margin:0;padding:0;align-items:stretch;flex:1 1 auto;grid-template-columns:minmax(0,1fr) minmax(248px,268px);gap:1rem}
.insights-page .insights-main{min-width:0;display:flex}
.insights-page .insights-chart-card{flex:1 1 auto;width:100%;margin:0;display:flex;flex-direction:column;min-height:510px}
.insights-page .insights-chart-card>.card-header{flex:0 0 auto}
.insights-page .insights-chart-card>.card-body{flex:1 1 auto;padding:.75rem .9rem .8rem;display:flex;flex-direction:column}
.insights-page .shiny-plot-output{margin:0}
.insights-page .dashboard-sidebar{position:sticky;top:1rem;height:auto;max-height:calc(100vh - 6rem);overflow-y:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg);align-self:start}
.filter-panel{--filter-chevron:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'%3E%3Cpath fill='none' stroke='%2364748b' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3E%3C/svg%3E\")}
[data-bs-theme='dark'] .filter-panel{--filter-chevron:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'%3E%3Cpath fill='none' stroke='%23cbd5e1' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3E%3C/svg%3E\")}
.filter-panel-head{display:flex;align-items:center;justify-content:space-between;gap:.55rem;padding:1rem 1rem .85rem;border-bottom:1px solid var(--app-border);font-size:.96rem;font-weight:650}
.filter-panel-body{padding:1rem}
.filter-panel-body>:last-child{margin-bottom:0}
.filter-panel .filter-field{margin:0 0 1rem}
.filter-panel .filter-panel-body>.filter-field:last-child,.filter-panel .advanced-filters .filter-field:last-child{margin-bottom:0}
.filter-panel .shiny-input-container{width:100%;margin:0}
.filter-panel .control-label{font-size:.78rem;font-weight:600;color:var(--app-muted-fg);margin-bottom:.25rem}
.filter-panel .form-control,.filter-panel .form-select,.filter-panel .selectize-input{background-color:var(--app-input-bg)!important;color:var(--bs-body-color)!important;border:1px solid var(--app-border)!important;border-radius:.5rem!important;box-shadow:none!important;min-height:2.45rem}
.filter-panel .form-select,.filter-panel .selectize-control.single .selectize-input,.filter-panel .selectize-control.multi .selectize-input{background-image:var(--filter-chevron)!important;background-repeat:no-repeat!important;background-position:right .75rem center!important;background-size:16px 12px!important;padding:.375rem 2.1rem .375rem .75rem!important;display:flex!important;align-items:center!important}
.filter-panel .form-select{appearance:none!important;-webkit-appearance:none!important;width:100%}
.filter-panel .selectize-control{border:0!important;box-shadow:none!important;background:transparent!important}
.filter-panel .selectize-input::after{display:none!important}
.filter-panel .selectize-input input{padding:0!important}
.filter-panel .selectize-input.focus,.filter-panel .form-control:focus,.filter-panel .form-select:focus{border-color:var(--app-accent)!important;box-shadow:none!important}
.filter-panel .selectize-input.disabled,.filter-panel .selectize-control.disabled .selectize-input{opacity:1!important;background-color:var(--app-input-bg)!important;cursor:not-allowed}
.filter-panel .selectize-input.disabled input{cursor:not-allowed!important}
.filter-panel .advanced-filters{margin:0;border-top:1px solid var(--app-border);padding:.7rem 0 0}
.filter-panel .advanced-filters summary{margin-bottom:0}
.filter-panel .advanced-filters[open] summary{margin-bottom:.7rem}
.filter-panel .status-chips{margin:0}
.insights-sidebar .sidebar-close-btn{display:none}
@media(max-width:860px){
  .insights-sidebar .sidebar-close-btn{display:inline-flex}
  .filter-panel-head{display:flex!important}
}
.insights-compare{display:flex;flex-direction:column;flex:1 1 auto;min-height:300px}
.insights-compare-plot{display:grid;grid-template-columns:2.8rem minmax(0,1fr);column-gap:.45rem;flex:1 1 auto;min-height:250px}
.insights-compare-y{display:flex;flex-direction:column;justify-content:space-between;color:var(--app-muted-fg);font-size:.68rem;font-weight:650;text-align:right;padding-bottom:1.15rem}
.insights-compare-main{display:flex;flex-direction:column;min-width:0;min-height:230px}
.insights-compare-area{position:relative;flex:1 1 auto;min-height:200px}
.insights-compare-grid{position:absolute;inset:0;display:flex;flex-direction:column;justify-content:space-between;pointer-events:none}
.insights-compare-grid span{display:block;border-top:1px solid rgba(100,116,139,.28);height:0}
.insights-compare-cols{position:absolute;inset:0;display:flex;align-items:stretch;gap:.2rem}
.insights-compare-col{flex:1 1 0;min-width:0;height:100%}
.insights-compare-group{height:100%;display:flex;align-items:flex-end;justify-content:center;gap:2px}
.insights-bar{flex:1 1 0;max-width:16px;min-width:3px;min-height:2px;border:0;padding:0;border-radius:2px 2px 0 0;cursor:pointer;position:relative;background:#0284C7}
.insights-bar:hover,.insights-bar:focus,.insights-bar.is-tip{filter:brightness(1.18);outline:1px solid rgba(15,23,42,.28);z-index:4}
.insights-bar::after{content:attr(data-tip);position:absolute;left:50%;bottom:calc(100% + 6px);transform:translateX(-50%);background:#0f172a;color:#fff;font-size:.72rem;line-height:1.3;font-weight:650;padding:.3rem .45rem;border-radius:.35rem;white-space:nowrap;opacity:0;pointer-events:none;z-index:5}
.insights-bar:hover::after,.insights-bar:focus::after,.insights-bar:focus-visible::after,.insights-bar.is-tip::after{opacity:1}
.insights-compare-x{display:flex;gap:.2rem;height:1.15rem}
.insights-compare-xlabel{flex:1 1 0;min-width:0;font-size:.68rem;font-weight:650;color:var(--app-muted-fg);text-align:center;overflow:hidden;text-overflow:ellipsis}
.insights-compare-legend{display:flex;flex-wrap:wrap;gap:.4rem .9rem;margin-top:.45rem}
.insights-compare-key{display:inline-flex;align-items:center;gap:.35rem;font-size:.75rem;font-weight:650;color:var(--bs-body-color)}
.insights-compare-swatch{width:.7rem;height:.7rem;border-radius:.15rem;flex:0 0 auto}
.insights-spend-line{height:300px;min-height:300px;display:flex;flex-direction:column}
.insights-spend-line .insights-compare-plot{height:100%;min-height:0}
.insights-spend-line .insights-compare-main{min-height:0;height:100%}
.insights-spend-line .insights-compare-area{min-height:0;flex:1 1 auto;height:auto}
.insights-spend-empty{min-height:300px}
.insights-spend-svg{position:absolute;inset:0;width:100%;height:100%;overflow:visible;pointer-events:none}
.insights-spend-points{position:absolute;inset:0}
.insights-spend-guide{position:absolute;top:0;bottom:0;width:1px;background:rgba(100,116,139,.7);opacity:0;pointer-events:none;z-index:2}
.insights-spend-guide.is-on{opacity:1}
.insights-spend-x{position:relative;display:block;height:1.15rem}
.insights-spend-x .insights-compare-xlabel{position:absolute;top:0;transform:translateX(-50%);flex:none;width:auto}
.insights-point{position:absolute;width:16px;height:16px;margin:0 0 -8px -8px;padding:0;border:0;border-radius:50%;background:transparent;transform:none;cursor:pointer;z-index:3}
.insights-point::before{content:'';position:absolute;left:50%;top:50%;width:8px;height:8px;border-radius:50%;background:var(--dot,#0284C7);transform:translate(-50%,-50%);transition:width .12s ease,height .12s ease,box-shadow .12s ease}
.insights-point:hover::before,.insights-point:focus::before,.insights-point.is-tip::before,.insights-point.is-hover::before{width:11px;height:11px;box-shadow:0 0 0 3px rgba(148,163,184,.45)}
.insights-point-tip{position:absolute;left:50%;bottom:calc(100% + 10px);transform:translateX(-50%);display:flex;flex-direction:column;align-items:flex-start;gap:.08rem;background:#0f172a;color:#fff;font-size:.72rem;line-height:1.35;font-weight:650;padding:.4rem .55rem;border-radius:.35rem;white-space:nowrap;opacity:0;pointer-events:none;z-index:6;box-shadow:0 8px 20px rgba(0,0,0,.18)}
.insights-point:hover .insights-point-tip,.insights-point:focus .insights-point-tip,.insights-point.is-tip .insights-point-tip,.insights-point.is-hover .insights-point-tip{opacity:1}
.insights-point-tip.is-left{left:auto;right:0;transform:none}
.insights-point-tip.is-below{bottom:auto;top:calc(100% + 10px)}
.insights-point-cum{font-weight:750}
.insights-point-day,.insights-point-n{color:#e2e8f0;font-weight:600}
.insights-advanced{margin:.15rem 0 .85rem}
.insights-advanced summary{cursor:pointer;font-weight:650;font-size:.86rem;margin-bottom:.55rem}
.insights-compare-delta{font-size:.82rem;font-weight:700;white-space:nowrap;color:#10b981}
.insights-compare-delta.is-down{color:#ef4444}
.insights-day-stats{display:flex;width:fit-content;max-width:100%;margin:0 0 .75rem auto;align-items:stretch;background:var(--app-muted);border:1px solid var(--app-border);border-radius:.5rem;overflow-x:auto}
.insights-day-stat{display:inline-flex;align-items:baseline;flex:0 0 auto;gap:.5rem;width:auto;padding:.5rem 1rem;white-space:nowrap;background:transparent;border:0;border-radius:0;color:var(--app-muted-fg);font-size:.78rem}
.insights-day-stat + .insights-day-stat{border-left:1px solid var(--app-border)}
[data-bs-theme='dark'] .insights-day-stats{background:#1e293b;border-color:#334155}
[data-bs-theme='dark'] .insights-day-stat + .insights-day-stat{border-left-color:#334155}
.insights-day-stat b{display:inline;margin:0;color:var(--bs-body-color);font-size:1.05rem;font-weight:700;white-space:nowrap}
.insights-day-key{display:flex;gap:1rem;margin:.15rem 0 .35rem;color:var(--app-muted-fg);font-size:.75rem;font-weight:650}
.insights-key{display:inline-flex;align-items:center;gap:.35rem}
.insights-key::before{content:'';width:.7rem;height:.7rem;border-radius:.15rem;background:currentColor}
.insights-key.is-ok{color:#10b981}
.insights-key.is-late{color:#ef4444}
.insights-day-center-label{display:grid;grid-template-columns:7rem minmax(0,1fr) 4.5rem;gap:.65rem;margin:0 0 .2rem}
.insights-day-center-label span{grid-column:2;justify-self:center;font-size:.75rem;font-weight:600;color:var(--app-muted-fg)}
.insights-day-rows{position:relative}
.insights-day-row{display:grid;grid-template-columns:7rem minmax(0,1fr) 4.5rem;gap:.65rem;align-items:center;margin:.65rem 0}
.insights-day-ref{font-size:.82rem;font-weight:600;color:var(--bs-body-color);display:flex;align-items:center;justify-content:space-between}
[data-bs-theme='dark'] .insights-day-ref{color:#cbd5e1}
.insights-day-ref-sep{color:var(--app-border);margin-left:auto;padding-right:.3rem}
.insights-day-track{position:relative;height:.65rem;background:var(--app-muted);border-radius:4px}
[data-bs-theme='dark'] .insights-day-track{background:#1e293b}
.insights-day-track::before{content:'';position:absolute;left:50%;top:-3px;bottom:-3px;width:1px;background:#475569;z-index:2}
.insights-day-fill{position:absolute;top:0;bottom:0;border-radius:3px;z-index:1}
.insights-day-fill.is-ok{right:50%;left:auto;background:#10b981}
.insights-day-fill.is-late{left:50%;right:auto;background:#ef4444}
.insights-day-delta{font-size:.78rem;font-weight:600;text-align:right;white-space:nowrap}
.insights-day-delta.is-ok{color:#10b981}
.insights-day-delta.is-late{color:#ef4444}
.insights-show-all{color:#38bdf8;font-size:.82rem;font-weight:600;text-decoration:none;display:inline-flex;align-items:center;gap:.3rem;margin-top:.6rem;cursor:pointer;border:0;background:transparent;padding:0}
.insights-show-all:hover{text-decoration:underline;color:#7dd3fc}
.insights-back{margin-bottom:.55rem}
@media(max-width:992px){
  .insights-page .dashboard-layout{grid-template-columns:minmax(0,1fr) minmax(248px,268px)}
  .insights-page .dashboard-sidebar{position:sticky;order:0;max-height:calc(100vh - 6rem)}
}
@media(max-width:860px){
  .insights-page{position:relative}
  .insights-page .dashboard-layout{display:block}
  .insights-page .dashboard-sidebar{display:none;position:fixed;left:0;right:0;bottom:0;top:auto;width:100%;max-height:78vh;z-index:1080;border-radius:1rem 1rem 0 0;background:var(--app-card-bg);box-shadow:0 -12px 36px rgba(0,0,0,.28)}
  .insights-page .dashboard-sidebar.is-open{display:block!important}
  .insights-page .dashboard-sidebar .requests-sidebar-body{display:block}
}
@media(max-width:600px){
  .insights-day-row,.insights-day-center-label{grid-template-columns:5.5rem minmax(0,1fr) 3.6rem}
  .insights-day-rows::before{left:calc(5.5rem + .65rem + (100% - 5.5rem - 3.6rem - 1.3rem) / 2)}
  .insights-chart-head-right{width:100%}
  .insights-chart-head .shiny-input-container{width:100%}
  .insights-breakdown-controls{width:auto;max-width:100%;margin-left:auto}
  .insights-breakdown-controls .shiny-input-container,#breakdown_status + .selectize-control{width:11.5rem!important;max-width:100%}
  .insights-status-label{max-width:4.5rem}
}
@media(max-width:480px){
  .insights-status-label{display:none}
}

"

js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('setTableStatus', function(m) {
    var status = (m && m.status) ? m.status : '';
    $('.status-chip').removeClass('is-on');
    $('.status-chip[data-status=\"' + status + '\"]').addClass('is-on');
    if (window.syncFilterPanels) syncFilterPanels();
  });
  Shiny.addCustomMessageHandler('filterLock', function(m) {
    var apply = function(tries) {
      var el = document.getElementById(m.id);
      if (!el || !el.selectize) {
        if (tries > 0) setTimeout(function() { apply(tries - 1); }, 50);
        return;
      }
      var inst = el.selectize;
      inst.settings.placeholder = m.placeholder || '';
      if (inst.updatePlaceholder) inst.updatePlaceholder();
      if (m.locked) {
        inst.clear(true);
        inst.disable();
        inst.$control.addClass('is-locked');
      } else {
        inst.enable();
        inst.$control.removeClass('is-locked');
      }
    };
    apply(20);
  });
  Shiny.addCustomMessageHandler('cleanupModal', function(m) {
    $('.modal-backdrop').remove();
    $('body').removeClass('modal-open').css({'overflow': '', 'padding-right': ''});
    $('.modal').modal('hide').removeClass('show');
    $('.modal:not(.show)').remove();
    setTimeout(function() {
      $('.modal-backdrop').remove();
      $('body').removeClass('modal-open').css({'overflow': '', 'padding-right': ''});
    }, 200);
  });
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
$(document).on('hidden.bs.modal hide.bs.modal', function() {
  setTimeout(function() {
    if (!$('.modal.show').length) {
      $('.modal-backdrop').remove();
      $('body').removeClass('modal-open').css({'overflow': '', 'padding-right': ''});
    }
  }, 100);
});
setInterval(function() {
  if (!$('.modal.show').length && ($('.modal-backdrop').length || $('body').hasClass('modal-open'))) {
    $('.modal-backdrop').remove();
    $('body').removeClass('modal-open').css({'overflow': '', 'padding-right': ''});
  }
}, 250);
$(document).on('shown.bs.modal', function() {
  if ($('#pay_confirm').length) {
    $('#btn_pay_confirm').prop('disabled', !$('#pay_confirm').is(':checked'));
  }
});
$(document).on('change', '#pay_confirm', function() {
  $('#btn_pay_confirm').prop('disabled', !this.checked);
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
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('badge', function(m) {
    $('#fcount').text(m.n).css('display', m.n > 0 ? 'inline-block' : 'none');
  });
});
// Toggle and close collapsible/popover filter for Requests
$(document).on('click', '#toggle_requests_filter', function(e) {
  e.preventDefault();
  e.stopPropagation();
  if (window.innerWidth <= 860) {
    $('.requests-sidebar').toggleClass('is-open');
    var isOpen = $('.requests-sidebar').hasClass('is-open');
    $('#toggle_requests_filter').toggleClass('is-active', isOpen);
  } else {
    var layout = $('.requests-layout');
    layout.toggleClass('sidebar-collapsed');
    var isCollapsed = layout.hasClass('sidebar-collapsed');
    $('#toggle_requests_filter').toggleClass('is-active', !isCollapsed);
    if (window.DataTable) {
      setTimeout(function() {
        $($.fn.dataTable.tables(true)).DataTable().columns.adjust();
      }, 220);
    }
  }
});
$(document).on('click', '#close_requests_filter', function(e) {
  e.preventDefault();
  if (window.innerWidth <= 860) {
    $('.requests-sidebar').removeClass('is-open');
    $('#toggle_requests_filter').removeClass('is-active');
  } else {
    $('.requests-layout').addClass('sidebar-collapsed');
    $('#toggle_requests_filter').removeClass('is-active');
  }
});
// Toggle and close collapsible/popover filter for Dashboards
$(document).on('click', '#toggle_dash_filter', function(e) {
  e.preventDefault();
  e.stopPropagation();
  if (window.innerWidth <= 860) {
    $('.dashboard-sidebar').toggleClass('is-open');
    var isOpen = $('.dashboard-sidebar').hasClass('is-open');
    $('#toggle_dash_filter').toggleClass('is-active', isOpen);
  } else {
    var layout = $('.dashboard-layout');
    layout.toggleClass('sidebar-collapsed');
    var isCollapsed = layout.hasClass('sidebar-collapsed');
    $('#toggle_dash_filter').toggleClass('is-active', !isCollapsed);
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
    }, 220);
  }
});
function placeSpendTip(point) {
  var tip = point.querySelector('.insights-point-tip');
  var line = point.closest('.insights-spend-line');
  var card = point.closest('.insights-chart-card') || line;
  if (!tip || !card) return;
  var cr = card.getBoundingClientRect();
  var pr = point.getBoundingClientRect();
  var tipW = tip.offsetWidth || 160;
  var tipH = tip.offsetHeight || 72;
  var left = pr.left + pr.width / 2 - tipW / 2;
  var top = pr.top - tipH - 10;
  if (left + tipW > cr.right - 8) left = cr.right - 8 - tipW;
  if (left < cr.left + 8) left = cr.left + 8;
  if (top < cr.top + 4) top = pr.bottom + 10;
  if (top + tipH > cr.bottom - 4) top = Math.max(cr.top + 4, cr.bottom - 4 - tipH);
  tip.style.position = 'fixed';
  tip.style.left = Math.round(left) + 'px';
  tip.style.top = Math.round(top) + 'px';
  tip.style.bottom = 'auto';
  tip.style.right = 'auto';
  tip.style.transform = 'none';
  var guide = line ? line.querySelector('.insights-spend-guide') : null;
  if (guide) {
    guide.style.left = point.style.left;
    guide.classList.add('is-on');
  }
}
function spendSegDist(px, py, ax, ay, bx, by) {
  var dx = bx - ax, dy = by - ay, len = dx * dx + dy * dy;
  var t = len ? Math.max(0, Math.min(1, ((px - ax) * dx + (py - ay) * dy) / len)) : 0;
  return Math.hypot(px - (ax + t * dx), py - (ay + t * dy));
}
function nearestPaidPoint(area, clientX, clientY) {
  var svg = area.querySelector('.insights-spend-svg');
  var poly = svg && svg.querySelector('polyline');
  var rect = svg ? svg.getBoundingClientRect() : null;
  var lineD = Infinity;
  if (poly && rect && rect.width && rect.height) {
    var pts = poly.getAttribute('points').trim().split(/\\s+/).map(function(p) {
      var xy = p.split(',');
      return {
        x: rect.left + (parseFloat(xy[0]) / 100) * rect.width,
        y: rect.top + (parseFloat(xy[1]) / 100) * rect.height
      };
    });
    for (var i = 1; i < pts.length; i++) {
      lineD = Math.min(lineD, spendSegDist(clientX, clientY, pts[i - 1].x, pts[i - 1].y, pts[i].x, pts[i].y));
    }
  }
  var markers = area.querySelectorAll('.insights-point');
  var best = null, bestD = Infinity;
  markers.forEach(function(p) {
    var r = p.getBoundingClientRect();
    var d = Math.hypot(clientX - (r.left + r.width / 2), clientY - (r.top + r.height / 2));
    if (d < bestD) { bestD = d; best = p; }
  });
  if ((best && bestD <= 16) || lineD <= 16) return best;
  return null;
}
$(document).on('mousemove', '.insights-spend-line .insights-compare-area', function(e) {
  var chosen = nearestPaidPoint(this, e.clientX, e.clientY);
  $(this).find('.insights-point').removeClass('is-hover');
  if (chosen) {
    chosen.classList.add('is-hover');
    placeSpendTip(chosen);
  } else if (!$(this).find('.insights-point:focus, .insights-point.is-tip').length) {
    $(this).find('.insights-spend-guide').removeClass('is-on');
  }
});
$(document).on('mouseleave', '.insights-spend-line', function() {
  $(this).find('.insights-point').removeClass('is-hover');
  if (!$(this).find('.insights-point:focus, .insights-point.is-tip').length) {
    $(this).find('.insights-spend-guide').removeClass('is-on');
  }
});
$(document).on('focusin', '.insights-point', function() {
  placeSpendTip(this);
});
$(document).on('click', '.insights-bar, .insights-point', function(e) {
  e.preventDefault();
  e.stopPropagation();
  var on = !$(this).hasClass('is-tip');
  $('.insights-bar, .insights-point').removeClass('is-tip');
  if (on) {
    $(this).addClass('is-tip');
    if (this.classList.contains('insights-point')) placeSpendTip(this);
  }
});
var infoTipBtn = null;
function infoTipNode() {
  var tip = document.getElementById('info-tip-portal');
  if (!tip) {
    tip = document.createElement('div');
    tip.id = 'info-tip-portal';
    tip.className = 'info-tip-portal';
    tip.setAttribute('role', 'tooltip');
    document.body.appendChild(tip);
  }
  return tip;
}
function hideInfoTip() {
  var tip = document.getElementById('info-tip-portal');
  if (tip) tip.classList.remove('is-open');
  document.querySelectorAll('.info-tip[aria-describedby]').forEach(function(btn) {
    btn.removeAttribute('aria-describedby');
  });
  if (infoTipBtn) infoTipBtn.classList.remove('is-tip');
  infoTipBtn = null;
}
function placeInfoTip(btn) {
  if (!btn) return;
  var tip = infoTipNode();
  tip.textContent = btn.getAttribute('data-tip') || '';
  tip.style.width = 'max-content';
  tip.style.maxWidth = '320px';
  tip.classList.add('is-open');
  btn.setAttribute('aria-describedby', 'info-tip-portal');
  var gap = 8, margin = 8;
  var br = btn.getBoundingClientRect();
  var tw = tip.offsetWidth, th = tip.offsetHeight;
  var vw = window.innerWidth, vh = window.innerHeight;
  var left = br.left;
  var top = br.bottom + gap;
  if (top + th > vh - margin && br.top - gap - th >= margin) top = br.top - gap - th;
  if (top < margin) top = margin;
  if (top + th > vh - margin) top = Math.max(margin, vh - margin - th);
  if (left + tw > vw - margin) left = vw - margin - tw;
  if (left < margin) left = margin;
  var covers = !(left + tw < br.left || left > br.right || top + th < br.top || top > br.bottom);
  if (covers) {
    if (br.right + gap + tw <= vw - margin) left = br.right + gap;
    else if (br.left - gap - tw >= margin) left = br.left - gap - tw;
  }
  tip.style.left = Math.round(left) + 'px';
  tip.style.top = Math.round(top) + 'px';
  infoTipBtn = btn;
}
function placeFilterMenu(inst) {
  var dd = inst.$dropdown && inst.$dropdown[0];
  var control = inst.$control && inst.$control[0];
  if (!dd || !control) return;
  var rect = control.getBoundingClientRect();
  var gap = 4;
  var below = window.innerHeight - rect.bottom - gap;
  var above = rect.top - gap;
  var up = below < 180 && above > below;
  var maxH = Math.min(280, Math.max(80, (up ? above : below) - 8));
  dd.style.position = 'fixed';
  dd.style.zIndex = '4000';
  dd.style.left = Math.max(8, rect.left) + 'px';
  dd.style.width = rect.width + 'px';
  dd.style.maxHeight = maxH + 'px';
  dd.style.overflowY = 'auto';
  var content = dd.querySelector('.selectize-dropdown-content');
  if (content) content.style.maxHeight = 'none';
  if (up) {
    var h = Math.min(dd.scrollHeight || maxH, maxH);
    dd.style.top = Math.max(8, rect.top - gap - h) + 'px';
  } else {
    dd.style.top = (rect.bottom + gap) + 'px';
  }
}
function filterFieldValue(field) {
  var type = field.getAttribute('data-filter-type');
  if (type === 'chips') {
    var on = field.querySelector('.status-chip.is-on');
    return on ? (on.getAttribute('data-status') || '') : '';
  }
  if (type === 'search') {
    var input = field.querySelector('input[type=\"text\"], input:not([type])');
    return input ? input.value.trim() : '';
  }
  var sel = field.querySelector('select');
  if (!sel) return '';
  var value = sel.selectize ? sel.selectize.getValue() : (sel.value || '');
  if (Array.isArray(value)) value = value.filter(function(v) { return v && v !== '__none__'; }).join('|');
  if (value === '__none__') value = '';
  return value || '';
}
function syncFilterPanels() {
  document.querySelectorAll('.filter-panel').forEach(function(panel) {
    var dirty = false, advDirty = false;
    panel.querySelectorAll('.filter-field').forEach(function(field) {
      var changed = filterFieldValue(field) !== (field.getAttribute('data-default') || '');
      if (changed) dirty = true;
      if (changed && field.closest('.advanced-filters')) advDirty = true;
    });
    var btn = panel.querySelector('.filter-clear');
    if (btn) {
      btn.classList.toggle('is-muted', !dirty);
      btn.setAttribute('aria-disabled', dirty ? 'false' : 'true');
    }
    var details = panel.querySelector('.advanced-filters');
    if (!details) return;
    if (advDirty) {
      details.dataset.autoOpen = '1';
      details.open = true;
      details.dataset.autoOpen = '';
    } else if (panel.dataset.clearing === '1') {
      details.open = false;
      delete details.dataset.userOpen;
    }
  });
}
window.syncFilterPanels = syncFilterPanels;
function armFilterSelect(el) {
  if (!el || !el.selectize || !el.closest || !el.closest('.filter-panel')) return;
  var inst = el.selectize;
  if (el._portalInst === inst) return;
  el._portalInst = inst;
  inst.settings.dropdownParent = 'body';
  if (inst.$dropdown && inst.$dropdown.parent()[0] !== document.body) inst.$dropdown.appendTo(document.body);
  inst.$dropdown.addClass('insights-portal');
  inst.positionDropdown = function() { placeFilterMenu(inst); };
  inst.on('dropdown_open', function() {
    var self = this;
    placeFilterMenu(self);
    setTimeout(function() { placeFilterMenu(self); }, 0);
  });
}
$(document).on('shiny:connected', function() {
  var ready = document.querySelectorAll('.filter-panel select');
  for (var i = 0; i < ready.length; i++) armFilterSelect(ready[i]);
  setTimeout(syncFilterPanels, 0);
});
$(document).on('shiny:bound', function(e) {
  var t = e.target;
  if (t && t.tagName === 'SELECT') armFilterSelect(t);
  if (t && t.querySelectorAll) {
    var nodes = t.querySelectorAll('select');
    for (var i = 0; i < nodes.length; i++) armFilterSelect(nodes[i]);
  }
});
document.addEventListener('scroll', function(e) {
  var host = e.target;
  if (host && host.closest && host.closest('.filter-panel')) {
    var nodes = document.querySelectorAll('.filter-panel select');
    for (var i = 0; i < nodes.length; i++) {
      if (nodes[i].selectize && nodes[i].selectize.isOpen) placeFilterMenu(nodes[i].selectize);
    }
  }
  if (infoTipBtn) placeInfoTip(infoTipBtn);
}, true);
$(window).on('resize', function() {
  var nodes = document.querySelectorAll('.filter-panel select');
  for (var i = 0; i < nodes.length; i++) {
    if (nodes[i].selectize && nodes[i].selectize.isOpen) placeFilterMenu(nodes[i].selectize);
  }
  if (infoTipBtn) placeInfoTip(infoTipBtn);
});
$(document).on('mouseenter focusin', '.info-tip', function() {
  document.querySelectorAll('.info-tip.is-tip').forEach(function(other) {
    if (other !== this) other.classList.remove('is-tip');
  }.bind(this));
  this.classList.add('is-open');
  placeInfoTip(this);
});
$(document).on('mouseleave focusout', '.info-tip', function() {
  if (this.classList.contains('is-tip')) return;
  this.classList.remove('is-open');
  if (infoTipBtn === this) hideInfoTip();
});
$(document).on('click', '.info-tip', function(e) {
  e.preventDefault();
  e.stopPropagation();
  var pin = !this.classList.contains('is-tip');
  document.querySelectorAll('.info-tip').forEach(function(other) { other.classList.remove('is-tip'); });
  if (pin) {
    this.classList.add('is-tip');
    placeInfoTip(this);
  } else {
    hideInfoTip();
  }
});
$(document).on('toggle', '.filter-panel .advanced-filters', function() {
  if (this.dataset.autoOpen === '1') return;
  if (this.open) this.dataset.userOpen = '1';
  else delete this.dataset.userOpen;
});
$(document).on('click', '.filter-clear', function() {
  var panel = this.closest('.filter-panel');
  if (panel) panel.dataset.clearing = '1';
  setTimeout(function() {
    if (panel) delete panel.dataset.clearing;
    syncFilterPanels();
  }, 700);
});
$(document).on('shiny:inputchanged shiny:value', function() {
  setTimeout(syncFilterPanels, 0);
});
$(document).on('keydown', function(e) {
  if (e.key !== 'Escape') return;
  hideInfoTip();
});
$(document).on('click', function(e) {
  if ($(e.target).closest('.info-tip').length) return;
  hideInfoTip();
  if ($(e.target).closest('.insights-bar, .insights-point').length) return;
  $('.insights-bar, .insights-point').removeClass('is-tip');
  $('.insights-spend-guide').removeClass('is-on');
});
$(document).on('click', '#close_dash_filter', function(e) {
  e.preventDefault();
  if (window.innerWidth <= 860) {
    $('.dashboard-sidebar').removeClass('is-open');
    $('#toggle_dash_filter').removeClass('is-active');
  } else {
    $('.dashboard-layout').addClass('sidebar-collapsed');
    $('#toggle_dash_filter').removeClass('is-active');
  }
});
// Close popovers on click outside on medium/lower screens
$(document).on('click', function(e) {
  if (window.innerWidth <= 860) {
    if (!$(e.target).closest('.requests-sidebar, #toggle_requests_filter, .datepicker, .datepicker-dropdown, .selectize-dropdown').length) {
      $('.requests-sidebar').removeClass('is-open');
      $('#toggle_requests_filter').removeClass('is-active');
    }
    if (!$(e.target).closest('.dashboard-sidebar, #toggle_dash_filter, .datepicker, .datepicker-dropdown, .selectize-dropdown').length) {
      $('.dashboard-sidebar').removeClass('is-open');
      $('#toggle_dash_filter').removeClass('is-active');
    }
  }
});
$(document).on('click', '.requests-sidebar, .dashboard-sidebar', function(e) {
  if (window.innerWidth <= 860) {
    e.stopPropagation();
  }
});
$(document).on('click', '#insights_apply', function() {
  if (window.innerWidth <= 860) {
    $('.dashboard-sidebar').removeClass('is-open');
    $('#toggle_dash_filter').removeClass('is-active');
  }
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

# Each request row opens that request. The description's see-more link stays separate.
request_table_init <- function(selected_status = "") {
  JS(r"---(
function(settings, json) {
  var table = this.api().table().node();
  $(table).off('click.expenseOpen', 'tbody tr').on('click.expenseOpen', 'tbody tr', function(e) {
    if ($(e.target).closest('a,button,input,select,label').length) return;
    var id = $(this).attr('data-id');
    if (!id) return;
    Shiny.setInputValue('open_request_row', {id: id, nonce: Math.random()}, {priority: 'event'});
  });
  $(table).off('click.expenseMore', 'a.dmore').on('click.expenseMore', 'a.dmore', function(e) {
    e.preventDefault(); e.stopPropagation();
    Shiny.setInputValue('see_more', {title: $(this).attr('data-title'), text: $(this).attr('data-full'), nonce: Math.random()}, {priority: 'event'});
  });
}
)---")
}

# Weekly report table initialization: row clicks open read-only request details.
weekly_table_init <- function() {
  JS(r"---(
function(settings, json) {
  var table = this.api().table().node();
  $(table).off('click.expenseWeekly', 'tbody tr').on('click.expenseWeekly', 'tbody tr', function(e) {
    if ($(e.target).closest('a,button,input,select,label').length) return;
    var id = $(this).attr('data-id');
    if (!id) return;
    Shiny.setInputValue('open_weekly_row', {id: id, nonce: Math.random()}, {priority: 'event'});
  });
  $(table).off('click.expenseMore', 'a.dmore').on('click.expenseMore', 'a.dmore', function(e) {
    e.preventDefault(); e.stopPropagation();
    Shiny.setInputValue('see_more', {title: $(this).attr('data-title'), text: $(this).attr('data-full'), nonce: Math.random()}, {priority: 'event'});
  });
}
)---")
}

dt_language <- list(
  search = "", searchPlaceholder = "Search all columns...",
  paginate = list(previous = "\u2190", `next` = "\u2192"),
  emptyTable = "No requests yet", zeroRecords = "No matching requests")

# =============================================================================
# UI
