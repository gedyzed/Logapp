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
}
[data-bs-theme='dark']{
  --app-bg:#0b1220; --app-card-bg:#121a2b; --app-input-bg:#121a2b;
  --app-border:#243044; --app-muted:#1a2436; --app-muted-fg:#cbd5e1;
  --app-accent:#38bdf8; --app-accent-soft:#0c4a6e;
  --bs-body-bg:#0b1220; --bs-body-color:#f8fafc; --bs-secondary-color:#cbd5e1;
  --bs-tertiary-bg:#1a2436; --bs-border-color:#243044; --bs-card-bg:#121a2b;
  --nav-bg:#23313F; --nav-fg:#ffffff; --nav-muted:rgba(255,255,255,.78);
  --nav-active:#9aabff;
}
html,body,.navbar,.navbar-brand,.nav-link,.btn,.form-control,.form-select,.selectize-input,.selectize-dropdown,table.dataTable,.dataTables_wrapper,.modal,.card,h1,h2,h3,h4,.brand-name,.brand-sub,.status-card{
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
body.modal-open .requests-page,body.modal-open .dashboard-shell,body.modal-open .fin-page,body.modal-open .manage-layout,body.modal-open .navbar,body.modal-open .app-footer{filter:blur(4px) grayscale(15%)!important;pointer-events:none!important;user-select:none!important;transition:filter .2s ease}
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
.status-waiting{background:#f1f5f9;color:#475569}
.status-under-validation{background:#fef3c7;color:#92400e}
.status-paid{background:#dcfce7;color:#166534}
.status-rejected,.status-cancelled{background:#fee2e2;color:#991b1b}
.status-under_review{background:#f1f5f9;color:#475569}
.status-pending{background:#fef3c7;color:#92400e}
[data-bs-theme='dark'] .status-waiting,[data-bs-theme='dark'] .status-under_review{background:rgba(148,163,184,.15);color:#94a3b8}
[data-bs-theme='dark'] .status-under-validation,[data-bs-theme='dark'] .status-pending{background:rgba(245,158,11,.15);color:#f59e0b}
[data-bs-theme='dark'] .status-paid{background:rgba(22,163,74,.15);color:#4ade80}
[data-bs-theme='dark'] .status-rejected,[data-bs-theme='dark'] .status-cancelled{background:rgba(220,38,38,.15);color:#f87171}
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
.requests-sidebar{position:sticky;top:1rem;max-height:calc(100vh - 6rem);overflow:auto;border:1px solid var(--app-border);border-radius:var(--app-radius);background:var(--app-card-bg)}
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
.request-status-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:.75rem;margin:0 0 1rem}
.status-card{display:block;width:100%;min-width:0;text-align:left;padding:.9rem 1rem .85rem;border:1px solid var(--app-border)!important;border-radius:.75rem;background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;white-space:normal;box-shadow:0 1px 2px rgba(15,23,42,.04)}
.status-card:hover,.status-card:focus{border-color:var(--app-accent)!important;color:var(--bs-body-color)!important}
.status-card.is-active{border-color:var(--app-accent)!important;box-shadow:0 0 0 2px rgba(2,132,199,.18)}
.status-card-top{display:flex;align-items:flex-start;justify-content:space-between;gap:.5rem}
.status-card-icon{width:1.85rem;height:1.85rem;border-radius:.5rem;display:inline-flex;align-items:center;justify-content:center;background:var(--app-muted);color:var(--app-muted-fg);flex:0 0 auto}
.status-card.tone-review{border-top:3px solid #94a3b8}
.status-card.tone-pending{border-top:3px solid #f59e0b}
.status-card.tone-paid{border-top:3px solid #16a34a}
.status-card.tone-cancelled{border-top:3px solid #a1a1aa}
.status-card-amount{margin-top:.35rem;font-weight:650;font-size:.95rem;letter-spacing:-.02em}
.status-card-note{margin-top:.15rem;font-size:.75rem;color:var(--app-muted-fg);font-weight:600}
.status-card.tone-paid .status-card-note{color:#16a34a}
.status-overdue-pill{display:inline-flex;align-items:center;gap:.35rem;padding:.2rem .65rem;border-radius:1rem;font-size:.74rem;font-weight:650;background:rgba(239,68,68,.12);border:1px solid rgba(239,68,68,.35);color:#dc2626;margin-top:.4rem;line-height:1.2}
.status-overdue-pill i,.status-overdue-pill svg{font-size:.76rem}
[data-bs-theme='dark'] .status-overdue-pill{background:rgba(239,68,68,.18);border-color:rgba(239,68,68,.45);color:#f87171}
.req-overdue-flag{display:flex;align-items:center;gap:.28rem;font-size:.74rem;font-weight:650;color:#dc2626;margin-top:.25rem;line-height:1}
.req-overdue-flag i,.req-overdue-flag svg{font-size:.72rem}
[data-bs-theme='dark'] .req-overdue-flag{color:#f87171}
.req-pill{display:inline-flex;align-items:center;justify-content:center;min-width:6.4rem;font-size:.75rem;font-weight:650;padding:.32rem .7rem;border-radius:999px;white-space:nowrap}
.req-pill-under_review{background:#e2e8f0;color:#334155}
.req-pill-pending{background:#fef3c7;color:#b45309}
.req-pill-paid{background:#dcfce7;color:#166534}
.req-pill-cancelled{background:#f4f4f5;color:#52525b}
[data-bs-theme='dark'] .req-pill-under_review{background:rgba(148,163,184,.18);color:#cbd5e1}
[data-bs-theme='dark'] .req-pill-pending{background:rgba(245,158,11,.18);color:#fbbf24}
[data-bs-theme='dark'] .req-pill-paid{background:rgba(22,163,74,.18);color:#4ade80}
[data-bs-theme='dark'] .req-pill-cancelled{background:rgba(161,161,170,.16);color:#d4d4d8}
.req-ref{color:var(--app-accent);font-weight:650;font-size:.8rem}
.status-chips{display:flex;flex-wrap:wrap;gap:.35rem;margin:0 0 .85rem}
.status-chip.btn{border:1px solid var(--app-border)!important;background:var(--app-card-bg)!important;color:var(--bs-body-color)!important;border-radius:999px!important;padding:.2rem .7rem!important;font-size:.78rem;font-weight:650;line-height:1.4}
.status-chip.btn.is-on{background:var(--app-accent)!important;border-color:var(--app-accent)!important;color:#fff!important}
.sidebar-clear.btn{border:0!important;background:transparent!important;color:var(--app-accent)!important;font-weight:650;padding:.1rem .2rem!important;font-size:.82rem}
.advanced-filters{margin-top:.35rem;border-top:1px solid var(--app-border);padding-top:.7rem}
.advanced-filters summary{cursor:pointer;font-weight:650;font-size:.84rem;color:var(--app-accent);margin-bottom:.7rem}
.filter-search{position:relative}
.filter-search>.fa,.filter-search>svg,.filter-search .fa-magnifying-glass{position:absolute;left:.75rem;top:.65rem;z-index:2;pointer-events:none;color:var(--app-muted-fg);font-size:.8rem}
.filter-search .form-control{padding-left:2.15rem}
.requests-filter-toggle{display:none!important}
.status-card-label{display:flex;align-items:center;gap:.45rem;color:var(--app-muted-fg);font-size:.78rem;font-weight:600;margin-bottom:.45rem}
.status-card-value{font-size:1.9rem;line-height:1.05;letter-spacing:-.05em;font-weight:700}
.status-dot{display:inline-block;width:.55rem;height:.55rem;border-radius:100%;flex:0 0 auto}
.status-dot-all{background:var(--app-accent)}.status-dot-waiting{background:#94a3b8}.status-dot-validation{background:#f59e0b}.status-dot-paid{background:#16a34a}.status-dot-rejected{background:#dc2626}
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
body.bslib-page-navbar>.container-fluid{flex:1 1 auto;display:flex;flex-direction:column;width:100%}
body.bslib-page-navbar .tab-content{flex:1 1 auto;display:flex;flex-direction:column}
body.bslib-page-navbar .tab-pane.active{flex:1 1 auto;display:flex;flex-direction:column}
.page-shell,.dashboard-layout,.manage-layout{flex:1 1 auto}
.app-footer{margin-top:auto;flex:0 0 auto;padding:.9rem 1.5rem;border-top:1px solid #1e293b;background:#1e293b!important;color:rgba(255,255,255,.82)!important;font-size:.82rem;width:100%;display:flex;align-items:center;justify-content:center;gap:1.5rem}
.footer-logo{height:28px;width:auto;display:block}
.footer-copy{font-weight:500;letter-spacing:.01em}
[data-bs-theme='dark'] .app-footer{border-top:1px solid #182234;background:#0d1527!important;color:rgba(255,255,255,.75)!important}
@media(max-width:1150px){.metrics-grid{grid-template-columns:repeat(3,minmax(0,1fr))}.dashboard-metrics-grid{grid-template-columns:repeat(2,minmax(0,1fr))!important}.dashboard-lower-grid{grid-template-columns:1fr}}
@media(max-width:860px){.request-status-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:992px){.dashboard-layout{grid-template-columns:1fr}.dashboard-sidebar{position:static;max-height:none;order:-1}}
@media(max-width:920px){.requests-layout{grid-template-columns:1fr}.requests-sidebar{position:static;max-height:none;grid-column:auto;grid-row:auto;order:-1}.requests-main{grid-column:auto;grid-row:auto}.manage-two-col{grid-template-columns:1fr}}
@media(max-width:720px){.dashboard-breakdown-title{align-items:flex-start}}
@media(max-width:600px){.request-status-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.status-card-value{font-size:1.35rem}.requests-page,.dashboard-layout{padding:.75rem}.dashboard-shell,.manage-layout{padding:.8rem .75rem 1.2rem}.metrics-grid{grid-template-columns:1fr}.dashboard-metrics-grid{grid-template-columns:1fr!important}.dashboard-chart-title .shiny-input-container{min-width:100%}}
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

"

js <- "
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('setTableStatus', function(m) {
    var status = (m && m.status) ? m.status : '';
    $('.status-chip').removeClass('is-on');
    $('.status-chip[data-status=\"' + status + '\"]').addClass('is-on');
  });
  Shiny.addCustomMessageHandler('cleanupModal', function(m) {
    $('.modal-backdrop').remove();
    $('body').removeClass('modal-open').css({'overflow': '', 'padding-right': ''});
    $('.modal:not(.show)').remove();
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
