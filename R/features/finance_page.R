<<<<<<< HEAD
# Financial Updates page. Mounted by requests_page.R. Server outputs register
# only when this file is sourced inside a Shiny session.
#
# Workflow: select paid projects, batch them, then move the batch from Active
# to Applied (deduction confirmed) or Rejected (projects return to the list).

fin_page_css <- '
.fin-heading-actions{display:flex;align-items:center;gap:.5rem;flex-wrap:wrap}
.fin-heading-actions .btn{font-weight:600}
.fin-alloc-card .card-body,.fin-history-card .card-header{background:var(--app-card-bg)}
.fin-alloc-card .card-body{padding:0}
.fin-card-head{width:100%;display:flex;align-items:center;justify-content:space-between;gap:.75rem;flex-wrap:wrap}
.fin-card-title{display:flex;align-items:center;gap:.45rem;font-weight:650;font-size:1rem}
.fin-count{display:inline-flex;align-items:center;padding:.12rem .5rem;border-radius:999px;background:var(--app-muted);color:var(--app-muted-fg);font-size:.75rem;font-weight:600}
.fin-search{position:relative;width:min(280px,100%)}
.fin-search-icon{position:absolute;left:.7rem;top:50%;transform:translateY(-50%);color:var(--app-muted-fg);pointer-events:none;display:flex}
.fin-search input{width:100%;min-height:2.3rem;padding:.4rem .75rem .4rem 2.15rem;border:1px solid var(--app-border);border-radius:.5rem;background:var(--app-input-bg);color:var(--bs-body-color)}
.fin-search input:focus{outline:0;border-color:var(--app-accent);box-shadow:0 0 0 3px rgba(2,132,199,.14)}
.fin-table-wrap{width:100%;overflow-x:auto}
.fin-table{width:100%;border-collapse:collapse;font-size:.9rem}
.fin-table thead th{background:var(--app-muted);color:var(--app-muted-fg);font-size:.72rem;font-weight:700;letter-spacing:.04em;text-transform:uppercase;padding:.7rem .85rem;border-bottom:1px solid var(--app-border);text-align:left;white-space:nowrap}
.fin-table td{padding:.75rem .85rem;border-bottom:1px solid var(--app-border);vertical-align:middle}
.fin-table tbody tr.fin-row:hover td{background:var(--app-muted)}
.fin-col-toggle,.fin-col-check{width:2.4rem;padding-left:.7rem;padding-right:.2rem}
.fin-num{text-align:right}
.fin-neg{color:#dc2626;font-weight:650;font-variant-numeric:tabular-nums;white-space:nowrap}
[data-bs-theme=dark] .fin-neg{color:#f87171}
.fin-code{font-weight:650;letter-spacing:-.01em}
.fin-sub{color:var(--app-muted-fg);font-size:.78rem;font-weight:500;margin-top:.12rem}
.fin-when{white-space:nowrap;color:var(--bs-body-color);font-size:.86rem}
.fin-project-check{width:1.05rem;height:1.05rem;accent-color:var(--app-accent)}
.fin-project-check:focus-visible,.fin-expand:focus-visible{outline:2px solid var(--app-accent);outline-offset:2px}
.fin-expand{width:1.7rem;height:1.7rem;border:0;background:transparent;color:var(--app-muted-fg);display:inline-flex;align-items:center;justify-content:center;cursor:pointer;border-radius:.35rem;padding:0}
.fin-expand:hover{background:var(--app-muted)}
.fin-chevron{width:.42rem;height:.42rem;border-right:2px solid currentColor;border-bottom:2px solid currentColor;transform:rotate(-45deg);display:block;margin-left:-.12rem}
.fin-expand.is-open .fin-chevron{transform:rotate(45deg);margin-left:0;margin-top:-.12rem}
.fin-lines{display:none}
.fin-lines.is-open{display:table-row}
.fin-row.fin-filtered,.fin-lines.fin-filtered,.fin-row.fin-paged-out,.fin-lines.fin-paged-out{display:none!important}
.fin-lines td{background:var(--app-muted);padding:.35rem 1rem .85rem 3.2rem}
.fin-lines-wrap{border:1px solid var(--app-border);border-radius:.5rem;overflow:hidden;background:var(--app-card-bg)}
.fin-lines-table{width:100%;border-collapse:collapse;font-size:.84rem}
.fin-lines-table th{font-size:.7rem;font-weight:700;letter-spacing:.04em;text-transform:uppercase;color:var(--app-muted-fg);text-align:left;padding:.45rem .75rem;border-bottom:1px solid var(--app-border)}
.fin-lines-table td{padding:.45rem .75rem;border-bottom:1px solid var(--app-border);background:var(--app-card-bg)}
.fin-lines-table tr:last-child td{border-bottom:0}
.fin-empty{margin:0;padding:1.15rem 1rem;color:var(--app-muted-fg)}
.fin-card-head{flex:1 1 auto}
.fin-lines-table th.fin-num,.fin-lines-table td.fin-num{text-align:right}
.fin-alloc-card{display:flex;flex-direction:column;flex:1 1 auto}
.fin-alloc-card>.card-body{display:flex;flex-direction:column;flex:1 1 auto}
.fin-foot{display:flex;align-items:center;justify-content:space-between;gap:.75rem;flex-wrap:wrap;padding:.75rem .9rem;margin-top:auto;color:var(--app-muted-fg);font-size:.82rem}
.fin-foot-hidden{display:none!important}
.fin-foot-controls{display:flex;align-items:center;gap:.75rem;flex-wrap:wrap}
.fin-page-size{display:flex;align-items:center;gap:.4rem;margin:0}
.fin-page-size select{border:1px solid var(--app-border);border-radius:.4rem;background:var(--app-input-bg);color:var(--bs-body-color);padding:.2rem .4rem}
#fin_pager{display:flex;gap:.3rem}
.fin-page-btn{min-width:1.85rem;height:1.85rem;padding:0 .45rem;border:1px solid var(--app-border);background:var(--app-card-bg);color:var(--bs-body-color);border-radius:.4rem;font-weight:600;cursor:pointer}
.fin-page-btn.is-current{background:var(--app-accent);border-color:var(--app-accent);color:#fff}
.fin-page-btn:disabled{opacity:.45;cursor:not-allowed}
.fin-history-card{margin-top:1rem}
.fin-tabs{display:flex;gap:.15rem;border-bottom:1px solid var(--app-border);margin:-.15rem 0 .9rem}
.fin-tab{appearance:none;background:transparent;border:0;border-bottom:2px solid transparent;color:var(--app-muted-fg);font-weight:650;font-size:.92rem;padding:.45rem .15rem .55rem;margin-right:1rem;cursor:pointer}
.fin-tab.is-active{color:var(--app-accent);border-bottom-color:var(--app-accent)}
.fin-tab-count{display:inline-flex;min-width:1.2rem;justify-content:center;margin-left:.4rem;padding:0 .35rem;border-radius:999px;background:var(--app-muted);font-size:.72rem}
.fin-tab.is-active .fin-tab-count{background:var(--app-accent-soft);color:var(--app-accent)}
.fin-batch-list{display:flex;flex-direction:column;gap:.7rem}
.fin-batch-card{display:flex;align-items:center;justify-content:space-between;gap:1rem;border:1px solid var(--app-border);border-radius:var(--app-radius);padding:.8rem 1rem;background:var(--app-card-bg)}
.fin-batch-main{display:flex;gap:.8rem;align-items:center;min-width:0}
.fin-batch-icon{width:2.4rem;height:2.4rem;border-radius:.55rem;display:flex;align-items:center;justify-content:center;background:var(--app-accent-soft);color:var(--app-accent);flex:0 0 auto}
.fin-batch-copy{min-width:0}
.fin-batch-title{display:flex;align-items:center;flex-wrap:wrap;gap:.35rem .45rem;font-weight:650}
.fin-batch-code{letter-spacing:-.01em}
.fin-pill{display:inline-block;font-size:.68rem;font-weight:700;letter-spacing:.03em;text-transform:uppercase;padding:.16rem .48rem;border-radius:999px}
.fin-pill-active{background:#fef3c7;color:#b45309}
.fin-pill-applied{background:#dcfce7;color:#166534}
.fin-pill-rejected{background:#fee2e2;color:#991b1b}
[data-bs-theme=dark] .fin-pill-active{background:rgba(245,158,11,.18);color:#fbbf24}
[data-bs-theme=dark] .fin-pill-applied{background:rgba(22,163,74,.18);color:#4ade80}
[data-bs-theme=dark] .fin-pill-rejected{background:rgba(220,38,38,.18);color:#f87171}
.fin-batch-meta{display:flex;flex-wrap:wrap;align-items:center;row-gap:.3rem;color:var(--app-muted-fg);font-size:.8rem;margin-top:.28rem}
.fin-meta-item{display:inline-flex;align-items:center;gap:.35rem;white-space:nowrap}
.fin-meta-item i,.fin-meta-item .fa,.fin-meta-item .fas,.fin-meta-item .far{font-size:.75rem;opacity:.85}
.fin-meta-item + .fin-meta-item{margin-left:.85rem;padding-left:.85rem;border-left:1px solid var(--app-border)}
.fin-batch-actions{display:flex;gap:.45rem;flex-wrap:wrap;justify-content:flex-end}
.fin-batch-actions .btn{white-space:nowrap}
.btn.fin-reject-btn{border:1px solid #dc2626;color:#dc2626;background:transparent;font-weight:600}
.btn.fin-reject-btn:hover,.btn.fin-reject-btn:focus{background:rgba(220,38,38,.08);color:#dc2626;border-color:#dc2626}
.fin-history-card .fin-empty{padding:.15rem 0 .2rem}
a.btn.fin-export-btn{display:inline-flex;align-items:center;gap:.4rem;text-decoration:none;font-weight:600;background:var(--app-card-bg);color:var(--bs-body-color);border:1px solid var(--app-border)}
a.btn.fin-export-btn:hover,a.btn.fin-export-btn:focus{background:var(--app-muted);color:var(--bs-body-color);border-color:var(--app-border)}
.fin-excel-icon{width:1rem;height:1rem;display:block;flex:0 0 auto;color:#217346}
[data-bs-theme=dark] .fin-excel-icon{color:#3ddc84}
@media(max-width:760px){
  .fin-heading-actions,.fin-search{width:100%}
  .fin-batch-card{flex-direction:column;align-items:stretch}
  .fin-batch-actions{justify-content:flex-start}
  .fin-lines td{padding-left:1rem}
}
'

fin_page_js <- "
(function() {
  var finPicked = {};
  var finLastSent = null;
  var finPage = 1;

  function finLinesFor(code) {
    return $('.fin-lines').filter(function() { return $(this).attr('data-project') === code; });
  }

  function finQuery() {
    return ($('#fin_project_search').val() || '').toLowerCase().trim();
  }

  function finMatchingRows() {
    var q = finQuery();
    return $('.fin-row').filter(function() {
      if (!q) return true;
      return (($(this).attr('data-search') || '').toLowerCase().indexOf(q) >= 0);
    });
  }

  function finRenderPager(pages) {
    var pager = $('#fin_pager');
    if (!pager.length) return;
    pager.empty();
    function add(label, page, opts) {
      var b = $('<button>', {type: 'button', 'class': 'fin-page-btn'});
      b.text(label);
      b.attr('aria-label', (opts && opts.name) ? opts.name : ('Page ' + label));
      if (opts && opts.current) b.addClass('is-current').attr('aria-current', 'page');
      if (opts && opts.disabled) b.prop('disabled', true);
      else b.on('click', function() { finPage = page; finApplyView(); });
      pager.append(b);
    }
    add('<', finPage - 1, {disabled: finPage <= 1, name: 'Previous page'});
    var windowStart = Math.max(1, finPage - 2);
    var windowEnd = Math.min(pages, windowStart + 4);
    windowStart = Math.max(1, windowEnd - 4);
    for (var p = windowStart; p <= windowEnd; p++) add(String(p), p, {current: p === finPage});
    add('>', finPage + 1, {disabled: finPage >= pages, name: 'Next page'});
  }

  function finSyncSelection() {
    var root = document.getElementById('fin_project_cards');
    var ready = root && (root.querySelector('.fin-table') || root.querySelector('.fin-empty'));
    var batchBtn = $('#fin_btn_batch');
    if (!ready) {
      batchBtn.prop('disabled', true).attr('title', 'Select a project first');
      $('.fin-batch-label').text('Batch Selected');
      $('#fin_select_all_btn').prop('disabled', true);
      return;
    }
    var visible = {};
    $('.fin-project-check').each(function() {
      var code = $(this).attr('data-project');
      visible[code] = true;
      this.checked = !!finPicked[code];
    });
    Object.keys(finPicked).forEach(function(code) {
      if (!visible[code]) delete finPicked[code];
    });
    var codes = Object.keys(finPicked);
    var sig = codes.slice().sort().join('||');
    batchBtn.prop('disabled', codes.length === 0);
    batchBtn.attr('title', codes.length ? '' : 'Select a project first');
    $('.fin-batch-label').text(codes.length ? ('Batch Selected (' + codes.length + ')') : 'Batch Selected');
    var enabled = finMatchingRows().find('.fin-project-check');
    var selectedCount = enabled.filter(function() { return !!finPicked[$(this).attr('data-project')]; }).length;
    var allOn = enabled.length > 0 && selectedCount === enabled.length;
    $('#fin_select_all_btn')
      .prop('disabled', enabled.length === 0)
      .attr('aria-pressed', allOn ? 'true' : 'false')
      .text(allOn ? 'Clear' : 'Select All');
    if (sig === finLastSent || !window.Shiny) return;
    finLastSent = sig;
    Shiny.setInputValue('fin_project_select', codes, {priority: 'event'});
  }

  function finApplyView() {
    var total = $('.fin-row').length;
    var count = document.getElementById('fin_project_count');
    if (count) count.textContent = total + (total === 1 ? ' project' : ' projects');
    $('.fin-foot').toggleClass('fin-foot-hidden', total === 0);
    var size = parseInt($('#fin_page_size').val(), 10);
    if (!size || size < 1) size = 10;
    var match = finMatchingRows();
    var matchCodes = {};
    match.each(function() { matchCodes[$(this).attr('data-project')] = true; });
    $('.fin-row').each(function() {
      var code = $(this).attr('data-project');
      var on = !!matchCodes[code];
      $(this).toggleClass('fin-filtered', !on);
      finLinesFor(code).toggleClass('fin-filtered', !on);
    });
    var n = match.length;
    var pages = Math.max(1, Math.ceil(n / size) || 1);
    if (finPage > pages) finPage = pages;
    if (finPage < 1) finPage = 1;
    match.each(function(i) {
      var code = $(this).attr('data-project');
      var show = n > 0 && i >= (finPage - 1) * size && i < finPage * size;
      $(this).toggleClass('fin-paged-out', !show);
      finLinesFor(code).toggleClass('fin-paged-out', !show);
    });
    var label = document.getElementById('fin_page_label');
    if (label) {
      if (!total) label.textContent = 'Showing 0 projects';
      else if (!n) label.textContent = 'No projects match that search.';
      else {
        var start = (finPage - 1) * size + 1;
        var end = Math.min(finPage * size, n);
        label.textContent = 'Showing ' + start + '-' + end + ' of ' + n + (n === 1 ? ' project' : ' projects');
      }
    }
    finRenderPager(n ? pages : 1);
    finSyncSelection();
  }

  function finBindMessages() {
    if (window.finMsgReady || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    window.finMsgReady = true;
    Shiny.addCustomMessageHandler('fin-show-history', function() {
      var el = document.getElementById('fin_batch_history');
      if (el && el.scrollIntoView) el.scrollIntoView({behavior: 'smooth', block: 'nearest'});
    });
  }

  $(document).off('change.finProject').on('change.finProject', '.fin-project-check', function() {
    var code = $(this).attr('data-project');
    if (this.checked) finPicked[code] = true;
    else delete finPicked[code];
    finSyncSelection();
  });

  $(document).off('click.finSelectAll').on('click.finSelectAll', '#fin_select_all_btn', function() {
    var targets = finMatchingRows().find('.fin-project-check');
    var allOn = targets.length > 0 && targets.filter(':checked').length === targets.length;
    targets.each(function() {
      var code = $(this).attr('data-project');
      if (allOn) delete finPicked[code];
      else finPicked[code] = true;
    });
    finSyncSelection();
  });

  $(document).off('click.finExpand').on('click.finExpand', '.fin-expand', function() {
    var btn = $(this);
    var row = btn.closest('.fin-row');
    if (row.hasClass('fin-filtered') || row.hasClass('fin-paged-out')) return;
    var code = row.attr('data-project');
    var open = btn.attr('aria-expanded') === 'true';
    btn.attr('aria-expanded', open ? 'false' : 'true').toggleClass('is-open', !open);
    finLinesFor(code).toggleClass('is-open', !open);
  });

  $(document).off('input.finSearch').on('input.finSearch', '#fin_project_search', function() {
    finPage = 1;
    finApplyView();
  });

  $(document).off('change.finPageSize').on('change.finPageSize', '#fin_page_size', function() {
    finPage = 1;
    finApplyView();
  });

  $(document).off('click.finApply').on('click.finApply', '.fin-apply-btn', function() {
    if (window.Shiny) Shiny.setInputValue('fin_btn_confirm', {code: $(this).attr('data-batch'), nonce: Math.random()}, {priority: 'event'});
  });

  $(document).off('click.finReject').on('click.finReject', '.fin-reject-btn', function() {
    if (window.Shiny) Shiny.setInputValue('fin_btn_reject', {code: $(this).attr('data-batch'), nonce: Math.random()}, {priority: 'event'});
  });

  $(document).off('click.finTab').on('click.finTab', '.fin-tab', function() {
    if ($(this).hasClass('is-active') || !window.Shiny) return;
    Shiny.setInputValue('fin_history_tab', {tab: $(this).attr('data-tab'), nonce: Math.random()}, {priority: 'event'});
  });

  $(document).off('shiny:value.finProjects').on('shiny:value.finProjects', function(event) {
    if (event.name === 'fin_project_cards') setTimeout(finApplyView, 0);
  });

  $(document).off('shiny:connected.finPage').on('shiny:connected.finPage', finBindMessages);
  $(function() {
    finBindMessages();
    setTimeout(finApplyView, 0);
  });
})();
"

fin_excel_icon <- function() {
  tags$svg(
    class = "fin-excel-icon", viewBox = "0 0 24 24", `aria-hidden` = "true", focusable = "false",
    tags$path(fill = "currentColor", d = "M6 2.5h8.2L19.5 7.8V20a1.5 1.5 0 0 1-1.5 1.5H6A1.5 1.5 0 0 1 4.5 20V4A1.5 1.5 0 0 1 6 2.5z"),
    tags$path(fill = "currentColor", opacity = "0.45", d = "M14.2 2.6 19.4 7.9H14.2z"),
    tags$path(fill = "#fff", d = "M8.05 16.85 10.2 13.7 8.15 10.5h1.75l1.15 1.95 1.15-1.95h1.7L11.85 13.7l2.15 3.15h-1.8l-1.25-2.05-1.2 2.05z")
  )
}

finance_nav_panel <- function() {
  nav_panel(
    title = "Financial Updates", value = "financial_update",
    div(class = "page-shell",
        tags$style(HTML(fin_page_css)),
        tags$script(HTML(fin_page_js)),
        div(class = "page-heading",
            div(
              h1("Financial Updates"),
              p("Select paid projects, batch them, then confirm the deduction or reject the batch.")
            ),
            div(class = "fin-heading-actions",
                tags$button(id = "fin_select_all_btn", type = "button", class = "btn btn-outline-secondary",
                            "Select All"),
                actionButton("fin_btn_batch",
                             tagList(icon("layer-group"), tags$span(class = "fin-batch-label", "Batch Selected")),
                             class = "btn-primary",
                             title = "Select a project first",
                             disabled = NA)
            )
        ),
        card(class = "fin-alloc-card", fill = FALSE,
             card_header(
               div(class = "fin-card-head",
                   div(class = "fin-card-title",
                       "Paid projects",
                       span(id = "fin_project_count", class = "fin-count", "0 projects")
                   ),
                   div(class = "fin-search",
                       span(class = "fin-search-icon", icon("magnifying-glass")),
                       tags$input(id = "fin_project_search", type = "search", class = "form-control",
                                  placeholder = "Search project code...", autocomplete = "off",
                                  "aria-label" = "Search project code")
                   )
               )
             ),
             div(class = "card-body",
                 uiOutput("fin_project_cards"),
                 div(class = "fin-foot",
                     span(id = "fin_page_label", "Showing 0 projects"),
                     div(class = "fin-foot-controls",
                         tags$label(class = "fin-page-size", "Rows per page",
                                    tags$select(id = "fin_page_size", "aria-label" = "Rows per page",
                                                tags$option(value = "10", selected = NA, "10"),
                                                tags$option(value = "25", "25"),
                                                tags$option(value = "50", "50"))),
                         div(id = "fin_pager", role = "navigation", "aria-label" = "Project pages")
                     )
                 )
             )
        ),
        card(class = "fin-history-card", fill = FALSE,
             card_header("Batch history"),
             div(class = "card-body", uiOutput("fin_batch_history"))
        )
    )
  )
}

if (exists("input", inherits = FALSE) && exists("output", inherits = FALSE)) {
  output$fin_project_cards <- renderUI({
    summary_df <- fin_project_summary()
    if (!nrow(summary_df)) {
      return(p(class = "fin-empty", "No paid projects are waiting to be batched."))
    }
    cur <- active_currency()
    lines <- fin_line_summary()
    body <- lapply(seq_len(nrow(summary_df)), function(i) {
      code <- as.character(summary_df$project_code[[i]])
      department <- as.character(summary_df$department[[i]] %||% "")
      if (is.na(department)) department <- ""
      amount <- summary_df$display_minor[[i]]
      if (is.na(amount)) amount <- 0
      part <- lines[lines$project_code == code, , drop = FALSE]
      line_rows <- if (!nrow(part)) {
        list(tags$tr(tags$td(colspan = "2", class = "fin-empty", "No budget lines")))
      } else {
        lapply(seq_len(nrow(part)), function(j) {
          tags$tr(
            tags$td(part$budget_line[[j]]),
            tags$td(class = "fin-num fin-neg", part$amount_label[[j]])
          )
        })
      }
      tagList(
        tags$tr(
          class = "fin-row",
          `data-project` = code,
          `data-search` = paste(code, department),
          tags$td(class = "fin-col-toggle",
                  tags$button(type = "button", class = "fin-expand", "aria-expanded" = "false",
                              "aria-label" = paste("Show budget lines for", code),
                              span(class = "fin-chevron"))),
          tags$td(class = "fin-col-check",
                  tags$input(type = "checkbox", class = "fin-project-check",
                             "data-project" = code, "aria-label" = paste("Select", code))),
          tags$td(
            div(class = "fin-code", code),
            if (nzchar(department)) div(class = "fin-sub", department)
          ),
          tags$td(class = "fin-num fin-neg", fin_signed_amount(amount, cur)),
          tags$td(class = "fin-when", summary_df$checkpoint_label[[i]])
        ),
        tags$tr(
          class = "fin-lines",
          `data-project` = code,
          tags$td(colspan = "5",
                  div(class = "fin-lines-wrap",
                      tags$table(class = "fin-lines-table",
                                 tags$thead(tags$tr(
                                   tags$th("Budget line"),
                                   tags$th(class = "fin-num", "Deduction amount")
                                 )),
                                 tags$tbody(line_rows))))
        )
      )
    })
    div(class = "fin-table-wrap",
        tags$table(class = "fin-table",
                   tags$thead(tags$tr(
                     tags$th(class = "fin-col-toggle", ""),
                     tags$th(class = "fin-col-check", ""),
                     tags$th("Project"),
                     tags$th(class = "fin-num", "Pending deductions"),
                     tags$th("Last checkpoint")
                   )),
                   tags$tbody(body)))
  })

  output$fin_batch_history <- renderUI({
    rows <- fin_batches()
    tab <- fin_history_tab()
    if (!tab %in% c("active", "applied", "rejected")) tab <- "active"
    counts <- c(active = 0L, applied = 0L, rejected = 0L)
    if (nrow(rows)) {
      counted <- table(factor(rows$bucket, levels = c("active", "applied", "rejected")))
      counts[names(counted)] <- as.integer(counted)
    }
    tab_btn <- function(id, label) {
      tags$button(
        type = "button",
        class = paste("fin-tab", if (identical(tab, id)) "is-active"),
        `data-tab` = id,
        role = "tab",
        `aria-selected` = if (identical(tab, id)) "true" else "false",
        label,
        span(class = "fin-tab-count", counts[[id]])
      )
    }
    shown <- if (nrow(rows)) rows[rows$bucket == tab, , drop = FALSE] else rows
    empty_copy <- switch(tab,
      active = "No active batches. Select one or more paid projects, then choose Batch Selected.",
      applied = "No applied batches yet. Confirm an active batch after the deduction is on the financial sheets.",
      "No rejected batches. Reject an active batch to return its projects to the list."
    )
    cards <- if (!nrow(shown)) {
      p(class = "fin-empty", empty_copy)
    } else {
      lapply(seq_len(nrow(shown)), function(i) {
        r <- shown[i, , drop = FALSE]
        code <- as.character(r$batch_code[[1]])
        bucket <- as.character(r$bucket[[1]])
        projects <- fin_split_codes(r$project_codes[[1]])
        if (length(projects) > 4) {
          project_text <- paste0(paste(projects[1:4], collapse = ", "), " +", length(projects) - 4, " more")
        } else if (length(projects)) {
          project_text <- paste(projects, collapse = ", ")
        } else {
          project_text <- "No projects"
        }
        included <- if (!length(projects)) {
          "No projects"
        } else if (length(projects) == 1) {
          project_text
        } else {
          paste(length(projects), "projects")
        }
        total_amount <- suppressWarnings(as.numeric(r$total_amount[[1]]))
        total_label <- if (!length(total_amount) || is.na(total_amount[[1]])) {
          "\u2014"
        } else {
          paste(r$currency[[1]], formatC(abs(total_amount[[1]]), format = "f", digits = 2, big.mark = ","))
        }
        card_date <- function(stamp) sub(" \\d{1,2}:\\d{2}.*$", "", fin_stamp_label(stamp))
        pill <- switch(bucket,
          active = list(class = "fin-pill fin-pill-active", text = "Awaiting confirmation"),
          applied = list(class = "fin-pill fin-pill-applied", text = "Applied"),
          list(class = "fin-pill fin-pill-rejected", text = "Rejected")
        )
        meta <- list(
          span(class = "fin-meta-item", icon("calendar"), paste0("Created: ", card_date(r$created[[1]]))),
          span(class = "fin-meta-item", icon("list"), included),
          span(class = "fin-meta-item", icon("coins"), total_label, " Total")
        )
        if (identical(bucket, "applied")) {
          meta <- c(meta, list(span(class = "fin-meta-item", icon("circle-check"), paste0("Applied: ", card_date(r$confirmed_at[[1]])))))
        }
        actions <- list(
          tags$a(class = "btn btn-sm fin-export-btn",
                 href = paste0(fin_export_base, "&batch=", URLencode(code, reserved = TRUE)),
                 fin_excel_icon(), "Export Excel")
        )
        if (identical(bucket, "active")) {
          actions <- c(actions, list(
            tags$button(type = "button", class = "btn btn-sm fin-reject-btn",
                        `data-batch` = code, icon("ban"), " Reject"),
            tags$button(type = "button", class = "btn btn-primary btn-sm fin-apply-btn",
                        `data-batch` = code, icon("circle-check"), " Confirm Applied")
          ))
        }
        div(class = paste("fin-batch-card", paste0("is-", bucket)),
            div(class = "fin-batch-main",
                div(class = "fin-batch-icon", icon("file-lines")),
                div(class = "fin-batch-copy",
                    div(class = "fin-batch-title",
                        span(class = "fin-batch-code", if (grepl("^BTH-", code)) paste("Batch", code) else code),
                        span(class = pill$class, pill$text)
                    ),
                    div(class = "fin-batch-meta", meta)
                )
            ),
            div(class = "fin-batch-actions", actions)
        )
      })
    }
    tagList(
      div(class = "fin-tabs", role = "tablist",
          tab_btn("active", "Active"),
          tab_btn("applied", "Applied"),
          tab_btn("rejected", "Rejected")),
      div(class = "fin-batch-list", cards)
    )
  })

  fin_batch_filename <- function(code) {
    safe <- gsub("[^A-Za-z0-9_-]", "_", code)
    safe <- gsub("_+", "_", safe)
    safe <- gsub("^_|_$", "", safe)
    if (!nzchar(safe)) safe <- "batch"
    paste0(safe, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  }

  fin_write_batch_xlsx <- function(code, file) {
    pulled <- tryCatch(
      withReactiveDomain(session, isolate(list(
        batch = fin_find_batch(code),
        rows = data(),
        cur = active_currency()
      ))),
      error = function(e) e
    )
    if (inherits(pulled, "error") || is.null(pulled$batch)) {
      note <- if (inherits(pulled, "error")) "Could not build this export" else "Batch not found"
      write_xlsx(list(Batch = data.frame(Message = note)), file)
      return(invisible(FALSE))
    }
    d <- pulled$rows
    d <- d[d$id %in% pulled$batch$request_id_list[[1]], , drop = FALSE]
    if (!nrow(d)) {
      write_xlsx(list(Batch = data.frame(Message = "This batch has no request lines")), file)
      return(invisible(FALSE))
    }
    d$amount_minor_value <- fin_amount_minor(d)
    d$display_minor <- fin_to_display_minor(
      d$amount_minor_value, d$currency, pulled$cur, euroinfo_month_rates()
    )
    d$display_minor[is.na(d$display_minor)] <- 0
    sheet <- data.frame(
      `Batch Code` = pulled$batch$batch_code[[1]],
      `Project Code` = d$project_code,
      `Budget Line` = d$budget_line,
      `Request ID` = d$id,
      Description = d$description,
      `Original Amount` = fmt_amount(d$amount_minor_value),
      `Original Currency` = d$currency,
      `Display Amount` = fmt_amount(d$display_minor),
      `Display Currency` = pulled$cur,
      `Payment Date` = d$payment_date,
      check.names = FALSE
    )
    write_xlsx(list(Request_Lines = sheet), file)
    invisible(TRUE)
  }

  fin_share_url <- function() {
    q <- list(
      v = "fin",
      cur = if (nzchar(active_currency())) active_currency() else NULL,
      tab = if (nzchar(fin_history_tab())) fin_history_tab() else NULL
    )
    q <- Filter(Negate(is.null), q)
    cd <- session$clientData
    host <- paste0(cd$url_hostname, if (nzchar(cd$url_port %||% "")) paste0(":", cd$url_port))
    paste0(cd$url_protocol, "//", host, cd$url_pathname, "?",
           paste0(names(q), "=", unlist(q), collapse = "&"))
  }

  fin_export_base <- session$registerDataObj("fin_batch_xlsx", NULL, function(data, req) {
    query <- parseQueryString(req$QUERY_STRING %||% "")
    code <- trimws(as.character(query$batch %||% ""))
    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp), add = TRUE)
    tryCatch({
      fin_write_batch_xlsx(code, tmp)
      bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
      shiny:::httpResponse(
        status = 200L,
        content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        content = bytes,
        headers = list(
          "Content-Disposition" = paste0("attachment; filename=\"", fin_batch_filename(code), "\""),
          "Cache-Control" = "no-store"
        )
      )
    }, error = function(e) {
      shiny:::httpResponse(
        status = 500L,
        content_type = "text/plain; charset=UTF-8",
        content = conditionMessage(e)
      )
    })
  })
}
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/finance_page.R
# Owner: Friend
# Purpose: Implement Financial Updates UI.
# Expected code: Namespaced module: budget cards, nested line rows, selection, preview modal, current batch cards, apply/abandon dialogs and history.
# Connections: Calls finance_service and finance_export; uses components and refresh callback.
# Boundary: Budget checkboxes only; no line checkpoint date. USD batch currency. No financial SQL or calculation in render functions.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
