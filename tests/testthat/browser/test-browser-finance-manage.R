# Financial Updates and Manage, in a real browser, against an isolated SQLite file.
# Not collected by run_tests.R. Run with: Rscript scripts/test_browser.R

test_that("undeducted paid requests show in Financial Updates, and new catalogue rows show in Manage", {
  problem <- tracker_browser_problem()
  skip_if(nzchar(problem), problem)

  fixture <- local_tracker_db("2026-11-06 13:00:00")
  app <- tracker_drive(fixture$path, "2026-11-06 13:00:00")
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  tracker_login(app)
  tracker_wait_js(
    app,
    "document.querySelector('.requests-page') ? 'yes' : 'no'",
    seconds = 25
  )

  tracker_open_tab(app, "financial_update")
  tracker_wait_js(app, tracker_finance_has("FIN01"), seconds = 25)
  finance <- app$get_js("document.querySelector('#fin_project_cards') ? document.querySelector('#fin_project_cards').innerText : ''")
  expect_match(finance, "FIN01", fixed = TRUE)
  expect_false(grepl("No paid projects are waiting to be batched", finance, fixed = TRUE))
  # ALPHA's seeded paid requests are already in an applied batch, so they stay off this list.
  expect_false(grepl("ALPHA", finance, fixed = TRUE))

  tracker_open_tab(app, "requests")
  tracker_wait_js(app, "document.querySelector('#f_query') ? 'yes' : 'no'", seconds = 20)
  app$set_inputs(f_query = "ANCHOR-PENDING", wait_ = FALSE)
  tracker_wait_js(
    app,
    "var text = document.querySelector('.request-table-card') ? document.querySelector('.request-table-card').innerText : ''; text.indexOf('ANCHOR-PENDING') >= 0 ? 'yes' : 'no'",
    seconds = 20
  )
  app$run_js(
    "(function() {
      var rows = document.querySelectorAll('.request-table-card tbody tr');
      for (var i = 0; i < rows.length; i++) {
        if (rows[i].innerText.indexOf('ANCHOR-PENDING') >= 0) { rows[i].click(); return; }
      }
    })()"
  )
  tracker_wait_js(app, "document.querySelector('#btn_pay') ? 'yes' : 'no'", seconds = 20)
  app$click(selector = "#btn_pay")
  tracker_wait_js(app, "document.querySelector('#pay_confirm') ? 'yes' : 'no'", seconds = 20)
  app$click(selector = "#pay_confirm")
  app$click(selector = "#btn_pay_confirm")
  tracker_wait_js(app, "document.querySelector('#btn_pay_confirm') ? 'no' : 'yes'", seconds = 25)

  tracker_open_tab(app, "financial_update")
  tracker_wait_js(app, tracker_finance_has("ALPHA"), seconds = 25)
  finance_after <- app$get_js("document.querySelector('#fin_project_cards') ? document.querySelector('#fin_project_cards').innerText : ''")
  finance_html <- app$get_html("#fin_project_cards")
  expect_match(finance_after, "ALPHA", fixed = TRUE)
  expect_match(finance_html, "COMMS", fixed = TRUE)

  tracker_open_tab(app, "manage")
  tracker_wait_js(app, "document.querySelector('#cfg_project_code') ? 'yes' : 'no'", seconds = 20)
  app$run_js(
    "$('#cfg_project_code').val('ZZBROWSER').trigger('change');
     $('#cfg_project_department').val('REACH').trigger('change');"
  )
  tracker_wait_js(
    app,
    "document.querySelector('#cfg_project_code') && document.querySelector('#cfg_project_code').value === 'ZZBROWSER' ? 'yes' : 'no'",
    seconds = 15
  )
  app$click(selector = "#cfg_add_project")
  tracker_wait_js(
    app,
    "document.body.innerText.indexOf('Project ZZBROWSER added') >= 0 ? 'yes' : 'no'",
    seconds = 25
  )
  tracker_wait_catalog(app, "ZZBROWSER")
  # selectize keeps the full choice list on the widget, not on the hidden <select>.
  tracker_wait_js(
    app,
    "(function() {
      var el = document.querySelector('#cfg_budget_project');
      if (!el) return 'no';
      if (el.selectize && el.selectize.options && Object.prototype.hasOwnProperty.call(el.selectize.options, 'ZZBROWSER')) return 'yes';
      var opts = el.options || [];
      for (var i = 0; i < opts.length; i++) if (opts[i].value === 'ZZBROWSER') return 'yes';
      return 'no';
    })()",
    seconds = 20
  )
  app$run_js(
    "(function() {
      var el = document.querySelector('#cfg_budget_project');
      if (el && el.selectize) el.selectize.setValue('ZZBROWSER', false);
      else if (window.jQuery) $('#cfg_budget_project').val('ZZBROWSER').trigger('change');
      if (window.jQuery) $('#cfg_budget_line').val('TRAVEL99').trigger('change');
    })()"
  )
  tracker_wait_js(
    app,
    "(function() {
      var line = document.querySelector('#cfg_budget_line');
      var project = document.querySelector('#cfg_budget_project');
      if (!line || line.value !== 'TRAVEL99' || !project) return 'no';
      var chosen = project.selectize ? project.selectize.getValue() : project.value;
      return chosen === 'ZZBROWSER' ? 'yes' : 'no';
    })()",
    seconds = 15
  )
  app$click(selector = "#cfg_add_budget_line")
  tracker_wait_js(
    app,
    "document.body.innerText.indexOf('Budget line added to ZZBROWSER') >= 0 ? 'yes' : 'no'",
    seconds = 25
  )
  tracker_wait_catalog(app, "TRAVEL99")
  expect_match(
    app$get_js("document.querySelector('#settings_catalog_table') ? document.querySelector('#settings_catalog_table').innerText : ''"),
    "ZZBROWSER",
    fixed = TRUE
  )
})
