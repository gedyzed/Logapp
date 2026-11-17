# Financial Update calculations and writes. The page UI lives in finance_page.R.
# Select paid projects, batch them once, then move that batch from Active to
# Applied (deduction confirmed) or Rejected (returned to the list).
# Amounts are displayed as minor units. A different currency is converted only
# when this month's EuroInfo rates are already in the database.

fin_project_pick <- reactiveVal(character())
fin_history_tab <- reactiveVal("active")
fin_pending_code <- reactiveVal("")
fin_batch_draft <- reactiveVal(NULL)

fin_selected_projects <- function(value) {
  if (is.null(value)) return(character())
  value <- as.character(unlist(value, use.names = FALSE))
  value <- value[!is.na(value) & nzchar(value)]
  unique(value)
}

fin_line_key <- function(project, line) {
  project <- ifelse(is.na(project) | !nzchar(project), "(no project)", as.character(project))
  line <- ifelse(is.na(line) | !nzchar(trimws(as.character(line))), "(unassigned)", trimws(as.character(line)))
  paste(project, line, sep = "||")
}

fin_amount_minor <- function(d) {
  n <- nrow(d)
  if (!n) return(numeric())
  stored <- if ("amount_minor" %in% names(d)) suppressWarnings(as.numeric(d$amount_minor)) else rep(NA_real_, n)
  raw <- suppressWarnings(as.numeric(d$amount))
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (!is.na(stored[[i]]) && stored[[i]] >= 0) {
      out[[i]] <- stored[[i]]
      next
    }
    raw_i <- round(raw[[i]], 2)
    if (is.na(raw_i) || raw_i < 0) next
    if (raw_i == 0) {
      out[[i]] <- 0
    } else {
      out[[i]] <- tryCatch(minor(raw_i), error = function(e) NA_real_)
    }
  }
  out
}

fin_as_minor <- function(major) {
  major <- round(suppressWarnings(as.numeric(major)), 2)
  if (is.na(major) || major <= 0) return(NA_real_)
  tryCatch(minor(major), error = function(e) NA_real_)
}

fin_to_display_minor <- function(amount_minor, currency, display_cur, rates) {
  n <- length(amount_minor)
  out <- rep(NA_real_, n)
  if (!n) return(out)
  display_cur <- currency_value(display_cur)
  currency <- vapply(seq_along(currency), function(i) currency_value(currency[[i]]), character(1))
  to_etb <- function(code, units) {
    if (is.na(units)) return(NA_real_)
    if (identical(code, "ETB")) return(units)
    if (is.null(rates) || !code %in% names(rates)) return(NA_real_)
    fin_as_minor((units / 100) * unname(rates[[code]]))
  }
  for (i in seq_len(n)) {
    units <- amount_minor[[i]]
    cur <- currency[[i]]
    if (is.na(units)) next
    if (identical(cur, display_cur)) {
      out[[i]] <- units
      next
    }
    etb_minor <- to_etb(cur, units)
    if (is.na(etb_minor)) next
    if (identical(display_cur, "ETB")) {
      out[[i]] <- etb_minor
    } else if (!is.null(rates) && display_cur %in% names(rates)) {
      out[[i]] <- fin_as_minor((etb_minor / 100) / unname(rates[[display_cur]]))
    }
  }
  out[is.na(out)] <- 0
  out
}

fin_card_total <- function(minor_units, currency) {
  minor_units <- suppressWarnings(as.numeric(minor_units))
  minor_units[is.na(minor_units)] <- 0
  if (!length(minor_units)) return(paste(currency, abbr(0)))
  paste(currency, abbr(sum(minor_units) / 100))
}

fin_project_code <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "(no project)"
  x
}

fin_split_codes <- function(value) {
  codes <- unlist(strsplit(as.character(value), "|", fixed = TRUE), use.names = FALSE)
  codes <- trimws(codes)
  unique(codes[!is.na(codes) & nzchar(codes)])
}

fin_stamp_label <- function(stamp, empty = "\u2014") {
  stamp <- trimws(as.character(if (is.null(stamp) || !length(stamp)) "" else stamp[[1]]))
  if (!nzchar(stamp) || is.na(stamp)) return(empty)
  if (!grepl("\\d:\\d", stamp)) {
    parsed_date <- suppressWarnings(as.Date(substr(stamp, 1, 10)))
    if (!is.na(parsed_date)) return(format(parsed_date, "%b %d, %Y"))
  }
  parsed <- suppressWarnings(as.POSIXct(stamp))
  if (is.na(parsed)) return(substr(stamp, 1, 16))
  sub(" 0(\\d:)", " \\1", format(parsed, "%b %d, %Y %I:%M %p"))
}

fin_signed_amount <- function(minor_units, currency) {
  minor_units <- suppressWarnings(as.numeric(minor_units))
  if (!length(minor_units) || is.na(minor_units[[1]])) return("\u2014")
  paste0("-", currency, " ", fmt_amount(abs(minor_units[[1]])))
}

fin_signed_major <- function(amount, currency) {
  amount <- suppressWarnings(as.numeric(amount))
  if (!length(amount) || is.na(amount[[1]])) return("\u2014")
  paste0("-", currency, " ", formatC(abs(amount[[1]]), format = "f", digits = 2, big.mark = ","))
}

# Active = paid and batched. Applied = deduction confirmed (legacy "confirmed").
# Rejected = sent back after batching (legacy "dismissed").
fin_status_bucket <- function(status) {
  status <- tolower(trimws(as.character(status)))
  status[is.na(status) | !nzchar(status)] <- ""
  ifelse(status == "active", "active",
         ifelse(status %in% c("applied", "confirmed"), "applied", "rejected"))
}

fin_next_batch_code <- function() {
  rows <- db_get("SELECT batch_code FROM financial_batches")
  nums <- suppressWarnings(as.integer(sub("^BTH-", "", toupper(as.character(rows$batch_code)))))
  nums <- nums[!is.na(nums)]
  n <- if (!length(nums)) 1001L else max(nums) + 1L
  sprintf("BTH-%04d", n)
}

# Paid requests with no applied or active batch membership. Batch status decides
# readiness. Checkpoint time is a label only and is not read here.
fin_ready_ids <- reactive({
  invisible(data())
  if (exists("view_generation", mode = "function")) view_generation()
  rows <- db_get("SELECT r.id
                  FROM requests r
                  WHERE r.deleted_at IS NULL
                    AND r.status = 'paid'
                    AND NOT EXISTS (
                      SELECT 1
                      FROM payment_events pe
                      JOIN batch_items bi ON bi.payment_event_id = pe.id
                      JOIN financial_batches fb ON fb.id = bi.batch_id
                      WHERE pe.request_id = r.id
                        AND fb.status IN ('applied', 'active')
                    )")
  if (!nrow(rows)) return(character())
  as.character(rows$id)
})

fin_base <- reactive({
  d <- data()
  ready <- fin_ready_ids()
  d <- d[d$status == "paid" & as.character(d$id) %in% ready, , drop = FALSE]
  d$amount_minor_value <- fin_amount_minor(d)
  d$display_minor <- fin_to_display_minor(
    d$amount_minor_value, d$currency, active_currency(), fin_effective_rates()
  )
  d$line_key <- fin_line_key(d$project_code, d$budget_line)
  d
})

fin_checkpoint_labels <- reactive({
  invisible(data())
  if (exists("view_generation", mode = "function")) view_generation()
  db_get("SELECT pb.code AS project_code, fb.applied_at AS checkpoint
          FROM financial_batches fb
          JOIN batch_projects bp ON bp.batch_id = fb.id
          JOIN project_budgets pb ON pb.id = bp.project_budget_id
          WHERE fb.status = 'applied'")
})

fin_parse_ids <- function(request_ids) {
  lapply(strsplit(as.character(request_ids), ",", fixed = TRUE), function(ids) {
    ids <- suppressWarnings(as.integer(trimws(ids)))
    ids[!is.na(ids)]
  })
}

fin_load_batches <- function() load_batches()

fin_batches <- reactive({
  invisible(data())
  if (exists("view_generation", mode = "function")) view_generation()
  fin_load_batches()
})

fin_active_batches <- reactive({
  rows <- fin_batches()
  if (!nrow(rows)) return(rows)
  rows[rows$bucket == "active", , drop = FALSE]
})

fin_find_batch <- function(code) {
  code <- trimws(as.character(if (is.null(code) || !length(code)) "" else code[[1]]))
  if (!nzchar(code)) return(NULL)
  rows <- fin_load_batches()
  if (!nrow(rows)) return(NULL)
  hit <- rows[rows$batch_code == code, , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  hit[1, , drop = FALSE]
}

fin_require_active <- function(code) {
  batch <- fin_find_batch(code)
  if (is.null(batch) || !identical(as.character(batch$bucket[[1]]), "active")) {
    showNotification("That batch is no longer active.", type = "warning")
    return(NULL)
  }
  batch
}

fin_reserved_ids <- reactive({
  batches <- fin_active_batches()
  if (!nrow(batches)) return(integer())
  ids <- unique(as.character(unlist(batches$request_id_list, use.names = FALSE)))
  ids[!is.na(ids) & nzchar(ids)]
})

# Paid lines that are not already inside an active batch. A later paid request
# on the same project still appears, so it can be batched on its own.
fin_pool <- reactive({
  d <- fin_base()
  reserved <- fin_reserved_ids()
  if (length(reserved) && nrow(d)) {
    d <- d[!d$id %in% reserved, , drop = FALSE]
  }
  d
})

fin_project_summary <- reactive({
  pend <- fin_pool()
  empty <- data.frame(
    project_code = character(), department = character(), display_minor = numeric(),
    pending_count = integer(), checkpoint_label = character(), stringsAsFactors = FALSE
  )
  if (!nrow(pend)) return(empty)
  labels <- fin_checkpoint_labels()
  codes <- fin_project_code(pend$project_code)
  totals <- split(pend$display_minor, codes)
  rows <- lapply(names(totals), function(code) {
    vals <- totals[[code]]
    vals[is.na(vals)] <- 0
    part <- pend[fin_project_code(pend$project_code) == code, , drop = FALSE]
    department <- if ("department" %in% names(part)) as.character(part$department[[1]]) else ""
    if (!length(department) || is.na(department[[1]])) department <- "" else department <- department[[1]]
    checkpoint <- ""
    if (nrow(labels)) {
      hit <- labels$checkpoint[labels$project_code == code]
      hit <- hit[!is.na(hit)]
      if (length(hit)) {
        ordered <- order(as.character(hit), decreasing = TRUE)
        checkpoint <- hit[[ordered[[1]]]]
      }
    }
    data.frame(
      project_code = code,
      department = department,
      display_minor = sum(vals),
      pending_count = length(vals),
      checkpoint_label = fin_stamp_label(checkpoint, "No checkpoint"),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)
  res[order(-res$display_minor, res$project_code), , drop = FALSE]
})

fin_line_summary <- reactive({
  d <- fin_pool()
  empty <- data.frame(key = character(), project_code = character(), budget_line = character(),
                      amount_label = character(), stringsAsFactors = FALSE)
  if (!nrow(d)) return(empty)
  cur <- active_currency()
  keys <- unique(d$line_key)
  rows <- lapply(keys, function(key) {
    part <- d[d$line_key == key, , drop = FALSE]
    total <- part$display_minor
    total[is.na(total)] <- 0
    line <- part$budget_line[[1]]
    if (is.na(line) || !nzchar(trimws(as.character(line)))) line <- "(unassigned)"
    data.frame(
      key = key,
      project_code = fin_project_code(part$project_code[[1]]),
      budget_line = line,
      amount_label = fin_signed_amount(sum(total), cur),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$project_code, out$budget_line), , drop = FALSE]
})

observeEvent(input$fin_history_tab, {
  tab <- as.character(input$fin_history_tab$tab %||% "")
  if (tab %in% c("active", "applied", "rejected")) fin_history_tab(tab)
}, ignoreInit = TRUE)

observeEvent(input$fin_project_select, {
  fin_project_pick(fin_selected_projects(input$fin_project_select))
}, ignoreNULL = FALSE, ignoreInit = TRUE)

fin_batch_candidates <- function() {
  codes <- fin_selected_projects(input$fin_project_select)
  pend <- fin_pool()
  if (nrow(pend)) pend <- pend[fin_project_code(pend$project_code) %in% codes, , drop = FALSE]
  projects <- if (nrow(pend)) sort(unique(fin_project_code(pend$project_code))) else character()
  list(codes = codes, pend = pend, ready = projects)
}

observeEvent(input$fin_btn_batch, {
  picked <- fin_batch_candidates()
  fin_project_pick(picked$codes)
  if (!length(picked$codes)) {
    showNotification("Select at least one project to batch.", type = "warning")
    return()
  }
  if (!length(picked$ready) || !nrow(picked$pend)) {
    showNotification("Selected projects have no paid lines left to batch.", type = "warning")
    return()
  }
  projects <- picked$ready
  part <- picked$pend
  total <- sum(replace(part$display_minor, is.na(part$display_minor), 0)) / 100
  cur <- active_currency()
  fin_batch_draft(list(projects = projects))
  showModal(modalDialog(
    title = "Name this batch",
    div(class = "nr",
        textInput("fin_batch_name", "Batch name *", value = "", width = "100%",
                  placeholder = "Name used on the batch card and in the export"),
        p(tags$b("Projects: "), paste(projects, collapse = ", ")),
        p(class = "mb-0", tags$b("Total: "), fin_signed_major(total, cur))),
    footer = tagList(
      modal_cancel_button("Cancel"),
      actionButton("fin_btn_batch_create", "Create batch", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

observeEvent(input$fin_btn_batch_create, {
  draft <- fin_batch_draft()
  if (is.null(draft)) {
    removeModal()
    return()
  }
  name <- trimws(v(input$fin_batch_name))
  if (!nzchar(name)) {
    showNotification("Enter a name for the batch.", type = "warning")
    return()
  }
  if (nchar(name) > 80) {
    showNotification("Batch name must be 80 characters or fewer.", type = "warning")
    return()
  }
  part <- fin_pool()
  if (nrow(part)) part <- part[fin_project_code(part$project_code) %in% draft$projects, , drop = FALSE]
  if (!nrow(part)) {
    removeModal()
    fin_batch_draft(NULL)
    showNotification("Selected projects have no paid lines left to batch.", type = "warning")
    return()
  }
  projects <- sort(unique(fin_project_code(part$project_code)))
  ids <- unique(as.character(part$id))
  gbp <- if ("gbp_value" %in% names(part)) suppressWarnings(as.numeric(part$gbp_value)) else rep(NA_real_, length(ids))
  cn <- con()
  on.exit(db_release(cn), add = TRUE)
  saved_code <- tryCatch(
    withProgress(message = "Saving batch...", value = 0.4,
                 create_financial_batch(cn, name, projects, ids, gbp)),
    error = function(e) {
      showNotification(conditionMessage(e), type = "warning")
      NULL
    }
  )
  if (is.null(saved_code)) return()
  if (exists("reload_expenses", mode = "function")) reload_expenses()
  fin_batch_draft(NULL)
  removeModal()
  fin_history_tab("active")
  session$sendCustomMessage("fin-show-history", list())
  showNotification(paste0(name, " is active. Confirm it after the deduction, or reject it to return the projects."), type = "message")
})

observeEvent(input$fin_btn_reject, {
  batch <- fin_require_active(input$fin_btn_reject$code)
  if (is.null(batch)) return()
  code <- batch$batch_code[[1]]
  fin_pending_code(code)
  projects <- fin_split_codes(batch$project_codes[[1]])
  showModal(modalDialog(
    title = tagList(icon("ban"), " Reject batch"),
    p("Reject ", tags$b(code), "? It moves to Rejected and ",
      if (length(projects) == 1) "the project returns" else "the projects return",
      " to the list. Nothing is deducted."),
    if (length(projects)) p(class = "mb-0", tags$b("Projects: "), paste(projects, collapse = ", ")),
    footer = tagList(
      modal_cancel_button("Cancel"),
      actionButton("fin_btn_reject_proceed", "Reject batch", class = "btn-danger text-white")
    ),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)

observeEvent(input$fin_btn_reject_proceed, {
  removeModal()
  code <- fin_pending_code()
  batch <- fin_require_active(code)
  if (is.null(batch)) return()
  cn <- con()
  on.exit(db_release(cn), add = TRUE)
  rejected <- tryCatch(
    withProgress(message = "Saving batch...", value = 0.4, reject_financial_batch(cn, batch$batch_code[[1]])),
    error = function(e) conditionMessage(e)
  )
  if (identical(rejected, "inactive")) {
    showNotification("That batch is no longer active.", type = "warning")
    return()
  }
  if (!isTRUE(rejected)) {
    showNotification("Could not reject the batch. Nothing was changed.", type = "error")
    return()
  }
  if (exists("reload_expenses", mode = "function")) reload_expenses()
  fin_history_tab("rejected")
  showNotification(paste("Batch", batch$batch_code[[1]], "rejected. The projects are back in the list."), type = "message")
}, ignoreInit = TRUE)

observeEvent(input$fin_btn_confirm, {
  batch <- fin_require_active(input$fin_btn_confirm$code)
  if (is.null(batch)) return()
  code <- batch$batch_code[[1]]
  fin_pending_code(code)
  projects <- fin_split_codes(batch$project_codes[[1]])
  ids <- batch$request_id_list[[1]]
  showModal(modalDialog(
    title = tagList(icon("circle-check"), " Confirm applied"),
    div(
      p("Confirm that ", tags$b(code), " has been deducted on the financial sheets. The batch moves to Applied."),
      tags$ul(
        tags$li(tags$b("Projects: "), paste(projects, collapse = ", ")),
        tags$li(tags$b("Requests: "), length(ids)),
        tags$li(tags$b("Total: "), fin_signed_major(batch$total_amount[[1]], batch$currency[[1]]))
      ),
      div(class = "alert alert-warning py-2 mb-0",
          icon("triangle-exclamation"),
          " Paid spending in this batch is cleared and each project's checkpoint is stored.")
    ),
    footer = tagList(
      modal_cancel_button("Cancel"),
      actionButton("fin_btn_confirm_proceed", tagList(icon("circle-check"), " Confirm applied"),
                   class = "btn-primary")
    ),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)

observeEvent(input$fin_btn_confirm_proceed, {
  removeModal()
  batch <- fin_require_active(fin_pending_code())
  if (is.null(batch)) return()
  ids <- batch$request_id_list[[1]]
  if (!length(ids)) {
    showNotification("This batch has no requests to confirm.", type = "warning")
    return()
  }
  code <- batch$batch_code[[1]]
  cn <- con()
  on.exit(db_release(cn), add = TRUE)
  applied <- tryCatch(
    withProgress(message = "Saving batch...", value = 0.4, apply_financial_batch(cn, code)),
    error = function(e) if (identical(conditionMessage(e), "inactive")) "inactive" else "error"
  )
  if (identical(applied, "inactive")) {
    showNotification("That batch is no longer active.", type = "warning")
    return()
  }
  if (!isTRUE(applied)) {
    showNotification("Could not confirm the batch. Nothing was changed.", type = "error")
    return()
  }
  if (exists("reload_expenses", mode = "function")) reload_expenses()
  session$sendCustomMessage("cleanupModal", list())
  fin_history_tab("applied")
  showNotification(paste("Batch", code, "applied. The deduction is recorded."), type = "message")
}, ignoreInit = TRUE)

