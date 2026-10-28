<<<<<<< HEAD
# Financial Update calculations and writes. The page UI lives in finance_page.R.
# Select paid projects, batch them once, then move that batch from Active to
# Applied (deduction confirmed) or Rejected (returned to the list).
# Amounts are displayed as minor units. A different currency is converted only
# when this month's EuroInfo rates are already in the database.

fin_project_pick <- reactiveVal(character())
fin_history_tab <- reactiveVal("active")
fin_pending_code <- reactiveVal("")

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

fin_base <- reactive({
  d <- data()
  undeducted <- is.na(d$deducted_at) | !nzchar(trimws(as.character(d$deducted_at)))
  d <- d[d$status == "paid" & undeducted, , drop = FALSE]
  d$amount_minor_value <- fin_amount_minor(d)
  d$display_minor <- fin_to_display_minor(
    d$amount_minor_value, d$currency, active_currency(), euroinfo_month_rates()
  )
  d$line_key <- fin_line_key(d$project_code, d$budget_line)
  d
})

fin_projects <- reactive({
  base <- project_catalog_view()
  base$checkpoint <- base$last_deducted_at
  fields <- tryCatch({
    cn <- con()
    on.exit(dbDisconnect(cn), add = TRUE)
    dbListFields(cn, "projects")
  }, error = function(e) character())
  if ("last_checkpoint" %in% fields) {
    extra <- db_get("SELECT project_code, COALESCE(last_checkpoint, '') AS last_checkpoint FROM projects")
    matched <- extra$last_checkpoint[match(base$project_code, extra$project_code)]
    use_checkpoint <- !is.na(matched) & nzchar(matched)
    base$checkpoint[use_checkpoint] <- matched[use_checkpoint]
  }
  base
})

fin_parse_ids <- function(request_ids) {
  lapply(strsplit(as.character(request_ids), ",", fixed = TRUE), function(ids) {
    ids <- suppressWarnings(as.integer(trimws(ids)))
    ids[!is.na(ids)]
  })
}

fin_load_batches <- function() {
  rows <- db_get("SELECT id, batch_code, project_codes, request_ids, total_amount, currency, status, created, confirmed_at
                  FROM financial_batches ORDER BY id DESC")
  rows$request_id_list <- if (nrow(rows)) fin_parse_ids(rows$request_ids) else vector("list", 0)
  rows$bucket <- if (nrow(rows)) fin_status_bucket(rows$status) else character()
  rows
}

fin_batches <- reactive({
  invisible(data())
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
  ids <- unique(unlist(batches$request_id_list, use.names = FALSE))
  ids <- suppressWarnings(as.integer(ids))
  ids[!is.na(ids)]
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
  projs <- fin_projects()
  proj_codes <- fin_project_code(projs$project_code)
  codes <- fin_project_code(pend$project_code)
  totals <- split(pend$display_minor, codes)
  rows <- lapply(names(totals), function(code) {
    vals <- totals[[code]]
    vals[is.na(vals)] <- 0
    idx <- match(code, proj_codes)
    department <- ""
    checkpoint <- ""
    if (!is.na(idx)) {
      if ("department" %in% names(projs)) {
        department <- as.character(projs$department[[idx]])
        if (!length(department) || is.na(department[[1]])) department <- "" else department <- department[[1]]
      }
      checkpoint <- projs$checkpoint[[idx]]
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
  ids <- sort(unique(as.integer(part$id)))
  total <- sum(replace(part$display_minor, is.na(part$display_minor), 0)) / 100
  cur <- active_currency()
  stamp <- now()
  saved_code <- NULL
  cn <- con()
  on.exit(dbDisconnect(cn), add = TRUE)
  for (attempt in 1:2) {
    batch_code <- fin_next_batch_code()
    saved <- tryCatch({
      dbExecute(cn, "INSERT INTO financial_batches(batch_code, project_codes, request_ids, total_amount, currency, status, created, confirmed_at) VALUES(?,?,?,?,?,?,?,?)",
                params = list(batch_code, paste(projects, collapse = "|"), paste(ids, collapse = ","),
                              total, cur, "active", stamp, NA))
      TRUE
    }, error = function(e) FALSE)
    if (saved) {
      saved_code <- batch_code
      break
    }
  }
  if (is.null(saved_code)) {
    showNotification("Could not create the batch. Try again.", type = "warning")
    return()
  }
  fin_history_tab("active")
  session$sendCustomMessage("fin-show-history", list())
  showNotification(paste0("Batch ", saved_code, " is active. Confirm it after the deduction, or reject it to return the projects."), type = "message")
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
      modalButton("Cancel"),
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
  on.exit(dbDisconnect(cn), add = TRUE)
  n <- dbExecute(cn, "UPDATE financial_batches SET status = 'rejected' WHERE batch_code = ? AND status = 'active'",
                 params = list(batch$batch_code[[1]]))
  if (!isTRUE(n > 0)) {
    showNotification("That batch is no longer active.", type = "warning")
    return()
  }
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
      modalButton("Cancel"),
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
  projects <- fin_split_codes(batch$project_codes[[1]])
  projects <- projects[projects != "(no project)"]
  stamp <- now()
  code <- batch$batch_code[[1]]
  cn <- con()
  on.exit(dbDisconnect(cn), add = TRUE)
  has_checkpoint <- "last_checkpoint" %in% dbListFields(cn, "projects")
  applied <- tryCatch({
    dbWithTransaction(cn, {
      n <- dbExecute(cn, "UPDATE financial_batches SET status = 'applied', confirmed_at = ? WHERE batch_code = ? AND status = 'active'",
                     params = list(stamp, code))
      if (!isTRUE(n > 0)) stop("inactive")
      id_placeholders <- paste(rep("?", length(ids)), collapse = ",")
      dbExecute(cn, paste0("UPDATE expenses SET deducted_at = ? WHERE id IN (", id_placeholders, ")"),
                params = c(list(stamp), as.list(as.integer(ids))))
      if (length(projects)) {
        proj_placeholders <- paste(rep("?", length(projects)), collapse = ",")
        dbExecute(cn, paste0("UPDATE projects SET last_deducted_at = ? WHERE project_code IN (", proj_placeholders, ")"),
                  params = c(list(stamp), as.list(projects)))
        if (has_checkpoint) {
          dbExecute(cn, paste0("UPDATE projects SET last_checkpoint = ? WHERE project_code IN (", proj_placeholders, ")"),
                    params = c(list(stamp), as.list(projects)))
        }
      }
    })
    TRUE
  }, error = function(e) if (identical(conditionMessage(e), "inactive")) "inactive" else "error")
  if (identical(applied, "inactive")) {
    showNotification("That batch is no longer active.", type = "warning")
    return()
  }
  if (!isTRUE(applied)) {
    showNotification("Could not confirm the batch. Nothing was changed.", type = "error")
    return()
  }
  fin_history_tab("applied")
  showNotification(paste("Batch", code, "applied. The deduction is recorded."), type = "message")
}, ignoreInit = TRUE)
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/finance_service.R
# Owner: Friend
# Purpose: Own budget-level checkpoint workflow and finance view models.
# Expected code: Budget/line summaries, eligibility, server-held preview tokens, prepare/apply/abandon orchestration and exact batch/history retrieval.
# Connections: Calls finance_repo and contracts; receives clock/settings; returns frozen export view model.
# Boundary: No request transitions or external workbook writes. Preview changed -> reconfirm. One draft per budget. Do not release applied payment membership.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
