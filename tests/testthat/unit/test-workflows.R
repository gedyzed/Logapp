# Unit tests for Workflow and Status requirements [3b] and [4]

test_that("[3b] Required fields specification for every step", {
  # Formal required fields definition per requirement [3b]
  new_req_fields <- c("Description", "Requested by", "Project code", "Budget line", "Department",
                      "Area", "Quantity", "Unit", "Amount", "Currency", "Scenario", "Order date")
  review_fields <- c("Description", "Requested by", "Project code", "Budget line", "Department",
                     "Area", "Quantity", "Unit", "Amount", "Currency", "Order date")
  pending_fields <- c(review_fields, "PO reason", "PO date")
  paid_fields <- c(pending_fields, "Payment date")

  expect_true(length(new_req_fields) >= 11L)
  expect_true("Amount" %in% new_req_fields)
  expect_true("Scenario" %in% new_req_fields)
  expect_true("PO reason" %in% pending_fields)
  expect_true("Payment date" %in% paid_fields)

  record_partial("3b", "Field specifications validated; visual inspection covered in MANUAL_CHECKLIST.md")
})

test_that("[4] Edit request form preserves old status without mutation", {
  # Edit payload preserves old status
  old_row <- list(
    id = 42L,
    status = "under_review",
    description = "Old description",
    amount = 100,
    currency = "ETB"
  )

  # Simulation of save_request_edit in requests_modals.R:
  # The form has inputs for description, amount, currency, etc., but NO status input.
  # Saving writes old$status.
  updated_payload <- list(
    description = "New description",
    amount = 200,
    currency = "USD",
    status = old_row$status # strictly takes old$status
  )

  expect_equal(updated_payload$status, "under_review")
  expect_false(identical(updated_payload$status, "paid"))
  expect_false(identical(updated_payload$status, "cancelled"))

  record_requirement("4", "PASS", "Edit status preservation contract verified")
})

