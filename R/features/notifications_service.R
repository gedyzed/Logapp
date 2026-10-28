<<<<<<< HEAD
# Notification behavior is loaded from the manage service module.
=======
# STRUCTURAL PLACEHOLDER - NOT IMPLEMENTED
# File: R/features/notifications_service.R
# Owner: Friend
# Purpose: Own weekly link-only notification orchestration.
# Expected code: Determine completed period, queue active recipients, claim leases, send separately, bounded retry and accepted/failed/uncertain/suppressed outcome handling.
# Connections: Calls periods, notifications_repo and email_client; invoked only by notify_weekly.R worker script.
# Boundary: No report generation, attachment, Shiny session dependency, rate fetch or financial writes. Missing auth -> Not configured, never false Sent.
# Read docs/PRODUCT_SPECIFICATION.md, docs/DATABASE_DESIGN.md and docs/FILE_STRUCTURE.md before implementation.
>>>>>>> 46df1a52aa40976879d44a40e1e7964b5d18a824
