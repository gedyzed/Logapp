# Agent instructions

Read `docs/PRODUCT_SPECIFICATION.md`, `docs/DATABASE_DESIGN.md` and `docs/FILE_STRUCTURE.md` before editing.
Stay inside the file you were asked to change. Do not add a CBE client. Monthly EuroInfo rates are read from the database once that table exists. Do not commit secrets or the SQLite database.

Test Suite:
- Unit tests: `tests/testthat/unit/`
- Integration tests: `tests/testthat/integration/`
- Runner: `Rscript run_tests.R` (or `Rscript scripts/test.R`)
- Manual release verification: `MANUAL_CHECKLIST.md`

After a finished change, run the test suite and commit it without waiting to be asked. Do not push unless asked.
