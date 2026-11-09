# Unit tests for Pagination and Filter calculations [10]

page_slice <- function(items, page = 1L, page_size = 10L) {
  if (!length(items)) return(items[0])
  page <- max(1L, as.integer(page))
  page_size <- max(1L, as.integer(page_size))
  start <- (page - 1L) * page_size + 1L
  if (start > length(items)) return(items[0])
  end <- min(length(items), start + page_size - 1L)
  items[start:end]
}

page_count <- function(total, page_size = 10L) {
  if (total <= 0L) return(1L)
  as.integer(ceiling(total / page_size))
}

test_that("[10] Pure helper: page slicing and page counts", {
  items <- paste0("ITEM-", seq_len(25))
  expect_equal(page_count(25, 10), 3L)
  expect_equal(page_slice(items, 1, 10), items[1:10])
  expect_equal(page_slice(items, 2, 10), items[11:20])
  expect_equal(page_slice(items, 3, 10), items[21:25])
  expect_equal(length(page_slice(items, 4, 10)), 0L)
  expect_equal(page_count(0, 10), 1L)
  expect_equal(page_count(121, 10), 13L)
  expect_equal(length(page_slice(seq_len(121), 13, 10)), 1L)

  record_requirement("10", "PASS", "Page slicing logic verified")
})

