context("Move")


# Before ------------------------------------------------------------------

test_that("can move columns with .before", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  # First column
  expect_named(move(tbl, a, .before = a), c("a", "b", "c", "d"))
  expect_named(move(tbl, a, .before = b), c("a", "b", "c", "d"))
  expect_named(move(tbl, a, .before = c), c("b", "a", "c", "d"))
  expect_named(move(tbl, a, .before = d), c("b", "c", "a", "d"))

  # Last column
  expect_named(move(tbl, d, .before = a), c("d", "a", "b", "c"))
  expect_named(move(tbl, d, .before = b), c("a", "d", "b", "c"))
  expect_named(move(tbl, d, .before = c), c("a", "b", "d", "c"))
  expect_named(move(tbl, d, .before = d), c("a", "b", "c", "d"))

  # Other column
  expect_named(move(tbl, b, .before = a), c("b", "a", "c", "d"))
  expect_named(move(tbl, b, .before = b), c("a", "b", "c", "d"))
  expect_named(move(tbl, b, .before = c), c("a", "b", "c", "d"))
  expect_named(move(tbl, b, .before = d), c("a", "c", "b", "d"))

  # Many columns
  expect_named(move(tbl, b, c, .before = a), c("b", "c", "a", "d"))
  expect_named(move(tbl, b, a, .before = d), c("c", "b", "a", "d"))
})

test_that(".before selects first of many as reference", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  expect_named(move(tbl, d, .before = a:c), c("d", "a", "b", "c"))
})


# After -------------------------------------------------------------------

test_that("can move columns with .after", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  # First column
  expect_named(move(tbl, a, .after = a), c("a", "b", "c", "d"))
  expect_named(move(tbl, a, .after = b), c("b", "a", "c", "d"))
  expect_named(move(tbl, a, .after = c), c("b", "c", "a", "d"))
  expect_named(move(tbl, a, .after = d), c("b", "c", "d", "a"))

  # Last column
  expect_named(move(tbl, d, .after = a), c("a", "d", "b", "c"))
  expect_named(move(tbl, d, .after = b), c("a", "b", "d", "c"))
  expect_named(move(tbl, d, .after = c), c("a", "b", "c", "d"))
  expect_named(move(tbl, d, .after = d), c("a", "b", "c", "d"))

  # Other column
  expect_named(move(tbl, b, .after = a), c("a", "b", "c", "d"))
  expect_named(move(tbl, b, .after = b), c("a", "b", "c", "d"))
  expect_named(move(tbl, b, .after = c), c("a", "c", "b", "d"))
  expect_named(move(tbl, b, .after = d), c("a", "c", "d", "b"))

  # Many columns
  expect_named(move(tbl, a, b, .after = c), c("c", "a", "b", "d"))
  expect_named(move(tbl, b, a, .after = d), c("c", "d", "b", "a"))
})

test_that(".after selects last of many as reference", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  expect_named(move(tbl, a, .after = b:d), c("b", "c", "d", "a"))
})


# Common ------------------------------------------------------------------

test_that("must give either .before or .after", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  expect_error(move(tbl, c), "`.before` or `.after`")
  expect_error(move(tbl, c, .before = b, .after = a), "`.before` or `.after`")
})

test_that("reference column does not move", {
  tbl <- tibble(a = 1, b = 2, c = 3, d = 4)

  expect_named(move(tbl, a:c, .before = a), c("b", "c", "a", "d"))
  expect_named(move(tbl, a:c, .before = b), c("a", "c", "b", "d"))
  expect_named(move(tbl, a:c, .before = c), c("a", "b", "c", "d"))

  expect_named(move(tbl, a:c, .after = a), c("a", "b", "c", "d"))
  expect_named(move(tbl, a:c, .after = b), c("b", "a", "c", "d"))
  expect_named(move(tbl, a:c, .after = c), c("c", "a", "b", "d"))
})

test_that("can move columns in a database connection", {
  skip_if_not_installed("dbplyr")

  tbl <- dbplyr::memdb_frame(a = 1, b = 2, c = 3, d = 4)

  expect_vars <- function(x, y) {
    expect_equal(as.character(tbl_vars(x)), y)
  }

  expect_vars(move(tbl, b, .before = a), c("b", "a", "c", "d"))
  expect_vars(move(tbl, b, .before = d), c("a", "c", "b", "d"))

  expect_vars(move(tbl, c, .after = d), c("a", "b", "d", "c"))
  expect_vars(move(tbl, c, .after = a), c("a", "c", "b", "d"))
})
