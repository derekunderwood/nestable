test_that("col_def defaults are correct", {
  cd <- col_def("market_cap")
  expect_equal(cd$key, "market_cap")
  expect_equal(cd$header, "Market Cap")
  expect_null(cd$width)
  expect_null(cd$color_fn)
  expect_equal(cd$format_fn(1234), format(1234))
})

test_that("col_def respects explicit header", {
  cd <- col_def("v", header = "Value")
  expect_equal(cd$header, "Value")
})

test_that("col_def width is stored", {
  cd <- col_def("v", width = "150px")
  expect_equal(cd$width, "150px")
})

test_that("col_def rollup sum works", {
  cd <- col_def("v", rollup = "sum")
  expect_equal(cd$rollup_fn(c(1, 2, 3), list()), 6)
})

test_that("col_def rollup mean works", {
  cd <- col_def("v", rollup = "mean")
  expect_equal(cd$rollup_fn(c(2, 4, 6), list()), 4)
})

test_that("col_def rejects unknown rollup string", {
  expect_error(col_def("v", rollup = "median"), "Unknown rollup")
})

test_that("fmt_currency formats correctly", {
  fn <- fmt_currency("$", "B", digits = 1L)
  expect_equal(fn(1.5), "$1.5B")
})

test_that("fmt_percent prefixes positive with +", {
  fn <- fmt_percent(digits = 2L)
  expect_match(fn(3.14), "^\\+3\\.14%$")
})

test_that("fmt_percent prefixes negative with -", {
  fn <- fmt_percent(digits = 2L)
  expect_match(fn(-1.5), "^-1\\.50%$")
})

test_that("weighted_rollup computes weighted average", {
  fn <- weighted_rollup("w")
  child_values <- list(list(v = 10, w = 2), list(v = 20, w = 3))
  vals <- c(10, 20)
  result <- fn(vals, child_values)
  expect_equal(result, (10 * 2 + 20 * 3) / 5)
})

test_that("weighted_rollup returns NA when total weight is zero", {
  fn <- weighted_rollup("w")
  child_values <- list(list(v = 1, w = 0), list(v = 2, w = 0))
  expect_true(is.na(fn(c(1, 2), child_values)))
})
