test_that("nestable() renders without error for a simple tree", {
  roots <- list(
    node("root",
         node("a", .values = list(v = 1)),
         node("b", .values = list(v = 2)))
  )
  cols <- list(col_def("v", rollup = "sum"))
  result <- nestable(roots, cols, uid = "test01")
  expect_s3_class(result, "shiny.tag.list")
})

test_that("nestable() sum-rolls up parent values", {
  roots <- list(
    node("root",
         node("a", .values = list(v = 3)),
         node("b", .values = list(v = 7)))
  )
  cols  <- list(col_def("v", rollup = "sum"))
  # build_tree is internal but accessible for testing
  tree  <- nestable:::build_tree(roots, cols, prefix = "t")
  expect_equal(tree[[1]]$values$v, 10)
})

test_that("nestable() name_col_width applies width style to header", {
  roots <- list(node("r", node("a", .values = list(v = 1))))
  cols  <- list(col_def("v"))
  result <- nestable(roots, cols, name_col_width = "200px", uid = "test02")
  html <- as.character(result)
  expect_match(html, "width:200px")
})

test_that("nestable() accepts character vector columns shorthand", {
  roots <- list(node("r",
                     node("a", .values = list(x = 1, y = 2)),
                     node("b", .values = list(x = 3, y = 4))))
  result <- nestable(roots, c("x", "y"), uid = "test03")
  expect_s3_class(result, "shiny.tag.list")
})

test_that("nestable() accepts named character vector for headers", {
  roots <- list(node("r", node("a", .values = list(x = 1))))
  result <- nestable(roots, c("X Value" = "x"), uid = "test04")
  html <- as.character(result)
  expect_match(html, "X Value")
})

test_that("nestable() default name_col derives 'Name' header", {
  roots <- list(node("r", node("a", .values = list(v = 1))))
  result <- nestable(roots, c("v"), uid = "test05")
  html <- as.character(result)
  expect_match(html, ">Name<")
})

test_that("nestable() name_col derives title-cased header", {
  roots <- list(node("r", node("a", .values = list(v = 1))))
  result <- nestable(roots, c("v"), name_col = "security_name", uid = "test06")
  html <- as.character(result)
  expect_match(html, "Security Name")
})

test_that("nestable() explicit name_header overrides name_col derivation", {
  roots <- list(node("r", node("a", .values = list(v = 1))))
  result <- nestable(roots, c("v"), name_col = "security_name",
                     name_header = "Asset", uid = "test07")
  html <- as.character(result)
  expect_match(html, "Asset")
  expect_false(grepl("Security Name", html))
})
