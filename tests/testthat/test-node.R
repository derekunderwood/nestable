test_that("node() returns correct structure", {
  n <- node("root", .values = list(x = 1))
  expect_equal(n$name, "root")
  expect_equal(n$values$x, 1)
  expect_length(n$children, 0)
})

test_that("node() with children stores them", {
  child <- node("child", .values = list(x = 5))
  parent <- node("parent", child)
  expect_length(parent$children, 1)
  expect_equal(parent$children[[1]]$name, "child")
})

# ---------------------------------------------------------------------------
# rows_to_nodes
# ---------------------------------------------------------------------------
test_that("rows_to_nodes converts each row to a leaf node", {
  df <- data.frame(label = c("A", "B"), v = c(10, 20), stringsAsFactors = FALSE)
  nodes <- rows_to_nodes(df, "label", "v")
  expect_length(nodes, 2)
  expect_equal(nodes[[1]]$name, "A")
  expect_equal(nodes[[2]]$values$v, 20)
})

test_that("rows_to_nodes errors on missing name_col", {
  df <- data.frame(v = 1)
  expect_error(rows_to_nodes(df, "label", "v"), "name_col")
})

test_that("rows_to_nodes errors on missing value_cols", {
  df <- data.frame(label = "A")
  expect_error(rows_to_nodes(df, "label", "missing"), "missing")
})

test_that("rows_to_nodes errors when name_col is not a scalar string", {
  df <- data.frame(a = 1, b = 2)
  expect_error(rows_to_nodes(df, c("a", "b"), "a"), "name_col")
})

# ---------------------------------------------------------------------------
# df_to_tree
# ---------------------------------------------------------------------------
test_that("df_to_tree with no group_col produces flat leaf list", {
  df <- data.frame(nm = c("x", "y"), v = c(1, 2), stringsAsFactors = FALSE)
  roots <- df_to_tree(df, "nm", "v")
  expect_length(roots, 2)
  expect_equal(roots[[1]]$name, "x")
})

test_that("df_to_tree group_col creates one parent per group", {
  df <- data.frame(
    nm  = c("a", "b", "c"),
    grp = c("G1", "G1", "G2"),
    v   = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  roots <- df_to_tree(df, "nm", "v", group_col = "grp")
  expect_length(roots, 2)
  expect_equal(roots[[1]]$name, "G1")
  expect_length(roots[[1]]$children, 2)
})

test_that("df_to_tree total wraps everything in a single root", {
  df <- data.frame(nm = c("a", "b"), v = c(1, 2), stringsAsFactors = FALSE)
  roots <- df_to_tree(df, "nm", "v", total = "All")
  expect_length(roots, 1)
  expect_equal(roots[[1]]$name, "All")
})

test_that("df_to_tree errors on bad name_col", {
  df <- data.frame(nm = "a", v = 1)
  expect_error(df_to_tree(df, "nope", "v"), "name_col")
})

test_that("df_to_tree errors on bad value_cols", {
  df <- data.frame(nm = "a", v = 1)
  expect_error(df_to_tree(df, "nm", "nope"), "nope")
})

test_that("df_to_tree errors on bad group_col", {
  df <- data.frame(nm = "a", v = 1)
  expect_error(df_to_tree(df, "nm", "v", group_col = "grp"), "grp")
})

test_that("df_to_tree respects node_values override", {
  df <- data.frame(nm = c("a", "b"), grp = c("G", "G"), v = c(1, 2),
                   stringsAsFactors = FALSE)
  roots <- df_to_tree(df, "nm", "v", group_col = "grp",
                      node_values = list(G = list(v = 99)))
  # rollup would compute sum=3; override should give 99
  cols  <- list(col_def("v", rollup = "sum"))
  tree  <- nestable:::build_tree(roots, cols, prefix = "t")
  expect_equal(tree[[1]]$values$v, 99)
})
