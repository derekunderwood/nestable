#' Define a tree node
#'
#' @param name Display label shown in the Name column.
#' @param ... Child [node()] objects. Supplying children makes this a parent
#'   (group) row whose column values are rolled up from children unless
#'   overridden via `.values`.
#' @param .values Named list of column values. For leaf nodes supply all values
#'   here. For parent nodes any value supplied here overrides the computed
#'   rollup for that column; omitted columns are still computed from children.
#' @return A named list with elements `name`, `values`, and `children`.
#' @export
node <- function(name, ..., .values = list()) {
  list(name = name, values = as.list(.values), children = list(...))
}

#' Convert data frame rows into leaf nodes
#'
#' @param df A data frame.
#' @param name_col Column name to use as the node label.
#' @param value_cols Character vector of column names to carry as `.values`.
#' @return A list of [node()] objects.
#' @export
rows_to_nodes <- function(df, name_col, value_cols) {
  if (!is.character(name_col) || length(name_col) != 1L)
    stop("`name_col` must be a single column name string.", call. = FALSE)
  if (!name_col %in% names(df))
    stop("`name_col` \"", name_col, "\" not found in `df`.", call. = FALSE)
  missing_vals <- setdiff(value_cols, names(df))
  if (length(missing_vals))
    stop("Columns not found in `df`: ",
         paste(missing_vals, collapse = ", "), call. = FALSE)
  lapply(seq_len(nrow(df)), function(i) {
    row    <- df[i, , drop = FALSE]
    values <- as.list(row[, value_cols, drop = FALSE])
    node(as.character(row[[name_col]]), .values = values)
  })
}

#' Convert a flat data frame into a nested node tree
#'
#' @param df A data frame.
#' @param name_col Column name to use as the node label (leaf rows).
#' @param value_cols Character vector of value column names.
#' @param group_col Character vector of grouping columns, outermost first.
#'   Each element adds one nesting level. `NULL` returns a flat list of leaves.
#' @param total Optional string. When non-`NULL` a single root node with this
#'   label wraps the entire tree (grand-total row). `NULL` for no total.
#' @param node_values Optional named list of pre-supplied values for group
#'   (and total) nodes. Each name is a node label; each value is a named list
#'   of column values that should be displayed as-is rather than rolled up from
#'   children. Useful when aggregated figures (e.g. time-weighted returns) are
#'   already known and differ from a simple weighted average of the leaves.
#'
#'   Example — supply a pre-computed return for the "Technology" sector and the
#'   "Mag 7" grand total:
#'   ```r
#'   node_values = list(
#'     "Technology" = list(ytd_return = 2.5),
#'     "Mag 7"      = list(ytd_return = 4.1)
#'   )
#'   ```
#'   Any column *not* listed for a node still falls back to rollup from
#'   children.
#' @return A list of [node()] objects suitable for passing to [nestable()].
#' @export
df_to_tree <- function(df, name_col, value_cols,
                       group_col   = NULL,
                       total       = NULL,
                       node_values = list()) {
  if (!is.character(name_col) || length(name_col) != 1L)
    stop("`name_col` must be a single column name string.", call. = FALSE)
  if (!name_col %in% names(df))
    stop("`name_col` \"", name_col, "\" not found in `df`.", call. = FALSE)
  missing_vals <- setdiff(value_cols, names(df))
  if (length(missing_vals))
    stop("Columns not found in `df`: ",
         paste(missing_vals, collapse = ", "), call. = FALSE)
  if (!is.null(group_col)) {
    missing_grp <- setdiff(group_col, names(df))
    if (length(missing_grp))
      stop("group_col columns not found in `df`: ",
           paste(missing_grp, collapse = ", "), call. = FALSE)
  }
  result <- if (is.null(group_col) || length(group_col) == 0) {
    rows_to_nodes(df, name_col, value_cols)
  } else {
    g      <- group_col[1]
    rest   <- if (length(group_col) > 1) group_col[-1] else NULL
    groups <- unique(df[[g]])
    lapply(groups, function(grp) {
      sub        <- df[df[[g]] == grp, , drop = FALSE]
      grp_key    <- as.character(grp)
      supplied   <- if (!is.null(node_values[[grp_key]])) node_values[[grp_key]] else list()
      children   <- df_to_tree(sub, name_col, value_cols, rest,
                               node_values = node_values)
      do.call(node, c(list(name = grp_key, .values = supplied), children))
    })
  }
  if (!is.null(total)) {
    supplied <- if (!is.null(node_values[[total]])) node_values[[total]] else list()
    list(do.call(node, c(list(name = total, .values = supplied), result)))
  } else {
    result
  }
}
