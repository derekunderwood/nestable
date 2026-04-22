# Internal: turn an underscore_key into a Title Case label.
pretty_header <- function(key) {
  words <- strsplit(key, "[_. ]+")[[1]]
  paste(paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words))),
        collapse = " ")
}

# Internal: resolve "sum" / "mean" shortcut strings to rollup functions.
resolve_rollup <- function(rollup) {
  if (is.function(rollup)) return(rollup)
  switch(rollup,
    sum  = function(vals, child_values) sum(vals,  na.rm = TRUE),
    mean = function(vals, child_values) mean(vals, na.rm = TRUE),
    stop("Unknown rollup shortcut '", rollup,
         "'. Use \"sum\", \"mean\", or a function.", call. = FALSE)
  )
}

#' Define a table column
#'
#' @param key Character. The value key — the column name in your data frame (or
#'   the name used in each node's `.values` list).
#' @param header Character. Column header text. Defaults to a title-cased
#'   version of `key` (e.g. `"market_cap"` → `"Market Cap"`).
#' @param format Function `function(x) -> character` for display formatting.
#'   Defaults to [base::format()].
#' @param color Function `function(x) -> CSS color string`, or `NULL`.
#'   Default `NULL`.
#' @param rollup How parent rows are aggregated. Either a shortcut string
#'   (`"sum"` or `"mean"`) or a function `function(vals, child_values) ->
#'   scalar`. `vals` is a numeric vector of children's values for this column;
#'   `child_values` is the full list of each child's value lists (useful for
#'   weighted aggregation via [weighted_rollup()]). Defaults to `"sum"`.
#' @param width CSS width string (e.g. `"120px"`, `"10%"`) applied to the
#'   column header and every data cell. `NULL` (default) leaves width unset.
#' @return A named list describing the column.
#' @export
col_def <- function(key,
                    header = NULL,
                    format = function(x) base::format(x),
                    color  = NULL,
                    rollup = "sum",
                    width  = NULL) {
  if (is.null(header)) header <- pretty_header(key)
  list(header    = header,
       key       = key,
       format_fn = format,
       color_fn  = color,
       rollup_fn = resolve_rollup(rollup),
       width     = width)
}

#' Currency format function factory
#'
#' Returns a formatting function for use as `format_fn` in [col_def()].
#'
#' @param prefix Character prepended before the number. Default `"$"`.
#' @param suffix Character appended after the number. Default `""`.
#' @param digits Integer decimal places. Default `2L`.
#' @param big_mark Thousands separator. Default `","`.
#' @return A function `function(x) -> character`.
#' @export
fmt_currency <- function(prefix = "$", suffix = "", digits = 2L, big_mark = ",") {
  function(x) {
    paste0(prefix,
           formatC(x, format = "f", digits = digits, big.mark = big_mark),
           suffix)
  }
}

#' Percentage format function factory
#'
#' Returns a formatting function for use as `format_fn` in [col_def()].
#'
#' @param digits Integer decimal places. Default `2L`.
#' @param plus Logical. Prefix non-negative values with `"+"`. Default `TRUE`.
#' @return A function `function(x) -> character`.
#' @export
fmt_percent <- function(digits = 2L, plus = TRUE) {
  function(x) {
    sign <- if (plus && x >= 0) "+" else ""
    paste0(sign, formatC(x, format = "f", digits = digits), "%")
  }
}

#' Weighted-average rollup function factory
#'
#' Returns a rollup function for use as `rollup_fn` in [col_def()]. Computes
#' the weighted average of `vals` using another key's values as weights.
#'
#' @param weight_key Character. The value key to use as weights (e.g.
#'   `"market_cap"`). Each child's value for this key is used as its weight.
#' @return A function `function(vals, child_values) -> numeric`.
#' @export
weighted_rollup <- function(weight_key) {
  function(vals, child_values) {
    weights <- sapply(child_values, function(v) {
      w <- v[[weight_key]]
      if (is.null(w) || is.na(w)) 0 else as.numeric(w)
    })
    total_w <- sum(weights, na.rm = TRUE)
    if (total_w == 0) NA_real_ else sum(vals * weights, na.rm = TRUE) / total_w
  }
}
