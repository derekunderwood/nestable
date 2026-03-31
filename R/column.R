#' Define a table column
#'
#' @param header Character. Column header text.
#' @param key Character. The name of the field in each node's `.values` list.
#' @param format_fn Function `function(x) -> character` for display formatting.
#'   Defaults to [base::format()].
#' @param color_fn Function `function(x) -> CSS color string`, or `NULL`.
#'   Default `NULL`.
#' @param rollup_fn Function `function(vals, child_values) -> scalar` for
#'   aggregating parent rows. `vals` is a numeric vector of this column's
#'   values for direct children; `child_values` is a list of full value lists
#'   (useful for weighted aggregation). Defaults to `sum`.
#' @return A named list describing the column.
#' @export
col_def <- function(header,
                    key,
                    format_fn  = function(x) format(x),
                    color_fn   = NULL,
                    rollup_fn  = function(vals, child_values) sum(vals, na.rm = TRUE)) {
  list(header    = header,
       key       = key,
       format_fn = format_fn,
       color_fn  = color_fn,
       rollup_fn = rollup_fn)
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
