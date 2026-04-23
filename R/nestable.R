# Internal: accept a character vector as a shorthand column spec.
#   c("market_cap", "ytd_return")          → auto-derived headers
#   c("Market Cap" = "market_cap", ...)    → explicit headers
# A list of col_def() objects is passed through unchanged.
normalise_columns <- function(columns) {
  if (is.character(columns)) {
    nms <- names(columns)
    return(lapply(seq_along(columns), function(i) {
      key <- columns[[i]]
      hdr <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else NULL
      col_def(key, header = hdr)
    }))
  }
  columns
}

# Random UID safe for HTML id attributes (must not start with a digit).
new_widget_uid <- function() {
  paste0("w", paste0(sample(c(letters, 0:9), 7L, replace = TRUE), collapse = ""))
}

nestable_dependency <- function() {
  ver <- tryCatch(utils::packageVersion("nestable"), error = function(e) "0.1.0")
  # system.file() works when installed; fall back to inst/www/ in development
  src <- system.file("www", package = "nestable")
  if (!nzchar(src)) src <- file.path(getwd(), "inst", "www")
  htmlDependency(
    name       = "nestable",
    version    = ver,
    src        = src,
    script     = "nestable.js",
    stylesheet = "nestable.css"
  )
}

# Internal: walk tree and return a list of htmltools tr tags.
render_rows <- function(nodes, cols, depth, parent_id, indent_px, name_col_width) {
  rows <- list()

  for (nd in nodes) {
    has_children <- length(nd$children) > 0
    padding_left <- depth * indent_px + 8L

    toggle <- if (has_children) {
      tags$button(
        class           = "ntbl-toggle",
        `data-target`   = nd$id,
        `aria-expanded` = "false",
        HTML("&#9654;")
      )
    } else {
      tags$span(class = "ntbl-toggle-spacer")
    }

    name_td_style <- paste0("padding-left:", padding_left, "px",
                            if (!is.null(name_col_width))
                              paste0(";width:", name_col_width,
                                     ";white-space:nowrap") else "")
    name_td <- tags$td(
      style = name_td_style,
      toggle,
      if (has_children) tags$strong(nd$name) else nd$name
    )

    value_tds <- lapply(cols, function(col) {
      val     <- nd$values[[col$key]]
      display <- if (is.null(val) || is.na(val)) HTML("&mdash;") else col$format_fn(val)
      parts   <- c(
        if (!is.null(col$width))    paste0("width:",     col$width)    else NULL,
        if (!is.null(col$width))    "white-space:nowrap"               else NULL,
        if (!is.null(col$color_fn) && !is.null(val) && !is.na(val))
          paste0("color:", col$color_fn(val))                          else NULL
      )
      style <- if (length(parts)) paste(parts, collapse = ";") else NULL
      tags$td(class = "ntbl-num", style = style, display)
    })

    row_attrs <- list(
      id    = nd$id,
      class = if (has_children) "ntbl-row ntbl-parent" else "ntbl-row ntbl-leaf"
    )
    if (!is.null(parent_id)) {
      row_attrs[["data-parent"]] <- parent_id
      row_attrs[["style"]]       <- "display:none"
    }

    rows <- c(rows, list(do.call(tags$tr, c(row_attrs, list(name_td), value_tds))))

    if (has_children) {
      rows <- c(rows, render_rows(nd$children, cols,
                                  depth          = depth + 1L,
                                  parent_id      = nd$id,
                                  indent_px      = indent_px,
                                  name_col_width = name_col_width))
    }
  }

  rows
}

#' Create a nestable collapsible HTML table
#'
#' @param data_root A list of top-level [node()] objects. Build with [node()],
#'   [rows_to_nodes()], or [df_to_tree()].
#' @param columns Column specification. Three forms are accepted:
#'   \itemize{
#'     \item A character vector of key names:
#'           `c("market_cap", "ytd_return")` — headers are auto-derived
#'           from the key (e.g. `"market_cap"` → `"Market Cap"`).
#'     \item A *named* character vector:
#'           `c("Market Cap" = "market_cap", "YTD Return" = "ytd_return")` —
#'           explicit headers, default formatting and rollup.
#'     \item A list of [col_def()] objects for full control over formatting,
#'           colours, and rollup behaviour.
#'   }
#' @param name_col Character. The node label key — the `name_col` used when
#'   building the tree with [df_to_tree()], or `"name"` when constructing nodes
#'   manually. Used to auto-derive `name_header` via title-casing when
#'   `name_header` is `NULL`. Default `"name"`.
#' @param name_header Character. Header label for the first (name/label) column.
#'   `NULL` (default) derives the label from `name_col` (e.g. `"security_name"`
#'   → `"Security Name"`).
#' @param name_col_width CSS width string (e.g. `"200px"`, `"30%"`) applied to
#'   the name column header and every name cell. `NULL` (default) leaves the
#'   width unset, allowing the browser to size the column automatically.
#' @param theme A theme list from [nestable_theme()].
#' @param uid Character. Widget UID prefix for HTML element `id` attributes.
#'   Defaults to a random string so multiple tables on one page never clash.
#'   Override only when reproducible IDs are needed (e.g. tests).
#' @return An [htmltools::browsable()] `tagList`. Renders inline in R Markdown,
#'   Quarto, and the RStudio Viewer; use inside [shiny::renderUI()] or
#'   [renderNestable()] in Shiny apps.
#' @importFrom htmltools tags div HTML browsable tagList htmlDependency
#' @export
nestable <- function(data_root,
                     columns,
                     theme          = nestable_theme(),
                     name_col       = "name",
                     name_header    = NULL,
                     name_col_width = NULL,
                     uid            = new_widget_uid()) {

  if (is.null(name_header)) name_header <- pretty_header(name_col)
  columns <- normalise_columns(columns)
  tree <- build_tree(data_root, columns, prefix = paste0(uid, "-"))

  zoom_css <- if (!is.null(theme$zoom) && theme$zoom != "normal")
    sprintf("; zoom: %s", theme$zoom) else ""

  css_vars <- paste0(paste(
    sprintf("--ntbl-font-family: %s",   theme$font_family),
    sprintf("--ntbl-font-size: %s",     theme$font_size),
    sprintf("--ntbl-table-bg: %s",      theme$table_bg),
    sprintf("--ntbl-table-shadow: %s",  theme$table_shadow),
    sprintf("--ntbl-table-radius: %s",  theme$table_radius),
    sprintf("--ntbl-table-max-w: %s",   theme$table_max_w),
    sprintf("--ntbl-header-bg: %s",     theme$header_bg),
    sprintf("--ntbl-header-color: %s",  theme$header_color),
    sprintf("--ntbl-row-border: %s",    theme$row_border),
    sprintf("--ntbl-row-hover-bg: %s",  theme$row_hover_bg),
    sprintf("--ntbl-parent-weight: %s", theme$parent_weight),
    sprintf("--ntbl-toggle-color: %s",  theme$toggle_color),
    sep = "; "
  ), zoom_css)

  name_th_style <- if (!is.null(name_col_width))
    paste0("width:", name_col_width, ";white-space:nowrap") else NULL

  header_cells <- lapply(columns, function(col) {
    hdr_style <- if (!is.null(col$width))
      paste0("width:", col$width, ";white-space:nowrap") else NULL
    tags$th(class = "ntbl-num", style = hdr_style, col$header)
  })

  body_rows <- render_rows(tree, columns,
                           depth          = 0L,
                           parent_id      = NULL,
                           indent_px      = theme$indent_px,
                           name_col_width = name_col_width)

  widget <- div(
    id    = uid,
    class = "ntbl-wrap",
    style = css_vars,
    if (nchar(theme$title) > 0) tags$h2(class = "ntbl-title", theme$title),
    tags$table(
      class = "ntbl",
      tags$thead(tags$tr(tags$th(style = name_th_style, name_header), header_cells)),
      tags$tbody(body_rows)
    )
  )

  browsable(tagList(nestable_dependency(), widget))
}

#' Shiny UI output for a nestable table
#'
#' Use with [renderNestable()] in the server. These are thin wrappers over
#' [shiny::uiOutput()] and [shiny::renderUI()] — no `htmlwidgets` dependency
#' is required.
#'
#' @param outputId The output variable name.
#' @param ... Additional arguments passed to [shiny::uiOutput()].
#' @return A Shiny UI element.
#' @export
nestableOutput <- function(outputId, ...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required. Install with install.packages('shiny').",
         call. = FALSE)
  shiny::uiOutput(outputId, ...)
}

#' Shiny render function for a nestable table
#'
#' @param expr An expression returning a [nestable()] widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Logical. Is `expr` already quoted? Default `FALSE`.
#' @return A Shiny render function.
#' @rdname nestableOutput
#' @export
renderNestable <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required. Install with install.packages('shiny').",
         call. = FALSE)
  if (!quoted) expr <- substitute(expr)
  shiny::renderUI(expr, env = env, quoted = TRUE)
}
