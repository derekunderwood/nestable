#' Create a nestable theme
#'
#' Every argument maps to a CSS custom property (`--ntbl-*`) set inline on
#' the widget's wrapper `<div>`, so multiple instances with different themes
#' can coexist on the same page without conflict.
#'
#' @param title Character. Title shown above the table. Default `""` (no title).
#' @param font_family CSS font-family string.
#' @param font_size CSS font-size string. Default `"14px"`.
#' @param table_bg Table background colour. Default `"#ffffff"`.
#' @param table_shadow CSS box-shadow for the table.
#' @param table_radius CSS border-radius. Default `"6px"`.
#' @param table_max_w CSS max-width. Default `"680px"`.
#' @param header_bg Header row background. Default `"#37474f"`.
#' @param header_color Header row text colour. Default `"#ffffff"`.
#' @param row_border Row separator colour. Default `"#eceff1"`.
#' @param row_hover_bg Row hover background. Default `"#f9fbe7"`.
#' @param parent_weight CSS font-weight for parent rows. Default `"600"`.
#' @param toggle_color Colour of the expand/collapse arrow. Default `"#546e7a"`.
#' @param indent_px Integer pixels of indentation per nesting level. Default `20L`.
#' @param zoom CSS zoom level applied to the entire widget. Accepts any valid
#'   CSS `zoom` value: a number (`1.25`), a percentage (`"125%"`), or `"normal"`
#'   (default). Useful for global size/scale adjustments without touching
#'   individual font-size or dimension settings.
#' @return A named list of theme values.
#' @export
nestable_theme <- function(
  title         = "",
  font_family   = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
  font_size     = "14px",
  table_bg      = "#ffffff",
  table_shadow  = "0 1px 4px rgba(0,0,0,.12)",
  table_radius  = "6px",
  table_max_w   = "680px",
  header_bg     = "#37474f",
  header_color  = "#ffffff",
  row_border    = "#eceff1",
  row_hover_bg  = "#f9fbe7",
  parent_weight = "600",
  toggle_color  = "#546e7a",
  indent_px     = 20L,
  zoom          = "normal"
) {
  list(
    title         = title,
    font_family   = font_family,
    font_size     = font_size,
    table_bg      = table_bg,
    table_shadow  = table_shadow,
    table_radius  = table_radius,
    table_max_w   = table_max_w,
    header_bg     = header_bg,
    header_color  = header_color,
    row_border    = row_border,
    row_hover_bg  = row_hover_bg,
    parent_weight = parent_weight,
    toggle_color  = toggle_color,
    indent_px     = as.integer(indent_px),
    zoom          = as.character(zoom)
  )
}
