# nestable.R
# Self-contained R script: generic nestable expandable HTML table.

# =============================================================================
# 1. DATA — define your hierarchy with node()
#
#   node(name, ..., .values = list())
#     name    — display label for this row
#     ...     — child node()s (makes this a parent/group row)
#     .values — named list of column values.
#               For leaf nodes, supply all column values here.
#               For parent nodes, any value supplied here overrides the rollup
#               for that column; omitted columns are still computed from children.
#
# data_root is a list of top-level nodes (no single wrapper = no grand total).
# Edit data_root and columns to fit your data.
# =============================================================================

node <- function(name, ..., .values = list()) {
  list(name = name, values = as.list(.values), children = list(...))
}

# --- Sample data ------------------------------------------------------------

data_root <- list(

  node("Equity",
    node("US Equity",
      node("Large Cap Growth", .values = list(market_value = 450000, performance =  12.4)),
      node("Large Cap Value",  .values = list(market_value = 380000, performance =   8.1))
    ),
    node("International",
      node("Developed Markets", .values = list(market_value = 310000, performance =   6.3)),
      node("Emerging Markets",  .values = list(market_value = 180000, performance =  -2.7))
    )
  ),

  node("Fixed Income",
    node("Investment Grade",
      node("US Treasuries",   .values = list(market_value = 220000, performance =  3.2)),
      node("Corporate Bonds", .values = list(market_value = 195000, performance =  4.8))
    ),
    node("High Yield", .values = list(market_value =  90000, performance = -1.5))
  ),

  node("Alternatives", .values = list(market_value = 160000, performance = 9.2))

)

# --- Column definitions -----------------------------------------------------
# Each column is a list with:
#   header    — column header string
#   key       — field name in node$values
#   format_fn — function(x) -> character string for display
#   color_fn  — function(x) -> CSS color string, or NULL for no color
#   rollup_fn — function(vals, child_values) -> scalar
#               vals         = numeric vector of this key for direct children
#               child_values = list of full value lists for direct children
#                              (useful for weighted aggregation)

fmt_usd <- function(x) paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))

fmt_pct <- function(x) {
  s <- if (x >= 0) "+" else ""
  paste0(s, formatC(x, format = "f", digits = 2), "%")
}

columns <- list(
  list(
    header    = "Market Value",
    key       = "market_value",
    format_fn = fmt_usd,
    color_fn  = NULL,
    rollup_fn = function(vals, child_values) sum(vals)
  ),
  list(
    header    = "Performance",
    key       = "performance",
    format_fn = fmt_pct,
    color_fn  = function(x) if (x >= 0) "#2e7d32" else "#c62828",
    rollup_fn = function(vals, child_values) {
      weights <- sapply(child_values, function(v) v[["market_value"]])
      total_w <- sum(weights)
      if (total_w == 0) 0 else sum(vals * weights) / total_w
    }
  )
)

# --- Theme ------------------------------------------------------------------
# Override any of these values to restyle the table without touching the logic.

theme <- list(
  title         = "Holdings",
  font_family   = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
  font_size     = "14px",
  page_bg       = "#f5f5f5",
  page_color    = "#212121",
  page_padding  = "24px",
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
  indent_px     = 20
)


# =============================================================================
# 2. ROLLUP ENGINE + ID ASSIGNMENT
# Walks the tree bottom-up, computing each parent's column values via the
# rollup_fn defined in columns. Then stamps each node with a unique id.
# =============================================================================

rollup <- function(node, cols) {
  if (length(node$children) == 0) return(node)

  node$children <- lapply(node$children, rollup, cols = cols)

  child_values <- lapply(node$children, `[[`, "values")

  for (col in cols) {
    existing <- node$values[[col$key]]
    if (!is.null(existing) && !is.na(existing)) next   # explicit value wins

    vals <- sapply(child_values, function(v) {
      x <- v[[col$key]]
      if (is.null(x)) NA_real_ else as.numeric(x)
    })
    node$values[[col$key]] <- col$rollup_fn(vals, child_values)
  }

  node
}

assign_ids <- function(node, env = new.env(parent = emptyenv())) {
  if (is.null(env$counter)) env$counter <- 0L
  env$counter   <- env$counter + 1L
  node$id       <- env$counter
  node$children <- lapply(node$children, assign_ids, env = env)
  node
}

build_tree <- function(roots, cols) {
  roots <- lapply(roots, rollup, cols = cols)
  roots <- lapply(roots, assign_ids)
  roots
}


# =============================================================================
# 3. HTML RENDERER
# Walks the tree recursively, emitting one <tr> per node.
# =============================================================================

render_rows <- function(nodes, cols, depth = 0, parent_css_id = NULL, th) {
  rows <- character(0)

  for (node in nodes) {
    has_children <- length(node$children) > 0
    indent_px    <- depth * th$indent_px
    css_id       <- paste0("node-", node$id)

    hidden_attr <- if (!is.null(parent_css_id)) ' style="display:none"' else ""
    parent_attr <- if (!is.null(parent_css_id))
                     paste0(' data-parent="', parent_css_id, '"') else ""

    toggle <- if (has_children) {
      paste0('<button class="toggle" data-target="', css_id,
             '" onclick="toggleChildren(this)" aria-expanded="false">&#9654;</button>')
    } else {
      '<span class="toggle-spacer"></span>'
    }

    name_td <- paste0(
      '<td style="padding-left:', indent_px + 8, 'px">',
      toggle,
      if (has_children) paste0('<strong>', node$name, '</strong>') else node$name,
      '</td>'
    )

    value_tds <- sapply(cols, function(col) {
      val      <- node$values[[col$key]]
      display  <- if (is.null(val) || is.na(val)) "&mdash;" else col$format_fn(val)
      color    <- if (!is.null(col$color_fn) && !is.null(val) && !is.na(val))
                    paste0(' style="color:', col$color_fn(val), '"') else ""
      paste0('<td class="num"', color, '>', display, '</td>')
    })

    row_class <- if (has_children) "parent-row" else "leaf-row"

    rows <- c(rows, paste0(
      '<tr id="', css_id, '" class="', row_class, '"', parent_attr, hidden_attr, '>',
      name_td, paste(value_tds, collapse = ""), '</tr>'
    ))

    if (has_children) {
      rows <- c(rows, render_rows(node$children, cols,
                                  depth         = depth + 1,
                                  parent_css_id = css_id,
                                  th            = th))
    }
  }

  rows
}


# =============================================================================
# 4. ASSEMBLE & DISPLAY
# =============================================================================

render_html <- function(tree, cols, th) {
  header_ths <- paste(
    sapply(cols, function(col) paste0('<th class="num">', col$header, '</th>')),
    collapse = "\n      "
  )

  rows_html <- paste(render_rows(tree, cols, th = th), collapse = "\n")

  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>', th$title, '</title>
<style>
  body {
    font-family: ', th$font_family, ';
    font-size: ', th$font_size, ';
    background: ', th$page_bg, ';
    padding: ', th$page_padding, ';
    color: ', th$page_color, ';
  }
  h2 { margin-bottom: 12px; color: ', th$header_bg, '; }
  table {
    border-collapse: collapse;
    width: 100%;
    max-width: ', th$table_max_w, ';
    background: ', th$table_bg, ';
    box-shadow: ', th$table_shadow, ';
    border-radius: ', th$table_radius, ';
    overflow: hidden;
  }
  thead tr { background: ', th$header_bg, '; color: ', th$header_color, '; }
  th { padding: 10px 14px; text-align: left; font-weight: 600; }
  th.num { text-align: right; }
  td { padding: 8px 14px; border-bottom: 1px solid ', th$row_border, '; }
  td.num { text-align: right; font-variant-numeric: tabular-nums; }
  tr:last-child td { border-bottom: none; }
  tr.parent-row > td:first-child { font-weight: ', th$parent_weight, '; }
  tr:hover td { background: ', th$row_hover_bg, '; }
  button.toggle {
    background: none;
    border: none;
    cursor: pointer;
    padding: 0 6px 0 0;
    font-size: 10px;
    color: ', th$toggle_color, ';
    transition: transform .15s;
    vertical-align: middle;
    line-height: 1;
  }
  button.toggle[aria-expanded="true"] { transform: rotate(90deg); }
  .toggle-spacer { display: inline-block; width: 16px; }
</style>
</head>
<body>
<h2>', th$title, '</h2>
<table>
  <thead>
    <tr>
      <th>Name</th>
      ', header_ths, '
    </tr>
  </thead>
  <tbody>
', rows_html, '
  </tbody>
</table>

<script>
function hideDescendants(parentId) {
  document.querySelectorAll("[data-parent=\'" + parentId + "\']").forEach(function(row) {
    row.style.display = "none";
    hideDescendants(row.id);
  });
}

function showChildren(parentId) {
  document.querySelectorAll("[data-parent=\'" + parentId + "\']").forEach(function(row) {
    row.style.display = "";
    var btn = row.querySelector("button.toggle");
    if (btn && btn.getAttribute("aria-expanded") === "true") {
      showChildren(row.id);
    }
  });
}

function toggleChildren(btn) {
  var targetId = btn.getAttribute("data-target");
  var expanded = btn.getAttribute("aria-expanded") === "true";
  if (expanded) {
    hideDescendants(targetId);
  } else {
    showChildren(targetId);
  }
  btn.setAttribute("aria-expanded", expanded ? "false" : "true");
}
</script>
</body>
</html>')
}

# --- Run --------------------------------------------------------------------
tree     <- build_tree(data_root, columns)
html_out <- render_html(tree, columns, theme)

out_file <- file.path(tempdir(), "nestable.html")
writeLines(html_out, out_file)

if (interactive()) {
  viewer <- getOption("viewer")
  if (!is.null(viewer)) viewer(out_file) else browseURL(out_file)
} else {
  message("Output written to: ", out_file)
}
