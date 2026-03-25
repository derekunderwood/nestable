# nestable.R
# Self-contained R script: nestable expandable portfolio table rendered as HTML.

# =============================================================================
# 1. MOCK DATA
# Flat data.frame representing leaf-level portfolio positions.
# parent = NA marks root-level portfolios.
# market_value and performance are leaf inputs; parents are rolled up later.
# =============================================================================

positions <- data.frame(
  id           = 1:14,
  name         = c(
    "Total Portfolio",
    "Equity",
      "US Equity",
        "Large Cap Growth",
        "Large Cap Value",
      "International Equity",
        "Developed Markets",
        "Emerging Markets",
    "Fixed Income",
      "Investment Grade",
        "US Treasuries",
        "Corporate Bonds",
      "High Yield",
    "Alternatives"
  ),
  parent       = c(
    NA,   # Total Portfolio  (root)
    1,    # Equity           -> Total Portfolio
    2,    # US Equity        -> Equity
    3,    # Large Cap Growth -> US Equity
    3,    # Large Cap Value  -> US Equity
    2,    # Intl Equity      -> Equity
    6,    # Developed Mkts   -> Intl Equity
    6,    # Emerging Mkts    -> Intl Equity
    1,    # Fixed Income     -> Total Portfolio
    9,    # Inv Grade        -> Fixed Income
    10,   # US Treasuries    -> Inv Grade
    10,   # Corporate Bonds  -> Inv Grade
    9,    # High Yield       -> Fixed Income
    1     # Alternatives     -> Total Portfolio
  ),
  # Leaf market values (USD); parent values will be summed from children
  market_value = c(
    NA, NA, NA,
    450000, 380000,          # Large Cap Growth/Value
    NA,
    310000, 180000,          # Developed/Emerging
    NA, NA,
    220000, 195000,          # Treasuries/Corporate
    90000,                   # High Yield
    160000                   # Alternatives
  ),
  # Leaf performance (%); parents computed as weighted average
  performance  = c(
    NA, NA, NA,
     12.4,  8.1,
    NA,
     6.3, -2.7,
    NA, NA,
     3.2,  4.8,
    -1.5,
     9.2
  ),
  stringsAsFactors = FALSE
)


# =============================================================================
# 2. HIERARCHY BUILDER & ROLLUP ENGINE
# Builds a tree from the flat frame and computes rollups bottom-up so that
# no values are hardcoded.
# =============================================================================

# build_tree() converts the flat data.frame into a nested list.
# Each node is a list with fields: id, name, market_value, performance, children.
# Rollups are computed recursively after attaching children.
build_tree <- function(df, parent_id = NA) {
  # Select rows whose parent matches parent_id (handles NA root correctly)
  if (is.na(parent_id)) {
    rows <- df[is.na(df$parent), ]
  } else {
    rows <- df[!is.na(df$parent) & df$parent == parent_id, ]
  }

  lapply(seq_len(nrow(rows)), function(i) {
    row      <- rows[i, ]
    children <- build_tree(df, row$id)   # recurse

    # --- rollup ---
    if (length(children) == 0) {
      # Leaf node: use raw values
      mv   <- row$market_value
      perf <- row$performance
    } else {
      # Parent node: sum children market values
      child_mvs   <- sapply(children, `[[`, "market_value")
      child_perfs <- sapply(children, `[[`, "performance")
      mv          <- sum(child_mvs)
      # Weighted average performance, weighted by each child's market value
      perf <- sum(child_perfs * child_mvs) / mv
    }

    list(
      id           = row$id,
      name         = row$name,
      market_value = mv,
      performance  = perf,
      children     = children
    )
  })
}


# =============================================================================
# 3. HTML RENDERER
# Walks the tree recursively and emits one <tr> per node.
# depth controls px indentation; parent rows get a toggle button.
# =============================================================================

# format helpers
fmt_currency <- function(x) {
  # e.g. $1,234,567.00
  paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
}

fmt_percent <- function(x) {
  # e.g. +12.40% or -1.50%
  sign <- if (x >= 0) "+" else ""
  paste0(sign, formatC(x, format = "f", digits = 2), "%")
}

perf_color <- function(x) if (x >= 0) "#2e7d32" else "#c62828"

# render_rows() returns a character vector of <tr> strings.
# node_id_prefix: unique CSS class prefix used by JS toggle logic.
render_rows <- function(nodes, depth = 0, parent_css_id = NULL) {
  rows <- character(0)

  for (node in nodes) {
    has_children <- length(node$children) > 0
    indent_px    <- depth * 20          # 20 px per level
    css_id       <- paste0("node-", node$id)

    # Child rows are hidden by default (except depth-0 which are always shown).
    # They carry a data-parent attribute so the JS can find them.
    hidden_attr  <- if (!is.null(parent_css_id)) ' style="display:none"' else ""
    parent_attr  <- if (!is.null(parent_css_id))
                      paste0(' data-parent="', parent_css_id, '"') else ""

    # Toggle button for parent rows; spacer for leaves
    toggle <- if (has_children) {
      paste0('<button class="toggle" data-target="', css_id,
             '" onclick="toggleChildren(this)" aria-expanded="false">&#9654;</button>')
    } else {
      '<span class="toggle-spacer"></span>'
    }

    # Name cell with indentation and toggle
    name_cell <- paste0(
      '<td style="padding-left:', indent_px + 8, 'px">',
      toggle,
      if (has_children) paste0('<strong>', node$name, '</strong>') else node$name,
      '</td>'
    )

    mv_cell <- paste0(
      '<td class="num">', fmt_currency(node$market_value), '</td>'
    )

    perf_cell <- paste0(
      '<td class="num" style="color:', perf_color(node$performance), '">',
      fmt_percent(node$performance),
      '</td>'
    )

    row_class <- if (has_children) ' class="parent-row"' else ' class="leaf-row"'

    tr <- paste0(
      '<tr id="', css_id, '"', row_class, parent_attr, hidden_attr, '>',
      name_cell, mv_cell, perf_cell,
      '</tr>'
    )
    rows <- c(rows, tr)

    # Recurse into children, passing this node's css_id so they know their parent
    if (has_children) {
      rows <- c(rows, render_rows(node$children,
                                  depth       = depth + 1,
                                  parent_css_id = css_id))
    }
  }

  rows
}


# =============================================================================
# 4. ASSEMBLE & DISPLAY
# Wraps rendered rows in a full HTML document with embedded CSS and JS.
# =============================================================================

render_portfolio_html <- function(tree) {

  rows_html <- paste(render_rows(tree), collapse = "\n")

  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Portfolio</title>
<style>
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 14px;
    background: #f5f5f5;
    padding: 24px;
    color: #212121;
  }
  h2 { margin-bottom: 12px; color: #37474f; }
  table {
    border-collapse: collapse;
    width: 100%;
    max-width: 680px;
    background: #fff;
    box-shadow: 0 1px 4px rgba(0,0,0,.12);
    border-radius: 6px;
    overflow: hidden;
  }
  thead tr { background: #37474f; color: #fff; }
  th { padding: 10px 14px; text-align: left; font-weight: 600; }
  th.num { text-align: right; }
  td { padding: 8px 14px; border-bottom: 1px solid #eceff1; }
  td.num { text-align: right; font-variant-numeric: tabular-nums; }
  tr:last-child td { border-bottom: none; }
  tr.parent-row > td:first-child { font-weight: 600; }
  tr:hover td { background: #f9fbe7; }
  button.toggle {
    background: none;
    border: none;
    cursor: pointer;
    padding: 0 6px 0 0;
    font-size: 10px;
    color: #546e7a;
    transition: transform .15s;
    vertical-align: middle;
    line-height: 1;
  }
  button.toggle[aria-expanded="true"] {
    transform: rotate(90deg);
  }
  .toggle-spacer { display: inline-block; width: 16px; }
</style>
</head>
<body>
<h2>Portfolio Holdings</h2>
<table>
  <thead>
    <tr>
      <th>Name</th>
      <th class="num">Market Value</th>
      <th class="num">Performance</th>
    </tr>
  </thead>
  <tbody>
', rows_html, '
  </tbody>
</table>

<script>
// Toggle direct children of a parent row.
// Uses data-parent attribute to find immediate children only.
function toggleChildren(btn) {
  var targetId = btn.getAttribute("data-target");
  var expanded = btn.getAttribute("aria-expanded") === "true";

  // Find all rows that are direct children of this node
  var children = document.querySelectorAll("[data-parent=\'" + targetId + "\']");

  children.forEach(function(row) {
    if (expanded) {
      // Collapse: hide this row and recursively collapse its own children
      row.style.display = "none";
      var childBtn = row.querySelector("button.toggle");
      if (childBtn && childBtn.getAttribute("aria-expanded") === "true") {
        toggleChildren(childBtn);   // cascade collapse
      }
    } else {
      // Expand: only show direct children (their children stay hidden)
      row.style.display = "";
    }
  });

  btn.setAttribute("aria-expanded", expanded ? "false" : "true");
}
</script>
</body>
</html>')

  html
}

# --- Run ---
tree     <- build_tree(positions)
html_out <- render_portfolio_html(tree)

# Write to file and open in viewer/browser
out_file <- file.path(tempdir(), "portfolio.html")
writeLines(html_out, out_file)

# Works in RStudio (Viewer pane) and plain R (system browser)
if (interactive()) {
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer(out_file)
  } else {
    browseURL(out_file)
  }
} else {
  message("Output written to: ", out_file)
}
