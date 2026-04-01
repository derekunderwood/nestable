# nestable

Collapsible, expandable HTML tables from hierarchical R data. Works in the RStudio Viewer, R Markdown, Quarto, and Shiny — no JavaScript framework required.

## Installation

```r
# development version
devtools::install_local(".")   # from the repo root
# or
devtools::install_github("you/nestable")
```

## Quick start

```r
library(nestable)

# Group iris rows by species; show means at each group level
iris_data <- iris
iris_data$obs <- paste0("Obs.", seq_len(nrow(iris_data)))

root <- df_to_tree(iris_data,
  name_col   = "obs",
  value_cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  group_col  = "Species",
  total      = "All Species"
)

cols <- list(
  col_def("Sepal.Length", rollup = "mean", format = function(x) formatC(x, digits = 2, format = "f")),
  col_def("Sepal.Width",  rollup = "mean", format = function(x) formatC(x, digits = 2, format = "f")),
  col_def("Petal.Length", rollup = "mean", format = function(x) formatC(x, digits = 2, format = "f")),
  col_def("Petal.Width",  rollup = "mean", format = function(x) formatC(x, digits = 2, format = "f"))
)

nestable(root, cols, name_header = "Observation")
```

## Core concepts

### Building the tree

| Function | Purpose |
|---|---|
| `node(name, ..., .values)` | Construct a single node by hand |
| `rows_to_nodes(df, name_col, value_cols)` | Convert data frame rows to leaf nodes |
| `df_to_tree(df, name_col, value_cols, group_col, total, node_values)` | Build a full nested tree from a flat data frame |

`df_to_tree` is the main entry point for data-frame workflows. `group_col` accepts a character vector of column names, outermost level first:

```r
df_to_tree(df,
  name_col  = "stock",
  value_cols = c("market_cap", "ytd_return"),
  group_col  = c("sector", "subsector"),   # two nesting levels
  total      = "Total Portfolio"           # optional grand-total root row
)
```

### Columns

`col_def()` describes one data column:

```r
col_def(
  key    = "ytd_return",        # matches a name in .values / value_cols
  header = "YTD Return",        # column header (auto-derived from key if omitted)
  format = fmt_percent(),       # display formatter
  color  = function(x) if (x >= 0) "green" else "red",
  rollup = "mean"               # how parent rows are aggregated
)
```

**Built-in formatters**

| Function | Output example |
|---|---|
| `fmt_currency("$", "B", digits = 1)` | `$2,990.0B` |
| `fmt_percent(digits = 2)` | `+17.20%` |
| any `function(x) character` | custom |

**Rollup options**

| Value | Behaviour |
|---|---|
| `"sum"` | Sum of children (default) |
| `"mean"` | Arithmetic mean of children |
| `weighted_rollup("weight_key")` | Weighted average using another column as weights |
| `function(vals, child_values)` | Arbitrary custom aggregation |

### Theming

```r
nestable_theme(
  title       = "My Table",
  header_bg   = "#4527a0",
  table_max_w = "800px",
  indent_px   = 20L
)
```

All visual properties map to CSS custom properties (`--ntbl-*`) scoped to the widget's wrapper `<div>`, so multiple tables with different themes coexist on one page.

### Rendering

```r
nestable(
  data_root,           # list of node() objects from df_to_tree / node()
  columns,             # character vector, named character vector, or list of col_def()
  theme       = nestable_theme(),
  name_header = "Name"   # label for the first (hierarchy) column
)
```

**Column shorthand** — like `kable`'s `col.names`:

```r
# unnamed: header auto-derived from key
nestable(root, c("market_cap", "ytd_return"))

# named: explicit headers, default format and rollup
nestable(root, c("Market Cap" = "market_cap", "YTD Return" = "ytd_return"))
```

### Shiny

```r
library(shiny)

ui     <- fluidPage(nestableOutput("tbl"))
server <- function(input, output) {
  output$tbl <- renderNestable(nestable(root, cols))
}
shinyApp(ui, server)
```

## Full example — Magnificent 7

See `nestable.R` in the repo root for a complete financial-portfolio example with:

- Three nesting levels (sector → subsector → stock)
- Currency and percentage formatting
- Weighted-average return rollup
- Pre-supplied aggregated values that override rollup
