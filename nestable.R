# nestable.R — usage example
# Load the package in development, then run this file to render in the Viewer.
#
#   devtools::load_all()   # or library(nestable) after installing
#   source("nestable.R")

devtools::load_all(".")

# ---------------------------------------------------------------------------
# 1. Data — simulates a database result set
# ---------------------------------------------------------------------------

mag7 <- data.frame(
  name       = c("Apple",      "Microsoft", "Nvidia",         "Alphabet",
                 "Meta",        "Amazon",    "Tesla"),
  sector     = c("Technology", "Technology", "Technology",     "Comm. Services",
                 "Comm. Services", "Cons. Discretionary",     "Cons. Discretionary"),
  subsector  = c("Hardware",   "Software",  "Semiconductors", "Search & Ads",
                 "Social Media", "E-Commerce",                "EV & Auto"),
  market_cap = c(3270, 2990, 2640, 1870, 1480, 2180,  790),   # $B
  ytd_return = c(-11.8, -8.3, 22.1,  4.6, 17.2,  2.9, -33.4), # %
  stringsAsFactors = FALSE
)

# Pre-supplied aggregated returns (e.g. from a performance attribution system).
# These override rollup for the named nodes; any unlisted column still rolls up.
supplied_returns <- list(
  "Hardware"              = list(ytd_return = -10.5),
  "Software"              = list(ytd_return =  -7.8),
  "Semiconductors"        = list(ytd_return =  23.0),
  "Search & Ads"          = list(ytd_return =   5.1),
  "Social Media"          = list(ytd_return =  18.0),
  "E-Commerce"            = list(ytd_return =   3.2),
  "EV & Auto"             = list(ytd_return = -32.8),
  "Mag 7"                 = list(ytd_return =   2.9)
)

# group_col applied outermost-first: sector > subsector > stock
# total = "Mag 7" adds an optional grand-total root row; set NULL to remove it
data_root <- df_to_tree(mag7,
  name_col    = "name",
  value_cols  = c("market_cap", "ytd_return"),
  group_col   = c("sector", "subsector"),
  total       = "Mag 7",
  node_values = supplied_returns
)

# ---------------------------------------------------------------------------
# 2. Column definitions
# ---------------------------------------------------------------------------
# Simple form: named character vector (like kable's col.names).
# Headers come from the names; default format and rollup are applied.
#
#   columns <- c("Market Cap" = "market_cap", "YTD Return" = "ytd_return")
#
# Full form: list of col_def() for custom formatting, colours, and rollup.

columns <- list(
  col_def("market_cap",
    header = "Market Cap",
    format = fmt_currency("$", "B", digits = 1L),
    width  = "130px"                               # fixed width; prevents wrapping
  ),
  col_def("ytd_return",
    header = "YTD Return",
    format = fmt_percent(digits = 2L),
    color  = function(x) if (x >= 0) "#2e7d32" else "#c62828",
    rollup = weighted_rollup("market_cap"),
    width  = "110px"
  )
)

# ---------------------------------------------------------------------------
# 3. Theme
# ---------------------------------------------------------------------------

th <- nestable_theme(
  title      = "Magnificent 7",
  header_bg  = "#37474f",
  indent_px  = 20L,
  zoom = 1.25
)

# ---------------------------------------------------------------------------
# 4. Render — displays in RStudio Viewer or browser
# ---------------------------------------------------------------------------

nestable(data_root, columns, th, name_header = "TEST")

# ---------------------------------------------------------------------------
# Example 2: iris — general (non-financial) table
# ---------------------------------------------------------------------------

iris_data <- iris
iris_data$obs <- paste0("Obs.", seq_len(nrow(iris_data)))

# Each row is a leaf (observation); Species is the grouping level.
# Rollup uses "mean" so group rows show the average measurement.
iris_root <- df_to_tree(iris_data,
  name_col   = "obs",
  value_cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  group_col  = "Species",
  total      = "All Species"
)

iris_cols <- list(
  col_def("Sepal.Length", rollup = "mean", width = "115px",
          format = function(x) formatC(x, digits = 2L, format = "f")),
  col_def("Sepal.Width",  rollup = "mean", width = "115px",
          format = function(x) formatC(x, digits = 2L, format = "f")),
  col_def("Petal.Length", rollup = "mean", width = "115px",
          format = function(x) formatC(x, digits = 2L, format = "f")),
  col_def("Petal.Width",  rollup = "mean", width = "115px",
          format = function(x) formatC(x, digits = 2L, format = "f"))
)

iris_theme <- nestable_theme(
  title       = "iris: measurements by species",
  table_max_w = "800px",
  header_bg   = "#4527a0",
  zoom        = 0.9          # scale the whole table down; useful for wide layouts
)

nestable(iris_root, iris_cols, iris_theme, name_header = "Observation")

# ---------------------------------------------------------------------------
# Shiny usage:
#
#   library(shiny)
#   ui     <- fluidPage(nestableOutput("tbl"))
#   server <- function(input, output) {
#     output$tbl <- renderNestable(nestable(data_root, columns, th))
#   }
#   shinyApp(ui, server)
# ---------------------------------------------------------------------------
