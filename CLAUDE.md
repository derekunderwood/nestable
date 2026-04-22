# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Goal

Build a single self-contained R script (`nestable.R`) that renders a nestable, expandable HTML table for hierarchical portfolio data. Output must work in RStudio Viewer or any browser — no Shiny.

## Running the Output

```r
# In R or RStudio:
source("nestable.R")
```

The script should write/open an HTML file or use `htmltools::browsable()` / `browseURL()` to display the result.

## Architecture

The script is organized into four logical sections:

1. **Mock data** — a flat `data.frame` with columns `id`, `name`, `parent` (NA for roots), `market_value`, `performance`.
2. **Hierarchy builder** — a function that converts the flat frame into a tree (list of nodes with children), computing rollups bottom-up:
   - Market Value = sum of children's market values
   - Performance = weighted average by market value
3. **HTML renderer** — a recursive function that walks the tree and emits `<tr>` rows with indentation, toggle buttons, and inline color styling.
4. **Glue** — assembles CSS + JS (expand/collapse via class toggling) with the table rows into a complete HTML document and displays it.

## Key Constraints

- Base R only; `htmltools` is acceptable if it meaningfully simplifies HTML assembly — no other packages.
- Rollup values are computed bottom-up from leaf data by default. Parent or group nodes may supply optional hardcoded values (via `.values` in `node()` or `node_values` in `df_to_tree()`) that override the computed rollup for specific columns; any column not explicitly supplied still falls back to aggregation from children.
- Inline JavaScript must be minimal (a single toggle function is sufficient).
- Deeper nesting (3+ levels) must work without code changes.

## UI Spec

| Column | Format |
|---|---|
| Name | Indented label with expand/collapse toggle on parent rows |
| Market Value | `$1,234,567.00` |
| Performance | `+1.23%` / `-0.45%`, green if ≥ 0, red if < 0 |

A "Total Portfolio" root row that aggregates everything is optional but desirable.

## You are working in a git repository.

Rules:
- Commit early and often
- Each commit must represent one logical change
- Always include a concise, descriptive commit message
- After every code edit, immediately show the git commands used to commit
- Never accumulate multiple changes before committing

If you forget to commit, correct yourself before proceeding.
