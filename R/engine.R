# Internal rollup and ID-assignment engine — not exported.

rollup <- function(node, cols) {
  if (length(node$children) == 0) return(node)
  node$children <- lapply(node$children, rollup, cols = cols)
  child_values  <- lapply(node$children, `[[`, "values")
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

assign_ids <- function(node, prefix, env) {
  env$counter  <- env$counter + 1L
  node$id      <- paste0(prefix, env$counter)
  node$children <- lapply(node$children, assign_ids, prefix = prefix, env = env)
  node
}

build_tree <- function(roots, cols, prefix) {
  roots  <- lapply(roots, rollup, cols = cols)
  id_env <- new.env(parent = emptyenv())
  id_env$counter <- 0L
  lapply(roots, assign_ids, prefix = prefix, env = id_env)
}
