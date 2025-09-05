# R/utils.R
# Utility functions and helpers

`%||%` <- function(a, b) if (is.null(a)) b else a

clean_ids <- function(x) {
  x %>% 
    as.character() %>%
    stringr::str_replace_all("\\p{Cf}", "") %>% 
    stringr::str_squish() %>% 
    stringr::str_trim()
}

edge_pairs <- function(v) {
  v <- v[!is.na(v) & v != ""]
  if (length(v) < 2) return(tibble(e1 = character(), e2 = character()))
  m <- combn(v, 2)
  tibble(e1 = m[1, ], e2 = m[2, ])
}

pct <- function(x) {
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}

safe_gini <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(0)
  ineq::Gini(x)
}

assert_has_columns <- function(df, cols, label = "data") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(sprintf("%s is missing required columns: %s", label, paste(miss, collapse=", ")), call. = FALSE)
  }
  invisible(TRUE)
}