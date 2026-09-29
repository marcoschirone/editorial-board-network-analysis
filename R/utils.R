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

#' Remove floating-point noise before rank-based procedures.
#'
#' Structurally equivalent editors have mathematically identical centrality
#' scores, but eigenvector routines return them with platform-dependent noise
#' in the last digits. Without this step, rank-based statistics (Spearman,
#' Wilcoxon, Kruskal-Wallis, percentile ranks) break those ties arbitrarily and
#' can differ across R versions, BLAS libraries, or operating systems.
tie_stable <- function(x, digits = 10) {
  if (is.numeric(x)) signif(x, digits) else x
}

#' Format a proportion as a percentage, rounding halves upward.
#'
#' scales::percent() inherits binary floating-point representation, so an
#' exact half such as 17/80 = 21.25% can print as 21.2%. Manuscript tables use
#' conventional half-up rounding; figure labels use the same rule.
percent_half_up <- function(p, digits = 1) {
  k <- 10^(digits + 2)
  sprintf(paste0("%.", digits, "f%%"), floor(p * k + 0.5 + 1e-9) / 10^digits)
}

#' Check that tied centrality scores form the same groups across precisions.
#'
#' Supports `tie_stable()`: if the number of distinct values is the same at
#' every precision from 4 to 14 significant digits, rounding at 10 digits
#' separates genuinely different scores and merges only floating-point noise.
#' Precisions beyond 14 digits are excluded because they are platform-dependent.
tie_structure_check <- function(x, digits = c(4, 6, 8, 10, 12, 14)) {
  x <- x[!is.na(x)]
  tibble::tibble(
    significant_digits = digits,
    n_values = length(x),
    n_distinct = vapply(digits, function(d) length(unique(signif(x, d))), integer(1))
  )
}

pct <- function(x) {
  x <- tie_stable(x)
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}

safe_gini <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  ineq::Gini(x)
}

safe_gini_corrected <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n <= 1) return(NA_real_)
  g <- ineq::Gini(x)
  g * n / (n - 1)
}
assert_has_columns <- function(df, cols, label = "data") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(sprintf("%s is missing required columns: %s", label, paste(miss, collapse=", ")), call. = FALSE)
  }
  invisible(TRUE)
}