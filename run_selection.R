# Compatibility runner.
# The selection analysis is now part of the authoritative targets pipeline.
# Running this script builds any stale dependencies and regenerates tracked
# selection outputs when needed.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Install it with install.packages('targets').")
}

targets::tar_make()

if (file.exists("output/selection/selection_results.rds")) {
  results <- readRDS("output/selection/selection_results.rds")
  message("Selection results available at output/selection/selection_results.rds")
} else {
  stop("Selection target completed but selection_results.rds was not found.")
}
