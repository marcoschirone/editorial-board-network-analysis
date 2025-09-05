# R/quality_checks.R
# Quality control and validation

perform_quality_checks <- function(metrics, networks) {
  message("Performing quality checks...")

  missing_gender <- sum(is.na(metrics$editor_stats$Gender))
  total_editors <- nrow(metrics$editor_stats)

  components_info <- igraph::components(networks$g_full)
  gc_proportion <- max(components_info$csize) / igraph::vcount(networks$g_full)

  isolated_nodes <- sum(igraph::degree(networks$g_full) == 0)

  message(sprintf("Missing gender: %d/%d (%.1f%%)", missing_gender, total_editors, 100 * missing_gender / total_editors))
  message(sprintf("Giant component: %.1f%% of nodes", 100 * gc_proportion))
  message(sprintf("Isolated nodes: %d", isolated_nodes))

  list(
    missing_gender_pct = 100 * missing_gender / total_editors,
    giant_component_pct = 100 * gc_proportion,
    isolated_nodes = isolated_nodes,
    total_editors = total_editors,
    total_edges = igraph::ecount(networks$g_full)
  )
}

print_final_summary <- function(metrics, journal_stats) {
  cat("\n", rep("=", 60), "\n")
  cat("   ANALYSIS SUMMARY\n")
  cat(rep("=", 60), "\n\n")

  cat(sprintf("Network size: %d editors, %d connections\n",
              igraph::vcount(metrics$g_gc), igraph::ecount(metrics$g_gc)))
  cat(sprintf("Communities detected: %d\n", length(unique(V(metrics$g_gc)$community))))
  cat(sprintf("Median EVC: %.4f\n", median(metrics$editor_stats$EVC, na.rm = TRUE)))
  cat(sprintf("Gini inequality: %.3f\n", metrics$inequality_measures$value[1]))
  cat(sprintf("Journals analyzed: %d\n", nrow(journal_stats)))
  cat("\nOutput Location: ./output/\n")
  cat(rep("=", 60), "\n")

  invisible(TRUE)
}
