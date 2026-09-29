# R/quality_checks.R
# Quality control and validation

perform_quality_checks <- function(metrics, networks) {
  message("Performing quality checks...")

  gender_col <- if ("Gender_namsor" %in% names(metrics$editor_stats)) "Gender_namsor" else "Gender"
  missing_gender <- sum(is.na(metrics$editor_stats[[gender_col]]) |
                        !metrics$editor_stats[[gender_col]] %in% c("Female", "Male"))
  total_editors <- nrow(metrics$editor_stats)

  components_info <- igraph::components(networks$g_full)
  gc_proportion <- max(components_info$csize) / igraph::vcount(networks$g_full)

  isolated_nodes <- sum(igraph::degree(networks$g_full) == 0)

  message(sprintf("Low-confidence/missing primary gender: %d/%d (%.1f%%)", missing_gender, total_editors, 100 * missing_gender / total_editors))
  message(sprintf("Giant component: %.1f%% of nodes", 100 * gc_proportion))
  message(sprintf("Isolated nodes: %d", isolated_nodes))

  list(
    low_confidence_gender_pct = 100 * missing_gender / total_editors,
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

#' Assert that every manuscript-facing Leiden result refers to one partition.
validate_leiden_consistency <- function(leiden_rec, metrics, robustness = NULL, tolerance = 1e-10) {
  primary <- metrics$community_summary
  if (is.null(primary) || nrow(primary) != 1) {
    stop("Missing primary Leiden community summary.", call. = FALSE)
  }

  if (primary$n_communities[[1]] != leiden_rec$recommendation$num_communities) {
    stop("Leiden inconsistency: stored sweep partition and final metrics have different community counts.", call. = FALSE)
  }
  if (abs(primary$modularity[[1]] - leiden_rec$recommendation$modularity) > tolerance) {
    stop("Leiden inconsistency: stored sweep partition and final metrics have different modularity.", call. = FALSE)
  }

  if (!is.null(robustness) && !is.null(robustness$resolution_sweep)) {
    rr <- robustness$resolution_sweep
    hit <- rr[abs(rr$resolution - primary$resolution[[1]]) < tolerance, , drop = FALSE]
    if (nrow(hit) == 1) {
      if (hit$n_communities[[1]] != primary$n_communities[[1]] ||
          abs(hit$modularity[[1]] - primary$modularity[[1]]) > tolerance) {
        stop("Leiden inconsistency: robustness sweep does not reproduce the selected partition at the selected resolution.", call. = FALSE)
      }
    }
  }

  message(sprintf(
    "Leiden consistency check passed: resolution %.2f, Q=%.6f, %d communities.",
    primary$resolution[[1]], primary$modularity[[1]], primary$n_communities[[1]]
  ))
  invisible(TRUE)
}
