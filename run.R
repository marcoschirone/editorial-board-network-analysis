# Convenience runner for a complete authoritative rebuild.

if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run: Rscript install_packages.R", call. = FALSE)
}

targets::tar_validate()
targets::tar_make()

manifest <- "output/manuscript_results_manifest.csv"
if (!file.exists(manifest)) {
  stop("Pipeline completed without creating the manuscript results manifest.", call. = FALSE)
}

message("Pipeline complete. Authoritative manuscript manifest: ", manifest)

readiness <- "output/selection/affiliation_publication_readiness.csv"
if (file.exists(readiness)) {
  x <- utils::read.csv(readiness, stringsAsFactors = FALSE)
  if (all(c("metric", "value") %in% names(x))) {
    pending <- x$value[x$metric == "pending_source_overrides"]
    if (length(pending) && !is.na(pending[[1]]) && pending[[1]] > 0) {
      message(
        "PUBLICATION HOLD: ", pending[[1]],
        " source-level affiliation override row(s) remain pending review. ",
        "The analytical run is usable for comparison, but do not publish a release until resolved."
      )
    } else {
      message("Affiliation publication-readiness check: no pending source-level overrides.")
    }
  }
}
