# Convenience runner for a complete pipeline rebuild.

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

manifest_data <- utils::read.csv(manifest, stringsAsFactors = FALSE)
required_manifest_sections <- c("board_size_sensitivity", "namsor_validation")
missing_manifest_sections <- setdiff(required_manifest_sections, unique(manifest_data$section))
if (length(missing_manifest_sections)) {
  stop(
    "Manifest is missing required section(s): ",
    paste(missing_manifest_sections, collapse = ", "),
    call. = FALSE
  )
}
message("Manifest completeness check: board-size sensitivity and NamSor validation are present.")

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
