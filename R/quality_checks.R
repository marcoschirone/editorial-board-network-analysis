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


#' Validate confident NamSor labels against independently completed labels
#'
#' The validation is restricted to interlocking editors (n_journals >= 2) for
#' whom the completed reference label comes from the legacy manual annotation
#' or an explicit manual adjudication. NamSor-derived fallback labels are never
#' used as the reference, avoiding circular validation.
#'
#' @param gender_metadata Person-level gender metadata from build_gender_metadata().
#' @param output_path Optional CSV path for the aggregate validation summary.
#' @return One-row tibble with coverage, agreement, Cohen's kappa, and directional
#'   disagreement counts. No person identifiers are returned or written.
validate_namsor_interlocking <- function(gender_metadata,
                                         output_path = "output/selection/namsor_validation.csv") {
  required <- c(
    "person_id", "n_journals", "Gender_namsor", "Gender_completed", "Gender_source"
  )
  assert_has_columns(gender_metadata, required, "gender metadata")

  interlocking <- gender_metadata |>
    dplyr::filter(n_journals >= 2)

  reference <- interlocking |>
    dplyr::filter(
      Gender_source %in% c("Legacy annotation", "Manual adjudication"),
      Gender_completed %in% c("Female", "Male")
    )

  validation <- reference |>
    dplyr::filter(Gender_namsor %in% c("Female", "Male"))

  n_total <- nrow(interlocking)
  n_reference <- nrow(reference)
  n_confident <- sum(interlocking$Gender_namsor %in% c("Female", "Male"))
  n_low_conf <- sum(interlocking$Gender_namsor == "Low confidence", na.rm = TRUE)
  n_validation <- nrow(validation)
  n_agree <- sum(validation$Gender_namsor == validation$Gender_completed)

  n_female_to_male <- sum(
    validation$Gender_namsor == "Female" & validation$Gender_completed == "Male"
  )
  n_male_to_female <- sum(
    validation$Gender_namsor == "Male" & validation$Gender_completed == "Female"
  )

  if (n_validation > 0) {
    observed <- n_agree / n_validation
    n_namsor_f <- sum(validation$Gender_namsor == "Female")
    n_namsor_m <- sum(validation$Gender_namsor == "Male")
    n_ref_f <- sum(validation$Gender_completed == "Female")
    n_ref_m <- sum(validation$Gender_completed == "Male")
    expected <- (n_namsor_f * n_ref_f + n_namsor_m * n_ref_m) / n_validation^2
    kappa <- if (isTRUE(all.equal(expected, 1))) NA_real_ else (observed - expected) / (1 - expected)
  } else {
    observed <- NA_real_
    kappa <- NA_real_
  }

  out <- tibble::tibble(
    total_interlocking = n_total,
    reference_labels_available = n_reference,
    namsor_confident = n_confident,
    namsor_low_confidence = n_low_conf,
    validation_n = n_validation,
    agreement_n = n_agree,
    agreement_pct = 100 * observed,
    cohen_kappa = kappa,
    namsor_female_reference_male = n_female_to_male,
    namsor_male_reference_female = n_male_to_female
  )

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(out, output_path)
  }

  message(sprintf(
    "NamSor validation among interlocking editors: %d/%d confident labels agree with independent completed labels (%.1f%%), kappa=%.3f.",
    n_agree, n_validation, 100 * observed, kappa
  ))

  out
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
