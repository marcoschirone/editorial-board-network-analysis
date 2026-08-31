# R/data_export.R
# Functions for exporting results to CSV and RDS files.

export_results <- function(final_results, output_dir) {
  message("Exporting results...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (!is.null(final_results$metrics$editor_stats)) {
    readr::write_csv(final_results$metrics$editor_stats, file.path(output_dir, "editor_metrics.csv"))
  }
  if (!is.null(final_results$journal_metrics$journal_stats)) {
    readr::write_csv(final_results$journal_metrics$journal_stats, file.path(output_dir, "journal_metrics.csv"))
  }
  if (!is.null(final_results$metrics$inequality_measures)) {
    readr::write_csv(final_results$metrics$inequality_measures, file.path(output_dir, "inequality_measures.csv"))
  }
  if (!is.null(final_results$disparity_results$gender)) {
    readr::write_csv(final_results$disparity_results$gender, file.path(output_dir, "gender_disparities.csv"))
  }
  
  # Export geographic disparities at all levels
  if (!is.null(final_results$disparity_results$geographic)) {
    # Continent level (original)
    readr::write_csv(final_results$disparity_results$geographic, 
                     file.path(output_dir, "geographic_disparities_continent.csv"))
  }
  if (!is.null(final_results$disparity_results$geographic_subregion)) {
    readr::write_csv(final_results$disparity_results$geographic_subregion, 
                     file.path(output_dir, "geographic_disparities_subregion.csv"))
  }
  if (!is.null(final_results$disparity_results$geographic_country)) {
    readr::write_csv(final_results$disparity_results$geographic_country, 
                     file.path(output_dir, "geographic_disparities_country.csv"))
  }
  
  if (!is.null(final_results$leiden_sweep)) {
    readr::write_csv(final_results$leiden_sweep$sweep_results, file.path(output_dir, "leiden_sweep_results.csv"))
  }
  
  saveRDS(final_results, file.path(output_dir, "full_analysis_results.rds"))
  
  invisible(TRUE)
}

create_publication_tables <- function(final_results, output_dir) {
  message("Creating publication tables...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  wb <- openxlsx::createWorkbook()
  sheets_added <- 0
  
  add_sheet <- function(name, data) {
    if (!is.null(data) && nrow(data) > 0) {
      openxlsx::addWorksheet(wb, name)
      openxlsx::writeData(wb, name, data)
      sheets_added <<- sheets_added + 1
    }
  }
  
  add_sheet("Editor_Metrics", final_results$metrics$editor_stats)
  add_sheet("Journal_Metrics", final_results$journal_metrics$journal_stats)
  add_sheet("Journal_Typology", final_results$journal_typology)
  add_sheet("Disparity_Gender", final_results$disparity_results$gender)
  add_sheet("Disparity_Geography", final_results$disparity_results$geographic)
  add_sheet("Board_Analysis", final_results$board_analysis)
  if (!is.null(final_results$selection$gender_selection)) {
    add_sheet("Gender_Selection_Primary", final_results$selection$gender_selection$primary)
    add_sheet("Gender_LowConf_Missing", final_results$selection$gender_selection$missingness)
    add_sheet("Gender_Mixed_Sensitivity", final_results$selection$gender_selection$mixed_sensitivity)
  }
  
  if (sheets_added > 0) {
    openxlsx::saveWorkbook(wb, file.path(output_dir, "publication_summary_tables.xlsx"), overwrite = TRUE)
    message(sprintf("Publication tables saved with %d sheets.", sheets_added))
  }
  
  invisible(TRUE)
}
#' Write one machine-generated source of truth for manuscript-facing results.
#'
#' The manifest is intentionally long-form: each row is one statistic with an
#' explicit analysis role. Manuscript numbers should be copied from this file,
#' not from console output or older prose.
create_manuscript_results_manifest <- function(population_data,
                                               data_clean,
                                               networks,
                                               metrics,
                                               journal_metrics,
                                               journal_typology,
                                               leiden_rec,
                                               robustness,
                                               selection,
                                               output_path = "output/manuscript_results_manifest.csv") {
  rows <- list()
  add <- function(section, metric, value, unit = "", role = "descriptive", note = "") {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      section = section,
      metric = metric,
      value = as.character(value),
      unit = unit,
      analysis_role = role,
      note = note
    )
  }

  person <- population_data$person
  positions <- population_data$positions

  add("population", "appointments", nrow(positions), "appointments")
  add("population", "unique_persons", nrow(person), "persons")
  add("population", "interlocking_editors", sum(person$interlocking), "persons")
  add("population", "editors_3plus_journals", sum(person$n_journals >= 3), "persons")
  add("population", "interlocking_appointments", nrow(data_clean), "appointments")
  add("population", "interlocking_journals_represented", dplyr::n_distinct(data_clean$Journal), "journals")

  add("editor_network", "full_nodes", igraph::vcount(networks$g_full), "nodes")
  add("editor_network", "full_edges", igraph::ecount(networks$g_full), "edges")
  add("editor_network", "giant_component_nodes", igraph::vcount(metrics$g_gc), "nodes")
  add("editor_network", "giant_component_edges", igraph::ecount(metrics$g_gc), "edges")
  add("editor_network", "median_evc", median(metrics$editor_stats$EVC, na.rm = TRUE), "EVC")
  add("editor_network", "gini_evc", metrics$inequality_measures$value[[1]], "Gini")

  if (!is.null(journal_typology) && nrow(journal_typology) > 0) {
    add("journal_typology", "eligible_journals",
        journal_typology$eligible_journal_count[[1]], "journals", "descriptive")
    add("journal_typology", "median_evc_threshold",
        journal_typology$evc_threshold_used[[1]], "EVC", "descriptive")
    add("journal_typology", "gini_threshold",
        journal_typology$gini_threshold_used[[1]], "Gini", "descriptive",
        paste0("Primary typology uses ", journal_typology$gini_measure_used[[1]], " Gini"))
    counts <- journal_typology %>%
      dplyr::count(typology, name = "n_journals")
    for (i in seq_len(nrow(counts))) {
      key <- gsub("[^a-z0-9]+", "_", tolower(counts$typology[[i]]))
      key <- gsub("^_|_$", "", key)
      add("journal_typology", paste0("n_", key),
          counts$n_journals[[i]], "journals", "descriptive")
    }
  }

  cs <- metrics$community_summary
  add("leiden_editor", "selected_resolution", cs$resolution[[1]], "resolution", "primary")
  add("leiden_editor", "modularity", cs$modularity[[1]], "Q", "primary")
  add("leiden_editor", "communities", cs$n_communities[[1]], "communities", "primary",
      "Exact partition selected in sweep and reused by final metrics")
  add("leiden_editor", "seed", cs$seed[[1]], "integer", "reproducibility")
  add("leiden_editor", "objective_function", cs$objective_function[[1]], "", "reproducibility")


  if (!is.null(journal_metrics$community_summary) &&
      nrow(journal_metrics$community_summary) > 0) {
    jcs <- journal_metrics$community_summary[1, ]
    add("leiden_journal", "resolution", jcs$resolution[[1]], "resolution", "descriptive",
        "Journal-journal network analysed separately from editor network")
    add("leiden_journal", "modularity", jcs$modularity[[1]], "Q", "descriptive",
        "Weighted Newman-Girvan modularity of fixed-resolution CPM partition")
    add("leiden_journal", "communities", jcs$n_communities[[1]], "communities", "descriptive")
    add("leiden_journal", "largest_community_size",
        jcs$largest_community_size[[1]], "journals", "descriptive")
    add("leiden_journal", "seed", jcs$seed[[1]], "integer", "reproducibility")
    add("leiden_journal", "objective_function",
        jcs$objective_function[[1]], "", "reproducibility")
  }

  if (!is.null(robustness$bootstrap_confidence)) {
    for (i in seq_len(nrow(robustness$bootstrap_confidence))) {
      x <- robustness$bootstrap_confidence[i, ]
      add("robustness", paste0(x$metric, "_estimate"), x$estimate)
      add("robustness", paste0(x$metric, "_ci_lower"), x$ci_lower, role = "95% CI")
      add("robustness", paste0(x$metric, "_ci_upper"), x$ci_upper, role = "95% CI")
    }
  }

  if (!is.null(robustness$centrality_correlations)) {
    for (i in seq_len(nrow(robustness$centrality_correlations))) {
      x <- robustness$centrality_correlations[i, ]
      key <- paste0(tolower(x$metric1), "_vs_", tolower(x$metric2))
      add("centrality", paste0(key, "_spearman_rho"), x$correlation, "rho", "robustness")
      add("centrality", paste0(key, "_p_value"), x$p_value, "p", "robustness")
    }
  }

  if (!is.null(robustness$component_rank_correlations)) {
    for (i in seq_len(nrow(robustness$component_rank_correlations))) {
      x <- robustness$component_rank_correlations[i, ]
      key <- paste0("full_vs_gc_", tolower(x$metric))
      add("component_sensitivity", paste0(key, "_rho"), x$spearman_rho, "rho", "robustness")
      add("component_sensitivity", paste0(key, "_n"), x$n_shared, "persons", "robustness")
      add("component_sensitivity", paste0(key, "_p_value"), x$p_value, "p", "robustness")
    }
  }

  if (!is.null(selection$omnibus_continent$summary)) {
    x <- selection$omnibus_continent$summary[1, ]
    add("geography", "continent_omnibus_fisher_p", x$fisher_p_value, "p", "primary")
    add("geography", "continent_omnibus_chisq", x$chisq_statistic, "chi-square", "diagnostic")
    add("geography", "continent_omnibus_chisq_df", x$chisq_df, "df", "diagnostic")
    add("geography", "continent_omnibus_chisq_p", x$chisq_p_value, "p", "diagnostic")
  }
  if (!is.null(selection$omnibus_subregion$summary)) {
    add("geography", "subregion_omnibus_fisher_p",
        selection$omnibus_subregion$summary$fisher_p_value[[1]], "p", "primary")
  }

  if (!is.null(selection$focal) && nrow(selection$focal) == 1) {
    x <- selection$focal[1, ]
    add("geography", "europe_focal_or", x$odds_ratio, "OR", "exploratory/focal")
    add("geography", "europe_focal_ci_low", x$ci_low, "95% CI", "exploratory/focal")
    add("geography", "europe_focal_ci_high", x$ci_high, "95% CI", "exploratory/focal")
    add("geography", "europe_focal_p", x$p_value, "p", "exploratory/focal")
  }
  if (!is.null(selection$continent)) {
    eu <- selection$continent[selection$continent$level == "Europe", , drop = FALSE]
    if (nrow(eu) == 1) add("geography", "europe_holm_p", eu$p_holm[[1]], "p", "multiplicity-adjusted")
  }

  if (!is.null(selection$model$table)) {
    for (term in c("Europe", "log_inst_loo")) {
      x <- selection$model$table[selection$model$table$term == term, , drop = FALSE]
      if (nrow(x) == 1) {
        key <- if (term == "Europe") "model_europe" else "model_log_inst_loo"
        add("selection_model", paste0(key, "_or"), x$odds_ratio[[1]], "OR", "primary")
        add("selection_model", paste0(key, "_ci_low"), x$ci_low[[1]], "95% CI", "primary")
        add("selection_model", paste0(key, "_ci_high"), x$ci_high[[1]], "95% CI", "primary")
        add("selection_model", paste0(key, "_p"), x$p_value[[1]], "p", "primary")
      }
    }
  }
  if (!is.null(selection$model$diagnostics)) {
    d <- selection$model$diagnostics[1, ]
    for (nm in names(d)) add("selection_model", nm, d[[nm]], role = "diagnostic")
  }

  if (!is.null(selection$gender_selection$primary)) {
    g <- selection$gender_selection$primary[1, ]
    for (nm in names(g)) add("gender_primary", nm, g[[nm]], role = "primary")
  }
  if (!is.null(selection$gender_selection$missingness)) {
    g <- selection$gender_selection$missingness[1, ]
    for (nm in names(g)) add("gender_missingness", nm, g[[nm]], role = "diagnostic")
  }
  if (!is.null(selection$gender_selection$mixed_sensitivity)) {
    g <- selection$gender_selection$mixed_sensitivity[1, ]
    for (nm in names(g)) add("gender_mixed_instrument", nm, g[[nm]], role = "sensitivity only")
  }

  if (!is.null(selection$perm_europe)) {
    add("network_position", "europe_evc_permutation_p", selection$perm_europe$p_permutation[[1]], "p", "permutation")
  }
  if (!is.null(selection$perm_gender)) {
    add("network_position", "gender_evc_permutation_p", selection$perm_gender$p_permutation[[1]], "p", "permutation")
    add("network_position", "gender_evc_n_female", selection$perm_gender$n_focal[[1]], "persons", "permutation")
    add("network_position", "gender_evc_n_male", selection$perm_gender$n_other[[1]], "persons", "permutation")
  }

  manifest <- dplyr::bind_rows(rows)
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(manifest, output_path)
  message("Manuscript results manifest written: ", output_path)
  output_path
}
