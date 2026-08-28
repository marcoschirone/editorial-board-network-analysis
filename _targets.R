# _targets.R
# Main pipeline definition for the editorial board network analysis.

library(targets)
library(tarchetypes)

# Set target options for all targets in this pipeline.
tar_option_set(
  packages = c(
    "tidyverse", "igraph", "ggraph", "readxl", "config",
    "here", "openxlsx", "ineq", "patchwork", "viridis", "forcats",
    "RColorBrewer"
  ),
  error = "continue",
  memory = "transient"
)

# Source all R functions from the R/ directory.
tar_source(c(
  "R/utils.R",
  "R/person_disambiguation.R",
  "R/selection_analysis.R",
  "R/data_processing.R", 
  "R/network_construction.R",
  "R/network_analysis.R",
  "R/disparity_analysis.R",
  "R/quality_checks.R",
  "R/data_export.R",
  "R/visualizations.R",
  "R/robustness_checks.R" 
))

# Helper function to write session information for reproducibility.
write_session_info <- function(path = "output/sessionInfo.txt") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    x <- utils::capture.output(sessioninfo::session_info(include_base = TRUE))
  } else {
    x <- utils::capture.output(sessionInfo())
  }
  writeLines(x, con = path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Helper function to generate BibTeX citations for used R packages.
write_pkg_citations <- function(path = "output/R-packages.bib") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  si <- sessionInfo()
  attached <- names(si$otherPkgs)
  pkgs <- sort(attached)
  bibs <- lapply(pkgs, function(p) utils::toBibtex(utils::citation(p), style = "BibTeX"))
  bibs_chr <- vapply(bibs, paste, character(1L), collapse = "\n\n")
  writeLines(bibs_chr, con = path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Define the pipeline structure as a list of targets.
list(
  # Configuration and setup
  tar_target(config, config::get(file = "config.yml")),
  tar_target(output_dirs, {
    dirs <- c("output/main_analysis", "output/supplementary", "output/tables", "output/robustness", "output/selection")
    lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
    dirs
  }),
  
  # Authoritative population reconstruction. The same post-merge object feeds
  # both network membership and the selection analysis.
  tar_target(full_population_file, config$full_population_path, format = "file"),
  tar_target(m49_lookup_file, config$m49_lookup_path, format = "file"),
  tar_target(confirmed_merges_file, config$confirmed_merges_path, format = "file"),
  tar_target(annotation_file, config$annotation_path, format = "file"),
  tar_target(gender_namsor_file, config$gender_namsor_path, format = "file"),
  tar_target(gender_adjudication_file, config$gender_adjudication_path, format = "file"),
  tar_target(population_data, {
    validate_config(config)
    build_person_level(
      full_path = full_population_file,
      lookup_path = m49_lookup_file,
      country_rule = "modal",
      merges_path = confirmed_merges_file
    )
  }),
  tar_target(gender_metadata, build_gender_metadata(
    population_data,
    gender_namsor_path = gender_namsor_file,
    annotation_path = annotation_file,
    gender_adjudication_path = gender_adjudication_file
  )),
  tar_target(data_clean, build_network_input(
    population_data,
    annotation_path = annotation_file,
    gender_metadata = gender_metadata
  )),
  tar_target(networks, build_networks(data_clean, config$min_shared_journals)),
  tar_target(g_journal, build_journal_network(data_clean, min_shared_editors = 1)),
  tar_target(g_journal_gc, get_giant_component(g_journal)),
  
  # Core analysis
  tar_target(leiden_rec, run_leiden_sweep(networks$g_gc, config)),
  tar_target(updated_config, utils::modifyList(config, list(leiden_resolution = leiden_rec$recommendation$resolution))),
  tar_target(metrics, calculate_network_metrics(networks$g_gc, updated_config)),
  tar_target(journal_metrics, calculate_journal_network_metrics(g_journal_gc, metrics$editor_stats, data_clean, updated_config)),
  tar_target(disparity_results, analyze_disparities(metrics$editor_stats)),
  tar_target(board_analysis, analyze_board_composition(journal_metrics$journal_stats, metrics$editor_stats, data_clean)),

  # Selection analysis uses the exact same reconstructed population and the
  # corrected network metrics, eliminating the former 71-vs-current mismatch.
  tar_target(selection_outputs, {
    selection_results <- run_selection_analysis(
      full_path = full_population_file,
      lookup_path = m49_lookup_file,
      editor_stats = metrics$editor_stats,
      output_dir = "output/selection",
      merges_path = confirmed_merges_file,
      built = population_data,
      gender_metadata = gender_metadata
    )
    saveRDS(selection_results, "output/selection/selection_results.rds")
    list.files("output/selection", full.names = TRUE, recursive = FALSE)
  }, format = "file"),
  
  # Figure 1: Editor network by gender
  tar_target(figure_1_plot, {
    generate_gender_network(metrics$g_gc, metrics$editor_stats, updated_config, "output/main_analysis")
    file.path("output/main_analysis", paste0("Figure_1.", c("png", "pdf", "tiff")))
  }, format = "file"),

  # Figure 2: Editor network by subregion
  tar_target(figure_2_plot, {
    generate_subregion_network(metrics$g_gc, metrics$editor_stats, updated_config, "output/main_analysis")
    file.path("output/main_analysis", paste0("Figure_2.", c("png", "pdf", "tiff")))
  }, format = "file"),

  # Figure 3: Journal network communities (giant component)
  tar_target(figure_3_plot, {
    generate_journal_community_visualization(g_journal_gc, journal_metrics$journal_stats, updated_config, "output/main_analysis")
    file.path("output/main_analysis", paste0("Figure_3.", c("png", "pdf", "tiff")))
  }, format = "file"),
  
  # Figure 4: Journal network panels — EVC + Gini (giant component)
  tar_target(figure_4_plot, {
    generate_journal_network_panels(g_journal_gc, journal_metrics$journal_stats, updated_config, "output/main_analysis")
    file.path("output/main_analysis", paste0("Figure_4.", c("png", "pdf", "tiff")))
  }, format = "file"),
  
  tar_target(disparity_plots, {
    create_full_disparity_dashboard(metrics$editor_stats, "output/main_analysis")
    list.files("output/main_analysis", pattern = "disparity_.*\\.png$", full.names = TRUE)
  }, format = "file"),
  
  # Robustness analysis
  tar_target(robustness_analysis, run_comprehensive_robustness(
    data_clean = data_clean,
    g_full = networks$g_full,
    g_gc = metrics$g_gc,
    cfg = updated_config,
    output_dir = "output/robustness"
  )
  ),
  
  # Supplementary analysis
  tar_target(supp_analysis, {
    run_supplementary_analysis(metrics, "output/supplementary")
    "output/supplementary/centrality_comparison_scatterplots.png"
  }, format = "file"),
  
  # Data export
  tar_target(exported_results, {
    final_results <- list(
      graphs = networks,
      journal_metrics = journal_metrics,
      metrics = metrics,
      disparity_results = disparity_results,
      leiden_sweep = leiden_rec,
      robustness = robustness_analysis,
      selection = { selection_outputs; readRDS("output/selection/selection_results.rds") }
    )
    export_results(final_results, "output")
    c(
      "output/editor_metrics.csv", "output/journal_metrics.csv",
      "output/inequality_measures.csv", "output/full_analysis_results.rds"
    )
  }, format = "file"),
  
  # Publication tables
  tar_target(
    publication_tables,
    {
      final_results_for_tables <- list(
        metrics = metrics,
        journal_metrics = journal_metrics,
        disparity_results = disparity_results,
        board_analysis = board_analysis,
        selection = { selection_outputs; readRDS("output/selection/selection_results.rds") }
      )
      create_publication_tables(final_results_for_tables, "output/tables")
      "output/tables/publication_summary_tables.xlsx"
    },
    format = "file"
  ),
  
  # Quality checks and summary
  tar_target(quality_checks, perform_quality_checks(metrics, networks)),
  tar_target(final_summary, print_final_summary(metrics, journal_metrics$journal_stats)),
  
  # Reproducibility artifacts
  tar_target(session_info_file, {
    output_dirs
    write_session_info("output/sessionInfo.txt")
  }, format = "file"),
  tar_target(r_packages_bib, {
    output_dirs
    write_pkg_citations("output/R-packages.bib")
  }, format = "file")
)
