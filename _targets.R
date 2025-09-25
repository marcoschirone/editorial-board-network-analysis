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
    dirs <- c("output/main_analysis", "output/supplementary", "output/tables", "output/robustness")
    lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
    dirs
  }),
  
  # Data processing & Network construction
  tar_target(data_clean, load_and_clean_data(config)),
  tar_target(networks, build_networks(data_clean, config$min_shared_journals)),
  tar_target(g_journal, build_journal_network(data_clean, min_shared_editors = 1)),
  
  # Core analysis
  tar_target(leiden_rec, run_leiden_sweep(networks$g_gc, config)),
  tar_target(updated_config, utils::modifyList(config, list(leiden_resolution = leiden_rec$recommendation$resolution))),
  tar_target(metrics, calculate_network_metrics(networks$g_gc, updated_config)),
  tar_target(journal_metrics, calculate_journal_network_metrics(g_journal, metrics$editor_stats, data_clean, updated_config)),
  tar_target(disparity_results, analyze_disparities(metrics$editor_stats)),
  tar_target(board_analysis, analyze_board_composition(journal_metrics$journal_stats, metrics$editor_stats, data_clean)),
  
  # Visualizations for main figures
  tar_target(figure_1_plot, {
    generate_editor_network_panels(metrics$g_gc, metrics$editor_stats, updated_config, "output/main_analysis")
    "output/main_analysis/figure_1_editor_network_panels.png"
  }, format = "file"),
  
  tar_target(figure_2_plot, {
    generate_journal_community_visualization(journal_metrics$g_journal, journal_metrics$journal_stats, updated_config, "output/main_analysis")
    "output/main_analysis/figure_2_journal_network_communities.png"
  }, format = "file"),
  
  tar_target(figure_3_plot, {
    generate_journal_network_panels(journal_metrics$g_journal, journal_metrics$journal_stats, updated_config, "output/main_analysis")
    "output/main_analysis/figure_3_journal_network_panels.png"
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
      robustness = robustness_analysis
    )
    export_results(final_results, "output")
    c(
      "output/editor_metrics.csv", "output/journal_metrics.csv",
      "output/inequality_measures.csv", "output/full_analysis_results.rds"
    )
  }, format = "file"),
  
  # Publication tables target with all necessary dependencies
  tar_target(
    publication_tables,
    {
      final_results_for_tables <- list(
        metrics = metrics,
        journal_metrics = journal_metrics,
        disparity_results = disparity_results,
        board_analysis = board_analysis # This was the missing piece
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
  tar_target(session_info_file, write_session_info("output/sessionInfo.txt"), format = "file"),
  tar_target(r_packages_bib, write_pkg_citations("output/R-packages.bib"), format = "file")
)