# _targets.R - Clean, organized pipeline
# Run with: targets::tar_make()

library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c(
    "tidyverse", "igraph", "ggraph", "readxl", "config",
    "here", "openxlsx", "ineq", "patchwork", "viridis", "forcats"
  )
)

# Source organized function files
tar_source(c(
  "R/utils.R",
  "R/data_processing.R", 
  "R/network_construction.R",
  "R/network_analysis.R",
  "R/disparity_analysis.R",
  "R/quality_checks.R",
  "R/data_export.R",
  "R/visualizations.R"  # We'll create this separately
))

# Reproducibility helpers
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

# Pipeline
list(
  
  # Configuration and setup
  tar_target(config, config::get(file = "config.yml")),
  tar_target(output_dirs, {
    dirs <- c("output/main_analysis", "output/supplementary", "output/tables")
    lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
    "output"
  }),
  
  # Data processing
  tar_target(data_clean, load_and_clean_data(config)),
  
  # Network construction
  tar_target(networks, build_networks(data_clean, config$min_shared_journals)),
  tar_target(g_journal, build_journal_network(data_clean, min_shared_editors = 1)),
  
  # Community detection optimization (optional)
  tar_target(leiden_rec, run_leiden_sweep(networks$g_gc, config)),
  tar_target(
    updated_config,
    utils::modifyList(config, list(leiden_resolution = leiden_rec$recommendation$resolution))
  ),
  
  # Core analysis
  tar_target(metrics, calculate_network_metrics(networks$g_gc, updated_config)),
  tar_target(journal_metrics, calculate_journal_network_metrics(g_journal, metrics$editor_stats, data_clean, updated_config)),
  tar_target(disparity_results, analyze_disparities(metrics$editor_stats)),
  tar_target(board_analysis, analyze_board_composition(journal_metrics$journal_stats, metrics$editor_stats, data_clean)),
  
  # Supplementary analysis
  tar_target(supp_analysis, {
    run_supplementary_analysis(metrics, file.path(output_dirs, "supplementary"))
    "supplementary_complete"
  }),
  
  # Visualizations
  tar_target(
    main_plots,
    {
      generate_visualizations(metrics$g_gc, updated_config, file.path(output_dirs, "main_analysis"))
      list.files(file.path(output_dirs, "main_analysis"), pattern = "network_.*\\.png$", full.names = TRUE)
    },
    format = "file"
  ),
  
  tar_target(
    journal_plots,
    {
      generate_journal_visualizations(journal_metrics$g_journal, journal_metrics$journal_stats, updated_config, file.path(output_dirs, "main_analysis"))
      generate_journal_community_visualization(journal_metrics$g_journal, journal_metrics$journal_stats, updated_config, file.path(output_dirs, "main_analysis"))
      list.files(file.path(output_dirs, "main_analysis"), pattern = "journal_.*\\.png$", full.names = TRUE)
    },
    format = "file"
  ),
  
  tar_target(
    disparity_plots,
    {
      create_full_disparity_dashboard(metrics$editor_stats, file.path(output_dirs, "main_analysis"))
      list.files(file.path(output_dirs, "main_analysis"), pattern = "disparity_.*\\.png$", full.names = TRUE)
    },
    format = "file"
  ),
  
  # Data export
  tar_target(
    exported_results,
    {
      final_results <- list(
        graphs = networks,
        journal_metrics = journal_metrics,
        board_analysis = board_analysis,
        metrics = metrics,
        disparity_results = disparity_results,
        leiden_sweep = leiden_rec
      )
      export_results(final_results, output_dirs)
      
      # Return created files
      c(
        file.path(output_dirs, "editor_metrics.csv"),
        file.path(output_dirs, "journal_metrics.csv"),
        file.path(output_dirs, "inequality_measures.csv"),
        file.path(output_dirs, "full_analysis_results.rds")
      )
    },
    format = "file"
  ),
  
  tar_target(
    publication_tables,
    {
      final_results <- list(
        graphs = networks,
        journal_metrics = journal_metrics,
        board_analysis = board_analysis,
        metrics = metrics,
        disparity_results = disparity_results,
        leiden_sweep = leiden_rec
      )
      create_publication_tables(final_results, file.path(output_dirs, "tables"))
      file.path(output_dirs, "tables", "publication_summary_tables.xlsx")
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