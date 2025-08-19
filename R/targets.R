# targets.R
# A declarative pipeline using the {targets} package.
# To run, execute `targets::tar_make()` in the R console.

library(targets)
library(tarchetypes)

# Set target options
tar_option_set(
  packages = c("tidyverse", "igraph", "ggraph", "readxl", "config", "here", "openxlsx", "ineq", "patchwork")
)

# Source all functions
tar_source("R/functions.R")
tar_source("R/additional_analysis.R")
tar_source("R/visualization_themes.R")

list(
  # 1. Configuration and Setup
  tar_target(config, config::get(file = "config.yml")),
  tar_target(output_dirs, {
    dir.create("output/main_analysis", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/supplementary", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
    "output"
  }),
  
  # 2. Data Processing and Network Building
  tar_target(data_clean, load_and_clean_data(config)),
  tar_target(networks, build_networks(data_clean, config$min_shared_journals)),
  tar_target(g_journal, build_journal_network(data_clean)),
  
  # 3. Analysis
  tar_target(leiden_rec, run_leiden_sweep(networks$g_gc, config)),
  tar_target(updated_config, c(config, list(leiden_resolution = leiden_rec$recommendation$resolution))),
  tar_target(metrics, calculate_network_metrics(networks$g_gc, updated_config)),
  tar_target(disparity_results, analyze_disparities(metrics$editor_stats, metrics$g_gc)),
  tar_target(journal_metrics, calculate_journal_network_metrics(g_journal, metrics$editor_stats, data_clean, updated_config)),
  tar_target(board_analysis, analyze_board_composition(journal_metrics, metrics$editor_stats, data_clean)),
  
  # 4. Visualizations (as file outputs)
  tar_target(main_plots, generate_visualizations(metrics$g_gc, updated_config, file.path(output_dirs, "main_analysis"))),
  tar_target(journal_plots, generate_journal_visualizations(g_journal, journal_metrics, updated_config, file.path(output_dirs, "main_analysis"))),
  tar_target(dashboards, {
    create_disparity_dashboard(disparity_results, metrics$editor_stats, file.path(output_dirs, "main_analysis"))
    create_symbolic_capital_dashboard(metrics, file.path(output_dirs, "main_analysis"))
  }),
  
  # 5. Export
  tar_target(export, {
    final_results <- list(
      graphs = networks, journal_graph = g_journal, journal_metrics = journal_metrics,
      board_analysis = board_analysis, leiden_sweep = leiden_rec, metrics = metrics,
      disparity_results = disparity_results
    )
    export_results(final_results, output_dirs)
    create_publication_tables(final_results, file.path(output_dirs, "tables"))
  })
)