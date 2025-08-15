# _targets.R
# A declarative pipeline using the {targets} package.
# To run, execute `targets::tar_make()` in the R console.

library(targets)

# Load packages, functions, and config
source("R/packages.R")
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "igraph", "readxl", "config"))

list(
  # Load configuration
  tar_target(config, config::get(), cue = tar_cue("always")),

  # 1. Preprocessing
  tar_target(
    data_clean,
    load_and_clean_data(config)
  ),

  # 2. Network Building
  tar_target(
    networks,
    build_networks(data_clean, config$min_shared_journals)
  ),

  # 2b. Leiden Sweep
  tar_target(
    leiden_results,
    run_leiden_sweep(networks$g_gc, config)
  ),

  # 3. Metrics Calculation (using recommended resolution)
  tar_target(
    network_metrics,
    calculate_network_metrics(
      networks$g_gc,
      config::merge(config, list(leiden_resolution = leiden_results$recommendation$resolution))
    )
  ),

  # 4. Visualizations
  tar_target(
    network_plots,
    generate_visualizations(
      networks$g_gc,
      network_metrics$editor_stats,
      config,
      "output"
    ),
    format = "file"
  ),

  # 5. Exports
  tar_target(
    exported_files,
    export_results(
      list(graphs = networks, leiden_sweep = leiden_results, metrics = network_metrics),
      "output"
    )
  )
)
