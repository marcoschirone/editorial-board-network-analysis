# run_analysis.R
# Purpose: Main script to execute the entire network analysis pipeline.

# ---- 1. SETUP ----
# Load packages, functions, and configuration
source(here::here("R", "packages.R"))
source(here::here("R", "functions.R"))
cfg <- config::get()
output_dir <- here::here("output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 2. PIPELINE EXECUTION ----
# Each step calls a function and uses its output in the next step.
data_clean <- load_and_clean_data(cfg)
networks <- build_networks(data_clean, cfg$min_shared_journals)
leiden_rec <- run_leiden_sweep(networks$g_gc, cfg)

# Update config with recommended resolution for the main analysis
cfg$leiden_resolution <- leiden_rec$recommendation$resolution

metrics <- calculate_network_metrics(networks$g_gc, cfg)
generate_visualizations(networks$g_gc, metrics$editor_stats, cfg, output_dir)

# Bundle all results for export
final_results <- list(
  graphs = networks,
  leiden_sweep = leiden_rec,
  metrics = metrics
)
export_results(final_results, output_dir)

message("\n🎉 Analysis complete!")
