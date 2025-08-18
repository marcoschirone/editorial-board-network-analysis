# run_analysis.R
# Purpose: Main script to execute the entire network analysis pipeline.

# ---- 1. SETUP ----
# Load packages, functions, and configuration
source(here::here("R", "packages.R"))
source(here::here("R", "functions.R"))
cfg <- config::get(file = "config.yml")
output_dir <- here::here("output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 2. PIPELINE EXECUTION ----
message("\n🎉 Starting Analysis Pipeline...\n")

data_clean <- load_and_clean_data(cfg)
networks <- build_networks(data_clean, cfg$min_shared_journals)
leiden_rec <- run_leiden_sweep(networks$g_gc, cfg)

# Update config with the recommended resolution for the main analysis
cfg$leiden_resolution <- leiden_rec$recommendation$resolution

metrics <- calculate_network_metrics(networks$g_gc, cfg)

# Use the UPDATED graph object (metrics$g_gc) which now contains the sci_index
generate_visualizations(metrics$g_gc, cfg, output_dir)


# ---- 3. JOURNAL NETWORK ANALYSIS (NEW) ----
message("\n--- Starting Journal Network Analysis ---\n")

g_journal <- build_journal_network(data_clean, metrics$editor_stats)
generate_journal_visualizations(g_journal, output_dir, cfg)


# ---- 4. EXPORT RESULTS ----
# Bundle all results for export
final_results <- list(
  graphs = networks,
  journal_graph = g_journal, # Add new graph to exports
  leiden_sweep = leiden_rec,
  metrics = metrics
)
export_results(final_results, output_dir)

message("\n🎉 Analysis complete! Check the 'output' folder for results.")