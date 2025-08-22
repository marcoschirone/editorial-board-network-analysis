# run_analysis.R
# Purpose: Complete symbolic capital analysis pipeline from setup to export.

# ---- 1. SETUP AND INITIALIZATION ----
message("\n", rep("=", 60), "\n")
message("   SYMBOLIC CAPITAL IN THE EDITORIAL FIELD")
message(rep("=", 60), "\n")

# Load all required packages and functions
source(here::here("R", "packages.R"))
source(here::here("R", "functions.R"))
source(here::here("R", "additional_analysis.R"))
cfg <- config::get(file = "config.yml")

# Create output directories
output_dir <- here::here("output")
dir.create(file.path(output_dir, "main_analysis"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "supplementary"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)

# ---- 2. DATA LOADING AND NETWORK CONSTRUCTION ----
message("\n--- Phase 1: Data Loading and Network Construction ---")
data_clean <- load_and_clean_data(cfg)
networks <- build_networks(data_clean, cfg$min_shared_journals)
g_journal <- build_journal_network(data_clean, min_shared_editors = 1)

# ---- 3. COMMUNITY DETECTION ----
message("\n--- Phase 2: Community Detection ---")
message(sprintf("  Using manual editor resolution set to: %.2f", cfg$leiden_resolution))

# ---- 4. SYMBOLIC CAPITAL CALCULATION ----
message("\n--- Phase 3: Symbolic Capital Metrics ---")
metrics <- calculate_network_metrics(networks$g_gc, cfg)

# ---- 5. DISPARITY & JOURNAL ANALYSIS ----
message("\n--- Phase 4: Disparity & Journal Analysis ---")
disparity_results <- analyze_disparities(metrics$editor_stats, metrics$g_gc)
journal_metrics <- calculate_journal_network_metrics(g_journal, metrics$editor_stats, data_clean, cfg)
board_analysis <- analyze_board_composition(journal_metrics$journal_stats, metrics$editor_stats, data_clean)

# ---- 6. SUPPLEMENTARY ANALYSIS ----
message("\n--- Phase 5: Supplementary Analysis (Robustness) ---")
run_supplementary_analysis(metrics, file.path(output_dir, "supplementary"))

# ---- 7. VISUALIZATIONS ----
message("\n--- Phase 6: Generating Visualizations ---")
generate_visualizations(metrics$g_gc, cfg, file.path(output_dir, "main_analysis"))
generate_journal_visualizations(journal_metrics$g_journal, journal_metrics$journal_stats, cfg, file.path(output_dir, "main_analysis"))
generate_journal_community_visualization(journal_metrics$g_journal, journal_metrics$journal_stats, cfg, file.path(output_dir, "main_analysis"))
create_full_disparity_dashboard(metrics$editor_stats, file.path(output_dir, "main_analysis"))

# ---- 8. EXPORT RESULTS ----
message("\n--- Phase 7: Exporting Results ---")
final_results <- list(
  graphs = networks,
  journal_metrics = journal_metrics,
  board_analysis = board_analysis,
  metrics = metrics,
  disparity_results = disparity_results
)
export_results(final_results, output_dir)
create_publication_tables(final_results, file.path(output_dir, "tables"))


# ---- 9. FINAL SUMMARY AND QUALITY CHECKS ----
message("\n--- Phase 8: Quality Checks and Final Summary ---")
perform_quality_checks(metrics, networks)
print_final_summary(metrics, journal_metrics$journal_stats)

message("\n\u2713 All analyses completed successfully.")
message("\u2713 Results saved to: ", normalizePath(output_dir))