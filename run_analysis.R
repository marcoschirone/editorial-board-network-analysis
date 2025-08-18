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
metrics <- calculate_network_metrics(networks$g_gc, cfg)

generate_visualizations(metrics$g_gc, cfg, output_dir)

# ---- 3. JOURNAL NETWORK ANALYSIS ----
message("\n--- Starting Journal Network Analysis ---\n")

journal_network_objects <- build_journal_network(data_clean, metrics$editor_stats)

# ** THIS BLOCK IS NOW CORRECTED **
# Calculate dominant community and subregion for each journal
journal_assignments <- data_clean %>%
  # Select only the 'community' column from editor_stats to avoid name conflicts
  left_join(metrics$editor_stats %>% select(name, community), by = c("anon_id" = "name")) %>%
  group_by(Journal) %>%
  summarise(
    community = statistical_mode(community),
    dominant_subregion = statistical_mode(Subregion_1),
    .groups = "drop"
  )

generate_journal_visualizations(
  g_journal = journal_network_objects$g_journal,
  journal_assignments = journal_assignments,
  output_dir = output_dir,
  cfg = cfg
)

# ---- 4. EXPORT RESULTS ----
final_results <- list(
  graphs = networks,
  journal_graph = journal_network_objects$g_journal,
  journal_stats = journal_network_objects$journal_stats,
  journal_assignments = journal_assignments,
  metrics = metrics
)
export_results(final_results, output_dir)

message("\n🎉 Analysis complete! Check the 'output' folder for your results.")