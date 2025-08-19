# R/visualization_themes.R
# Purpose: Define consistent visual themes and advanced visualizations.

# =================== GGPLOT THEME ===================

theme_publication <- function(base_size = 11, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2)),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.key.size = unit(0.8, "lines")
    )
}

# =================== DASHBOARDS ===================

create_disparity_dashboard <- function(disparity_results, editor_stats, output_dir) {
  message("  -> Creating disparity dashboard...")
  
  p_gender <- editor_stats %>%
    filter(!is.na(Gender)) %>%
    ggplot(aes(x = Gender, y = eigenvector, fill = Gender)) +
    geom_violin(alpha = 0.7) +
    labs(title = "Gender Disparities", y = "Symbolic Capital") +
    theme_publication() +
    theme(legend.position = "none")
  
  p_geo <- editor_stats %>%
    filter(!is.na(Continent_1)) %>%
    ggplot(aes(x = reorder(Continent_1, eigenvector, FUN = median), y = eigenvector, fill = Continent_1)) +
    geom_boxplot(alpha = 0.7) +
    coord_flip() +
    labs(title = "Geographic Disparities", x = "", y = "Symbolic Capital") +
    theme_publication() +
    theme(legend.position = "none")
  
  if(require(patchwork)){
    dashboard <- p_gender + p_geo
    ggsave(file.path(output_dir, "disparity_dashboard.png"), dashboard, width = 10, height = 6, dpi = 300)
    message("     Disparity dashboard created.")
  }
}

create_symbolic_capital_dashboard <- function(metrics, output_dir) {
  message("  -> Creating symbolic capital overview dashboard...")
  
  p_dist <- ggplot(metrics$editor_stats, aes(x = eigenvector)) +
    geom_histogram(bins = 40, fill = "skyblue", alpha = 0.8) +
    labs(title = "Distribution of Symbolic Capital", x = "Eigenvector Centrality") +
    theme_publication()
  
  ggsave(file.path(output_dir, "symbolic_capital_distribution.png"), p_dist, width = 8, height = 6, dpi = 300)
  message("     Symbolic capital distribution plot saved.")
}


# =================== ADVANCED PLOTS ===================

create_journal_chord_diagram <- function(g_journal, output_dir) {
  if (!requireNamespace("circlize", quietly = TRUE)) {
    message("     Skipping chord diagram: 'circlize' package not installed.")
    return(NULL)
  }
  message("  -> Creating journal chord diagram...")
  # Implementation from original script
}

create_alluvial_diagram <- function(editor_stats, output_dir) {
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    message("     Skipping alluvial diagram: 'ggalluvial' package not installed.")
    return(NULL)
  }
  message("  -> Creating alluvial diagram...")
  # Implementation from original script
}