# R/additional_analysis.R
# Purpose: Extended functions for disparity, supplementary, and board analysis.

# =================== DISPARITY ANALYSIS ===================

analyze_disparities <- function(editor_stats, g_gc) {
  message("  -> Performing comprehensive disparity analysis...")
  results <- list()
  
  # Gender disparity analysis
  if ("Gender" %in% names(editor_stats)) {
    gender_data <- editor_stats %>% filter(!is.na(Gender) & Gender %in% c("Male", "Female"))
    if (nrow(gender_data) > 0 && length(unique(gender_data$Gender)) == 2) {
      test <- wilcox.test(eigenvector ~ Gender, data = gender_data)
      results$gender <- gender_data %>%
        group_by(Gender) %>%
        summarise(n = n(), mean_sc = mean(eigenvector), .groups = "drop") %>%
        mutate(p_value = test$p.value)
    }
  }
  
  # Geographic disparity analysis
  if ("Continent_1" %in% names(editor_stats)) {
    geo_data <- editor_stats %>% filter(!is.na(Continent_1))
    if (nrow(geo_data) > 0 && n_distinct(geo_data$Continent_1) > 1) {
      test <- kruskal.test(eigenvector ~ Continent_1, data = geo_data)
      results$geographic <- geo_data %>%
        group_by(Continent_1) %>%
        summarise(n = n(), mean_sc = mean(eigenvector), .groups = "drop") %>%
        arrange(desc(mean_sc))
    }
  }
  
  return(results)
}

# =================== BOARD COMPOSITION ===================

analyze_board_composition <- function(journal_stats, editor_stats, data_clean) {
  message("  -> Analyzing board composition patterns...")
  board_composition <- data_clean %>%
    left_join(
      editor_stats %>% select(name, eigenvector, degree, betweenness), 
      by = c("anon_id" = "name")
    ) %>%
    group_by(Journal) %>%
    summarise(
      prop_female = mean(Gender == "Female", na.rm = TRUE),
      geographic_diversity = n_distinct(Continent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(journal_stats, by = "Journal")
  
  return(board_composition)
}

# =================== SUPPLEMENTARY ANALYSIS (ROBUSTNESS) ===================

run_supplementary_analysis <- function(metrics, output_dir) {
  message("  -> Running supplementary analysis for robustness checks...")
  
  cor_matrix <- metrics$editor_stats %>%
    select(eigenvector, betweenness, degree) %>%
    cor(use = "complete.obs", method = "spearman")
  
  message(sprintf("     Spearman Correlation (Eigenvector-Betweenness): %.3f", cor_matrix["eigenvector", "betweenness"]))
  
  p_supp <- ggplot(metrics$editor_stats, aes(x = eigenvector, y = betweenness)) +
    geom_point(alpha = 0.2, color = "#2E6E98") +
    geom_smooth(method = "loess", color = "#E15759", se = TRUE) +
    scale_y_log10() +
    labs(
      title = "Supplementary: Eigenvector vs. Betweenness Centrality",
      subtitle = "Illustrating distinct dimensions of network influence",
      x = "Symbolic Capital (Eigenvector)",
      y = "Brokerage (Betweenness) - Log Scale"
    ) +
    theme_bw()
  
  ggsave(file.path(output_dir, "eigenvector_vs_betweenness.png"), p_supp, width = 10, height = 8, dpi = 300)
  message("     Supplementary plot saved.")
}


# =================== PUBLICATION TABLES ===================

create_publication_tables <- function(results, output_dir) {
  message("  -> Creating publication-ready tables...")
  wb <- openxlsx::createWorkbook()
  
  if (!is.null(results$metrics$editor_stats)) {
    openxlsx::addWorksheet(wb, "Editor_Metrics")
    openxlsx::writeData(wb, "Editor_Metrics", results$metrics$editor_stats)
  }
  if (!is.null(results$journal_metrics$journal_stats)) {
    openxlsx::addWorksheet(wb, "Journal_Metrics")
    openxlsx::writeData(wb, "Journal_Metrics", results$journal_metrics$journal_stats)
  }
  if (!is.null(results$disparity_results$gender)) {
    openxlsx::addWorksheet(wb, "Disparity_Gender")
    openxlsx::writeData(wb, "Disparity_Gender", results$disparity_results$gender)
  }
  if (!is.null(results$disparity_results$geographic)) {
    openxlsx::addWorksheet(wb, "Disparity_Geography")
    openxlsx::writeData(wb, "Disparity_Geography", results$disparity_results$geographic)
  }
  
  if (length(openxlsx::sheets(wb)) > 0) {
    openxlsx::saveWorkbook(wb, file.path(output_dir, "publication_summary_tables.xlsx"), overwrite = TRUE)
    message("     Publication tables saved to .xlsx file.")
  }
}

# =================== COMPREHENSIVE DISPARITY DASHBOARD ===================

create_full_disparity_dashboard <- function(editor_stats, output_dir) {
  message("  -> Creating comprehensive disparity dashboard...")
  axis_label <- "Symbolic Capital (Eigenvector Centrality)"
  
  # Plot A: Gender Disparities
  p_gender <- editor_stats %>%
    filter(!is.na(Gender)) %>%
    ggplot(aes(x = Gender, y = eigenvector, fill = Gender)) +
    geom_violin() +
    labs(title = "Gender Disparities", x = "Gender", y = axis_label) +
    theme_bw() + theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_gender.png"), p_gender, width = 7, height = 7, dpi = 300)
  
  # Plot B: Geographic Disparities (Continent)
  p_continent <- editor_stats %>%
    filter(!is.na(Continent_1)) %>%
    ggplot(aes(x = reorder(Continent_1, eigenvector, FUN = median), y = eigenvector, fill = Continent_1)) +
    geom_boxplot() + coord_flip() +
    labs(title = "Geographic Disparities (Continent)", x = "", y = axis_label) +
    theme_bw() + theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_continent.png"), p_continent, width = 8, height = 6, dpi = 300)
  
  # Plot C: Geographic Disparities (Subregion)
  p_subregion <- editor_stats %>%
    filter(!is.na(Subregion_1)) %>%
    mutate(Subregion_lumped = forcats::fct_lump_n(Subregion_1, n = 7, other_level = "Other")) %>%
    ggplot(aes(x = reorder(Subregion_lumped, eigenvector, FUN = median), y = eigenvector, fill = Subregion_lumped)) +
    geom_boxplot() + coord_flip() +
    labs(title = "Geographic Disparities (Subregion)", x = "", y = axis_label) +
    theme_bw() + theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_subregion.png"), p_subregion, width = 8, height = 6, dpi = 300)
  
  # Plot D: Geographic Disparities (Country)
  p_country <- editor_stats %>%
    filter(!is.na(Country_1)) %>%
    mutate(Country_lumped = forcats::fct_lump_n(Country_1, n = 10, other_level = "Other")) %>%
    ggplot(aes(x = reorder(Country_lumped, eigenvector, FUN = median), y = eigenvector, fill = Country_lumped)) +
    geom_boxplot() + coord_flip() +
    labs(title = "Geographic Disparities (Country)", x = "", y = axis_label) +
    theme_bw() + theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_country.png"), p_country, width = 8, height = 6, dpi = 300)
  
  message("     Saved 4 separate disparity plots: gender, continent, subregion, and country.")
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    dashboard <- (p_gender + p_continent) / (p_subregion + p_country) +
      patchwork::plot_annotation(title = "Disparity Analysis of Symbolic Capital in the Editorial Field")
    ggsave(file.path(output_dir, "disparity_dashboard_full.png"), dashboard, width = 14, height = 12, dpi = 300)
    message("     Full combined disparity dashboard also saved.")
  }
}