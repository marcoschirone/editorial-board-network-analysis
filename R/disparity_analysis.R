# R/disparity_analysis.R
# Functions for disparity and supplementary analysis.

analyze_disparities <- function(editor_stats) {
  message("Performing disparity analysis...")
  results <- list()
  
  if (!"EVC" %in% names(editor_stats)) {
    stop("'EVC' column not found in editor_stats.")
  }
  
  if ("Gender" %in% names(editor_stats)) {
    gender_data <- editor_stats %>% filter(!is.na(Gender) & Gender %in% c("Male", "Female"))
    if (nrow(gender_data) > 0 && length(unique(gender_data$Gender)) >= 2) {
      test <- wilcox.test(EVC ~ Gender, data = gender_data)
      results$gender <- gender_data %>%
        group_by(Gender) %>%
        summarise(n = n(), median_sc = median(EVC, na.rm = TRUE), .groups = "drop") %>%
        mutate(p_value = test$p.value)
    }
  }
  
  if ("Continent_1" %in% names(editor_stats)) {
    geo_data <- editor_stats %>% filter(!is.na(Continent_1))
    if (nrow(geo_data) > 0 && n_distinct(geo_data$Continent_1) > 1) {
      test <- kruskal.test(EVC ~ Continent_1, data = geo_data)
      results$geographic <- geo_data %>%
        group_by(Continent_1) %>%
        summarise(n = n(), median_sc = median(EVC, na.rm = TRUE), .groups = "drop") %>%
        mutate(p_value = test$p.value)
    }
  }
  
  results
}

analyze_board_composition <- function(journal_stats, editor_stats, data_clean) {
  message("Analyzing board composition patterns...")
  
  data_clean %>%
    left_join(editor_stats %>% select(name, EVC, degree, betweenness), by = c("editor_id" = "name")) %>%
    group_by(Journal) %>%
    summarise(
      n_editors = n(),
      prop_female = mean(Gender == "Female", na.rm = TRUE),
      geographic_diversity = n_distinct(Continent, na.rm = TRUE),
      median_evc = median(EVC, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(journal_stats, by = "Journal")
}

run_supplementary_analysis <- function(metrics, output_dir) {
  message("Running supplementary analysis for centrality comparisons...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  centrality_data_long <- metrics$editor_stats %>%
    select(EVC, degree, betweenness, closeness) %>%
    tidyr::pivot_longer(
      cols = -EVC,
      names_to = "centrality_metric",
      values_to = "value"
    ) %>%
    mutate(centrality_metric = stringr::str_to_title(centrality_metric))
  
  p <- ggplot(centrality_data_long, aes(x = value, y = EVC)) +
    geom_point(alpha = 0.4, color = "navy") +
    geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', color = "red", linetype = "dashed") +
    facet_wrap(~ centrality_metric, scales = "free_x") +
    labs(
      title = "Comparison of Eigenvector Centrality with Other Centrality Measures",
      subtitle = "Spearman rank correlation is used for formal robustness testing",
      y = "Eigenvector Centrality (EVC)",
      x = "Centrality Score"
    ) +
    theme_bw()
  
  ggsave(
    file.path(output_dir, "centrality_comparison_scatterplots.png"), 
    p, width = 12, height = 5, dpi = 300
  )
  
  message("Supplementary centrality comparison plot saved.")
  return(invisible(TRUE))
}