# R/disparity_analysis.R
# Disparity and supplementary analysis

analyze_disparities <- function(editor_stats) {
  message("Performing disparity analysis...")
  results <- list()

  if (!"EVC" %in% names(editor_stats)) {
    stop("'EVC' column not found in editor_stats.")
  }

  # Gender disparity
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

  # Geographic disparity
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
  message("Running supplementary analysis...")

  p <- ggplot(metrics$editor_stats, aes(x = betweenness, y = EVC)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') +
    labs(
      title = "EVC vs. Betweenness Centrality",
      x = "Betweenness Centrality",
      y = "Eigenvector Centrality (EVC)"
    ) +
    theme_bw()

  ggsave(file.path(output_dir, "robustness_evc_vs_betweenness.png"), p, width = 8, height = 6, dpi = 300)
}
