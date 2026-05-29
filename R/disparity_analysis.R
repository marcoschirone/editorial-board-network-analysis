# R/disparity_analysis.R
# Functions for disparity and supplementary analysis.

analyze_disparities <- function(editor_stats) {
  message("Performing disparity analysis...")
  results <- list()
  
  if (!"EVC" %in% names(editor_stats)) {
    stop("'EVC' column not found in editor_stats.")
  }
  
  # Gender disparities 
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
  
  # Geographic disparities: Continent level 
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
  
  # Geographic disparities: Subregion level
  if ("Subregion_1" %in% names(editor_stats)) {
    geo_data_sub <- editor_stats %>% filter(!is.na(Subregion_1))
    if (nrow(geo_data_sub) > 0 && n_distinct(geo_data_sub$Subregion_1) > 1) {
      test_sub <- kruskal.test(EVC ~ Subregion_1, data = geo_data_sub)
      results$geographic_subregion <- geo_data_sub %>%
        group_by(Subregion_1) %>%
        summarise(n = n(), median_sc = median(EVC, na.rm = TRUE), .groups = "drop") %>%
        mutate(p_value = test_sub$p.value)
    }
  }
  
  # Geographic disparities: Country level
  if ("Country_1" %in% names(editor_stats)) {
    geo_data_country <- editor_stats %>% filter(!is.na(Country_1))
    if (nrow(geo_data_country) > 0 && n_distinct(geo_data_country$Country_1) > 1) {
      test_country <- kruskal.test(EVC ~ Country_1, data = geo_data_country)
      results$geographic_country <- geo_data_country %>%
        group_by(Country_1) %>%
        summarise(n = n(), median_sc = median(EVC, na.rm = TRUE), .groups = "drop") %>%
        mutate(p_value = test_country$p.value)
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

#' Classify journals into the 2x2 typology used in Table 2.
#' Both dimensions split at their respective sample medians across all journals.
#' Journals with n_editors <= 1 are excluded as Gini is undefined for them.
#' The threshold values are stored in the output so the classification
#' criteria are fully transparent and reproducible.
#'
#' @param journal_stats Data frame from calculate_journal_network_metrics(),
#'   must contain columns: Journal, n_editors, median_evc, gini_evc.
#' @return A data frame with typology labels and the threshold values used.

classify_journal_typology <- function(journal_stats) {
  message("Classifying journals into 2x2 typology (Table 2)...")
  
  # Exclude journals where Gini is undefined (single-editor boards)
  df <- journal_stats %>%
    dplyr::filter(!is.na(gini_evc), n_editors > 1)
  
  if (nrow(df) < 2) {
    message("Too few journals to classify — returning empty tibble.")
    return(tibble::tibble())
  }
  
  # Thresholds: sample medians across all eligible journals
  evc_threshold  <- median(df$median_evc, na.rm = TRUE)
  gini_threshold <- median(df$gini_evc,   na.rm = TRUE)
  
  message(sprintf(
    "Typology thresholds — Median EVC: %.4f | Gini: %.4f  (n = %d journals)",
    evc_threshold, gini_threshold, nrow(df)
  ))
  
  result <- df %>%
    dplyr::mutate(
      capital_level   = dplyr::if_else(
        median_evc >= evc_threshold, "High capital", "Low capital"
      ),
      inequality_level = dplyr::if_else(
        gini_evc >= gini_threshold, "High inequality", "Low inequality"
      ),
      typology = paste0(capital_level, " / ", inequality_level),
      # Store thresholds as columns so the classification is self-documenting
      evc_threshold_used  = evc_threshold,
      gini_threshold_used = gini_threshold
    ) %>%
    dplyr::select(
      Journal, n_editors,
      median_evc, gini_evc,
      capital_level, inequality_level, typology,
      evc_threshold_used, gini_threshold_used
    ) %>%
    dplyr::arrange(typology, dplyr::desc(median_evc))
  
  # Print summary count per quadrant for quick inspection
  summary_counts <- result %>%
    dplyr::count(typology, name = "n_journals")
  message("Typology distribution:")
  print(summary_counts)
  
  result
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