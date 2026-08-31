# R/disparity_analysis.R
# Functions for disparity and supplementary analysis.

analyze_disparities <- function(editor_stats) {
  message("Performing disparity analysis...")
  results <- list()
  
  if (!"EVC" %in% names(editor_stats)) {
    stop("'EVC' column not found in editor_stats.")
  }
  
  # Gender disparities: primary NamSor classification only. Low-confidence
  # cases are excluded symmetrically rather than manually completed here.
  gender_col <- if ("Gender_namsor" %in% names(editor_stats)) "Gender_namsor" else "Gender"
  if (gender_col %in% names(editor_stats)) {
    gender_data <- editor_stats %>%
      filter(.data[[gender_col]] %in% c("Male", "Female")) %>%
      mutate(Gender_primary = .data[[gender_col]])
    if (nrow(gender_data) > 0 && length(unique(gender_data$Gender_primary)) >= 2) {
      test <- wilcox.test(EVC ~ Gender_primary, data = gender_data, exact = FALSE)
      results$gender <- gender_data %>%
        group_by(Gender_primary) %>%
        summarise(n = n(), median_sc = median(EVC, na.rm = TRUE), .groups = "drop") %>%
        rename(Gender = Gender_primary) %>%
        mutate(p_value = test$p.value, classification = "NamSor primary")
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
      prop_female = {
        g <- Gender_namsor[Gender_namsor %in% c("Female", "Male")]
        if (length(g)) mean(g == "Female") else NA_real_
      },
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

classify_journal_typology <- function(journal_stats,
                                      gini_measure = c("corrected", "raw"),
                                      min_editors = 2L) {
  gini_measure <- match.arg(gini_measure)
  min_editors <- as.integer(min_editors)

  message(sprintf(
    "Classifying journals into 2x2 prominence/inequality typology using %s Gini...",
    gini_measure
  ))

  required <- c("Journal", "n_editors", "median_evc", "gini_evc", "gini_corrected")
  missing_cols <- setdiff(required, names(journal_stats))
  if (length(missing_cols) > 0) {
    stop(
      "classify_journal_typology() is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  gini_col <- if (gini_measure == "corrected") "gini_corrected" else "gini_evc"

  # Gini is undefined for single-editor boards. n <= 3 boards remain eligible
  # but retain size_flag in the source journal_metrics table and should be
  # interpreted cautiously in the manuscript.
  df <- journal_stats %>%
    dplyr::filter(
      n_editors >= min_editors,
      !is.na(median_evc),
      !is.na(.data[[gini_col]])
    ) %>%
    dplyr::mutate(
      gini_raw = gini_evc,
      gini_corrected_value = gini_corrected,
      gini_evc = .data[[gini_col]]
    )

  if (nrow(df) < 2) {
    message("Too few journals to classify — returning empty tibble.")
    return(tibble::tibble())
  }

  evc_threshold  <- stats::median(df$median_evc, na.rm = TRUE)
  gini_threshold <- stats::median(df$gini_evc, na.rm = TRUE)

  message(sprintf(
    paste0(
      "Typology thresholds — Median EVC: %.6f | %s Gini: %.6f ",
      "(n = %d eligible journals)"
    ),
    evc_threshold, gini_measure, gini_threshold, nrow(df)
  ))

  result <- df %>%
    dplyr::mutate(
      prominence_level = dplyr::if_else(
        median_evc >= evc_threshold, "High prominence", "Low prominence"
      ),
      inequality_level = dplyr::if_else(
        gini_evc >= gini_threshold, "High inequality", "Low inequality"
      ),
      typology = paste(prominence_level, inequality_level, sep = " / "),
      evc_threshold_used = evc_threshold,
      gini_threshold_used = gini_threshold,
      gini_measure_used = gini_measure,
      eligible_journal_count = nrow(df)
    ) %>%
    dplyr::select(
      Journal, n_editors, median_evc, gini_evc,
      prominence_level, inequality_level, typology,
      evc_threshold_used, gini_threshold_used,
      gini_measure_used, eligible_journal_count,
      gini_raw, gini_corrected_value
    ) %>%
    dplyr::arrange(typology, dplyr::desc(median_evc), Journal)

  summary_counts <- result %>%
    dplyr::count(typology, name = "n_journals") %>%
    dplyr::arrange(typology)

  message("Typology distribution:")
  print(summary_counts)

  attr(result, "summary_counts") <- summary_counts
  result
}

compare_journal_typology_gini <- function(journal_stats, min_editors = 2L) {
  corrected <- classify_journal_typology(
    journal_stats,
    gini_measure = "corrected",
    min_editors = min_editors
  )
  raw <- classify_journal_typology(
    journal_stats,
    gini_measure = "raw",
    min_editors = min_editors
  )

  if (nrow(corrected) == 0 || nrow(raw) == 0) {
    return(tibble::tibble())
  }

  corrected %>%
    dplyr::select(
      Journal,
      corrected_typology = typology,
      corrected_gini = gini_evc,
      corrected_gini_threshold = gini_threshold_used
    ) %>%
    dplyr::left_join(
      raw %>%
        dplyr::select(
          Journal,
          raw_typology = typology,
          raw_gini = gini_evc,
          raw_gini_threshold = gini_threshold_used
        ),
      by = "Journal"
    ) %>%
    dplyr::mutate(changed_quadrant = corrected_typology != raw_typology) %>%
    dplyr::arrange(dplyr::desc(changed_quadrant), Journal)
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