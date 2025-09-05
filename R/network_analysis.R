# R/network_analysis.R
# Core network analysis functions

run_leiden_sweep <- function(g, cfg) {
  message("Running Leiden sweep to find optimal resolution...")

  res_values <- seq(0.5, 2.0, by = 0.1)
  results <- purrr::map_dfr(res_values, function(res) {
    tryCatch({
      comm <- igraph::cluster_leiden(g, resolution = res, objective_function = "CPM", weights = E(g)$weight)
      mod_score <- igraph::modularity(g, membership = comm$membership, weights = E(g)$weight)
      tibble::tibble(
        resolution = res,
        modularity = mod_score,
        num_communities = length(unique(comm$membership))
      )
    }, error = function(e) {
      tibble::tibble()
    })
  })

  if (nrow(results) == 0) stop("Leiden sweep failed for all resolution values")

  optimal <- results %>% dplyr::slice_max(modularity, n = 1, with_ties = FALSE)
  message(sprintf("Optimal resolution: %.2f (modularity: %.3f)", optimal$resolution, optimal$modularity))

  list(sweep_results = results, recommendation = list(resolution = optimal$resolution))
}

calculate_network_metrics <- function(g_gc_input, cfg) {
  message("Calculating network metrics...")

  g_gc <- g_gc_input

  set.seed(cfg$seed_layout)
  comm <- igraph::cluster_leiden(g_gc, resolution = cfg$leiden_resolution, weights = igraph::E(g_gc)$weight)
  igraph::V(g_gc)$community <- comm$membership

  V(g_gc)$EVC <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = E(g_gc)$weight)$vector
  V(g_gc)$degree <- igraph::degree(g_gc)
  V(g_gc)$betweenness <- igraph::betweenness(g_gc, directed = FALSE, weights = E(g_gc)$weight)

  editor_stats <- igraph::as_data_frame(g_gc, "vertices") %>%
    dplyr::mutate(
      EVC_pct = pct(EVC),
      betweenness_pct = pct(betweenness),
      degree_pct = pct(degree)
    )

  gini_evc <- safe_gini(editor_stats$EVC)
  inequality_measures <- tibble::tibble(measure = "Gini_EVC", value = gini_evc)

  message(sprintf("Median EVC: %.4f | Gini: %.3f | Communities: %d",
                  median(editor_stats$EVC, na.rm = TRUE), gini_evc, length(unique(comm$membership))))

  list(g_gc = g_gc, editor_stats = editor_stats, inequality_measures = inequality_measures)
}

calculate_journal_network_metrics <- function(g_journal, editor_stats, data_clean, cfg) {
  message("Calculating journal-level metrics...")

  V(g_journal)$eigenvector <- eigen_centrality(g_journal, weights = E(g_journal)$shared_editors)$vector
  V(g_journal)$degree <- degree(g_journal)

  set.seed(cfg$seed_layout)
  comm_journal <- igraph::cluster_leiden(g_journal, weights = E(g_journal)$shared_editors, resolution = cfg$journal_leiden_resolution)
  V(g_journal)$community <- comm_journal$membership

  journal_aggregated_stats <- data_clean %>%
    left_join(editor_stats, by = c("editor_id" = "name")) %>%
    group_by(Journal) %>%
    summarise(
      median_evc = median(EVC, na.rm = TRUE),
      mean_evc = mean(EVC, na.rm = TRUE),
      gini_evc = safe_gini(EVC),
      .groups = "drop"
    )

  journal_stats <- as_data_frame(g_journal, "vertices") %>%
    left_join(journal_aggregated_stats, by = c("name" = "Journal")) %>%
    rename(Journal = name) %>%
    replace_na(list(median_evc = 0, mean_evc = 0, gini_evc = 0))

  list(g_journal = g_journal, journal_stats = journal_stats)
}
