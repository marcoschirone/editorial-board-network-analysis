# R/network_analysis.R
# Core network analysis functions.

# Run one Leiden partition reproducibly.
#
# The RNG is reset for every call so the same graph/resolution/seed triple
# produces the same partition regardless of what stochastic operations ran
# earlier in the pipeline.  The objective function is explicit everywhere.
run_leiden_once <- function(g, resolution, seed = 123L, objective_function = "CPM") {
  set.seed(as.integer(seed))
  comm <- igraph::cluster_leiden(
    g,
    resolution = resolution,
    objective_function = objective_function,
    weights = igraph::E(g)$weight
  )
  membership <- as.integer(comm$membership)
  list(
    membership = membership,
    modularity = as.numeric(igraph::modularity(
      g, membership = membership, weights = igraph::E(g)$weight
    )),
    n_communities = as.integer(length(unique(membership)))
  )
}

run_leiden_sweep <- function(g, cfg) {
  message("Running deterministic Leiden sweep to find optimal resolution...")

  res_values <- seq(0.5, 2.0, by = 0.1)
  seed <- if (!is.null(cfg$seed_leiden)) cfg$seed_leiden else cfg$seed_layout

  runs <- lapply(res_values, function(res) {
    tryCatch({
      fit <- run_leiden_once(g, resolution = res, seed = seed, objective_function = "CPM")
      list(
        summary = tibble::tibble(
          resolution = as.numeric(res),
          modularity = fit$modularity,
          num_communities = fit$n_communities
        ),
        membership = fit$membership
      )
    }, error = function(e) NULL)
  })
  runs <- Filter(Negate(is.null), runs)

  if (length(runs) == 0) stop("Leiden sweep failed for all resolution values.")

  results <- dplyr::bind_rows(lapply(runs, `[[`, "summary"))

  # Deterministic tie-break: highest modularity, then smallest resolution.
  # Use explicit vectors instead of tidy evaluation here. In some attached
  # package combinations, the bare name `modularity` can resolve to
  # igraph::modularity() inside desc(), producing:
  #   `x` must be a vector, not a function.
  ord <- order(-results$modularity, results$resolution, na.last = TRUE)
  if (length(ord) == 0L || is.na(ord[[1]])) {
    stop("Leiden sweep produced no finite candidate rows.", call. = FALSE)
  }
  best_row <- results[ord[[1]], , drop = FALSE]

  best_resolution <- as.numeric(best_row$resolution[[1]])
  best_index <- which(vapply(runs, function(x) {
    isTRUE(all.equal(as.numeric(x$summary$resolution[[1]]), best_resolution))
  }, logical(1)))[1]
  if (length(best_index) == 0L || is.na(best_index)) {
    stop("Could not match the selected Leiden resolution back to its stored partition.", call. = FALSE)
  }
  best_membership <- runs[[best_index]]$membership

  message(sprintf(
    "Optimal resolution: %.2f (modularity: %.3f; communities: %d)",
    best_row$resolution, best_row$modularity, best_row$num_communities
  ))

  list(
    sweep_results = results,
    recommendation = list(
      resolution = best_row$resolution[[1]],
      modularity = best_row$modularity[[1]],
      num_communities = best_row$num_communities[[1]]
    ),
    optimal_membership = best_membership,
    seed = as.integer(seed),
    objective_function = "CPM"
  )
}

#' Compute the four centrality measures used throughout the pipeline.
#'
#' E(g)$weight is shared-journal (or shared-editor) count, a tie-STRENGTH
#' measure: higher means more connected. eigen_centrality() treats weights as
#' strength (correct, as-is). betweenness() and closeness() instead treat
#' weights as edge DISTANCES for shortest-path computation, where higher means
#' less connected -- so they need the inverse, 1/weight, or a strong tie
#' (many shared journals) would be misread as a long, weak path.
#'
#' Single source of truth for this calculation: calculate_network_metrics()
#' and run_centrality_correlation() (R/robustness_checks.R) both call this
#' rather than each recomputing the four measures themselves.
compute_centrality_measures <- function(g) {
  tibble::tibble(
    EVC         = igraph::eigen_centrality(g, directed = FALSE, weights = igraph::E(g)$weight)$vector,
    degree      = igraph::degree(g),
    betweenness = igraph::betweenness(g, directed = FALSE, weights = 1 / igraph::E(g)$weight),
    closeness   = igraph::closeness(g, weights = 1 / igraph::E(g)$weight)
  )
}

calculate_network_metrics <- function(g_gc_input, cfg, leiden_rec = NULL) {
  message("Calculating network metrics...")
  g_gc <- g_gc_input

  seed <- if (!is.null(cfg$seed_leiden)) cfg$seed_leiden else cfg$seed_layout

  if (!is.null(leiden_rec) && !is.null(leiden_rec$optimal_membership)) {
    membership <- as.integer(leiden_rec$optimal_membership)
    if (length(membership) != igraph::vcount(g_gc)) {
      stop("Stored Leiden membership length does not match giant-component node count.", call. = FALSE)
    }
    resolution <- leiden_rec$recommendation$resolution
    modularity_score <- as.numeric(igraph::modularity(
      g_gc, membership = membership, weights = igraph::E(g_gc)$weight
    ))
  } else {
    fit <- run_leiden_once(
      g_gc, resolution = cfg$leiden_resolution, seed = seed, objective_function = "CPM"
    )
    membership <- fit$membership
    resolution <- cfg$leiden_resolution
    modularity_score <- fit$modularity
  }

  igraph::V(g_gc)$community <- membership

  cm <- compute_centrality_measures(g_gc)
  V(g_gc)$EVC <- cm$EVC
  V(g_gc)$degree <- cm$degree
  V(g_gc)$betweenness <- cm$betweenness
  V(g_gc)$closeness <- cm$closeness

  editor_stats <- igraph::as_data_frame(g_gc, "vertices") %>%
    dplyr::mutate(
      EVC_pct = pct(EVC),
      betweenness_pct = pct(betweenness),
      degree_pct = pct(degree)
    )

  gini_evc <- safe_gini(editor_stats$EVC)
  inequality_measures <- tibble::tibble(measure = "Gini_EVC", value = gini_evc)
  n_communities <- length(unique(membership))
  community_summary <- tibble::tibble(
    resolution = as.numeric(resolution),
    modularity = modularity_score,
    n_communities = as.integer(n_communities),
    seed = as.integer(seed),
    objective_function = "CPM"
  )

  message(sprintf(
    "Median EVC: %.4f | Gini: %.3f | Leiden Q: %.3f | Communities: %d",
    median(editor_stats$EVC, na.rm = TRUE), gini_evc, modularity_score, n_communities
  ))

  list(
    g_gc = g_gc,
    editor_stats = editor_stats,
    inequality_measures = inequality_measures,
    community_summary = community_summary
  )
}

calculate_journal_network_metrics <- function(g_journal, editor_stats, data_clean, cfg) {
  message("Calculating journal-level metrics...")
  
  V(g_journal)$eigenvector <- eigen_centrality(g_journal, weights = E(g_journal)$shared_editors)$vector
  V(g_journal)$degree <- degree(g_journal)
  
  journal_seed <- if (!is.null(cfg$seed_leiden)) cfg$seed_leiden else cfg$seed_layout
  set.seed(as.integer(journal_seed))
  comm_journal <- igraph::cluster_leiden(
    g_journal,
    weights = E(g_journal)$shared_editors,
    resolution = cfg$journal_leiden_resolution,
    objective_function = "CPM"
  )
  V(g_journal)$community <- comm_journal$membership
  
  journal_aggregated_stats <- data_clean %>%
    left_join(editor_stats, by = c("editor_id" = "name")) %>%
    group_by(Journal) %>%
    summarise(
      median_evc     = median(EVC, na.rm = TRUE),
      mean_evc       = mean(EVC, na.rm = TRUE),
      max_evc = ifelse(
        all(is.na(EVC)),
        NA_real_,
        max(EVC, na.rm = TRUE)
      ),
      gini_evc       = safe_gini(EVC),
      # Finite-sample corrected Gini following Deltas (2003): n / (n - 1) * raw Gini..
      # Adjusts for downward bias in small samples.
      # NA for boards with n <= 1 where correction is undefined.
      gini_corrected = safe_gini_corrected(EVC),
      
      # Flag boards where Gini estimates are unreliable due to small n.
      # These are reported separately and interpreted with caution.
      size_flag      = dplyr::n() <= 3,
      .groups        = "drop"
    )
  
  journal_stats <- as_data_frame(g_journal, "vertices") %>%
    left_join(journal_aggregated_stats, by = c("name" = "Journal")) %>%
    rename(Journal = name) %>%
    replace_na(list(
      median_evc     = 0,
      mean_evc       = 0,
      max_evc        = 0,
      gini_evc       = NA_real_,
      gini_corrected = NA_real_,
      size_flag      = FALSE
    ))
  
  list(g_journal = g_journal, journal_stats = journal_stats)
}