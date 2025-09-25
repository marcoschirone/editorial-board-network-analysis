# R/robustness_checks.R
# Functions for comprehensive robustness and sensitivity analysis.

#' Threshold Sensitivity Analysis
run_threshold_sweep <- function(data_clean, thresholds = c(1, 2, 3, 4, 5), cfg) {
  message("Running threshold sensitivity analysis...")
  
  results <- purrr::map_dfr(thresholds, function(th) {
    networks <- build_networks(data_clean, min_shared_journals = th)
    g_full <- networks$g_full
    g_gc <- networks$g_gc
    
    comp_info <- igraph::components(g_full)
    
    if (igraph::vcount(g_gc) > 1 && igraph::ecount(g_gc) > 0) {
      tryCatch({
        evc <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
        median_evc <- median(evc, na.rm = TRUE)
        gini_evc <- safe_gini(evc)
      }, error = function(e) {
        median_evc <<- NA_real_
        gini_evc <<- NA_real_
      })
    } else {
      median_evc <- NA_real_
      gini_evc <- NA_real_
    }
    
    tibble::tibble(
      threshold = as.integer(th),
      n_nodes_full = as.integer(igraph::vcount(g_full)),
      n_edges_full = as.integer(igraph::ecount(g_full)),
      n_nodes_gc = as.integer(igraph::vcount(g_gc)),
      n_edges_gc = as.integer(igraph::ecount(g_gc)),
      gc_share_nodes = max(comp_info$csize) / igraph::vcount(g_full),
      n_components = as.integer(comp_info$no),
      median_evc = as.numeric(median_evc),
      gini_evc = as.numeric(gini_evc),
      density = igraph::edge_density(g_full)
    )
  })
  
  message(sprintf("Threshold sweep completed for %d thresholds.", length(thresholds)))
  results
}

#' Bootstrap Confidence Intervals for Key Metrics
run_bootstrap_analysis <- function(g_gc, n_bootstrap = 100, seed = 123) {
  message("Running bootstrap analysis...")
  set.seed(seed)
  
  if (igraph::vcount(g_gc) < 2 || igraph::ecount(g_gc) < 1) {
    message("Network too small for bootstrap analysis.")
    return(tibble::tibble())
  }
  
  vertices_df <- igraph::as_data_frame(g_gc, "vertices")
  edges_df <- igraph::as_data_frame(g_gc, "edges")
  
  orig_evc <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
  
  boot_metrics <- purrr::map_dfr(seq_len(n_bootstrap), function(i) {
    boot_edges <- edges_df[sample(1:nrow(edges_df), replace = TRUE), ]
    g_boot <- igraph::graph_from_data_frame(boot_edges, directed = FALSE, vertices = vertices_df)
    
    if (igraph::vcount(g_boot) > 1 && igraph::ecount(g_boot) > 0) {
      tryCatch({
        evc_boot <- igraph::eigen_centrality(g_boot, directed = FALSE, weights = igraph::E(g_boot)$weight)$vector
        tibble::tibble(median_evc = median(evc_boot, na.rm = TRUE), gini_evc = safe_gini(evc_boot))
      }, error = function(e) {
        tibble::tibble(median_evc = NA_real_, gini_evc = NA_real_)
      })
    } else {
      tibble::tibble(median_evc = NA_real_, gini_evc = NA_real_)
    }
  })
  
  tibble::tibble(
    metric = c("median_evc", "gini_evc"),
    estimate = c(median(orig_evc, na.rm = TRUE), safe_gini(orig_evc)),
    ci_lower = c(quantile(boot_metrics$median_evc, 0.025, na.rm = TRUE), quantile(boot_metrics$gini_evc, 0.025, na.rm = TRUE)),
    ci_upper = c(quantile(boot_metrics$median_evc, 0.975, na.rm = TRUE), quantile(boot_metrics$gini_evc, 0.975, na.rm = TRUE))
  )
}

#' Alternative Centrality Measures Correlation
run_centrality_correlation <- function(g_gc) {
  message("Running centrality correlation analysis...")
  
  if (igraph::vcount(g_gc) < 2 || igraph::ecount(g_gc) < 1) {
    message("Network too small for centrality correlation.")
    return(tibble::tibble())
  }
  
  metrics_df <- tibble::tibble(
    EVC = igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector,
    Degree = igraph::degree(g_gc),
    Betweenness = igraph::betweenness(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight),
    Closeness = igraph::closeness(g_gc, weights = igraph::E(g_gc)$weight)
  )
  
  results <- list()
  metric_names <- names(metrics_df)
  for (i in 1:(length(metric_names) - 1)) {
    for (j in (i + 1):length(metric_names)) {
      test <- cor.test(metrics_df[[i]], metrics_df[[j]], method = "spearman")
      results[[length(results) + 1]] <- tibble::tibble(
        metric1 = metric_names[i],
        metric2 = metric_names[j],
        correlation = as.numeric(test$estimate),
        p_value = as.numeric(test$p.value)
      )
    }
  }
  
  dplyr::bind_rows(results)
}

#' Component Inclusion Analysis
run_component_analysis <- function(g_full, g_gc) {
  message("Running component inclusion analysis...")
  
  if (igraph::vcount(g_full) == 0) {
    message("Empty full network - skipping component analysis")
    return(tibble::tibble())
  }
  
  calculate_stats <- function(g, network_name) {
    if (igraph::vcount(g) < 2 || igraph::ecount(g) < 1) {
      return(tibble::tibble(
        network = network_name, n_nodes = igraph::vcount(g), n_edges = igraph::ecount(g),
        median_evc = NA_real_, gini_evc = NA_real_, avg_clustering = NA_real_
      ))
    }
    
    tryCatch({
      evc <- igraph::eigen_centrality(g, directed = FALSE, weights = igraph::E(g)$weight)$vector
      tibble::tibble(
        network = network_name, n_nodes = igraph::vcount(g), n_edges = igraph::ecount(g),
        median_evc = median(evc, na.rm = TRUE),
        gini_evc = safe_gini(evc),
        avg_clustering = igraph::transitivity(g, type = "average")
      )
    }, error = function(e) {
      tibble::tibble(
        network = network_name, n_nodes = igraph::vcount(g), n_edges = igraph::ecount(g),
        median_evc = NA_real_, gini_evc = NA_real_, avg_clustering = NA_real_
      )
    })
  }
  
  full_stats <- calculate_stats(g_full, "Full")
  gc_stats <- calculate_stats(g_gc, "Giant_Component")
  
  dplyr::bind_rows(full_stats, gc_stats)
}

#' Resolution Parameter Sweep for Community Detection
run_resolution_sweep <- function(g_gc, resolutions = seq(0.1, 2.0, by = 0.1), seed = 123) {
  message("Running resolution parameter sweep...")
  set.seed(seed)
  
  if (igraph::vcount(g_gc) < 2 || igraph::ecount(g_gc) < 1) {
    message("Network too small for resolution sweep.")
    return(tibble::tibble())
  }
  
  results <- purrr::map_dfr(resolutions, function(res) {
    tryCatch({
      comm <- igraph::cluster_leiden(g_gc, resolution = res, weights = igraph::E(g_gc)$weight)
      mod_score <- igraph::modularity(g_gc, membership = comm$membership, weights = igraph::E(g_gc)$weight)
      
      tibble::tibble(
        resolution = as.numeric(res),
        n_communities = as.integer(length(unique(comm$membership))),
        modularity = as.numeric(mod_score),
        largest_community_size = as.integer(max(table(comm$membership)))
      )
    }, error = function(e) {
      tibble::tibble()
    })
  })
  
  results
}

#' Comprehensive Robustness Analysis
run_comprehensive_robustness <- function(data_clean, g_full, g_gc, cfg, output_dir) {
  message("--- Running Comprehensive Robustness Analysis ---")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_results <- list(
    threshold_sensitivity = run_threshold_sweep(data_clean, thresholds = c(1, 2, 3, 4, 5), cfg),
    bootstrap_confidence = run_bootstrap_analysis(g_gc, n_bootstrap = 1000),
    centrality_correlations = run_centrality_correlation(g_gc),
    component_comparison = run_component_analysis(g_full, g_gc),
    resolution_sweep = run_resolution_sweep(g_gc)
  )
  
  for (name in names(all_results)) {
    if (nrow(all_results[[name]]) > 0) {
      readr::write_csv(all_results[[name]], file.path(output_dir, paste0(name, ".csv")))
    }
  }
  
  create_robustness_plots(all_results, output_dir)
  
  message("--- Comprehensive Robustness Analysis Complete ---")
  all_results
}

#' Create Robustness Visualization Dashboard
create_robustness_plots <- function(all_results, output_dir) {
  message("Creating robustness visualization dashboard...")
  
  if ("threshold_sensitivity" %in% names(all_results) && nrow(all_results$threshold_sensitivity) > 0) {
    p_threshold <- ggplot(all_results$threshold_sensitivity, aes(x = threshold, y = n_nodes_gc)) +
      geom_line(color = "blue") + geom_point(color = "blue") +
      labs(title = "Network Size vs. Co-membership Threshold", x = "Min Shared Journals", y = "Nodes in Giant Component") +
      theme_bw()
    ggsave(file.path(output_dir, "threshold_sensitivity.png"), p_threshold, width = 8, height = 6, dpi = 300)
  }
  
  if ("resolution_sweep" %in% names(all_results) && nrow(all_results$resolution_sweep) > 0) {
    p_resolution <- ggplot(all_results$resolution_sweep, aes(x = resolution, y = modularity)) +
      geom_line(color = "red") + geom_point(color = "red") +
      labs(title = "Modularity vs. Leiden Resolution", x = "Leiden Resolution Parameter", y = "Modularity Score") +
      theme_bw()
    ggsave(file.path(output_dir, "resolution_sweep.png"), p_resolution, width = 8, height = 6, dpi = 300)
  }
  
  if ("centrality_correlations" %in% names(all_results) && nrow(all_results$centrality_correlations) > 0) {
    p_corr <- ggplot(all_results$centrality_correlations, aes(x = metric1, y = metric2, fill = correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(correlation, 2)), color = "black") +
      scale_fill_gradient2(low = "#3B9AB2", mid = "#EEEEEE", high = "#F21A00", midpoint = 0.5, limit = c(0,1)) +
      labs(title = "Spearman Correlation of Centrality Measures", x = "", y = "", fill = "Rho") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    ggsave(file.path(output_dir, "centrality_correlations.png"), p_corr, width = 8, height = 7, dpi = 300)
  }
  
  message("Robustness plots saved.")
}