# R/robustness_checks.R
# Comprehensive robustness and sensitivity analysis - COMPLETE VERSION

#' Threshold Sensitivity Analysis
#' Tests how network properties change with different co-membership thresholds
run_threshold_sweep <- function(data_clean, thresholds = c(1, 2, 3, 4, 5), cfg) {
  message("Running threshold sensitivity analysis...")
  
  results <- purrr::map_dfr(thresholds, function(th) {
    # Build network with this threshold
    networks <- build_networks(data_clean, min_shared_journals = th)
    g_full <- networks$g_full
    g_gc <- networks$g_gc
    
    # Calculate basic properties with error handling
    comp_info <- igraph::components(g_full)
    
    # Calculate EVC if giant component exists and has edges
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
    
    # FIXED: Ensure all variables are the correct type
    tibble::tibble(
      threshold = as.integer(th),  # Force integer type
      n_nodes_full = as.integer(igraph::vcount(g_full)),
      n_edges_full = as.integer(igraph::ecount(g_full)),
      n_nodes_gc = as.integer(igraph::vcount(g_gc)),
      n_edges_gc = as.integer(igraph::ecount(g_gc)),
      gc_share_nodes = as.numeric(max(comp_info$csize) / igraph::vcount(g_full)),
      n_components = as.integer(comp_info$no),
      median_evc = as.numeric(median_evc),
      gini_evc = as.numeric(gini_evc),
      density = as.numeric(igraph::edge_density(g_full))
    )
  })
  
  message(sprintf("Threshold sweep completed for %d thresholds", length(thresholds)))
  results
}

#' Bootstrap Confidence Intervals for Key Metrics
#' Resamples edges to estimate uncertainty in network metrics
run_bootstrap_analysis <- function(g_gc, n_bootstrap = 100, seed = 123) {
  message("Running bootstrap analysis...")
  set.seed(seed)
  
  if (igraph::vcount(g_gc) == 0 || igraph::ecount(g_gc) == 0) {
    message("Empty network - skipping bootstrap")
    return(tibble::tibble(
      metric = c("median_evc", "gini_evc"),
      estimate = c(NA_real_, NA_real_),
      ci_lower = c(NA_real_, NA_real_),
      ci_upper = c(NA_real_, NA_real_)
    ))
  }
  
  # Get original metrics
  tryCatch({
    orig_evc <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
    orig_median <- median(orig_evc, na.rm = TRUE)
    orig_gini <- safe_gini(orig_evc)
  }, error = function(e) {
    message("Error calculating original metrics: ", e$message)
    return(tibble::tibble(
      metric = c("median_evc", "gini_evc"),
      estimate = c(NA_real_, NA_real_),
      ci_lower = c(NA_real_, NA_real_),
      ci_upper = c(NA_real_, NA_real_)
    ))
  })
  
  # Bootstrap storage
  boot_medians <- numeric(n_bootstrap)
  boot_ginis <- numeric(n_bootstrap)
  
  # Get edge list for resampling
  edges <- igraph::as_data_frame(g_gc, "edges")
  vertices <- igraph::as_data_frame(g_gc, "vertices")
  
  for (i in seq_len(n_bootstrap)) {
    # Resample edges with replacement
    boot_edges <- edges[sample(nrow(edges), replace = TRUE), ]
    
    # Create bootstrap graph
    tryCatch({
      g_boot <- igraph::graph_from_data_frame(boot_edges, directed = FALSE, vertices = vertices)
      
      # Calculate EVC for bootstrap sample
      if (igraph::vcount(g_boot) > 0 && igraph::ecount(g_boot) > 0) {
        evc_boot <- igraph::eigen_centrality(g_boot, directed = FALSE, weights = igraph::E(g_boot)$weight)$vector
        boot_medians[i] <- median(evc_boot, na.rm = TRUE)
        boot_ginis[i] <- safe_gini(evc_boot)
      } else {
        boot_medians[i] <- NA
        boot_ginis[i] <- NA
      }
    }, error = function(e) {
      boot_medians[i] <- NA
      boot_ginis[i] <- NA
    })
  }
  
  # Calculate confidence intervals
  tibble::tibble(
    metric = c("median_evc", "gini_evc"),
    estimate = c(orig_median, orig_gini),
    ci_lower = c(
      quantile(boot_medians, 0.025, na.rm = TRUE),
      quantile(boot_ginis, 0.025, na.rm = TRUE)
    ),
    ci_upper = c(
      quantile(boot_medians, 0.975, na.rm = TRUE),
      quantile(boot_ginis, 0.975, na.rm = TRUE)
    )
  )
}

#' Alternative Centrality Measures Correlation
#' Tests robustness by comparing different centrality measures
run_centrality_correlation <- function(g_gc) {
  message("Running centrality correlation analysis...")
  
  if (igraph::vcount(g_gc) == 0 || igraph::ecount(g_gc) == 0) {
    message("Empty network - skipping centrality analysis")
    return(tibble::tibble(
      metric1 = character(0),
      metric2 = character(0),
      correlation = numeric(0),
      p_value = numeric(0)
    ))
  }
  
  # Calculate different centrality measures with error handling
  tryCatch({
    evc <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
    degree_cent <- igraph::degree(g_gc)
    between_cent <- igraph::betweenness(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)
    close_cent <- igraph::closeness(g_gc, weights = igraph::E(g_gc)$weight)
    
    # Create all pairwise correlations
    metrics <- list(
      EVC = evc,
      Degree = degree_cent,
      Betweenness = between_cent,
      Closeness = close_cent
    )
    
    # Calculate correlations
    results <- list()
    metric_names <- names(metrics)
    
    for (i in 1:(length(metrics) - 1)) {
      for (j in (i + 1):length(metrics)) {
        test <- cor.test(metrics[[i]], metrics[[j]], method = "spearman")
        results[[length(results) + 1]] <- tibble::tibble(
          metric1 = metric_names[i],
          metric2 = metric_names[j],
          correlation = as.numeric(test$estimate),
          p_value = as.numeric(test$p.value)
        )
      }
    }
    
    dplyr::bind_rows(results)
    
  }, error = function(e) {
    message("Error in centrality correlation: ", e$message)
    return(tibble::tibble(
      metric1 = character(0),
      metric2 = character(0),
      correlation = numeric(0),
      p_value = numeric(0)
    ))
  })
}

#' Component Inclusion Analysis
#' Tests sensitivity to including only giant component vs. full network
run_component_analysis <- function(g_full, g_gc) {
  message("Running component inclusion analysis...")
  
  if (igraph::vcount(g_full) == 0 || igraph::vcount(g_gc) == 0) {
    message("Empty networks - skipping component analysis")
    return(tibble::tibble(
      network = character(0),
      n_nodes = numeric(0),
      n_edges = numeric(0),
      median_evc = numeric(0),
      gini_evc = numeric(0),
      avg_clustering = numeric(0)
    ))
  }
  
  # Calculate metrics for both networks with error handling
  tryCatch({
    # Full network metrics
    if (igraph::ecount(g_full) > 0) {
      evc_full <- igraph::eigen_centrality(g_full, directed = FALSE, weights = igraph::E(g_full)$weight)$vector
      median_evc_full <- median(evc_full, na.rm = TRUE)
      gini_evc_full <- safe_gini(evc_full)
      clustering_full <- igraph::transitivity(g_full, type = "average")
    } else {
      median_evc_full <- NA_real_
      gini_evc_full <- NA_real_
      clustering_full <- NA_real_
    }
    
    # Giant component metrics
    if (igraph::ecount(g_gc) > 0) {
      evc_gc <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
      median_evc_gc <- median(evc_gc, na.rm = TRUE)
      gini_evc_gc <- safe_gini(evc_gc)
      clustering_gc <- igraph::transitivity(g_gc, type = "average")
    } else {
      median_evc_gc <- NA_real_
      gini_evc_gc <- NA_real_
      clustering_gc <- NA_real_
    }
    
    tibble::tibble(
      network = c("Full", "Giant_Component"),
      n_nodes = c(igraph::vcount(g_full), igraph::vcount(g_gc)),
      n_edges = c(igraph::ecount(g_full), igraph::ecount(g_gc)),
      median_evc = c(median_evc_full, median_evc_gc),
      gini_evc = c(gini_evc_full, gini_evc_gc),
      avg_clustering = c(clustering_full, clustering_gc)
    )
    
  }, error = function(e) {
    message("Error in component analysis: ", e$message)
    return(tibble::tibble(
      network = c("Full", "Giant_Component"),
      n_nodes = c(igraph::vcount(g_full), igraph::vcount(g_gc)),
      n_edges = c(igraph::ecount(g_full), igraph::ecount(g_gc)),
      median_evc = c(NA_real_, NA_real_),
      gini_evc = c(NA_real_, NA_real_),
      avg_clustering = c(NA_real_, NA_real_)
    ))
  })
}

#' Editor-Level Full vs Giant Component EVC Rank Correlation
#' Computes Spearman correlation between Full and Giant Component EVC rankings at editor level
run_full_vs_gc_evc_correlation <- function(g_full, g_gc) {
  message("Running Full vs Giant Component EVC rank correlation analysis...")
  
  if (igraph::vcount(g_full) == 0 || igraph::vcount(g_gc) == 0 || 
      igraph::ecount(g_full) == 0 || igraph::ecount(g_gc) == 0) {
    message("Empty or disconnected networks - skipping EVC correlation")
    return(list(
      paired_data = tibble::tibble(
        editor_id = character(0),
        evc_full = numeric(0),
        evc_gc = numeric(0)
      ),
      correlation_test = tibble::tibble(
        method = character(0),
        rho = numeric(0),
        p_value = numeric(0),
        n_pairs = numeric(0)
      )
    ))
  }
  
  tryCatch({
    # Calculate EVC for full network
    evc_full_values <- igraph::eigen_centrality(g_full, directed = FALSE, weights = igraph::E(g_full)$weight)$vector
    evc_full_df <- tibble::tibble(
      editor_id = names(evc_full_values),
      evc_full = as.numeric(evc_full_values)
    )
    
    # Calculate EVC for giant component  
    evc_gc_values <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
    evc_gc_df <- tibble::tibble(
      editor_id = names(evc_gc_values),
      evc_gc = as.numeric(evc_gc_values)
    )
    
    # Merge the two datasets by editor_id (inner join for editors in both networks)
    paired_data <- evc_full_df %>%
      dplyr::inner_join(evc_gc_df, by = "editor_id") %>%
      dplyr::filter(!is.na(evc_full) & !is.na(evc_gc))
    
    # Compute Spearman rank correlation
    if (nrow(paired_data) >= 3) {  # Need at least 3 observations for meaningful correlation
      cor_test <- cor.test(paired_data$evc_full, paired_data$evc_gc, method = "spearman")
      
      correlation_test <- tibble::tibble(
        method = "spearman",
        rho = as.numeric(cor_test$estimate),
        p_value = as.numeric(cor_test$p.value),
        n_pairs = nrow(paired_data)
      )
      
      message(sprintf("Full vs GC EVC correlation: ρ = %.4f (p = %.4f, n = %d)", 
                      correlation_test$rho, correlation_test$p_value, correlation_test$n_pairs))
      
    } else {
      message("Insufficient overlapping editors for correlation analysis")
      correlation_test <- tibble::tibble(
        method = "spearman",
        rho = NA_real_,
        p_value = NA_real_,
        n_pairs = nrow(paired_data)
      )
    }
    
    list(
      paired_data = paired_data,
      correlation_test = correlation_test
    )
    
  }, error = function(e) {
    message("Error in Full vs GC EVC correlation: ", e$message)
    return(list(
      paired_data = tibble::tibble(
        editor_id = character(0),
        evc_full = numeric(0),
        evc_gc = numeric(0)
      ),
      correlation_test = tibble::tibble(
        method = character(0),
        rho = numeric(0),
        p_value = numeric(0),
        n_pairs = numeric(0)
      )
    ))
  })
}

#' Resolution Parameter Sweep for Community Detection
#' Tests sensitivity to Leiden resolution parameter
run_resolution_sweep <- function(g_gc, resolutions = seq(0.1, 2.0, by = 0.1), seed = 123) {
  message("Running resolution parameter sweep...")
  set.seed(seed)
  
  if (igraph::vcount(g_gc) == 0 || igraph::ecount(g_gc) == 0) {
    message("Empty network - skipping resolution sweep")
    return(tibble::tibble(
      resolution = numeric(0),
      n_communities = numeric(0),
      modularity = numeric(0),
      largest_community_size = numeric(0)
    ))
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
      tibble::tibble(
        resolution = as.numeric(res),
        n_communities = NA_integer_,
        modularity = NA_real_,
        largest_community_size = NA_integer_
      )
    })
  })
  
  results
}

#' Comprehensive Robustness Analysis
#' Runs all robustness checks and saves results
run_comprehensive_robustness <- function(data_clean, g_full, g_gc, cfg, output_dir = "output/robustness") {
  message("Running comprehensive robustness analysis...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Run all robustness checks with better error handling
  threshold_results <- tryCatch({
    run_threshold_sweep(data_clean, thresholds = c(1, 2, 3, 4, 5), cfg)
  }, error = function(e) {
    message("Threshold sweep failed: ", e$message)
    tibble::tibble()
  })
  
  bootstrap_results <- tryCatch({
    run_bootstrap_analysis(g_gc, n_bootstrap = 100)
  }, error = function(e) {
    message("Bootstrap analysis failed: ", e$message)
    tibble::tibble()
  })
  
  centrality_results <- tryCatch({
    run_centrality_correlation(g_gc)
  }, error = function(e) {
    message("Centrality correlation failed: ", e$message)
    tibble::tibble()
  })
  
  component_results <- tryCatch({
    run_component_analysis(g_full, g_gc)
  }, error = function(e) {
    message("Component analysis failed: ", e$message)
    tibble::tibble()
  })
  
  resolution_results <- tryCatch({
    run_resolution_sweep(g_gc)
  }, error = function(e) {
    message("Resolution sweep failed: ", e$message)
    tibble::tibble()
  })
  
  # Full vs Giant Component EVC correlation analysis
  full_vs_gc_results <- tryCatch({
    run_full_vs_gc_evc_correlation(g_full, g_gc)
  }, error = function(e) {
    message("Full vs GC EVC correlation failed: ", e$message)
    list(
      paired_data = tibble::tibble(),
      correlation_test = tibble::tibble()
    )
  })
  
  # Save individual results (only if they have data)
  if (nrow(threshold_results) > 0) {
    readr::write_csv(threshold_results, file.path(output_dir, "threshold_sensitivity.csv"))
  }
  if (nrow(bootstrap_results) > 0) {
    readr::write_csv(bootstrap_results, file.path(output_dir, "bootstrap_confidence.csv"))
  }
  if (nrow(centrality_results) > 0) {
    readr::write_csv(centrality_results, file.path(output_dir, "centrality_correlations.csv"))
  }
  if (nrow(component_results) > 0) {
    readr::write_csv(component_results, file.path(output_dir, "component_comparison.csv"))
  }
  if (nrow(resolution_results) > 0) {
    readr::write_csv(resolution_results, file.path(output_dir, "resolution_sweep.csv"))
  }
  
  # Save Full vs GC EVC correlation results
  if (nrow(full_vs_gc_results$paired_data) > 0) {
    readr::write_csv(full_vs_gc_results$paired_data, file.path(output_dir, "full_vs_gc_evc_paired_data.csv"))
  }
  if (nrow(full_vs_gc_results$correlation_test) > 0) {
    readr::write_csv(full_vs_gc_results$correlation_test, file.path(output_dir, "full_vs_gc_evc_correlation.csv"))
  }
  
  # Create summary visualization
  tryCatch({
    create_robustness_plots(threshold_results, bootstrap_results, centrality_results, 
                            resolution_results, output_dir)
  }, error = function(e) {
    message("Plot creation failed: ", e$message)
  })
  
  # Return combined results
  list(
    threshold = threshold_results,
    bootstrap = bootstrap_results,
    centrality = centrality_results,
    component = component_results,
    resolution = resolution_results,
    full_vs_gc_evc = full_vs_gc_results
  )
}

#' Create Robustness Visualization Dashboard
create_robustness_plots <- function(threshold_results, bootstrap_results, centrality_results, 
                                    resolution_results, output_dir) {
  message("Creating robustness visualization dashboard...")
  
  # Load ggplot2 if not already loaded
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 not available - skipping plots")
    return(invisible(NULL))
  }
  
  library(ggplot2)
  
  # Threshold sensitivity plot
  if (nrow(threshold_results) > 0) {
    tryCatch({
      p_threshold <- ggplot(threshold_results, aes(x = threshold)) +
        geom_line(aes(y = n_nodes_gc), color = "blue") +
        geom_point(aes(y = n_nodes_gc), color = "blue") +
        labs(title = "Network Size vs. Threshold", x = "Min Shared Journals", y = "Nodes in Giant Component") +
        theme_bw()
      
      ggsave(file.path(output_dir, "threshold_sensitivity.png"), p_threshold, width = 8, height = 6, dpi = 300)
    }, error = function(e) {
      message("Threshold plot failed: ", e$message)
    })
  }
  
  # Resolution sweep plot
  if (nrow(resolution_results) > 0 && any(!is.na(resolution_results$modularity))) {
    tryCatch({
      p_resolution <- ggplot(resolution_results, aes(x = resolution)) +
        geom_line(aes(y = modularity), color = "red") +
        geom_point(aes(y = modularity), color = "red") +
        labs(title = "Modularity vs. Resolution", x = "Leiden Resolution", y = "Modularity") +
        theme_bw()
      
      ggsave(file.path(output_dir, "resolution_sweep.png"), p_resolution, width = 8, height = 6, dpi = 300)
    }, error = function(e) {
      message("Resolution plot failed: ", e$message)
    })
  }
  
  # Centrality correlation heatmap
  if (nrow(centrality_results) > 0) {
    tryCatch({
      p_corr <- ggplot(centrality_results, aes(x = metric1, y = metric2, fill = correlation)) +
        geom_tile() +
        geom_text(aes(label = round(correlation, 2)), color = "white") +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(title = "Centrality Measure Correlations", x = "", y = "") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(output_dir, "centrality_correlations.png"), p_corr, width = 8, height = 6, dpi = 300)
    }, error = function(e) {
      message("Correlation plot failed: ", e$message)
    })
  }
  
  message("Robustness plots saved")
}