# R/functions.R
# Purpose: Define all core functions for the network analysis pipeline.

# =================== UTILS ===================

clean_ids <- function(x) {
  x %>% as.character() %>%
    stringr::str_replace_all("\\p{Cf}", "") %>%
    stringr::str_squish() %>%
    stringr::str_trim()
}

edge_pairs <- function(v) {
  v <- v[!is.na(v) & v != ""]
  if (length(v) < 2) return(tibble(e1 = character(), e2 = character()))
  m <- combn(v, 2)
  tibble(e1 = m[1, ], e2 = m[2, ])
}

pct <- function(x) {
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}

# =================== PIPELINE FUNCTIONS ===================

# ---- 1. Data Preprocessing ----
load_and_clean_data <- function(cfg) {
  message("  -> Loading and preprocessing data...")
  data_raw <- readxl::read_excel(cfg$file_path)
  data_clean <- data_raw %>%
    rename(
      ORCID = !!sym(cfg$col_orcid), Journal = !!sym(cfg$col_journal),
      Country = !!sym(cfg$col_country), Continent = !!sym(cfg$col_continent),
      Subregion = !!sym(cfg$col_subregion), Gender = !!sym(cfg$col_gender)
    ) %>%
    mutate(across(where(is.character), str_squish), anon_id = clean_ids(ORCID)) %>%
    filter(!is.na(anon_id) & !is.na(Journal))
  message(sprintf("     Loaded %d valid editorial records.", nrow(data_clean)))
  return(data_clean)
}

# ---- 2. Network Building ----
build_networks <- function(data_clean, min_shared_journals) {
  message("  -> Building editor co-membership network...")
  nodes_df <- data_clean %>%
    group_by(anon_id) %>%
    summarise(
      Gender_raw = first(Gender),
      Gender = case_when(
        grepl("^m", Gender_raw, ignore.case = TRUE) ~ "Male",
        grepl("^f", Gender_raw, ignore.case = TRUE) ~ "Female",
        TRUE ~ "Unknown"
      ),
      Continent_1 = first(Continent),
      Country_1 = first(Country),
      Subregion_1 = first(Subregion),
      .groups = "drop"
    ) %>%
    select(-Gender_raw)
  edges_df <- data_clean %>%
    group_by(Journal) %>%
    reframe(edge_pairs(unique(anon_id))) %>%
    count(e1, e2, name = "weight") %>%
    filter(weight >= min_shared_journals)
  g_full <- igraph::graph_from_data_frame(d = edges_df, directed = FALSE, vertices = nodes_df)
  comp_info <- igraph::components(g_full)
  gc_nodes <- which(comp_info$membership == which.max(comp_info$csize))
  g_gc <- igraph::induced_subgraph(g_full, gc_nodes)
  message(sprintf("     Giant component: %d editors, %d links.", vcount(g_gc), ecount(g_gc)))
  return(list(g_full = g_full, g_gc = g_gc))
}

build_journal_network <- function(data_clean, min_shared_editors = 1) {
  message("  -> Building journal-journal network...")
  journal_edges <- data_clean %>%
    group_by(anon_id) %>%
    reframe(edge_pairs(unique(Journal))) %>%
    count(e1, e2, name = "shared_editors") %>%
    filter(shared_editors >= min_shared_editors)
  journal_nodes <- data_clean %>%
    distinct(Journal)
  g_journal <- igraph::graph_from_data_frame(d = journal_edges, directed = FALSE, vertices = journal_nodes)
  message(sprintf("     Journal network: %d journals, %d links.", vcount(g_journal), ecount(g_journal)))
  return(g_journal)
}


# ---- 3. Community Detection Sweep ----
run_leiden_sweep <- function(g, cfg) {
  message("  -> Running Leiden sweep to find optimal resolution...")
  res_values <- seq(0.5, 2.0, by = 0.1)
  results <- purrr::map_dfr(res_values, function(res) {
    tryCatch({
      comm <- igraph::cluster_leiden(g, resolution = res, objective_function = "CPM", weights = E(g)$weight)
      mod_score <- igraph::modularity(g, membership = comm$membership, weights = E(g)$weight)
      tibble::tibble(resolution = res, modularity = mod_score, num_communities = length(comm))
    }, error = function(e) {
      warning(sprintf("Failed at resolution = %.2f. Skipping. Error: %s", res, e$message))
      tibble::tibble()
    })
  })
  if (nrow(results) == 0) { stop("Leiden sweep failed for all resolution values. Cannot proceed.") }
  optimal <- results %>% dplyr::slice_max(modularity, n = 1, with_ties = FALSE)
  return(list(sweep_results = results, recommendation = list(resolution = optimal$resolution)))
}


# ---- 4. Metrics Calculation ----
calculate_network_metrics <- function(g_gc, cfg) {
  message("  -> Calculating symbolic capital and other network metrics...")
  
  comm <- igraph::cluster_leiden(g_gc, resolution = cfg$leiden_resolution, weights = igraph::E(g_gc)$weight)
  igraph::V(g_gc)$community <- comm$membership
  
  igraph::V(g_gc)$eigenvector  <- igraph::eigen_centrality(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)$vector
  igraph::V(g_gc)$degree       <- igraph::degree(g_gc)
  igraph::V(g_gc)$betweenness  <- igraph::betweenness(g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight)
  
  editor_stats <- igraph::as_data_frame(g_gc, "vertices") %>%
    dplyr::mutate(
      eigenvector_pct = pct(eigenvector),
      betweenness_pct = pct(betweenness),
      degree_pct      = pct(degree)
    )
  
  gini_eigen  <- ineq::ineq(editor_stats$eigenvector, type = "Gini", na.rm = TRUE)
  inequality_measures <- tibble::tibble(measure = "Gini", value = gini_eigen)
  
  message(sprintf("     Mean symbolic capital: %.4f", mean(editor_stats$eigenvector, na.rm = TRUE)))
  message(sprintf("     Inequality — Gini: %.3f", gini_eigen))
  
  return(list(
    g_gc = g_gc,
    editor_stats = editor_stats,
    inequality_measures = inequality_measures
  ))
}

calculate_journal_network_metrics <- function(g_journal, editor_stats, data_clean, cfg) {
  message("  -> Calculating journal-level metrics...")
  V(g_journal)$eigenvector <- eigen_centrality(g_journal, weights = E(g_journal)$shared_editors)$vector
  V(g_journal)$degree <- degree(g_journal)
  comm_journal <- igraph::cluster_leiden(g_journal, weights = E(g_journal)$shared_editors, resolution = cfg$journal_leiden_resolution)
  V(g_journal)$community <- comm_journal$membership
  message(sprintf("     Detected %d journal communities using resolution %.2f", length(unique(comm_journal$membership)), cfg$journal_leiden_resolution))
  
  journal_aggregated_stats <- data_clean %>%
    left_join(editor_stats, by = c("anon_id" = "name")) %>%
    group_by(Journal) %>%
    summarise(
      mean_symbolic_capital = mean(eigenvector, na.rm = TRUE),
      n_editors = n(),
      gini_symbolic_capital = if(n() > 1) ineq::Gini(eigenvector, na.rm = TRUE) else 0,
      .groups = "drop"
    )
  
  journal_stats <- as_data_frame(g_journal, "vertices") %>%
    left_join(journal_aggregated_stats, by = c("name" = "Journal")) %>%
    rename(Journal = name)
  
  return(list(g_journal = g_journal, journal_stats = journal_stats))
}

# ---- 5. Visualizations ----
generate_visualizations <- function(g_gc, cfg, output_dir) {
  message("  -> Generating main editor network visualizations...")
  set.seed(cfg$seed_layout)
  layout <- igraph::layout_with_fr(g_gc)
  
  # Plot 1: Symbolic Capital Distribution
  p_sc <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = eigenvector, size = degree)) +
    scale_color_viridis_c(name = "Symbolic Capital\n(Eigenvector Centrality)") +
    labs(title = "Editor Network: Distribution of Symbolic Capital (Eigenvector Centrality)") +
    theme_graph()
  ggsave(file.path(output_dir, "network_symbolic_capital.png"), p_sc, width = 10, height = 8, dpi = 300)
  
  # Plot 2: Community Structure
  p_comm <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = factor(community)), size = 3) +
    labs(title = "Editor Network: Community Structure") +
    theme_graph() + theme(legend.position = "none")
  ggsave(file.path(output_dir, "network_communities.png"), p_comm, width = 10, height = 8, dpi = 300)
  
  message("     Core network plots saved.")
  
  # Plot 3: Shape by Gender
  V(g_gc)$Gender <- tidyr::replace_na(V(g_gc)$Gender, "Unknown")
  p_gender_shape <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = eigenvector, size = degree, shape = Gender), na.rm = TRUE) +
    scale_color_viridis_c(name = "Symbolic Capital\n(Eigenvector Centrality)") +
    scale_shape_manual(name = "Gender", values = c("Male" = 16, "Female" = 17, "Unknown" = 15), na.translate = TRUE) +
    labs(title = "Editor Network: Symbolic Capital by Gender")
  ggsave(file.path(output_dir, "network_shape_gender.png"), p_gender_shape, width = 10, height = 8, dpi = 300)
  
  # Plot 4: Shape by Continent
  V(g_gc)$Continent_lumped <- forcats::fct_lump_n(V(g_gc)$Continent_1, n = 5, other_level = "Other")
  p_continent_shape <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = eigenvector, size = degree, shape = Continent_lumped)) +
    scale_color_viridis_c(name = "Symbolic Capital\n(Eigenvector Centrality)") +
    scale_shape_discrete(name = "Continent") +
    labs(title = "Editor Network: Symbolic Capital by Continent")
  ggsave(file.path(output_dir, "network_shape_continent.png"), p_continent_shape, width = 10, height = 8, dpi = 300)
  
  # Plot 5: Shape by Subregion
  V(g_gc)$Subregion_lumped <- forcats::fct_lump_n(V(g_gc)$Subregion_1, n = 5, other_level = "Other")
  p_subregion_shape <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = eigenvector, size = degree, shape = Subregion_lumped)) +
    scale_color_viridis_c(name = "Symbolic Capital\n(Eigenvector Centrality)") +
    scale_shape_discrete(name = "Subregion") +
    labs(title = "Editor Network: Symbolic Capital by Subregion")
  ggsave(file.path(output_dir, "network_shape_subregion.png"), p_subregion_shape, width = 10, height = 8, dpi = 300)
  
  message("     New plots with shapes and updated labels saved.")
}

generate_journal_visualizations <- function(g_journal, journal_stats, cfg, output_dir) {
  message("  -> Generating journal network visualizations...")
  vertex_df <- dplyr::left_join(data.frame(Journal = V(g_journal)$name), journal_stats, by = "Journal")
  for (col in names(vertex_df)) {
    if (col != "Journal") { g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]]) }
  }
  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')
  
  p_journal_mean_sc <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.2, color = "grey") +
    geom_node_point(aes(size = n_editors, color = mean_symbolic_capital)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
    scale_color_viridis_c(name = "Mean Board SC") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network: Connections by Shared Editors") +
    theme_graph()
  ggsave(file.path(output_dir, "journal_network_mean_sc.png"), p_journal_mean_sc, width = 12, height = 10, dpi = 300)
  message("     Journal network plot (by Mean SC) saved.")
  
  p_journal_gini <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.2, color = "grey") +
    geom_node_point(aes(size = n_editors, color = gini_symbolic_capital)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
    scale_color_viridis_c(name = "Board Inequality (Gini)") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network: Board Inequality (Gini Coefficient)") +
    theme_graph()
  ggsave(file.path(output_dir, "journal_network_gini.png"), p_journal_gini, width = 12, height = 10, dpi = 300)
  message("     Journal network plot (by Gini) saved.")
}

generate_journal_community_visualization <- function(g_journal, journal_stats, cfg, output_dir) {
  message("  -> Generating journal community visualization...")
  vertex_df <- dplyr::left_join(data.frame(Journal = V(g_journal)$name), journal_stats, by = "Journal")
  for (col in names(vertex_df)) {
    if (col != "Journal") { g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]]) }
  }
  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')
  p_journal_comm <- ggraph(layout) +
    geom_edge_link(alpha = 0.2, color = "grey") +
    geom_node_point(aes(size = n_editors, color = factor(community))) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    scale_size_continuous(name = "# Editors") +
    scale_color_discrete(name = "Journal Community") +
    labs(title = "Journal Network: Community Structure") +
    theme_graph() +
    guides(color = guide_legend(override.aes = list(size=5)))
  ggsave(file.path(output_dir, "journal_network_communities.png"), p_journal_comm, width = 14, height = 12, dpi = 300)
  message("     Journal community plot saved.")
}

# ---- 6. Quality Checks & Summary ----
perform_quality_checks <- function(metrics, networks) {
  message("  -> Performing quality checks...")
  missing_gender <- sum(is.na(metrics$editor_stats$Gender))
  message(sprintf("     Missing gender data: %d (%.1f%%)",
                  missing_gender, 100 * missing_gender / nrow(metrics$editor_stats)))
  components_info <- igraph::components(networks$g_full)
  message(sprintf("     Giant component contains %.1f%% of all nodes.",
                  100 * max(components_info$csize) / igraph::vcount(networks$g_full)))
}

print_final_summary <- function(metrics, journal_stats) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("   ANALYSIS SUMMARY\n")
  cat(rep("=", 60), "\n\n", sep = "")
  cat(sprintf("Network size: %d editors, %d connections\n",
              igraph::vcount(metrics$g_gc), igraph::ecount(metrics$g_gc)))
  cat(sprintf("Communities detected: %d\n", length(unique(V(metrics$g_gc)$community))))
  cat(sprintf("Mean symbolic capital (Eigenvector): %.4f\n", mean(metrics$editor_stats$eigenvector, na.rm = TRUE)))
  cat(sprintf("Overall Inequality (Gini Coefficient): %.3f\n", metrics$inequality_measures$value[metrics$inequality_measures$measure == "Gini"]))
  cat(sprintf("Journals analyzed: %d\n", nrow(journal_stats)))
  cat("\nOutput Location: ./output/\n")
  cat(rep("=", 60), "\n", sep = "")
}

# ---- 7. Exports ----
export_results <- function(final_results, output_dir) {
  message("  -> Exporting all results...")
  
  write_csv(final_results$metrics$editor_stats, file.path(output_dir, "editor_metrics.csv"))
  write_csv(final_results$journal_metrics$journal_stats, file.path(output_dir, "journal_metrics.csv"))
  
  if (!is.null(final_results$metrics$inequality_measures)) {
    write_csv(final_results$metrics$inequality_measures, file.path(output_dir, "inequality_measures.csv"))
  }
  if (!is.null(final_results$disparity_results$gender)) {
    write_csv(final_results$disparity_results$gender, file.path(output_dir, "gender_disparities.csv"))
  }
  if (!is.null(final_results$disparity_results$geographic)) {
    write_csv(final_results$disparity_results$geographic, file.path(output_dir, "geographic_disparities.csv"))
  }
  
  saveRDS(final_results, file.path(output_dir, "full_analysis_results.rds"))
  message("     All results exported successfully.")
}