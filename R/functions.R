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

build_graph_safely <- function(edges_df, nodes_df, from = "e1", to = "e2",
                               vid = "anon_id", weight_col = "weight",
                               directed = FALSE) {
  nodes_df <- nodes_df %>%
    mutate(!!vid := clean_ids(.data[[vid]])) %>%
    filter(!is.na(.data[[vid]]), .data[[vid]] != "") %>%
    distinct(.data[[vid]], .keep_all = TRUE)
  
  edges_df <- edges_df %>%
    mutate(!!from := clean_ids(.data[[from]]), !!to := clean_ids(.data[[to]])) %>%
    filter(.data[[from]] %in% nodes_df[[vid]], .data[[to]] %in% nodes_df[[vid]])
  
  g <- graph_from_data_frame(
    d = edges_df,
    directed = directed,
    vertices = nodes_df %>% rename(name = !!vid)
  )
  message(sprintf("Graph built: %d nodes, %d edges.", vcount(g), ecount(g)))
  g
}

safe_rescale <- function(x, to = c(0, 1)) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
    return(rep(mean(to), length(x)))
  }
  scales::rescale(x, to = to, from = rng)
}

pct <- function(x) {
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}


# =================== PIPELINE FUNCTIONS ===================

# ---- 1. Data Preprocessing ----
load_and_clean_data <- function(cfg) {
  message("Step 1: Loading and preprocessing data...")
  req_cols <- with(cfg, c(col_orcid, col_journal, col_country, col_continent, col_gender))
  data_raw <- readxl::read_excel(cfg$file_path, sheet = cfg$sheet_name %||% 1)
  
  if (!cfg$col_subregion %in% names(data_raw)) {
    warning("Column '", cfg$col_subregion, "' not in file. Filling with 'Unknown'.")
    data_raw[[cfg$col_subregion]] <- "Unknown"
  }
  
  data_clean <- data_raw %>%
    rename(
      ORCID = !!sym(cfg$col_orcid), Journal = !!sym(cfg$col_journal),
      Country_1 = !!sym(cfg$col_country), Continent_1 = !!sym(cfg$col_continent),
      Subregion_1 = !!sym(cfg$col_subregion), Gender = !!sym(cfg$col_gender)
    ) %>%
    mutate(across(where(is.character), str_squish), anon_id = clean_ids(ORCID)) %>%
    filter(!is.na(anon_id) & !is.na(Journal))
  
  stopifnot("Data is empty after cleaning. Check ORCID column." = nrow(data_clean) > 0)
  return(data_clean)
}

# ---- 2. Network Building ----
build_networks <- function(data_clean, min_shared_journals) {
  message("Step 2: Building networks...")
  nodes_df <- data_clean %>%
    mutate(Gender = case_when(
      str_detect(Gender, regex("^m(ale)?$", ignore_case = TRUE)) ~ "Male",
      str_detect(Gender, regex("^f(emale)?$", ignore_case = TRUE)) ~ "Female",
      TRUE ~ NA_character_
    )) %>%
    distinct(anon_id, Gender, Country_1, Continent_1, Subregion_1)
  
  edges_df <- data_clean %>%
    group_by(Journal) %>%
    reframe(edge_pairs(unique(anon_id))) %>%
    count(e1, e2, name = "weight") %>%
    filter(weight >= min_shared_journals)
  
  g_full <- build_graph_safely(edges_df, nodes_df)
  
  # Extract Giant Component
  gc_members <- components(g_full)$membership
  g_gc <- induced_subgraph(g_full, vids = which(gc_members == which.max(tabulate(gc_members))))
  
  message(paste0(
    "Full network: ", vcount(g_full), " nodes, ", ecount(g_full), " edges. ",
    "GC has ", vcount(g_gc), " nodes and ", ecount(g_gc), " edges."
  ))
  
  return(list(g_full = g_full, g_gc = g_gc))
}

# ---- 2b. Leiden Resolution Sweep ----
run_leiden_sweep <- function(g_gc, cfg) {
  message("Step 2b: Running Leiden resolution sweep...")
  run_one_leiden <- function(res, g, iters) {
    igraph::cluster_leiden(
      g, weights = E(g)$weight, resolution = res,
      n_iterations = iters, objective_function = "modularity"
    )
  }
  
  # Parallel setup
  if (cfg$use_parallel) {
    n_cores_to_use <- min(cfg$n_cores, parallel::detectCores() - 1)
    plan(multisession, workers = n_cores_to_use)
    message(paste("Using", n_cores_to_use, "cores for parallel processing."))
  } else {
    plan(sequential)
  }
  
  sweep_plan <- tidyr::crossing(resolution = cfg$res_sweep, repetition = 1:cfg$sweep_repeats)
  
  # ** UPDATED THIS LINE TO FIX PARALLEL PROCESSING ERROR **
  sweep_results_raw <- sweep_plan %>%
    mutate(run = future_map(
      resolution, 
      ~run_one_leiden(., g = g_gc, iters = cfg$leiden_iterations), 
      .options = furrr_options(seed = TRUE, globals = c("g_gc", "cfg", "run_one_leiden"))
    ))
  
  plan(sequential) # Close parallel workers
  
  # Process results
  res_results <- sweep_results_raw %>%
    mutate(
      modularity = map_dbl(run, ~if (is.null(.x$modularity)) NA_real_ else .x$modularity),
      membership = map(run, ~as.integer(membership(.)))
    ) %>%
    select(-run) %>%
    filter(!is.na(modularity)) %>%
    group_by(resolution) %>%
    summarise(
      across(modularity, list(mean = mean, sd = sd), .names = "{.col}_{.fn}"),
      n_communities = mean(map_int(membership, ~length(unique(.)))),
      ari_mean = if (n() > 1) {
        mean(combn(membership, 2, function(x) igraph::compare(x[[1]], x[[2]], method = "adjusted.rand"), simplify = TRUE))
      } else {NA_real_},
      .groups = "drop"
    )
  
  recommendation <- res_results %>%
    filter(!is.na(ari_mean)) %>% 
    mutate(
      mod_norm = safe_rescale(modularity_mean),
      ari_norm = safe_rescale(ari_mean),
      score = cfg$sweep_weight_mod * mod_norm + cfg$sweep_weight_ari * ari_norm
    ) %>%
    slice_max(score, n = 1, with_ties = FALSE)
  
  if (nrow(recommendation) == 0) {
    warning("Could not determine a recommended resolution from sweep. Falling back to default value.", call. = FALSE)
    recommendation <- tibble(resolution = cfg$leiden_resolution)
  }
  
  message(paste0("Recommended resolution: ", recommendation$resolution))
  return(list(sweep_results = res_results, recommendation = recommendation))
}


# ---- 3. Network Metrics Calculation ----
calculate_network_metrics <- function(g_gc, cfg) {
  message("Step 3: Calculating network metrics...")
  # Community detection
  cl <- cluster_leiden(g_gc, resolution = cfg$leiden_resolution, weights = E(g_gc)$weight)
  V(g_gc)$community <- as.character(cl$membership)
  
  # Centrality metrics
  V(g_gc)$degree <- degree(g_gc)
  V(g_gc)$strength <- strength(g_gc)
  V(g_gc)$betweenness <- betweenness(g_gc)
  V(g_gc)$closeness <- closeness(g_gc)
  V(g_gc)$pagerank <- page_rank(g_gc)$vector
  V(g_gc)$eigenvector <- eigen_centrality(g_gc)$vector
  V(g_gc)$constraint <- constraint(g_gc)
  V(g_gc)$inv_constraint <- 1 / (V(g_gc)$constraint + 1e-6)
  
  # Calculate SCI
  measures <- c("degree", "strength", "betweenness", "closeness", "pagerank", "eigenvector", "inv_constraint")
  editor_stats <- as_data_frame(g_gc, "vertices") %>%
    mutate(across(all_of(measures), list(pct = pct), .names = "{.col}_pct")) %>%
    rowwise() %>%
    mutate(sci_index = mean(c_across(ends_with("_pct")), na.rm = TRUE)) %>%
    ungroup()
  
  # Community stats
  community_stats <- editor_stats %>%
    group_by(community) %>%
    summarise(
      n_editors = n(),
      n_male = sum(Gender == "Male", na.rm = TRUE),
      avg_sci = mean(sci_index, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Assortativity and Mixing Matrices
  assort_gender <- assortativity_nominal(g_gc, as.factor(V(g_gc)$Gender))
  assort_continent <- assortativity_nominal(g_gc, as.factor(V(g_gc)$Continent_1))
  
  el <- as_edgelist(g_gc, names = FALSE)
  v_gender <- V(g_gc)$Gender
  mixing_gender <- as.data.frame.matrix(table(from = v_gender[el[,1]], to = v_gender[el[,2]])) %>%
    tibble::as_tibble(rownames = "group1")
  
  v_continent <- V(g_gc)$Continent_1
  mixing_continent <- as.data.frame.matrix(table(from = v_continent[el[,1]], to = v_continent[el[,2]])) %>%
    tibble::as_tibble(rownames = "group1")
  
  # Add all key stats back to the graph object for visualization
  all_stats <- editor_stats %>% select(name, sci_index)
  V(g_gc)$sci_index <- all_stats$sci_index[match(V(g_gc)$name, all_stats$name)]
  
  return(list(
    g_gc = g_gc,
    editor_stats = editor_stats,
    community_stats = community_stats,
    assortativity = tibble(attr = c("gender", "continent"), value = c(assort_gender, assort_continent)),
    mixing_matrices = list(gender = mixing_gender, continent = mixing_continent)
  ))
}

# ---- 4. Visualizations ----
generate_visualizations <- function(g_gc, cfg, output_dir) {
  message("Step 4: Generating editor visualizations...")
  set.seed(cfg$seed_layout)
  
  # Prepare data for plotting: lump infrequent subregions into "Other"
  subregion_lumped <- fct_lump_n(V(g_gc)$Subregion_1, n = 5, other_level = "Other")
  V(g_gc)$Subregion_lumped <- subregion_lumped
  
  layout <- create_layout(g_gc, layout = "fr")
  
  # Base plot components
  base_plot <- ggraph(layout) +
    geom_edge_link(aes(alpha = weight), color = "grey70", show.legend = FALSE) +
    theme_graph()
  
  shape_scale_gender <- scale_shape_manual(
    name = "Gender",
    values = c("Male" = 15, "Female" = 16),
    na.value = 4
  )
  
  # --- PLOT 1: SCI with Gender Shapes ---
  p_sci_gender <- base_plot +
    geom_node_point(aes(color = sci_index, size = strength, shape = Gender), alpha = 0.8) +
    scale_color_viridis_c(name = "SCI") +
    shape_scale_gender
  ggsave(file.path(output_dir, "network_sci_gender_shape.png"), p_sci_gender, width = 11, height = 9)
  
  # --- PLOT 2: SCI with Subregion Shapes ---
  p_sci_subregion <- base_plot +
    geom_node_point(aes(color = sci_index, size = strength, shape = Subregion_lumped), alpha = 0.8) +
    scale_color_viridis_c(name = "SCI") +
    scale_shape_discrete(name = "Subregion (Top 5)")
  ggsave(file.path(output_dir, "network_sci_subregion_shape.png"), p_sci_subregion, width = 11, height = 9)
  
  # --- PLOT 3: Individual Centrality Plots ---
  message("Generating individual centrality plots...")
  centrality_measures <- c("degree", "strength", "betweenness", "closeness", "pagerank", "eigenvector")
  
  for (measure in centrality_measures) {
    p <- base_plot +
      geom_node_point(aes(color = .data[[measure]], size = strength, shape = Gender), alpha = 0.8) +
      scale_color_viridis_c(name = str_to_title(measure)) +
      shape_scale_gender +
      labs(title = paste("Network colored by", str_to_title(measure)))
    
    filename <- file.path(output_dir, paste0("network_", measure, "_gender.png"))
    ggsave(filename, p, width = 11, height = 9)
  }
  
  message("Editor visualizations saved.")
}

# ---- 5. Exports ----
export_results <- function(results, output_dir) {
  message("Step 5: Exporting all results...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # CSV Exports
  write_csv(results$metrics$editor_stats, file.path(output_dir, "editor_stats_gc.csv"))
  write_csv(results$metrics$community_stats, file.path(output_dir, "community_stats_gc.csv"))
  
  # Excel Export
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Editor_Stats")
  openxlsx::addWorksheet(wb, "Community_Stats")
  openxlsx::addWorksheet(wb, "Assortativity")
  openxlsx::addWorksheet(wb, "Mixing_Gender")
  openxlsx::writeData(wb, "Editor_Stats", results$metrics$editor_stats)
  openxlsx::writeData(wb, "Community_Stats", results$metrics$community_stats)
  openxlsx::writeData(wb, "Assortativity", results$metrics$assortativity)
  openxlsx::writeData(wb, "Mixing_Gender", results$metrics$mixing_matrices$gender)
  openxlsx::saveWorkbook(wb, file.path(output_dir, "network_analysis_results.xlsx"), overwrite = TRUE)
  
  # RDS bundle
  saveRDS(results, file.path(output_dir, "full_analysis_results.rds"))
  
  message(paste("All results exported to:", normalizePath(output_dir)))
}


# =================== NEW JOURNAL NETWORK FUNCTIONS ===================

# ---- Build Journal Network ----
build_journal_network <- function(data_clean, editor_stats) {
  message("Building journal network...")
  
  # 1. Calculate node attributes for journals (mean SCI, number of editors)
  journal_nodes <- data_clean %>%
    left_join(editor_stats, by = c("anon_id" = "name")) %>%
    group_by(Journal) %>%
    summarise(
      mean_sci = mean(sci_index, na.rm = TRUE),
      n_editors = n()
    ) %>%
    filter(!is.na(Journal))
  
  # 2. Create edges between journals based on shared editors
  journal_edges <- data_clean %>%
    group_by(anon_id) %>%
    filter(n_distinct(Journal) > 1) %>%
    reframe(edge_pairs(unique(Journal))) %>%
    ungroup() %>%
    count(e1, e2, name = "shared_editors")
  
  # 3. Build the graph
  g_journal <- build_graph_safely(
    edges_df = journal_edges,
    nodes_df = journal_nodes,
    from = "e1", to = "e2",
    vid = "Journal"
  )
  
  return(g_journal)
}

# ---- Generate Journal Visualization ----
generate_journal_visualizations <- function(g_journal, output_dir, cfg) {
  message("Generating journal network visualization...")
  set.seed(cfg$seed_layout)
  
  layout_journal <- create_layout(g_journal, layout = "fr")
  
  p_journal <- ggraph(layout_journal) +
    geom_edge_link(aes(alpha = shared_editors), show.legend = FALSE) +
    geom_node_point(aes(color = mean_sci, size = n_editors)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 15) +
    scale_color_viridis_c(name = "Mean Editor SCI") +
    scale_size_continuous(name = "# of Editors") +
    labs(title = "Network of Journals Based on Shared Editors") +
    theme_graph()
  
  ggsave(file.path(output_dir, "network_journals.png"), p_journal, width = 14, height = 11)
  message("Journal network visualization saved.")
}