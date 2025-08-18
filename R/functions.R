# R/functions.R
# Purpose: Definitive script with all analysis and visualization functions.

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

statistical_mode <- function(x) {
  ux <- na.omit(unique(x))
  if (!length(ux)) return(NA_character_)
  ux[which.max(tabulate(match(x, ux)))]
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

# =================== VISUALIZATION & EXPORT HELPERS ===================

save_plot_formats <- function(plot_object, base_filepath, width, height) {
  ggsave(paste0(base_filepath, ".png"), plot_object, width = width, height = height, dpi = 300, bg = "white")
  ggsave(paste0(base_filepath, ".pdf"), plot_object, width = width, height = height, device = cairo_pdf)
  ggsave(paste0(base_filepath, ".tiff"), plot_object, width = width, height = height, dpi = 300)
}

# =================== PIPELINE FUNCTIONS ===================

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
  gc_members <- components(g_full)$membership
  g_gc <- induced_subgraph(g_full, vids = which(gc_members == which.max(tabulate(gc_members))))
  
  message(paste0("GC has ", vcount(g_gc), " nodes and ", ecount(g_gc), " edges."))
  return(list(g_full = g_full, g_gc = g_gc))
}

calculate_network_metrics <- function(g_gc, cfg) {
  message("Step 3: Calculating network metrics...")
  
  cl <- cluster_leiden(g_gc, resolution = cfg$leiden_resolution, weights = E(g_gc)$weight)
  V(g_gc)$community <- as.character(cl$membership)
  
  V(g_gc)$strength <- strength(g_gc)
  V(g_gc)$constraint <- constraint(g_gc)
  V(g_gc)$inv_constraint <- 1 / (V(g_gc)$constraint + 1e-6)
  V(g_gc)$katz <- alpha_centrality(g_gc)
  
  editor_stats <- as_data_frame(g_gc, "vertices")
  
  return(list(g_gc = g_gc, editor_stats = editor_stats))
}

# ---- Visualizations ----
generate_visualizations <- function(g_gc, cfg, output_dir) {
  message("Step 4: Generating editor visualizations...")
  set.seed(cfg$seed_layout)
  
  V(g_gc)$Subregion_lumped <- fct_lump_n(V(g_gc)$Subregion_1, n = 5, other_level = "Other")
  
  layout <- create_layout(g_gc, layout = "fr")
  
  base_plot <- ggraph(layout) +
    geom_edge_link(aes(alpha = weight), color = "grey60", show.legend = FALSE) +
    scale_edge_alpha_continuous(range = c(0.1, 0.5)) +
    theme_graph(base_family = "sans")
  
  shape_scale_gender <- scale_shape_manual(name = "Gender", values = c("Male" = 15, "Female" = 16), na.value = 4)
  size_scale_strength <- scale_size_continuous(name = "Strength", range = c(2, 10))
  
  # --- PLOT 1: Community Structure ---
  p_communities <- base_plot +
    geom_node_point(aes(color = community, size = strength, shape = Gender), alpha = 0.8) +
    scale_color_brewer(name = "Community", palette = "Set2") + 
    shape_scale_gender + size_scale_strength +
    guides(color = guide_legend(override.aes = list(size=5, shape=15))) +
    labs(title = "Editor Network: Community Structure")
  save_plot_formats(p_communities, file.path(output_dir, "network_editors_community"), 12, 10)
  
  # --- PLOT 2: Subregion Plot ---
  p_subregion <- base_plot +
    geom_node_point(aes(color = katz, size = strength, shape = Subregion_lumped), alpha = 0.8) +
    scale_color_viridis_c(name = "Katz Centrality") +
    scale_shape_manual(name = "Subregion (Top 5)", values = c(15:18, 8, 4)) +
    size_scale_strength +
    guides(shape = guide_legend(override.aes = list(size=5))) +
    labs(title = "Editor Network: Katz Centrality by Subregion")
  save_plot_formats(p_subregion, file.path(output_dir, "network_editors_subregion"), 12, 10)
  
  message("Editor visualizations saved.")
}

# ---- Journal Network and Visualization Functions ----
build_journal_network <- function(data_clean, editor_stats) {
  message("Building journal network...")
  
  journal_nodes <- editor_stats %>%
    select(name, katz) %>%
    left_join(data_clean, by = c("name" = "anon_id")) %>%
    group_by(Journal) %>%
    summarise(
      mean_katz = mean(katz, na.rm = TRUE),
      n_editors = n()
    ) %>%
    filter(!is.na(Journal))
  
  journal_edges <- data_clean %>%
    group_by(anon_id) %>%
    filter(n_distinct(Journal) > 1) %>%
    reframe(edge_pairs(unique(Journal))) %>%
    ungroup() %>%
    count(e1, e2, name = "shared_editors")
  
  g_journal <- build_graph_safely(edges_df = journal_edges, nodes_df = journal_nodes, from = "e1", to = "e2", vid = "Journal")
  
  return(list(g_journal = g_journal, journal_stats = journal_nodes))
}

generate_journal_visualizations <- function(g_journal, journal_assignments, output_dir, cfg) {
  message("Generating journal network visualizations...")
  set.seed(cfg$seed_layout)
  
  V(g_journal)$community <- journal_assignments$community[match(V(g_journal)$name, journal_assignments$Journal)]
  V(g_journal)$label <- V(g_journal)$name
  
  layout_journal <- create_layout(g_journal, layout = "fr")
  
  base_journal_plot <- ggraph(layout_journal) +
    geom_edge_link(aes(alpha = shared_editors), color = "grey60", show.legend = FALSE) +
    scale_edge_alpha_continuous(range = c(0.1, 0.6)) +
    theme_graph(base_family = "sans")
  
  size_scale_journals <- scale_size_continuous(name = "# of Editors", range = c(3, 12))
  
  # --- PLOT 1: Journal Network by Community ---
  p_journal_comm <- base_journal_plot +
    geom_node_point(aes(color = community, size = n_editors), alpha = 0.8) +
    geom_node_text(aes(label = label), repel = TRUE, size = 3, max.overlaps = 20) +
    scale_color_brewer(name = "Community", palette = "Set2", na.translate = FALSE) + 
    size_scale_journals +
    guides(color = guide_legend(override.aes = list(size=5, shape=16))) +
    labs(title = "Journal Network: Community Structure")
  save_plot_formats(p_journal_comm, file.path(output_dir, "network_journals_community"), 14, 11)
  
  message("Journal visualizations saved.")
}

# ---- Exports ----
export_results <- function(results, output_dir) {
  message("Step 5: Exporting all results...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  write_csv(results$metrics$editor_stats, file.path(output_dir, "editor_stats_gc.csv"))
  if (!is.null(results$journal_stats)) {
    write_csv(results$journal_stats, file.path(output_dir, "journal_summary_stats.csv"))
  }
  
  saveRDS(results, file.path(output_dir, "full_analysis_results.rds"))
  
  message(paste("All results exported to:", normalizePath(output_dir)))
}