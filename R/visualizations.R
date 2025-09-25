# R/visualizations.R
# All visualization and plotting functions for the project.

# A single, consistent theme, using the default font family to avoid errors.
theme_publication <- function(base_size = 12) {
  ggraph::theme_graph(base_size = base_size, base_family = "") +
    theme(
      plot.title = element_text(size = rel(1.2)),
      legend.key.size = unit(0.8, "lines"),
      legend.position = "right",
      legend.box = "vertical"
    )
}

# Helper function to save plots in multiple formats
save_plot <- function(plot, output_dir, filename, width = 10, height = 8, dpi = 300, 
                      formats = c("png", "pdf", "tiff")) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  base_name <- tools::file_path_sans_ext(filename)
  
  for (format in formats) {
    full_path <- file.path(output_dir, paste0(base_name, ".", format))
    ggsave(full_path, plot, width = width, height = height, dpi = dpi)
  }
  message(paste("Saved plot:", base_name, "in", paste(formats, collapse = ", ")))
}

#' Create the multi-panel editor network visualization (Figure 1)
generate_editor_network_panels <- function(g_gc, editor_stats, cfg, output_dir) {
  message("Generating multi-panel editor network visualization for Figure 1...")
  
  V(g_gc)$EVC <- editor_stats$EVC[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$degree <- editor_stats$degree[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$Gender <- editor_stats$Gender[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$Subregion_1 <- editor_stats$Subregion_1[match(V(g_gc)$name, editor_stats$name)]
  
  set.seed(cfg$seed_layout)
  layout <- igraph::layout_with_fr(g_gc)
  
  p_gender <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = EVC, size = degree, shape = Gender)) +
    scale_color_viridis_c(name = "EVC") +
    scale_size_continuous(name = "Degree") +
    scale_shape_manual(name = "Gender", values = c("Male" = 16, "Female" = 17, "Unknown" = 4)) +
    labs(title = "Editor Network by Gender", tag = "a") +
    theme_publication()
  
  V(g_gc)$Subregion_lumped <- forcats::fct_lump_n(V(g_gc)$Subregion_1, n = 5, other_level = "Other")
  unique_subregions <- levels(V(g_gc)$Subregion_lumped)
  shape_values <- setNames(c(16, 17, 15, 3, 8, 4)[1:length(unique_subregions)], unique_subregions)
  
  p_subregion <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = EVC, size = degree, shape = Subregion_lumped)) +
    scale_color_viridis_c(name = "EVC") +
    scale_size_continuous(name = "Degree") +
    scale_shape_manual(name = "Subregion", values = shape_values) +
    labs(title = "Editor Network by Subregion", tag = "b") +
    theme_publication()
  
  combined_plot <- p_gender + p_subregion + 
    plot_layout(guides = 'collect') & theme(legend.position = 'right')
  
  save_plot(combined_plot, output_dir, "figure_1_editor_network_panels", width = 16, height = 8)
  
  message("Figure 1 saved with unified legends and panel labels.")
  return(invisible(TRUE))
}

#' Generate the journal community visualization (Figure 2) with enhanced edge visibility
generate_journal_community_visualization <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating journal community visualization for Figure 2...")
  
  vertex_df <- data.frame(Journal = V(g_journal)$name, stringsAsFactors = FALSE) %>%
    left_join(journal_stats, by = "Journal")
  for (col in names(vertex_df)) {
    if (col != "Journal") g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
  }
  
  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')
  
  # Dynamically create integer breaks for the edge legend
  edge_weights <- E(g_journal)$shared_editors
  legend_breaks <- if (length(edge_weights) > 0) {
    max_weight <- max(edge_weights, na.rm = TRUE)
    floor(unique(pretty(1:max_weight))) %>% .[. >= 1]
  } else { c(1) }
  
  p_journal_comm <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors, alpha = shared_editors), color = "grey50", show.legend = TRUE) +
    geom_node_point(aes(size = n_editors, fill = factor(community)), shape = 21, color = "white", stroke = 1) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5, bg.color = "white", bg.r = 0.1) +
    # Apply integer breaks to the edge scales
    scale_edge_width_continuous(name = "Shared Editors", range = c(0.8, 4), breaks = legend_breaks) +
    scale_edge_alpha_continuous(name = "Shared Editors", range = c(0.4, 1.0), breaks = legend_breaks) +
    scale_size_continuous(name = "# Editors", range = c(4, 15)) +
    scale_fill_brewer(name = "Journal Community", palette = "Set2") +
    labs(
      title = "Journal Network: Community Structure",
      subtitle = "Edges are weighted by the number of shared editors",
      caption = "Node size: # of editors | Edge width/alpha: # of shared editors | Fill: Community"
    ) +
    theme_publication() +
    guides(
      fill = guide_legend(override.aes = list(size = 5)),
      size = guide_legend(override.aes = list(fill = "black")),
      width = guide_legend(title = "Shared Editors"),
      alpha = guide_legend(title = "Shared Editors")
    )
  
  save_plot(p_journal_comm, output_dir, "figure_2_journal_network_communities", width = 14, height = 11)
  
  message("Figure 2 saved with uniform edge color and integer breaks.")
  return(invisible(TRUE))
}

#' Create the multi-panel journal network visualization (Figure 3)
generate_journal_network_panels <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating multi-panel journal network visualization for Figure 3...")
  
  vertex_df <- as_data_frame(g_journal, "vertices") %>% left_join(journal_stats, by = c("name" = "Journal"))
  for (col in names(vertex_df)) {
    if (col != "name") g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
  }
  
  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')
  
  edge_weights <- E(g_journal)$shared_editors
  legend_breaks <- if (length(edge_weights) > 0) {
    max_weight <- max(edge_weights, na.rm = TRUE)
    floor(unique(pretty(1:max_weight))) %>% .[. >= 1]
  } else { c(1) }
  
  p_journal_evc <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.3, color = "grey") +
    geom_node_point(aes(size = n_editors, color = median_evc)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width_continuous(name = "Shared Editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Median Board EVC") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network by Median Editor EVC", tag = "a") +
    theme_publication()
  
  p_journal_gini <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.3, color = "grey") +
    geom_node_point(aes(size = n_editors, color = gini_evc)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_edge_width_continuous(name = "Shared Editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Board Inequality (Gini)", option = "plasma") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network by Board Inequality", tag = "b") +
    theme_publication()
  
  combined_plot <- p_journal_evc + p_journal_gini +
    plot_layout(guides = 'collect') & theme(legend.position = 'right')
  
  save_plot(combined_plot, output_dir, "figure_3_journal_network_panels", width = 18, height = 9)
  
  message("Figure 3 saved with unified legends, panel labels, and integer breaks.")
  return(invisible(TRUE))
}

# Disparity dashboard
create_full_disparity_dashboard <- function(editor_stats, output_dir) {
  message("Creating disparity dashboard...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  axis_label <- "Eigenvector Centrality (EVC)"
  
  p_gender <- editor_stats %>%
    filter(!is.na(Gender) & Gender %in% c("Male", "Female")) %>%
    ggplot(aes(x = Gender, y = EVC, fill = Gender)) +
    geom_violin(alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    labs(title = "Disparity by Gender", x = NULL, y = axis_label) +
    theme_bw(base_family = "") + theme(legend.position = "none")
  
  p_continent <- editor_stats %>%
    filter(!is.na(Continent_1)) %>%
    ggplot(aes(x = reorder(Continent_1, EVC, FUN = median), y = EVC, fill = Continent_1)) +
    geom_boxplot() + coord_flip() +
    labs(title = "Disparity by Continent", x = "", y = axis_label) +
    theme_bw(base_family = "") + theme(legend.position = "none")
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- p_gender + p_continent +
      patchwork::plot_annotation(title = "Disparity Dashboard")
    save_plot(combined_plot, output_dir, "disparity_dashboard_full", width = 12, height = 6)
  }
}