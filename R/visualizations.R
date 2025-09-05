# R/visualizations.R
# All visualization and plotting functions

theme_publication <- function(base_size = 11, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2)),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.key.size = unit(0.8, "lines")
    )
}

# Editor network visualizations
generate_visualizations <- function(g_gc, cfg, output_dir) {
  message("Generating editor network visualizations...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  set.seed(cfg$seed_layout)
  layout <- igraph::layout_with_fr(g_gc)
  
  # EVC Distribution Plot
  p_evc <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = EVC, size = degree)) +
    scale_color_viridis_c(name = "EVC") +
    scale_size_continuous(name = "Degree") +
    labs(title = "Editor Network: EVC Distribution") +
    theme_graph()
  ggsave(file.path(output_dir, "network_evc_distribution.png"), p_evc, width = 10, height = 8, dpi = 300)
  
  # Community Structure Plot
  p_comm <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = factor(community)), size = 3) +
    labs(title = "Editor Network: Community Structure") +
    theme_graph() + 
    theme(legend.position = "none")
  ggsave(file.path(output_dir, "network_communities.png"), p_comm, width = 10, height = 8, dpi = 300)
  
  # Gender visualization (if available)
  if ("Gender" %in% vertex_attr_names(g_gc)) {
    p_gender <- ggraph(g_gc, layout = layout) +
      geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
      geom_node_point(aes(color = EVC, size = degree, shape = Gender)) +
      scale_color_viridis_c(name = "EVC") +
      scale_shape_manual(name = "Gender", values = c("Male" = 16, "Female" = 17, "Unknown" = 15)) +
      labs(title = "Editor Network: EVC by Gender") +
      theme_graph()
    ggsave(file.path(output_dir, "network_gender.png"), p_gender, width = 10, height = 8, dpi = 300)
  }
  
  # Subregion visualization (if available)
  if ("Subregion_1" %in% vertex_attr_names(g_gc)) {
    # Create lumped subregion factor to avoid too many categories
    V(g_gc)$Subregion_lumped <- forcats::fct_lump_n(V(g_gc)$Subregion_1, n = 6, other_level = "Other")
    
    p_subregion <- ggraph(g_gc, layout = layout) +
      geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
      geom_node_point(aes(color = EVC, size = degree, shape = Subregion_lumped)) +
      scale_color_viridis_c(name = "EVC") +
      scale_shape_manual(
        name = "Subregion", 
        values = c("Eastern Asia" = 16, "Northern America" = 17, "Northern Europe" = 15, 
                   "Southern Europe" = 3, "Western Europe" = 8, "Other" = 4)
      ) +
      labs(title = "Editor Network: EVC by Subregion") +
      theme_graph()
    ggsave(file.path(output_dir, "network_subregion.png"), p_subregion, width = 10, height = 8, dpi = 300)
  }
  
  # Continent visualization (if available)
  if ("Continent_1" %in% vertex_attr_names(g_gc)) {
    V(g_gc)$Continent_lumped <- forcats::fct_lump_n(V(g_gc)$Continent_1, n = 5, other_level = "Other")
    
    p_continent <- ggraph(g_gc, layout = layout) +
      geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
      geom_node_point(aes(color = EVC, size = degree, shape = Continent_lumped)) +
      scale_color_viridis_c(name = "EVC") +
      scale_shape_manual(
        name = "Continent", 
        values = c("Europe" = 16, "Asia" = 17, "North America" = 15, "South America" = 3, "Africa" = 8, "Other" = 4)
      ) +
      labs(title = "Editor Network: EVC by Continent") +
      theme_graph()
    ggsave(file.path(output_dir, "network_continent.png"), p_continent, width = 10, height = 8, dpi = 300)
  }
  
  message("Editor network plots saved")
}

# Journal network visualizations
generate_journal_visualizations <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating journal network visualizations...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Add journal stats as vertex attributes
  vertex_df <- as_data_frame(g_journal, "vertices") %>% 
    left_join(journal_stats, by = c("name" = "Journal"))
  
  for (col in names(vertex_df)) {
    if (col != "name") {
      g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
    }
  }
  
  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')
  
  # Median EVC Journal Plot
  p_journal_evc <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.2, color = "grey") +
    geom_node_point(aes(size = n_editors, color = median_evc)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
    scale_color_viridis_c(name = "Median Board EVC") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network by Median Editor EVC") +
    theme_graph()
  ggsave(file.path(output_dir, "journal_network_median_evc.png"), p_journal_evc, width = 12, height = 10, dpi = 300)
  
  # Gini inequality plot
  p_journal_gini <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.2, color = "grey") +
    geom_node_point(aes(size = n_editors, color = gini_evc)) +
    geom_node_text(aes(label = name), repel = TRUE, size = 2.5) +
    scale_color_viridis_c(name = "Board Inequality (Gini)") +
    scale_size_continuous(name = "# Editors") +
    labs(title = "Journal Network: Board Inequality") +
    theme_graph()
  ggsave(file.path(output_dir, "journal_network_gini.png"), p_journal_gini, width = 12, height = 10, dpi = 300)
  
  message("Journal network plots saved")
}

# Journal community visualization
generate_journal_community_visualization <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating journal community visualization...")
  
  # Add stats as vertex attributes
  vertex_df <- data.frame(Journal = V(g_journal)$name, stringsAsFactors = FALSE) %>%
    left_join(journal_stats, by = "Journal")
  
  for (col in names(vertex_df)) {
    if (col != "Journal") {
      g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
    }
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
    guides(color = guide_legend(override.aes = list(size = 5)))
  
  ggsave(file.path(output_dir, "journal_network_communities.png"), p_journal_comm, width = 14, height = 12, dpi = 300)
  message("Journal community plot saved")
}

# Disparity dashboard
create_full_disparity_dashboard <- function(editor_stats, output_dir) {
  message("Creating disparity dashboard...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  axis_label <- "Eigenvector Centrality (EVC)"
  
  # Plot A: Gender Disparities
  p_gender <- editor_stats %>%
    filter(!is.na(Gender) & Gender %in% c("Male", "Female")) %>%
    ggplot(aes(x = Gender, y = EVC, fill = Gender)) +
    geom_violin(alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    labs(title = "Disparity by Gender", x = NULL, y = axis_label) +
    theme_publication() +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_gender.png"), p_gender, width = 8, height = 6, dpi = 300)
  
  # Plot B: Geographic Disparities (Continent)
  p_continent <- editor_stats %>%
    filter(!is.na(Continent_1)) %>%
    ggplot(aes(x = reorder(Continent_1, EVC, FUN = median), y = EVC, fill = Continent_1)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "Disparity by Continent", x = "", y = axis_label) +
    theme_publication() +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_continent.png"), p_continent, width = 8, height = 6, dpi = 300)
  
  # Plot C: Geographic Disparities (Subregion)
  p_subregion <- editor_stats %>%
    filter(!is.na(Subregion_1)) %>%
    mutate(Subregion_lumped = forcats::fct_lump_n(Subregion_1, n = 7, other_level = "Other")) %>%
    ggplot(aes(x = reorder(Subregion_lumped, EVC, FUN = median), y = EVC, fill = Subregion_lumped)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "Disparity by Subregion", x = "", y = axis_label) +
    theme_publication() +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_subregion.png"), p_subregion, width = 8, height = 6, dpi = 300)
  
  # Plot D: Geographic Disparities (Country)
  p_country <- editor_stats %>%
    filter(!is.na(Country_1)) %>%
    mutate(Country_lumped = forcats::fct_lump_n(Country_1, n = 10, other_level = "Other")) %>%
    ggplot(aes(x = reorder(Country_lumped, EVC, FUN = median), y = EVC, fill = Country_lumped)) +
    geom_boxplot() +
    coord_flip() +
    labs(title = "Disparity by Country", x = "", y = axis_label) +
    theme_publication() +
    theme(legend.position = "none")
  ggsave(file.path(output_dir, "disparity_country.png"), p_country, width = 8, height = 6, dpi = 300)
  
  # Combined dashboard using patchwork
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- (p_gender | p_continent) / (p_subregion | p_country) +
      patchwork::plot_annotation(title = "Comprehensive Disparity Dashboard")
    ggsave(file.path(output_dir, "disparity_dashboard_full.png"), combined_plot, width = 14, height = 10, dpi = 300)
  }
  
  message("Disparity plots saved")
}