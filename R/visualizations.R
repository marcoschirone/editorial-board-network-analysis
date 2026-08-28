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

# Helper function to save plots in multiple formats.
# FIX 1: dpi raised from 300 to 600 to meet Wiley line art requirement.
save_plot <- function(plot, output_dir, filename, width = 10, height = 8, dpi = 600,
                      formats = c("png", "pdf", "tiff")) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  base_name <- tools::file_path_sans_ext(filename)

  for (format in formats) {
    full_path <- file.path(output_dir, paste0(base_name, ".", format))
    ggsave(full_path, plot, width = width, height = height, dpi = dpi)
  }
  message(paste("Saved plot:", base_name, "in", paste(formats, collapse = ", ")))
}

#' Generate Figure 1: Editor network by gender
generate_gender_network <- function(g_gc, editor_stats, cfg, output_dir) {
  message("Generating Figure 1: editor network by gender...")

  V(g_gc)$EVC    <- editor_stats$EVC[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$degree <- editor_stats$degree[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$Gender <- editor_stats$Gender_namsor[match(V(g_gc)$name, editor_stats$name)]

  set.seed(cfg$seed_layout)
  layout <- igraph::layout_with_fr(g_gc)

  # Shapes: Male = circle (16), Female = triangle (17), Unknown = filled square (15).
  # Circle (16) and triangle (17) are reserved for this figure only.
  # range = c(2, 14) makes degree-based size differences visually apparent.
  # FIX 2: caption removed from figure file per Wiley guidelines;
  #         full caption belongs in the manuscript after references.
  p_gender <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = EVC, size = degree, shape = Gender)) +
    scale_color_viridis_c(name = "EVC") +
    scale_size_continuous(name = "Degree", range = c(2, 14)) +
    scale_shape_manual(
      name   = "Gender",
      values = c("Male" = 16, "Female" = 17, "Low confidence" = 15)
    ) +
    labs(title = NULL, caption = NULL) +
    theme_publication()

  # FIX 3: file renamed to Wiley convention (Figure_N)
  save_plot(p_gender, output_dir, "Figure_1", width = 10, height = 8)
  message("Figure 1 (gender) saved.")
  return(invisible(TRUE))
}

#' Generate Figure 2: Editor network by geographic subregion
generate_subregion_network <- function(g_gc, editor_stats, cfg, output_dir) {
  message("Generating Figure 2: editor network by subregion...")

  V(g_gc)$EVC         <- editor_stats$EVC[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$degree      <- editor_stats$degree[match(V(g_gc)$name, editor_stats$name)]
  V(g_gc)$Subregion_1 <- editor_stats$Subregion_1[match(V(g_gc)$name, editor_stats$name)]

  set.seed(cfg$seed_layout)
  layout <- igraph::layout_with_fr(g_gc)

  # Shapes: diamond (18), plus (3), asterisk (8), cross (4), inverted triangle (25), square-X (7).
  # Circle (16) and triangle (17) deliberately excluded to avoid overlap with Figure 1.
  # range = c(2, 14) makes degree-based size differences visually apparent.
  V(g_gc)$Subregion_lumped <- forcats::fct_lump_n(V(g_gc)$Subregion_1, n = 5, other_level = "Other")
  unique_subregions <- levels(V(g_gc)$Subregion_lumped)
  shape_values <- setNames(
    c(18, 3, 8, 4, 25, 7)[1:length(unique_subregions)],
    unique_subregions
  )

  # FIX 2: caption removed from figure file per Wiley guidelines.
  p_subregion <- ggraph(g_gc, layout = layout) +
    geom_edge_fan(aes(alpha = after_stat(index)), color = "lightgrey", show.legend = FALSE) +
    geom_node_point(aes(color = EVC, size = degree, shape = Subregion_lumped)) +
    scale_color_viridis_c(name = "EVC") +
    scale_size_continuous(name = "Degree", range = c(2, 14)) +
    scale_shape_manual(name = "Subregion", values = shape_values) +
    labs(title = NULL, caption = NULL) +
    theme_publication()

  # FIX 3: file renamed to Wiley convention (Figure_N)
  save_plot(p_subregion, output_dir, "Figure_2", width = 10, height = 8)
  message("Figure 2 (subregion) saved.")
  return(invisible(TRUE))
}

#' Generate Figure 3: Journal network community structure
generate_journal_community_visualization <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating Figure 3: journal community visualization...")

  vertex_df <- data.frame(Journal = V(g_journal)$name, stringsAsFactors = FALSE) %>%
    left_join(journal_stats, by = "Journal")
  for (col in names(vertex_df)) {
    if (col != "Journal") g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
  }

  set.seed(cfg$seed_layout)
  layout <- create_layout(g_journal, layout = 'fr')

  edge_weights <- E(g_journal)$shared_editors
  legend_breaks <- if (length(edge_weights) > 0) {
    max_weight <- max(edge_weights, na.rm = TRUE)
    floor(unique(pretty(1:max_weight))) %>% .[. >= 1]
  } else { c(1) }

  # FIX 4: improved label repulsion and explicit size breaks.
  # FIX 2: title/caption removed from figure file per Wiley guidelines.
  p_journal_comm <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors, alpha = shared_editors),
                   color = "grey50", show.legend = TRUE) +
    geom_node_point(aes(size = n_editors, fill = factor(community)),
                    shape = 21, color = "white", stroke = 1) +
    geom_node_text(
      aes(label = name),
      repel         = TRUE,
      size          = 3.5,
      max.overlaps  = 20,
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.size  = 0.2,
      segment.alpha = 0.5,
      bg.color      = "white",
      bg.r          = 0.1
    ) +
    scale_edge_width_continuous(name = "Shared Editors",
                                range = c(0.8, 4), breaks = legend_breaks) +
    scale_edge_alpha_continuous(name = "Shared Editors",
                                range = c(0.4, 1.0), breaks = legend_breaks) +
    scale_size_continuous(name = "# Editors", range = c(4, 15),
                          breaks = c(5, 10, 15, 20, 25), limits = c(1, 29)) +
    # Dynamic palette: handles any number of communities without warning.
    # colorRampPalette extends RColorBrewer's "Paired" (12 colours max) if needed.
    scale_fill_manual(
      name   = "Journal Community",
      values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(
                 length(unique(factor(V(g_journal)$community)))
               )
    ) +
    labs(title = NULL, caption = NULL) +
    theme_publication() +
    guides(
      fill  = guide_legend(override.aes = list(size = 5)),
      size  = guide_legend(override.aes = list(fill = "black")),
      width = guide_legend(title = "Shared Editors"),
      alpha = guide_legend(title = "Shared Editors")
    )

  # FIX 3: file renamed to Wiley convention (Figure_N)
  save_plot(p_journal_comm, output_dir, "Figure_3", width = 14, height = 11)
  message("Figure 3 (journal communities) saved.")
  return(invisible(TRUE))
}

#' Generate Figure 4: Journal network panels (median EVC and Gini)
generate_journal_network_panels <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating Figure 4: journal network panels...")

  vertex_df <- as_data_frame(g_journal, "vertices") %>%
    left_join(journal_stats, by = c("name" = "Journal"))
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

  # FIX 4: improved label repulsion and explicit size breaks.
  # FIX 2: embedded panel captions removed per Wiley guidelines;
  #         panel tags (a/b) retained for identification.
  p_journal_evc <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.3, color = "grey") +
    geom_node_point(aes(size = n_editors, color = median_evc)) +
    geom_node_text(
      aes(label = name),
      repel         = TRUE,
      size          = 3,
      max.overlaps  = 20,
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.size  = 0.2,
      segment.alpha = 0.5
    ) +
    scale_edge_width_continuous(name = "Shared Editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Median Board EVC") +
    scale_size_continuous(name = "# Editors", range = c(3, 12),
                          breaks = c(5, 10, 15, 20, 25), limits = c(1, 29)) +
    labs(title = NULL, tag = "a", caption = NULL) +
    theme_publication()

  p_journal_gini <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.3, color = "grey") +
    geom_node_point(aes(size = n_editors, color = gini_evc)) +
    geom_node_text(
      aes(label = name),
      repel         = TRUE,
      size          = 3,
      max.overlaps  = 20,
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.size  = 0.2,
      segment.alpha = 0.5
    ) +
    scale_edge_width_continuous(name = "Shared Editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Board Inequality (Gini)", option = "plasma") +
    scale_size_continuous(name = "# Editors", range = c(3, 12),
                          breaks = c(5, 10, 15, 20, 25), limits = c(1, 29)) +
    labs(title = NULL, tag = "b", caption = NULL) +
    theme_publication()

  combined_plot <- p_journal_evc + p_journal_gini +
    plot_layout(guides = 'collect') & theme(legend.position = 'right')

  # FIX 3: file renamed to Wiley convention (Figure_N)
  save_plot(combined_plot, output_dir, "Figure_4", width = 18, height = 9)
  message("Figure 4 (journal network panels) saved.")
  return(invisible(TRUE))
}

# Disparity dashboard (supplementary — not a main manuscript figure)
create_full_disparity_dashboard <- function(editor_stats, output_dir) {
  message("Creating disparity dashboard...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  axis_label <- "Eigenvector Centrality (EVC)"

  gender_col <- if ("Gender_namsor" %in% names(editor_stats)) "Gender_namsor" else "Gender"
  p_gender <- editor_stats %>%
    filter(.data[[gender_col]] %in% c("Male", "Female")) %>%
    mutate(Gender_primary = .data[[gender_col]]) %>%
    ggplot(aes(x = Gender_primary, y = EVC, fill = Gender_primary)) +
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
