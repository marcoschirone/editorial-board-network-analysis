# R/visualizations.R
# All visualization and plotting functions for the project.

# Publication theme used across manuscript figures.
# The defaults are intentionally restrained: figures carry no embedded titles,
# typography is consistent across panels, and legends are compact enough for
# journal page layouts.
theme_publication <- function(base_size = 12, legend_position = "right") {
  ggraph::theme_graph(base_size = base_size, base_family = "") +
    theme(
      plot.title = element_text(size = rel(1.08), face = "bold"),
      plot.tag = element_text(size = rel(1.0), face = "bold"),
      legend.key.height = unit(0.72, "lines"),
      legend.key.width = unit(0.92, "lines"),
      legend.position = legend_position,
      legend.box = "vertical",
      legend.title = element_text(size = rel(0.92)),
      legend.text = element_text(size = rel(0.86)),
      plot.margin = margin(8, 8, 8, 8)
    )
}

# Compute one deterministic Fruchterman-Reingold layout and normalize it to a
# common plotting window. Reusing this helper prevents accidental layout drift
# between reruns or between figures that represent the same graph.
make_fixed_fr_layout <- function(graph, seed, niter = 2000L) {
  set.seed(seed)
  xy <- igraph::layout_with_fr(graph, niter = niter)
  if (nrow(xy) > 1L) {
    xy[, 1] <- scales::rescale(xy[, 1], to = c(-1, 1))
    xy[, 2] <- scales::rescale(xy[, 2], to = c(-1, 1))
  }
  ggraph::create_layout(graph, layout = "manual", x = xy[, 1], y = xy[, 2])
}

# Wrap long journal labels for plotting only. The exact journal names stored in
# the graph and exported analytical tables are never changed.
wrap_journal_labels <- function(x, width = 27L) {
  stringr::str_wrap(x, width = width)
}

# Helper function to save plots in multiple formats.
# FIX 1: dpi raised from 300 to 600 to meet Wiley line art requirement.
save_plot <- function(plot, output_dir, filename, width = 10, height = 8, dpi = 600,
                      formats = c("png", "pdf", "tiff")) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  base_name <- tools::file_path_sans_ext(filename)

  for (format in formats) {
    full_path <- file.path(output_dir, paste0(base_name, ".", format))
    if (format == "tiff") {
      ggsave(
        full_path, plot, width = width, height = height, dpi = dpi,
        bg = "white", device = "tiff", compression = "lzw"
      )
    } else {
      ggsave(full_path, plot, width = width, height = height, dpi = dpi, bg = "white")
    }
  }
  message(paste("Saved plot:", base_name, "in", paste(formats, collapse = ", ")))
}

#' Prepare descriptive composition groups for Figure 3
#'
#' Figure 3 compares the full analytical population with the interlocking subset.
#' This is deliberately descriptive: inferential selection tests still compare
#' interlocking and non-interlocking editors in the Results. The full-population
#' bars provide the most intuitive visual baseline for the article's composition
#' narrative and match the descriptive percentages reported in the manuscript.
prepare_composition_data <- function(population_data, gender_metadata) {
  if (is.null(population_data$person)) {
    stop("`population_data` must be the object returned by build_person_level().", call. = FALSE)
  }
  if (is.null(gender_metadata)) {
    stop("`gender_metadata` is required for the instrument-consistent gender panel.", call. = FALSE)
  }

  person <- population_data$person %>%
    dplyr::select(person_id, interlocking, Continent)

  continent_levels <- c("Europe", "Asia", "Americas", "Oceania", "Africa")

  continent_all <- person %>%
    dplyr::filter(Continent %in% continent_levels) %>%
    dplyr::count(Continent, name = "n") %>%
    dplyr::mutate(Comparison_group = "All editors")

  continent_interlocking <- person %>%
    dplyr::filter(interlocking, Continent %in% continent_levels) %>%
    dplyr::count(Continent, name = "n") %>%
    dplyr::mutate(Comparison_group = "Interlocking editors")

  continent_data <- dplyr::bind_rows(continent_all, continent_interlocking) %>%
    tidyr::complete(
      Comparison_group = c("All editors", "Interlocking editors"),
      Continent = continent_levels,
      fill = list(n = 0L)
    ) %>%
    dplyr::group_by(Comparison_group) %>%
    dplyr::mutate(total = sum(n), share = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      variable = "Continent",
      category = factor(Continent, levels = continent_levels),
      Comparison_group = factor(
        Comparison_group,
        levels = c("All editors", "Interlocking editors")
      )
    ) %>%
    dplyr::select(variable, Comparison_group, category, n, total, share)

  gender_levels <- c("Female", "Male")
  gender_base <- person %>%
    dplyr::select(person_id, interlocking) %>%
    dplyr::left_join(
      gender_metadata %>% dplyr::select(person_id, Gender_namsor),
      by = "person_id"
    ) %>%
    dplyr::filter(Gender_namsor %in% gender_levels)

  gender_all <- gender_base %>%
    dplyr::count(Gender_namsor, name = "n") %>%
    dplyr::mutate(Comparison_group = "All confidently classified editors")

  gender_interlocking <- gender_base %>%
    dplyr::filter(interlocking) %>%
    dplyr::count(Gender_namsor, name = "n") %>%
    dplyr::mutate(Comparison_group = "Confidently classified interlocking editors")

  gender_data <- dplyr::bind_rows(gender_all, gender_interlocking) %>%
    tidyr::complete(
      Comparison_group = c("All confidently classified editors", "Confidently classified interlocking editors"),
      Gender_namsor = gender_levels,
      fill = list(n = 0L)
    ) %>%
    dplyr::group_by(Comparison_group) %>%
    dplyr::mutate(total = sum(n), share = n / total) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      variable = "Gender",
      category = factor(Gender_namsor, levels = gender_levels),
      Comparison_group = factor(
        Comparison_group,
        levels = c("All confidently classified editors", "Confidently classified interlocking editors")
      )
    ) %>%
    dplyr::select(variable, Comparison_group, category, n, total, share)

  # Validation checks prevent stale descriptive baselines from silently entering
  # the manuscript after future data changes.
  continent_totals <- continent_data %>%
    dplyr::distinct(Comparison_group, total)
  gender_totals <- gender_data %>%
    dplyr::distinct(Comparison_group, total)

  got_continent <- stats::setNames(continent_totals$total, as.character(continent_totals$Comparison_group))
  got_gender <- stats::setNames(gender_totals$total, as.character(gender_totals$Comparison_group))

  if (!identical(as.integer(got_continent[["All editors"]]), 2033L) ||
      !identical(as.integer(got_continent[["Interlocking editors"]]), 80L)) {
    warning(sprintf(
      "Figure 3 continent denominators changed: all=%s, interlocking=%s; expected 2033 and 80.",
      got_continent[["All editors"]], got_continent[["Interlocking editors"]]
    ))
  }
  if (!identical(as.integer(got_gender[["All confidently classified editors"]]), 1797L) ||
      !identical(as.integer(got_gender[["Confidently classified interlocking editors"]]), 69L)) {
    warning(sprintf(
      "Figure 3 gender denominators changed: all-classified=%s, interlocking=%s; expected 1797 and 69.",
      got_gender[["All confidently classified editors"]], got_gender[["Confidently classified interlocking editors"]]
    ))
  }

  list(continent = continent_data, gender = gender_data)
}

#' Build one publication-quality 100% composition panel
#'
#' Segment coordinates are calculated explicitly rather than delegated to
#' ggplot2's stacking algorithm. This guarantees that category order, legend
#' order, segment placement, and percentage labels cannot drift apart.
make_composition_panel <- function(data, palette, panel_title, category_order,
                                   group_order, label_threshold = 0.075) {
  plot_data <- data %>%
    dplyr::mutate(
      category_chr = as.character(category),
      category_index = match(category_chr, category_order),
      group_chr = as.character(Comparison_group),
      group_index = match(group_chr, group_order)
    ) %>%
    dplyr::arrange(group_index, category_index) %>%
    dplyr::group_by(group_chr) %>%
    dplyr::mutate(
      xmin = dplyr::lag(cumsum(share), default = 0),
      xmax = cumsum(share),
      midpoint = (xmin + xmax) / 2,
      pct_label = dplyr::if_else(
        share >= label_threshold,
        scales::percent(share, accuracy = 0.1),
        ""
      )
    ) %>%
    dplyr::ungroup()

  group_meta <- plot_data %>%
    dplyr::distinct(group_chr, group_index, total) %>%
    dplyr::arrange(group_index) %>%
    dplyr::mutate(
      y = group_index,
      row_label = sprintf("%s  (n = %s)", group_chr, scales::comma(total))
    )

  plot_data <- plot_data %>%
    dplyr::left_join(group_meta %>% dplyr::select(group_chr, y), by = "group_chr") %>%
    dplyr::mutate(
      ymin = y - 0.29,
      ymax = y + 0.29
    )

  ggplot(plot_data) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = category_chr),
      color = "white", linewidth = 0.7
    ) +
    geom_text(
      data = plot_data %>% dplyr::filter(nzchar(pct_label)),
      aes(x = midpoint, y = y, label = pct_label),
      inherit.aes = FALSE,
      size = 3.55,
      fontface = "bold",
      color = "white"
    ) +
    scale_fill_manual(
      values = palette,
      breaks = category_order,
      name = NULL,
      drop = FALSE
    ) +
    scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      breaks = group_meta$y,
      labels = group_meta$row_label,
      limits = c(0.55, length(group_order) + 0.45),
      expand = expansion(mult = 0)
    ) +
    labs(title = panel_title, x = "Share within group", y = NULL) +
    theme_minimal(base_size = 12, base_family = "") +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, margin = margin(b = 6)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.45),
      axis.text.y = element_text(size = 10.8, color = "grey18"),
      axis.text.x = element_text(size = 10.2, color = "grey35"),
      axis.title.x = element_text(size = 10.8, margin = margin(t = 8)),
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.text = element_text(size = 10.2),
      legend.key.width = unit(1.10, "lines"),
      plot.margin = margin(6, 8, 8, 6)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

#' Generate Figure 3: Composition by continent and gender
#'
#' Panel A compares all editors with the interlocking subset across continents.
#' Panel B compares all confidently NamSor-classified editors with confidently
#' classified interlocking editors. The panels are descriptive. Omnibus tests,
#' focal contrasts, odds ratios, confidence intervals, and multiplicity
#' adjustments remain numerical inferential results in the manuscript text.
generate_composition_comparison_plot <- function(population_data, gender_metadata, output_dir) {
  message("Generating Figure 3: continent and gender composition comparison...")

  prepared <- prepare_composition_data(population_data, gender_metadata)

  continent_order <- c("Europe", "Asia", "Americas", "Oceania", "Africa")
  continent_palette <- c(
    "Europe" = "#0072B2",
    "Asia" = "#E69F00",
    "Americas" = "#009E73",
    "Oceania" = "#CC79A7",
    "Africa" = "#D55E00"
  )
  gender_order <- c("Female", "Male")
  gender_palette <- c(
    "Female" = "#0072B2",
    "Male" = "#009E73"
  )

  p_continent <- make_composition_panel(
    prepared$continent,
    palette = continent_palette,
    panel_title = "A  Continent",
    category_order = continent_order,
    group_order = c("All editors", "Interlocking editors"),
    label_threshold = 0.075
  )

  p_gender <- make_composition_panel(
    prepared$gender,
    palette = gender_palette,
    panel_title = "B  Gender",
    category_order = gender_order,
    group_order = c("All confidently classified editors", "Confidently classified interlocking editors"),
    label_threshold = 0
  )

  combined <- patchwork::wrap_plots(
    p_continent,
    p_gender,
    ncol = 1,
    heights = c(1.08, 0.92)
  ) & theme(plot.margin = margin(4, 6, 4, 6))

  save_plot(combined, output_dir, "Figure_3", width = 9.4, height = 6.8)
  message("Figure 3 saved. Panels use the full descriptive baseline versus the interlocking subset; inference remains in the manuscript text.")
  return(invisible(TRUE))
}

#' Generate Figure 2: Descriptive interlocking-editor network across attributes
#'
#' The full 80-editor network is shown in three panels using one fixed layout.
#' Panel A visualizes full-network EVC and degree for descriptive orientation;
#' the manuscript's primary prominence analyses remain based on the 78-node
#' giant component. Panels B and C use completed gender and continent only as
#' descriptive node attributes. No visual clustering is treated as inferential.
generate_editor_network <- function(g_full, editor_stats, cfg, output_dir) {
  message("Generating Figure 2: three-panel descriptive interlocking-editor network...")

  # Full-network centralities are calculated here for visualization only. This
  # does not alter the authoritative giant-component metrics or exported results.
  cm_full <- compute_centrality_measures(g_full)
  V(g_full)$EVC_plot <- cm_full$EVC
  V(g_full)$degree_plot <- cm_full$degree

  # Demographic attributes are carried on the full graph by build_networks().
  gender_levels <- c("Female", "Male")
  continent_levels <- c("Europe", "Asia", "Americas", "Oceania", "Africa")

  gender_vals <- as.character(V(g_full)$Gender_completed)
  continent_vals <- as.character(V(g_full)$Continent_1)

  if (any(!gender_vals %in% gender_levels)) {
    bad <- unique(gender_vals[!gender_vals %in% gender_levels])
    stop(
      "Figure 2 requires completed Female/Male labels for all 80 interlocking editors. Missing/other values: ",
      paste(bad, collapse = ", "), call. = FALSE
    )
  }
  if (any(!continent_vals %in% continent_levels)) {
    bad <- unique(continent_vals[!continent_vals %in% continent_levels])
    stop(
      "Figure 2 requires one of the five continent categories for all 80 interlocking editors. Missing/other values: ",
      paste(bad, collapse = ", "), call. = FALSE
    )
  }
  if (igraph::vcount(g_full) != 80L) {
    warning(sprintf("Figure 2 full interlocking network has %d nodes; expected 80.", igraph::vcount(g_full)))
  }

  V(g_full)$Gender_plot <- factor(gender_vals, levels = gender_levels)
  V(g_full)$Continent_plot <- factor(continent_vals, levels = continent_levels)

  layout <- make_fixed_fr_layout(g_full, cfg$seed_layout)

  edge_layer <- function() {
    geom_edge_link(
      color = "grey62", alpha = 0.16, linewidth = 0.28,
      show.legend = FALSE
    )
  }

  base_network_theme <- theme_publication(base_size = 11.5) +
    theme(
      plot.title = element_text(face = "bold", size = 11.8, hjust = 0),
      legend.spacing.y = unit(0.10, "cm"),
      legend.spacing.x = unit(0.16, "cm"),
      legend.box.spacing = unit(0.12, "cm"),
      legend.margin = margin(2, 2, 0, 2),
      plot.margin = margin(4, 4, 2, 4)
    )

  p_evc <- ggraph(layout) +
    edge_layer() +
    geom_node_point(
      aes(fill = EVC_plot, size = degree_plot),
      shape = 21, color = "grey15", stroke = 0.48
    ) +
    scale_fill_viridis_c(
      name = "Relational prominence\n(EVC)",
      option = "D", end = 0.96
    ) +
    scale_size_continuous(
      name = "Degree", range = c(2.7, 6.1),
      breaks = c(10, 20, 30, 40)
    ) +
    labs(title = "A  Relational prominence") +
    base_network_theme +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "left"
    ) +
    guides(
      fill = guide_colorbar(
        order = 1,
        title.position = "top",
        barwidth = unit(3.0, "cm"),
        barheight = unit(0.32, "cm")
      ),
      size = guide_legend(
        order = 2,
        title.position = "top",
        nrow = 1,
        override.aes = list(shape = 21, fill = "grey65")
      )
    )

  gender_palette <- c("Female" = "#0072B2", "Male" = "#009E73")
  p_gender <- ggraph(layout) +
    edge_layer() +
    geom_node_point(
      aes(fill = Gender_plot),
      shape = 21, size = 4.1, color = "grey15", stroke = 0.48
    ) +
    scale_fill_manual(
      values = gender_palette, breaks = gender_levels,
      name = "Gender", drop = FALSE
    ) +
    labs(title = "B  Gender") +
    base_network_theme +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4.5)))

  continent_palette <- c(
    "Europe" = "#0072B2",
    "Asia" = "#E69F00",
    "Americas" = "#009E73",
    "Oceania" = "#CC79A7",
    "Africa" = "#D55E00"
  )
  p_continent <- ggraph(layout) +
    edge_layer() +
    geom_node_point(
      aes(fill = Continent_plot),
      shape = 21, size = 4.1, color = "grey15", stroke = 0.48
    ) +
    scale_fill_manual(
      values = continent_palette, breaks = continent_levels,
      name = "Continent", drop = FALSE
    ) +
    labs(title = "C  Continent") +
    base_network_theme +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4.5)))

  # Equal-width horizontal panels make the common topology directly comparable
  # across relational prominence, gender, and continent. The fixed layout object
  # above is reused unchanged in all three panels.
  combined <- p_evc | p_gender | p_continent

  save_plot(combined, output_dir, "Figure_2", width = 15.0, height = 5.25)
  message(paste0(
    "Figure 2 saved. All three panels use the same full 80-editor network layout. ",
    "Gender and continent are descriptive attributes; primary inference remains in the manuscript text."
  ))
  return(invisible(TRUE))
}

#' Generate Figure 4: Journal network community structure
generate_journal_community_visualization <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating Figure 4: journal community visualization...")

  vertex_df <- data.frame(Journal = V(g_journal)$name, stringsAsFactors = FALSE) %>%
    left_join(journal_stats, by = "Journal")
  for (col in names(vertex_df)) {
    if (col != "Journal") g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
  }

  # Collapse the six singleton Leiden communities into one neutral display class.
  # This is a visualization-only transformation: the underlying nine-community
  # partition and community IDs are unchanged in the analysis and outputs.
  community_sizes <- tibble::tibble(
    community = as.character(V(g_journal)$community)
  ) %>%
    dplyr::count(community, name = "community_n")

  display_groups <- tibble::tibble(
    community = as.character(V(g_journal)$community)
  ) %>%
    dplyr::left_join(community_sizes, by = "community") %>%
    dplyr::mutate(
      community_display = dplyr::if_else(
        community_n == 1,
        "Singleton community",
        paste0("Multi-journal community (n = ", community_n, ")")
      )
    )

  g_journal <- igraph::set_vertex_attr(
    g_journal, "community_display", value = display_groups$community_display
  )

  # Wrap only the display labels; exact journal names remain unchanged in the graph.
  display_label <- wrap_journal_labels(V(g_journal)$name, width = 27L)
  g_journal <- igraph::set_vertex_attr(g_journal, "display_label", value = display_label)

  layout <- make_fixed_fr_layout(g_journal, cfg$seed_layout)

  edge_weights <- E(g_journal)$shared_editors
  legend_breaks <- if (length(edge_weights) > 0) {
    max_weight <- max(edge_weights, na.rm = TRUE)
    floor(unique(pretty(1:max_weight))) %>% .[. >= 1]
  } else {
    c(1)
  }

  multi_labels <- sort(unique(display_groups$community_display[display_groups$community_n > 1]))
  multi_cols <- setNames(
    viridisLite::viridis(length(multi_labels), option = "D", begin = 0.18, end = 0.82),
    multi_labels
  )
  fill_values <- c(multi_cols, "Singleton community" = "grey78")

  p_journal_comm <- ggraph(layout) +
    geom_edge_link(
      aes(width = shared_editors),
      color = "grey72", alpha = 0.44,
      show.legend = TRUE
    ) +
    geom_node_point(
      aes(size = n_editors, fill = community_display),
      shape = 21, color = "white", stroke = 1.0
    ) +
    geom_node_text(
      aes(label = display_label),
      repel = TRUE,
      size = 3.55,
      lineheight = 0.94,
      max.overlaps = Inf,
      box.padding = 0.75,
      point.padding = 0.52,
      segment.size = 0.20,
      segment.alpha = 0.34,
      bg.color = "white",
      bg.r = 0.08
    ) +
    scale_edge_width_continuous(
      name = "Shared editors",
      range = c(0.45, 3.6),
      breaks = legend_breaks
    ) +
    scale_size_continuous(
      name = "Interlocking editors",
      range = c(4.2, 13.0),
      breaks = c(5, 10, 15, 20, 25),
      limits = c(1, 29)
    ) +
    scale_fill_manual(
      name = "Leiden community structure",
      values = fill_values,
      breaks = c(multi_labels, "Singleton community")
    ) +
    labs(title = NULL, caption = NULL) +
    theme_publication(base_size = 12) +
    theme(
      legend.spacing.y = unit(0.12, "cm"),
      legend.box.spacing = unit(0.14, "cm"),
      plot.margin = margin(6, 6, 6, 6)
    ) +
    guides(
      fill = guide_legend(order = 1, override.aes = list(size = 5.5)),
      size = guide_legend(order = 2, override.aes = list(fill = "grey55")),
      edge_width = guide_legend(order = 3)
    )

  save_plot(p_journal_comm, output_dir, "Figure_4", width = 13.2, height = 9.4)
  message("Figure 4 saved. Singleton communities use a common neutral display treatment.")
  return(invisible(TRUE))
}

#' Generate Figure 5: Journal network panels (median EVC and Gini)
generate_journal_network_panels <- function(g_journal, journal_stats, cfg, output_dir) {
  message("Generating Figure 5: journal network panels...")

  vertex_df <- as_data_frame(g_journal, "vertices") %>%
    left_join(journal_stats, by = c("name" = "Journal"))
  for (col in names(vertex_df)) {
    if (col != "name") g_journal <- set_vertex_attr(g_journal, name = col, value = vertex_df[[col]])
  }

  # Use the same deterministic coordinates in both panels so differences are
  # attributable to the node metric rather than to layout variation.
  layout <- make_fixed_fr_layout(g_journal, cfg$seed_layout)
  layout$display_label <- wrap_journal_labels(layout$name, width = 24L)

  edge_weights <- E(g_journal)$shared_editors
  legend_breaks <- if (length(edge_weights) > 0) {
    max_weight <- max(edge_weights, na.rm = TRUE)
    floor(unique(pretty(1:max_weight))) %>% .[. >= 1]
  } else { c(1) }

  # FIX 4: improved label repulsion and explicit size breaks.
  # FIX 2: embedded panel captions removed per Wiley guidelines;
  #         panel tags (a/b) retained for identification.
  p_journal_evc <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.24, color = "grey70") +
    geom_node_point(aes(size = n_editors, color = median_evc)) +
    geom_node_text(
      aes(label = display_label),
      repel         = TRUE,
      size          = 3.2,
      max.overlaps  = Inf,
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.size  = 0.2,
      segment.alpha = 0.5
    ) +
    scale_edge_width_continuous(name = "Shared editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Median member EVC") +
    scale_size_continuous(name = "Interlocking editors", range = c(3, 12),
                          breaks = c(5, 10, 15, 20, 25), limits = c(1, 29)) +
    labs(title = NULL, tag = "a", caption = NULL) +
    theme_publication()

  p_journal_gini <- ggraph(layout) +
    geom_edge_link(aes(width = shared_editors), alpha = 0.24, color = "grey70") +
    geom_node_point(aes(size = n_editors, color = gini_evc)) +
    geom_node_text(
      aes(label = display_label),
      repel         = TRUE,
      size          = 3.2,
      max.overlaps  = Inf,
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.size  = 0.2,
      segment.alpha = 0.5
    ) +
    scale_edge_width_continuous(name = "Shared editors", breaks = legend_breaks) +
    scale_color_viridis_c(name = "Within-board Gini", option = "plasma") +
    scale_size_continuous(name = "Interlocking editors", range = c(3, 12),
                          breaks = c(5, 10, 15, 20, 25), limits = c(1, 29)) +
    labs(title = NULL, tag = "b", caption = NULL) +
    theme_publication()

  combined_plot <- p_journal_evc + p_journal_gini +
    plot_layout(guides = 'collect') & theme(legend.position = 'right')

  # FIX 3: file renamed to Wiley convention (Figure_N)
  save_plot(combined_plot, output_dir, "Figure_5", width = 17, height = 8.6)
  message("Figure 5 (journal network panels) saved.")
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
