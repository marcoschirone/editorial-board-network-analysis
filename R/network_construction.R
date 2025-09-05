# R/network_construction.R
# Network building functions

build_networks <- function(data_clean, min_shared_journals) {
  message("Building editor co-membership network...")

  nodes_df <- data_clean %>%
    group_by(editor_id) %>%
    summarise(
      Gender = case_when(
        grepl("^[Mm]", first(Gender), ignore.case = TRUE) ~ "Male",
        grepl("^[FfWw]", first(Gender), ignore.case = TRUE) ~ "Female",
        TRUE ~ "Unknown"
      ),
      Continent_1 = first(Continent),
      Country_1 = first(Country),
      Subregion_1 = first(Subregion),
      n_journals = n(),
      .groups = "drop"
    )

  edges_df <- data_clean %>%
    group_by(Journal) %>%
    reframe(edge_pairs(unique(editor_id))) %>%
    count(e1, e2, name = "weight") %>%
    filter(weight >= min_shared_journals)

  g_full <- igraph::graph_from_data_frame(d = edges_df, directed = FALSE, vertices = nodes_df)

  comp_info <- igraph::components(g_full)
  gc_nodes <- which(comp_info$membership == which.max(comp_info$csize))
  g_gc <- igraph::induced_subgraph(g_full, gc_nodes)

  message(sprintf("Full network: %d editors, %d links", vcount(g_full), ecount(g_full)))
  message(sprintf("Giant component: %d editors, %d links", vcount(g_gc), ecount(g_gc)))

  list(g_full = g_full, g_gc = g_gc)
}

build_journal_network <- function(data_clean, min_shared_editors = 1) {
  message("Building journal-journal network...")

  journal_edges <- data_clean %>%
    group_by(editor_id) %>%
    reframe(edge_pairs(unique(Journal))) %>%
    count(e1, e2, name = "shared_editors") %>%
    filter(shared_editors >= min_shared_editors)

  journal_nodes <- data_clean %>%
    group_by(Journal) %>%
    summarise(n_editors = n_distinct(editor_id), .groups = "drop")

  g_journal <- igraph::graph_from_data_frame(d = journal_edges, directed = FALSE, vertices = journal_nodes)

  message(sprintf("Journal network: %d journals, %d links", vcount(g_journal), ecount(g_journal)))
  g_journal
}
