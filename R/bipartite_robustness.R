# R/bipartite_robustness.R
# Bipartite robustness check for Reviewer 1.
# Compares EVC from the projected editor-editor network against
# HITS hub scores and SVD-based centrality computed directly
# on the original bipartite (editor x journal) incidence matrix.

run_bipartite_comparison <- function(data_clean, g_gc, output_dir) {
  message("=== Bipartite Robustness Check ===")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ── 1. Restrict to editors in the giant component ──────────────────────────
  gc_editors <- igraph::V(g_gc)$name
  data_gc    <- dplyr::filter(data_clean, editor_id %in% gc_editors)
  
  editors  <- sort(unique(data_gc$editor_id))
  journals <- sort(unique(data_gc$Journal))
  n_e <- length(editors)
  n_j <- length(journals)
  
  message(sprintf("Giant component: %d editors across %d journals", n_e, n_j))
  
  # ── 2. Build sparse biadjacency matrix B (editors × journals) ───────────────
  ei <- match(data_gc$editor_id, editors)
  ji <- match(data_gc$Journal,   journals)
  
  B <- Matrix::sparseMatrix(
    i = ei, j = ji, x = 1,
    dims = c(n_e, n_j),
    dimnames = list(editors, journals)
  )
  
  # ── 3. HITS hub scores  ──────────────────────────────────────────────────────
  # Hub score = leading eigenvector of B B^T
  # Equivalent to the left singular vector of B weighted by its singular value
  BBt <- Matrix::tcrossprod(B)   # n_e × n_e
  
  hits_scores <- tryCatch({
    res <- irlba::irlba(BBt, nv = 1, tol = 1e-9)
    abs(res$u[, 1])
  }, error = function(e) {
    message("irlba failed on BBt, using base::eigen (slower): ", e$message)
    abs(eigen(as.matrix(BBt), symmetric = TRUE)$vectors[, 1])
  })
  names(hits_scores) <- editors
  hits_scores <- hits_scores / max(hits_scores)
  
  # ── 4. SVD-based centrality ─────────────────────────────────────────────────
  # First left singular vector of B directly
  svd_scores <- tryCatch({
    res <- irlba::irlba(B, nv = 1, tol = 1e-9)
    abs(res$u[, 1])
  }, error = function(e) {
    message("irlba failed on B, using base::svd (slower): ", e$message)
    abs(svd(as.matrix(B))$u[, 1])
  })
  names(svd_scores) <- editors
  svd_scores <- svd_scores / max(svd_scores)
  
  # ── 5. EVC from projected network ───────────────────────────────────────────
  evc_scores <- igraph::eigen_centrality(
    g_gc, directed = FALSE, weights = igraph::E(g_gc)$weight
  )$vector
  names(evc_scores) <- igraph::V(g_gc)$name
  
  # ── 6. Align and assemble comparison data frame ─────────────────────────────
  common <- intersect(names(evc_scores), editors)
  
  df <- tibble::tibble(
    editor     = common,
    EVC        = evc_scores[common],
    HITS       = hits_scores[common],
    SVD        = svd_scores[common]
  )
  
  # ── 7. Spearman correlations ─────────────────────────────────────────────────
  cor_eh  <- cor.test(df$EVC, df$HITS, method = "spearman")
  cor_es  <- cor.test(df$EVC, df$SVD,  method = "spearman")
  cor_hs  <- cor.test(df$HITS, df$SVD, method = "spearman")
  
  corr_table <- tibble::tibble(
    comparison = c("EVC vs HITS", "EVC vs SVD", "HITS vs SVD"),
    rho        = round(c(cor_eh$estimate, cor_es$estimate, cor_hs$estimate), 4),
    p_value    = signif(c(cor_eh$p.value, cor_es$p.value, cor_hs$p.value), 3),
    n_editors  = nrow(df)
  )
  
  message("\n── Correlation Results ──────────────────────────────")
  print(corr_table)
  message("─────────────────────────────────────────────────────\n")
  
  # ── 8. Scatter plots ─────────────────────────────────────────────────────────
  make_scatter <- function(x, y, xlab, ylab, rho, colour) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(alpha = 0.65, size = 2.2, colour = colour) +
      ggplot2::geom_smooth(method = "lm", se = FALSE,
                           colour = "black", linewidth = 0.7, linetype = "dashed") +
      ggplot2::annotate("text", x = Inf, y = -Inf,
                        hjust = 1.1, vjust = -0.5, size = 4,
                        label = sprintf("\u03c1 = %.4f", rho)) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::theme_bw(base_size = 12)
  }
  
  p_eh <- make_scatter("EVC", "HITS",
                       "EVC (projected network)", "HITS hub score (bipartite)",
                       cor_eh$estimate, "#2C7BB6")
  
  p_es <- make_scatter("EVC", "SVD",
                       "EVC (projected network)", "SVD centrality (bipartite)",
                       cor_es$estimate, "#D7191C")
  
  combined <- patchwork::wrap_plots(p_eh, p_es, ncol = 2) +
    patchwork::plot_annotation(
      title    = "Bipartite vs. Projected Network: Centrality Comparison",
      subtitle = sprintf(
        "EVC vs HITS: \u03c1 = %.4f | EVC vs SVD: \u03c1 = %.4f  (n = %d editors, Spearman)",
        cor_eh$estimate, cor_es$estimate, nrow(df)
      ),
      caption  = "Bipartite centrality computed on the editor \u00d7 journal incidence matrix."
    )
  
  plot_path <- file.path(output_dir, "bipartite_comparison.png")
  ggplot2::ggsave(plot_path, combined, width = 12, height = 6, dpi = 300)
  message(sprintf("Plot saved: %s", plot_path))
  
  # ── 9. Save CSV ──────────────────────────────────────────────────────────────
  csv_path <- file.path(output_dir, "bipartite_correlation_summary.csv")
  readr::write_csv(corr_table, csv_path)
  message(sprintf("CSV saved: %s", csv_path))
  
  list(
    comparison_df  = df,
    correlations   = corr_table,
    plot           = combined,
    plot_path      = plot_path
  )
}