# run_journal_resolution_sweep.R
# Sensitivity of the journal-network Leiden partition to the resolution
# parameter. The primary journal partition uses the fixed resolution in
# config.yml (journal_leiden_resolution). This script re-runs exactly the
# same procedure (run_journal_leiden(): CPM, shared-editor weights, seed_leiden)
# across the configured resolution grid and compares every partition with the
# primary one. It reads the 22-journal giant component from the targets store
# and does not rebuild the network. Run the main pipeline first.

required <- c("config", "targets", "igraph", "dplyr", "tibble", "readr")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) {
  stop("Missing required package(s): ", paste(missing, collapse = ", "),
       ". Run Rscript install_packages.R first.", call. = FALSE)
}

source("R/network_analysis.R")

cfg <- config::get(file = "config.yml")
g <- targets::tar_read(g_journal_gc)
out_dir <- "output/communities"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

primary_res <- as.numeric(cfg$journal_leiden_resolution)
primary <- run_journal_leiden(g, cfg)
primary_m <- primary$membership[igraph::V(g)$name]

grid <- get_leiden_resolution_grid(cfg)

rows <- lapply(grid, function(res) {
  cfg_r <- cfg
  cfg_r$journal_leiden_resolution <- res
  fit <- run_journal_leiden(g, cfg_r)
  m <- fit$membership[igraph::V(g)$name]
  sizes <- table(m)
  tibble::tibble(
    resolution = res,
    modularity = fit$summary$modularity[[1]],
    n_communities = length(sizes),
    n_singletons = sum(sizes == 1),
    n_multi_journal_communities = sum(sizes > 1),
    largest_community_size = as.integer(max(sizes)),
    ari_vs_primary = igraph::compare(m, primary_m, method = "adjusted.rand"),
    nmi_vs_primary = igraph::compare(m, primary_m, method = "nmi"),
    identical_to_primary = igraph::compare(m, primary_m, method = "vi") < 1e-12
  )
})

sweep <- dplyr::bind_rows(rows)
readr::write_csv(sweep, file.path(out_dir, "journal_resolution_sweep.csv"))

same <- sweep$resolution[sweep$identical_to_primary]
message(sprintf("Journal giant component: %d journals, %d ties", igraph::vcount(g), igraph::ecount(g)))
message(sprintf("Primary resolution %.1f: Q = %.3f, %d communities", primary_res,
                primary$summary$modularity[[1]], primary$summary$n_communities[[1]]))
message("Resolutions reproducing the primary partition exactly: ",
        if (length(same)) paste(format(same, nsmall = 1), collapse = ", ") else "none")
print(as.data.frame(sweep), digits = 3, row.names = FALSE)
message("Written: ", file.path(out_dir, "journal_resolution_sweep.csv"))
