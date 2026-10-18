# run_journal_resolution_sweep.R
# Sensitivity of the journal-network Leiden partition to the resolution
# parameter. The primary journal partition is selected by run_journal_leiden()
# (highest weighted modularity across the configured grid; ties: smallest
# resolution). This script re-runs exactly the same procedure (CPM,
# shared-editor weights, seed_leiden) at every grid value and compares every
# partition with the primary one. It reads the 22-journal giant component from the targets store
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

primary <- run_journal_leiden(g, cfg)
primary_res <- primary$summary$resolution[[1]]
primary_m <- primary$membership[igraph::V(g)$name]

grid <- get_leiden_resolution_grid(cfg)

rows <- lapply(grid, function(res) {
  fit <- run_journal_leiden(g, cfg, resolution = res)
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
message(sprintf("Primary (selected) resolution %.1f: Q = %.3f, %d communities", primary_res,
                primary$summary$modularity[[1]], primary$summary$n_communities[[1]]))
message("Resolutions reproducing the primary partition exactly: ",
        if (length(same)) paste(format(same, nsmall = 1), collapse = ", ") else "none")
print(as.data.frame(sweep), digits = 3, row.names = FALSE)
message("Written: ", file.path(out_dir, "journal_resolution_sweep.csv"))
