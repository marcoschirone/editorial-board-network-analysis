# ---- _targets.R ----
# A declarative pipeline using the {targets} package.
# Run with: targets::tar_make()

library(targets)
library(tarchetypes)

# Set target options (packages attached for pipeline + show up in sessionInfo)
tar_option_set(
  packages = c(
    "tidyverse", "igraph", "ggraph", "readxl", "config",
    "here", "openxlsx", "ineq", "patchwork"
  )
)

# ---- Reproducibility helper functions (inline) ----
write_session_info <- function(path = "output/sessionInfo.txt",
                               use_sessioninfo = TRUE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (use_sessioninfo && requireNamespace("sessioninfo", quietly = TRUE)) {
    x <- utils::capture.output(sessioninfo::session_info(include_base = TRUE))
  } else {
    x <- utils::capture.output(sessionInfo())
  }
  writeLines(x, con = path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

write_pkg_citations <- function(path = "output/R-packages.bib", pkgs = NULL) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  
  # default: cite all attached packages (those you library() via tar_option_set)
  if (is.null(pkgs)) {
    si <- sessionInfo()
    attached <- names(si$otherPkgs %||% list())
    pkgs <- sort(attached)
  }
  
  bibs <- lapply(
    pkgs,
    function(p) utils::toBibtex(utils::citation(p), style = "BibTeX")
  )
  bibs_chr <- vapply(bibs, paste, character(1L), collapse = "\n\n")
  writeLines(bibs_chr, con = path)
  
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# ---- Source your project functions/themes ----
tar_source("R/functions.R")
tar_source("R/additional_analysis.R")
tar_source("R/visualization_themes.R")

# ---------------- Pipeline ----------------
list(
  
  # 1) Configuration and setup
  tar_target(config, config::get(file = "config.yml")),
  tar_target(output_dirs, {
    dir.create("output/main_analysis", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/supplementary", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
    "output"
  }),
  
  # 2) Data processing and network building
  tar_target(data_clean, load_and_clean_data(config)),
  tar_target(networks, build_networks(data_clean, config$default$min_shared_journals %||% config$min_shared_journals)),
  tar_target(g_journal, build_journal_network(data_clean)),
  
  # 3) Analysis
  tar_target(leiden_rec, run_leiden_sweep(networks$g_gc, config)),
  tar_target(
    updated_config,
    {
      # safer than c(list, list): preserves named list semantics
      base_cfg <- if (!is.null(config$default)) config$default else config
      utils::modifyList(base_cfg, list(leiden_resolution = leiden_rec$recommendation$resolution))
    }
  ),
  tar_target(metrics, calculate_network_metrics(networks$g_gc, updated_config)),
  tar_target(disparity_results, analyze_disparities(metrics$editor_stats, metrics$g_gc)),
  tar_target(journal_metrics, calculate_journal_network_metrics(g_journal, metrics$editor_stats, data_clean, updated_config)),
  tar_target(board_analysis, analyze_board_composition(journal_metrics, metrics$editor_stats, data_clean)),
  
  # 4) Visualizations (as file outputs)
  tar_target(main_plots, generate_visualizations(metrics$g_gc, updated_config, file.path(output_dirs, "main_analysis"))),
  tar_target(journal_plots, generate_journal_visualizations(g_journal, journal_metrics, updated_config, file.path(output_dirs, "main_analysis"))),
  tar_target(dashboards, {
    create_disparity_dashboard(disparity_results, metrics$editor_stats, file.path(output_dirs, "main_analysis"))
    create_symbolic_capital_dashboard(metrics, file.path(output_dirs, "main_analysis"))
  }),
  
  # 5) Export
  tar_target(export, {
    final_results <- list(
      graphs = networks,
      journal_graph = g_journal,
      journal_metrics = journal_metrics,
      board_analysis = board_analysis,
      leiden_sweep = leiden_rec,
      metrics = metrics,
      disparity_results = disparity_results
    )
    export_results(final_results, output_dirs)
    create_publication_tables(final_results, file.path(output_dirs, "tables"))
  }),
  
  # 6) Reproducibility artifacts (auto-generated each run)
  tar_target(
    session_info_file,
    write_session_info("output/sessionInfo.txt"),
    format = "file"
  ),
  tar_target(
    r_packages_bib,
    write_pkg_citations("output/R-packages.bib"),
    format = "file"
  ),
  tar_target(
    renv_lockfile,
    {
      if (requireNamespace("renv", quietly = TRUE)) {
        renv::snapshot(prompt = FALSE)
        "renv.lock"
      } else {
        f <- "output/renv_NOT_installed.txt"
        dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
        writeLines("Install 'renv' to snapshot package versions.", f)
        f
      }
    },
    format = "file"
  )
)