# run_exchangeability_audit.R
# Focused diagnostic/sensitivity analysis for the EVC node-feature permutation tests.
#
# This script uses the existing output/editor_metrics.csv and therefore does not
# rebuild the network. Run the full pipeline first if editor_metrics.csv is not
# current.

required <- c("config", "readr", "dplyr", "tibble")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) {
  stop("Missing required package(s): ", paste(missing, collapse = ", "),
       ". Run Rscript install_packages.R first.", call. = FALSE)
}

source("R/selection_analysis.R")

cfg <- config::get(file = "config.yml")
metrics_path <- "output/editor_metrics.csv"
out_dir <- "output/selection"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(metrics_path)) {
  stop("Missing ", metrics_path, ". Run the main pipeline first.", call. = FALSE)
}

editor_stats <- readr::read_csv(metrics_path, show_col_types = FALSE)

read_primary_p <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  x <- readr::read_csv(path, show_col_types = FALSE)
  if (!"p_permutation" %in% names(x) || nrow(x) < 1) return(NA_real_)
  x$p_permutation[[1]]
}

message("Exchangeability audit: Europe")
europe <- run_exchangeability_audit(
  editor_stats = editor_stats,
  attr_col = "Continent_1",
  focal_level = "Europe",
  block_col = "community",
  value_col = "EVC",
  n_perm = cfg$attribute_permutation_n,
  fisher_B = cfg$fisher_monte_carlo_B,
  seed = cfg$seed_selection_simulation,
  unrestricted_p = read_primary_p(file.path(out_dir, "permutation_Continent_1_Europe.csv")),
  output_dir = out_dir
)

message("Exchangeability audit: gender")
gender_col <- if ("Gender_namsor" %in% names(editor_stats)) "Gender_namsor" else "Gender"
gender_stats <- editor_stats |>
  dplyr::filter(.data[[gender_col]] %in% c("Female", "Male"))

gender <- run_exchangeability_audit(
  editor_stats = gender_stats,
  attr_col = gender_col,
  focal_level = "Female",
  block_col = "community",
  value_col = "EVC",
  n_perm = cfg$attribute_permutation_n,
  fisher_B = cfg$fisher_monte_carlo_B,
  seed = cfg$seed_selection_simulation,
  unrestricted_p = read_primary_p(file.path(out_dir, paste0("permutation_", gender_col, "_Female.csv"))),
  output_dir = out_dir
)

saveRDS(
  list(europe = europe, gender = gender),
  file.path(out_dir, "exchangeability_audit_results.rds")
)

message("Exchangeability audit complete. Outputs written to ", out_dir)
