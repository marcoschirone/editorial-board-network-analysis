# Install CRAN packages required by the editorial-board network analysis.
# Existing packages are left untouched.

required <- c(
  "tidyverse", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "tarchetypes", "config", "ineq", "patchwork",
  "viridis", "forcats", "here", "RColorBrewer", "Matrix",
  "irlba", "sessioninfo", "logistf"
)

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
