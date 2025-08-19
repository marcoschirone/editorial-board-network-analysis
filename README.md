# Editorial Network Analysis

This project provides a complete pipeline to build, analyze, and visualize academic editorial board co-membership networks. It uses a modular, function-based approach that is both robust and easy to configure. The analysis identifies key editors, detects communities, and calculates a Symbolic Capital Index (SCI) based on network centrality.

## Key Features

* **Modular by Design**: All analysis steps are encapsulated in functions for clarity and reusability.
* **Centralized Configuration**: Easily manage all file paths, column names, and analysis parameters in a single `config.yml` file.
* **Advanced Analysis**: Includes disparity analysis, journal-level metrics, and robustness checks.
* **Modern Pipeline Option**: Includes an optional `_targets.R` file for a highly efficient, reproducible workflow that only re-runs steps that have changed.

---

## Folder Structure

The project is organized into the following directories and key files:

* `R/`: Contains all core R code.
    * `packages.R`: Loads all required libraries.
    * `functions.R`: Defines core functions for data loading, network building, and metrics.
    * `additional_analysis.R`: Contains functions for disparity analysis and supplementary checks.
    * `visualization_themes.R`: Defines ggplot themes and advanced plot functions.
* `data/`: Stores the input dataset (e.g., `editorial_board_data.xlsx`).
* `output/`: All generated results, including plots, tables, and R objects, are saved here.
* `config.yml`: The central YAML file for all project configurations.
* `run_analysis.R`: The **main script** to execute the entire analysis pipeline from start to finish.
* `_targets.R`: (Optional) A file that defines the analysis workflow for the `{targets}` package.

---

## Dependencies

This project relies on several R packages. You can install them all by running the following command in your R console:

```r
install.packages(c(
  "here", "tidyverse", "config", "igraph", "ggraph", "readxl", "openxlsx",
  "future", "furrr", "targets", "viridis", "patchwork", "ggalluvial", 
  "circlize", "ineq"
))