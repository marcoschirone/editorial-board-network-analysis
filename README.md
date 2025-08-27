# Editorial Network Analysis

This project provides a complete pipeline to build, analyze, and visualize academic editorial board co-membership networks. It uses a modular, function-based approach that is both robust and easy to configure. The analysis identifies key editors by calculating their **Eigenvector Centrality (EVC)**, detects communities, and measures inequality using the **Gini coefficient**.

## Key Features

* **Modular by Design**: All analysis steps are encapsulated in functions for clarity and reusability.
* **Centralized Configuration**: Easily manage file paths, column names, and analysis parameters in a single `config.yml` file.
* **Advanced Analysis**: Includes disparity analysis (gender, geography), journal-level metrics (board EVC, Gini), and robustness checks.
* **Comprehensive Visualization**: Generates multiple network plots and a 4-part disparity dashboard.

---
## Folder Structure

* `R/`: Contains all core R code (`packages.R`, `functions.R`, `additional_analysis.R`).
* `data/`: Stores the input dataset (e.g., `editorial_board_data.xlsx`).
* `output/`: All generated results are saved here.
* `config.yml`: The central YAML file for all project configurations.
* `run_analysis.R`: The **main script** to execute the entire analysis pipeline.

---
## Dependencies

This project relies on several R packages. You can install them all by running this command in your R console:

```r
install.packages(c(
  "here", "tidyverse", "config", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "viridis", "patchwork", "ineq", "forcats"
))