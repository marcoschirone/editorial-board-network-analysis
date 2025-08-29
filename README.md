# Editorial Network Analysis

This project provides a complete pipeline to build, analyze, and visualize academic editorial board co-membership networks. It uses a modular, function-based approach that is both robust and easy to configure. The analysis identifies key editors by calculating their **Eigenvector Centrality (EVC)**, detects communities, and measures inequality using the **Gini coefficient**.

## Key Features

* **Modular by Design**: All analysis steps are encapsulated in functions for clarity and reusability.  
* **Centralized Configuration**: Manage file paths, column names, thresholds, seeds, and analysis parameters in a single `config.yml` file.  
* **Robustness Checks**:  
  * Alternative prestige measures (EVC, degree, betweenness)  
  * Leiden community detection repeated across a resolution sweep (0.5–2.0)  
  * Board-level inequality scores tested for sensitivity to small boards (*n* ≤ 3)  
* **Comprehensive Visualization**: Generates editor and journal network plots, community visualizations, and a 4-part disparity dashboard.  
* **Reproducibility Artifacts**: Each run produces `sessionInfo.txt` (R version + package versions) and `R-packages.bib` (citations). Optional support for `renv` lockfiles is included.  

---

## Folder Structure

* `R/`: All core R code (`packages.R`, `functions.R`, `additional_analysis.R`, `visualization_themes.R`)  
* `data/`: Input dataset (e.g., `editorial_board_data.xlsx`)  
* `output/`: All generated results (plots, tables, session info)  
* `config.yml`: Central YAML file with project parameters  
* `_targets.R`: Declarative pipeline definition (for `targets::tar_make()`)  
* `run_analysis.R`: Script to execute the entire analysis pipeline without `targets`  

---

## Dependencies

This project relies on several R packages. Install them all with:

```r
install.packages(c(
  "here", "tidyverse", "config", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "patchwork", "ineq", "forcats", "janitor", "digest"
))
```

Optional but recommended:

```r
install.packages(c("sessioninfo", "renv"))
```

---

## Running the Analysis

### Option 1: With `targets` (recommended)

```r
library(targets)
tar_make()
```

### Option 2: Direct script

```r
source("run_analysis.R")
```

Outputs will be saved under the `output/` folder:

* `main_analysis/`: network plots and dashboards  
* `tables/`: publication-ready tables  
* `supplementary/`: robustness plots  
* `sessionInfo.txt`: full environment details  
* `R-packages.bib`: BibTeX citations of R packages  

---

## Reproducibility

This pipeline is designed for full reproducibility:

* **Seeds fixed**: random seeds are set for Fruchterman–Reingold layouts and Leiden community detection.  
* **Configurable parameters**: all thresholds, resolutions, and seeds are externalized in `config.yml`.  
* **Recorded environment**: `sessionInfo.txt` and `R-packages.bib` are generated on each run.  
* **Optional environment lock**: if `renv` is installed, a `renv.lock` file snapshots package versions.  

---

## How to Verify Robustness

1. **Alternative centrality measures**:  
   Run `tar_make()` and inspect `output/supplementary/robustness_centrality_correlations.png` (degree & betweenness vs. EVC).  

2. **Resolution sweep**:  
   The function `run_leiden_sweep()` evaluates Leiden clustering modularity across `0.5–2.0` (step `0.1`). Results are stored in `output/final_results.rds`.  

3. **Board-level inequality**:  
   Gini coefficients are only computed for boards with more than 3 members; very small boards are excluded (`safe_gini()` in `R/functions.R`).  

---

## Data Availability

The code is fully available in this repository. Input data should be placed in the `data/` folder (e.g., `editorial_board_data.xlsx`). Full reproducibility is ensured via the `config.yml` file, `sessionInfo.txt`, and `R-packages.bib`. Optional support for `renv` allows a frozen environment for long-term reproducibility.  

---

## Citation

If you use this code, please cite the associated article (once published) and the R packages listed in `R-packages.bib`.  
