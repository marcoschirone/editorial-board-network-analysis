
# Editorial Network Analysis 🌐

This project provides a complete pipeline to build, analyze, and visualize academic editorial board co-membership networks. It uses a modular, function-based approach that is both robust and easy to configure. The analysis identifies key editors, detects communities, and calculates a Symbolic Capital Index (SCI) based on network centrality.

## Key Features ✨

  * **Modular by Design**: All analysis steps are encapsulated in functions for clarity and reusability.
  * **Centralized Configuration**: Easily manage all file paths, column names, and analysis parameters in a single `_config.yml` file.
  * **Parallel Processing**: The time-intensive community detection sweep is optimized to run in parallel, significantly speeding up the analysis.
  * **Modern Pipeline Option**: Includes an optional `_targets.R` file for a highly efficient, reproducible workflow that only re-runs steps that have changed.

## Folder Structure 📁

The project is organized into the following directories and key files:

  * `R/`: Contains all core R code.
      * `packages.R`: Loads all required libraries.
      * `functions.R`: Defines all functions used in the analysis pipeline.
  * `data/`: Stores the input dataset (e.g., `Dataset_Editorial_Boards_Complete.xlsx`).
  * `output/`: All generated results, including plots, tables, and R objects, are saved here.
  * `_config.yml`: The central YAML file for all project configurations.
  * `run_analysis.R`: The main script to execute the entire analysis pipeline from start to finish.
  * `_targets.R`: (Optional) A file that defines the analysis workflow for the `{targets}` package.

## Dependencies 📦

This project relies on several R packages. You can install them all by running the following command in your R console:

```r
install.packages(c("here", "tidyverse", "igraph", "ggraph", "readxl", "openxlsx", "config", "future", "furrr", "targets"))
```

## How to Run 🚀

#### Step 1: Configure Your Analysis

Before running, open the **`_config.yml`** file in a text editor. Review the parameters and update them as needed for your specific dataset (e.g., file paths, column names, analysis thresholds).

#### Step 2: Execute the Pipeline

You have two options for running the analysis.

**Method A: Direct Execution (Recommended)**

1.  Open your R project in RStudio.
2.  Open the `run_analysis.R` script.
3.  Click the **"Source"** button in the top-right of the script editor, or run `source('run_analysis.R')` in the console.

**Method B: Advanced Execution with `{targets}`**

1.  Open your R project in RStudio.
2.  Run the command `targets::tar_make()` in the console.

The pipeline will execute, and you will see progress messages in the console.

## Output 📊

After a successful run, the `output/` directory will contain:

  * **Network plots** (`.png`) visualizing the giant component by gender and SCI.
  * A comprehensive **Excel report** (`network_analysis_results.xlsx`) with multiple sheets for editor stats, community stats, and assortativity measures.
  * **CSV files** for key data frames.
  * An **RDS file** (`full_analysis_results.rds`) containing all results in a single R object for further analysis.
