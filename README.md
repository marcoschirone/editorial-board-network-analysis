# Editorial Network Analysis

A modular pipeline for analyzing academic editorial board co-membership networks.  
This analysis identifies influential editors using **Eigenvector Centrality (EVC)**, detects communities, and measures inequality using the **Gini coefficient**.

---

## Key Features

- **Clean modular design** — functions organized by purpose across focused files
- **Centralized configuration** — all parameters managed in `config.yml`
- **Targets pipeline** — reproducible workflow with dependency tracking
- **Comprehensive analysis** — network metrics, community detection, and disparity analysis
- **Rich visualizations** — network plots, community maps, and disparity dashboards
- **Automatic community detection optimization** — finds the optimal Leiden resolution based on modularity
- **Robustness & sensitivity suite** — includes threshold sweeps, resolution sweeps, bootstrapping, and component analysis
- **Reproducibility** — session info, package citations, and optional `renv` support

---

## Repository Structure

```text
├── R/                          # Modular R functions
│   ├── utils.R                 # Helper functions
│   ├── data_processing.R       # Data loading and cleaning
│   ├── network_construction.R  # Network building
│   ├── network_analysis.R      # Metrics and community detection
│   ├── disparity_analysis.R    # Inequality analysis
│   ├── quality_checks.R        # Validation functions
│   ├── data_export.R           # Export and table creation
│   ├── visualizations.R        # All plotting functions
│   └── robustness_checks.R     # Robustness and sensitivity tests
├── data/                       # Input data
│   └── editorial_board_data.xlsx
├── output/                     # Generated results
├── config.yml                  # Configuration parameters
└── _targets.R                  # Pipeline definition
```

---

## Quick Start

### 1. Install Dependencies

```r
# Core packages
install.packages(c(
  "tidyverse","igraph","ggraph","readxl","openxlsx",
  "targets","config","ineq","patchwork","viridis","forcats","here"
))

# Optional (recommended)
install.packages(c("sessioninfo","renv"))
```

### 2. Configure Analysis

Edit `config.yml` to match your data.  
The pipeline automatically optimizes `leiden_resolution`; the config value is used as a fallback.

```yaml
default:
  file_path: "data/editorial_board_data.xlsx"
  col_orcid: "ORCID"
  col_journal: "Journal"
  leiden_resolution: 0.2
  journal_leiden_resolution: 0.2
```

### 3. Run Analysis

```r
library(targets)
tar_make()
```

---

## Output Structure

```text
output/
├── main_analysis/              # Core visualizations
│   ├── network_evc_distribution.png
│   ├── network_communities.png
│   ├── journal_network_median_evc.png
│   ├── journal_network_communities.png
│   └── disparity_dashboard_full.png
├── robustness/                 # Robustness check results and plots
│   ├── threshold_sensitivity.csv
│   ├── bootstrap_confidence.csv
│   ├── centrality_correlations.csv
│   ├── component_comparison.csv
│   ├── resolution_sweep.csv
│   ├── threshold_sensitivity.png
│   ├── resolution_sweep.png
│   └── centrality_correlations.png
├── tables/                     # Publication-ready tables
│   └── publication_summary_tables.xlsx
├── editor_metrics.csv          # Individual editor statistics
├── journal_metrics.csv         # Journal-level metrics
├── inequality_measures.csv     # Gini coefficients
├── gender_disparities.csv      # Gender disparity test results
├── geographic_disparities.csv  # Geographic disparity test results
├── leiden_sweep_results.csv    # Results from the community optimization
├── full_analysis_results.rds   # Complete results object
├── sessionInfo.txt             # R environment details
└── R-packages.bib              # Package citations
```

---

## Key Analysis Components

- **Network Construction**
  - Editor co-membership networks based on shared journal affiliations
  - Journal networks based on shared editors
  - Configurable thresholds for network density (`min_shared_journals`)

- **Community Detection**
  - Leiden algorithm with automatic resolution optimization
  - Separate communities detected for both editor and journal networks

- **Centrality Analysis**
  - Eigenvector centrality (EVC) as primary measure of influence (symbolic capital)
  - Degree and betweenness centrality for robustness
  - Percentile rankings and Gini coefficient for inequality assessment

- **Disparity & Composition Analysis**
  - Gender disparities in editorial influence
  - Geographic disparities (continent, subregion, country)
  - Board-level characteristics like gender proportion and geographic diversity

---

## Pipeline Management

```r
# Validate pipeline structure
tar_validate()

# Visualize dependencies
tar_visnetwork()

# Check what needs updating
tar_outdated()

# Run specific targets
tar_make(robustness_analysis)

# View results
tar_read(metrics)
```

---

## Dependencies

- **Data:** tidyverse, readxl, openxlsx, config, here  
- **Networks:** igraph, ggraph  
- **Pipeline:** targets  
- **Visualization:** viridis, patchwork, forcats  
- **Analysis:** ineq  

**System requirements:**  
- R ≥ 4.0  
- ~1GB RAM for typical datasets  
- Graphics device for plot generation

---

## Citation

If you use this code in your research, please cite:

```bibtex
@software{editorial_network_analysis,
  title = {Editorial Network Analysis Pipeline},
  author = {Marco Schirone},
  year = {2025},
  url = {https://github.com/[your-username]/editorial-network-analysis}
}
```

Also cite the R packages used (automatically generated in `output/R-packages.bib`).

---

## License

MIT License

---

## Contributing

1. Fork the repository  
2. Create a feature branch  
3. Add functions to the appropriate `R/` files  
4. Update `_targets.R` if needed  
5. Submit a pull request  

---

## Issues and Support

- **Bug reports:** Use GitHub Issues  
- **Questions:** Check existing issues or open a new one  
- **Feature requests:** Describe the functionality and use case  
