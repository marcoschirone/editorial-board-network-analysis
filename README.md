# Editorial Network Analysis

A modular pipeline for analyzing academic editorial board co-membership networks. This analysis identifies influential editors using **Eigenvector Centrality (EVC)**, detects communities, and measures inequality using the **Gini coefficient**.

## Key Features

* **Clean Modular Design**: Functions organized by purpose across focused files
* **Centralized Configuration**: All parameters managed in `config.yml`
* **Targets Pipeline**: Reproducible workflow with dependency tracking
* **Comprehensive Analysis**: Network metrics, community detection, and disparity analysis
* **Rich Visualizations**: Network plots, community maps, and disparity dashboards
* **Reproducibility**: Automatic session info, package citations, and optional `renv` support

## Project Structure

```
├── R/                          # Modular R functions
│   ├── utils.R                 # Helper functions
│   ├── data_processing.R       # Data loading and cleaning
│   ├── network_construction.R  # Network building
│   ├── network_analysis.R      # Metrics and community detection
│   ├── disparity_analysis.R    # Inequality analysis
│   ├── quality_checks.R        # Validation functions
│   ├── data_export.R          # Export and table creation
│   └── visualizations.R       # All plotting functions
├── data/                       # Input data
│   └── editorial_board_data.xlsx
├── output/                     # Generated results
├── config.yml                 # Configuration parameters
└── _targets.R                 # Pipeline definition
```

## Quick Start

### 1. Install Dependencies

```r
# Core packages
install.packages(c(
  "tidyverse", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "config", "ineq", "patchwork", "viridis", "forcats"
))

# Optional (recommended)
install.packages(c("sessioninfo", "renv"))
```

### 2. Configure Analysis

Edit `config.yml` to match your data:

```yaml
default:
  file_path: "data/editorial_board_data.xlsx"
  col_orcid: "ORCID"
  col_journal: "Journal"
  # ... other column mappings
```

### 3. Run Analysis

```r
library(targets)
tar_make()
```

## Output Structure

The analysis generates:

```
output/
├── main_analysis/              # Core visualizations
│   ├── network_evc_distribution.png
│   ├── network_communities.png
│   ├── journal_network_median_evc.png
│   ├── journal_network_communities.png
│   ├── disparity_gender.png
│   ├── disparity_continent.png
│   └── disparity_dashboard_full.png
├── supplementary/              # Robustness checks
│   └── robustness_evc_vs_betweenness.png
├── tables/                     # Publication-ready tables
│   └── publication_summary_tables.xlsx
├── editor_metrics.csv          # Individual editor statistics
├── journal_metrics.csv         # Journal-level metrics
├── inequality_measures.csv     # Gini coefficients
├── full_analysis_results.rds   # Complete results object
├── sessionInfo.txt            # R environment details
└── R-packages.bib            # Package citations
```

## Key Analysis Components

### Network Construction
- Editor co-membership networks based on shared journal affiliations
- Journal networks based on shared editors
- Configurable thresholds for network density

### Community Detection
- Leiden algorithm with automatic resolution optimization
- Separate communities for editors and journals
- Modularity-based quality assessment

### Centrality Analysis
- Eigenvector centrality as measure of symbolic capital
- Degree and betweenness centrality for robustness
- Percentile rankings and inequality measures

### Disparity Analysis
- Gender disparities in editorial influence
- Geographic disparities (continent, subregion, country)
- Statistical tests (Wilcoxon, Kruskal-Wallis)

## Pipeline Management

```r
# Validate pipeline structure
tar_validate()

# Visualize dependencies
tar_visnetwork()

# Check what needs updating
tar_outdated()

# Run specific targets
tar_make(data_clean)

# View results
tar_read(metrics)
```

## Reproducibility Features

- **Fixed seeds**: Consistent network layouts and community detection
- **Version tracking**: Automatic `sessionInfo.txt` and package citations
- **Environment snapshots**: Optional `renv` lockfiles
- **Parameter externalization**: All settings in `config.yml`

## Customization

### Adding New Metrics
Add functions to `R/network_analysis.R` and update the pipeline in `_targets.R`

### New Visualizations
Add plotting functions to `R/visualizations.R` and create corresponding targets

### Different Data Sources
Update column mappings in `config.yml` - no code changes needed

## Robustness Checks

The pipeline includes several robustness validations:

1. **Alternative centrality measures**: Correlation analysis between EVC, degree, and betweenness
2. **Resolution sweep**: Systematic evaluation of community detection parameters
3. **Bootstrap analysis**: Confidence intervals for key metrics (when implemented)

## Dependencies

### Required R Packages
- **Data**: `tidyverse`, `readxl`, `openxlsx`, `config`
- **Networks**: `igraph`, `ggraph`
- **Pipeline**: `targets`
- **Visualization**: `viridis`, `patchwork`, `forcats`
- **Analysis**: `ineq`

### System Requirements
- R ≥ 4.0
- ~1GB RAM for typical editorial board datasets
- Graphics device for plot generation

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

## License

MIT License

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add functions to the appropriate R/ files
4. Update `_targets.R` if needed
5. Submit a pull request

## Issues and Support

- **Bug reports**: Use GitHub issues
- **Questions**: Check existing issues or create a new one
- **Feature requests**: Describe the proposed functionality and use case