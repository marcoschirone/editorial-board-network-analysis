Editorial Network Analysis
A modular pipeline for analyzing academic editorial board co-membership networks. This analysis identifies influential editors using Eigenvector Centrality (EVC), detects communities, and measures inequality using the Gini coefficient.

Key Features
Clean Modular Design: Functions organized by purpose across focused files.

Centralized Configuration: All parameters managed in config.yml.

Targets Pipeline: Reproducible workflow with dependency tracking.

Comprehensive Analysis: Network metrics, community detection, and disparity analysis.

Rich Visualizations: Network plots, community maps, and disparity dashboards.

Automatic Community Detection Optimization: Finds the optimal Leiden algorithm resolution for community detection based on modularity.

Comprehensive Robustness & Sensitivity Analysis: Includes a suite of tests to validate the stability of the results.

Reproducibility: Automatic session info, package citations, and optional renv support.

Project Structure
├── R/                          # Modular R functions
│   ├── utils.R                 # Helper functions
│   ├── data_processing.R       # Data loading and cleaning
│   ├── network_construction.R  # Network building
│   ├── network_analysis.R      # Metrics and community detection
│   ├── disparity_analysis.R    # Inequality analysis
│   ├── quality_checks.R        # Validation functions
│   ├── data_export.R          # Export and table creation
│   ├── visualizations.R       # All plotting functions
│   └── robustness_checks.R    # Robustness and sensitivity tests
├── data/                       # Input data
│   └── editorial_board_data.xlsx
├── output/                     # Generated results
├── config.yml                 # Configuration parameters
└── _targets.R                 # Pipeline definition
Quick Start
1. Install Dependencies

R
# Core packages
install.packages(c(
  "tidyverse", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "config", "ineq", "patchwork", "viridis", "forcats", "here"
))

# Optional (recommended)
install.packages(c("sessioninfo", "renv"))
2. Configure Analysis

Edit config.yml to match your data. The pipeline can now automatically optimize the leiden_resolution, so the value in the config file serves as a fallback.

YAML
default:
  file_path: "data/editorial_board_data.xlsx"
  col_orcid: "ORCID"
  col_journal: "Journal"
  # ... other column mappings
  leiden_resolution: 0.2
  journal_leiden_resolution: 0.2
3. Run Analysis

R
library(targets)
tar_make()
Output Structure
The analysis generates a comprehensive set of outputs:

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
Key Analysis Components
Network Construction

Editor co-membership networks based on shared journal affiliations.

Journal networks based on shared editors.

Configurable thresholds for network density (min_shared_journals).

Community Detection

Leiden algorithm with automatic resolution optimization: The pipeline sweeps through resolution parameters to find the value that maximizes modularity, ensuring optimal community structure. The value in config.yml is used as a fallback.

Separate communities are detected for both editor and journal networks.

Centrality Analysis

Eigenvector centrality (EVC) as the primary measure of influence (symbolic capital).

Degree and betweenness centrality are also calculated for robustness checks.

Percentile rankings and Gini coefficient for inequality assessment.

Disparity & Composition Analysis

Gender disparities in editorial influence.

Geographic disparities (continent, subregion, country).

Editorial board composition analysis: Assesses board-level characteristics like gender proportion and geographic diversity.

Pipeline Management
R
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
Reproducibility Features
Fixed seeds: Consistent network layouts and community detection.

Version tracking: Automatic sessionInfo.txt and package citations.

Environment snapshots: Optional renv lockfiles.

Parameter externalization: All settings in config.yml.

Customization
Adding New Metrics

Add functions to R/network_analysis.R and update the pipeline in _targets.R.

New Visualizations

Add plotting functions to R/visualizations.R and create corresponding targets.

Different Data Sources

Update column mappings in config.yml—no code changes needed.

Robustness Checks
The pipeline includes a comprehensive suite of automated robustness checks to ensure the validity and stability of the findings.

Network Density Sensitivity: Tests how key network metrics (e.g., giant component size, Gini coefficient) change as the co-membership threshold (min_shared_journals) is varied.

Centrality Measure Correlation: Analyzes the Spearman correlation between EVC, degree, betweenness, and closeness centrality to ensure they capture similar aspects of influence.

Community Detection Stability: Evaluates how modularity and the number of communities change across a range of Leiden resolution parameters.

Bootstrap Confidence Intervals: Resamples the network edges to calculate 95% confidence intervals for key metrics like the median EVC and Gini coefficient, providing a measure of uncertainty.

Component Inclusion Analysis: Compares metrics calculated on the full network versus only the giant component to assess the impact of isolates.

Dependencies
Required R Packages

Data: tidyverse, readxl, openxlsx, config, here

Networks: igraph, ggraph

Pipeline: targets

Visualization: viridis, patchwork, forcats

Analysis: ineq

System Requirements

R ≥ 4.0

~1GB RAM for typical editorial board datasets

Graphics device for plot generation

Citation
If you use this code in your research, please cite:

Code snippet
@software{editorial_network_analysis,
  title = {Editorial Network Analysis Pipeline},
  author = {Marco Schirone},
  year = {2025},
  url = {https://github.com/[your-username]/editorial-network-analysis}
}
Also cite the R packages used (automatically generated in output/R-packages.bib).

License
MIT License

Contributing
Fork the repository

Create a feature branch

Add functions to the appropriate R/ files

Update _targets.R if needed

Submit a pull request

Issues and Support
Bug reports: Use GitHub issues

Questions: Check existing issues or create a new one

Feature requests: Describe the proposed functionality and use case



