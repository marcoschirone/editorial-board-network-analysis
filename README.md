# Editorial Board Network Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-≥4.5.0-blue.svg)](https://www.r-project.org/)

## Overview

This repository contains a reproducible analytical pipeline for studying symbolic capital distribution in academic editorial networks, applied to sustainability science journals. The analysis operationalizes Bourdieu's concept of symbolic capital through eigenvector centrality and network analysis.

**Related Publication:** [Link to preprint/published paper]

## Key Features

- **Modular R functions** organized by analytical purpose
- **Targets pipeline** for reproducible workflow management
- **Network analysis** using eigenvector centrality as symbolic capital proxy
- **Community detection** via Leiden algorithm with optimization
- **Disparity analysis** across gender and geographic dimensions
- **Comprehensive robustness checks** including sensitivity analyses
- **Publication-ready visualizations**

## Repository Structure
```
├── R/                          # Modular R functions
│   ├── utils.R                 # Helper functions
│   ├── data_processing.R       # Data loading and cleaning
│   ├── network_construction.R  # Network building
│   ├── network_analysis.R      # Metrics and community detection
│   ├── disparity_analysis.R    # Inequality analysis
│   ├── quality_checks.R        # Validation functions
│   ├── data_export.R           # Export and table creation
│   ├── visualizations.R        # Plotting functions
│   └── robustness_checks.R     # Robustness and sensitivity tests
├── data/                       # Input data
│   └── sample_editorial_board_data.xlsx  # Sample dataset
├── output/                     # Generated results
│   ├── main_analysis/          # Core visualizations
│   ├── robustness/             # Robustness check results
│   └── tables/                 # Publication tables
├── config.yml                  # Configuration parameters
├── _targets.R                  # Pipeline definition
└── README.md                   # This file
```

## Quick Start

### Prerequisites
```r
# Required R version
R >= 4.5.0

# Install required packages
install.packages(c(
  "tidyverse", "igraph", "ggraph", "readxl", "openxlsx",
  "targets", "tarchetypes", "config", "ineq", "patchwork", 
  "viridis", "forcats", "here", "RColorBrewer"
))
```

### Running the Analysis
```r
# Load targets
library(targets)

# Validate pipeline
tar_validate()

# Visualize dependencies
tar_visnetwork()

# Run complete analysis
tar_make()

# View results
tar_read(metrics)
tar_read(journal_metrics)
```

### Configuration

Edit `config.yml` to customize analysis parameters:
```yaml
default:
  file_path: "data/editorial_board_data.xlsx"
  min_shared_journals: 1
  leiden_resolution: 0.2  # Automatically optimized
  seed_layout: 123
```

## Output Files

### Main Analysis
- `figure_1_editor_network_panels.png` - Editor network by gender and geography
- `figure_2_journal_network_communities.png` - Journal communities
- `figure_3_journal_network_panels.png` - Journal-level metrics
- `disparity_dashboard_full.png` - Inequality visualizations

### Data Tables
- `editor_metrics.csv` - Individual editor statistics
- `journal_metrics.csv` - Journal-level metrics
- `inequality_measures.csv` - Gini coefficients
- `gender_disparities.csv` - Gender disparity results
- `geographic_disparities_*.csv` - Geographic analyses (continent/subregion/country)

### Robustness Checks
- `threshold_sensitivity.csv` - Network sensitivity to thresholds
- `resolution_sweep.csv` - Community detection across parameters
- `bootstrap_confidence.csv` - Bootstrapped confidence intervals
- `centrality_correlations.csv` - Correlation between centrality measures

### Reproducibility
- `sessionInfo.txt` - R environment details
- `R-packages.bib` - Package citations
- `full_analysis_results.rds` - Complete results object

## Methodology

### Network Construction
- **Editor-editor network**: Co-membership based on shared journals
- **Journal-journal network**: Shared editors between journals
- **Edge weights**: Number of shared affiliations

### Symbolic Capital Operationalization
- **Primary measure**: Eigenvector centrality (EVC)
- **Rationale**: Captures recursive prestige (recognition by the recognized)
- **Validation**: Compared with degree, betweenness, closeness centrality

### Community Detection
- **Algorithm**: Leiden (Traag et al., 2019)
- **Optimization**: Automatic resolution parameter selection via modularity
- **Robustness**: Resolution sweep from 0.1 to 2.0

### Inequality Measurement
- **Metric**: Gini coefficient on EVC scores
- **Network level**: Distribution across all interlocking editors
- **Board level**: Within-journal inequality

## Key Findings

*[Add 2-3 sentence summary of main results]*

## Citation

If you use this code or methodology, please cite:
```bibtex
@article{schirone2025editorial,
  title={Symbolic Capital and Inequality in Scholarly Communication: A Network-Analytical Study of Editorial Boards},
  author={Schirone, Marco},
  journal={Journal of the Association for Information Science and Technology},
  year={2025},
  note={Under review}
}
```

### Package Citations

All R packages used are cited in `output/R-packages.bib`. Key packages:

- **targets**: Landau (2021) - Pipeline management
- **igraph**: Csárdi & Nepusz (2006) - Network analysis
- **ggraph**: Pedersen (2025) - Network visualization

## Data Availability

Due to privacy considerations, the complete editorial board dataset is not included. A sample dataset demonstrating the required data structure is provided in `data/sample_editorial_board_data.xlsx`.

### Required Data Format

| Column | Description | Example |
|--------|-------------|---------|
| ORCID | Editor identifier | 0000-0002-XXXX-XXXX |
| Journal | Journal name | Journal of X |
| Country | Country affiliation | Sweden |
| Continent | Continental region | Europe |
| Subregion | UN M49 subregion | Northern Europe |
| Gender | Gender classification | Female/Male |

## License

This project is licensed under the MIT License - see LICENSE file for details.

## Contact

Marco Schirone  
Swedish School of Library and Information Science, University of Borås  
Email: marco.schirone@hb.se  
ORCID: [0000-0002-4166-153X](https://orcid.org/0000-0002-4166-153X)

## Acknowledgments

*[Add funding sources, institutional support, etc.]*

## References

- Traag, V. A., Waltman, L., & van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing well-connected communities. *Scientific Reports*, 9(1), 5233.
- Bourdieu, P. (2004). *Science of science and reflexivity*. University of Chicago Press.
- Newman, M. (2018). *Networks* (2nd ed.). Oxford University Press.

---

**Last Updated:** 2025-10-10  
**Pipeline Version:** 1.0.0