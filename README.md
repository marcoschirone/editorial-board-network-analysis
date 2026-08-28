# Editorial Board Network Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Version](https://img.shields.io/badge/R-%E2%89%A54.5.0-blue.svg)](https://www.r-project.org/)

## Overview

This repository contains the R workflow for analyzing interlocking editorial-board networks in sustainability-oriented scholarly journals. The study examines how editorial-board positions are distributed across scholars, institutions, and geographic locations, and operationalizes symbolic capital primarily through eigenvector centrality in the editor co-membership network.

The current workflow uses a single reconstructed person-level population as the source of truth for both network membership and the selection analysis. Interlocking status is derived after controlled person-name disambiguation and duplicate appointment collapse; it is not read from the legacy 71-editor workbook.

**Related publication**

Schirone, M. (2025). *Symbolic Capital and Inequality in Scholarly Communication: A Bibliometric Study of Editorial Boards*. SocArXiv Preprint, Version 2.  
https://osf.io/preprints/socarxiv/v8zmp_v2

## Authoritative analytical invariants

A clean build currently reconstructs:

- **2,135 raw rows**
- **2,122 person-journal appointments** after collapsing 13 duplicate role rows
- **2,044 exact-name person records** before confirmed identity resolution
- **11 confirmed pairwise identity links across 10 identity components**
- **2,033 unique persons** after identity resolution
- **80 interlocking editors** serving on at least 2 distinct journals
- **6 editors** serving on at least 3 distinct journals
- **80 editors / 942 edges** in the full editor co-membership network
- **78 editors / 941 edges** in the giant component
- **25 journals / 58 edges** in the journal-journal network
- **0 unknown-gender editors among the 80 interlocking editors** after manual adjudication of the 9 newly recovered cases

These quantities are recomputed from source files in the pipeline rather than hard-coded.

## Key features

- Modular R functions organized by analytical purpose
- `{targets}` pipeline for dependency-aware reproducible execution
- Controlled person-name disambiguation with an auditable confirmed-merge layer
- Duplicate person-journal role collapse before interlocking status is calculated
- Editor-editor and journal-journal network construction
- Eigenvector centrality as the primary symbolic-capital proxy
- Leiden community detection with resolution sensitivity analysis
- Gender and geographic disparity analyses
- Full-population selection analysis using enrichment tests and Firth logistic regression
- Robustness checks for thresholds, centrality measures, components, and community resolution
- Publication-ready figures and tables generated from the pipeline

## Repository structure

```text
.
├── R/
│   ├── utils.R
│   ├── data_processing.R
│   ├── person_disambiguation.R
│   ├── selection_analysis.R
│   ├── network_construction.R
│   ├── network_analysis.R
│   ├── disparity_analysis.R
│   ├── quality_checks.R
│   ├── robustness_checks.R
│   ├── bipartite_robustness.R
│   ├── data_export.R
│   └── visualizations.R
├── data/
│   ├── sample_editorial_board_data.xlsx
│   └── country_m49_lookup.csv
├── _targets.R
├── config.yml
├── run_selection.R
├── editorial_network_analysis.Rproj
├── LICENSE
└── README.md
```

Generated results are written to `output/`, which is intentionally excluded from version control.

## Data and reproducibility

### Public repository contents

The repository contains the complete analytical code, the UN M49 geographic lookup used by the pipeline, and a sample workbook that demonstrates the expected input structure.

The sample workbook is **illustrative only**. It does not reproduce the empirical results reported above.

### Restricted empirical inputs

Full empirical reproduction requires source and adjudication files that are not distributed publicly:

```text
data/Dataset_Editorial_Boards_All.xlsx
data/editorial_board_data.xlsx
data/confirmed_name_merges.csv
data/gender_adjudication.csv
data/multi_affiliation_adjudication.csv
```

These files contain the full editorial-board population and/or person-level adjudication information. They are excluded through `.gitignore`.

Accordingly, this repository provides the complete computational workflow, but the public repository alone is not sufficient to reproduce the empirical results without access to the restricted source and adjudication files.

## Person disambiguation

Identity resolution is performed before interlocking status is calculated.

The workflow:

1. reads the full editorial-board population;
2. collapses duplicate person-journal role rows;
3. applies manually confirmed identity links from `confirmed_name_merges.csv`;
4. resolves connected identity components to a single person identity;
5. recalculates the number of distinct journals per person;
6. defines interlocking editors as people serving on at least two distinct journals.

The current confirmed merge audit contains **11 confirmed pairwise links across 10 identity components**, producing **11 net person reductions** from 2,044 exact-name records to 2,033 persons.

This distinction matters because one component contains three name strings linked by two confirmed pairs, so the number of identity components is not the same as the number of net person reductions.

## Gender adjudication

The legacy annotation workbook supplies manual Gender/ORCID metadata for previously identified editors. The corrected identity workflow recovered 9 additional interlocking editors. These 9 cases were manually reviewed and adjudicated as male using external identity evidence.

The adjudication layer is applied after person reconstruction and before gender-dependent analyses. The pipeline validates that all 9 adjudications match the corrected population and stops if any expected match is lost.

Current invariant:

```text
Gender adjudications matched: 9/9
Unknown gender among interlocking editors: 0/80
```

## Geographic classification

Country information from the source data is mapped to UN M49 continent and subregion classifications using:

```text
data/country_m49_lookup.csv
```

Three editors have more than one country across appointments. The current person-level construction uses a deterministic modal-country fallback; these cases are separately surfaced for adjudication.

## Network construction

### Editor network

Nodes are interlocking editors. Two editors are connected when they share at least one journal board.

- Node population: 80 interlocking editors
- Full network: 80 nodes, 942 edges
- Giant component: 78 nodes, 941 edges

### Journal network

Nodes are journals. Two journals are connected when they share at least one editor.

- 25 journals
- 58 edges

### Edge weights

Edge weights represent the number of shared journals/editors, depending on the projection.

For weighted shortest-path measures, tie strength is converted to distance before betweenness and closeness are calculated.

## Symbolic capital operationalization

The primary network-level measure is **eigenvector centrality (EVC)**.

EVC is interpreted as a recursive measure of network prestige: an editor receives greater centrality when connected to other highly connected editors.

The pipeline also computes degree, betweenness, and closeness centrality for validation and robustness analysis.

Current giant-component summary:

- Median EVC: **0.3635**
- Gini coefficient of EVC: **0.389**

## Community detection

Communities are identified with the Leiden algorithm.

The current optimized/specified solution uses:

- Resolution: **0.50**
- Modularity: **0.385**
- Communities: **9**

A resolution sweep is included as a robustness check.

## Selection into interlocking editorship

The selection module benchmarks interlocking editors against the full population of 2,033 unique editors.

### Omnibus geography

For continent, the primary omnibus test is a simulated Fisher exact test because some expected cell counts are below 5:

- Fisher exact, simulated: **p = 0.0790**
- Chi-square: **X²(4) = 8.818, p = 0.0658**

The chi-square result is treated as secondary because of sparse expected counts.

For M49 subregion:

- Fisher exact, simulated: **p = 0.1666**
- Chi-square: **X²(17) = 25.138, p = 0.0917**

### Europe focal contrast

Europe contains 785 of 2,033 editors and 43 of the 80 interlocking editors.

One-versus-rest enrichment:

- OR = **1.896**
- 95% CI = **1.181–3.060**
- p = **0.00668**
- Holm-adjusted p = **0.0334**

This focal Europe contrast should not be described as pre-specified.

### Firth selection model

The primary selection model uses Firth penalized logistic regression.

Complete-case model:

- n = **2,014**
- events = **80**
- McFadden pseudo-R² = **0.0244**
- LR X²(2) = **16.4**
- p = **0.00027**

Adjusted associations:

- Europe: OR = **1.962**, 95% CI = **1.253–3.086**, p = **0.00326**
- Log institutional representation, leave-one-out: OR = **1.458**, 95% CI = **1.140–1.859**, p = **0.00285**

The self-inclusive institutional representation estimate is OR = **1.639** and is retained as a sensitivity comparison.

A missingness-indicator sensitivity model retains all 2,033 editors and produces essentially the same institutional estimate.

### Definition sensitivity

- At least 2 distinct journals: **80 editors**
- At least 2 post-collapse positions: **80 editors**
- At least 3 distinct journals: **6 editors**
- At least 3 post-collapse positions: **6 editors**

### Network-position permutation tests

Using 10,000 permutations on the giant component:

- Europe vs. other regions, EVC difference: **p = 0.315**
- Female vs. male, EVC difference: **p = 0.625**

These tests concern network position among interlocking editors and are conceptually distinct from the full-population selection model.

## Robustness analysis

The pipeline includes:

- threshold sensitivity analysis;
- bootstrap confidence assessment;
- correlations among alternative centrality measures;
- giant-component inclusion sensitivity;
- Leiden resolution sweep;
- board-size/bipartite robustness analyses where applicable.

Spearman tests explicitly use approximate p-values when tied ranks are present.

## Requirements

Recommended R version:

```text
R >= 4.5.0
```

Core packages include:

```r
install.packages(c(
  "tidyverse",
  "igraph",
  "ggraph",
  "readxl",
  "openxlsx",
  "targets",
  "tarchetypes",
  "config",
  "ineq",
  "patchwork",
  "viridis",
  "forcats",
  "here",
  "RColorBrewer",
  "sessioninfo",
  "logistf"
))
```

`logistf` is recommended because the primary selection model uses Firth penalized logistic regression.

## Running the full analysis

With the restricted empirical input files in the locations specified in `config.yml`:

```r
library(targets)

tar_validate()
tar_make()
```

The selection analysis is integrated into the targets graph. `run_selection.R` is retained as a convenience runner for the selection module.

For a completely fresh rebuild:

```bash
rm -rf _targets
rm -rf output
mkdir output

Rscript -e 'targets::tar_validate()'
Rscript -e 'targets::tar_make()'
```

A successful clean run should report the authoritative invariants:

```text
2033 persons
2122 appointments
80 interlocking editors
6 editors with >=3 journals
0/80 interlocking editors with unknown gender
```

## Configuration

The principal paths are configured in `config.yml`:

```yaml
default:
  full_population_path: "data/Dataset_Editorial_Boards_All.xlsx"
  m49_lookup_path: "data/country_m49_lookup.csv"
  confirmed_merges_path: "data/confirmed_name_merges.csv"
  annotation_path: "data/editorial_board_data.xlsx"
  gender_adjudication_path: "data/gender_adjudication.csv"
```

Other parameters control layout seeds, network thresholds, Leiden resolution, and robustness settings.

## Generated outputs

The `output/` directory is regenerated by the pipeline and is not version-controlled.

Principal outputs include:

```text
output/
├── editor_metrics.csv
├── journal_metrics.csv
├── inequality_measures.csv
├── full_analysis_results.rds
├── sessionInfo.txt
├── R-packages.bib
├── main_analysis/
│   ├── Figure_1.{png,pdf,tiff}
│   ├── Figure_2.{png,pdf,tiff}
│   ├── Figure_3.{png,pdf,tiff}
│   ├── Figure_4.{png,pdf,tiff}
│   └── disparity_dashboard_full.{png,pdf,tiff}
├── robustness/
├── supplementary/
├── tables/
└── selection/
```

The selection directory contains person-level analytical data, enrichment-test outputs, model estimates and diagnostics, definition-sensitivity results, permutation outputs, and audit files generated during the workflow.

## Data format

The full source data contain editor, journal, role, affiliation, and country information. Derived geographic variables are added from the M49 lookup, while person identity, interlocking status, and network metrics are computed by the pipeline.

The legacy annotation file is used only for metadata annotation and **does not determine network membership**.

## Citation

If you use this code or methodology, please cite:

```bibtex
@misc{schirone2025preprint,
  title        = {Symbolic Capital and Inequality in Scholarly Communication: A Bibliometric Study of Editorial Boards},
  author       = {Schirone, Marco},
  year         = {2025},
  note         = {SocArXiv, Version 2. https://osf.io/preprints/socarxiv/v8zmp_v2},
  howpublished = {\url{https://osf.io/preprints/socarxiv/v8zmp_v2}}
}
```

Package citations are generated automatically in `output/R-packages.bib`.

## License

This software is released under the [MIT License](LICENSE).

The license applies to the software and documentation in this repository. It does not grant redistribution rights for third-party or restricted source datasets that are not included in the repository.

## Contact

**Marco Schirone**  
Swedish School of Library and Information Science, University of Borås  
Email: marco.schirone@hb.se  
ORCID: https://orcid.org/0000-0002-4166-153X

## Acknowledgments

The author thanks Prof. Björn Hammarfelt and Assoc. Prof. Gustaf Nelhans for their supervision and support during the development of this research. The author is also grateful to Dr. Jens Peter Andersen, Assoc. Prof. Jonas Lindahl, and Assoc. Prof. David Gunnarsson Lorentzen for comments on earlier versions of the manuscript, and to the anonymous reviewers for constructive feedback.

Any remaining errors are the author's own.

## References

- Bourdieu, P. (2004). *Science of science and reflexivity*. University of Chicago Press.
- Newman, M. (2018). *Networks* (2nd ed.). Oxford University Press.
- Traag, V. A., Waltman, L., & van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing well-connected communities. *Scientific Reports*, 9(1), 5233.

---

**Last updated:** 2026-08-28  
**Pipeline version:** 2.0.0
