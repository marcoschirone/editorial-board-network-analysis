# Editorial Board Network Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Version](https://img.shields.io/badge/R-%E2%89%A54.5.0-blue.svg)](https://www.r-project.org/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22145645.svg)](https://doi.org/10.5281/zenodo.22145645)

## Overview

This repository contains the R workflow for analyzing interlocking editorial-board networks in sustainability-oriented scholarly journals. The study examines how editorial-board positions are distributed across scholars, institutions, and geographic locations, and measures relational editorial prominence using eigenvector centrality in the editor co-membership network.

The current workflow uses a single reconstructed person-level population as the source of truth for both network membership and the selection analysis. Interlocking status is derived after controlled person-name disambiguation and duplicate appointment collapse; it is not read from the legacy 71-editor workbook.

Eigenvector centrality is treated as a network measure of relational editorial prominence. It is not treated as a direct operationalization of symbolic capital, prestige, authority, or any underlying causal mechanism.

**Related publication**

Schirone, M. (2025). *Symbolic Capital and Inequality in Scholarly Communication: A Bibliometric Study of Editorial Boards*. SocArXiv Preprint, Version 2.  
https://osf.io/preprints/socarxiv/v8zmp_v2

The title and theoretical framing above refer to the archived 2025 preprint. The current manuscript revision uses relational editorial prominence rather than treating eigenvector centrality as a direct measure of symbolic capital.

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
- **69 of 80 interlocking editors** confidently classified by NamSor

These quantities are recomputed from source files in the pipeline rather than hard-coded.

## Key features

- Modular R functions organized by analytical purpose
- `{targets}` pipeline for dependency-aware reproducible execution
- Controlled person-name disambiguation with an auditable confirmed-merge layer
- Duplicate person-journal role collapse before interlocking status is calculated
- Editor-editor and journal-journal network construction
- Eigenvector centrality as the primary measure of relational editorial prominence
- Separate editor-network and journal-network Leiden community analyses
- Deterministic editor-community resolution selection with resolution sensitivity analysis
- Gender and geographic disparity analyses
- Full-population selection analysis using enrichment tests and Firth logistic regression
- Robustness checks for thresholds, centrality measures, components, community resolution, and network projection
- Direct bipartite robustness analysis using HITS and singular value decomposition
- Reproducible journal-level prominence/inequality typology
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

The sample workbook is **illustrative only**. It does not reproduce the empirical results reported below.

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

A synthetic schema example is provided as `data/multi_affiliation_adjudication_example.csv`. Exact reproduction of the manuscript results requires the restricted `data/multi_affiliation_adjudication.csv`; the pipeline does not silently substitute the public example for the empirical adjudication file.

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

Primary gender analyses use NamSor consistently across the full reconstructed population and the interlocking subset, with low-confidence classifications excluded symmetrically. Manual completed labels for the interlocking editors are retained only as a sensitivity specification.

The primary analysis therefore avoids combining manual gender coding for interlocking editors with NamSor coding for the comparison population. The mixed-instrument specification is reported separately as a methodological sensitivity analysis.

Current invariant:

```text
Interlocking NamSor gender: Female=16; Low confidence=11; Male=53
Primary Fisher test: OR=0.640, 95% CI 0.338-1.149, p=0.146
```

## Geographic classification

Country information from the source data is mapped to UN M49 continent and subregion classifications using:

```text
data/country_m49_lookup.csv
```

Three editors have concurrent affiliations in more than one country. Country is assigned to the documented primary substantive institutional affiliation where this can be established from external evidence; the deterministic modal country across appointments is retained as the fallback rule when primary affiliation cannot be established. In the current empirical analysis, all three multi-country cases were manually adjudicated.

## Network construction

### Editor network

Nodes are interlocking editors. Two editors are connected when they share at least one journal board.

- Node population: **80 interlocking editors**
- Full network: **80 nodes, 942 edges**
- Giant component: **78 nodes, 941 edges**

### Journal network

Nodes are journals. Two journals are connected when they share at least one editor.

- **25 journals**
- **58 edges**

### Edge weights

Edge weights represent the number of shared journals or editors, depending on the projection.

For weighted shortest-path measures, tie strength is converted to distance before betweenness and closeness are calculated.

## Relational editorial prominence

The primary network-level measure is **eigenvector centrality (EVC)**.

EVC is interpreted as a recursive measure of relational prominence: an editor receives greater centrality when connected to other highly connected editors. EVC is not treated as a direct operationalization of symbolic capital, prestige, authority, or an underlying causal mechanism.

The pipeline also computes degree, betweenness, and closeness centrality for validation and robustness analysis.

Current giant-component summary:

- Median EVC: **0.3635**
- Gini coefficient of EVC: **0.389**

## Community detection

Editor and journal communities are analysed separately using the Leiden algorithm.

### Editor network

The editor-community analysis uses one authoritative deterministic candidate grid configured in `config.yml`, ranging from **0.1 to 2.0 in increments of 0.1**.

At each candidate resolution, Leiden optimizes the **Constant Potts Model (CPM)** objective. The resulting partitions are then compared using weighted Newman-Girvan modularity as a cross-resolution selection criterion. The partition with the highest modularity is retained, with ties resolved in favour of the lower resolution.

The same candidate grid is used for the primary analysis and the resolution-sensitivity analysis.

Current editor-network solution:

- Selected resolution: **0.20**
- Weighted modularity: **0.412**
- Communities: **6**

### Journal network

The journal-journal network is analysed separately and does not inherit the selected editor-network resolution. It uses the independently configured `journal_leiden_resolution`.

Current journal-network solution:

- Resolution: **0.50**
- Weighted modularity: **0.295**
- Communities: **9**
- Largest community: **9 journals**

The editor and journal partitions are distinct analytical objects and do not share the same resolution-selection procedure.

### Community outputs

Each pipeline run exports reproducible community assignments and summaries to `output/communities/`:

```text
output/communities/
├── editor_community_assignments.csv
├── editor_community_summary.csv
├── journal_community_assignments.csv
├── journal_community_summary.csv
└── journal_leiden_summary.csv
```

These files provide an explicit audit trail between the community-detection procedure, manuscript interpretation, and generated figures.

Figure 3 displays the journal-community partition. Figure 4 displays journal-level median EVC and Gini and does not encode community membership.

## Selection into interlocking editorship

The selection module benchmarks interlocking editors against the full population of 2,033 unique editors.

### Omnibus geography

For continent, the primary omnibus test is a simulated Fisher exact test because some expected cell counts are below 5:

- Fisher exact, simulated: **p = 0.0531**
- Chi-square: **χ²(4) = 10.307, p = 0.0356**

The chi-square result is treated as secondary because of sparse expected counts.

For M49 subregion:

- Fisher exact, simulated: **p = 0.3359**

### Europe focal contrast

Europe was identified as the main contributor to the observed geographic deviation and is therefore treated as an exploratory focal contrast rather than a pre-specified test.

One-versus-rest enrichment:

- OR = **1.994**
- 95% CI = **1.242–3.221**
- p = **0.00317**
- Holm-adjusted p = **0.0159**

### Firth selection model

The primary selection model uses Firth penalized logistic regression.

Complete-case model:

- n = **2,014**
- events = **80**
- McFadden pseudo-R² = **0.0264**
- LR χ²(2) = **17.789**
- p = **0.000137**

Adjusted associations:

- Europe: OR = **2.065**, 95% CI = **1.319–3.253**, p = **0.00155**
- Log institutional representation, leave-one-out: OR = **1.462**, 95% CI = **1.143–1.865**, p = **0.00269**

A missingness-indicator sensitivity model retains all 2,033 editors and produces essentially the same institutional estimate.

### Definition sensitivity

- At least 2 distinct journals: **80 editors**
- At least 2 post-collapse positions: **80 editors**
- At least 3 distinct journals: **6 editors**
- At least 3 post-collapse positions: **6 editors**

### Network-position permutation tests

Using 10,000 permutations on the giant component:

- Europe vs. other editors, EVC difference: **p = 0.467**
- Female vs. male, EVC difference: **p = 0.463**

These tests concern network position among interlocking editors and are conceptually distinct from the full-population selection model.

## Journal-level prominence and inequality typology

Journal-level prominence is summarized using the median EVC of editors belonging to each journal within the editor network. Within-board inequality in prominence is measured using a finite-sample-corrected Gini coefficient.

The primary typology includes the **20 journals with at least two eligible editors** and uses sample median splits:

- Median EVC threshold: **0.299**
- Corrected Gini threshold: **0.366**

The resulting four configurations are:

- **High prominence / High inequality:** 3 journals
- **High prominence / Low inequality:** 7 journals
- **Low prominence / High inequality:** 7 journals
- **Low prominence / Low inequality:** 3 journals

Raw-Gini classification is retained as a sensitivity analysis.

## Robustness analysis

The pipeline includes:

- threshold sensitivity analysis;
- bootstrap confidence assessment;
- correlations among alternative centrality measures;
- giant-component inclusion sensitivity;
- Leiden resolution sensitivity;
- board-level sensitivity analyses;
- attribute-permutation tests;
- direct bipartite robustness analysis.

To assess whether projection of the editor-journal bipartite network into an editor-editor network materially changes relative prominence, projected-network EVC is compared with HITS and SVD-based centrality calculated directly from the editor × journal incidence matrix.

In the current giant component:

- EVC vs. HITS: **Spearman's ρ = 0.997, p < 0.001, n = 78**
- EVC vs. SVD: **Spearman's ρ = 0.997, p < 0.001, n = 78**
- HITS vs. SVD: **Spearman's ρ = 1.000, n = 78**

These results indicate that the one-mode projection does not materially alter the relative prominence ranking of editors.

Spearman tests use approximate p-values where tied ranks prevent computation of exact p-values.

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
  "Matrix",
  "irlba",
  "sessioninfo",
  "logistf"
))
```

`logistf` is used for the primary Firth penalized logistic regression, while `Matrix` and `irlba` support the direct bipartite robustness analysis.

## Running the full analysis

With the restricted empirical input files in the locations specified in `config.yml`:

```r
library(targets)

tar_validate()
tar_make()
```

The selection analysis and bipartite robustness analysis are integrated into the targets graph. `run_selection.R` is retained as a convenience runner for the selection module.

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
69/80 interlocking editors confidently classified by NamSor
```

## Configuration

The principal paths are configured in `config.yml`:

```yaml
default:
  full_population_path: "data/Dataset_Editorial_Boards_All.xlsx"
  m49_lookup_path: "data/country_m49_lookup.csv"
  confirmed_merges_path: "data/confirmed_name_merges.csv"
  multi_affiliation_adjudication_path: "data/multi_affiliation_adjudication.csv"
  annotation_path: "data/editorial_board_data.xlsx"
  gender_adjudication_path: "data/gender_adjudication.csv"
```

Other parameters control layout seeds, network thresholds, editor-community resolution selection, the independently specified journal-community resolution, and robustness settings.

## Generated outputs

The `output/` directory is regenerated by the pipeline and is not version-controlled.

Principal outputs include:

```text
output/
├── editor_metrics.csv
├── journal_metrics.csv
├── inequality_measures.csv
├── manuscript_results_manifest.csv
├── full_analysis_results.rds
├── sessionInfo.txt
├── R-packages.bib
├── communities/
│   ├── editor_community_assignments.csv
│   ├── editor_community_summary.csv
│   ├── journal_community_assignments.csv
│   ├── journal_community_summary.csv
│   └── journal_leiden_summary.csv
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

The manuscript results manifest provides a machine-generated record of principal analytical quantities used for manuscript verification.

## Data format

The full source data contain editor, journal, role, affiliation, and country information. Derived geographic variables are added from the M49 lookup, while person identity, interlocking status, and network metrics are computed by the pipeline.

The legacy annotation file is used only for metadata annotation and **does not determine network membership**.

## Citation

If you use this code or methodology, please cite the archived preprint:

```bibtex
@misc{schirone2025preprint,
  title        = {Symbolic Capital and Inequality in Scholarly Communication: A Bibliometric Study of Editorial Boards},
  author       = {Schirone, Marco},
  year         = {2025},
  note         = {SocArXiv, Version 2. https://osf.io/preprints/socarxiv/v8zmp_v2},
  howpublished = {\url{https://osf.io/preprints/socarxiv/v8zmp_v2}}
}
```

The citation above refers to the archived 2025 preprint. The current manuscript revision uses the relational-prominence framing described in this repository.

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

**Last updated:** 2026-08-31  
**Pipeline version:** 2.0.0