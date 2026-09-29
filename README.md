# Editorial Board Network Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Version](https://img.shields.io/badge/R-%E2%89%A54.5.0-blue.svg)](https://www.r-project.org/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23020953.svg)](https://doi.org/10.5281/zenodo.23020953)

The DOI badge points to the latest archived release (v2.1.0). This working tree is v2.1.1-dev and must not claim a new DOI until a new release is deposited.

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
- **25 journals / 58 edges** in the full journal-journal network (**density = 0.193**)
- **22 journals / 56 edges** in the journal-network giant component (**density = 0.242**)
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
│   ├── country_m49_lookup.csv
│   ├── institution_aliases.csv
│   ├── institution_country_overrides.csv
│   └── multi_affiliation_adjudication_example.csv
├── scripts/
│   ├── prepare_public_git.sh
│   ├── check_public_privacy.R
│   ├── install_privacy_precommit_hook.sh
│   └── check_sensitive_history.sh
├── private/                  # local only; ignored by Git
├── _targets.R
├── config.yml
├── run_selection.R
├── editorial_network_analysis.Rproj
├── PRIVATE_INPUTS.md
├── PUBLIC_GIT_SAFETY.md
├── LICENSE
└── README.md
```

Generated results are written to `output/`, which is intentionally excluded from version control.

### Public Git privacy gate

Person-level empirical inputs are stored under `private/` in a local working copy and are ignored by Git. Before every public commit or release, run `bash scripts/prepare_public_git.sh`. The script removes known restricted legacy paths from the Git index, stages only the intended public surface, and compares every indexed public file against the private editor-name list without printing those names. The release check fails if the private population workbook is unavailable, so a public release cannot bypass the name-content audit. `scripts/install_privacy_precommit_hook.sh` can install the same audit as a local pre-commit hook.

Because a file removed from the current branch can still remain in older Git commits, `bash scripts/check_sensitive_history.sh` separately reports whether known restricted legacy paths occur anywhere in repository history.

## Data and reproducibility

### Public repository contents

The repository contains the complete analytical code, the UN M49 geographic lookup used by the pipeline, and a sample workbook that demonstrates the expected input structure.

The sample workbook is **illustrative only**. It does not reproduce the empirical results reported below. The institution-alias table (`data/institution_aliases.csv`) contains only institution-name harmonization candidates and is designed to be public and auditable. Semantic aliases are applied only after their status is explicitly changed to `approved`.

### Restricted empirical inputs

Full empirical reproduction requires source and adjudication files that are not distributed publicly:

```text
private/Dataset_Editorial_Boards_All.xlsx
private/editorial_board_data.xlsx
private/confirmed_name_merges.csv
private/gender_adjudication.csv
private/gender_classified_full.csv
private/multi_affiliation_adjudication.csv
```

These files contain the full editorial-board population and/or person-level adjudication information. They are excluded through `.gitignore`.

Accordingly, this repository provides the complete computational workflow, but the public repository alone is not sufficient to reproduce the empirical results without access to the restricted source and adjudication files.

A synthetic schema example is provided as `data/multi_affiliation_adjudication_example.csv`. Exact reproduction of the manuscript results requires the restricted `private/multi_affiliation_adjudication.csv`; the pipeline does not silently substitute the public example for the empirical adjudication file.

## Person disambiguation

Identity resolution is performed before interlocking status is calculated.

The workflow:

1. reads the full editorial-board population;
2. collapses duplicate person-journal role rows;
3. applies manually confirmed identity links from `private/confirmed_name_merges.csv`;
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

Country and institution are handled jointly at the person level. A unique modal institution-country pair across appointments is selected automatically. Ties are never broken by spreadsheet row order. Under the primary policy, ambiguous dimensions are retained as missing while any dimension shared by all tied top pairs is preserved; an explicitly documented adjudication can override this when contemporaneous evidence is available. This prevents institution-country combinations that were never observed in the source data.

Before M49 mapping, reviewed source-level location corrections can be applied through `data/institution_country_overrides.csv`. This exact-match layer is used for demonstrable source inconsistencies and for the study's explicit Hong Kong SAR coding; rows marked `pending_review` are never applied.

Hong Kong and Macao use the separate UN M49 statistical-area labels `Hong Kong SAR, China` (344) and `Macao SAR, China` (446). Taiwan is retained as a separate affiliation location and assigned to Eastern Asia because it is not separately listed in the M49 lookup used here. China, Hong Kong SAR, Macao SAR, and Taiwan therefore remain in the same subregion for continent/subregion analyses; the distinction affects country-level counts and map labels only.

## Institution harmonization

Institution processing is intentionally conservative and auditable:

1. exact syntactic variants are normalized by ignoring case, diacritics, punctuation, repeated whitespace, and a leading English article `The`;
2. language variants and department-to-parent rollups are applied only through `data/institution_aliases.csv`, and only rows marked `approved` are used; aliases can optionally be scoped to a specific country so identically named institutions in different countries are not collapsed globally;
3. no fuzzy institution matching is used in the analytical pipeline;
4. reviewed exact-match institution/country corrections are applied before UN M49 mapping;
5. person-level institution and country are selected jointly from complete appointment-level pairs;
6. tied modal pairs are never broken by row order: unresolved dimensions remain missing in the primary analysis unless an evidence-based adjudication is supplied;
7. leave-one-out institutional representation is counted by canonical institution **plus country**, not institution name alone.

The pipeline writes `institution_normalization_audit.csv`, `institution_country_override_audit.csv`, `person_affiliation_pair_audit.csv`, `person_affiliation_status_counts.csv`, and institution-country count tables to `output/selection/`. Unresolved cases are written to `person_affiliation_review.csv` for audit but do not stop the primary pipeline when `unresolved_affiliation_action: retain_missing` is used.

The numerical results listed below are the v2.1.0 baseline until the affiliation-adjudication audit is completed and the corrected pipeline is rerun. They must not be assumed unchanged.

## Network construction

### Editor network

Nodes are interlocking editors. Two editors are connected when they share at least one journal board.

- Node population: **80 interlocking editors**
- Full network: **80 nodes, 942 edges**
- Giant component: **78 nodes, 941 edges**

### Journal network

Nodes are journals. Two journals are connected when they share at least one editor.

- Full network: **25 journals, 58 edges, density = 0.193**
- Giant component: **22 journals, 56 edges, density = 0.242**

The pipeline writes the unrounded node counts, edge counts, and densities for both scopes to `output/manuscript_results_manifest.csv`. Density is calculated directly from the saved graph objects with `igraph::edge_density()`, so manuscript-facing density values are traceable to a machine-generated output rather than reconstructed from prose.

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

The manuscript uses five main figures. Figure 1 is the reviewer-accepted geographic map maintained by the separate interactive-map workflow and is not regenerated by this `targets` pipeline. Figure 2 uses one fixed layout of the full 80-editor interlocking network across three descriptive panels: Panel A shows weighted eigenvector centrality (relational editorial prominence), recalculated on the full graph for descriptive visualization with degree by node size; Panel B shows completed Female/Male gender labels; Panel C shows continent of primary institutional affiliation. The demographic panels are descriptive only, and primary EVC inference remains based on the 78-node giant component. Figure 3 is a two-panel descriptive composition figure: Panel A compares the full 2,033-editor population with the 80 interlocking editors by continent; Panel B compares all confidently NamSor-classified editors with confidently classified interlocking editors by Female/Male composition. Figure 4 displays the journal-community partition with singleton communities visually grouped in a neutral display class, and Figure 5 displays journal-level median member EVC and within-board Gini. Inferential tests remain numerical results reported in the manuscript rather than graphical significance annotations.

To regenerate the manuscript figures produced by this repository, run `Rscript -e "targets::tar_make(c(figure_2_plot, figure_3_plot, figure_4_plot, figure_5_plot))"`. Figure 1 and its interactive supplementary version remain under the separate reviewer-accepted map workflow and should be preserved rather than replaced by this pipeline. Pre-redesign figures under `output/archive_pre_redesign/` are audit artifacts only and should not be used in the manuscript.

## Selection into interlocking editorship

The selection module benchmarks interlocking editors against the full population of 2,033 unique editors.

### Omnibus geography

For continent, the primary omnibus test is a simulated Fisher exact test because some expected cell counts are below 5:

- Fisher exact, simulated with **100,000 Monte Carlo replicates**: current value is written to `output/selection/omnibus_Continent.csv` and `output/manuscript_results_manifest.csv` after rerunning the pipeline.
- Chi-square: **χ²(4) = 10.307, p = 0.0356**

The chi-square result is treated as secondary because of sparse expected counts.

For M49 subregion:

- Fisher exact, simulated with **100,000 Monte Carlo replicates**: current value is written to `output/selection/omnibus_Subregion.csv` and `output/manuscript_results_manifest.csv` after rerunning the pipeline.

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

The current pipeline uses **100,000 permutations** on the giant component (configured in `config.yml`).

- Europe vs. other editors, EVC difference: current p-value is written to `output/selection/permutation_Continent_1_Europe.csv` after rerunning the pipeline.
- Female vs. male, EVC difference: current p-value is written to `output/selection/permutation_Gender_namsor_Female.csv` after rerunning the pipeline.

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

With the restricted empirical input files in the locations specified in `config.yml`, the convenience scripts provide the shortest clean workflow:

```bash
Rscript install_packages.R
Rscript run.R
```

Equivalently, the pipeline can be invoked directly with `{targets}`:

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
  full_population_path: "private/Dataset_Editorial_Boards_All.xlsx"
  m49_lookup_path: "data/country_m49_lookup.csv"
  confirmed_merges_path: "private/confirmed_name_merges.csv"
  multi_affiliation_adjudication_path: "private/multi_affiliation_adjudication.csv"
  institution_alias_path: "data/institution_aliases.csv"
  institution_country_override_path: "data/institution_country_overrides.csv"
  annotation_path: "private/editorial_board_data.xlsx"
  gender_namsor_path: "private/gender_classified_full.csv"
  gender_adjudication_path: "private/gender_adjudication.csv"
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
│   ├── Figure_2.{png,pdf,tiff}
│   ├── Figure_3.{png,pdf,tiff}
│   ├── Figure_4.{png,pdf,tiff}
│   ├── Figure_5.{png,pdf,tiff}
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

**Last updated:** 2026-09-28  
**Pipeline version:** 2.1.1-dev
### Publication-quality figure implementation (V4)

The current visualization functions use deterministic normalized layouts, consistent typography, 600-dpi PNG/TIFF export, LZW TIFF compression, and PDF vector output. Figure 1 is intentionally outside this visualization pipeline. Figure 2 uses identical coordinates across its three panels, a continuous Viridis scale for relational prominence recalculated on the full graph for descriptive visualization in Panel A, and color-blind-safe categorical palettes for completed gender and continent in Panels B and C. Figure 3 computes segment coordinates explicitly, so category order, color, percentage labels, and legend order cannot diverge. Figure 4 groups singleton journal communities neutrally for display only, without changing the analytical Leiden partition. Figure 5 uses identical journal-network coordinates in both panels.

## V5.3 final main-text figure architecture (2026-09-12)

V5.5 supersedes the V5.3/V5.4 map redesign. The main manuscript uses five figures, but Figure 1 remains the reviewer-accepted geographic map from the separate interactive-map workflow and is intentionally not regenerated by the main `targets` pipeline. Figure 2 is a three-panel descriptive view of the full 80-editor interlocking network using identical coordinates: relational prominence/degree, completed gender, and continent. Figure 3 contains two aligned 100% composition panels: continent and gender. Panel A compares the full 2,033-editor population with the 80 interlocking editors; Panel B compares all 1,797 confidently NamSor-classified editors with the 69 confidently classified interlocking editors; the remaining 11 of the 80 interlocking editors are below the .70 NamSor confidence threshold and are excluded from this panel. These comparisons are descriptive; the inferential selection tests continue to use mutually exclusive interlocking versus non-interlocking groups. Figure 4 is the journal-community network, and Figure 5 contains the journal prominence/inequality panels. The former subregional main-text network remains retired. Gender and continent return only as separate descriptive panels on a shared topology, rather than being layered simultaneously with centrality in one overloaded encoding.

Figure 3 uses explicit segment coordinates rather than ggplot2's implicit stacking order. This prevents the label/category mismatch observed in V5.2 and guarantees that segment order, legend order, colors, and percentage labels remain synchronized. The categorical composition panels use a color-blind-safe Okabe-Ito palette; the continuous prominence scale in Figure 2 remains Viridis. Statistical significance is intentionally not encoded in Figure 3.


To regenerate only the main manuscript figures:

```bash
Rscript -e "targets::tar_validate()"
Rscript -e 'targets::tar_make(c(figure_2_plot, figure_3_plot, figure_4_plot, figure_5_plot))'
```

To rerun the complete analytical pipeline:

```bash
Rscript -e "targets::tar_make()"
```

### Pre-publication affiliation checks for v2.1.1

The definitive v2.1.1 rerun should use the reviewed restricted person-affiliation adjudication file in `private/multi_affiliation_adjudication.csv`. Generated adjudication candidate files under `output/selection/` are run-specific and are not publication inputs. Hong Kong SAR is harmonized as a separate UN M49 location before person-level adjudication; adjudication rows must therefore use the canonical label `Hong Kong SAR, China` rather than `China` for Hong Kong institutions. The University of Queensland / United Kingdom source-row anomaly remains pending historical source verification and must not be silently corrected.
