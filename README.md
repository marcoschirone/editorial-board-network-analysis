# Editorial Board Network Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R Version](https://img.shields.io/badge/R-%E2%89%A54.5.0-blue.svg)](https://www.r-project.org/)

This repository contains the R workflow for analyzing interlocking editorial-board networks in a curated set of 30 sustainability science journals. The study distinguishes three analytical levels: editorial-board composition, selection into interlocking editorship, and relational prominence within the interlocking editor network.

Eigenvector centrality (EVC) is used as a network measure of relational editorial prominence. It is not treated as a direct measure of symbolic capital, prestige, authority, recognition, power, or a causal mechanism.

The public repository contains code, configuration, synthetic examples, and institution/geography rule tables. Person-level empirical inputs and generated person-level outputs are intentionally excluded.

## Current analytical summary

The current pipeline reconstructs:

- **2,135 raw rows**
- **2,122 person-journal appointments** after collapsing 13 duplicate role rows
- **2,033 unique persons** after identity resolution
- **80 interlocking editors** serving on at least two distinct journals
- **6 editors** serving on at least three distinct journals
- **80 editors / 942 edges** in the full editor co-membership network
- **78 editors / 941 edges** in the giant component
- **25 journals / 58 edges** in the full journal co-membership network (density = **0.193**)
- **22 journals / 56 edges** in the journal-network giant component (density = **0.242**)
- **1,086 distinct canonical institution labels** at person level, irrespective of country
- **1,092 canonical institution-country units** at person level, the unit used for leave-one-out institutional representation
- **19 persons** with unresolved or missing institution and **0** with unresolved or missing country

The principal Firth selection model includes **2,014 editors and all 80 interlocking events**. The adjusted association for leave-one-out institutional representation is **OR = 1.406, 95% CI [1.097, 1.796], p = .00737**. The adjusted Europe association is **OR = 2.074, 95% CI [1.324, 3.269], p = .00147**.

These values come from the current machine-generated results manifest.

## Repository structure

```text
.
├── R/
│   ├── utils.R
│   ├── data_processing.R
│   ├── institution_harmonization.R
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
│   ├── migrate_private_inputs.sh
│   └── check_sensitive_history.sh
├── private/                  # local only; ignored by Git
├── _targets.R
├── config.yml
├── install_packages.R
├── run.R
├── run_selection.R
├── editorial_network_analysis.Rproj
├── PRIVATE_INPUTS.md
├── PUBLIC_GIT_SAFETY.md
├── CHANGELOG.md
├── CITATION.cff
├── LICENSE
└── README.md
```

Generated results are written to `output/`, which is excluded from version control because several outputs contain person identifiers.

## Data and reproducibility

### Public repository contents

The public repository includes the complete analytical code, the geographic lookup used by the pipeline, institution-level harmonization rules, and a synthetic workbook that demonstrates the expected input schema.

`data/sample_editorial_board_data.xlsx` is illustrative only and does not reproduce the empirical results. The public institution-alias and source-correction tables contain institution-level rules and provenance, not person-level adjudications.

### Restricted empirical inputs

A full-data run requires local files under `private/`:

```text
private/Dataset_Editorial_Boards_All.xlsx
private/editorial_board_data.xlsx
private/confirmed_name_merges.csv
private/gender_adjudication.csv
private/gender_classified_full.csv
private/multi_affiliation_adjudication.csv
```

These files contain the full editorial-board population and/or person-level adjudication information and are excluded by `.gitignore`.

The public repository therefore documents and implements the complete computational workflow, but it is not sufficient by itself to reproduce the empirical results. The restricted `multi_affiliation_adjudication.csv` is required to reproduce the current full-data analysis exactly; if it is omitted under the `retain_missing` policy, unresolved affiliation ties remain missing and the selection-model results can differ.

See [PRIVATE_INPUTS.md](PRIVATE_INPUTS.md) for the local input layout.

## Public Git privacy gate

Before every public commit or release, run:

```bash
bash scripts/prepare_public_git.sh
```

The script stages the intended public surface and runs `scripts/check_public_privacy.R` against the exact indexed files. The privacy audit requires the local full-population workbook, compares indexed public files against the empirical editor-name list, checks person-specific URLs, and reports filenames only if a problem is found.

Optional local protection can be installed with:

```bash
bash scripts/install_privacy_precommit_hook.sh
```

Because removal from the current branch does not by itself remove historical Git objects, `scripts/check_sensitive_history.sh` separately checks for known restricted legacy paths in repository history.

See [PUBLIC_GIT_SAFETY.md](PUBLIC_GIT_SAFETY.md) for details.

## Person disambiguation

Identity resolution is performed before interlocking status is calculated. The workflow:

1. reads the full editorial-board population;
2. collapses duplicate person-journal role rows;
3. applies manually confirmed identity links from `private/confirmed_name_merges.csv`;
4. resolves connected identity components to one person identity;
5. recalculates the number of distinct journals per person; and
6. defines interlocking editors as people serving on at least two distinct journals.

The current confirmed-merge audit contains **11 confirmed pairwise links across 10 identity components**, producing **11 net person reductions** from 2,044 exact-name records to 2,033 persons. Identity resolution is deliberately conservative because false person merges can create spurious interlocks and network ties.

## Institution and affiliation harmonization

Institution processing is conservative and auditable:

1. reviewed source-level institution/country corrections are applied by exact institution-country match before geographic lookup;
2. country labels are canonicalized before UN M49 mapping;
3. raw institution values are retained for audit;
4. syntactic institution normalization ignores case, diacritics, punctuation, repeated whitespace, and a leading English article `The`;
5. substantive language variants and department-to-parent rollups are applied only through approved entries in `data/institution_aliases.csv`;
6. person-level institution and country are selected jointly from observed appointment-level institution-country pairs;
7. a unique modal pair is selected automatically;
8. tied top pairs are never broken by spreadsheet row order; ambiguous dimensions remain missing unless a documented private adjudication selects one of the observed harmonized pairs; and
9. leave-one-out institutional representation is counted by canonical institution **plus country**, not institution name alone.

The current full-data run contains **1,086 distinct canonical institution labels** and **1,092 institution-country units** at person level. These are different quantities and should not be used interchangeably.

The primary institutional predictor is the log-transformed leave-one-out count of other editors in the same canonical institution-country unit.

## Geographic classification

Country information is mapped to UN M49 continent and subregion classifications through `data/country_m49_lookup.csv`.

Hong Kong and Macao use separate statistical-area labels: `Hong Kong SAR, China` (M49 344) and `Macao SAR, China` (M49 446). Taiwan is retained as a separate affiliation location and assigned to Eastern Asia because it is not separately listed in the M49 lookup used here. China, Hong Kong SAR, Macao SAR, and Taiwan are therefore all assigned to Eastern Asia; this distinction affects country-level counts but not continent or subregion assignment.

Current continent counts are:

- Europe: **786**
- Asia: **571**
- Americas: **503**
- Oceania: **116**
- Africa: **57**

The dataset contains **79 affiliation locations** at the country/statistical-area level.

For continent, the primary omnibus test is a simulated Fisher exact test with **100,000 Monte Carlo replicates**: **p = .0525**. The asymptotic chi-square diagnostic is **χ²(4) = 10.316, p = .0354**; it is secondary because some expected counts are below 5. The M49 subregion omnibus Fisher test gives **p = .3315**.

Europe is treated as an exploratory focal contrast rather than a pre-specified omnibus result. The one-versus-rest enrichment estimate is **OR = 1.994, 95% CI [1.242, 3.221], p = .00317**, with Holm-adjusted **p = .0159**.

## Gender analysis

Primary gender analyses use NamSor consistently across the reconstructed full population and the interlocking subset, with low-confidence classifications excluded symmetrically.

Current primary counts among classifiable editors are:

- interlocking: **16 female / 53 male**
- non-interlocking: **554 female / 1,174 male**

The Fisher exact result is **OR = 0.640, 95% CI [0.338, 1.149], p = .146**. The mixed-instrument specification is retained only as a sensitivity analysis.

## Network construction and relational prominence

### Editor network

Nodes are interlocking editors. Two editors are connected when they share at least one journal board.

- Full network: **80 nodes, 942 edges**
- Giant component: **78 nodes, 941 edges**

### Journal network

Nodes are journals. Two journals are connected when they share at least one editor.

- Full network: **25 journals, 58 edges, density = 0.193**
- Giant component: **22 journals, 56 edges, density = 0.242**

### Relational prominence

The primary network measure is weighted eigenvector centrality (EVC). EVC is interpreted as relational prominence within the observed network, not as a direct measure of symbolic capital, prestige, authority, recognition, power, or causal influence.

Current giant-component summary:

- Median EVC: **0.3635**
- Gini coefficient of EVC: **0.389**

Weighted shortest-path measures convert tie strength to distance before betweenness and closeness are calculated.

## Community detection

Editor and journal communities are analyzed separately with Leiden.

### Editor network

The editor-community analysis uses the configured candidate resolution grid from 0.1 to 2.0 in increments of 0.1. Leiden optimizes the Constant Potts Model (CPM) objective at each candidate resolution; weighted Newman-Girvan modularity is used to compare the resulting partitions across the grid.

Current solution:

- Selected resolution: **0.20**
- Weighted modularity: **0.412**
- Communities: **6**

### Journal network

The journal network uses its separately configured fixed resolution.

Current solution:

- Resolution: **0.50**
- Weighted modularity: **0.295**
- Communities: **9**
- Largest community: **9 journals**

The editor and journal partitions are distinct analytical objects and do not share the same resolution-selection procedure.

## Selection into interlocking editorship

The primary selection model uses Firth penalized logistic regression and benchmarks interlocking editors against the full population.

Complete-case model:

- **n = 2,014**
- **80 events**
- McFadden pseudo-R² = **0.0237**
- LR χ²(2) = **15.982**, **p = .000339**

Adjusted associations:

- Europe: **OR = 2.074, 95% CI [1.324, 3.269], p = .00147**
- Log leave-one-out institutional representation: **OR = 1.406, 95% CI [1.097, 1.796], p = .00737**

A missingness-indicator sensitivity model retains all 2,033 editors and yields the same institutional estimate to the reported precision.

These are cross-sectional associations and are not interpreted causally. Institutional representation is not treated as institutional prestige.

## Network-position permutation tests

Attribute-permutation tests hold the observed giant-component network fixed and randomly reassign the focal attribute labels. The current pipeline uses **100,000 permutations**.

- Europe vs. other editors, EVC difference: **p = .466**
- Female vs. male, EVC difference: **p = .464**

These tests concern prominence among interlocking editors and are conceptually distinct from the full-population selection model.

## Journal-level prominence and inequality

Journal-level prominence is summarized by the median EVC of eligible editors belonging to each journal. Within-board inequality is measured with a finite-sample-corrected Gini coefficient.

The primary typology includes **20 journals** with at least two eligible editors and uses sample median splits:

- Median EVC threshold: **0.299**
- Corrected Gini threshold: **0.366**
- High prominence / High inequality: **3 journals**
- High prominence / Low inequality: **7 journals**
- Low prominence / High inequality: **7 journals**
- Low prominence / Low inequality: **3 journals**

Raw-Gini classification is retained as a sensitivity analysis.

## Robustness analyses

The pipeline includes threshold sensitivity, bootstrap confidence intervals, alternative centrality correlations, component sensitivity, Leiden resolution sensitivity, board-level sensitivity, attribute-permutation tests, and direct bipartite robustness analysis.

Projected-network EVC is compared with HITS and SVD-based centrality calculated directly from the editor × journal incidence matrix. In the current giant component:

- EVC vs. HITS: **Spearman's ρ = 0.9985, p = 2.05 × 10⁻97, n = 78**
- EVC vs. SVD: **Spearman's ρ = 0.9985, p = 2.05 × 10⁻97, n = 78**
- HITS vs. SVD: **Spearman's ρ = 1.000, n = 78**

These correlations support robustness of relative prominence rankings to projected versus bipartite representations; they do not imply mathematical equivalence of the measures. Spearman tests use approximate p-values when tied ranks prevent exact computation. Structurally equivalent editors have mathematically identical centrality scores; before any rank-based procedure (Spearman, Wilcoxon, Kruskal-Wallis, percentile ranks), scores are rounded to 10 significant digits (`tie_stable()` in `R/utils.R`) so that these ties are preserved rather than broken by platform-dependent floating-point noise. The tie grouping is checked on every run: `output/robustness/tie_structure_check.csv` reports the number of distinct EVC values at 4 to 14 significant digits (currently 47 of 78 at every precision), and the manifest records the result in its `tie_structure` section.

## Figures

The main manuscript uses five figures. Figure 1 is the geographic map maintained by a separate map workflow and is not regenerated by this `{targets}` pipeline. Figures 2–5 are generated here:

- **Figure 2:** three descriptive panels using one fixed layout of the full 80-editor interlocking network: relational prominence/degree, completed gender, and continent;
- **Figure 3:** descriptive composition comparisons for continent and confidently classified gender;
- **Figure 4:** journal-community network, with singleton communities given a common neutral display treatment without changing the analytical partition;
- **Figure 5:** journal-network panels for median member EVC and within-board inequality.

To regenerate these figures:

```bash
Rscript -e 'targets::tar_make(c(figure_2_plot, figure_3_plot, figure_4_plot, figure_5_plot))'
```

## Requirements

Recommended R version:

```text
R >= 4.5.0
```

Install required packages with:

```bash
Rscript install_packages.R
```

The workflow uses packages including `tidyverse`, `igraph`, `ggraph`, `readxl`, `openxlsx`, `targets`, `tarchetypes`, `config`, `ineq`, `patchwork`, `viridis`, `forcats`, `here`, `RColorBrewer`, `Matrix`, `irlba`, `sessioninfo`, and `logistf`.

## Running the analysis

With the restricted empirical inputs in the locations specified in `config.yml`:

```bash
Rscript install_packages.R
Rscript run.R
```

The pipeline can also be invoked directly:

```r
library(targets)
tar_validate()
tar_make()
```

For a completely fresh local rebuild:

```bash
rm -rf _targets
rm -rf output
Rscript run.R
```

A successful current full-data run should reconstruct:

```text
2033 persons
2122 appointments
80 interlocking editors
6 editors with >=3 journals
69/80 interlocking editors confidently classified by NamSor
```

## Configuration

Principal paths are configured in `config.yml`:

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

Other parameters control layout seeds, network thresholds, community detection, simulation counts, and robustness settings.

## Generated outputs

`output/` is regenerated locally and is not version-controlled. It contains manuscript-facing summary outputs as well as person-level audit files, so it should not be attached wholesale to a public release.

Principal generated outputs include:

```text
output/manuscript_results_manifest.csv
output/journal_metrics.csv
output/inequality_measures.csv
output/main_analysis/
output/robustness/
output/selection/
output/tables/
```

The selection directory contains person-level analytical and audit files. Treat it as local analytical output, not as part of the public repository surface.

## Citation

For software citation, use the metadata in [`CITATION.cff`](CITATION.cff). A release-specific DOI should be added only after the corresponding archive has been created.

The associated manuscript is under revision; repository metadata intentionally do not hard-code a provisional manuscript title.

Package citations for a full local run are generated in `output/R-packages.bib`.

## License

This software is released under the [MIT License](LICENSE).

The license applies to the software and documentation in this repository. It does not grant redistribution rights for restricted or third-party source data that are not included here.

## Contact

**Marco Schirone**  
Swedish School of Library and Information Science, University of Borås  
Email: marco.schirone@hb.se  
ORCID: https://orcid.org/0000-0002-4166-153X

## Acknowledgments

The author thanks Prof. Björn Hammarfelt and Assoc. Prof. Gustaf Nelhans for supervision and support during this research, and Dr. Jens Peter Andersen, Assoc. Prof. Jonas Lindahl, and Assoc. Prof. David Gunnarsson Lorentzen for comments on earlier versions of the manuscript. The author also thanks the anonymous peer reviewers for constructive feedback.

## References

- Newman, M. (2018). *Networks* (2nd ed.). Oxford University Press.
- Traag, V. A., Waltman, L., & van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing well-connected communities. *Scientific Reports*, 9(1), 5233.

---

**Last updated:** 2026-09-29  
**Pipeline version:** 2.1.2
