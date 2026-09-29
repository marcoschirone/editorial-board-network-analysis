# Changelog

## 2.1.4 — 2026-09-29

- Integrates the existing board-size sensitivity analysis into the `{targets}` pipeline and manuscript results manifest.
- Adds aggregate NamSor validation against independently completed interlocking-editor labels to the pipeline and manifest.
- Adds a manifest completeness check so a full run fails if either manuscript-facing validation section is absent.
- No changes to the primary selection model, network construction, centrality values, community detection, inequality measures, or permutation tests.

## 2.1.2 — 2026-09-29

- Rank-based statistics now preserve mathematically tied centrality scores. Structurally equivalent editors have identical scores, but eigenvector routines return them with platform-dependent floating-point noise; `tie_stable()` rounds scores to 10 significant digits immediately before Spearman, Wilcoxon, Kruskal-Wallis, and percentile-rank computations. Centrality values themselves, graph construction, Leiden, Gini, bootstrap intervals, typology thresholds, and permutation tests are unchanged. Affected rank statistics: EVC vs. degree ρ = .906, EVC vs. closeness ρ = .832, EVC vs. betweenness ρ = .296, full- vs. giant-component EVC ρ = 1.000, EVC vs. HITS/SVD ρ = .9985. v2.1.1 is superseded for these values.
- Added a machine-generated tie-structure check (`tie_structure_check()`; `output/robustness/tie_structure_check.csv` and manifest section `tie_structure`) confirming that EVC tie groups are identical at 4 to 14 significant digits.
- Code comments and console messages revised for accuracy and plain wording; comments now describe the post hoc status of the Europe contrast and the current table numbering consistently with the manuscript. `run_comprehensive_robustness()` renamed `run_robustness_analyses()`. No analytical output changes.
- Figure 3 percentage labels use conventional half-up rounding (`percent_half_up()`), matching the manuscript tables (e.g., 17/80 = 21.3%).

## 2.1.1 — 2026-09-29

- Harmonized institution names conservatively through approved alias and parent-institution rules.
- Counted institutional representation by canonical institution-country unit rather than institution name alone.
- Removed spreadsheet row order as a fallback for tied person-level affiliations.
- Added documented person-level affiliation adjudication support while keeping adjudication data private.
- Standardized Hong Kong SAR and Macao SAR as separate UN M49 statistical areas within Eastern Asia.
- Added reviewed source-level institution/country corrections with provenance.
- Increased simulated Fisher tests and attribute-permutation tests to 100,000 replicates.
- Exported journal-network densities directly from the pipeline.
- Made tied-rank Spearman handling explicit in the bipartite robustness analysis.
- Added public-repository privacy checks and moved restricted empirical inputs to an ignored `private/` directory.
- Removed person-level generated outputs from the public release surface.
- Updated documentation to the current analytical results.
