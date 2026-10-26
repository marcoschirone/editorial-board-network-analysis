# Changelog

## 2.1.8 — 2026-10-26

- **Data change — institution harmonization.** `data/institution_aliases.csv` gains one approved `campus_rollup` entry: "University of California Davis" → "University of California" (United States). In the source workbook every other University of California campus affiliation is recorded at system level in `Affiliation_1`; this row was the only exception and had been counted as a separate institution. University of California now has 25 editors (previously 24 + 1); person-level institution–country units 1,091 (previously 1,092); canonical institution labels 1,085 (previously 1,086).
- Results affected (Firth selection model only): institutional representation OR = 1.403, 95% CI [1.095, 1.791], p = .00762 (previously 1.406 [1.097, 1.796], p = .00737); exploratory Europe OR = 2.076 [1.325, 3.273], p = .00145 (previously 2.074 [1.324, 3.269], p = .00147); LR χ²(2) = 15.920, p = .000349. Network construction, centralities, communities, permutation tests, gender and geography results are unchanged.
- Figure 2 (display only): components disconnected from the giant component (the two-editor dyad) are placed next to it rather than at the Fruchterman–Reingold position, so the giant component fills more of each panel. Panel A title and legend use "relational editorial prominence"; colour-bar breaks 0, 0.5, 1.
- Verified: an unchanged v2.1.7 rebuild reproduced all 266 manifest values before these changes were applied.

## 2.1.7 — 2026-10-18

- Figures 4 and 5: journal labels use the official titles (Earth System Governance; Environment, Development and Sustainability; International Journal of Sustainable Development & World Ecology). Display only; stored names, exported tables and the results manifest are unchanged.
- Figure 5: panel tags in upper case (A, B), consistent with Figures 2 and 3.
- Figure 4: the legend lists the multi-journal communities from largest to smallest (display only).
- CHANGELOG: 2.1.3 entry corrected.
- **Analytical change — journal communities.** The journal-network Leiden resolution is now selected by the same rule as the editor network (highest weighted modularity across the configured grid 0.1–2.0; ties: smallest resolution), run separately on the journal graph. This replaces the fixed `journal_leiden_resolution: 0.5`, which is removed from `config.yml`. Selected resolution 0.2: Q = 0.335, six communities (three singletons), largest community 10 journals (previously Q = 0.295, nine communities, six singletons). Global Environmental Change, Sustainable Development and International Journal of Precision Engineering and Manufacturing-Green Technology now join the three multi-journal communities. Figure 4, `output/communities/journal_*` and the manifest rows `leiden_journal` change; no other result depends on the journal partition. Manifest gains `leiden_journal / selection`.
- New `run_journal_resolution_sweep.R`: sensitivity of the journal-network Leiden partition to the resolution parameter (same procedure as `run_journal_leiden()`, configured grid 0.1–2.0), compared with the selected partition. Writes `output/communities/journal_resolution_sweep.csv`.

## 2.1.6 — 2026-10-16

- Results manifest: the Europe coefficient of the Firth selection model (`selection_model` rows `model_europe_*`) is now labelled `exploratory/focal` instead of `primary`, matching its post hoc status (Europe was selected after inspecting the continent residuals). The institutional-representation coefficient remains `primary`.
- README marks the adjusted Europe association as exploratory.
- No analytical changes: all manifest values are unchanged.

## 2.1.5 — 2026-10-15

- Adds `run_exchangeability_audit()` for the Europe and gender EVC attribute-permutation analyses.
- Adds `run_exchangeability_audit.R` so the diagnostic can be rerun from current `output/editor_metrics.csv` without rebuilding the full pipeline.
- Diagnoses attribute clustering across editor communities using a Monte Carlo Fisher test and Cramer's V.
- Adds a within-community attribute-permutation sensitivity analysis that preserves focal/non-focal counts within each detected editor community while holding EVC and the observed network fixed.
- Records mixed/homogeneous community counts and the log10 size of the unrestricted and constrained permutation spaces.
- Exports selected exchangeability diagnostics to the manuscript results manifest.
- The new analysis is explicitly labelled diagnostic/sensitivity only; it does not replace the primary unrestricted attribute-permutation tests and does not claim to prove exchangeability.
- No changes to network construction, centrality calculation, community detection, selection models, or primary permutation-test code.
- `public_results/` updated with the regenerated manifest and the aggregate exchangeability tables; `scripts/prepare_public_git.sh` now stages `run_exchangeability_audit.R` and `public_results/`.
- Clean end-to-end rerun (2026-10-15): all previously reported manifest values unchanged; eight exchangeability rows added.

## 2.1.4 — 2026-09-29

- Integrates the existing board-size sensitivity analysis into the `{targets}` pipeline and manuscript results manifest.
- Adds aggregate NamSor validation against independently completed interlocking-editor labels to the pipeline and manifest.
- Adds a manifest completeness check so a full run fails if either manuscript-facing validation section is absent.
- No changes to the primary selection model, network construction, centrality values, community detection, inequality measures, or permutation tests.

## 2.1.3 — 2026-09-29

- Version-number update only (commit 25feb37): CITATION.cff and README set to 2.1.3 in preparation for the public release corresponding to the revised manuscript.
- No analytical changes relative to 2.1.2 (commit ca93c8d).
- Tagged and released on GitHub, but Zenodo archiving of the release failed (no DOI); superseded the same day by 2.1.4. The tag is no longer on GitHub.

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
