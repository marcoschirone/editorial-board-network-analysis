# Affiliation harmonization correction (R2 working update)

This development update removes row-order dependence and institution-name collisions from the person-level institutional-representation analysis. It is a methods correction, not a change to the editor-network identity definition.

## Current affiliation workflow

1. Reviewed source-level institution/country corrections are applied by exact institution-country match before geographic lookup.
2. Country labels are canonicalized. Hong Kong SAR and Macao SAR are retained as separate UN M49 statistical areas (`Hong Kong SAR, China`, code 344; `Macao SAR, China`, code 446), both in Eastern Asia. Taiwan remains a separate affiliation location assigned to Eastern Asia.
3. Institution strings retain their raw values for audit. Conservative syntactic normalization ignores case, diacritics, punctuation, repeated whitespace, and a leading English article.
4. Substantive institution equivalences are applied only through approved entries in `data/institution_aliases.csv`. Aliases may be country-scoped to prevent same-name institutions in different countries from being merged.
5. Person-level institution and country are selected jointly from observed appointment-level pairs. A unique modal pair is selected automatically.
6. Tied top pairs are never broken by spreadsheet order. Under the primary `retain_missing` policy, only dimensions common to all tied top pairs are retained; ambiguous dimensions remain missing unless an approved private adjudication selects one of the observed harmonized pairs.
7. Leave-one-out institutional representation is counted on the joint canonical `(Institution, Country)` unit, not institution name alone.

## Audit outputs

The pipeline writes local audit files to `output/selection/`, including institution normalization, source overrides, person-affiliation pair/status audits, country counts, institution-country counts, and publication-readiness diagnostics. These outputs can contain person identifiers and are never part of the public Git surface.

## Publication hold

A source-row anomaly remains marked `pending_review` in the public institution-country override table. Pending rows are not applied. `run.R` reports a publication hold until every pending source-level override has been resolved or explicitly retired. A corrected analytical run may still be used for comparison while the hold is active.

## Privacy architecture

All empirical files containing editor names or person-level adjudications are local-only under `private/`. `output/` and `private/` are ignored by Git. Before a public commit, `scripts/prepare_public_git.sh` removes known restricted legacy files from the Git index, stages only the public workflow, and runs `scripts/check_public_privacy.R`. The privacy check compares indexed public content against the full private editor-name list and fails without printing names if a match is found.

Historical commits are a separate issue: deleting a sensitive file from the current branch does not erase earlier commits. `scripts/check_sensitive_history.sh` reports known restricted paths that remain in Git history so they can be addressed before claiming that no person-level material is present anywhere in the public repository history.

## Results to compare after the corrected rerun

At minimum compare the corrected run with v2.1.0 for:

- canonical institution-country unit count;
- top institution-country counts at person and appointment level;
- number of unresolved institutional assignments;
- complete-case sample size for the Firth model;
- leave-one-out institutional representation values;
- Firth coefficient, OR, confidence interval, and p-value for `log_inst_loo`;
- Europe coefficient;
- harmonized China/Hong Kong SAR/Macao SAR country counts;
- Table 3 and all institutional or country-level descriptive text.

Network identity and topology quantities (2,033 persons, 80 interlocking editors, editor-network edges, EVC, and Leiden communities) should remain invariant. Any change in those quantities should be investigated before manuscript updates.
