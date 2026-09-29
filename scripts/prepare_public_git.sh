#!/usr/bin/env bash
set -euo pipefail

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "ERROR: run this script from inside the Git repository." >&2
  exit 1
fi

# Remove restricted files from the Git index if an older release ever tracked
# them. --cached preserves local working copies. This also stages deletion of
# legacy person-level files that may still be tracked on main.
git rm -r --cached --ignore-unmatch \
  output _targets private private_data \
  data/Dataset_Editorial_Boards_All.xlsx \
  data/editorial_board_data.xlsx \
  data/confirmed_name_merges.csv \
  data/gender_adjudication.csv \
  data/gender_classified_full.csv \
  data/multi_affiliation_adjudication.csv \
  data/interlocking_gender_adjudication_audit_80.csv \
  'data/person_affiliation_adjudication_*.csv' \
  'data/*_person_level_*.csv' \
  AFFILIATION_HARMONIZATION_UPDATE.md \
  FIGURE_REDESIGN_UPDATE.md \
  >/dev/null 2>&1 || true

# Stage only the intended public surface. Do not replace this with `git add -A`
# in the release workflow.
git add \
  .gitignore .gitattributes \
  R README.md config.yml _targets.R \
  data/country_m49_lookup.csv \
  data/institution_aliases.csv \
  data/institution_country_overrides.csv \
  data/multi_affiliation_adjudication_example.csv \
  data/sample_editorial_board_data.xlsx \
  scripts PUBLIC_GIT_SAFETY.md PRIVATE_INPUTS.md CHANGELOG.md \
  CITATION.cff LICENSE \
  editorial_network_analysis.Rproj install_packages.R run.R run_selection.R

# Audit the exact indexed file set before commit.
Rscript scripts/check_public_privacy.R

echo "Public Git safety checks passed."
echo "Review both commands before committing:"
echo "  git status"
echo "  git diff --cached --stat"
echo "  git diff --cached"
