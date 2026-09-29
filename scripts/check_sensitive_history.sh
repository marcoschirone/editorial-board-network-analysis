#!/usr/bin/env bash
set -euo pipefail

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "ERROR: run from inside the Git repository." >&2
  exit 1
fi

paths=(
  data/interlocking_gender_adjudication_audit_80.csv
  data/confirmed_name_merges.csv
  data/multi_affiliation_adjudication.csv
  data/gender_adjudication.csv
  data/gender_classified_full.csv
  data/Dataset_Editorial_Boards_All.xlsx
  data/editorial_board_data.xlsx
)

found=0
for p in "${paths[@]}"; do
  if git log --all --format='%H' -- "$p" | grep -q .; then
    echo "HISTORY WARNING: restricted path appears in Git history: $p"
    found=1
  fi
done

if [[ "$found" -eq 0 ]]; then
  echo "No known restricted legacy paths were found in Git history."
else
  echo
  echo "Current-branch cleanup does not erase old commits from GitHub."
  echo "If complete historical removal is required, rewrite history with a dedicated tool"
  echo "(for example git-filter-repo), force-push all rewritten refs, and rotate/recreate releases as appropriate."
  exit 5
fi
