#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"
mkdir -p private

files=(
  Dataset_Editorial_Boards_All.xlsx
  editorial_board_data.xlsx
  confirmed_name_merges.csv
  gender_adjudication.csv
  gender_classified_full.csv
  multi_affiliation_adjudication.csv
  interlocking_gender_adjudication_audit_80.csv
)

moved=0
for f in "${files[@]}"; do
  if [[ -f "data/$f" && ! -f "private/$f" ]]; then
    mv "data/$f" "private/$f"
    echo "Moved data/$f -> private/$f"
    moved=$((moved + 1))
  elif [[ -f "data/$f" && -f "private/$f" ]]; then
    echo "Skipped data/$f because private/$f already exists" >&2
  fi
done

echo "Private-input migration complete: $moved file(s) moved."
