# Private analytical inputs

The public repository intentionally excludes person-level empirical and adjudication files. For a local full-data run, place the restricted files at the paths configured in `config.yml`:

```text
private/Dataset_Editorial_Boards_All.xlsx
private/editorial_board_data.xlsx
private/confirmed_name_merges.csv
private/gender_classified_full.csv
private/gender_adjudication.csv
private/multi_affiliation_adjudication.csv
```

The person-affiliation adjudication file is optional under the primary `retain_missing` policy, but approved rows can resolve documented ties. Every approved row must select an institution-country pair observed in that person's harmonized appointment data; the pipeline stops if an adjudication invents a new pair.

These files are ignored by Git and must remain local. Do not attach them to a GitHub release. Before pushing, run `bash scripts/prepare_public_git.sh` and inspect `git status` plus `git diff --cached`.


If updating an older local checkout that still keeps restricted inputs under `data/`, run:

```bash
bash scripts/migrate_private_inputs.sh
```

The migration moves only known restricted empirical files and never stages them.
