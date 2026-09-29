# Public Git safety gate

The analytical working copy contains person-level empirical inputs and local audit outputs. Those materials are required for the private analytical run but are not part of the public repository.

## Local/private layout

Restricted empirical inputs live under `private/`. Generated outputs live under `output/`. Both directories are ignored by Git. Legacy restricted paths under `data/` are also ignored so an older working copy cannot accidentally re-stage them.

## Before every public commit or release

Run:

```bash
bash scripts/prepare_public_git.sh
```

The script:

1. removes known restricted current and legacy paths from the Git index while preserving local files;
2. stages only the intended public workflow, documentation, synthetic sample, and institution/geography rule tables;
3. runs `scripts/check_public_privacy.R` against the exact indexed file set.

The privacy audit requires the local full-population workbook. It compares all indexed public text/workbook content against the empirical editor-name list and also checks person-specific URLs. It reports filenames only and never prints editor names. If the private population workbook is unavailable, the release audit fails rather than silently skipping the name scan.

Always inspect:

```bash
git status
git diff --cached --stat
git diff --cached
```

Optional local protection:

```bash
bash scripts/install_privacy_precommit_hook.sh
```

This installs the same privacy audit as a pre-commit hook in the local clone.

## Git history

Removing a restricted file from the current branch does not remove it from earlier commits. Before stating that the public repository contains no person-level material anywhere in its history, run:

```bash
bash scripts/check_sensitive_history.sh
```

If it reports a restricted historical path, a separate history-rewrite procedure is required. Do not run a history rewrite casually: it changes commit hashes and requires coordinated force-pushing of rewritten refs.

## Public rule tables

`data/institution_aliases.csv` and `data/institution_country_overrides.csv` are public only because they contain institution-level rules and provenance, not person-level adjudications. Evidence URLs in those tables must point to institution- or organization-level sources rather than person-specific profile URLs whenever possible.
