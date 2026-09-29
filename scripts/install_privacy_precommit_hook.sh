#!/usr/bin/env bash
set -euo pipefail

root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "$root" ]]; then
  echo "ERROR: run from inside the Git repository." >&2
  exit 1
fi

hook="$root/.git/hooks/pre-commit"
cat > "$hook" <<'HOOK'
#!/usr/bin/env bash
set -euo pipefail
Rscript scripts/check_public_privacy.R
HOOK
chmod +x "$hook"
echo "Installed privacy pre-commit hook: $hook"
