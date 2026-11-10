#!/bin/sh
# Githooks pre-commit hook: scan staged changes for secrets with gitleaks.
#
# Only the staged diff is scanned, never full history, so commits stay fast.
# Git runs this hook from the repository root, so gitleaks picks up the
# repo's .gitleaks.toml and .gitleaksignore. CI and `make check` remain the
# hard gates; this hook only catches a secret before it enters history.
#
# On a false positive, update .gitleaks.toml (allowlist) or add the finding's
# fingerprint to .gitleaksignore. Do not skip hooks.
set -eu

if ! command -v gitleaks >/dev/null 2>&1; then
	echo "gitleaks not found; skipping secret scan of staged changes." >&2
	echo "Install with:" >&2
	echo "  cd devtools/setup-dev/ansible && ./ensure.sh gitleaks" >&2
	exit 0
fi

exec gitleaks git --pre-commit --staged --redact --no-banner --verbose .
