"""Lint aspect definitions for the repository."""

load("@aspect_rules_lint//lint:lint_test.bzl", "lint_test")
load("@aspect_rules_lint//lint:ruff.bzl", "lint_ruff_aspect")
load("@aspect_rules_lint//lint:shellcheck.bzl", "lint_shellcheck_aspect")

# Ruff linter for Python files
# Visits py_binary, py_library, py_test, and filegroups tagged "python" or "lint-with-ruff"
ruff = lint_ruff_aspect(
    args = ["--no-fix"],
    binary = "@aspect_rules_lint//lint:ruff_bin",
    configs = [Label("//:pyproject.toml")],
)

ruff_test = lint_test(aspect = ruff)

# ShellCheck linter for shell scripts
# Visits sh_binary, sh_library, and sh_test rules
shellcheck = lint_shellcheck_aspect(
    args = ["-x"],
    binary = "@aspect_rules_lint//lint:shellcheck_bin",
    config = Label("//:.shellcheckrc"),
)

shellcheck_test = lint_test(aspect = shellcheck)
