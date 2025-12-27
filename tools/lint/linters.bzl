"""Lint aspect definitions for the repository."""

load("@aspect_rules_lint//lint:shellcheck.bzl", "lint_shellcheck_aspect")

# ShellCheck linter for shell scripts
# Visits sh_binary, sh_library, and sh_test rules
shellcheck = lint_shellcheck_aspect(
    binary = "@aspect_rules_lint//lint:shellcheck",
    config = Label("//:.shellcheckrc"),
)
