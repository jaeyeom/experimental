#!/usr/bin/env bash
# Hermetic test verifying that //tools/format:go_format detects and fixes both
# unformatted Go imports (goimports) and gofumpt style violations.
set -euo pipefail

go_format_bin="$(realpath "$1")"

workdir="$(mktemp -d "${TEST_TMPDIR:-/tmp}/go_format_test.XXXXXX")"
trap 'rm -rf "$workdir"' EXIT

# 1. File with unsorted/ungrouped imports (valid gofumpt, invalid goimports).
cat >"$workdir/bad_imports.go" <<'EOF'
package example

import (
	"strings"
	"github.com/example/pkg"
	"fmt"
)

func Use() {
	fmt.Println(strings.ToUpper(pkg.Name))
}
EOF

# 2. File with gofumpt-only violation (extra blank line at start of function).
cat >"$workdir/bad_gofumpt.go" <<'EOF'
package example

import "fmt"

func ExtraBlank() {

	fmt.Println("hi")
}
EOF

# 3. Clean file.
cat >"$workdir/clean.go" <<'EOF'
package example

import (
	"fmt"
	"strings"

	"github.com/example/pkg"
)

func Clean() {
	fmt.Println(strings.ToUpper(pkg.Name))
}
EOF

out="$("$go_format_bin" -l "$workdir/bad_imports.go" "$workdir/bad_gofumpt.go" "$workdir/clean.go")"
if ! printf '%s\n' "$out" | grep -qxF "$workdir/bad_imports.go"; then
  echo "FAIL: expected go_format -l to report bad_imports.go, got:" >&2
  printf '%s\n' "$out" >&2
  exit 1
fi
if ! printf '%s\n' "$out" | grep -qxF "$workdir/bad_gofumpt.go"; then
  echo "FAIL: expected go_format -l to report bad_gofumpt.go, got:" >&2
  printf '%s\n' "$out" >&2
  exit 1
fi
if printf '%s\n' "$out" | grep -qxF "$workdir/clean.go"; then
  echo "FAIL: did not expect go_format -l to report clean.go, got:" >&2
  printf '%s\n' "$out" >&2
  exit 1
fi

# Fix both files in-place and confirm -l now reports nothing.
"$go_format_bin" -w "$workdir/bad_imports.go" "$workdir/bad_gofumpt.go" "$workdir/clean.go"

after="$("$go_format_bin" -l "$workdir/bad_imports.go" "$workdir/bad_gofumpt.go" "$workdir/clean.go")"
if [[ -n "$after" ]]; then
  echo "FAIL: expected no unformatted files after go_format -w, got:" >&2
  printf '%s\n' "$after" >&2
  exit 1
fi
