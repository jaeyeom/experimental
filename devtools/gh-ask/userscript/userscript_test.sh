#!/bin/sh
# Runs gh-ask.test.js with node's built-in test runner. Node is a host
# tool here, not a Bazel dependency. The test fails, not skips, when node
# is missing, so a missing node never hides a broken userscript.
set -eu

if ! command -v node >/dev/null 2>&1; then
	echo "FAIL: node is required for the gh-ask userscript test" >&2
	echo "Install with: cd devtools/setup-dev/ansible && ./ensure.sh npm" >&2
	exit 1
fi

DIR=$(cd "$(dirname "$0")" && pwd)
node --check "$DIR/gh-ask.user.js"
exec node --test "$DIR/gh-ask.test.js"
