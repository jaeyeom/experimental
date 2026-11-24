#!/bin/sh
# Runs the front-end unit tests with node's built-in test runner. Node is a
# host tool here, not a Bazel dependency. Fails, not skips, without node.
set -eu

if ! command -v node >/dev/null 2>&1; then
	echo "FAIL: node is required for the decide web front-end tests" >&2
	exit 1
fi

DIR=$(cd "$(dirname "$0")" && pwd)/static
for f in app api store sw dom keymap help_sheet inbox_model inbox_view swipe snooze_sheet text_render thread_model draft_sync sheet thread_view compose_model compose_view status_model status_view contacts_model contacts_view push_model settings_view; do
	node --check "$DIR/$f.js"
done
exec node --test "$DIR"/*_test.mjs
