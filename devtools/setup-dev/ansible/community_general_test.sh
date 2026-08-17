#!/bin/sh
# Tests for community.general collection install policy helpers.
set -eu

fail() {
    echo "FAIL: $1" >&2
    exit 1
}

assert_eq() {
    _label="$1"
    _got="$2"
    _want="$3"
    if [ "$_got" != "$_want" ]; then
        fail "$_label: got '$_got', want '$_want'"
    fi
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
sh -n "$SCRIPT_DIR/community_general.sh"
sh -n "$SCRIPT_DIR/community_general_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/community_general.sh"

# --- community_general_spec_for_core ---

assert_eq "2.15 pin" \
    "$(community_general_spec_for_core "2.15.3")" \
    "community.general:>=10.0.0,<11.0.0"
assert_eq "2.16 pin" \
    "$(community_general_spec_for_core "2.16.14")" \
    "community.general:>=11.0.0,<12.0.0"
assert_eq "2.17 pin" \
    "$(community_general_spec_for_core "2.17.0")" \
    "community.general:>=12.0.0,<13.0.0"
assert_eq "2.18 pin" \
    "$(community_general_spec_for_core "2.18.9")" \
    "community.general:>=13.0.0,<14.0.0"
assert_eq "2.19 unpinned" \
    "$(community_general_spec_for_core "2.19.1")" \
    "community.general"
assert_eq "empty core unpinned" \
    "$(community_general_spec_for_core "")" \
    "community.general"

# --- community_general_has_bundled ---
# Only the collection next to the *active* ansible python tree counts.

if community_general_has_bundled ""; then
    fail "empty python module location must not count as bundled"
fi

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

_active="$_tmp/pip/ansible"
_leftover="$_tmp/dist-packages/ansible_collections/community/general"
mkdir -p "$_active" "$_leftover"
if community_general_has_bundled "$_active"; then
    fail "leftover apt collection must not count when active tree has none"
fi

mkdir -p "$_tmp/pip/ansible_collections/community/general"
if ! community_general_has_bundled "$_active"; then
    fail "collection next to active ansible python module must count"
fi

# --- community_general_plan ---
# Args: OS TERMUX UPGRADED HAS_APT HAS_BUNDLED CORE
# TERMUX/UPGRADED/HAS_APT/HAS_BUNDLED are 0 or 1.

assert_eq "darwin latest" \
    "$(community_general_plan Darwin 0 0 0 0 "2.16.3")" \
    "install community.general"
assert_eq "darwin after upgrade forces latest" \
    "$(community_general_plan Darwin 0 1 0 0 "2.16.3")" \
    "install community.general --force"
assert_eq "termux latest even if apt present" \
    "$(community_general_plan Linux 1 0 1 1 "2.16.3")" \
    "install community.general"
assert_eq "termux after upgrade forces latest" \
    "$(community_general_plan Linux 1 1 0 0 "2.18.0")" \
    "install community.general --force"

assert_eq "apt with bundled collection uses it" \
    "$(community_general_plan Linux 0 0 1 1 "2.16.3")" \
    "use_bundled"
assert_eq "apt without bundled pins for 2.16" \
    "$(community_general_plan Linux 0 0 1 0 "2.16.3")" \
    "install community.general:>=11.0.0,<12.0.0 --force"
assert_eq "apt without bundled pins for 2.15" \
    "$(community_general_plan Linux 0 0 1 0 "2.15.0")" \
    "install community.general:>=10.0.0,<11.0.0 --force"
assert_eq "apt upgrade ignores leftover bundled and pins with force" \
    "$(community_general_plan Linux 0 1 1 1 "2.18.1")" \
    "install community.general:>=13.0.0,<14.0.0 --force"
assert_eq "apt upgrade without bundled still pins with force" \
    "$(community_general_plan Linux 0 1 1 0 "2.17.5")" \
    "install community.general:>=12.0.0,<13.0.0 --force"

assert_eq "other linux latest" \
    "$(community_general_plan Linux 0 0 0 0 "2.16.3")" \
    "install community.general"
assert_eq "other linux after upgrade forces latest" \
    "$(community_general_plan Linux 0 1 0 0 "2.16.3")" \
    "install community.general --force"

# --- community_general_needs_repair ---
# Args: HAS_APT TERMUX OS HAS_BUNDLED HAS_USER_GALAXY UPGRADED

if ! community_general_needs_repair 1 0 Linux 1 1 0; then
    fail "debian with bundled + user galaxy should need repair"
fi
if community_general_needs_repair 1 0 Linux 1 1 1; then
    fail "after pip upgrade, do not repair (install instead)"
fi
if community_general_needs_repair 1 0 Linux 1 0 0; then
    fail "no user galaxy: nothing to repair"
fi
if community_general_needs_repair 1 0 Linux 0 1 0; then
    fail "no bundled collection: do not delete user galaxy"
fi
if community_general_needs_repair 0 0 Linux 1 1 0; then
    fail "no apt: not a debian repair case"
fi
if community_general_needs_repair 1 1 Linux 1 1 0; then
    fail "termux: not a debian repair case"
fi
if community_general_needs_repair 1 0 Darwin 1 1 0; then
    fail "darwin: not a debian repair case"
fi

echo "All community_general policy tests passed."
