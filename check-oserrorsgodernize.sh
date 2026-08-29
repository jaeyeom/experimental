#!/bin/bash
# Fail if oserrorsgodernize is missing, built against an old golang.org/x/tools,
# or built with a different Go major.minor than the current toolchain.
# Go 1.27 fatals with "package without types" when the analyzer uses x/tools
# older than v0.49.0 (see #283). A binary built with an older Go can also fail
# to load current stdlib packages (e.g. math/rand/v2).

set -euo pipefail

MIN_XTOOLS="${OSERRORS_MIN_XTOOLS:-v0.49.0}"

install_hint() {
	echo "Reinstall with:" >&2
	echo "  go install github.com/jaeyeom/godernize/oserrors/cmd/oserrorsgodernize@latest" >&2
	echo "or:" >&2
	echo "  cd devtools/setup-dev/ansible && ./ensure.sh oserrorsgodernize" >&2
}

# go1.26.7 -> go1.26
go_major_minor() {
	sed -n 's/^go\([0-9][0-9]*\.[0-9][0-9]*\).*/go\1/p' <<<"$1"
}

# Returns 0 if $1 < $2 (dotted versions, optional leading v).
version_lt() {
	local a=${1#v}
	local b=${2#v}
	local a1 a2 a3 b1 b2 b3
	IFS=. read -r a1 a2 a3 _ <<<"${a}..."
	IFS=. read -r b1 b2 b3 _ <<<"${b}..."
	a1=${a1:-0}
	a2=${a2:-0}
	a3=${a3:-0}
	b1=${b1:-0}
	b2=${b2:-0}
	b3=${b3:-0}
	if [ "$a1" -lt "$b1" ]; then
		return 0
	fi
	if [ "$a1" -gt "$b1" ]; then
		return 1
	fi
	if [ "$a2" -lt "$b2" ]; then
		return 0
	fi
	if [ "$a2" -gt "$b2" ]; then
		return 1
	fi
	[ "$a3" -lt "$b3" ]
}

bin=$(command -v oserrorsgodernize) || {
	echo "oserrorsgodernize not found." >&2
	install_hint
	exit 1
}

xtools=$(go version -m "$bin" 2>/dev/null | awk '$2 == "golang.org/x/tools" { print $3; exit }')
if [ -z "$xtools" ]; then
	echo "Could not determine golang.org/x/tools version of $bin" >&2
	install_hint
	exit 1
fi

if version_lt "$xtools" "$MIN_XTOOLS"; then
	echo "oserrorsgodernize was built with golang.org/x/tools $xtools; need >= $MIN_XTOOLS on Go 1.27+." >&2
	install_hint
	exit 1
fi

bin_go=$(go version -m "$bin" 2>/dev/null | awk 'NR==1 { print $2; exit }')
cur_go=$(go version | awk '{ print $3 }')
bin_mm=$(go_major_minor "$bin_go")
cur_mm=$(go_major_minor "$cur_go")
if [ -z "$bin_mm" ] || [ -z "$cur_mm" ]; then
	echo "Could not determine Go versions (binary=$bin_go current=$cur_go)" >&2
	install_hint
	exit 1
fi
if [ "$bin_mm" != "$cur_mm" ]; then
	echo "oserrorsgodernize was built with $bin_go but the current toolchain is $cur_go." >&2
	echo "Rebuild the analyzer with the current Go toolchain." >&2
	install_hint
	exit 1
fi
