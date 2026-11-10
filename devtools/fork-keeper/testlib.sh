# shellcheck shell=bash
# Shared fixture helpers for the fork-keeper tests. Source it; don't run it.
#
# A fixture is a temp dir holding bare `upstream.git` and `origin.git` (the
# fork), plus `useed/` and `fseed/` working clones used to author commits on
# each. Nothing here touches the network or a real GitHub repository.

# Bazel runs tests with a minimal PATH; jq usually lives in a package
# manager prefix.
PATH="$PATH:/opt/homebrew/bin:/usr/local/bin:/home/linuxbrew/.linuxbrew/bin"
for _tool in git jq; do
	command -v "$_tool" >/dev/null 2>&1 || {
		echo "FAIL: $_tool is required" >&2
		exit 1
	}
done

T="$(mktemp -d "${TMPDIR:-/tmp}/fork-keeper-test.XXXXXX")"
T="$(cd "$T" && pwd -P)"
trap 'rm -rf "$T"' EXIT

# Isolate git from the user's config: identity, credential helpers, shared
# hooks.
mkdir -p "$T/nohooks"
cat >"$T/gitconfig" <<EOF
[user]
	name = Fork Keeper Test
	email = fork-keeper@example.invalid
[core]
	hooksPath = $T/nohooks
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF
export GIT_CONFIG_GLOBAL="$T/gitconfig"
export GIT_CONFIG_NOSYSTEM=1
export HOME="$T/home"
mkdir -p "$HOME"

pass=0
fail=0
CUR=""
ok() { pass=$((pass + 1)); }
bad() {
	fail=$((fail + 1))
	echo "FAIL [$CUR] $*"
}
check() {
	local msg=$1
	shift
	if "$@"; then ok; else bad "$msg"; fi
}
finish() {
	echo "$(basename "$0"): $pass passed, $fail failed"
	[[ $fail -eq 0 ]]
}

g() { git -C "$1" "${@:2}"; }

# mk_fixture: fresh fixture whose upstream and fork share commit U0.
mk_fixture() {
	FX="$(mktemp -d "$T/fx.XXXXXX")"
	git init -q --bare -b main "$FX/upstream.git"
	git init -q --bare -b main "$FX/origin.git"
	git init -q -b main "$FX/useed"
	g "$FX/useed" remote add origin "$FX/upstream.git"
	echo base >"$FX/useed/README"
	g "$FX/useed" add README
	g "$FX/useed" commit -q -m U0
	g "$FX/useed" push -q origin main
	g "$FX/useed" push -q "$FX/origin.git" main
	git clone -q "$FX/origin.git" "$FX/fseed"
}

# ucommit <file> <content> [msg]: append to a file on upstream; prints sha.
ucommit() {
	mkdir -p "$(dirname "$FX/useed/$1")"
	printf '%s\n' "$2" >>"$FX/useed/$1"
	g "$FX/useed" add "$1"
	g "$FX/useed" commit -q -m "${3:-upstream: $1}"
	g "$FX/useed" push -q -f origin HEAD:main
	g "$FX/useed" rev-parse HEAD
}

# fcommit <file> <content>: the user's own commit on the fork; prints sha.
fcommit() {
	g "$FX/fseed" pull -q --rebase origin main
	mkdir -p "$(dirname "$FX/fseed/$1")"
	printf '%s\n' "$2" >>"$FX/fseed/$1"
	g "$FX/fseed" add "$1"
	g "$FX/fseed" commit -q -m "mine: $1"
	g "$FX/fseed" push -q origin HEAD:main
	g "$FX/fseed" rev-parse HEAD
}

# record_head <sha>: what a previous promotion leaves on the fork.
record_head() {
	g "$FX/useed" push -q -f "$FX/origin.git" "$1:refs/fork-keeper/upstream-head"
}

# fork_at_upstream: point the fork's main at upstream's, as a promotion with
# no user commits would.
fork_at_upstream() {
	g "$FX/useed" push -q -f "$FX/origin.git" HEAD:main
}

oref() { git --git-dir="$FX/origin.git" rev-parse -q --verify "$1" 2>/dev/null; }
uref() { git --git-dir="$FX/upstream.git" rev-parse -q --verify "$1" 2>/dev/null; }
