#!/bin/sh
# Tests go_version_compare.py and end-to-end Ansible Go install version
# checks (ref resolution, pseudo-version ordering, no-downgrade skipping,
# and experimental_ref overrides) against a fake go binary in a temp HOME.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
COMPARATOR="${SCRIPT_DIR}/go_version_compare.py"

# --- Part 1: Unit tests for go_version_compare.py ---
python3 - "$COMPARATOR" <<'PY'
import json
import subprocess
import sys

comp = sys.argv[1]

def check(installed, target, installed_go="", toolchain_go="", want_action=None, want_substr=""):
    args = ["python3", comp, installed, target, installed_go, toolchain_go]
    out = subprocess.check_output(args, text=True)
    data = json.loads(out)
    if data["action"] != want_action:
        raise AssertionError(
            f"compare({installed!r}, {target!r}, {installed_go!r}, {toolchain_go!r}) "
            f"= {data!r}, want action={want_action!r}"
        )
    if want_substr and want_substr not in data["reason"]:
        raise AssertionError(
            f"compare({installed!r}, {target!r}) reason={data['reason']!r}, want {want_substr!r}"
        )

# Not installed -> install
check("", "", "", "go1.27.4", want_action="install")
check("", "v0.1.0", "", "go1.27.4", want_action="install")

# Pseudo-version newer than release tag -> skip_older (never downgrade)
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.0",
    "go1.27.4",
    "go1.27.4",
    want_action="skip_older",
    want_substr="newer than target",
)
# Even when Go toolchain changed, never downgrade to an older target
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.0",
    "go1.26.0",
    "go1.27.4",
    want_action="skip_older",
    want_substr="newer than target",
)

# Release tag older than main pseudo-version -> install
check(
    "v0.1.0",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.27.4",
    "go1.27.4",
    want_action="install",
)

# Pseudo-version vs pseudo-version
check(
    "v0.1.1-0.20261124000000-111111111111",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.27.4",
    "go1.27.4",
    want_action="install",
)
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.1-0.20261124000000-111111111111",
    "go1.27.4",
    "go1.27.4",
    want_action="skip_older",
)

# Release tag is newer than its own prerelease pseudo-version
check(
    "v0.1.1",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.27.4",
    "go1.27.4",
    want_action="skip_older",
)
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.1",
    "go1.27.4",
    "go1.27.4",
    want_action="install",
)

# Pre-tag pseudo-versions and +dirty builds
check(
    "v0.0.0-20261125230000-abcdef123456",
    "v0.0.0-20261124000000-111111111111",
    want_action="skip_older",
)
check(
    "v0.1.1-0.20261125230000-abcdef123456+dirty",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.26.0",
    "go1.27.4",
    want_action="skip_older",
)
check(
    "v0.1.1-0.20261125230000-abcdef123456+dirty",
    "v0.1.1-0.20261126000000-222222222222",
    "go1.27.4",
    "go1.27.4",
    want_action="install",
)

# Equal version: skip if toolchain matches, rebuild if toolchain changed
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.27.4",
    "go1.27.4",
    want_action="skip_current",
)
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.26.0",
    "go1.27.4",
    want_action="install",
)

# Unresolved target keeps installed binary
check(
    "v0.1.1-0.20261125230000-abcdef123456",
    "",
    "go1.27.4",
    "go1.27.4",
    want_action="skip_unresolved",
)

# Unversioned (devel) binary upgrades to resolved version
check(
    "(devel)",
    "v0.1.1-0.20261125230000-abcdef123456",
    "go1.27.4",
    "go1.27.4",
    want_action="install",
)

print("go_version_compare.py unit tests passed")
PY

# --- Part 2: Ansible integration tests with fake go ---
if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
export TMPDIR="$ROOT"

HOME="${ROOT}/home"
GOPATH_DIR="${ROOT}/gopath"
mkdir -p "$HOME" "${ROOT}/bin" "${GOPATH_DIR}/bin"
export HOME GOPATH_DIR
unset XDG_CONFIG_HOME GOPATH

GO_LOG="${ROOT}/go.log"
OUT="${ROOT}/out"
export GO_LOG

cat >"${ROOT}/bin/go" <<'EOF'
#!/bin/sh
printf 'go %s\n' "$*" >>"$GO_LOG"
case "$1" in
env)
    case "$2" in
    GOPATH) printf '%s\n' "$GOPATH_DIR" ;;
    GOVERSION) printf '%s\n' "${FAKE_TOOLCHAIN_GO:-go1.27.4}" ;;
    *) exit 1 ;;
    esac
    ;;
version)
    # go version -m <bin>
    printf '%s: %s\n' "$3" "${FAKE_INSTALLED_GO:-go1.27.4}"
    printf '\tpath\t%s\n' "${FAKE_MOD_PATH:-github.com/jaeyeom/experimental/devtools/decide/cmd/decide}"
    printf '\tmod\t%s\t%s\th1:fake=\n' "${FAKE_MOD_ROOT:-github.com/jaeyeom/experimental}" "${FAKE_INSTALLED_VER}"
    ;;
list)
    # go list -m -f ... <mod>@<ref>
    target_spec=$5
    case "$target_spec" in
    *@main) printf '%s\n' "${FAKE_MAIN_VER}" ;;
    *@latest) printf '%s\n' "${FAKE_LATEST_VER}" ;;
    *@deadbeef1234) printf '%s\n' "${FAKE_PINNED_VER}" ;;
    *)
        echo "unexpected go list spec: $target_spec" >&2
        exit 1
        ;;
    esac
    ;;
install)
    printf 'INSTALL %s\n' "$2" >>"$GO_LOG"
    ;;
*)
    echo "unexpected go subcommand: $*" >&2
    exit 1
    ;;
esac
EOF
chmod +x "${ROOT}/bin/go"

cat >"${ROOT}/bin/systemctl" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${ROOT}/bin/systemctl"

cat >"${ROOT}/bin/launchctl" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${ROOT}/bin/launchctl"

# Fake decide binary on PATH so `command -v decide` succeeds when installed.
install_fake_decide() {
	cat >"${GOPATH_DIR}/bin/decide" <<'EOF'
#!/bin/sh
exit 0
EOF
	chmod +x "${GOPATH_DIR}/bin/decide"
}

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH
if [ -e "${ANSIBLE_BIN_DIR}/decide" ]; then
	echo "SKIP: decide is next to ansible-playbook; cannot hide it"
	exit 0
fi

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_DISPLAY_SKIPPED_HOSTS=1
export ANSIBLE_RETRY_FILES_ENABLED=0
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible-local"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible-remote"

INVENTORY="${SCRIPT_DIR}/inventory.ini"

run_pb() {
	: >"$GO_LOG"
	if ! ansible-playbook -i "$INVENTORY" \
		-e go_playbook_imported=true \
		-e shell_profile_detection_playbook_imported=true \
		-e "user_profile_path=${HOME}/.profile" \
		"$@" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook failed: $*"
	fi
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

# Scenario 1: decide.yml skips downgrade when installed main pseudo-version is
# newer than resolved target (even if built with an older Go toolchain).
install_fake_decide
FAKE_INSTALLED_VER="v0.1.1-0.20261125230000-abcdef123456" \
FAKE_INSTALLED_GO="go1.26.0" \
FAKE_TOOLCHAIN_GO="go1.27.4" \
FAKE_MAIN_VER="v0.1.0" \
	run_pb "${SCRIPT_DIR}/decide.yml"
grep -qE 'go list -m -f \{\{\.Version ?\}\} github\.com/jaeyeom/experimental@main$' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "decide.yml did not resolve @main"
}
if grep -q '^INSTALL ' "$GO_LOG"; then
	cat "$GO_LOG" >&2
	fail "decide.yml downgraded newer pseudo-version to older target"
fi
grep -qF 'Skipping decide: installed version v0.1.1-0.20261125230000-abcdef123456 is newer than target v0.1.0' "$OUT" || {
	cat "$OUT" >&2
	fail "decide.yml did not print skip message on older target"
}

# Scenario 2: decide.yml upgrades when target @main is a newer pseudo-version.
FAKE_INSTALLED_VER="v0.1.0" \
FAKE_INSTALLED_GO="go1.27.4" \
FAKE_TOOLCHAIN_GO="go1.27.4" \
FAKE_MAIN_VER="v0.1.1-0.20261125230000-abcdef123456" \
	run_pb "${SCRIPT_DIR}/decide.yml"
grep -qxF 'INSTALL github.com/jaeyeom/experimental/devtools/decide/cmd/decide@main' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "decide.yml did not install @main when target was newer"
}

# Scenario 3: experimental_ref pins both go list -m and go install to one ref.
FAKE_INSTALLED_VER="v0.1.0" \
FAKE_INSTALLED_GO="go1.27.4" \
FAKE_TOOLCHAIN_GO="go1.27.4" \
FAKE_PINNED_VER="v0.1.1-0.20261125120000-deadbeef1234" \
	run_pb -e experimental_ref=deadbeef1234 "${SCRIPT_DIR}/decide.yml"
grep -qE 'go list -m -f \{\{\.Version ?\}\} github\.com/jaeyeom/experimental@deadbeef1234$' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "decide.yml did not pass experimental_ref to go list"
}
grep -qxF 'INSTALL github.com/jaeyeom/experimental/devtools/decide/cmd/decide@deadbeef1234' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "decide.yml did not pass experimental_ref to go install"
}

# Scenario 4: experimental_ref can be set in ~/.config/setup-dev/vars.yml.
mkdir -p "${HOME}/.config/setup-dev"
printf 'experimental_ref: deadbeef1234\n' >"${HOME}/.config/setup-dev/vars.yml"
FAKE_INSTALLED_VER="v0.1.0" \
FAKE_INSTALLED_GO="go1.27.4" \
FAKE_TOOLCHAIN_GO="go1.27.4" \
FAKE_PINNED_VER="v0.1.1-0.20261125120000-deadbeef1234" \
	run_pb "${SCRIPT_DIR}/decide.yml"
grep -qxF 'INSTALL github.com/jaeyeom/experimental/devtools/decide/cmd/decide@deadbeef1234' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "decide.yml did not read experimental_ref from vars.yml"
}
rm -f "${HOME}/.config/setup-dev/vars.yml"

# Scenario 5: Third-party tool (gopls.yml) keeps using @latest.
cat >"${GOPATH_DIR}/bin/gopls" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${GOPATH_DIR}/bin/gopls"
if [ ! -e "${ANSIBLE_BIN_DIR}/gopls" ]; then
	FAKE_MOD_PATH="golang.org/x/tools/gopls" \
	FAKE_MOD_ROOT="golang.org/x/tools/gopls" \
	FAKE_INSTALLED_VER="v0.16.0" \
	FAKE_INSTALLED_GO="go1.27.4" \
	FAKE_TOOLCHAIN_GO="go1.27.4" \
	FAKE_LATEST_VER="v0.17.0" \
		run_pb "${SCRIPT_DIR}/gopls.yml"
	grep -qE 'go list -m -f \{\{\.Version ?\}\} golang\.org/x/tools/gopls@latest$' "$GO_LOG" || {
		cat "$GO_LOG" >&2
		fail "gopls.yml did not use @latest for go list"
	}
	grep -qxF 'INSTALL golang.org/x/tools/gopls@latest' "$GO_LOG" || {
		cat "$GO_LOG" >&2
		fail "gopls.yml did not use @latest for go install"
	}
fi

# Scenario 6: diskwatch.yml uses @main and refuses to downgrade.
cat >"${GOPATH_DIR}/bin/diskwatch" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${GOPATH_DIR}/bin/diskwatch"
if [ ! -e "${ANSIBLE_BIN_DIR}/diskwatch" ]; then
	FAKE_MOD_PATH="github.com/jaeyeom/experimental/devtools/diskwatch/cmd/diskwatch" \
	FAKE_MOD_ROOT="github.com/jaeyeom/experimental" \
	FAKE_INSTALLED_VER="v0.1.1-0.20261125230000-abcdef123456" \
	FAKE_INSTALLED_GO="go1.27.4" \
	FAKE_TOOLCHAIN_GO="go1.27.4" \
	FAKE_MAIN_VER="v0.1.0" \
		run_pb "${SCRIPT_DIR}/diskwatch.yml"
	grep -qE 'go list -m -f \{\{\.Version ?\}\} github\.com/jaeyeom/experimental@main$' "$GO_LOG" || {
		cat "$GO_LOG" >&2
		fail "diskwatch.yml did not resolve @main"
	}
	if grep -q '^INSTALL ' "$GO_LOG"; then
		cat "$GO_LOG" >&2
		fail "diskwatch.yml downgraded newer pseudo-version"
	fi
	grep -qF 'Skipping diskwatch:' "$OUT" || {
		cat "$OUT" >&2
		fail "diskwatch.yml did not print skip message"
	}
fi

# Scenario 7: setup-hub-client.yml (--tags hub-install) uses main by default and
# refuses to downgrade.
cat >"${GOPATH_DIR}/bin/agentbus" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${GOPATH_DIR}/bin/agentbus"
FAKE_INSTALLED_VER="v0.1.1-0.20261125230000-abcdef123456" \
FAKE_MAIN_VER="v0.1.0" \
	run_pb --tags hub-install -e hub_ssh_target=me@hub.example -e client_machine_label=laptop "${SCRIPT_DIR}/setup-hub-client.yml"
grep -qE 'go list -m -f \{\{\.Version ?\}\} github\.com/jaeyeom/experimental@main$' "$GO_LOG" || {
	cat "$GO_LOG" >&2
	fail "setup-hub-client.yml did not resolve @main"
}
if grep -q '^INSTALL ' "$GO_LOG"; then
	cat "$GO_LOG" >&2
	fail "setup-hub-client.yml downgraded newer pseudo-version"
fi
grep -qF 'Skipping agentbus:' "$OUT" || {
	cat "$OUT" >&2
	fail "setup-hub-client.yml did not print skip message for agentbus"
}

# Scenario 8: factory.yml migrates an existing factory binary installed from
# github.com/jaeyeom/agentic-factory to github.com/jaeyeom/experimental without
# querying the stale module path.
cat >"${GOPATH_DIR}/bin/factory" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${GOPATH_DIR}/bin/factory"
if [ ! -e "${ANSIBLE_BIN_DIR}/factory" ]; then
	FAKE_MOD_PATH="github.com/jaeyeom/agentic-factory/cmd/factory" \
	FAKE_MOD_ROOT="github.com/jaeyeom/agentic-factory" \
	FAKE_INSTALLED_VER="v9.9.9" \
	FAKE_INSTALLED_GO="go1.27.4" \
	FAKE_TOOLCHAIN_GO="go1.27.4" \
	FAKE_MAIN_VER="v0.1.0" \
		run_pb -e gh_playbook_imported=true -e herdr_playbook_imported=true "${SCRIPT_DIR}/factory.yml"
	if grep -q 'github\.com/jaeyeom/agentic-factory@' "$GO_LOG"; then
		cat "$GO_LOG" >&2
		fail "factory.yml queried stale agentic-factory module in go list -m"
	fi
	grep -qxF 'INSTALL github.com/jaeyeom/experimental/devtools/factory/cmd/factory@main' "$GO_LOG" || {
		cat "$GO_LOG" >&2
		fail "factory.yml did not reinstall factory from github.com/jaeyeom/experimental"
	}
fi

# Scenario 9: Subpackage third-party tool (goimports.yml) whose installed mod
# is an ancestor of PkgPath does not trigger a false module mismatch.
cat >"${GOPATH_DIR}/bin/goimports" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "${GOPATH_DIR}/bin/goimports"
if [ ! -e "${ANSIBLE_BIN_DIR}/goimports" ]; then
	FAKE_MOD_PATH="golang.org/x/tools/cmd/goimports" \
	FAKE_MOD_ROOT="golang.org/x/tools" \
	FAKE_INSTALLED_VER="v0.40.0" \
	FAKE_INSTALLED_GO="go1.27.4" \
	FAKE_TOOLCHAIN_GO="go1.27.4" \
	FAKE_LATEST_VER="v0.40.0" \
		run_pb "${SCRIPT_DIR}/goimports.yml"
	grep -qE 'go list -m -f \{\{\.Version ?\}\} golang\.org/x/tools@latest$' "$GO_LOG" || {
		cat "$GO_LOG" >&2
		fail "goimports.yml did not resolve ancestor module golang.org/x/tools@latest"
	}
	if grep -q '^INSTALL ' "$GO_LOG"; then
		cat "$GO_LOG" >&2
		fail "goimports.yml reinstalled on matching ancestor module"
	fi
fi

echo "PASS: all go_install_version tests passed"
