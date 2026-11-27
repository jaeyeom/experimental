#!/bin/sh
# setup-hub-host.yml and setup-hub-client.yml (jaeyeom/experimental#683).
# Each playbook runs only its tagged tasks against a temp HOME, with fake
# decide, agentbus, and herdr scripts, so the real dotfiles, configs, and
# hub are never read or written and nothing connects over ssh.
#
#   - hub-path: the PATH block goes above a non-interactive `return` guard
#     in ~/.bashrc (or into ~/.zshenv), a second run changes nothing, and
#     the check fails with a clear message when a command is missing.
#   - hub-config: the decide JSON merge keeps unrelated keys, the agentbus
#     TOML keys go above an existing [section], both files are backed up,
#     and a second run changes nothing.
#   - hub-check: a failing command fails the play with its stderr.
#   - hub-ssh-agent: false changes nothing; the default writes the rc
#     block and ssh_identity_agent once, leaves ~/.ssh/config alone,
#     and a second run changes nothing. keychain in ~/.zshrc wins over
#     an existing ~/.zprofile. An explicit rc that runs too early warns.
#     The Apple keychain load is Darwin plus UseKeychain yes only.
#   - Neither playbook names a host or user.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

# The scenarios to run are the arguments. With none, all of them run.
# BUILD.bazel has one target per scenario, so editing a playbook reruns
# only the scenarios that use it. An unknown name fails, so a typo in
# BUILD.bazel cannot pass without running anything.
KNOWN="names path-bash path-zsh config check agent-optout agent-default agent-zshrc agent-explicit agent-sourced keychain-darwin keychain-linux keychain-darwin-off"
for _s in "$@"; do
	case " $KNOWN " in
	*" $_s "*) ;;
	*) fail "unknown scenario: $_s" ;;
	esac
done
SCENARIOS=" $* "
want() {
	case "$SCENARIOS" in
	"  ") return 0 ;;
	*" $1 "*) return 0 ;;
	esac
	return 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
HOST_PLAYBOOK="${SCRIPT_DIR}/setup-hub-host.yml"
CLIENT_PLAYBOOK="${SCRIPT_DIR}/setup-hub-client.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

# --- No host or user names in the playbooks ---
# A literal ssh target (user@host) must not appear. `@{{` is the go
# install version, and a host under .example is a documentation placeholder.
# github.com/<owner>/ and an owner/repo token name the GitHub module, not
# this machine. A home path or a bare user still counts.
_name_filtered=$(mktemp)
_name_fixture=$(mktemp)
trap 'rm -f "$_name_filtered" "$_name_fixture"' EXIT INT TERM

# strip_github_owner copies src to dst with GitHub owner references to
# name removed. name is literal text.
strip_github_owner() {
	python3 -c '
import pathlib, sys
name, src, dst = sys.argv[1:]
text = pathlib.Path(src).read_text()
text = text.replace("github.com/%s/" % name, "github.com/")
text = text.replace("(%s/" % name, "(")
text = text.replace(" %s/" % name, " ")
pathlib.Path(dst).write_text(text)
' "$1" "$2" "$3"
}

# names_local is true when file still contains name after that strip.
names_local() {
	strip_github_owner "$2" "$1" "$_name_filtered"
	if grep -qF -- "$2" "$_name_filtered"; then
		return 0
	fi
	return 1
}

if want names; then
	_fixname=zzfixtureuser
	printf 'github.com/%s/experimental\n(%s/experimental#683)\n repo: %s/gmailctl\n' \
		"$_fixname" "$_fixname" "$_fixname" >"$_name_fixture"
	if names_local "$_name_fixture" "$_fixname"; then
		fail "a GitHub owner was treated as a local user"
	fi
	printf '/Users/%s\nuser: %s\n' "$_fixname" "$_fixname" >"$_name_fixture"
	if names_local "$_name_fixture" "$_fixname"; then
		:
	else
		fail "a home path or bare user was allowed"
	fi

	for _pb in "$HOST_PLAYBOOK" "$CLIENT_PLAYBOOK"; do
		if grep -noE '[A-Za-z0-9._-]+@[A-Za-z0-9{][A-Za-z0-9._-]*' "$_pb" | grep -vE '@(\{|[A-Za-z0-9._-]*\.example$)'; then
			fail "$(basename "$_pb") names an ssh target"
		fi
		for _name in "$(id -un 2>/dev/null || true)" "$(hostname -s 2>/dev/null || true)"; do
			# Short names (root, ci) match ordinary words; skip them.
			[ "${#_name}" -ge 5 ] || continue
			if names_local "$_pb" "$_name"; then
				grep -niF -- "$_name" "$_name_filtered" || true
				fail "$(basename "$_pb") names this machine or user: ${_name}"
			fi
		done
	done
	rm -f "$_name_filtered" "$_name_fixture"
fi

# Exit cleanly when only pure-text checks (e.g. `names`) were requested so
# `hub_setup_names_test` can run hermetically on RBE without ansible-playbook.
if ! want path-bash && ! want path-zsh && ! want config && ! want check && \
	! want agent-optout && ! want agent-default && ! want agent-zshrc && \
	! want agent-explicit && ! want agent-sourced && \
	! want keychain-darwin && ! want keychain-linux && ! want keychain-darwin-off; then
	echo "PASS"
	exit 0
fi

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")

# shellcheck disable=SC1091  # Sourced from the same directory as this script.
. "${SCRIPT_DIR}/ansible_test_env.sh"
ansible_test_init_env 0

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT

HOME="${ROOT}/home"
mkdir -p "${ROOT}/bin"
export HOME
unset XDG_CONFIG_HOME DECIDE_HUB AGENTBUS_HUB

PATH="${ROOT}/bin:${ANSIBLE_BIN_DIR}:/usr/bin:/bin"
export PATH

OUT="${ROOT}/out"
FACTS_JSON="${ROOT}/facts.json"
export ANSIBLE_HOME="${ROOT}/ansible"
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible-local"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible-remote"

# run PLAYBOOK ARGS... runs the playbook; it must succeed.
run() {
	_pb="$1"
	shift
	ansible_test_write_facts "$FACTS_JSON"
	if ! ansible-playbook -i "$INVENTORY" -e "@${FACTS_JSON}" "$@" "$_pb" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook $(basename "$_pb") failed: $*"
	fi
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

# run_fails PLAYBOOK ARGS... runs the playbook; it must fail.
run_fails() {
	_pb="$1"
	shift
	ansible_test_write_facts "$FACTS_JSON"
	if ansible-playbook -i "$INVENTORY" -e "@${FACTS_JSON}" "$@" "$_pb" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook $(basename "$_pb") succeeded, want failure: $*"
	fi
}

expect_no_change() {
	if ! grep -qE 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "$1: second run changed something"
	fi
}

expect_out() {
	if ! grep -qF -- "$2" "$OUT"; then
		cat "$OUT" >&2
		fail "$1: output lacks '$2'"
	fi
}

# fake NAME DIR EXIT_CODE writes a command that prints its name, or
# fails with a hub error when EXIT_CODE is not 0.
fake() {
	mkdir -p "$2"
	cat >"$2/$1" <<EOF
#!/bin/sh
if [ "$3" -ne 0 ]; then
    echo "$1: hub me@hub.example unreachable: host unreachable" >&2
    exit $3
fi
echo "$1 \$*"
EOF
	chmod +x "$2/$1"
}

# has_backup FILE succeeds when Ansible left a FILE.<pid>.<time>~ backup.
has_backup() {
	for _b in "$1".*~; do
		[ -e "$_b" ] && return 0
	done
	return 1
}

# new_home [NAME] empties HOME and the fake commands, so a scenario starts
# from nothing and never reads what another scenario left behind. When NAME
# is passed, it isolates HOME, OUT, FACTS_JSON, and Ansible temp dirs under
# ROOT/cases/NAME so subcases can run concurrently.
new_home() {
	if [ -n "${1:-}" ]; then
		_case_dir="${ROOT}/cases/$1"
		rm -rf "${_case_dir}"
		HOME="${_case_dir}/home"
		OUT="${_case_dir}/out"
		FACTS_JSON="${_case_dir}/facts.json"
		DECIDE_CONFIG="${HOME}/.config/decide/config.json"
		AGENTBUS_CONFIG="${HOME}/.config/agentbus/config.toml"
		export HOME
		export ANSIBLE_HOME="${_case_dir}/ansible"
		export ANSIBLE_LOCAL_TEMP="${_case_dir}/ansible-local"
		export ANSIBLE_REMOTE_TEMP="${_case_dir}/ansible-remote"
		mkdir -p "$HOME"
	else
		rm -rf "${ROOT:?}/home" "${ROOT:?}/bin"
		mkdir -p "${ROOT}/home" "${ROOT}/bin"
	fi
}

# ======================== setup-hub-host.yml ========================

# bashrc_with_guard writes a ~/.bashrc with a non-interactive guard, and
# fake commands for the three the hub PATH check looks for.
bashrc_with_guard() {
	BASHRC="${HOME}/.bashrc"
	cat >"$BASHRC" <<'EOF'
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

PATH="$HOME/interactive-only:$PATH"
EOF
	fake decide "${HOME}/go/bin" 0
	fake agentbus "${HOME}/go/bin" 0
	fake herdr "${HOME}/.local/bin" 0
}

if want path-bash; then
	new_home
	bashrc_with_guard

	run "$HOST_PLAYBOOK" --tags hub-path -e hub_host_shell=bash
	_block=$(grep -n 'BEGIN setup-dev hub PATH' "$BASHRC" | cut -d: -f1)
	_guard=$(grep -n 'return;;' "$BASHRC" | cut -d: -f1)
	[ -n "$_block" ] || fail "bashrc: PATH block missing"
	[ "$_block" -lt "$_guard" ] || fail "bashrc: PATH block at line ${_block} is not above the return guard at ${_guard}"
	grep -q 'interactive-only' "$BASHRC" || fail "bashrc: existing content lost"

	# The block makes the commands visible to a non-interactive bash, the
	# way sshd runs `bash -c` after sourcing ~/.bashrc.
	# shellcheck disable=SC2016 # $HOME expands in the child shell.
	_found=$(env -i HOME="$HOME" PATH=/usr/bin:/bin bash -c '. "$HOME/.bashrc"; command -v decide agentbus herdr')
	case "$_found" in
	*go/bin/decide*go/bin/agentbus*.local/bin/herdr*) ;;
	*) fail "bashrc: non-interactive bash found: ${_found}" ;;
	esac

	# Sourcing twice does not repeat a PATH entry.
	# shellcheck disable=SC2016
	_path=$(env -i HOME="$HOME" PATH=/usr/bin:/bin bash -c '. "$HOME/.bashrc"; . "$HOME/.bashrc"; echo "$PATH"')
	[ "$_path" = "${HOME}/.local/bin:${HOME}/go/bin:/usr/bin:/bin" ] || [ "$_path" = "${HOME}/go/bin:${HOME}/.local/bin:/usr/bin:/bin" ] ||
		fail "bashrc: PATH after two sources: ${_path}"

	cp "$BASHRC" "${ROOT}/bashrc.first"
	run "$HOST_PLAYBOOK" --tags hub-path -e hub_host_shell=bash
	expect_no_change "bashrc second run"
	cmp -s "$BASHRC" "${ROOT}/bashrc.first" || fail "bashrc: second run changed the file"

	# A missing command fails with its name.
	rm "${HOME}/.local/bin/herdr"
	run_fails "$HOST_PLAYBOOK" --tags hub-path -e hub_host_shell=bash
	expect_out "missing herdr" "not found on PATH for a non-interactive ssh command: herdr"
fi

# zsh reads ~/.zshenv for `ssh host cmd`.
if want path-zsh && command -v zsh >/dev/null 2>&1; then
	new_home
	fake decide "${HOME}/go/bin" 0
	fake agentbus "${HOME}/go/bin" 0
	fake herdr "${HOME}/.local/bin" 0
	run "$HOST_PLAYBOOK" --tags hub-path -e hub_host_shell=zsh
	grep -q 'BEGIN setup-dev hub PATH' "${HOME}/.zshenv" || fail "zshenv: PATH block missing"
	_found=$(env -i HOME="$HOME" PATH=/usr/bin:/bin zsh -c 'command -v herdr')
	[ "$_found" = "${HOME}/.local/bin/herdr" ] || fail "zshenv: non-interactive zsh found: ${_found}"
	run "$HOST_PLAYBOOK" --tags hub-path -e hub_host_shell=zsh
	expect_no_change "zshenv second run"
fi

# ======================= setup-hub-client.yml =======================

HUB_ARGS="-e hub_ssh_target=me@hub.example -e client_machine_label=laptop -e hub_connect_timeout=7s"
DECIDE_CONFIG="${HOME}/.config/decide/config.json"
AGENTBUS_CONFIG="${HOME}/.config/agentbus/config.toml"

# agentbus_config writes a minimal agentbus config.
agentbus_config() {
	mkdir -p "$(dirname "$AGENTBUS_CONFIG")"
	printf '%s\n' 'machines = ["linux2"]' >"$AGENTBUS_CONFIG"
}

# agent_home sets up the files the ssh-agent scenarios start from: an
# ssh config sentinel, a ~/.zprofile that starts an agent, a
# ~/.bash_profile that must stay alone, and an agentbus config.
agent_home() {
	new_home
	agentbus_config
	mkdir -p "${HOME}/.ssh" "${HOME}/Library/LaunchAgents"
	printf '%s\n' 'Host sentinel' >"${HOME}/.ssh/config"
	printf '%s\n' 'agent-start' >"${HOME}/.zprofile"
	printf '%s\n' 'not-this' >"${HOME}/.bash_profile"
	: >"${HOME}/Library/LaunchAgents/com.agentbus.tunnel.plist"
}

# The literal rc block line the agent playbook writes, for grep -F.
# shellcheck disable=SC2016 # Match the literal rc block, including $.
AGENT_LN='  ln -sf "$SSH_AUTH_SOCK" "$HOME/.ssh/agent.sock"'

if want config; then
	_cfg_pids=""

	# Required variables: without hub_ssh_target the play stops early.
	(
		new_home cfg_missing
		run_fails "$CLIENT_PLAYBOOK" --tags hub-config -e client_machine_label=laptop
		expect_out "no hub_ssh_target" "hub_ssh_target"
	) &
	_cfg_pids="$_cfg_pids $!"

	(
		new_home cfg_merge
		mkdir -p "$(dirname "$DECIDE_CONFIG")" "$(dirname "$AGENTBUS_CONFIG")"
		cat >"$DECIDE_CONFIG" <<'EOF'
{"notifyTemplate": "herdr pane send {pane}", "hub": "old@hub.example", "openCommand": ["xdg-open"]}
EOF
		cat >"$AGENTBUS_CONFIG" <<'EOF'
# agentbus config
machines = ["linux2"]
remote_timeout = "2s"

[extra]
hub = "section value stays"
EOF

		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-config $HUB_ARGS

		_json=$(python3 -c '
import json, sys
d = json.load(open(sys.argv[1]))
print(d.get("hub"), d.get("hubTimeout"), d.get("machine"), d.get("notifyTemplate"), d.get("openCommand"))
' "$DECIDE_CONFIG")
		[ "$_json" = "me@hub.example 7s laptop herdr pane send {pane} ['xdg-open']" ] ||
			fail "decide config: got ${_json}"
		has_backup "$DECIDE_CONFIG" || fail "decide config: no backup"

		_section=$(grep -n '^\[extra\]' "$AGENTBUS_CONFIG" | cut -d: -f1)
		[ -n "$_section" ] || fail "agentbus config: [extra] section lost"
		for _kv in 'hub = "me@hub.example"' 'machine = "laptop"' 'remote_timeout = "7s"'; do
			_line=$(grep -nxF -- "$_kv" "$AGENTBUS_CONFIG" | head -1 | cut -d: -f1)
			[ -n "$_line" ] || {
				cat "$AGENTBUS_CONFIG" >&2
				fail "agentbus config: missing ${_kv}"
			}
			[ "$_line" -lt "$_section" ] || fail "agentbus config: ${_kv} is not above [extra]"
		done
		grep -qxF 'hub = "section value stays"' "$AGENTBUS_CONFIG" || fail "agentbus config: [extra] hub changed"
		grep -qxF 'machines = ["linux2"]' "$AGENTBUS_CONFIG" || fail "agentbus config: machines lost"
		[ "$(grep -c '^remote_timeout' "$AGENTBUS_CONFIG")" -eq 1 ] || fail "agentbus config: remote_timeout repeated"
		has_backup "$AGENTBUS_CONFIG" || fail "agentbus config: no backup"

		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-config $HUB_ARGS
		expect_no_change "client config second run"
	) &
	_cfg_pids="$_cfg_pids $!"

	# A config that does not exist yet is created.
	(
		new_home cfg_fresh
		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-config $HUB_ARGS
		grep -q '"machine": "laptop"' "$DECIDE_CONFIG" || fail "decide config: not created"
		grep -qxF 'hub = "me@hub.example"' "$AGENTBUS_CONFIG" || fail "agentbus config: not created"
	) &
	_cfg_pids="$_cfg_pids $!"

	# shellcheck disable=SC2086  # _cfg_pids is space-separated integer PIDs.
	ansible_test_wait_pids $_cfg_pids || exit 1
fi

if want check; then
	new_home

	# hub-check runs both commands through the hub.
	fake decide "${ROOT}/bin" 0
	fake agentbus "${ROOT}/bin" 0
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-check $HUB_ARGS
	expect_out "hub-check ok" "decide status list"
	expect_out "hub-check ok" "agentbus list"

	fake agentbus "${ROOT}/bin" 255
	# shellcheck disable=SC2086
	run_fails "$CLIENT_PLAYBOOK" --tags hub-check $HUB_ARGS
	expect_out "hub-check unreachable" "agentbus: hub me@hub.example unreachable: host unreachable"
fi

if want agent-optout; then
	new_home
	agentbus_config

	# false opts out. It writes no rc, no ssh config, and no ssh_identity_agent key.
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=false $HUB_ARGS
	expect_no_change "stable agent opt-out"
	for _absent in "${HOME}/.zprofile" "${HOME}/.bash_profile" "${HOME}/.profile" "${HOME}/.ssh/config"; do
		[ ! -e "$_absent" ] || fail "stable agent opt-out created ${_absent}"
	done
	if grep -q ssh_identity_agent "$AGENTBUS_CONFIG"; then
		fail "stable agent opt-out wrote ssh_identity_agent"
	fi
fi

if want agent-default; then
	agent_home
	_backups=$(find "$(dirname "$AGENTBUS_CONFIG")" -name 'config.toml.*~' | wc -l | tr -d ' ')

	# The default (no hub_ssh_stable_agent) writes the block and the config key.
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent $HUB_ARGS
	expect_out "stable agent install hint" "agentbus install"
	# shellcheck disable=SC2016 # Match the literal rc block, including $.
	grep -qF 'if [ -n "$SSH_AUTH_SOCK" ] && [ -S "$SSH_AUTH_SOCK" ] && [ "$SSH_AUTH_SOCK" != "$HOME/.ssh/agent.sock" ]; then' "${HOME}/.zprofile" || fail "zprofile: agent block missing"
	grep -qF "$AGENT_LN" "${HOME}/.zprofile" || fail "zprofile: ln missing"
	grep -qF 'BEGIN setup-dev hub ssh agent (setup-hub-client.yml)' "${HOME}/.zprofile" || fail "zprofile: marker missing"
	_start=$(grep -n 'agent-start' "${HOME}/.zprofile" | head -1 | cut -d: -f1)
	_link=$(grep -n 'ln -sf' "${HOME}/.zprofile" | head -1 | cut -d: -f1)
	[ -n "$_start" ] && [ -n "$_link" ] && [ "$_link" -gt "$_start" ] || fail "zprofile: block is not after agent-start"
	[ "$(cat "${HOME}/.bash_profile")" = "not-this" ] || fail "bash_profile changed"
	[ ! -e "${HOME}/.profile" ] || fail "profile created while zprofile exists"
	[ "$(cat "${HOME}/.ssh/config")" = "Host sentinel" ] || fail "ssh config changed"
	grep -qxF 'ssh_identity_agent = "~/.ssh/agent.sock"' "$AGENTBUS_CONFIG" || {
		cat "$AGENTBUS_CONFIG" >&2
		fail "agentbus config: ssh_identity_agent missing"
	}
	[ "$(grep -c '^ssh_identity_agent' "$AGENTBUS_CONFIG")" -eq 1 ] || fail "ssh_identity_agent repeated"
	has_backup "${HOME}/.zprofile" || fail "zprofile: no backup"
	_backups_after=$(find "$(dirname "$AGENTBUS_CONFIG")" -name 'config.toml.*~' | wc -l | tr -d ' ')
	[ "$_backups_after" -gt "$_backups" ] || fail "agentbus config: no new backup"

	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent $HUB_ARGS
	expect_no_change "stable agent second run"
	[ "$(grep -c 'ln -sf' "${HOME}/.zprofile")" -eq 1 ] || fail "zprofile: block repeated"
fi

if want agent-zshrc; then
	agent_home

	# keychain in .zshrc runs after .zprofile, so the block follows it.
	printf '%s\n' 'keychain' >"${HOME}/.zshrc"
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true $HUB_ARGS
	grep -qF "$AGENT_LN" "${HOME}/.zshrc" || fail "zshrc: agent block missing"
	_key=$(grep -n 'keychain' "${HOME}/.zshrc" | head -1 | cut -d: -f1)
	_link=$(grep -n 'ln -sf' "${HOME}/.zshrc" | head -1 | cut -d: -f1)
	[ -n "$_key" ] && [ -n "$_link" ] && [ "$_link" -gt "$_key" ] || fail "zshrc: block is not after keychain"
	if grep -q 'ln -sf' "${HOME}/.zprofile"; then
		fail "zprofile: block written beside a zshrc keychain"
	fi
	[ "$(cat "${HOME}/.bash_profile")" = "not-this" ] || fail "bash_profile changed"
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true $HUB_ARGS
	expect_no_change "zshrc agent second run"
	[ "$(grep -c 'ln -sf' "${HOME}/.zshrc")" -eq 1 ] || fail "zshrc: block repeated"
fi

if want agent-explicit; then
	agent_home
	printf '%s\n' 'keychain' >"${HOME}/.zshrc"

	# An explicit rc that runs before the agent stays put and warns.
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true -e hub_ssh_agent_rc=~/.zprofile $HUB_ARGS
	expect_out "explicit rc warns zprofile" "${HOME}/.zprofile"
	expect_out "explicit rc warns zshrc" "${HOME}/.zshrc"
	expect_out "explicit rc warns order" "runs before the agent starts"
	grep -qF "$AGENT_LN" "${HOME}/.zprofile" || fail "zprofile: explicit block missing"
	if grep -q 'ln -sf' "${HOME}/.zshrc"; then
		fail "zshrc: block written despite an explicit rc"
	fi
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true -e hub_ssh_agent_rc=~/.zprofile $HUB_ARGS
	expect_no_change "explicit rc second run"
fi

if want agent-sourced; then
	agent_home

	# A sourced file one level deep holds the last agent-start line.
	mkdir -p "${HOME}/.config"
	# shellcheck disable=SC2016 # The sourced file starts ssh-agent literally.
	printf '%s\n' 'eval "$(ssh-agent -s)"' >"${HOME}/.config/agent-start.sh"
	# shellcheck disable=SC2016 # The source line is literal text for the scanner.
	printf '%s\n' 'source "$HOME/.config/agent-start.sh"' >"${HOME}/.zshrc"
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true $HUB_ARGS
	grep -qF "$AGENT_LN" "${HOME}/.config/agent-start.sh" || fail "sourced file: agent block missing"
	grep -qF 'BEGIN setup-dev hub ssh agent' "${HOME}/.zshrc" && fail "zshrc: block written beside a source"
	_agent=$(grep -n 'ssh-agent' "${HOME}/.config/agent-start.sh" | head -1 | cut -d: -f1)
	_link=$(grep -n 'ln -sf' "${HOME}/.config/agent-start.sh" | head -1 | cut -d: -f1)
	[ -n "$_agent" ] && [ -n "$_link" ] && [ "$_link" -gt "$_agent" ] || fail "sourced file: block is not after ssh-agent"
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e hub_ssh_stable_agent=true $HUB_ARGS
	expect_no_change "sourced agent second run"
fi

# The Apple keychain line is only for Darwin with UseKeychain yes, and
# only when the rc does not already have it.
if want keychain-darwin; then
	_kd_pids=""
	(
		new_home kd_fresh
		agentbus_config
		mkdir -p "${HOME}/.ssh" "${HOME}/Library/LaunchAgents"
		printf '%s\n' 'Host sentinel' >"${HOME}/.ssh/config"
		printf '%s\n' 'agent-start' >"${HOME}/.zprofile"
		printf '%s\n' 'not-this' >"${HOME}/.bash_profile"
		: >"${HOME}/Library/LaunchAgents/com.agentbus.tunnel.plist"
		printf '%s\n' 'Host *' '  UseKeychain yes' >"${HOME}/.ssh/config"
		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e ansible_system=Darwin $HUB_ARGS
		grep -qF 'ssh-add --apple-load-keychain -q 2>/dev/null || true' "${HOME}/.zprofile" || fail "darwin: keychain line missing"
		[ "$(grep -c 'ssh-add --apple-load-keychain' "${HOME}/.zprofile")" -eq 1 ] || fail "darwin: keychain line repeated"
		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e ansible_system=Darwin $HUB_ARGS
		expect_no_change "darwin keychain second run"
		[ "$(grep -c 'ssh-add --apple-load-keychain' "${HOME}/.zprofile")" -eq 1 ] || fail "darwin: keychain line repeated on second run"
	) &
	_kd_pids="$_kd_pids $!"

	(
		new_home kd_existing
		agentbus_config
		mkdir -p "${HOME}/.ssh" "${HOME}/Library/LaunchAgents"
		printf '%s\n' 'Host *' '  UseKeychain yes' >"${HOME}/.ssh/config"
		printf '%s\n' 'not-this' >"${HOME}/.bash_profile"
		: >"${HOME}/Library/LaunchAgents/com.agentbus.tunnel.plist"
		printf '%s\n' 'agent-start' 'ssh-add --apple-load-keychain -q 2>/dev/null || true' >"${HOME}/.zprofile"
		# shellcheck disable=SC2086
		run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e ansible_system=Darwin $HUB_ARGS
		[ "$(grep -c 'ssh-add --apple-load-keychain' "${HOME}/.zprofile")" -eq 1 ] || fail "darwin: existing keychain line was copied"
	) &
	_kd_pids="$_kd_pids $!"

	# shellcheck disable=SC2086  # _kd_pids is space-separated integer PIDs.
	ansible_test_wait_pids $_kd_pids || exit 1
fi

if want keychain-linux; then
	agent_home
	printf '%s\n' 'Host *' '  UseKeychain yes' >"${HOME}/.ssh/config"
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e ansible_system=Linux $HUB_ARGS
	if grep -q 'apple-load-keychain' "${HOME}/.zprofile"; then
		fail "linux: keychain line written"
	fi
	grep -qF "$AGENT_LN" "${HOME}/.zprofile" || fail "linux: agent block missing"
fi

if want keychain-darwin-off; then
	agent_home
	# shellcheck disable=SC2086
	run "$CLIENT_PLAYBOOK" --tags hub-ssh-agent -e ansible_system=Darwin $HUB_ARGS
	if grep -q 'apple-load-keychain' "${HOME}/.zprofile"; then
		fail "darwin without UseKeychain wrote the keychain line"
	fi
fi

echo "PASS"
