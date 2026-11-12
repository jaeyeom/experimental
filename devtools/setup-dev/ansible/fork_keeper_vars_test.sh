#!/bin/sh
# setup-fork-keeper.yml reads fork_keeper_cron_enabled from the per-machine
# vars file ($XDG_CONFIG_HOME or ~/.config)/setup-dev/vars.yml, and -e still
# overrides it. The playbook runs in --check mode with only the cron tasks
# (--tags fork_keeper_cron), a temp HOME, and a fake crontab first on PATH,
# so the real crontab is never read or written.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-fork-keeper.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT

HOME="${ROOT}/home"
mkdir -p "$HOME" "${ROOT}/bin"
export HOME
unset XDG_CONFIG_HOME
TRACE="${ROOT}/crontab-trace"
export TRACE

# Fake crontab: an empty table for -l; anything else would be a write.
cat >"${ROOT}/bin/crontab" <<'EOF'
#!/bin/sh
printf 'crontab %s\n' "$*" >>"$TRACE"
case "$*" in
    -l | *" -l") exit 0 ;;
esac
echo "fake crontab: unexpected write: $*" >&2
exit 1
EOF
chmod +x "${ROOT}/bin/crontab"
PATH="${ROOT}/bin:${PATH}"
export PATH

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_DISPLAY_SKIPPED_HOSTS=1
export ANSIBLE_GATHERING=smart
export ANSIBLE_RETRY_FILES_ENABLED=0

OUT="${ROOT}/out"

# run_playbook runs the cron tasks in check mode; extra args go to
# ansible-playbook.
run_playbook() {
	: >"$TRACE"
	if ! ansible-playbook -i "$INVENTORY" --check --tags fork_keeper_cron \
		-e "user_bin_directory=${HOME}/.local/bin" \
		"$@" "$PLAYBOOK" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook failed: $*"
	fi
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

# task_result prints the first result word (ok, changed, skipping) after the
# TASK line with the given name.
task_result() {
	awk -v name="TASK [$1]" '
        index($0, name) == 1 { found = 1; next }
        found && /^(ok|changed|skipping|fatal):/ { sub(/:.*/, ""); print; exit }
        found && /^TASK \[/ { exit }
    ' "$OUT"
}

REMOVE="Remove the fork-keeper cron entry on machines that are not designated"
SCHEDULE="Schedule fork-keeper with cron"

expect_removed() {
	_label="$1"
	_rm=$(task_result "$REMOVE")
	_sched=$(task_result "$SCHEDULE")
	case "$_rm" in
	ok | changed) ;;
	*)
		cat "$OUT" >&2
		fail "${_label}: remove task got '${_rm}', want it to run"
		;;
	esac
	case "$_sched" in
	skipping | "") ;;
	*)
		cat "$OUT" >&2
		fail "${_label}: schedule task got '${_sched}', want skipping"
		;;
	esac
}

expect_scheduled() {
	_label="$1"
	_rm=$(task_result "$REMOVE")
	_sched=$(task_result "$SCHEDULE")
	case "$_rm" in
	skipping | "") ;;
	*)
		cat "$OUT" >&2
		fail "${_label}: remove task got '${_rm}', want skipping"
		;;
	esac
	if [ "$_sched" != "changed" ]; then
		cat "$OUT" >&2
		fail "${_label}: schedule task got '${_sched}', want changed"
	fi
}

expect_fake_crontab_used() {
	if ! grep -q '^crontab ' "$TRACE"; then
		cat "$OUT" >&2
		fail "$1: the fake crontab was not called"
	fi
}

write_vars() {
	mkdir -p "$(dirname "$1")"
	printf 'fork_keeper_cron_enabled: %s\n' "$2" >"$1"
}

VARS="${HOME}/.config/setup-dev/vars.yml"

# --- no persisted value, no -e: the entry is removed ---
run_playbook
expect_removed "no vars file"
expect_fake_crontab_used "no vars file"

# --- persisted true, no -e: the entry is installed ---
write_vars "$VARS" true
run_playbook
expect_scheduled "persisted true"
expect_fake_crontab_used "persisted true"

# --- persisted true, -e false: -e wins and the entry is removed ---
run_playbook -e fork_keeper_cron_enabled=false
expect_removed "persisted true, -e false"

# --- no persisted value, -e true: the entry is installed ---
rm -f "$VARS"
run_playbook -e fork_keeper_cron_enabled=true
expect_scheduled "-e true"

# --- XDG_CONFIG_HOME moves the vars file ---
XDG_CONFIG_HOME="${ROOT}/xdg"
export XDG_CONFIG_HOME
write_vars "${XDG_CONFIG_HOME}/setup-dev/vars.yml" true
run_playbook
expect_scheduled "XDG_CONFIG_HOME vars file"

echo "PASS"
