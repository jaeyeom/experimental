#!/bin/sh
# Shared helper for local Ansible integration tests in this directory.
# Pre-populates minimal ansible_facts from the current process environment so
# test runs skip full setup fact-gathering while keeping normal env overrides.

# ansible_test_init_env [DISPLAY_SKIPPED_HOSTS] configures fast non-interactive
# Ansible environment defaults and captures python3 before PATH narrowing.
ansible_test_init_env() {
	if [ -z "${ANSIBLE_TEST_PYTHON:-}" ]; then
		ANSIBLE_TEST_PYTHON=$(command -v python3 || true)
		export ANSIBLE_TEST_PYTHON
	fi
	export ANSIBLE_NOCOLOR=1
	export ANSIBLE_STDOUT_CALLBACK=default
	export ANSIBLE_DISPLAY_SKIPPED_HOSTS="${1:-0}"
	export ANSIBLE_GATHERING=explicit
	export ANSIBLE_PIPELINING=1
	export ANSIBLE_LOAD_CALLBACK_PLUGINS=0
	export ANSIBLE_RETRY_FILES_ENABLED=0
}

# ansible_test_write_facts OUT_JSON writes minimal preloaded facts matching the
# current environment (HOME, PATH, TMPDIR, XDG_*, HERDR_SOCKET_PATH, etc.).
ansible_test_write_facts() {
	_facts_out="$1"
	_facts_py="${ANSIBLE_TEST_PYTHON:-${PYTHON_BIN:-$(command -v python3)}}"
	"$_facts_py" - "$_facts_out" "$_facts_py" <<'PY'
import json
import os
import platform
import pwd
import sys

out_path, py_bin = sys.argv[1], sys.argv[2]
sysname = platform.system()
os_family = "Darwin" if sysname == "Darwin" else "Debian"
uid = os.getuid()
try:
    user_id = pwd.getpwuid(uid).pw_name
except KeyError:
    user_id = os.environ.get("USER", "user")

env = dict(os.environ)
facts = {
    "env": env,
    "os_family": os_family,
    "system": sysname,
    "architecture": platform.machine(),
    "user_uid": uid,
    "user_id": user_id,
    "user_shell": env.get("SHELL", "/bin/sh"),
}
payload = {
    "ansible_facts_preloaded": True,
    "ansible_python_interpreter": py_bin,
    "ansible_env": env,
    "ansible_os_family": os_family,
    "ansible_system": sysname,
    "ansible_architecture": facts["architecture"],
    "ansible_facts": facts,
}
with open(out_path, "w", encoding="utf-8") as fh:
    json.dump(payload, fh)
PY
}

# ansible_test_wait_pids PIDS... waits for each background PID and fails if any
# exited non-zero.
ansible_test_wait_pids() {
	_wait_rc=0
	for _pid in "$@"; do
		if ! wait "$_pid"; then
			_wait_rc=1
		fi
	done
	return "$_wait_rc"
}
