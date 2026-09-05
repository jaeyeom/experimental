#!/bin/sh
# Tests for playbook_resume: parse Ansible failure logs and resolve --from
# to the include-guard task name in the leaf playbook file.
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
sh -n "$SCRIPT_DIR/playbook_resume.sh"
sh -n "$SCRIPT_DIR/playbook_resume_test.sh"
# shellcheck disable=SC1091  # Sourced from the same directory as this script
. "$SCRIPT_DIR/playbook_resume.sh"

_tmp=$(mktemp -d)
trap 'rm -rf "$_tmp"' EXIT

# --- failed_task_path / failed_task_name from a canned ansible-playbook log ---

_log="$_tmp/ansible.log"
cat >"$_log" <<'EOF'
PLAY [Ensure git.yml is present] ***********************************************

TASK [Include guard for git playbook] ******************************************
ok: [localhost]

PLAY [Ensure git is properly set up] *******************************************

TASK [Include guard for setup-git playbook] ************************************
ok: [localhost]

TASK [Update claude-toolbox marketplace] ***************************************
task path: /home/user/experimental/devtools/setup-dev/ansible/setup-git.yml:135
fatal: [localhost]: FAILED! =>
    msg: non-zero return code

PLAY RECAP *********************************************************************
localhost                  : ok=12   changed=0    unreachable=0    failed=1
EOF

assert_eq "failed_task_path uses last task path basename:line" \
    "$(failed_task_path "$_log")" \
    "setup-git.yml:135"

assert_eq "failed_task_name uses last TASK name" \
    "$(failed_task_name "$_log")" \
    "Update claude-toolbox marketplace"

_empty="$_tmp/empty.log"
: >"$_empty"
if failed_task_path "$_empty" >/dev/null; then
    fail "failed_task_path must fail when the log has no task path"
fi
if failed_task_name "$_empty" >/dev/null; then
    fail "failed_task_name must fail when the log has no TASK line"
fi

# --- include_guard_task_name reads the file; does not guess from the filename ---

_direnv="$_tmp/setup-direnv.yml"
cat >"$_direnv" <<'EOF'
---
- import_playbook: direnv.yml
- name: Setup direnv shell hooks
  hosts: all
  tasks:
    - name: Include guard for direnv setup playbook
      block:
        - name: Stop early if the direnv setup playbook is already included
          meta: end_play
          when: direnv_setup_playbook_imported is defined
EOF

assert_eq "direnv include-guard wording is not the filename" \
    "$(include_guard_task_name "$_direnv")" \
    "Stop early if the direnv setup playbook is already included"

_sandbox="$_tmp/setup-claude-sandbox.yml"
cat >"$_sandbox" <<'EOF'
---
- name: Setup Claude sandbox
  hosts: all
  tasks:
    - name: Include guard for claude sandbox playbook
      block:
        - name: Stop early if the claude sandbox playbook is already included
          meta: end_play
EOF

assert_eq "claude sandbox include-guard wording is not the filename" \
    "$(include_guard_task_name "$_sandbox")" \
    "Stop early if the claude sandbox playbook is already included"

_git="$_tmp/setup-git.yml"
cat >"$_git" <<'EOF'
- name: Include guard for setup-git playbook
  block:
    - name: Stop early if the setup-git playbook is already included
      meta: end_play
EOF

assert_eq "setup-git include-guard task name" \
    "$(include_guard_task_name "$_git")" \
    "Stop early if the setup-git playbook is already included"

# Suffix-less name is resolved relative to the given path's directory.
assert_eq "suffix-less name next to the file" \
    "$(include_guard_task_name "$_tmp/setup-git")" \
    "Stop early if the setup-git playbook is already included"

if include_guard_task_name "$_tmp/missing" 2>/dev/null; then
    fail "include_guard_task_name must fail when the playbook file is missing"
fi

_nofacts="$_tmp/gather-facts.yml"
cat >"$_nofacts" <<'EOF'
---
- name: Gather facts
  hosts: all
  gather_facts: true
  tasks: []
EOF

if include_guard_task_name "$_nofacts" 2>/dev/null; then
    fail "include_guard_task_name must fail when there is no include-guard task"
fi

_err=$(include_guard_task_name "$_nofacts" 2>&1) || true
case "$_err" in
    *"include-guard"*) ;;
    *) fail "missing include-guard error must mention include-guard, got '$_err'" ;;
esac

# --- print_playbook_failure summary ---

_summary=$(print_playbook_failure "all.yml" "$_log" " all" 2>&1)
assert_eq "failure summary" "$_summary" \
    "Error: playbook all.yml failed.
Failed in: setup-git.yml:135
Task: Update claude-toolbox marketplace
Resume from that play and continue:
  ./ensure.sh --from setup-git -- all"

_summary_generic=$(print_playbook_failure "all.yml" "$_empty" " all" 2>&1)
assert_eq "failure summary without task path" "$_summary_generic" \
    "Error: playbook all.yml failed."

echo "All playbook_resume tests passed."
