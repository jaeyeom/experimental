#!/bin/sh
# Tests setup-agent-roles-trust.yml and agent_roles_trust.py against a
# temporary HOME without touching the real home directory.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-agent-roles-trust.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"
HELPER="${SCRIPT_DIR}/agent_roles_trust.py"

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_RETRY_FILES_ENABLED=0
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible-local"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible-remote"

# 1. Tools not installed -> skipped cleanly, no config files created.
HOME_NONE="${ROOT}/home-none"
mkdir -p "$HOME_NONE"
OUT_NONE="${ROOT}/out-none.txt"
HOME="$HOME_NONE" PATH="${ANSIBLE_BIN_DIR}:/usr/bin:/bin" \
	ansible-playbook -i "$INVENTORY" "$PLAYBOOK" \
	--tags agent-roles-trust >"$OUT_NONE" 2>&1 || {
	cat "$OUT_NONE" >&2
	fail "playbook failed when no tools installed"
}
grep -q "changed=0" "$OUT_NONE" || {
	cat "$OUT_NONE" >&2
	fail "expected changed=0 when no tools installed"
}
for p in ".claude.json" ".claude/settings.json" ".codex/config.toml" \
	".gemini/antigravity-cli/settings.json" ".grok/trusted_folders.toml"; do
	[ ! -e "${HOME_NONE}/${p}" ] || fail "${p} should not be created when tools are missing"
done

# 2. All tools installed -> clean machine setup + idempotency.
FAKE_BIN="${ROOT}/bin"
mkdir -p "$FAKE_BIN"
for cmd in claude codex agy grok; do
	printf '#!/bin/sh\nexit 0\n' >"${FAKE_BIN}/${cmd}"
	chmod +x "${FAKE_BIN}/${cmd}"
done

HOME_CLEAN="${ROOT}/home-clean"
mkdir -p "$HOME_CLEAN"
ROLES_CLEAN="${HOME_CLEAN}/agents/roles"
OUT_CLEAN1="${ROOT}/out-clean1.txt"
HOME="$HOME_CLEAN" PATH="${FAKE_BIN}:${ANSIBLE_BIN_DIR}:/usr/bin:/bin" \
	ansible-playbook -i "$INVENTORY" "$PLAYBOOK" \
	--tags agent-roles-trust >"$OUT_CLEAN1" 2>&1 || {
	cat "$OUT_CLEAN1" >&2
	fail "playbook failed on clean machine"
}
grep -q "changed=1" "$OUT_CLEAN1" || {
	cat "$OUT_CLEAN1" >&2
	fail "expected changed=1 on first clean run"
}

python3 - "$HOME_CLEAN" "$ROLES_CLEAN" <<'PY'
import json, pathlib, sys

home = pathlib.Path(sys.argv[1])
roles = sys.argv[2]

claude_json = json.loads((home / ".claude.json").read_text())
assert claude_json["projects"][roles]["hasTrustDialogAccepted"] is True, claude_json

claude_settings = json.loads((home / ".claude/settings.json").read_text())
assert roles in claude_settings["permissions"]["additionalDirectories"], claude_settings
assert f"Read(/{roles}/**)" in claude_settings["permissions"]["allow"], claude_settings
assert f"Edit(/{roles}/**)" in claude_settings["permissions"]["allow"], claude_settings
assert "sandbox" not in claude_settings, claude_settings

codex_toml = (home / ".codex/config.toml").read_text()
assert f'[projects."{roles}"]\ntrust_level = "trusted"' in codex_toml, codex_toml
assert f'[sandbox_workspace_write]\nwritable_roots = ["{roles}"]' in codex_toml, codex_toml

agy_settings = json.loads((home / ".gemini/antigravity-cli/settings.json").read_text())
assert roles in agy_settings["trustedWorkspaces"], agy_settings
assert f"read_file({roles})" in agy_settings["permissions"]["allow"], agy_settings
assert f"write_file({roles})" in agy_settings["permissions"]["allow"], agy_settings

grok_tf = (home / ".grok/trusted_folders.toml").read_text()
assert f'[folders."{roles}"]\ntrusted = true\ndecided_at = ' in grok_tf, grok_tf
PY

# Capture exact file contents to verify second run is byte-for-byte idempotent.
SNAP1="${ROOT}/snap1"
mkdir -p "$SNAP1"
cp "${HOME_CLEAN}/.claude.json" "${SNAP1}/claude.json"
cp "${HOME_CLEAN}/.claude/settings.json" "${SNAP1}/claude_settings.json"
cp "${HOME_CLEAN}/.codex/config.toml" "${SNAP1}/codex.toml"
cp "${HOME_CLEAN}/.gemini/antigravity-cli/settings.json" "${SNAP1}/agy.json"
cp "${HOME_CLEAN}/.grok/trusted_folders.toml" "${SNAP1}/grok.toml"

OUT_CLEAN2="${ROOT}/out-clean2.txt"
HOME="$HOME_CLEAN" PATH="${FAKE_BIN}:${ANSIBLE_BIN_DIR}:/usr/bin:/bin" \
	ansible-playbook -i "$INVENTORY" "$PLAYBOOK" \
	--tags agent-roles-trust >"$OUT_CLEAN2" 2>&1 || {
	cat "$OUT_CLEAN2" >&2
	fail "playbook failed on second clean run"
}
grep -q "changed=0" "$OUT_CLEAN2" || {
	cat "$OUT_CLEAN2" >&2
	fail "expected changed=0 on second clean run"
}

cmp -s "${HOME_CLEAN}/.claude.json" "${SNAP1}/claude.json" || fail ".claude.json changed on rerun"
cmp -s "${HOME_CLEAN}/.claude/settings.json" "${SNAP1}/claude_settings.json" || fail ".claude/settings.json changed on rerun"
cmp -s "${HOME_CLEAN}/.codex/config.toml" "${SNAP1}/codex.toml" || fail ".codex/config.toml changed on rerun"
cmp -s "${HOME_CLEAN}/.gemini/antigravity-cli/settings.json" "${SNAP1}/agy.json" || fail "agy settings.json changed on rerun"
cmp -s "${HOME_CLEAN}/.grok/trusted_folders.toml" "${SNAP1}/grok.toml" || fail "grok trusted_folders.toml changed on rerun"

# 3. Merging into existing config files + sandbox enabled + custom agent_roles_dir.
HOME_MERGE="${ROOT}/home-merge"
mkdir -p "${HOME_MERGE}/.claude" "${HOME_MERGE}/.codex" \
	"${HOME_MERGE}/.gemini/antigravity-cli" "${HOME_MERGE}/.grok"
CUSTOM_ROLES="${HOME_MERGE}/custom/roles"

cat >"${HOME_MERGE}/.claude.json" <<'JSON'
{
  "userID": "keep-me",
  "projects": {
    "/existing/project": {
      "hasTrustDialogAccepted": true
    }
  }
}
JSON

cat >"${HOME_MERGE}/.claude/settings.json" <<'JSON'
{
  "includeCoAuthoredBy": false,
  "sandbox": {
    "enabled": true,
    "filesystem": {
      "allowWrite": ["/existing/write"]
    }
  },
  "permissions": {
    "additionalDirectories": ["/existing/dir"],
    "allow": ["Bash(git status *)"]
  }
}
JSON

cat >"${HOME_MERGE}/.codex/config.toml" <<'TOML'
# Existing user comment
model = "gpt-5"

[projects."/existing/project"]
trust_level = "trusted"

[sandbox_workspace_write]
network_access = false
writable_roots = [
  "/existing/root",
]
TOML

cat >"${HOME_MERGE}/.gemini/antigravity-cli/settings.json" <<'JSON'
{
  "model": "gemini-2.5-pro",
  "trustedWorkspaces": ["/existing/workspace"],
  "permissions": {
    "allow": ["command(git status)"]
  }
}
JSON

cat >"${HOME_MERGE}/.grok/trusted_folders.toml" <<'TOML'
[folders."/existing/folder"]
trusted = true
decided_at = 1700000000
TOML

OUT_MERGE="${ROOT}/out-merge.txt"
HOME="$HOME_MERGE" PATH="${FAKE_BIN}:${ANSIBLE_BIN_DIR}:/usr/bin:/bin" \
	ansible-playbook -i "$INVENTORY" "$PLAYBOOK" \
	--tags agent-roles-trust \
	-e "agent_roles_dir=${CUSTOM_ROLES}" >"$OUT_MERGE" 2>&1 || {
	cat "$OUT_MERGE" >&2
	fail "playbook failed on merge scenario"
}
grep -q "changed=1" "$OUT_MERGE" || {
	cat "$OUT_MERGE" >&2
	fail "expected changed=1 on merge run"
}

python3 - "$HOME_MERGE" "$CUSTOM_ROLES" <<'PY'
import json, pathlib, sys

home = pathlib.Path(sys.argv[1])
roles = sys.argv[2]

cj = json.loads((home / ".claude.json").read_text())
assert cj["userID"] == "keep-me"
assert cj["projects"]["/existing/project"]["hasTrustDialogAccepted"] is True
assert cj["projects"][roles]["hasTrustDialogAccepted"] is True

cs = json.loads((home / ".claude/settings.json").read_text())
assert cs["includeCoAuthoredBy"] is False
assert cs["sandbox"]["filesystem"]["allowWrite"] == ["/existing/write", roles]
assert cs["permissions"]["additionalDirectories"] == ["/existing/dir", roles]
assert cs["permissions"]["allow"] == [
    "Bash(git status *)",
    f"Read(/{roles}/**)",
    f"Edit(/{roles}/**)",
]

ct = (home / ".codex/config.toml").read_text()
assert "# Existing user comment" in ct
assert 'model = "gpt-5"' in ct
assert '[projects."/existing/project"]' in ct
assert f'[projects."{roles}"]\ntrust_level = "trusted"' in ct
assert "network_access = false" in ct
assert f'writable_roots = ["/existing/root", "{roles}"]' in ct

ag = json.loads((home / ".gemini/antigravity-cli/settings.json").read_text())
assert ag["model"] == "gemini-2.5-pro"
assert ag["trustedWorkspaces"] == ["/existing/workspace", roles]
assert ag["permissions"]["allow"] == [
    "command(git status)",
    f"read_file({roles})",
    f"write_file({roles})",
]

gt = (home / ".grok/trusted_folders.toml").read_text()
assert '[folders."/existing/folder"]' in gt
assert "decided_at = 1700000000" in gt
assert f'[folders."{roles}"]\ntrusted = true' in gt
PY

# 4. Security check: refuse trusting HOME or /.
if HOME="$HOME_CLEAN" python3 "$HELPER" --roles-dir "$HOME_CLEAN" >/dev/null 2>&1; then
	fail "helper should refuse trusting HOME"
fi
if HOME="$HOME_CLEAN" python3 "$HELPER" --roles-dir "/" >/dev/null 2>&1; then
	fail "helper should refuse trusting /"
fi

echo "PASS: all agent_roles_trust tests passed"
