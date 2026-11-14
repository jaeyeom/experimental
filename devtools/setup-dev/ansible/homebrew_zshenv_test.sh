#!/bin/sh
# setup-homebrew-env.yml prepends Homebrew to PATH in ~/.zshenv
# (jaeyeom/experimental#760). Only the homebrew-zshenv tag runs, against
# a temp HOME. Brew prefixes are temp files, so the real ~/.zshenv is
# not written.
#
#   - Apple Silicon prefix wins over Intel.
#   - Intel is used when only that brew exists.
#   - Neither brew: ~/.zshenv is not created.
#   - A second run does not duplicate the block, and sourcing it twice
#     does not duplicate PATH entries.
#   - A non-Darwin family does not create or edit ~/.zshenv.
set -eu

fail() {
	printf 'FAIL: %s\n' "$1" >&2
	exit 1
}

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYBOOK="${SCRIPT_DIR}/setup-homebrew-env.yml"
INVENTORY="${SCRIPT_DIR}/inventory.ini"

if ! command -v ansible-playbook >/dev/null 2>&1; then
	echo "SKIP: ansible-playbook not found"
	exit 0
fi
ANSIBLE_BIN_DIR=$(dirname "$(command -v ansible-playbook)")

REAL_HOME=${HOME:-}
REAL_ZSHENV=""
REAL_SUM=""
if [ -n "$REAL_HOME" ]; then
	REAL_ZSHENV="${REAL_HOME}/.zshenv"
	if [ -f "$REAL_ZSHENV" ]; then
		REAL_SUM=$(cksum "$REAL_ZSHENV")
	fi
fi

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT

export ANSIBLE_NOCOLOR=1
export ANSIBLE_STDOUT_CALLBACK=default
export ANSIBLE_RETRY_FILES_ENABLED=0
export ANSIBLE_LOCAL_TEMP="${ROOT}/ansible-local"
export ANSIBLE_REMOTE_TEMP="${ROOT}/ansible-remote"
export PATH="${ANSIBLE_BIN_DIR}:/usr/bin:/bin"

OUT="${ROOT}/out"
ZSH=$(command -v zsh || true)

grep -q 'user_profile_path' "$PLAYBOOK" || fail "interactive Homebrew block was removed"

# fake_brew PATH creates an executable brew.
fake_brew() {
	mkdir -p "$(dirname "$1")"
	printf '%s\n' '#!/bin/sh' 'exit 0' >"$1"
	chmod +x "$1"
}

# use_home NAME switches HOME to a fresh temp directory.
use_home() {
	HOME="${ROOT}/$1"
	export HOME
	mkdir -p "$HOME"
}

# run EXTRA... runs the tagged playbook.
run() {
	if ! ansible-playbook -i "$INVENTORY" --tags homebrew-zshenv "$@" "$PLAYBOOK" >"$OUT" 2>&1; then
		cat "$OUT" >&2
		fail "ansible-playbook failed"
	fi
	if grep -q 'libc.so' "$OUT"; then
		echo "SKIP: ansible-playbook cannot run in sandbox (libc issue)"
		exit 0
	fi
}

# path_count DIR counts PATH entries equal to DIR.
path_count() {
	printf '%s' "$2" | awk -F: -v d="$1" '{c=0; for(i=1;i<=NF;i++) if($i==d) c++; print c}'
}

# expect_prefix BIN SBIN checks the block, command -v, and a second run.
expect_prefix() {
	_bin=$1
	_sbin=$2
	shift 2
	_file="${HOME}/.zshenv"
	grep -q "BEGIN setup-dev Homebrew PATH" "$_file" || fail "zshenv: block missing for ${_bin}"
	grep -q "$_bin" "$_file" || fail "zshenv: ${_bin} missing"
	grep -q "$_sbin" "$_file" || fail "zshenv: ${_sbin} missing"
	if [ -n "$ZSH" ]; then
		_found=$(env -i HOME="$HOME" PATH=/usr/bin:/bin:/usr/sbin:/sbin "$ZSH" -c 'command -v brew')
		[ "$_found" = "${_bin}/brew" ] || fail "zshenv: command -v brew: ${_found}"
		# shellcheck disable=SC2016 # $HOME and $PATH are for zsh, not this shell.
		_path=$(env -i HOME="$HOME" PATH=/usr/bin:/bin:/usr/sbin:/sbin "$ZSH" -c '. "$HOME/.zshenv"; printf %s "$PATH"')
		[ "$(path_count "$_bin" "$_path")" -eq 1 ] || fail "zshenv: PATH duplicated ${_bin}: ${_path}"
		[ "$(path_count "$_sbin" "$_path")" -eq 1 ] || fail "zshenv: PATH duplicated ${_sbin}: ${_path}"
	fi
	cp "$_file" "${ROOT}/zshenv.first"
	run "$@"
	if ! grep -q 'changed=0 ' "$OUT"; then
		cat "$OUT" >&2
		fail "second run changed something"
	fi
	cmp -s "$_file" "${ROOT}/zshenv.first" || fail "second run rewrote ~/.zshenv"
	[ "$(grep -c 'BEGIN setup-dev Homebrew PATH' "$_file")" -eq 1 ] || fail "block repeated"
}

# Apple Silicon wins when both brews exist.
use_home opt
OPT="${ROOT}/prefixes/opt"
USR="${ROOT}/prefixes/usr"
fake_brew "${OPT}/bin/brew"
fake_brew "${USR}/bin/brew"
printf '%s\n' '# keep me' >"${HOME}/.zshenv"
run -e homebrew_env_os=Darwin -e "homebrew_opt_brew=${OPT}/bin/brew" -e "homebrew_usr_brew=${USR}/bin/brew"
grep -q 'keep me' "${HOME}/.zshenv" || fail "opt: existing zshenv text dropped"
grep -q "${USR}/bin" "${HOME}/.zshenv" && fail "opt: Intel prefix was written"
expect_prefix "${OPT}/bin" "${OPT}/sbin" -e homebrew_env_os=Darwin -e "homebrew_opt_brew=${OPT}/bin/brew" -e "homebrew_usr_brew=${USR}/bin/brew"

# Intel when the Apple Silicon brew is absent.
use_home intel
fake_brew "${USR}/bin/brew"
run -e homebrew_env_os=Darwin -e "homebrew_opt_brew=${OPT}/missing/bin/brew" -e "homebrew_usr_brew=${USR}/bin/brew"
grep -q "${OPT}/" "${HOME}/.zshenv" && fail "intel: Apple Silicon prefix was written"
expect_prefix "${USR}/bin" "${USR}/sbin" -e homebrew_env_os=Darwin -e "homebrew_opt_brew=${OPT}/missing/bin/brew" -e "homebrew_usr_brew=${USR}/bin/brew"

# Neither brew: do not create ~/.zshenv.
use_home none
run -e homebrew_env_os=Darwin -e "homebrew_opt_brew=${OPT}/missing/bin/brew" -e "homebrew_usr_brew=${USR}/missing/bin/brew"
[ ! -e "${HOME}/.zshenv" ] || fail "neither: ~/.zshenv was created"

# Non-Darwin does not edit an existing ~/.zshenv.
use_home linux
printf '%s\n' '# leave me' >"${HOME}/.zshenv"
cp "${HOME}/.zshenv" "${ROOT}/linux.before"
run -e homebrew_env_os=Debian -e "homebrew_opt_brew=${OPT}/bin/brew" -e "homebrew_usr_brew=${USR}/bin/brew"
cmp -s "${HOME}/.zshenv" "${ROOT}/linux.before" || fail "linux: ~/.zshenv changed"

# The default paths, on a Mac that has Homebrew.
if [ "$(uname -s)" = "Darwin" ]; then
	if [ -x /opt/homebrew/bin/brew ]; then
		use_home real
		run -e homebrew_env_os=Darwin
		expect_prefix /opt/homebrew/bin /opt/homebrew/sbin -e homebrew_env_os=Darwin
	elif [ -x /usr/local/bin/brew ]; then
		use_home real
		run -e homebrew_env_os=Darwin
		expect_prefix /usr/local/bin /usr/local/sbin -e homebrew_env_os=Darwin
	fi
fi

if [ -n "$REAL_SUM" ] && [ "$(cksum "$REAL_ZSHENV")" != "$REAL_SUM" ]; then
	fail "real ~/.zshenv changed"
fi
