#!/bin/sh

# Script ensure.sh runs the provided playbooks with the provided arguments.

LOG_FILE="$HOME/.cache/setup-dev/ensure_sh.log"
LOG_DIR=$(dirname "$LOG_FILE")
mkdir -p "$LOG_DIR"

# Generate a simple session ID. Prefer /dev/urandom when available, fall back to time+pid.
if [ -r /dev/urandom ]; then
    SESSION_ID=$(LC_ALL=C tr -dc 'A-Za-z0-9' </dev/urandom | head -c 16)
else
    SESSION_ID="$(date +%s)-$$"
fi

finish() {
    exit_code=$?
    finish_reason="Success"
    if [ "$exit_code" -ne 0 ]; then
        finish_reason="Failure (Exit Code: $exit_code)"
    fi

    {
        echo "---"
        echo "Session ID: $SESSION_ID"
        echo "Finish Time: $(date)"
        echo "Finish Reason: $finish_reason"
    } >>"$LOG_FILE"
}
trap 'finish' EXIT

{
    echo "---"
    echo "Session ID: $SESSION_ID"
    echo "Start Time: $(date)"
    echo "Arguments: $*"
} >>"$LOG_FILE"

CACHE_DIR="$HOME/.cache/last_upgrade"
mkdir -p "$CACHE_DIR"

PKG_CACHE="$CACHE_DIR/termux-pkg-upgrade"
BREW_CACHE="$CACHE_DIR/brew-update"
NALA_CACHE="$CACHE_DIR/nala-upgrade"
PIP_CACHE="$CACHE_DIR/pip"
RUSTUP_CACHE="$CACHE_DIR/rustup-update"
ANSIBLE_GALAXY_CACHE="$CACHE_DIR/ansible-galaxy-collection"

# Detect OS
OS="$(uname -s)"

# Collect all playbook files transitively from the given arguments.
# Outputs one playbook path per line (deduplicated). Skips flags (args
# starting with -) and appends .yml when missing.
_cp_visited=""
_collect_playbooks_recurse() {
    _cpr_file="$1"
    [ -f "$_cpr_file" ] || return 0
    case " $_cp_visited " in *" $_cpr_file "*) return 0 ;; esac
    _cp_visited="$_cp_visited $_cpr_file"
    echo "$_cpr_file"
    # shellcheck disable=SC2013  # Playbook filenames never contain spaces
    for _cpr_dep in $(sed -n 's/^- import_playbook: *//p' "$_cpr_file"); do
        _collect_playbooks_recurse "$_cpr_dep"
    done
}

collect_playbooks() {
    _cp_visited=""
    for _cb_pb in "$@"; do
        case "$_cb_pb" in -*) continue ;; esac
        case "$_cb_pb" in *.yml) ;; *) _cb_pb="$_cb_pb.yml" ;; esac
        _collect_playbooks_recurse "$_cb_pb"
    done
}

# Check if any non-flag argument transitively imports a target playbook.
# Usage: any_arg_imports TARGET "$@"
any_arg_imports() {
    _ai_target="$1"; shift
    collect_playbooks "$@" 2>/dev/null | grep -qx "$_ai_target"
}

# Pre-approve verified-run URLs found in the transitively-collected playbooks.
# Runs before the Ansible playbook loop so that non-interactive verified-run
# exec calls don't fail mid-run.
pre_approve_urls() {
    _pa_script_dir="$(dirname "$0")"
    _pa_urls=$(collect_playbooks "$@" | while read -r _pa_f; do
        grep 'verified-run exec' "$_pa_f" 2>/dev/null | grep -o 'https://[^ ]*'
    done | sort -u)

    # shellcheck disable=SC2086  # Intentional word splitting — URLs never contain spaces
    for _pa_url in $_pa_urls; do
        if ! "$_pa_script_dir/verified-run" check "$_pa_url" 2>/dev/null; then
            echo "Script needs approval: $_pa_url"
            "$_pa_script_dir/verified-run" review "$_pa_url"
        fi
    done
}

# Pre-check for SSH key authentication
ssh_key_exists=false
for key_file in "$HOME/.ssh/id_ed25519" "$HOME/.ssh/id_ed25519_sk" "$HOME/.ssh/id_ecdsa_sk"; do
    if [ -f "$key_file" ]; then
        ssh_key_exists=true
        break
    fi
done

if [ "$ssh_key_exists" = false ]; then
    echo "Error: No SSH key found." >&2
    echo "Generate one with:" >&2
    echo "  ssh-keygen -t ed25519 -C \"your_email@example.com\"" >&2
    exit 1
fi

ssh_add_exit=0
ssh-add -l >/dev/null 2>&1 || ssh_add_exit=$?
if [ "$ssh_add_exit" -eq 2 ]; then
    # Exit code 2 means the agent is not running
    if [ -n "$SSH_AGENT_PID" ]; then
        echo "Error: SSH agent is running but has no keys loaded." >&2
        echo "Add your key with:" >&2
        echo "  ssh-add" >&2
    elif [ -n "$TERMUX_VERSION" ]; then
        echo "Error: SSH agent is not running." >&2
        echo "Enable the ssh-agent service with:" >&2
        echo "  sv-enable ssh-agent" >&2
        echo "Then restart your shell." >&2
    elif command -v keychain >/dev/null 2>&1; then
        echo "Error: SSH agent is not running." >&2
        echo "Start the agent with keychain:" >&2
        echo "  eval \$(keychain --eval --agents ssh id_ed25519)" >&2
    else
        echo "Error: SSH agent is not running." >&2
        echo "Install keychain and start the agent:" >&2
        if [ "$OS" = "Darwin" ]; then
            echo "  brew install keychain" >&2
        else
            echo "  sudo apt install keychain  # or your package manager" >&2
        fi
        echo "  eval \$(keychain --eval --agents ssh id_ed25519)" >&2
    fi
    exit 1
elif [ "$ssh_add_exit" -eq 1 ]; then
    # Exit code 1 means the agent is running but has no keys
    if [ -z "$SSHPASS" ]; then
        echo "Error: No SSH keys are added to the agent, and the SSHPASS environment variable is not set." >&2
        if [ -n "$TERMUX_VERSION" ]; then
            echo "Add your key with:" >&2
            echo "  ssh-add ~/.ssh/id_ed25519" >&2
        elif command -v keychain >/dev/null 2>&1; then
            echo "Add your key via keychain:" >&2
            echo "  eval \$(keychain --eval --agents ssh id_ed25519)" >&2
        else
            echo "Add your key with:" >&2
            echo "  ssh-add ~/.ssh/id_ed25519" >&2
        fi
        exit 1
    fi
fi

# Pre-check for git identity variables when setup-git is transitively needed.
#
# The setup-git playbook needs your name, email, and GitHub username to
# configure git.  It reads from environment variables first and falls back
# to existing git config values.  Once git config is fully populated by
# the playbook, you no longer need to export these variables on subsequent
# runs.
if any_arg_imports setup-git.yml "$@"; then
    git_check_failed=false
    if [ -z "$GIT_AUTHOR_NAME" ] && [ -z "$(git config --global user.name 2>/dev/null)" ]; then
        echo "Error: Git user name is not configured." >&2
        echo "  Either set the environment variable for this run:" >&2
        echo "    export GIT_AUTHOR_NAME=\"Your Name\"" >&2
        echo "  Or configure git directly (permanent):" >&2
        echo "    git config --global user.name \"Your Name\"" >&2
        git_check_failed=true
    fi
    if [ -z "$GIT_AUTHOR_EMAIL" ] && [ -z "$(git config --global user.email 2>/dev/null)" ]; then
        echo "Error: Git user email is not configured." >&2
        echo "  Either set the environment variable for this run:" >&2
        echo "    export GIT_AUTHOR_EMAIL=\"you@example.com\"" >&2
        echo "  Or configure git directly (permanent):" >&2
        echo "    git config --global user.email \"you@example.com\"" >&2
        git_check_failed=true
    fi
    if [ -z "$GITHUB_USERNAME" ] && [ -z "$(git config --global github.user 2>/dev/null)" ]; then
        echo "Error: GitHub username is not configured." >&2
        echo "  Either set the environment variable for this run:" >&2
        echo "    export GITHUB_USERNAME=\"your-github-handle\"" >&2
        echo "  Or configure git directly (permanent):" >&2
        echo "    git config --global github.user \"your-github-handle\"" >&2
        git_check_failed=true
    fi
    if [ "$git_check_failed" = true ]; then
        echo "" >&2
        echo "Note: The environment variables are only needed for the first run." >&2
        echo "Once setup-git writes your git config, they are no longer required." >&2
        exit 1
    fi
fi

# Interactive prompts for SSH key generation when setup-ssh-key is transitively needed
if any_arg_imports setup-ssh-key.yml "$@"; then
    if [ ! -f "$HOME/.ssh/id_ed25519" ] && [ ! -f "$HOME/.ssh/id_ed25519_sk" ] && [ ! -f "$HOME/.ssh/id_ecdsa_sk" ]; then
        # Determine default email from git config or environment
        default_email="${GIT_AUTHOR_EMAIL:-$(git config --global user.email 2>/dev/null)}"

        printf "Enter your email address for the SSH key comment [%s]: " "$default_email"
        read -r ssh_email_input
        if [ -n "$ssh_email_input" ]; then
            export SSH_KEY_COMMENT="$ssh_email_input"
        elif [ -n "$default_email" ]; then
            export SSH_KEY_COMMENT="$default_email"
        fi

        printf "Do you have a hardware security key (e.g., YubiKey)? (yes/no) [no]: "
        read -r ssh_hw_input
        case "$ssh_hw_input" in
            [Yy]|[Yy][Ee][Ss]) export SSH_USE_HARDWARE_KEY=yes ;;
            *) export SSH_USE_HARDWARE_KEY=no ;;
        esac
    fi
fi

# Set GITHUB_TOKEN from gh CLI if not already set (for higher API rate limits)
if [ -z "$GITHUB_TOKEN" ] && command -v gh >/dev/null 2>&1; then
    if gh_token=$(gh auth token 2>/dev/null); then
        export GITHUB_TOKEN="$gh_token"
    fi
fi

if [ -n "$TERMUX_VERSION" ]; then
    # Ansible remote temp directory is messed up on Termux like
    # /data/.ansible/tmp, so we need to set it to a writable directory.
    export ANSIBLE_REMOTE_TEMP=$HOME/.ansible/tmp

    # Upgrading pkg is necessary to avoid issues on Termux. Instead of handling
    # that in Ansible, we do it here.
    # Only upgrade if not done in the last 24 hours
    if [ ! -f "$PKG_CACHE" ] || [ "$(find "$PKG_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        pkg upgrade -y
        pkg install -y rust python-pip python-cryptography
        touch "$PKG_CACHE"
    fi

    if ! command -v rustc >/dev/null 2>&1; then
        echo "Installing Rust..."
        pkg install -y rust
    fi
    if ! command -v pip >/dev/null 2>&1; then
        echo "Installing pip..."
        pkg install -y python-pip
    fi
    if ! python -c "import cryptography" >/dev/null 2>&1; then
        echo "Installing python-cryptography..."
        pkg install -y python-cryptography
    fi

    # Termux's CPython is built without the grp C extension because
    # Android's Bionic libc lacks the POSIX group database API.  Ansible
    # imports grp unconditionally, so provide a minimal stub.
    if ! python -c "import grp" >/dev/null 2>&1; then
        echo "Creating grp stub module for Termux..."
        _site_packages="$(python -c 'import site; print(site.getsitepackages()[0])')"
        cat > "$_site_packages/grp.py" <<'STUBEOF'
"""Minimal grp stub for Termux where the C extension is unavailable."""

class struct_group(tuple):
    __slots__ = ()
    gr_name = property(lambda s: s[0])
    gr_passwd = property(lambda s: s[1])
    gr_gid = property(lambda s: s[2])
    gr_mem = property(lambda s: s[3])
    def __new__(cls, name, passwd, gid, mem):
        return tuple.__new__(cls, (name, passwd, gid, mem))
    def __repr__(self):
        return ("grp.struct_group(gr_name=%r, gr_passwd=%r, "
                "gr_gid=%r, gr_mem=%r)" % self)

def getgrgid(gid):
    return struct_group(str(gid), "x", gid, [])

def getgrnam(name):
    return struct_group(name, "x", 0, [])

def getgrall():
    return []
STUBEOF
    fi

    # Termux's CPython is also built without the _multiprocessing C
    # extension (semaphore-based locks).  Ansible needs it for its task
    # queue.  Provide a ctypes wrapper around libandroid-posix-semaphore.
    if ! python -c "import _multiprocessing" >/dev/null 2>&1; then
        echo "Creating _multiprocessing stub module for Termux..."
        _site_packages="$(python -c 'import site; print(site.getsitepackages()[0])')"
        cat > "$_site_packages/_multiprocessing.py" <<'STUBEOF'
"""ctypes-based _multiprocessing for Termux (missing C extension).

Wraps libandroid-posix-semaphore to provide SemLock, which the stdlib
multiprocessing.synchronize module requires.
"""

import ctypes
import errno
import os
import threading
import time

_libsem = ctypes.CDLL("libandroid-posix-semaphore.so", use_errno=True)

_libsem.sem_open.restype = ctypes.c_void_p
_libsem.sem_open.argtypes = [ctypes.c_char_p, ctypes.c_int,
                              ctypes.c_uint, ctypes.c_uint]
_libsem.sem_close.restype = ctypes.c_int
_libsem.sem_close.argtypes = [ctypes.c_void_p]
_libsem.sem_unlink.restype = ctypes.c_int
_libsem.sem_unlink.argtypes = [ctypes.c_char_p]
_libsem.sem_wait.restype = ctypes.c_int
_libsem.sem_wait.argtypes = [ctypes.c_void_p]
_libsem.sem_trywait.restype = ctypes.c_int
_libsem.sem_trywait.argtypes = [ctypes.c_void_p]
_libsem.sem_post.restype = ctypes.c_int
_libsem.sem_post.argtypes = [ctypes.c_void_p]
_libsem.sem_getvalue.restype = ctypes.c_int
_libsem.sem_getvalue.argtypes = [ctypes.c_void_p,
                                  ctypes.POINTER(ctypes.c_int)]


class _timespec(ctypes.Structure):
    _fields_ = [("tv_sec", ctypes.c_long), ("tv_nsec", ctypes.c_long)]


_libsem.sem_timedwait.restype = ctypes.c_int
_libsem.sem_timedwait.argtypes = [ctypes.c_void_p,
                                   ctypes.POINTER(_timespec)]

_SEM_FAILED = ctypes.c_void_p(-1).value


def sem_unlink(name):
    if isinstance(name, str):
        name = name.encode("utf-8")
    if _libsem.sem_unlink(name) < 0:
        err = ctypes.get_errno()
        if err != errno.ENOENT:
            raise OSError(err, os.strerror(err))


class SemLock:
    # libandroid-posix-semaphore crashes with values above ~1 billion.
    SEM_VALUE_MAX = 100000000

    def __init__(self, kind, value, maxvalue, name, unlink_now=True):
        self.kind = kind
        self.maxvalue = maxvalue
        self.name = name if not unlink_now else None
        bname = name.encode("utf-8") if isinstance(name, str) else name
        handle = _libsem.sem_open(
            bname, os.O_CREAT | os.O_EXCL, 0o600, value)
        if handle == _SEM_FAILED:
            err = ctypes.get_errno()
            if err == errno.EEXIST:
                raise FileExistsError(errno.EEXIST, os.strerror(errno.EEXIST))
            raise OSError(err, os.strerror(err))
        self.handle = handle
        self._bname = bname
        if unlink_now:
            _libsem.sem_unlink(bname)
        self._owner_tid = None
        self._count_val = 0
        self._lock = threading.Lock()

    def acquire(self, block=True, timeout=None):
        if not block:
            rc = _libsem.sem_trywait(self.handle)
            if rc < 0:
                err = ctypes.get_errno()
                if err == errno.EAGAIN:
                    return False
                raise OSError(err, os.strerror(err))
        elif timeout is not None:
            deadline = time.time() + timeout
            ts = _timespec()
            ts.tv_sec = int(deadline)
            ts.tv_nsec = int((deadline - int(deadline)) * 1e9)
            rc = _libsem.sem_timedwait(self.handle, ctypes.byref(ts))
            if rc < 0:
                err = ctypes.get_errno()
                if err == errno.ETIMEDOUT:
                    return False
                raise OSError(err, os.strerror(err))
        else:
            rc = _libsem.sem_wait(self.handle)
            if rc < 0:
                err = ctypes.get_errno()
                raise OSError(err, os.strerror(err))
        tid = threading.get_ident()
        with self._lock:
            self._owner_tid = tid
            self._count_val += 1
        return True

    def release(self):
        with self._lock:
            if self._count_val > 0:
                self._count_val -= 1
            if self._count_val == 0:
                self._owner_tid = None
        if _libsem.sem_post(self.handle) < 0:
            err = ctypes.get_errno()
            raise OSError(err, os.strerror(err))

    def __enter__(self):
        self.acquire()
        return self

    def __exit__(self, *args):
        self.release()

    def _count(self):
        with self._lock:
            if self._owner_tid == threading.get_ident():
                return self._count_val
            return 0

    def _is_mine(self):
        with self._lock:
            return self._owner_tid == threading.get_ident()

    def _get_value(self):
        val = ctypes.c_int()
        if _libsem.sem_getvalue(self.handle, ctypes.byref(val)) < 0:
            err = ctypes.get_errno()
            raise OSError(err, os.strerror(err))
        return val.value

    def _is_zero(self):
        return self._get_value() == 0

    def _after_fork(self):
        self._owner_tid = None
        self._count_val = 0
        self._lock = threading.Lock()

    @classmethod
    def _rebuild(cls, handle, kind, maxvalue, name):
        self = cls.__new__(cls)
        self.handle = handle
        self.kind = kind
        self.maxvalue = maxvalue
        self.name = name
        self._bname = (name.encode("utf-8")
                       if isinstance(name, str) else name)
        self._owner_tid = None
        self._count_val = 0
        self._lock = threading.Lock()
        return self

    def __del__(self):
        try:
            _libsem.sem_close(self.handle)
        except Exception:
            pass
STUBEOF
    fi

    # Install necessary packages for Ansible and also install ansible.
    if [ ! -f "$PIP_CACHE" ] || [ "$(find "$PIP_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        pip install -U ansible
    fi
elif [ "$OS" = "Darwin" ]; then
    # Check if Homebrew is installed
    if ! command -v brew >/dev/null 2>&1; then
        echo "Installing Homebrew..."
        BREW_URL="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
        SCRIPT_DIR="$(dirname "$0")"

        # Use verified-run with interactive mode for on-the-fly approval
        if ! "$SCRIPT_DIR/verified-run" exec --interactive "$BREW_URL"; then
            echo "Homebrew installation cancelled or failed."
            exit 1
        fi

        # Add Homebrew to PATH based on chip architecture
        if [ "$(uname -m)" = "arm64" ]; then
            # For Apple Silicon Macs
            # shellcheck disable=SC2016  # Single quotes intentional - write literal string to .zprofile
            echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> "$HOME/.zprofile"
            eval "$(/opt/homebrew/bin/brew shellenv)"
        else
            # For Intel Macs
            # shellcheck disable=SC2016  # Single quotes intentional - write literal string to .zprofile
            echo 'eval "$(/usr/local/bin/brew shellenv)"' >> "$HOME/.zprofile"
            eval "$(/usr/local/bin/brew shellenv)"
        fi
    fi

    # Update Homebrew
    # Only update if not done in the last 24 hours
    if [ ! -f "$BREW_CACHE" ] || [ "$(find "$BREW_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        echo "Updating Homebrew..."
        brew update && brew upgrade
        touch "$BREW_CACHE"
    fi

    # Install Ansible if not already installed
    if ! command -v ansible >/dev/null 2>&1; then
        echo "Installing Ansible..."
        brew install ansible
    fi

    # Install python-packaging if not already installed
    if ! python -c "import packaging" >/dev/null 2>&1; then
        echo "Installing python-packaging..."
        brew install python-packaging
    fi
else
    # Detect whether we can use sudo for system package operations
    CAN_SUDO=false
    if [ "$(id -u)" -eq 0 ]; then
        CAN_SUDO=true
    elif command -v sudo >/dev/null 2>&1; then
        if sudo -n true 2>/dev/null; then
            CAN_SUDO=true
        elif [ -t 0 ]; then
            # Interactive terminal: prompt the user for password
            echo "sudo access is needed for system package operations."
            if sudo -v; then
                CAN_SUDO=true
            fi
        fi
    fi
    if [ "$CAN_SUDO" = false ]; then
        echo "Warning: sudo is not available. System package operations will be skipped."
    fi

    # Check if we're on a Debian/Ubuntu system and handle nala
    if [ "$CAN_SUDO" = true ] && command -v apt >/dev/null 2>&1; then
        # Check if nala is installed, install if not
        if ! command -v nala >/dev/null 2>&1; then
            echo "Installing nala..."
            sudo apt update
            sudo apt install -y nala
        fi

        # Upgrade packages with nala
        # Only upgrade if not done in the last 24 hours
        if [ ! -f "$NALA_CACHE" ] || [ "$(find "$NALA_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
            echo "Upgrading packages with nala..."
            sudo nala upgrade -y
            touch "$NALA_CACHE"
        fi
    fi

    # Install Ansible if not already installed on Linux systems
    if ! command -v ansible >/dev/null 2>&1; then
        echo "Installing Ansible..."
        if [ "$CAN_SUDO" = true ]; then
            if command -v nala >/dev/null 2>&1; then
                sudo nala install -y ansible
            elif command -v apt >/dev/null 2>&1; then
                sudo apt update
                sudo apt install -y ansible
            elif command -v yum >/dev/null 2>&1; then
                sudo yum install -y epel-release
                sudo yum install -y ansible
            elif command -v dnf >/dev/null 2>&1; then
                sudo dnf install -y ansible
            else
                echo "Unable to install Ansible - unsupported package manager"
                exit 1
            fi
        elif command -v pip >/dev/null 2>&1; then
            pip install --user ansible
        elif command -v pip3 >/dev/null 2>&1; then
            pip3 install --user ansible
        else
            echo "Error: Cannot install Ansible. No sudo access and no pip/pip3 available." >&2
            echo "Please install Ansible manually (e.g., 'pip install --user ansible')." >&2
            exit 1
        fi
    fi
fi

# Update rustup if installed (upgrades rust toolchain and all components)
# Only update if not done in the last 24 hours
if command -v rustup >/dev/null 2>&1; then
    if [ ! -f "$RUSTUP_CACHE" ] || [ "$(find "$RUSTUP_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
        echo "Updating Rust toolchain via rustup..."
        rustup update
        touch "$RUSTUP_CACHE"
    fi
fi

# Install community.general collection if not already installed
if [ ! -f "$ANSIBLE_GALAXY_CACHE" ] || [ "$(find "$ANSIBLE_GALAXY_CACHE" -mtime +1 2>/dev/null | wc -l)" -gt 0 ]; then
    ansible-galaxy collection install community.general
    touch "$ANSIBLE_GALAXY_CACHE"
fi

# Parse arguments: separate Ansible flags from playbook targets.
# Arguments starting with - are treated as Ansible flags; all others are
# playbook targets.  Use -- to separate flags from playbooks when passing
# flags that take a value argument:
#   ./ensure.sh --tags setup -- profile-cloudflare-dev
# Without --, a flag's value argument would be mistaken for a playbook name.
flags=""
playbooks=""
_past_separator=false
for _arg in "$@"; do
    if [ "$_past_separator" = true ]; then
        playbooks="$playbooks $_arg"
    elif [ "$_arg" = "--" ]; then
        _past_separator=true
    else
        case "$_arg" in
            -*) flags="$flags $_arg" ;;
            *) playbooks="$playbooks $_arg" ;;
        esac
    fi
done

# Pre-approve any verified-run URLs before the Ansible run so that
# non-interactive verified-run exec calls inside playbooks don't fail.
pre_approve_urls "$@"

# Run each playbook target, tracking failures.
any_failed=false
# shellcheck disable=SC2086  # Intentional word splitting — playbook names never contain spaces
for playbook in $playbooks; do
    # Add .yml suffix only if it doesn't already exist
    case "$playbook" in
        *.yml) ;;
        *) playbook="$playbook.yml" ;;
    esac
    echo "==> Running playbook: $playbook"
    # shellcheck disable=SC2086  # Intentional word splitting for flags
    if ! ansible-playbook -i inventory.ini $flags "$playbook"; then
        echo "Error: playbook $playbook failed." >&2
        any_failed=true
    fi
done

if [ "$any_failed" = true ]; then
    exit 1
fi
