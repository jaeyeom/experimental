#!/usr/bin/env python3
"""Pre-trust and authorize an agent roles directory across AI coding CLIs."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import sys
import time
from pathlib import Path
from typing import Any


def _atomic_write(path: Path, content: str, default_mode: int = 0o644) -> None:
    """Write content atomically, preserving existing file mode if present."""
    path.parent.mkdir(parents=True, exist_ok=True)
    mode = (path.stat().st_mode & 0o777) if path.exists() else default_mode
    tmp = path.with_name(f".{path.name}.tmp.{os.getpid()}")
    try:
        fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            f.write(content)
        os.chmod(tmp, mode)
        os.replace(tmp, path)
    except Exception:
        tmp.unlink(missing_ok=True)
        raise


def _load_json(path: Path) -> dict[str, Any]:
    """Load a JSON object from path, or return {} if missing/empty."""
    if not path.exists():
        return {}
    raw = path.read_text(encoding="utf-8").strip()
    if not raw:
        return {}
    data = json.loads(raw)
    if not isinstance(data, dict):
        raise TypeError(f"{path} must contain a JSON object")
    return data


def _append_unique(items: list[Any], value: str) -> bool:
    """Append value to items if not already present. Returns True if modified."""
    if value in items:
        return False
    items.append(value)
    return True


def configure_claude(home: Path, roles_dir: str) -> bool:
    """Configure Claude Code trust (~/.claude.json) and permissions (~/.claude/settings.json)."""
    changed = False

    claude_json_path = home / ".claude.json"
    claude_json = _load_json(claude_json_path)
    projects = claude_json.get("projects")
    if not isinstance(projects, dict):
        projects = {}
        claude_json["projects"] = projects
        changed = True
    entry = projects.get(roles_dir)
    if not isinstance(entry, dict):
        entry = {}
        projects[roles_dir] = entry
        changed = True
    if entry.get("hasTrustDialogAccepted") is not True:
        entry["hasTrustDialogAccepted"] = True
        changed = True
    if changed:
        _atomic_write(
            claude_json_path,
            json.dumps(claude_json, indent=2) + "\n",
            default_mode=0o600,
        )

    settings_changed = False
    settings_path = home / ".claude" / "settings.json"
    settings = _load_json(settings_path)

    permissions = settings.get("permissions")
    if not isinstance(permissions, dict):
        permissions = {}
        settings["permissions"] = permissions
        settings_changed = True

    add_dirs = permissions.get("additionalDirectories")
    if not isinstance(add_dirs, list):
        add_dirs = []
        permissions["additionalDirectories"] = add_dirs
        settings_changed = True
    if _append_unique(add_dirs, roles_dir):
        settings_changed = True

    allow = permissions.get("allow")
    if not isinstance(allow, list):
        allow = []
        permissions["allow"] = allow
        settings_changed = True
    for rule in (f"Read(/{roles_dir}/**)", f"Edit(/{roles_dir}/**)"):
        if _append_unique(allow, rule):
            settings_changed = True

    sandbox = settings.get("sandbox")
    if isinstance(sandbox, dict) and sandbox.get("enabled") is not False:
        fs = sandbox.get("filesystem")
        if not isinstance(fs, dict):
            fs = {}
            sandbox["filesystem"] = fs
            settings_changed = True
        allow_write = fs.get("allowWrite")
        if not isinstance(allow_write, list):
            allow_write = []
            fs["allowWrite"] = allow_write
            settings_changed = True
        if _append_unique(allow_write, roles_dir):
            settings_changed = True

    if settings_changed:
        _atomic_write(
            settings_path,
            json.dumps(settings, indent=2) + "\n",
            default_mode=0o644,
        )

    return changed or settings_changed


def configure_agy(home: Path, roles_dir: str) -> bool:
    """Configure Antigravity CLI (agy) trustedWorkspaces and permissions."""
    settings_path = home / ".gemini" / "antigravity-cli" / "settings.json"
    settings = _load_json(settings_path)
    changed = False

    trusted = settings.get("trustedWorkspaces")
    if not isinstance(trusted, list):
        trusted = []
        settings["trustedWorkspaces"] = trusted
        changed = True
    if _append_unique(trusted, roles_dir):
        changed = True

    permissions = settings.get("permissions")
    if not isinstance(permissions, dict):
        permissions = {}
        settings["permissions"] = permissions
        changed = True
    allow = permissions.get("allow")
    if not isinstance(allow, list):
        allow = []
        permissions["allow"] = allow
        changed = True
    for rule in (f"read_file({roles_dir})", f"write_file({roles_dir})"):
        if _append_unique(allow, rule):
            changed = True

    if changed:
        _atomic_write(
            settings_path,
            json.dumps(settings, indent=2) + "\n",
            default_mode=0o600,
        )
    return changed


def _find_toml_section(
    lines: list[str], header: str
) -> tuple[int, int] | None:
    """Return (header_idx, end_idx) for a TOML table header, or None."""
    for i, line in enumerate(lines):
        if line.strip() == header:
            end = len(lines)
            for j in range(i + 1, len(lines)):
                stripped = lines[j].strip()
                if stripped.startswith("[") and stripped.endswith("]"):
                    end = j
                    break
            return i, end
    return None


def _upsert_toml_kv(
    text: str,
    header: str,
    key: str,
    value_literal: str,
) -> tuple[str, bool]:
    """Set key = value_literal under [header] in TOML text, preserving other content."""
    lines = text.splitlines()
    assignment = f"{key} = {value_literal}"
    section = _find_toml_section(lines, header)
    if section is None:
        if lines and lines[-1].strip():
            lines.append("")
        lines.append(header)
        lines.append(assignment)
        return "\n".join(lines) + "\n", True

    start, end = section
    for j in range(start + 1, end):
        stripped = lines[j].strip()
        if not stripped or stripped.startswith("#") or "=" not in stripped:
            continue
        k = stripped.split("=", 1)[0].strip()
        if k == key:
            if stripped == assignment:
                return text if text.endswith("\n") else text + "\n", False
            lines[j] = assignment
            return "\n".join(lines) + "\n", True

    insert_at = end
    while insert_at > start + 1 and not lines[insert_at - 1].strip():
        insert_at -= 1
    lines.insert(insert_at, assignment)
    return "\n".join(lines) + "\n", True


def _parse_toml_string_list(raw: str) -> list[str]:
    """Parse a TOML string array literal into a Python list of strings."""
    cleaned = re.sub(r",(\s*\])", r"\1", raw.strip())
    parsed = json.loads(cleaned)
    if not isinstance(parsed, list):
        raise TypeError("Expected TOML array")
    return [str(x) for x in parsed]


def _upsert_toml_list_item(
    text: str,
    header: str,
    key: str,
    item: str,
) -> tuple[str, bool]:
    """Ensure item is in the TOML string list `key = [...]` under [header]."""
    lines = text.splitlines()
    section = _find_toml_section(lines, header)
    if section is None:
        return _upsert_toml_kv(text, header, key, json.dumps([item]))

    start, end = section
    j = start + 1
    while j < end:
        stripped = lines[j].strip()
        if stripped and not stripped.startswith("#") and "=" in stripped:
            k, rhs = stripped.split("=", 1)
            if k.strip() == key:
                rhs_stripped = rhs.strip()
                k_end = j
                buf = rhs_stripped
                while buf.count("[") > buf.count("]") and k_end + 1 < end:
                    k_end += 1
                    line_no_comment = lines[k_end].split("#", 1)[0].strip()
                    buf += " " + line_no_comment
                items = _parse_toml_string_list(buf)
                if item in items:
                    return text if text.endswith("\n") else text + "\n", False
                items.append(item)
                lines[j : k_end + 1] = [f"{key} = {json.dumps(items)}"]
                return "\n".join(lines) + "\n", True
        j += 1

    insert_at = end
    while insert_at > start + 1 and not lines[insert_at - 1].strip():
        insert_at -= 1
    lines.insert(insert_at, f"{key} = {json.dumps([item])}")
    return "\n".join(lines) + "\n", True


def configure_codex(codex_home: Path, roles_dir: str) -> bool:
    """Configure Codex CLI project trust and sandbox writable_roots."""
    config_path = codex_home / "config.toml"
    original = (
        config_path.read_text(encoding="utf-8") if config_path.exists() else ""
    )
    escaped = json.dumps(roles_dir)
    text, changed_trust = _upsert_toml_kv(
        original,
        f"[projects.{escaped}]",
        "trust_level",
        '"trusted"',
    )
    text, changed_roots = _upsert_toml_list_item(
        text,
        "[sandbox_workspace_write]",
        "writable_roots",
        roles_dir,
    )
    if changed_trust or changed_roots:
        _atomic_write(config_path, text, default_mode=0o600)
        return True
    return False


def configure_grok(grok_home: Path, roles_dir: str) -> bool:
    """Configure Grok Build trusted_folders.toml."""
    tf_path = grok_home / "trusted_folders.toml"
    original = tf_path.read_text(encoding="utf-8") if tf_path.exists() else ""
    escaped = json.dumps(roles_dir)
    header = f"[folders.{escaped}]"
    text, changed = _upsert_toml_kv(original, header, "trusted", "true")
    if changed:
        text, _ = _upsert_toml_kv(
            text, header, "decided_at", str(int(time.time()))
        )
        _atomic_write(tf_path, text, default_mode=0o600)
        return True
    return False


def _validate_roles_dir(raw_path: str, home: Path) -> str:
    """Resolve and validate the target directory so trust is never broadened."""
    if not raw_path or not raw_path.strip():
        raise ValueError("roles directory path must not be empty")
    resolved = os.path.abspath(os.path.expanduser(raw_path.strip()))
    home_abs = os.path.abspath(str(home))
    if resolved in ("/", home_abs):
        raise ValueError(
            f"refusing to trust overly broad directory: {resolved}"
        )
    return resolved


def main(argv: list[str] | None = None) -> int:
    """CLI entry point for configuring AI coding CLI trust."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--roles-dir",
        required=True,
        help="Path to the agent roles directory to trust",
    )
    parser.add_argument(
        "--force-tools",
        default="",
        help="Comma-separated list of tools to configure without PATH lookup (for testing)",
    )
    args = parser.parse_args(argv)

    home = Path(os.environ.get("HOME", str(Path.home())))
    roles_dir = _validate_roles_dir(args.roles_dir, home)

    forced = {t.strip() for t in args.force_tools.split(",") if t.strip()}

    def is_installed(cmd: str) -> bool:
        return cmd in forced or shutil.which(cmd) is not None

    any_changed = False
    statuses: list[str] = []

    if is_installed("claude"):
        if configure_claude(home, roles_dir):
            any_changed = True
            statuses.append("claude:changed")
        else:
            statuses.append("claude:unchanged")
    else:
        statuses.append("claude:skipped")

    if is_installed("codex"):
        codex_home = Path(os.environ.get("CODEX_HOME") or (home / ".codex"))
        if configure_codex(codex_home, roles_dir):
            any_changed = True
            statuses.append("codex:changed")
        else:
            statuses.append("codex:unchanged")
    else:
        statuses.append("codex:skipped")

    if is_installed("agy"):
        if configure_agy(home, roles_dir):
            any_changed = True
            statuses.append("agy:changed")
        else:
            statuses.append("agy:unchanged")
    else:
        statuses.append("agy:skipped")

    if is_installed("grok"):
        grok_home = Path(os.environ.get("GROK_HOME") or (home / ".grok"))
        if configure_grok(grok_home, roles_dir):
            any_changed = True
            statuses.append("grok:changed")
        else:
            statuses.append("grok:unchanged")
    else:
        statuses.append("grok:skipped")

    print(" ".join(statuses))
    if any_changed:
        print("AGENT_ROLES_TRUST_CHANGED")
    else:
        print("AGENT_ROLES_TRUST_UNCHANGED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
