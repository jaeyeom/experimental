"""Bazel test running Bazel-managed Semgrep with pinned offline rules."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from ruamel.yaml import YAML

_RULES_SUBDIRS = (
    "go/lang/security",
    "python/lang/security",
    "yaml/github-actions/security",
)

# Noisy audit rules in semgrep-rules that are not part of Semgrep's p/default.
_EXCLUDED_RULE_IDS = frozenset(
    {
        "go.lang.security.shared-url-struct-mutation.shared-url-struct-mutation",
        (
            "python.lang.security.audit.dangerous-subprocess-use-audit"
            ".dangerous-subprocess-use-audit"
        ),
    }
)


def _env_binary(var: str) -> Path:
    """Return a Bazel-managed binary path from an environment variable."""
    raw = os.environ.get(var)
    assert raw, f"{var} environment variable must be set by Bazel"
    path = Path(raw).resolve()
    assert path.is_file(), f"binary from {var} not found at {path}"
    return path


def _semgrep_rules_root() -> Path:
    """Return the root directory of the pinned @semgrep_rules archive."""
    raw = os.environ.get("SEMGREP_RULES_ANCHOR")
    assert raw, "SEMGREP_RULES_ANCHOR must be set by Bazel"
    path = Path(raw).resolve()
    assert path.is_file(), f"semgrep rules anchor not found at {path}"
    return path.parent


def _configured_rule_subdirs() -> tuple[str, ...]:
    """Return the @semgrep_rules subdirectories configured for this test."""
    raw = os.environ.get("SEMGREP_RULE_SUBDIRS", "").strip()
    if not raw:
        return _RULES_SUBDIRS
    return tuple(part.strip() for part in raw.split(",") if part.strip())


def _stage_custom_config(tmp_path: Path) -> Path:
    """Copy declared custom Semgrep rule files into a non-symlink directory."""
    raw = os.environ.get("SEMGREP_CONFIG_FILES", "").strip()
    assert raw, (
        "SEMGREP_CONFIG_FILES environment variable must be set by Bazel"
    )
    cfg_dir = tmp_path / "custom_config"
    cfg_dir.mkdir(parents=True, exist_ok=True)
    for idx, entry in enumerate(raw.split()):
        src = Path(entry).resolve()
        assert src.is_file(), f"Semgrep config file not found at {src}"
        shutil.copyfile(src, cfg_dir / f"{idx:03d}_{src.name}")
    return cfg_dir


def _stage_manifest_files(manifest_path: Path, stage_dir: Path) -> list[str]:
    """Copy manifest-listed runfiles into stage_dir as regular files.

    Semgrep skips symbolic links during directory traversal and rejects symlink
    targets, while Bazel stages runfiles as symlinks. Copying into stage_dir
    preserves repo-relative paths without reading outside declared inputs.
    """
    raw_lines = manifest_path.read_text(encoding="utf-8").splitlines()
    rel_paths = [line.strip() for line in raw_lines if line.strip()]
    assert rel_paths, f"manifest {manifest_path} contained no files"
    for rel in rel_paths:
        src = Path(rel).resolve()
        assert src.is_file(), f"declared Semgrep input not found at {rel}"
        dest = stage_dir / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dest)
    return rel_paths


def _build_pinned_rules(out_path: Path) -> Path:
    """Combine pinned @semgrep_rules YAML files with registry-style rule IDs.

    Semgrep's registry prefixes rule IDs with ``<relpath_with_dots>.<id>``
    (matching existing ``nosemgrep`` annotations in the repository). Loading
    individual local files directly uses a different prefix, so we normalize
    each rule ID into a single combined ruleset file.
    """
    yaml = YAML()
    rules_root = _semgrep_rules_root()
    rules: list[object] = []

    for subdir in _configured_rule_subdirs():
        base = rules_root / subdir
        assert base.is_dir(), f"expected rules subdir at {base}"
        for rule_file in sorted(base.rglob("*.yaml")):
            data = yaml.load(rule_file.read_text(encoding="utf-8"))
            if not isinstance(data, dict) or "rules" not in data:
                continue
            rel = rule_file.relative_to(rules_root).with_suffix("")
            prefix = ".".join(rel.parts)
            for rule in data["rules"]:
                if not isinstance(rule, dict) or "id" not in rule:
                    continue
                rule_id = f"{prefix}.{rule['id']}"
                if rule_id in _EXCLUDED_RULE_IDS:
                    continue
                rule["id"] = rule_id
                rules.append(rule)

    assert rules, "no rules loaded from @semgrep_rules"
    with out_path.open("w", encoding="utf-8") as fh:
        yaml.dump({"rules": rules}, fh)
    return out_path


def _semgrep_env(tmp_path: Path) -> dict[str, str]:
    """Build an isolated environment without host semgrep on PATH."""
    home_dir = tmp_path / "home"
    home_dir.mkdir(exist_ok=True)
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir(exist_ok=True)

    # Bazel's py_binary stub uses `#!/usr/bin/env python3`; point it to the
    # hermetic Python interpreter rather than anything on host PATH. Also
    # expose Bazel-managed pysemgrep so osemgrep's execvp fallback stays
    # hermetic, and keep /usr/bin:/bin for standard POSIX utilities (git,
    # uname) while excluding user/Homebrew directories.
    python_link = bin_dir / "python3"
    if not python_link.exists():
        python_link.symlink_to(sys.executable)
    pysemgrep_link = bin_dir / "pysemgrep"
    if not pysemgrep_link.exists():
        pysemgrep_link.symlink_to(_env_binary("PYSEMGREP_BIN"))

    env = os.environ.copy()
    env["HOME"] = str(home_dir)
    env["PATH"] = f"{bin_dir}:/usr/bin:/bin"
    env["SEMGREP_SEND_METRICS"] = "off"
    env["SEMGREP_ENABLE_VERSION_CHECK"] = "0"
    return env


def _run_semgrep(
    target_dir: Path,
    configs: list[Path],
    tmp_path: Path,
    *,
    no_git_ignore: bool = False,
    json_output: bool = False,
) -> subprocess.CompletedProcess[str]:
    """Run Bazel-managed semgrep without requiring host semgrep on PATH."""
    cmd = [
        str(_env_binary("SEMGREP_BIN")),
        "scan",
        "--metrics=off",
        "--error",
    ]
    if no_git_ignore:
        cmd.append("--no-git-ignore")
    if json_output:
        cmd.append("--json")
    for cfg in configs:
        cmd.extend(["--config", str(cfg)])
    cmd.append(str(target_dir))
    return subprocess.run(
        cmd,
        cwd=target_dir,
        env=_semgrep_env(tmp_path),
        capture_output=True,
        text=True,
        check=False,
    )


@pytest.fixture(scope="module")
def pinned_rules_file(tmp_path_factory: pytest.TempPathFactory) -> Path:
    """Build the normalized pinned ruleset once for all tests in the module."""
    out = tmp_path_factory.mktemp("semgrep_rules") / "pinned-rules.yaml"
    return _build_pinned_rules(out)


def test_repo_passes_semgrep(pinned_rules_file: Path, tmp_path: Path) -> None:
    """Declared repository inputs pass offline Semgrep scan."""
    manifest_env = os.environ.get("SEMGREP_MANIFEST", "").strip()
    if not manifest_env:
        pytest.skip("SEMGREP_MANIFEST not set (run via bazel test)")
        return
    manifest_path = Path(manifest_env).resolve()
    assert manifest_path.is_file(), f"manifest not found at {manifest_path}"

    stage_dir = tmp_path / "repo"
    stage_dir.mkdir()
    _stage_manifest_files(manifest_path, stage_dir)

    configs: list[Path] = [pinned_rules_file]
    if os.environ.get("SEMGREP_USE_CUSTOM_CONFIG") == "1":
        configs.append(_stage_custom_config(tmp_path))

    result = _run_semgrep(
        stage_dir,
        configs,
        tmp_path,
        no_git_ignore=True,
        json_output=True,
    )
    assert result.returncode == 0, (
        f"semgrep found issues:\n{result.stdout}{result.stderr}"
    )
    report = json.loads(result.stdout)
    scanned = report.get("paths", {}).get("scanned", [])
    assert scanned, f"semgrep scanned 0 files for manifest {manifest_env}"


def test_custom_semgrep_rule_detects_violation(tmp_path: Path) -> None:
    """Custom .semgrep/py3-logging-format.yaml catches f-string log calls."""
    sample_dir = tmp_path / "sample_py"
    sample_dir.mkdir()
    (sample_dir / "bad_log.py").write_text(
        'import logging\nlogger = logging.getLogger(__name__)\n'
        'x = 1\nlogger.info(f"value: {x}")\n',
        encoding="utf-8",
    )
    custom_config = _stage_custom_config(tmp_path)
    result = _run_semgrep(
        sample_dir,
        [custom_config],
        tmp_path,
        no_git_ignore=True,
    )
    output = result.stdout + result.stderr
    assert result.returncode != 0, (
        f"expected custom semgrep rule to fail:\n{output}"
    )
    assert "py3-logging-improper-formatting" in output, output


def test_pinned_go_security_rule_detects_violation(
    pinned_rules_file: Path, tmp_path: Path
) -> None:
    """Pinned Go security rules fail on dangerous-exec-command."""
    sample_dir = tmp_path / "sample_go"
    sample_dir.mkdir()
    (sample_dir / "bad.go").write_text(
        'package main\n\nimport "os/exec"\n\n'
        'func run(cmd string) {\n\t_ = exec.Command(cmd)\n}\n',
        encoding="utf-8",
    )
    result = _run_semgrep(
        sample_dir,
        [pinned_rules_file],
        tmp_path,
        no_git_ignore=True,
    )
    output = result.stdout + result.stderr
    assert result.returncode != 0, (
        f"expected pinned Go security rule to fail:\n{output}"
    )
    assert "dangerous-exec-command" in output, output


def test_nosemgrep_suppression_matches_registry_rule_id(
    pinned_rules_file: Path, tmp_path: Path
) -> None:
    """Existing registry-format nosemgrep annotations suppress findings."""
    sample_dir = tmp_path / "sample_go_suppressed"
    sample_dir.mkdir()
    (sample_dir / "ok.go").write_text(
        'package main\n\nimport "os/exec"\n\n'
        "func run(cmd string) {\n"
        "\t// nosemgrep: go.lang.security.audit.dangerous-exec-command"
        ".dangerous-exec-command\n"
        "\t_ = exec.Command(cmd)\n"
        "}\n",
        encoding="utf-8",
    )
    result = _run_semgrep(
        sample_dir,
        [pinned_rules_file],
        tmp_path,
        no_git_ignore=True,
    )
    assert result.returncode == 0, (
        f"expected nosemgrep suppression to work:\n"
        f"{result.stdout}{result.stderr}"
    )


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
