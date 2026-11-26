"""Hermetic Bazel test validating release-please configuration and release workflow."""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from typing import Any

import pytest
from ruamel.yaml import YAML

_SHA_USE_RE = re.compile(r"^[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+@[0-9a-f]{40}$")
_SEMVER_RE = re.compile(r"^\d+\.\d+\.\d+$")


def _load_yaml(path: Path) -> Any:
    yaml = YAML(typ="safe")
    return yaml.load(path.read_text(encoding="utf-8"))


def test_release_please_manifest_valid() -> None:
    """Manifest seeds root package at a valid SemVer version."""
    manifest = json.loads(
        Path(".release-please-manifest.json").read_text(encoding="utf-8")
    )
    assert isinstance(manifest, dict)
    assert "." in manifest
    assert _SEMVER_RE.match(manifest["."]), (
        f"invalid root version: {manifest['.']}"
    )


def test_release_please_config_valid() -> None:
    """Config defines single root Go package with pre-major and changelog rules."""
    config = json.loads(
        Path("release-please-config.json").read_text(encoding="utf-8")
    )
    assert config["release-type"] == "go"
    assert config["bump-minor-pre-major"] is True
    assert config["include-component-in-tag"] is False
    assert config["include-v-in-tag"] is True
    assert config["packages"] == {".": {}}

    sections = {
        entry["type"]: (entry["section"], entry.get("hidden", False))
        for entry in config["changelog-sections"]
    }
    assert sections["feat"] == ("Features", False)
    assert sections["fix"] == ("Bug Fixes", False)
    assert sections["perf"] == ("Performance Improvements", False)
    assert sections["docs"] == ("Documentation", False)
    for hidden_type in ("build", "ci", "chore", "refactor", "style", "test"):
        assert sections[hidden_type][1] is True, (
            f"{hidden_type} should be hidden"
        )


def test_changelog_has_header_and_baseline_release() -> None:
    """CHANGELOG.md begins with top-level heading and v0.1.0 entry."""
    changelog = Path("CHANGELOG.md").read_text(encoding="utf-8")
    assert changelog.startswith("# Changelog\n")
    assert "## [0.1.0](" in changelog


def test_release_workflow_valid() -> None:
    """Release workflow uses SHA-pinned release-please, permissions, and verification."""
    workflow = _load_yaml(Path(".github/workflows/release.yml"))
    assert workflow["permissions"] == {
        "contents": "write",
        "pull-requests": "write",
    }
    assert "main" in workflow["on"]["push"]["branches"]

    steps = workflow["jobs"]["release"]["steps"]
    uses_list = [step["uses"] for step in steps if "uses" in step]
    assert any(
        u.startswith("googleapis/release-please-action@") for u in uses_list
    )
    for uses in uses_list:
        assert _SHA_USE_RE.match(uses), (
            f"action must be pinned to 40-char SHA: {uses}"
        )

    raw = Path(".github/workflows/release.yml").read_text(encoding="utf-8")
    assert "--generate-notes" not in raw
    assert "make install" in raw
    assert "steps.release.outputs.releases_created == 'true'" in raw


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
