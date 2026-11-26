"""Unit tests for go_version_compare."""

from __future__ import annotations

import json
import sys
from typing import TYPE_CHECKING

import go_version_compare as gvc
import pytest

if TYPE_CHECKING:
    from _pytest.capture import CaptureFixture


@pytest.mark.parametrize(
    (
        "installed_mod",
        "target_mod",
        "installed",
        "target",
        "inst_go",
        "tool_go",
    ),
    [
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "v9.9.9",
            "v0.1.0",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "v0.1.0",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "v0.1.0+dirty",
            "v0.1.0",
            "go1.26.0",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "not-a-semver",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic-factory",
            "github.com/jaeyeom/experimental",
            "(devel)",
            "",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/agentic",
            "github.com/jaeyeom/agentic-factory",
            "v1.0.0",
            "v1.0.0",
            "go1.27.4",
            "go1.27.4",
        ),
        (
            "github.com/jaeyeom/experimental/devtools/factory",
            "github.com/jaeyeom/experimental",
            "v1.0.0",
            "v1.0.0",
            "go1.27.4",
            "go1.27.4",
        ),
    ],
    ids=[
        "higher_old_version",
        "equal_version",
        "dirty_old_build",
        "empty_target",
        "invalid_target",
        "devel_with_empty_target",
        "prefix_without_slash_boundary",
        "submodule_to_root_module",
    ],
)
def test_module_path_mismatch_always_installs(
    installed_mod: str,
    target_mod: str,
    installed: str,
    target: str,
    inst_go: str,
    tool_go: str,
) -> None:
    """Any module-path mismatch triggers install and names both modules."""
    got = gvc.decide_upgrade(
        installed,
        target,
        installed_go=inst_go,
        toolchain_go=tool_go,
        installed_module=installed_mod,
        target_module=target_mod,
    )
    assert got["action"] == "install"
    assert installed_mod in got["reason"]
    assert target_mod in got["reason"]


@pytest.mark.parametrize(
    (
        "installed_mod",
        "target_mod",
        "installed",
        "target",
        "inst_go",
        "tool_go",
        "want_action",
    ),
    [
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.2.0",
            "v0.1.0",
            "go1.27.4",
            "go1.27.4",
            "skip_older",
        ),
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "v0.2.0",
            "go1.27.4",
            "go1.27.4",
            "install",
        ),
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "v0.1.0",
            "go1.27.4",
            "go1.27.4",
            "skip_current",
        ),
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "v0.1.0",
            "go1.26.0",
            "go1.27.4",
            "install",
        ),
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.1.0+dirty",
            "v0.1.0",
            "go1.26.0",
            "go1.27.4",
            "skip_older",
        ),
        (
            "github.com/jaeyeom/experimental",
            "github.com/jaeyeom/experimental",
            "v0.1.0",
            "",
            "go1.27.4",
            "go1.27.4",
            "skip_unresolved",
        ),
        (
            "golang.org/x/tools",
            "golang.org/x/tools/cmd/goimports",
            "v0.39.0",
            "v0.39.0",
            "go1.27.4",
            "go1.27.4",
            "skip_current",
        ),
        (
            "golang.org/x/tools",
            "golang.org/x/tools/cmd/goimports",
            "v0.38.0",
            "v0.39.0",
            "go1.27.4",
            "go1.27.4",
            "install",
        ),
        (
            "github.com/bazelbuild/buildtools",
            "github.com/bazelbuild/buildtools/buildozer",
            "v0.0.0-20260101000000-abcdef123456",
            "v0.0.0-20250101000000-111111111111",
            "go1.27.4",
            "go1.27.4",
            "skip_older",
        ),
    ],
    ids=[
        "exact_match_older_target",
        "exact_match_newer_target",
        "exact_match_equal",
        "exact_match_toolchain_changed",
        "exact_match_dirty_build",
        "exact_match_unresolved_target",
        "subpackage_match_current",
        "subpackage_match_upgrade",
        "subpackage_match_older_target",
    ],
)
def test_matching_module_preserves_semver_and_toolchain_behavior(
    installed_mod: str,
    target_mod: str,
    installed: str,
    target: str,
    inst_go: str,
    tool_go: str,
    want_action: str,
) -> None:
    """Matching modules (exact or ancestor) preserve version/toolchain logic."""
    got = gvc.decide_upgrade(
        installed,
        target,
        installed_go=inst_go,
        toolchain_go=tool_go,
        installed_module=installed_mod,
        target_module=target_mod,
    )
    assert got["action"] == want_action


def test_not_installed_ignores_target_module() -> None:
    """Missing binary always returns not installed."""
    got = gvc.decide_upgrade(
        "",
        "",
        installed_module="",
        target_module="github.com/jaeyeom/experimental",
    )
    assert got == {"action": "install", "reason": "not installed"}


def test_cli_backwards_compatible_positional_args(
    capsys: CaptureFixture[str],
) -> None:
    """CLI supports 2, 3, or 4 positional args without module flags."""
    rc = gvc.main(["v0.1.0", "v0.1.0"])
    assert rc == 0
    assert json.loads(capsys.readouterr().out)["action"] == "skip_current"

    rc = gvc.main(["v0.1.0", "v0.1.0", "go1.26.0"])
    assert rc == 0
    assert json.loads(capsys.readouterr().out)["action"] == "skip_current"

    rc = gvc.main(["v0.1.0", "v0.1.0", "go1.26.0", "go1.27.4"])
    assert rc == 0
    assert json.loads(capsys.readouterr().out)["action"] == "install"


def test_cli_module_flags_detect_mismatch(
    capsys: CaptureFixture[str],
) -> None:
    """CLI --installed-module and --target-module detect module migration."""
    rc = gvc.main(
        [
            "v0.2.0",
            "",
            "go1.27.4",
            "go1.27.4",
            "--installed-module",
            "github.com/jaeyeom/agentic-factory",
            "--target-module",
            "github.com/jaeyeom/experimental",
        ]
    )
    assert rc == 0
    data = json.loads(capsys.readouterr().out)
    assert data["action"] == "install"
    assert "github.com/jaeyeom/agentic-factory" in data["reason"]
    assert "github.com/jaeyeom/experimental" in data["reason"]


def test_cli_rejects_too_few_args() -> None:
    """CLI rejects missing required positional args."""
    with pytest.raises(SystemExit) as exc:
        gvc.main(["v0.1.0"])
    assert exc.value.code == 2


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
