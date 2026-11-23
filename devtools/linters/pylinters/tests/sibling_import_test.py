"""Tests for pylinters.sibling_import."""

import sys
from collections.abc import Collection
from pathlib import Path

import pytest
from pylinters import sibling_import
from pylinters.core import config, errors, files

Config = sibling_import.Config


def run(
    srcs: dict[str, str],
    cfg: Config | None = None,
    paths: Collection[str] | None = None,
) -> sibling_import.Result:
    """Check an in-memory tree of ``path -> source``."""
    return sibling_import.check(
        files.FileSet(srcs),
        cfg or Config(),
        read=lambda p: srcs[p].encode(),
        paths=paths,
    )


def names(result: sibling_import.Result) -> list[tuple[str, int, str]]:
    """Summarize violations as (path, line, name)."""
    return [(v.path, v.line, v.name) for v in result.violations]


def test_namespace_package_detected_by_evidence() -> None:
    """A qualified import elsewhere makes a dir without __init__ a package."""
    result = run(
        {
            "tools/rr/policy.py": "",
            "tools/rr/policy_test.py": "import policy\n",
            "tools/rr/main.py": "from tools.rr import policy\n",
        }
    )
    assert [str(v) for v in result.violations] == [
        (
            "tools/rr/policy_test.py:1: import policy: bare import of "
            "sibling module; use `from tools.rr import policy`"
        )
    ]
    assert result.package_dirs["tools/rr"] == "tools.rr"
    assert result.files_scanned == 3


def test_flat_script_dir_not_flagged() -> None:
    """Without __init__ or qualified evidence, bare imports are correct."""
    result = run({"scripts/s.py": "", "scripts/s_test.py": "import s\n"})
    assert result.violations == []
    assert result.package_dirs == {}


def test_init_package_uses_own_source_root() -> None:
    """An __init__-only package is named from its own source root."""
    result = run(
        {
            "codelab/proto/gen/__init__.py": "",
            "codelab/proto/gen/a.py": "",
            "codelab/proto/gen/b.py": "import a\n",
        }
    )
    assert [v.suggestion for v in result.violations] == ["from gen import a"]


@pytest.mark.parametrize(
    ("stmt", "pkg"),
    [
        ("import x.y.z", "x/y"),
        ("from x.y import z", "x/y"),
        ("from x import y", "x"),
    ],
)
def test_evidence_forms(stmt: str, pkg: str) -> None:
    """Each qualified-import form marks the directory it reaches into."""
    result = run({"x/y/z.py": "", "main.py": stmt + "\n"})
    assert pkg in result.package_dirs


def test_evidence_from_excluded_and_generated_files() -> None:
    """Skipped files still provide qualified-import evidence."""
    srcs = {
        "p/a.py": "",
        "p/b.py": "import a\n",
        "skip/e.py": "from p import a\n",
    }
    assert names(run(srcs, Config(exclude=("skip/**",)))) == [
        ("p/b.py", 1, "a")
    ]
    srcs = {"p/a.py": "", "p/b.py": "import a\n", "e_pb2.py": "import p.a\n"}
    assert names(run(srcs)) == [("p/b.py", 1, "a")]


def test_nested_imports_flagged() -> None:
    """Function-local, try and TYPE_CHECKING imports are flagged."""
    result = run(
        {
            "p/__init__.py": "",
            "p/a.py": "",
            "p/b.py": "def f():\n    import a\n"
            "try:\n    import a\nexcept ImportError:\n    pass\n"
            "if TYPE_CHECKING:\n    import a\n",
        }
    )
    assert [v.line for v in result.violations] == [2, 4, 8]


def test_top_level_name_forms() -> None:
    """The top-level name decides; one violation per offending alias."""
    result = run(
        {
            "p/__init__.py": "",
            "p/policy.py": "",
            "p/policy/sub.py": "",
            "p/b.py": "import policy.sub\n"
            "from policy import x\n"
            "import os, policy\n"
            "import policy as p\n",
        }
    )
    assert [(v.line, v.statement, v.name) for v in result.violations] == [
        (1, "import policy.sub", "policy"),
        (2, "from policy import ...", "policy"),
        (3, "import policy", "policy"),
        (4, "import policy", "policy"),
    ]


def test_sibling_subpackage_flagged() -> None:
    """A sibling directory with Python below it is a sibling too."""
    result = run(
        {"p/__init__.py": "", "p/sub/m.py": "", "p/b.py": "import sub\n"}
    )
    assert names(result) == [("p/b.py", 1, "sub")]


def test_relative_not_flagged() -> None:
    """Relative imports are the other correct form."""
    result = run(
        {"p/__init__.py": "", "p/a.py": "", "p/b.py": "from . import a\n"}
    )
    assert result.violations == []


def test_stdlib_named_sibling_flagged() -> None:
    """A sibling that shadows a stdlib name is still flagged."""
    result = run(
        {"p/__init__.py": "", "p/json.py": "", "p/b.py": "import json\n"}
    )
    assert names(result) == [("p/b.py", 1, "json")]


def test_init_is_not_a_sibling() -> None:
    """``import __init__`` is not a sibling import."""
    result = run({"p/__init__.py": "", "p/b.py": "import __init__\n"})
    assert result.violations == []


def test_directive_on_close_paren_line() -> None:
    """The directive may sit on any line of the statement."""
    result = run(
        {
            "p/__init__.py": "",
            "p/a.py": "",
            "p/b.py": "from a import (\n    x,\n)  # py-sibling-import: allow\n",
        }
    )
    assert result.violations == []


def test_custom_directive() -> None:
    """A configured directive replaces the default."""
    srcs = {
        "p/__init__.py": "",
        "p/a.py": "",
        "p/b.py": "import a  # ok-bare\n",
    }
    assert run(srcs, Config(allow_directive="ok-bare")).violations == []
    assert len(run(srcs).violations) == 1


def test_generated_skipped_by_default() -> None:
    """Generated protoc output is not scanned unless re-enabled."""
    srcs = {
        "gen/__init__.py": "",
        "gen/a_pb2.py": "",
        "gen/a_pb2_grpc.py": "import a_pb2\n",
    }
    assert run(srcs).violations == []
    assert names(run(srcs, Config(generated=()))) == [
        ("gen/a_pb2_grpc.py", 1, "a_pb2")
    ]


def test_src_default_root() -> None:
    """src/ is a default root, so suggestions are relative to it."""
    result = run(
        {
            "src/p/m.py": "",
            "src/p/n.py": "import m\n",
            "src/p/o.py": "from p import m\n",
        }
    )
    assert [v.suggestion for v in result.violations] == ["from p import m"]


def test_non_identifier_dir_not_package() -> None:
    """A directory with no valid dotted name is never a package dir."""
    result = run(
        {
            "my-tool/__init__.py": "",
            "my-tool/a.py": "",
            "my-tool/b.py": "import a\n",
        }
    )
    assert result.violations == []
    assert result.package_dirs == {}


def test_import_of_own_package_name_not_flagged() -> None:
    """``pkg/pkg.py`` beside ``from pkg import x``: the import is qualified."""
    result = run(
        {
            "src/pkg/__init__.py": "",
            "src/pkg/pkg.py": "",
            "src/pkg/other.py": "from pkg import pkg\nimport pkg.pkg\n",
        }
    )
    assert result.violations == []


def test_self_named_import_not_flagged() -> None:
    """A shim importing its own name means the external module."""
    result = run(
        {
            "p/__init__.py": "",
            "p/message_filters.py": "import message_filters\n",
        }
    )
    assert result.violations == []


def test_init_name_preferred_over_evidence() -> None:
    """The __init__ chain name beats a longer repo-root path."""
    result = run(
        {
            "a/src/p/__init__.py": "",
            "a/src/p/m.py": "",
            "a/src/p/n.py": "import m\n",
            "x.py": "from a.src.p import m\n",
        }
    )
    assert [v.suggestion for v in result.violations] == ["from p import m"]


def test_parse_error_in_scanned_file_raises() -> None:
    """A scanned file that does not parse is an error."""
    with pytest.raises(errors.ParseError) as exc:
        run({"a.py": "import (\n"})
    assert exc.value.path == "a.py"


def test_parse_error_in_excluded_file_ignored() -> None:
    """A skipped file that does not parse only loses its evidence."""
    result = run(
        {"testdata/bad.py": "import (\n", "a.py": ""},
        Config(exclude=("testdata/**",)),
    )
    assert result.violations == []
    assert result.files_scanned == 1


def test_paths_restricts_reporting_not_detection() -> None:
    """Evidence comes from the whole tree; reports only from paths."""
    srcs = {
        "p/a.py": "",
        "p/b.py": "import a\n",
        "p/c.py": "import a\n",
        "main.py": "from p import a\n",
    }
    assert names(run(srcs, paths=["p/b.py"])) == [("p/b.py", 1, "a")]


def test_empty_configured_root_raises() -> None:
    """A configured root with no Python is a configuration error."""
    with pytest.raises(errors.ConfigError):
        run({"a.py": ""}, Config(roots=("lib",)))


def test_original_motivating_case() -> None:
    """The pennybot case: namespace package, function-local bare import."""
    pkg = "tools/productivity/review_router"
    result = run(
        {
            f"{pkg}/policy.py": "",
            f"{pkg}/ping.py": "",
            f"{pkg}/cli.py": f"from {pkg.replace('/', '.')} import policy\n",
            f"{pkg}/policy_test.py": "def test_x(monkeypatch):\n"
            "    import policy\n",
        }
    )
    assert [v.suggestion for v in result.violations] == [
        "from tools.productivity.review_router import policy"
    ]


def test_format_report() -> None:
    """The report lists violations and names the allow directive."""
    cfg = Config()
    result = run({"p/__init__.py": "", "p/a.py": "", "p/b.py": "import a\n"})
    report = sibling_import.format_report(result, cfg)
    assert str(result.violations[0]) in report
    assert "py-sibling-import: allow" in report
    assert sibling_import.format_report(run({"a.py": ""}), cfg) == ""


def test_load_config_merges_excludes(tmp_path: Path) -> None:
    """Shared and checker-specific excludes are merged."""
    path = tmp_path / "pyproject.toml"
    path.write_text(
        '[tool.pylinters]\nexclude = ["a/**"]\n'
        '[tool.pylinters.sibling-import]\nexclude = ["b/**"]\n'
        'allow-directive = "ok"\n'
    )
    cfg = sibling_import.load_config(str(path))
    assert cfg.exclude == ("a/**", "b/**")
    assert cfg.allow_directive == "ok"
    assert cfg.generated == config.DEFAULT_GENERATED


def test_load_config_rejects_unknown_checker_key(tmp_path: Path) -> None:
    """An unknown key in the checker subtable is an error."""
    path = tmp_path / "pyproject.toml"
    path.write_text("[tool.pylinters.sibling-import]\nroots = []\n")
    with pytest.raises(errors.ConfigError, match="roots"):
        sibling_import.load_config(str(path))


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
