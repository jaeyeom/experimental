"""Tests for pylinters.module_import."""

import sys
from collections.abc import Sequence

import pytest
from pylinters import module_import
from pylinters.core import files, imports

Config = module_import.Config


def viol(
    srcs: dict[str, str],
    path: str,
    cfg: Config | None = None,
    roots: Sequence[str] = (),
) -> list[str]:
    """Render the violations of ``path`` in an in-memory tree."""
    fs = files.FileSet(srcs)
    return [
        str(v)
        for v in module_import.find_violations(
            fs,
            imports.parse(srcs[path].encode(), path),
            roots,
            cfg or Config(),
        )
    ]


def test_symbol_import_flagged() -> None:
    """Importing a symbol out of a module is flagged."""
    srcs = {
        "app/__init__.py": "",
        "app/types.py": "",
        "main.py": "from app.types import CheckResult\n",
    }
    assert viol(srcs, "main.py") == [
        (
            "main.py:1:23: from app.types import CheckResult: "
            "import the module instead (from app import types)"
        )
    ]


def test_module_import_ok() -> None:
    """Importing the module itself is fine."""
    srcs = {
        "app/__init__.py": "",
        "app/types.py": "",
        "main.py": "from app import types\n",
    }
    assert viol(srcs, "main.py") == []


def test_third_party_and_stdlib_skipped() -> None:
    """Unresolvable modules are not checked."""
    srcs = {"main.py": "from requests import get\nfrom os.path import join\n"}
    assert viol(srcs, "main.py") == []


def test_relative_symbol_flagged() -> None:
    """A relative symbol import suggests ``from . import m``."""
    srcs = {"p/__init__.py": "", "p/m.py": "", "p/q.py": "from .m import X\n"}
    assert viol(srcs, "p/q.py") == [
        (
            "p/q.py:1:16: from .m import X: "
            "import the module instead (from . import m)"
        )
    ]


def test_relative_parent_suggestion() -> None:
    """A multi-level relative module splits at its last dot."""
    srcs = {
        "p/__init__.py": "",
        "p/pkg/__init__.py": "",
        "p/pkg/m.py": "",
        "p/a/__init__.py": "",
        "p/a/q.py": "from ..pkg.m import X\n",
    }
    assert viol(srcs, "p/a/q.py") == [
        (
            "p/a/q.py:1:21: from ..pkg.m import X: "
            "import the module instead (from ..pkg import m)"
        )
    ]


def test_from_dot_import_module_ok() -> None:
    """``from . import m`` is the preferred form."""
    srcs = {"p/__init__.py": "", "p/m.py": "", "p/q.py": "from . import m\n"}
    assert viol(srcs, "p/q.py") == []


def test_own_source_root() -> None:
    """Absolute imports resolve against the file's own source root."""
    srcs = {
        "tools/dash/api/__init__.py": "",
        "tools/dash/api/views.py": "",
        "tools/dash/api/urls.py": "from api.views import index\n",
    }
    assert len(viol(srcs, "tools/dash/api/urls.py")) == 1


def test_extra_root() -> None:
    """Configured roots are searched."""
    srcs = {
        "src/lib/__init__.py": "",
        "src/lib/util.py": "",
        "app.py": "from lib.util import f\n",
    }
    assert len(viol(srcs, "app.py", roots=("src",))) == 1
    assert viol(srcs, "app.py") == []


def test_stdlib_guard_on_extra_root() -> None:
    """A stdlib name never resolves against a configured root."""
    srcs = {
        "tools/subprocess.py": "",
        "a/x.py": "from subprocess import run\n",
    }
    assert viol(srcs, "a/x.py", roots=("tools",)) == []


def test_third_party_guard() -> None:
    """Configured third-party names never resolve against a root."""
    srcs = {"tools/requests.py": "", "a/x.py": "from requests import get\n"}
    assert len(viol(srcs, "a/x.py", roots=("tools",))) == 1
    cfg = Config(third_party=("requests",))
    assert viol(srcs, "a/x.py", cfg, roots=("tools",)) == []


def test_guard_spares_own_root_and_repo_root() -> None:
    """The guard applies only to configured roots."""
    srcs = {"subprocess.py": "", "x.py": "from subprocess import run\n"}
    assert len(viol(srcs, "x.py", roots=("subprocess",))) == 1


def test_init_reexport_allowed() -> None:
    """An ``__init__`` may re-export from its own package."""
    srcs = {
        "p/__init__.py": "from .health import health\nfrom p.health import S\n",
        "p/health.py": "",
    }
    assert viol(srcs, "p/__init__.py") == []


def test_init_import_from_sibling_package_flagged() -> None:
    """The re-export allowance does not reach outside the package."""
    srcs = {
        "p/__init__.py": "",
        "p/a/__init__.py": "from ..b import X\n",
        "p/b.py": "",
    }
    assert len(viol(srcs, "p/a/__init__.py")) == 1


def test_submodule_name_ok() -> None:
    """Importing a subpackage by name is a module import."""
    srcs = {
        "p/__init__.py": "",
        "p/sub/__init__.py": "",
        "x.py": "from p import sub\n",
    }
    assert viol(srcs, "x.py") == []


def test_generated_pb2_ok() -> None:
    """Names matching generated globs are treated as modules."""
    srcs = {
        "proto/__init__.py": "",
        "x.py": "from proto import foo_pb2, foo_pb2_grpc\n",
    }
    assert viol(srcs, "x.py") == []


def test_star_skipped() -> None:
    """Star imports are not checked."""
    srcs = {"p/__init__.py": "", "p/m.py": "", "x.py": "from p.m import *\n"}
    assert viol(srcs, "x.py") == []


def test_per_name_and_statement_directives() -> None:
    """A name-line directive exempts that name; the paren line, all."""
    base = {"p/__init__.py": "", "p/m.py": ""}
    per_name = (
        "from p.m import (\n    a,  # py-module-import: allow\n    b,\n)\n"
    )
    out = viol({**base, "x.py": per_name}, "x.py")
    assert len(out) == 1
    assert out[0].startswith("x.py:3:5: from p.m import b:")
    whole = "from p.m import (\n    a,\n    b,\n)  # py-module-import: allow\n"
    assert viol({**base, "x.py": whole}, "x.py") == []


def test_directive_in_string_does_not_exempt() -> None:
    """Directive text in a string literal is not a comment."""
    srcs = {
        "p/__init__.py": "",
        "p/m.py": "",
        "x.py": 'from p.m import X; s = "# py-module-import: allow"\n',
    }
    assert len(viol(srcs, "x.py")) == 1


def test_import_inside_function_checked() -> None:
    """Nested imports are checked too."""
    srcs = {
        "p/__init__.py": "",
        "p/m.py": "",
        "x.py": "def f():\n    from p.m import X\n",
    }
    out = viol(srcs, "x.py")
    assert len(out) == 1
    assert out[0].startswith("x.py:2:")


def test_single_component_suggestion() -> None:
    """A top-level module is suggested as a plain import."""
    srcs = {"util.py": "", "x.py": "from util import f\n"}
    assert viol(srcs, "x.py") == [
        (
            "x.py:1:18: from util import f: "
            "import the module instead (import util)"
        )
    ]


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
