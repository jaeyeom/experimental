"""Require the ``__main__`` guard on files run directly.

Three rules, each about a silent failure:

- ``missing-guard``: an entry module (see :mod:`pylinters.core.entries`)
  has no module-level ``if __name__ == '__main__':``. Its work runs on
  import, or running it does nothing and exits 0.
- ``dropped-exit``: a guard calls ``pytest.main(...)`` or
  ``unittest.main(exit=False)`` without passing the result to
  ``sys.exit`` / ``SystemExit``, so failing tests exit 0.
- ``missing-test-runner`` (opt-in): a test entry's guard runs no test
  runner, so running the file runs no tests.
"""

from __future__ import annotations

import ast
from collections.abc import Callable, Collection, Iterator, Mapping
from dataclasses import dataclass, field

from pylinters.core import (
    config,
    entries,
    errors,
    files,
    globs,
    guards,
    imports,
    names,
)

CHECKER = "main-guard"
DEFAULT_DIRECTIVE = "py-main-guard: allow"

Reader = Callable[[str], bytes]

_KEYS = (
    "require-test-runner",
    "exclude",
    "allow-directive",
)


@dataclass(frozen=True)
class Config:
    """Settings for one run; ``exclude`` is shared plus checker excludes."""

    entries: entries.EntryConfig = field(default_factory=entries.EntryConfig)
    exclude: tuple[str, ...] = ()
    generated: tuple[str, ...] = config.DEFAULT_GENERATED
    require_test_runner: bool = False
    allow_directive: str = DEFAULT_DIRECTIVE


@dataclass(frozen=True)
class Violation:
    """One finding."""

    path: str
    line: int
    rule: str
    message: str

    def __str__(self) -> str:
        """Render as ``path:line: rule: message``."""
        return f"{self.path}:{self.line}: {self.rule}: {self.message}"


@dataclass(frozen=True)
class Result:
    """The outcome of a check."""

    violations: list[Violation]
    counts: dict[str, int]
    files_scanned: int
    discovery: entries.Discovery


def load_config(pyproject_path: str) -> Config:
    """Read the shared, ``entries``, and ``main-guard`` settings.

    Raises:
        ConfigError: Invalid TOML, a moved or unknown key, or a wrong value
            type.
    """
    shared, table = config.load_table(pyproject_path, (CHECKER,))
    own = table.get(CHECKER, {})
    where = f"{pyproject_path} [tool.pylinters.{CHECKER}]"
    for key in own:
        if key in entries.CONFIG_KEYS:
            raise errors.ConfigError(
                f"{where}: {key} moved to [tool.pylinters.entries]"
            )
    config.check_keys(own, _KEYS, where)
    require = own.get("require-test-runner", False)
    if not isinstance(require, bool):
        raise errors.ConfigError(
            f"{where}: require-test-runner must be a boolean"
        )
    return Config(
        entries=entries.load_config(pyproject_path),
        exclude=shared.exclude
        + config.string_list(own.get("exclude", []), "exclude", where),
        generated=shared.generated,
        require_test_runner=require,
        allow_directive=_string(
            own, "allow-directive", DEFAULT_DIRECTIVE, where
        ),
    )


def in_scope(cfg: Config, path: str) -> bool:
    """Return whether ``path`` is subject to the check."""
    return not (
        globs.match_any(cfg.exclude, path)
        or globs.match_any(cfg.generated, path)
    )


def check(
    fs: files.FileSet,
    cfg: Config,
    found: entries.Discovery,
    read: Reader | None = None,
    paths: Collection[str] | None = None,
) -> Result:
    """Check every in-scope file, reporting only ``paths`` when given.

    Raises:
        ParseError: A scanned file is not valid Python.
    """
    reader = read if read is not None else files.root_reader(fs)
    violations: list[Violation] = []
    counts: dict[str, int] = {}
    for path in fs.py_files():
        if (paths is not None and path not in paths) or not in_scope(
            cfg, path
        ):
            continue
        try:
            data = reader(path)
        except FileNotFoundError:
            continue  # tracked but gone from the work tree
        tree, comments = imports.parse_module(data, path)
        got = find_violations(
            tree, comments, path, found.entries.get(path), cfg
        )
        counts[path] = len(got)
        violations.extend(got)
    violations.sort(key=lambda v: (v.path, v.line))
    return Result(violations, counts, len(counts), found)


def find_violations(
    tree: ast.Module,
    comments: Mapping[int, str],
    path: str,
    entry: entries.Entry | None,
    cfg: Config,
) -> list[Violation]:
    """Return the findings for one parsed file."""
    found_guards = guards.module_guards(tree)
    bound = names.Names(tree)
    out: list[Violation] = []
    runners = 0
    for call, stmt, ok in _runner_calls(found_guards, bound):
        runners += 1
        if ok or guards.comment_in(
            comments,
            stmt.lineno,
            stmt.end_lineno or stmt.lineno,
            cfg.allow_directive,
        ):
            continue
        spelled = _spelled(call.func)
        fix = (
            "use `unittest.main()`"
            if _is_unittest_main(call.func, bound)
            else f"use `sys.exit({spelled}(...))`"
        )
        out.append(
            Violation(
                path,
                call.lineno,
                "dropped-exit",
                f"`{spelled}(...)` result is discarded, so failures exit 0; {fix}",
            )
        )
    if entry is None or _file_exempt(tree, comments, cfg.allow_directive):
        return out
    reasons = ", ".join(entry.reasons)
    if not found_guards:
        out.insert(
            0,
            Violation(
                path,
                1,
                "missing-guard",
                f"run directly ({reasons}) but has no "
                "`if __name__ == '__main__':` guard",
            ),
        )
    elif cfg.require_test_runner and entry.kind == "test" and not runners:
        out.insert(
            0,
            Violation(
                path,
                found_guards[0].lineno,
                "missing-test-runner",
                f"run as a test ({reasons}) but its `__main__` guard runs "
                "no test runner",
            ),
        )
    return out


def _is_unittest_main(func: ast.expr, bound: names.Names) -> bool:
    return bound.qualified(func) == "unittest.main"


def _is_exit(func: ast.expr, bound: names.Names) -> bool:
    # Unbound ``exit`` / ``quit`` are the builtins, which raise
    # SystemExit with their argument.
    return bound.qualified(func) in (
        "sys.exit",
        "SystemExit",
        "exit",
        "quit",
    )


_SELF_EXITING = frozenset({"absl.testing.absltest.main", "absltest.main"})


def _runner_calls(
    found_guards: list[ast.If], bound: names.Names
) -> Iterator[tuple[ast.Call, ast.stmt, bool]]:
    """Yield ``(call, enclosing statement, propagated-or-harmless)``."""
    for guard in found_guards:
        for top in guard.body:
            for stmt in _statements(top):
                for node in _own_nodes(stmt):
                    if not isinstance(node, ast.Call):
                        continue
                    qualified = bound.qualified(node.func)
                    if qualified == "pytest.main":
                        yield node, stmt, _propagated(node, stmt, bound)
                    elif qualified in _SELF_EXITING:
                        # Exits with the result by itself.
                        yield node, stmt, True
                    elif qualified == "unittest.main":
                        harmless = not _exit_false(node)
                        yield (
                            node,
                            stmt,
                            harmless or _propagated(node, stmt, bound),
                        )


def _statements(stmt: ast.stmt) -> Iterator[ast.stmt]:
    """Yield ``stmt`` and every statement nested in it."""
    yield stmt
    for child in ast.iter_child_nodes(stmt):
        if isinstance(child, ast.stmt):
            yield from _statements(child)
        elif isinstance(child, ast.excepthandler | ast.match_case):
            for grandchild in ast.iter_child_nodes(child):
                if isinstance(grandchild, ast.stmt):
                    yield from _statements(grandchild)


def _own_nodes(stmt: ast.stmt) -> Iterator[ast.AST]:
    """Yield the expression nodes of ``stmt``, not of nested statements."""
    stack = [
        c
        for c in ast.iter_child_nodes(stmt)
        if not isinstance(c, ast.stmt | ast.excepthandler | ast.match_case)
    ]
    while stack:
        node = stack.pop()
        yield node
        stack.extend(ast.iter_child_nodes(node))


def _propagated(call: ast.Call, stmt: ast.stmt, bound: names.Names) -> bool:
    """Report whether ``call`` is the sole argument of an exit call.

    ``return call`` (in a function defined in the guard) hands the result
    to the caller; what the caller does with it is not followed.
    """
    if isinstance(stmt, ast.Return):
        return stmt.value is call
    if isinstance(stmt, ast.Raise):
        exc = stmt.exc
    elif isinstance(stmt, ast.Expr):
        exc = stmt.value
    else:
        return False
    return (
        isinstance(exc, ast.Call)
        and _is_exit(exc.func, bound)
        and len(exc.args) == 1
        and not exc.keywords
        and exc.args[0] is call
    )


def _exit_false(call: ast.Call) -> bool:
    return any(
        k.arg == "exit"
        and isinstance(k.value, ast.Constant)
        and k.value.value is False
        for k in call.keywords
    )


def _spelled(func: ast.expr) -> str:
    if isinstance(func, ast.Name):
        return func.id
    if isinstance(func, ast.Attribute):
        return f"{_spelled(func.value)}.{func.attr}"
    return "?"


def _file_exempt(
    tree: ast.Module, comments: Mapping[int, str], directive: str
) -> bool:
    """Report whether the directive comes before the first statement.

    A module docstring may sit above it, like a shebang.
    """
    body = list(tree.body)
    if (
        body
        and isinstance(body[0], ast.Expr)
        and isinstance(body[0].value, ast.Constant)
        and isinstance(body[0].value.value, str)
    ):
        body = body[1:]
    last = body[0].lineno - 1 if body else max(comments, default=0)
    return guards.comment_in(comments, 1, last, directive)


def _string(
    own: Mapping[str, object], key: str, default: str, where: str
) -> str:
    value = own.get(key, default)
    if not isinstance(value, str) or not value:
        raise errors.ConfigError(f"{where}: {key} must be a non-empty string")
    return value
