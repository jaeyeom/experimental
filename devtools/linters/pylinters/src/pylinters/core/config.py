"""Configuration read from ``[tool.pylinters]`` in ``pyproject.toml``."""

from __future__ import annotations

import tomllib
from collections.abc import Collection, Mapping
from dataclasses import dataclass
from typing import Any

from pylinters.core import errors

DEFAULT_GENERATED: tuple[str, ...] = ("**/*_pb2.py", "**/*_pb2_grpc.py")

_SHARED_KEYS = ("roots", "exclude", "generated")

# Every checker's ``[tool.pylinters.<name>]`` subtable. Each checker accepts
# all of them, so configuring one checker never breaks another.
CHECKERS: tuple[str, ...] = ("module-import", "sibling-import", "main-guard")


@dataclass(frozen=True)
class SharedConfig:
    """Settings shared by every pylinters checker.

    ``roots`` is ``None`` when unset, meaning the default roots apply.
    """

    roots: tuple[str, ...] | None
    exclude: tuple[str, ...]
    generated: tuple[str, ...]


def load_table(
    pyproject_path: str, known_subtables: Collection[str]
) -> tuple[SharedConfig, Mapping[str, Any]]:
    """Read the shared config and the raw ``[tool.pylinters]`` table.

    A missing file or table yields defaults and an empty table.

    Raises:
        ConfigError: Invalid TOML, an unknown key, or a wrong value type.
    """
    try:
        with open(pyproject_path, "rb") as handle:
            doc = tomllib.load(handle)
    except FileNotFoundError:
        doc = {}
    except (OSError, tomllib.TOMLDecodeError) as err:
        raise errors.ConfigError(f"{pyproject_path}: {err}") from err
    table = doc.get("tool", {}).get("pylinters", {})
    where = f"{pyproject_path} [tool.pylinters]"
    check_keys(table, (*_SHARED_KEYS, *CHECKERS, *known_subtables), where)
    for name in (*CHECKERS, *known_subtables):
        if name in table and not isinstance(table[name], dict):
            raise errors.ConfigError(f"{where}: {name} must be a table")
    roots = table.get("roots")
    shared = SharedConfig(
        roots=None if roots is None else string_list(roots, "roots", where),
        exclude=string_list(table.get("exclude", []), "exclude", where),
        generated=string_list(
            table.get("generated", list(DEFAULT_GENERATED)),
            "generated",
            where,
        ),
    )
    return shared, table


def check_keys(
    table: Mapping[str, Any], allowed: Collection[str], where: str
) -> None:
    """Raise :class:`ConfigError` for a key of ``table`` not in allowed."""
    for key in table:
        if key not in allowed:
            raise errors.ConfigError(f"{where}: unknown key {key!r}")


def string_list(value: Any, key: str, where: str) -> tuple[str, ...]:
    """Return ``value`` as a tuple of strings, or raise ConfigError."""
    if not isinstance(value, list) or not all(
        isinstance(v, str) for v in value
    ):
        raise errors.ConfigError(f"{where}: {key} must be a list of strings")
    return tuple(value)
