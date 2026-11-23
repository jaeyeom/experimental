"""Tests for pylinters.core.config."""

import sys
from pathlib import Path

import pytest
from pylinters.core import config, errors


def _write(tmp_path: Path, text: str) -> str:
    path = tmp_path / "pyproject.toml"
    path.write_text(text)
    return str(path)


def test_defaults_when_no_pyproject(tmp_path: Path) -> None:
    """A missing pyproject.toml yields defaults."""
    shared, table = config.load_table(str(tmp_path / "pyproject.toml"), ())
    assert shared == config.SharedConfig(None, (), config.DEFAULT_GENERATED)
    assert table == {}


def test_defaults_when_no_table(tmp_path: Path) -> None:
    """A pyproject.toml without [tool.pylinters] yields defaults."""
    shared, table = config.load_table(
        _write(tmp_path, '[project]\nname = "x"\n'), ()
    )
    assert shared == config.SharedConfig(None, (), config.DEFAULT_GENERATED)
    assert table == {}


def test_reads_shared_and_subtable(tmp_path: Path) -> None:
    """Shared keys and checker subtables are read."""
    path = _write(
        tmp_path,
        '[tool.pylinters]\nroots = ["lib"]\nexclude = ["x/**"]\n'
        "generated = []\n[tool.pylinters.sibling-import]\n"
        'allow-directive = "ok"\n',
    )
    shared, table = config.load_table(path, ("sibling-import",))
    assert shared == config.SharedConfig(("lib",), ("x/**",), ())
    assert table["sibling-import"] == {"allow-directive": "ok"}


def test_unknown_shared_key(tmp_path: Path) -> None:
    """A typo in a shared key is an error naming the key."""
    path = _write(tmp_path, '[tool.pylinters]\nroot = ["lib"]\n')
    with pytest.raises(errors.ConfigError, match="root"):
        config.load_table(path, ())


def test_unknown_subtable(tmp_path: Path) -> None:
    """A subtable no checker knows is an error."""
    path = _write(tmp_path, "[tool.pylinters.nope]\n")
    with pytest.raises(errors.ConfigError, match="nope"):
        config.load_table(path, ("sibling-import",))


def test_wrong_type(tmp_path: Path) -> None:
    """Shared keys must be lists of strings."""
    path = _write(tmp_path, '[tool.pylinters]\nroots = "lib"\n')
    with pytest.raises(errors.ConfigError, match="roots"):
        config.load_table(path, ())


def test_invalid_toml(tmp_path: Path) -> None:
    """Invalid TOML is a ConfigError."""
    with pytest.raises(errors.ConfigError):
        config.load_table(_write(tmp_path, "[tool\n"), ())


def test_check_keys() -> None:
    """check_keys names where the unknown key was found."""
    config.check_keys({"a": 1}, ["a"], "t")
    with pytest.raises(errors.ConfigError, match=r"t.*\bb\b"):
        config.check_keys({"b": 1}, ["a"], "t")


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
