"""Error types shared by pylinters checkers."""


class PylintersError(Exception):
    """Base class for errors that end a run with exit code 2."""


class ConfigError(PylintersError):
    """Invalid configuration, flags, or paths."""


class GitError(PylintersError):
    """Git is unavailable or the path is not inside a work tree."""


class ParseError(PylintersError):
    """A Python source file could not be parsed."""

    def __init__(self, path: str, msg: str) -> None:
        """Record the repo-relative ``path`` that failed to parse."""
        super().__init__(f"{path}: {msg}")
        self.path = path


class SourceError(PylintersError):
    """An entry source (``bazel query``, an entry list) failed."""
