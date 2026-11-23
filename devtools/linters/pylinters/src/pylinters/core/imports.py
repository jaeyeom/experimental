"""Import statements and comments extracted from Python source."""

from __future__ import annotations

import ast
import io
import tokenize
from collections.abc import Mapping
from dataclasses import dataclass

from pylinters.core import errors


@dataclass(frozen=True)
class ImportStmt:
    """One ``import`` or ``from ... import`` statement."""

    line: int
    end_line: int
    level: int
    module: str | None
    names: tuple[str, ...]
    is_from: bool


@dataclass(frozen=True)
class Source:
    """The imports and comments of one parsed file."""

    path: str
    imports: tuple[ImportStmt, ...]
    comments: Mapping[int, str]

    def has_directive(self, stmt: ImportStmt, directive: str) -> bool:
        """Report whether a comment on any line of ``stmt`` has directive."""
        return any(
            directive in self.comments.get(line, "")
            for line in range(stmt.line, stmt.end_line + 1)
        )


def parse(data: bytes, path: str) -> Source:
    """Parse ``data`` (raw file bytes) and collect its imports and comments.

    Raises:
        ParseError: The source is not valid Python.
    """
    try:
        tree = ast.parse(data, filename=path)
        comments = {
            tok.start[0]: tok.string
            for tok in tokenize.tokenize(io.BytesIO(data).readline)
            if tok.type == tokenize.COMMENT
        }
    except (SyntaxError, ValueError, tokenize.TokenError) as err:
        raise errors.ParseError(path, str(err)) from err
    stmts = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            stmts.append(_stmt(node, 0, None, False))
        elif isinstance(node, ast.ImportFrom):
            stmts.append(_stmt(node, node.level, node.module, True))
    stmts.sort(key=lambda s: (s.line, s.end_line))
    return Source(path, tuple(stmts), comments)


def _stmt(
    node: ast.Import | ast.ImportFrom,
    level: int,
    module: str | None,
    is_from: bool,
) -> ImportStmt:
    return ImportStmt(
        line=node.lineno,
        end_line=node.end_lineno or node.lineno,
        level=level,
        module=module,
        names=tuple(alias.name for alias in node.names),
        is_from=is_from,
    )
