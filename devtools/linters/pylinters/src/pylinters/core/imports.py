"""Import statements and comments extracted from Python source."""

from __future__ import annotations

import ast
import io
import tokenize
import warnings
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
    name_lines: tuple[int, ...] = ()
    name_cols: tuple[int, ...] = ()


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

    def name_exempt(
        self, stmt: ImportStmt, index: int, directive: str
    ) -> bool:
        """Report whether the comment on name ``index``'s line has directive."""
        return directive in self.comments.get(stmt.name_lines[index], "")

    def statement_exempt(self, stmt: ImportStmt, directive: str) -> bool:
        """Report whether directive exempts the whole statement.

        It must be on the statement's first line, or on a line of the
        statement that is not a name's own line (e.g. the closing paren).
        """
        own = set(stmt.name_lines)
        return any(
            directive in self.comments.get(line, "")
            for line in range(stmt.line, stmt.end_line + 1)
            if line == stmt.line or line not in own
        )


def parse(data: bytes, path: str) -> Source:
    """Parse ``data`` (raw file bytes) and collect its imports and comments.

    Raises:
        ParseError: The source is not valid Python.
    """
    try:
        with warnings.catch_warnings():
            # Warnings about the checked file's own code are not ours.
            warnings.simplefilter("ignore")
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
        name_lines=tuple(alias.lineno for alias in node.names),
        name_cols=tuple(alias.col_offset + 1 for alias in node.names),
    )
