"""The ``py-sibling-import`` command."""

from __future__ import annotations

import argparse
import dataclasses
import os
import sys
from collections.abc import Sequence
from typing import BinaryIO, NoReturn, TextIO

from pylinters import sibling_import
from pylinters.core import cli, config, errors, files

PROG = "py-sibling-import"


def main(
    argv: Sequence[str] | None = None,
    *,
    stdin: BinaryIO | None = None,
    stdout: TextIO | None = None,
    stderr: TextIO | None = None,
) -> int:
    """Run the checker and return the exit code."""
    out = stdout or sys.stdout
    err = stderr or sys.stderr
    args = _parser().parse_args(argv)
    try:
        fs = files.GitResolver.from_repo(args.root or os.getcwd())
        cfg = _config(fs.root, args)
        paths = cli.selected_paths(
            fs,
            args.paths,
            (stdin or sys.stdin.buffer) if args.stdin0 else None,
        )
        result = sibling_import.check(fs, cfg, paths=paths)
    except (errors.PylintersError, OSError) as exc:
        print(f"{PROG}: error: {exc}", file=err)
        return cli.EXIT_ERROR
    if result.violations:
        out.write(sibling_import.format_report(result, cfg))
        return cli.EXIT_VIOLATIONS
    print(
        f"PASSED: no bare sibling import ({result.files_scanned} file(s) "
        f"scanned, {len(result.package_dirs)} package director(ies) "
        "detected).",
        file=out,
    )
    return cli.EXIT_OK


def run() -> NoReturn:
    """Console-script entry point."""
    sys.exit(main())


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog=PROG,
        description="Flag bare imports of sibling modules inside packages.",
    )
    parser.add_argument(
        "paths",
        nargs="*",
        metavar="PATH",
        help="only report these files (directories expand to their .py files)",
    )
    parser.add_argument("--root", help="any directory inside the repo")
    parser.add_argument(
        "--stdin0",
        action="store_true",
        help="also read NUL-separated top-level-relative paths from stdin",
    )
    parser.add_argument(
        "--root-dir",
        action="append",
        metavar="DIR",
        help="import root (repeatable; replaces config roots)",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        metavar="GLOB",
        help="checker exclude glob (repeatable; replaces the checker "
        "excludes; shared excludes still apply)",
    )
    parser.add_argument(
        "--generated",
        action="append",
        metavar="GLOB",
        help="generated-file glob (repeatable; replaces config)",
    )
    parser.add_argument("--allow-directive", metavar="TEXT")
    parser.add_argument(
        "--version", action="version", version=f"%(prog)s {cli.version()}"
    )
    return parser


def _config(root: str, args: argparse.Namespace) -> sibling_import.Config:
    pyproject = os.path.join(root, "pyproject.toml")
    cfg = sibling_import.load_config(pyproject)
    changes: dict[str, object] = {}
    if args.root_dir is not None:
        changes["roots"] = tuple(args.root_dir)
    if args.exclude is not None:
        shared, _ = config.load_table(pyproject, (sibling_import.CHECKER,))
        changes["exclude"] = shared.exclude + tuple(args.exclude)
    if args.generated is not None:
        changes["generated"] = tuple(args.generated)
    if args.allow_directive is not None:
        changes["allow_directive"] = args.allow_directive
    return dataclasses.replace(cfg, **changes)  # type: ignore[arg-type]


if __name__ == "__main__":
    run()
