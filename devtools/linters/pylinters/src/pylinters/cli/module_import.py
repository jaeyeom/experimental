"""The ``py-module-import`` command."""

from __future__ import annotations

import argparse
import dataclasses
import os
import sys
from collections.abc import Collection, Mapping, Sequence
from typing import BinaryIO, NoReturn, TextIO

from pylinters import module_import
from pylinters.core import baseline, cli, errors, files

PROG = "py-module-import"


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
    if (args.update_baseline or args.tighten_baseline) and (
        args.paths or args.stdin0
    ):
        flag = "--update" if args.update_baseline else "--tighten"
        print(
            f"{PROG}: error: {flag}-baseline rewrites the whole-tree baseline; "
            "do not pass paths",
            file=err,
        )
        return cli.EXIT_ERROR
    try:
        fs = files.GitResolver.from_repo(args.root or os.getcwd())
        cfg = _config(fs.root, args)
        paths = None
        if args.paths or args.stdin0:
            paths = set(cli.to_repo_paths(args.paths, os.getcwd(), fs.root))
            if args.stdin0:
                # git diff --name-only prints top-level-relative paths.
                stdin_paths = cli.read_stdin0(stdin or sys.stdin.buffer)
                paths |= set(cli.to_repo_paths(stdin_paths, fs.root, fs.root))
        bl_path = os.path.join(
            os.getcwd(),
            args.baseline
            or os.path.join(fs.root, ".baselines", f"{PROG}.json"),
        )
        bl = baseline.load(bl_path, PROG)
        result = module_import.check(fs, cfg, paths=paths)
        # A tracked file deleted only from the work tree no longer exists
        # for the baseline, so its entry is reported stale.
        existing = [
            p
            for p in fs.py_files()
            if module_import.in_scope(fs, cfg, p)
            and os.path.isfile(os.path.join(fs.root, p))
        ]
        stale: list[str] = []
        if args.update_baseline:
            baseline.save(
                bl_path, baseline.updated(bl, result.counts, existing)
            )
            failures: list[str] = []
        elif args.tighten_baseline:
            new, failures = baseline.tightened(bl, result.counts, existing)
            baseline.save(bl_path, new)
        else:
            ev = baseline.evaluate(bl, result.counts, existing, scanned=paths)
            failures = ev.failures
            stale = ev.stale if args.strict_baseline == "true" else []
    except (errors.PylintersError, OSError) as exc:
        print(f"{PROG}: error: {exc}", file=err)
        return cli.EXIT_ERROR
    if not failures and not stale:
        print(
            "PASSED: no module symbol import "
            f"({result.files_scanned} file(s) scanned).",
            file=out,
        )
        return cli.EXIT_OK
    failing = _failing_paths(bl, result.counts) if failures else set()
    for v in result.violations:
        if v.path in failing:
            print(v, file=out)
    for line in stale:
        print(f"stale baseline: {line}", file=out)
    print(
        "\nImport the module instead, or mark a genuine exception with "
        f"`# {cfg.allow_directive}`. To record existing violations as "
        "debt, rerun the whole tree with --update-baseline.",
        file=out,
    )
    return cli.EXIT_VIOLATIONS


def run() -> NoReturn:
    """Console-script entry point."""
    sys.exit(main())


def _failing_paths(bl: baseline.File, counts: Mapping[str, int]) -> set[str]:
    """Return the paths whose counts exceed what ``bl`` allows."""
    empty: Collection[str] = ()
    return {
        path
        for path, count in counts.items()
        if baseline.evaluate(bl, {path: count}, empty, scanned=empty).failures
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog=PROG,
        description="Flag symbol imports from first-party modules; "
        "import the module instead.",
    )
    parser.add_argument(
        "paths", nargs="*", metavar="PATH", help="only report these files"
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
        help="checker exclude glob (repeatable; added to shared excludes)",
    )
    parser.add_argument(
        "--generated",
        action="append",
        metavar="GLOB",
        help="generated-file glob (repeatable; replaces config)",
    )
    parser.add_argument("--allow-directive", metavar="TEXT")
    parser.add_argument(
        "--exempt-marker",
        action="append",
        metavar="NAME",
        help="file name marking an exempt tree (repeatable; replaces config)",
    )
    parser.add_argument(
        "--third-party",
        action="append",
        metavar="NAME",
        help="top-level third-party module name (repeatable; replaces config)",
    )
    parser.add_argument(
        "--baseline",
        metavar="FILE",
        help=f"baseline file (default: <toplevel>/.baselines/{PROG}.json)",
    )
    rewrite = parser.add_mutually_exclusive_group()
    rewrite.add_argument(
        "--update-baseline",
        action="store_true",
        help="record current violations in the baseline",
    )
    rewrite.add_argument(
        "--tighten-baseline",
        action="store_true",
        help="only remove stale entries and lower limits",
    )
    parser.add_argument(
        "--strict-baseline",
        choices=("true", "false"),
        default="true",
        help="fail on stale baseline entries (default: true)",
    )
    parser.add_argument(
        "--version", action="version", version=f"%(prog)s {cli.version()}"
    )
    return parser


def _config(root: str, args: argparse.Namespace) -> module_import.Config:
    pyproject = os.path.join(root, "pyproject.toml")
    cfg = module_import.load_config(pyproject)
    changes: dict[str, object] = {}
    if args.root_dir is not None:
        changes["roots"] = tuple(args.root_dir)
    if args.exclude is not None:
        changes["exclude"] = cfg.exclude + tuple(args.exclude)
    if args.generated is not None:
        changes["generated"] = tuple(args.generated)
    if args.allow_directive is not None:
        changes["allow_directive"] = args.allow_directive
    if args.exempt_marker is not None:
        changes["exempt_markers"] = tuple(args.exempt_marker)
    if args.third_party is not None:
        changes["third_party"] = tuple(args.third_party)
    return dataclasses.replace(cfg, **changes)  # type: ignore[arg-type]


if __name__ == "__main__":
    run()
