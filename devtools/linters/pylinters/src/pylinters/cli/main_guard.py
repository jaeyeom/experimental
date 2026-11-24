"""The ``py-main-guard`` command."""

from __future__ import annotations

import argparse
import dataclasses
import os
import sys
from collections.abc import Sequence
from typing import BinaryIO, NoReturn, TextIO

from pylinters import main_guard
from pylinters.core import baseline, cli, entries, errors, files

PROG = "py-main-guard"


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
        paths = cli.selected_paths(
            fs,
            args.paths,
            (stdin or sys.stdin.buffer) if args.stdin0 else None,
        )
        read = files.root_reader(fs)
        entry_list = None
        if args.entry_list is not None:
            with open(args.entry_list, encoding="utf-8") as handle:
                entry_list = entries.parse_entry_list(
                    handle.read(), args.entry_list, fs
                )
        # Discovery only needs each file's first bytes, for a ``#!``.
        found = entries.discover(
            fs,
            cfg.entries,
            files.root_reader(fs, limit=2),
            entry_list=entry_list,
        )
        bl_path = os.path.join(
            os.getcwd(),
            args.baseline
            or os.path.join(fs.root, ".baselines", f"{PROG}.json"),
        )
        bl = baseline.load(bl_path, PROG)
        result = main_guard.check(fs, cfg, found, read, paths=paths)
        # A tracked file deleted only from the work tree no longer exists
        # for the baseline, so its entry is reported stale.
        existing = [
            p
            for p in fs.py_files()
            if main_guard.in_scope(cfg, p)
            and os.path.isfile(os.path.join(fs.root, p))
        ]
        stale: list[str] = []
        failures: list[str] = []
        if args.update_baseline:
            new = baseline.updated(bl, result.counts, existing)
        elif args.tighten_baseline:
            new, failures = baseline.tightened(bl, result.counts, existing)
        else:
            ev = baseline.evaluate(bl, result.counts, existing, scanned=paths)
            failures = ev.failures
            stale = ev.stale if args.strict_baseline == "true" else []
        if args.update_baseline or args.tighten_baseline:
            bl = new
            baseline.save(bl_path, bl)
            print(f"baseline written to {_display(bl_path)}", file=out)
    except (errors.PylintersError, OSError) as exc:
        print(f"{PROG}: error: {exc}", file=err)
        return cli.EXIT_ERROR
    if not failures and not stale:
        print(_pass_line(result), file=out)
        for note in result.discovery.notes:
            print(note, file=out)
        return cli.EXIT_OK
    for v in result.violations:
        if result.counts[v.path] > baseline.allowed(bl, v.path):
            print(v, file=out)
    # Without baseline entries every limit is 0; the violations say it all.
    for line in failures if bl.entries else []:
        print(f"baseline exceeded: {line}", file=out)
    for line in stale:
        print(f"stale baseline: {line}", file=out)
    if failures:
        print(
            "\nAdd the guard, propagate the runner's exit code, or mark a "
            f"genuine exception with `# {cfg.allow_directive}`. To record "
            "existing violations as debt, rerun the whole tree with "
            "--update-baseline.",
            file=out,
        )
    else:
        print(
            "\nRemove the stale entries by rerunning the whole tree with "
            "--tighten-baseline.",
            file=out,
        )
    return cli.EXIT_VIOLATIONS


def run() -> NoReturn:
    """Console-script entry point."""
    sys.exit(main())


def _pass_line(result: main_guard.Result) -> str:
    disc = result.discovery
    sources = ", ".join(f"{name} {n}" for name, n in disc.counts.items())
    debt = sum(result.counts.values())
    tail = f", {debt} baselined violation(s)" if debt else ""
    return (
        f"PASSED: {PROG} ({len(disc.entries)} entries: {sources}; "
        f"{result.files_scanned} file(s) scanned{tail})."
    )


def _display(path: str) -> str:
    """Return ``path`` relative to the cwd when it lies under it."""
    rel = os.path.relpath(path)
    outside = rel == os.pardir or rel.startswith(os.pardir + os.sep)
    return path if outside else rel


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog=PROG,
        description="Require the __main__ guard on files run directly, and "
        "that guard test runners propagate their exit code.",
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
        "--entry-source",
        action="append",
        choices=entries.SOURCES,
        help="entry source (repeatable; replaces config)",
    )
    parser.add_argument(
        "--script-glob",
        action="append",
        metavar="GLOB",
        help="glob of script entries (repeatable; replaces config)",
    )
    parser.add_argument(
        "--test-glob",
        action="append",
        metavar="GLOB",
        help="glob of test entries (repeatable; replaces config)",
    )
    parser.add_argument(
        "--entry-list",
        metavar="FILE",
        help="file of 'script PATH' / 'test PATH' lines, paths relative to "
        "the repo top-level",
    )
    parser.add_argument(
        "--require-test-runner",
        action="store_true",
        default=None,
        help="test entries must run a test runner in their guard",
    )
    parser.add_argument(
        "--bazel-dir", metavar="DIR", help="Bazel workspace directory"
    )
    parser.add_argument(
        "--bazel", metavar="PATH", help="bazel binary (default: bazel)"
    )
    parser.add_argument(
        "--exclude",
        action="append",
        metavar="GLOB",
        help=cli.EXCLUDE_HELP,
    )
    parser.add_argument("--allow-directive", metavar="TEXT")
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


def _config(root: str, args: argparse.Namespace) -> main_guard.Config:
    cfg = main_guard.load_config(os.path.join(root, "pyproject.toml"))
    ent: dict[str, object] = {}
    if args.entry_source is not None:
        ent["sources"] = tuple(args.entry_source)
    if args.script_glob is not None:
        ent["script_globs"] = tuple(args.script_glob)
    if args.test_glob is not None:
        ent["test_globs"] = tuple(args.test_glob)
    if args.bazel_dir is not None:
        ent["bazel_dir"] = args.bazel_dir
    if args.bazel is not None:
        ent["bazel"] = args.bazel
    changes: dict[str, object] = {
        "entries": dataclasses.replace(cfg.entries, **ent)  # type: ignore[arg-type]
    }
    if args.exclude is not None:
        changes["exclude"] = cfg.exclude + tuple(args.exclude)
    if args.require_test_runner is not None:
        changes["require_test_runner"] = True
    if args.allow_directive is not None:
        changes["allow_directive"] = args.allow_directive
    return dataclasses.replace(cfg, **changes)  # type: ignore[arg-type]


if __name__ == "__main__":
    run()
