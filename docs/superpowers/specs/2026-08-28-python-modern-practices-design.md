# Modern Python Practices Migration

Date: 2026-08-28
Status: approved in conversation; awaiting spec review

## Summary

Replace the split Poetry + pip-tools Python toolchain with uv and
`rules_python` 2.3.2 native `uv.lock` import. Run pytest under Bazel
`py_test`, type-check with mypy from Make, drop unused html_convert
dependencies, stop shipping a Poetry playbook, and modernize the two
handwritten proto demo files (types, logging, injectable collaborators,
pytest).

This is a practices and toolchain migration, not a new product.

## Current state

- Ruff is already configured in `pyproject.toml` (`UP`, `D`, Google
  docstrings, line length 79) and run by `make lint-ruff`.
- Bazel `//tools/format` already formats Python with Ruff.
- Ansible already installs uv, ruff, pylsp, and mypy.
- Packaging is split: Poetry (`pyproject.toml` / `poetry.lock`, Python
  `^3.10`) and pip-tools (`requirements.in` / `requirements.txt`, compiled
  on 3.12). Those locks have drifted (`beautifulsoup4` 4.12.3 vs 4.13.3).
- `MODULE.bazel` uses `rules_python` 1.0.0 and
  `pip.parse(..., requirements_lock = "//:requirements.txt")` at Python
  3.11. Hub name is `@main_pip_deps`. No `py_library` / `py_test` /
  `py_binary` consumes it.
- `html_convert` was deleted; beautifulsoup4, cssutils, soupsieve, and
  requests have no remaining Python importers.
- Handwritten Python is only `codelab/proto/client.py` and `server.py`:
  no annotations, `print()` in library paths, `uuid.uuid4()` not
  injectable, no tests. The gRPC `ValidationInterceptor` treats
  `continuation(...)` as the request, which is not how
  `grpc.ServerInterceptor` works.
- Proto Makefile sets `PYTHONPATH=./gen` while the code does
  `from gen import contacts_pb2`. Those disagree.
- Poetry is an opt-in Ansible package in `packages_data.go`. It is not
  imported by `all.yml` or `setup-python-tools.yml`.

## Goals

1. One lockfile workflow: PEP 621 `pyproject.toml` + `uv.lock`.
2. Bazel tests Python with `py_test` so `make test` (`bazel test //...`)
   runs the suite.
3. `make lint` type-checks first-party Python with mypy (strict).
4. Ansible no longer offers Poetry; uv remains the Python installer.
5. Proto demo follows current Python practices: modern type hints,
   module imports, logging in library code, injectable collaborators,
   pytest with fakes (no monkeypatch of first-party `uuid`).

## Non-goals

- Adding basedpyright, pydantic, or a second type checker.
- Adding pytest as a globally installed Ansible package.
- Standing up a live gRPC server in tests.
- Publishing this repo as a PyPI package.
- Changing Ruff line length (79), quote style (`preserve`), or docstring
  convention (Google).
- Rewriting generated `*_pb2.py` / `*_pb2_grpc.py`.
- Forcing grpcio onto Termux if no wheel exists; mark the target
  incompatible instead of a host pip install.

## Constraints

- Python floor is 3.11, matching the existing Bazel `pip.parse`
  `python_version`.
- Bazel is 8.5.0 (`.bazeliskrc`). `rules_python` 2.3.2 is compatible.
- `make test` stays `bazel test //...`. Do not add a parallel
  `uv run pytest` Make test path.
- Host linters stay host tools (ruff, mypy), matching golangci-lint.
- Follow existing Make + Bazel + Ansible generator conventions.
- Import modules, not types/functions, except `typing` /
  `collections.abc` / `typing_extensions` annotation helpers.
- Wrap exceptions with `raise ... from err`. Do not log and re-raise.

## Architecture

```
pyproject.toml  ── uv lock ──► uv.lock
       │                          │
       │                          ▼
       │                 rules_python 2.3.2
       │                 pip.parse(uv_lock)
       │                          │
       ▼                          ▼
  ruff / mypy (host)        @pypi//... wheels
                                  │
                                  ▼
                    //codelab/proto:py_library
                    //codelab/proto:*_test (pytest)
```

uv is the developer-facing package manager. Bazel consumes `uv.lock`
directly. Host ruff/mypy lint what developers edit. Format stays on the
existing Bazel format target.

## Packaging and Bazel

### pyproject.toml

Replace `[tool.poetry]` with PEP 621:

```toml
[project]
name = "experimental"
version = "0.1.0"
description = "Monorepo"
readme = "README.org"
requires-python = ">=3.11"
authors = [{ name = "Jaehyun Yeom", email = "jae.yeom@gmail.com" }]
dependencies = [
    "grpcio",
    "protobuf",
    "protovalidate",
]

[dependency-groups]
dev = ["pytest"]

[tool.uv]
package = false
```

This repo is not a published package. `package = false` lets uv lock
and sync without a `src/` tree or a build backend.

Do not list beautifulsoup4, cssutils, soupsieve, requests, or poetry
`sys_platform == 'linux-android'` markers for buf/grpcio.

Keep the existing `[tool.ruff]` block (line length 79, `UP`+`D`, Google
docstrings, `quote-style = "preserve"`, `known-first-party = ["gen"]`,
generated-file excludes).

Add:

```toml
[tool.mypy]
python_version = "3.11"
strict = true
exclude = [
    "(^|/)gen/",
    ".*_pb2\\.py$",
    ".*_pb2_grpc\\.py$",
    "^bazel-",
]
```

Add `.python-version` containing `3.11`. Gitignore `.venv/` if it is
not already ignored.

### Lockfiles

- Create `uv.lock` with `uv lock`.
- Delete `poetry.lock`, `requirements.in`, `requirements.txt`.
- Replace `make check-requirements` / `requirements.txt:` pip-compile
  rules with `uv lock --check` so `uv.lock` cannot drift from
  `pyproject.toml`.

### MODULE.bazel

Bump `rules_python` from 1.0.0 to **2.3.2**.

Register a toolchain and import `uv.lock`:

```python
bazel_dep(name = "rules_python", version = "2.3.2")

python = use_extension("@rules_python//python/extensions:python.bzl", "python")
python.toolchain(python_version = "3.11", is_default = True)

pip = use_extension("@rules_python//python/extensions:pip.bzl", "pip")
pip.parse(
    hub_name = "pypi",
    python_version = "3.11",
    uv_lock = "//:uv.lock",
    pyproject_toml = "//:pyproject.toml",
)
use_repo(pip, "pypi")
```

Rename `@main_pip_deps` to `@pypi`. Nothing currently depends on the old
hub.

`rules_python` 2.x defaults to venv-based binaries. Accept that default
on Linux/macOS. If 2.3.2 `uv_lock` import fails in this repo (missing
wheels, source-less workspace member, Termux host), fall back to
`uv export --frozen` into a Bazel requirements lock and keep
`pip.parse(requirements_lock=...)`. That fallback is only for a
demonstrated 2.3.2 failure, not the planned path.

### Proto Bazel targets

Create `codelab/proto/BUILD.bazel`:

- `py_library` `gen` — generated pb2 sources plus empty `gen/__init__.py`.
  `imports = ["."]` so `from gen import contacts_pb2` works.
- `py_library` `server` / `client` — handwritten modules, deps on `:gen`
  and `@pypi//grpcio`, `@pypi//protobuf`, `@pypi//protovalidate`.
- `py_test` `server_test` / `client_test` — pytest files, deps on the
  libraries plus `@pypi//pytest`.
- Optional `py_binary` for the demo entry points.

Gazelle is Go-only in this repo. Write Python BUILD files by hand; do
not enable Python gazelle for this change.

If grpcio has no usable wheel on Termux/Android, set
`target_compatible_with` so `bazel test //...` skips those targets
instead of failing the whole suite.

### Proto Makefile

Change `PYTHONPATH=./gen` to `PYTHONPATH=.` so it matches
`from gen import contacts_pb2`.

## Make, lint, format, type-check

`make format` stays `goimports` + `bazel run //:format`. Ruff format is
already on that Bazel target. Do not add a host `ruff format` target.

`make lint` / `make fix`:

| Target | Command |
|--------|---------|
| `lint-ruff` | `ruff check` |
| `fix-ruff` | `ruff check --fix` |
| `lint-mypy` | `mypy` |

Both `lint` and `fix` depend on `lint-mypy`. mypy has no autofix.

Do not add `# noqa` for the proto rewrite; fix the code. Generated pb2
files remain excluded.

`make test` remains `bazel test --test_summary=terse //...`.

`make check` drops `check-requirements` and gains the uv lock check
described above.

mypy is a host binary from Ansible `setup-python-tools.yml`. Do not add
mypy as a Bazel toolchain for this migration.

## Ansible / setup-dev

Remove this entry from `devtools/setup-dev/ansible/packages_data.go`:

```go
{command: "poetry", debianPkgName: "python3-poetry", termuxPkgName: "python-poetry"},
```

Regenerate with `make generate-ansible` from the repo root. That deletes
`poetry.yml` and its syntax test. Machines that already have Poetry keep
it; new setups will not install it.

Leave `uv.yml`, `ruff.yml`, `mypy.yml`, `pylsp.yml`, and
`setup-python-tools.yml` unchanged. Do not add pytest to Ansible.

## Proto demo code

Keep two handwritten modules: `codelab/proto/server.py` and
`client.py`. Add `server_test.py` and `client_test.py` beside them. Do
not invent extra packages.

### Types and imports

Annotate public functions and methods. Use
`list[contacts_pb2.Contact]`, `MutableMapping[str, bytes]`,
`X | None`. Import modules (`uuid`, `logging`, `grpc`). Annotation
helpers (`Callable`, `MutableMapping`) come from `collections.abc`.

### Database

```python
class Database:
    def __init__(
        self,
        kv_storage: MutableMapping[str, bytes],
        *,
        new_id: Callable[[], str] | None = None,
    ) -> None:
        ...
```

`new_id` defaults to `lambda: str(uuid.uuid4())`. Tests pass a dict and
a fixed `new_id`. Do not `monkeypatch` `uuid`.

Behavior to preserve:

- `list_contacts(query)` returns contacts whose name, email, or phone
  contains `query` (empty query matches all).
- `upsert_contact` assigns `new_id()` when `contact.uuid` is empty;
  otherwise keeps the existing uuid.
- `delete_contact` pops by uuid and returns the decoded contact;
  missing keys raise `KeyError`.

### Logging vs print

Library paths (`ValidationInterceptor`, client request validation) use
`logging.getLogger(__name__)`. `main()` may `print` CLI results.

### Client

`ContactsClient` methods raise `protovalidate.ValidationError` instead
of printing and returning `None`. `main()` catches that and prints.

Inject:

- the stub (or a channel that produces the stub)
- `validate: Callable[[object], None] | None = None`, defaulting to
  `protovalidate.validate`

Invalid input must not call the stub.

### Interceptor

Replace the current `continuation(...)` as-request implementation.

`ValidationInterceptor.intercept_service` wraps the unary handler and
calls an injected `validate` on the request. On
`protovalidate.ValidationError`, call
`context.abort(grpc.StatusCode.INVALID_ARGUMENT, str(err.violations))`
and do not also raise. Non-unary handlers pass through unchanged.

Do not start a real gRPC server in tests. Inject `validate` and a fake
handler/context.

### pytest under Bazel

Each test module ends with:

```python
if __name__ == "__main__":
    import sys

    import pytest

    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
```

so `py_test` actually runs pytest. rules_python can treat an inert
module as a passing test; this shim is required.

## Testing

### Database (`server_test.py`)

- list/filter: parametrize matches on name, email, phone, and empty
  query.
- upsert assigns id when uuid is missing (injected `new_id`).
- upsert keeps an existing id.
- delete returns the stored contact.
- delete of a missing uuid raises `KeyError`.

### Client (`client_test.py`)

- Fake stub records calls.
- Valid request reaches the stub and returns its result.
- Invalid request raises `protovalidate.ValidationError` and the stub
  is not called.

### Interceptor

If the wrapper is extracted or reachable without a server, test that a
failing `validate` aborts with `INVALID_ARGUMENT` and a passing
`validate` delegates to the inner handler. Skip this if it requires a
live server.

No `monkeypatch` of `uuid`, `datetime`, or first-party classes. No
`unittest.mock.MagicMock` for collaborators we own; use fakes.

## Documentation

- `README.org`: mention uv (not pip-compile) if dependency generation
  is documented; keep ruff in the linter list; mention mypy.
- Proto Makefile `PYTHONPATH` as above.
- Do not write a new user-facing Python style guide in this change.

## PR plan

Implement as a Graphite stack if Graphite is available; otherwise a
single branch with three commits that can be stacked later.

### PR 1 — Toolchain

pyproject PEP 621, `uv.lock`, delete Poetry/pip-tools lockfiles, bump
`rules_python` to 2.3.2, register Python 3.11 toolchain, `pip.parse`
on `uv.lock`, rename hub to `@pypi`, Make uv lock check, gitignore
`.venv/`, `.python-version`.

Success: `uv lock --check` passes; `bazel test //...` still passes
(no Python tests yet, but the module loads).

### PR 2 — Ansible

Remove Poetry from `packages_data.go` and regenerate playbooks.

Success: Ansible generator tests pass; `poetry.yml` is gone.

### PR 3 — Proto demo, Bazel tests, mypy

Modernize `client.py` / `server.py`, add tests, `BUILD.bazel`,
`gen/__init__.py`, Makefile `PYTHONPATH`, Termux
`target_compatible_with` if needed. Add `[tool.mypy]` and the
`lint-mypy` Make target in the same PR so strict mypy never runs
against untyped files.

Success: `bazel test //codelab/proto:all` passes; `ruff check` and
`mypy` are clean on the handwritten files; `make lint` runs mypy.

PR 1 is the risk (rules_python 1.0.0 → 2.3.2). If it breaks unrelated
targets, stop and fix before PR 2/3.

## File list

Create:

- `uv.lock`
- `.python-version`
- `docs/superpowers/specs/2026-08-28-python-modern-practices-design.md`
  (this file)
- `codelab/proto/BUILD.bazel`
- `codelab/proto/server_test.py`
- `codelab/proto/client_test.py`
- `codelab/proto/gen/__init__.py`

Modify:

- `pyproject.toml`
- `MODULE.bazel` / `MODULE.bazel.lock`
- `Makefile`
- `.gitignore`
- `README.org`
- `codelab/proto/client.py`
- `codelab/proto/server.py`
- `codelab/proto/Makefile`
- `devtools/setup-dev/ansible/packages_data.go`
- generated Ansible artifacts (`poetry.yml` removed, `BUILD.bazel`,
  `README.org` as the generator updates them)

Delete:

- `poetry.lock`
- `requirements.in`
- `requirements.txt`
- `devtools/setup-dev/ansible/poetry.yml`

## Risks

- `rules_python` 2.x venv bootstrap and `uv_lock` import are newer than
  this repo's 1.0.0 pin. Mitigate by landing PR 1 first and using the
  export fallback only if `uv_lock` import fails.
- grpcio native wheels may be missing on Termux. Mitigate with
  `target_compatible_with`, not a second install path.
- A naive hatchling/setuptools layout would require a `src/` package
  that does not exist. Mitigate with `[tool.uv] package = false`.
