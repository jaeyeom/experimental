# Modern Python Practices Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Poetry + pip-tools with uv and `rules_python` 2.3.2 `uv.lock` import, run proto tests as Bazel `py_test`, type-check with mypy from Make, drop the Poetry playbook, and modernize the proto demo.

**Architecture:** `pyproject.toml` + `uv.lock` is the lock source. Bazel `pip.parse(uv_lock=...)` exposes wheels as `@pypi//...`. Host ruff and mypy lint handwritten Python. Format stays on `bazel run //:format`. Proto tests run only through `bazel test`, not a parallel `uv run pytest` Make target.

**Tech Stack:** Python 3.11, uv, ruff, mypy, pytest, rules_python 2.3.2, grpcio, protobuf, protovalidate, GNU Make, Ansible generator in `devtools/setup-dev/ansible`.

**Spec:** `docs/superpowers/specs/2026-08-28-python-modern-practices-design.md`

## Global Constraints

- Python floor is `>=3.11` (Bazel `python_version = "3.11"`).
- `rules_python` version is `2.3.2`.
- `make test` stays `bazel test //...`. Do not add `uv run pytest` to Make.
- Host linters stay host tools: `ruff check`, `mypy`.
- Import modules, not types/functions, except `collections.abc` / `typing` annotation helpers.
- Wrap with `raise ... from err`. Do not log and re-raise. Interceptor uses `context.abort` and does not also raise `ValidationError`.
- No `# noqa` for the proto rewrite. Fix the code.
- No `monkeypatch` of `uuid` or first-party classes. No `MagicMock` for collaborators we own.
- Do not add basedpyright, pydantic, pytest-as-Ansible-package, or a `src/` package tree.
- `[tool.uv] package = false`. Do not publish a wheel.
- Keep Ruff line length 79, `quote-style = "preserve"`, Google docstrings, `UP`+`D`.
- If `pip.parse(uv_lock=...)` fails in this repo, fall back to `uv export --frozen` + `requirements_lock` only after that failure is demonstrated. Do not start on the fallback path.
- grpcio on Termux: mark targets `target_compatible_with`, do not host-pip install.

## File map

| File | Role |
|------|------|
| `pyproject.toml` | PEP 621 project, uv, ruff, mypy |
| `uv.lock` | Canonical lock |
| `.python-version` | `3.11` |
| `MODULE.bazel` | rules_python 2.3.2, toolchain, `pip.parse(uv_lock)` |
| `Makefile` | `check-uv-lock`, later `lint-mypy` |
| `BUILD.bazel` | export `uv.lock` and `pyproject.toml` |
| `devtools/setup-dev/ansible/packages_data.go` | Drop poetry package |
| `codelab/proto/server.py` | Database, service, interceptor |
| `codelab/proto/client.py` | Client library + CLI main |
| `codelab/proto/server_test.py` | Database + interceptor tests |
| `codelab/proto/client_test.py` | Client tests |
| `codelab/proto/BUILD.bazel` | `py_library` / `py_test` / `py_binary` |
| `codelab/proto/gen/__init__.py` | Make `gen` a package |
| `codelab/proto/Makefile` | `PYTHONPATH=.` |

Delete: `poetry.lock`, `requirements.in`, `requirements.txt`, `devtools/setup-dev/ansible/poetry.yml` (via generator).

---

### Task 1: Toolchain (PR 1)

Do packaging and Bazel in one task. Deleting `requirements.txt` without updating `MODULE.bazel` breaks `bazel test`.

**Files:**
- Modify: `pyproject.toml` (replace Poetry tables; keep `[tool.ruff]*`)
- Create: `uv.lock`, `.python-version`
- Modify: `.gitignore`, `Makefile`, `MODULE.bazel`, `BUILD.bazel`, `README.org`
- Delete: `poetry.lock`, `requirements.in`, `requirements.txt`

**Interfaces:**
- Consumes: nothing from later tasks
- Produces: `@pypi` hub, `uv lock --check`, Python 3.11 toolchain, no Poetry/pip-tools files

- [ ] **Step 1: Rewrite `pyproject.toml`**

Replace the Poetry header and dependencies. Keep every existing `[tool.ruff]` table unchanged.

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

Do not add `[tool.mypy]` yet (strict mypy would fail on untyped proto files). Do not list beautifulsoup4, cssutils, soupsieve, requests, buf, or android-only grpcio markers.

- [ ] **Step 2: Pin Python and ignore the venv**

Create `.python-version` with exactly:

```
3.11
```

Append to `.gitignore`:

```
# Python
.venv/
__pycache__/
*.py[cod]
```

- [ ] **Step 3: Create `uv.lock` and delete old lockfiles**

```bash
uv lock
rm poetry.lock requirements.in requirements.txt
uv lock --check
```

Expected: `uv lock --check` exits 0. `uv.lock` is committed later.

- [ ] **Step 4: Point Make at `uv lock --check`**

In `Makefile`:

1. Change `.PHONY` `check-requirements` to `check-uv-lock`.
2. `all:` first phase: drop `requirements.txt`. Keep `generate-ansible generate-pkl`.
3. `check:` first phase: `check-uv-lock check-generated` instead of `check-requirements check-generated`.
4. Replace the `check-requirements` / `requirements.txt:` block with:

```makefile
.PHONY: lint-ruff fix-ruff check-uv-lock

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

check-uv-lock: pyproject.toml uv.lock
	@if ! command -v uv >/dev/null 2>&1; then \
		echo "Error: uv is required to check uv.lock"; \
		exit 1; \
	fi
	uv lock --check
```

- [ ] **Step 5: Bump `rules_python` and import `uv.lock`**

In `MODULE.bazel` replace the Python block (the `rules_python` 1.0.0 `pip_ext` block) with:

```python
# Python
bazel_dep(name = "rules_python", version = "2.3.2")

python = use_extension(
    "@rules_python//python/extensions:python.bzl",
    "python",
)
python.toolchain(is_default = True, python_version = "3.11")

pip = use_extension("@rules_python//python/extensions:pip.bzl", "pip")
pip.parse(
    hub_name = "pypi",
    python_version = "3.11",
    pyproject_toml = "//:pyproject.toml",
    uv_lock = "//:uv.lock",
)
use_repo(pip, "pypi")
```

In root `BUILD.bazel`, add the two files to `exports_files`:

```python
exports_files(
    [
        "MODULE.bazel",
        ".shellcheckrc",
        "pyproject.toml",
        "uv.lock",
    ],
)
```

- [ ] **Step 6: Mention uv in `README.org`**

In Getting Started linters, keep ruff. In GNU Make table, `make check` still runs tests/lint. Add uv next to the linter bullet:

```
- =golangci-lint=, =ruff=, =shellcheck=, =semgrep= — linters (run by =make lint= / =make check=).
- =uv= — Python lockfile tool (`uv.lock`; checked by =make check=).
```

Do not mention mypy yet.

- [ ] **Step 7: Load the Bazel module and run tests**

```bash
bazel test --test_summary=terse //...
uv lock --check
make check-uv-lock
```

Expected: Bazel fetches `rules_python` 2.3.2, updates `MODULE.bazel.lock`, and existing Go tests still pass. There are no Python `py_test` targets yet.

If `pip.parse(uv_lock=...)` errors (cannot parse lock, missing wheels, source-less workspace member), stop. Switch that parse call to:

```python
pip.parse(
    hub_name = "pypi",
    python_version = "3.11",
    requirements_lock = "//:requirements.txt",
)
```

after `uv export --frozen --no-hashes -o requirements.txt` (include
dev/pytest; Bazel tests need it), and add a Make rule that fails if
`requirements.txt` is stale relative to `uv.lock`. Only do this after
the `uv_lock` attribute fails. Record the error in the commit message.

- [ ] **Step 8: Commit PR 1**

```bash
git add pyproject.toml uv.lock .python-version .gitignore Makefile MODULE.bazel MODULE.bazel.lock BUILD.bazel README.org
git rm poetry.lock requirements.in requirements.txt
git commit -m "$(cat <<'EOF'
build(python): switch packaging to uv and rules_python 2.3.2

Replace Poetry and pip-tools with PEP 621 pyproject.toml plus uv.lock.
Bazel consumes uv.lock via pip.parse. Drop leftover html_convert deps.
EOF
)"
```

---

### Task 2: Drop Poetry from Ansible (PR 2)

**Files:**
- Modify: `devtools/setup-dev/ansible/packages_data.go` (remove the poetry `PackageData` entry)
- Generated: delete `devtools/setup-dev/ansible/poetry.yml`; update `devtools/setup-dev/ansible/BUILD.bazel` and `devtools/setup-dev/ansible/README.org` via the generator

**Interfaces:**
- Consumes: nothing from Task 1
- Produces: no `poetry.yml` playbook; generator tests pass

- [ ] **Step 1: Remove the poetry package**

In `devtools/setup-dev/ansible/packages_data.go`, delete this line from the `packages` slice (it sits between `pkg-config` and `protoc`):

```go
{command: "poetry", debianPkgName: "python3-poetry", termuxPkgName: "python-poetry"},
```

Do not edit `poetry.yml` by hand.

- [ ] **Step 2: Regenerate playbooks**

From the repo root:

```bash
make generate-ansible
```

Expected: `devtools/setup-dev/ansible/poetry.yml` is gone. `BUILD.bazel` no longer has `poetry_syntax_test`.

- [ ] **Step 3: Verify generator tests**

```bash
bazel test --test_summary=terse //devtools/setup-dev/ansible:ansible_syntax_tests
go test ./devtools/setup-dev/ansible/
```

Expected: PASS. `poetry.yml` is not in `git ls-files`.

- [ ] **Step 4: Commit PR 2**

```bash
git add devtools/setup-dev/ansible/packages_data.go \
  devtools/setup-dev/ansible/BUILD.bazel \
  devtools/setup-dev/ansible/README.org
git rm devtools/setup-dev/ansible/poetry.yml
git commit -m "$(cat <<'EOF'
chore(ansible): stop installing Poetry

uv is the Python package manager in setup-dev. Drop the opt-in poetry
playbook so new machines do not get Poetry.
EOF
)"
```

---

### Task 3: Proto Bazel scaffolding

**Files:**
- Create: `codelab/proto/gen/__init__.py`, `codelab/proto/BUILD.bazel`
- Modify: `codelab/proto/Makefile`

**Interfaces:**
- Consumes: `@pypi//grpcio`, `@pypi//protobuf`, `@pypi//protovalidate`, `@pypi//pytest` from Task 1
- Produces: `//codelab/proto:server`, `:client`, `:gen`, `:server_bin`, `:client_bin`; tests added in later tasks

- [ ] **Step 1: Make `gen` a package**

Create empty `codelab/proto/gen/__init__.py`.

- [ ] **Step 2: Write `codelab/proto/BUILD.bazel`**

```python
load("@rules_python//python:defs.bzl", "py_binary", "py_library", "py_test")

package(default_visibility = ["//visibility:private"])

# grpcio wheels are not expected on Termux/Android.
_PY_COMPATIBLE = select({
    "@platforms//os:linux": [],
    "@platforms//os:osx": [],
    "//conditions:default": ["@platforms//:incompatible"],
})

py_library(
    name = "gen",
    srcs = glob(
        ["gen/**/*.py"],
        exclude = ["**/__pycache__/**"],
    ),
    imports = ["."],
    target_compatible_with = _PY_COMPATIBLE,
)

py_library(
    name = "server",
    srcs = ["server.py"],
    imports = ["."],
    target_compatible_with = _PY_COMPATIBLE,
    deps = [
        ":gen",
        "@pypi//grpcio",
        "@pypi//protobuf",
        "@pypi//protovalidate",
    ],
)

py_library(
    name = "client",
    srcs = ["client.py"],
    imports = ["."],
    target_compatible_with = _PY_COMPATIBLE,
    deps = [
        ":gen",
        "@pypi//grpcio",
        "@pypi//protobuf",
        "@pypi//protovalidate",
    ],
)

py_binary(
    name = "server_bin",
    srcs = ["server.py"],
    imports = ["."],
    main = "server.py",
    target_compatible_with = _PY_COMPATIBLE,
    deps = [":server"],
)

py_binary(
    name = "client_bin",
    srcs = ["client.py"],
    imports = ["."],
    main = "client.py",
    target_compatible_with = _PY_COMPATIBLE,
    deps = [":client"],
)
```

Do not add `py_test` until the test files exist (empty glob / missing srcs will fail).

- [ ] **Step 3: Fix proto Makefile PYTHONPATH**

```makefile
run-server: generate
	PYTHONPATH=. python3 -m server

run-client: generate
	PYTHONPATH=. python3 -m client
```

- [ ] **Step 4: Build the libraries**

```bash
bazel build //codelab/proto:server //codelab/proto:client
```

Expected: PASS on Linux/macOS. If `@pypi//grpcio` is missing, stop and inspect `uv.lock` / `pip.parse` before continuing.

- [ ] **Step 5: Commit scaffolding**

```bash
git add codelab/proto/BUILD.bazel codelab/proto/gen/__init__.py codelab/proto/Makefile
git commit -m "$(cat <<'EOF'
build(proto): add Python Bazel targets and fix PYTHONPATH

Import gen as a package and skip Android so grpcio wheels are not
required on Termux.
EOF
)"
```

---

### Task 4: Database — tests then implementation

**Files:**
- Create: `codelab/proto/server_test.py`
- Modify: `codelab/proto/server.py` (`Database` only in this task)
- Modify: `codelab/proto/BUILD.bazel` (add `server_test`)

**Interfaces:**
- Consumes: `py_library` `:server` and `:gen`
- Produces:

```python
class Database:
    def __init__(
        self,
        kv_storage: MutableMapping[str, bytes],
        *,
        new_id: Callable[[], str] | None = None,
    ) -> None: ...
    def list_contacts(
        self, query: str = '',
    ) -> list[contacts_pb2.Contact]: ...
    def upsert_contact(
        self, contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact: ...
    def delete_contact(
        self, contact_uuid: str,
    ) -> contacts_pb2.Contact: ...
```

`new_id` defaults to `lambda: str(uuid.uuid4())`. Missing delete key raises `KeyError`.

- [ ] **Step 1: Write failing Database tests**

Create `codelab/proto/server_test.py`:

```python
"""Tests for the contacts Database."""

import sys

import pytest

from gen import contacts_pb2
import server


def _contact(
    *,
    uuid: str = '',
    name: str = '',
    email: str = '',
    phone: str = '',
) -> contacts_pb2.Contact:
    return contacts_pb2.Contact(
        uuid=uuid, name=name, email=email, phone=phone,
    )


def _store(*contacts: contacts_pb2.Contact) -> dict[str, bytes]:
    return {c.uuid: c.SerializeToString() for c in contacts}


@pytest.mark.parametrize(
    ('query', 'want_names'),
    [
        ('', ['Alice', 'Bob']),
        ('Ali', ['Alice']),
        ('example.com', ['Alice']),
        ('222', ['Bob']),
        ('zzz', []),
    ],
)
def test_list_contacts_filters_by_query(
    query: str, want_names: list[str],
) -> None:
    db = server.Database(
        _store(
            _contact(uuid='a', name='Alice', email='alice@example.com'),
            _contact(uuid='b', name='Bob', phone='222-333-4444'),
        ),
    )
    got = [c.name for c in db.list_contacts(query)]
    assert got == want_names


def test_upsert_assigns_id_when_missing() -> None:
    db = server.Database({}, new_id=lambda: 'fixed-id')
    got = db.upsert_contact(_contact(name='Carol'))
    assert got.uuid == 'fixed-id'
    assert 'fixed-id' in db.contacts


def test_upsert_keeps_existing_id() -> None:
    db = server.Database({}, new_id=lambda: 'should-not-run')
    got = db.upsert_contact(_contact(uuid='kept-id', name='Dave'))
    assert got.uuid == 'kept-id'


def test_delete_returns_contact() -> None:
    stored = _contact(uuid='a', name='Alice')
    db = server.Database(_store(stored))
    got = db.delete_contact('a')
    assert got.name == 'Alice'
    assert db.contacts == {}


def test_delete_missing_uuid_raises_key_error() -> None:
    db = server.Database({})
    with pytest.raises(KeyError):
        db.delete_contact('missing')


if __name__ == '__main__':
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
```

Do not `monkeypatch` `uuid`.

- [ ] **Step 2: Add the `py_test` target**

Append to `codelab/proto/BUILD.bazel`:

```python
py_test(
    name = "server_test",
    srcs = ["server_test.py"],
    imports = ["."],
    target_compatible_with = _PY_COMPATIBLE,
    deps = [
        ":server",
        "@pypi//pytest",
    ],
)
```

- [ ] **Step 3: Run tests and confirm they fail**

```bash
bazel test --test_summary=terse //codelab/proto:server_test
```

Expected: FAIL (`Database.__init__` does not accept `new_id`, or upsert still calls `uuid.uuid4()`).

- [ ] **Step 4: Implement `Database`**

Replace `Database` in `codelab/proto/server.py` (leave ContactsService, interceptor, and `main` for later tasks, but add the new imports at top):

```python
"""Server implements the gRPC server for the contacts service."""

from collections.abc import Callable, MutableMapping
from concurrent import futures
import logging
import uuid

import grpc
import protovalidate

from gen import contacts_pb2
from gen import contacts_pb2_grpc

logger = logging.getLogger(__name__)


class Database:
    """In-memory contact store."""

    def __init__(
        self,
        kv_storage: MutableMapping[str, bytes],
        *,
        new_id: Callable[[], str] | None = None,
    ) -> None:
        """Initialize the database.

        Args:
            kv_storage: Mapping from contact uuid to serialized Contact.
            new_id: Factory for new uuids. Defaults to uuid4 strings.
        """
        self.contacts = kv_storage
        self._new_id = (
            new_id if new_id is not None else lambda: str(uuid.uuid4())
        )

    def list_contacts(
        self, query: str = '',
    ) -> list[contacts_pb2.Contact]:
        """List contacts whose name, email, or phone contains query."""
        decoded = (
            contacts_pb2.Contact.FromString(raw)
            for raw in self.contacts.values()
        )
        return [
            contact
            for contact in decoded
            if (
                query in contact.name
                or query in contact.email
                or query in contact.phone
            )
        ]

    def upsert_contact(
        self, contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact:
        """Insert or update a contact, assigning uuid when missing."""
        if not contact.uuid:
            contact.uuid = self._new_id()
        self.contacts[contact.uuid] = contact.SerializeToString()
        return contacts_pb2.Contact.FromString(
            self.contacts[contact.uuid],
        )

    def delete_contact(
        self, contact_uuid: str,
    ) -> contacts_pb2.Contact:
        """Delete a contact by uuid.

        Raises:
            KeyError: If contact_uuid is not in the store.
        """
        return contacts_pb2.Contact.FromString(
            self.contacts.pop(contact_uuid),
        )
```

Keep the existing `ContactsService`, `ValidationInterceptor`, and `main` below this class for now.

- [ ] **Step 5: Re-run Database tests**

```bash
bazel test --test_summary=terse //codelab/proto:server_test
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add codelab/proto/server.py codelab/proto/server_test.py codelab/proto/BUILD.bazel
git commit -m "$(cat <<'EOF'
feat(proto): inject Database id factory and add pytest coverage

List, upsert, and delete are tested with a dict store and a fixed
new_id. uuid is not patched.
EOF
)"
```

---

### Task 5: ValidationInterceptor — tests then implementation

**Files:**
- Modify: `codelab/proto/server.py` (`ValidationInterceptor`)
- Modify: `codelab/proto/server_test.py` (append interceptor tests)

**Interfaces:**
- Consumes: `Database` from Task 4
- Produces:

```python
class ValidationInterceptor(grpc.ServerInterceptor):
    def __init__(
        self,
        *,
        validate: Callable[[object], None] | None = None,
    ) -> None: ...
    def intercept_service(
        self,
        continuation: Callable[..., grpc.RpcMethodHandler | None],
        handler_call_details: grpc.HandlerCallDetails,
    ) -> grpc.RpcMethodHandler | None: ...
```

On `protovalidate.ValidationError`, call
`context.abort(grpc.StatusCode.INVALID_ARGUMENT, str(err.violations))`
and do not also raise that error. Non-unary handlers pass through.

- [ ] **Step 1: Write failing interceptor tests**

Append to `codelab/proto/server_test.py` (keep the pytest.main shim at the file bottom):

```python
class _AbortError(Exception):
    def __init__(self, code: grpc.StatusCode, details: str) -> None:
        super().__init__(details)
        self.code = code
        self.details = details


class _FakeContext:
    def abort(self, code: grpc.StatusCode, details: str) -> None:
        raise _AbortError(code, details)


class _FakeHandler:
    def __init__(self, unary_unary: object | None) -> None:
        self.unary_unary = unary_unary
        self.request_deserializer = None
        self.response_serializer = None


def test_interceptor_aborts_on_validation_error() -> None:
    def fail(_request: object) -> None:
        raise protovalidate.ValidationError([])

    interceptor = server.ValidationInterceptor(validate=fail)
    inner_called = []

    def inner(request: object, context: object) -> str:
        inner_called.append(request)
        return 'ok'

    handler = interceptor.intercept_service(
        lambda _details: _FakeHandler(inner),
        handler_call_details=None,
    )
    context = _FakeContext()
    with pytest.raises(_AbortError) as caught:
        handler.unary_unary(object(), context)
    assert caught.value.code == grpc.StatusCode.INVALID_ARGUMENT
    assert inner_called == []


def test_interceptor_delegates_when_valid() -> None:
    interceptor = server.ValidationInterceptor(validate=lambda _r: None)
    handler = interceptor.intercept_service(
        lambda _details: _FakeHandler(lambda _req, _ctx: 'ok'),
        handler_call_details=None,
    )
    assert handler.unary_unary(object(), _FakeContext()) == 'ok'


def test_interceptor_passes_through_non_unary() -> None:
    original = _FakeHandler(unary_unary=None)
    interceptor = server.ValidationInterceptor(validate=lambda _r: None)
    got = interceptor.intercept_service(
        lambda _details: original,
        handler_call_details=None,
    )
    assert got is original
```

Add `import grpc` and `import protovalidate` at the top of `server_test.py`.

`ValidationError([])` may not accept a list. If the constructor fails when you first run the test, use a tiny subclass:

```python
class _ValidationFailed(protovalidate.ValidationError):
    def __init__(self) -> None:
        Exception.__init__(self, 'invalid')
        self.violations = []
```

and raise `_ValidationFailed()` from `fail`. Prefer the real type if it constructs.

- [ ] **Step 2: Run interceptor tests, expect fail**

```bash
bazel test --test_summary=terse //codelab/proto:server_test
```

Expected: FAIL (`ValidationInterceptor.__init__` does not take `validate`, or `intercept_service` still treats continuation as the request).

- [ ] **Step 3: Implement the interceptor**

Replace `ValidationInterceptor` in `server.py`:

```python
class ValidationInterceptor(grpc.ServerInterceptor):
    """Validate unary requests with protovalidate before the handler."""

    def __init__(
        self,
        *,
        validate: Callable[[object], None] | None = None,
    ) -> None:
        """Initialize the interceptor.

        Args:
            validate: Request validator. Defaults to protovalidate.validate.
        """
        self._validate = (
            validate
            if validate is not None
            else protovalidate.validate
        )

    def intercept_service(
        self,
        continuation: Callable[
            [grpc.HandlerCallDetails],
            grpc.RpcMethodHandler | None,
        ],
        handler_call_details: grpc.HandlerCallDetails,
    ) -> grpc.RpcMethodHandler | None:
        """Wrap unary handlers with request validation."""
        handler = continuation(handler_call_details)
        if handler is None or handler.unary_unary is None:
            return handler

        inner = handler.unary_unary

        def unary_unary(
            request: object,
            context: grpc.ServicerContext,
        ) -> object:
            try:
                self._validate(request)
            except protovalidate.ValidationError as err:
                context.abort(
                    grpc.StatusCode.INVALID_ARGUMENT,
                    str(err.violations),
                )
            return inner(request, context)

        return grpc.unary_unary_rpc_method_handler(
            unary_unary,
            request_deserializer=handler.request_deserializer,
            response_serializer=handler.response_serializer,
        )
```

Remove the `print` in the old interceptor. Do not log and abort. Optional: `logger.debug` is unnecessary; skip it.

Wire the interceptor in `main` when you touch `main` in Task 6. In this task, keep `main` compiling: `grpc.server(...)` may still omit the interceptor until Task 6.

- [ ] **Step 4: Re-run tests**

```bash
bazel test --test_summary=terse //codelab/proto:server_test
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add codelab/proto/server.py codelab/proto/server_test.py
git commit -m "$(cat <<'EOF'
fix(proto): validate unary requests in the gRPC interceptor

Wrap the handler instead of treating continuation as the request.
Abort INVALID_ARGUMENT on ValidationError. Tests inject validate.
EOF
)"
```

---

### Task 6: ContactsClient, service types, and main

**Files:**
- Create: `codelab/proto/client_test.py`
- Modify: `codelab/proto/client.py`, `codelab/proto/server.py` (`ContactsService`, `main`)
- Modify: `codelab/proto/BUILD.bazel` (add `client_test`)

**Interfaces:**
- Consumes: `Database`, `ValidationInterceptor` from Tasks 4–5
- Produces:

```python
class ContactsClient:
    def __init__(
        self,
        channel: grpc.Channel | None = None,
        *,
        stub: ContactsStub | None = None,
        validate: Callable[[object], None] | None = None,
    ) -> None: ...
    def list_contacts(
        self, query: str = '',
    ) -> list[contacts_pb2.Contact]: ...
    def upsert_contact(
        self, contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact: ...
    def delete_contact(
        self, contact_uuid: str,
    ) -> contacts_pb2.Contact: ...
```

Methods raise `protovalidate.ValidationError`. They do not print. `main()` catches and prints.

- [ ] **Step 1: Write failing client tests**

Create `codelab/proto/client_test.py`:

```python
"""Tests for ContactsClient."""

import sys

import pytest
import protovalidate

from gen import contacts_pb2
import client as contacts_client


class FakeStub:
    """Records RPC calls and returns canned responses."""

    def __init__(self) -> None:
        self.calls: list[object] = []
        self.list_response = contacts_pb2.ContactListResponse()
        self.upsert_response = contacts_pb2.UpsertContactResponse()
        self.delete_response = contacts_pb2.DeleteContactResponse()

    def ListContacts(
        self, request: contacts_pb2.ContactListRequest,
    ) -> contacts_pb2.ContactListResponse:
        self.calls.append(request)
        return self.list_response

    def UpsertContact(
        self, request: contacts_pb2.UpsertContactRequest,
    ) -> contacts_pb2.UpsertContactResponse:
        self.calls.append(request)
        return self.upsert_response

    def DeleteContact(
        self, request: contacts_pb2.DeleteContactRequest,
    ) -> contacts_pb2.DeleteContactResponse:
        self.calls.append(request)
        return self.delete_response


def test_list_contacts_calls_stub_when_valid() -> None:
    stub = FakeStub()
    alice = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.list_response = contacts_pb2.ContactListResponse(
        contacts=[alice],
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.list_contacts(query='Ali')
    assert list(got) == [alice]
    assert stub.calls[0].query == 'Ali'


def test_list_contacts_skips_stub_when_invalid() -> None:
    stub = FakeStub()

    def fail(_request: object) -> None:
        raise protovalidate.ValidationError([])

    svc = contacts_client.ContactsClient(stub=stub, validate=fail)
    with pytest.raises(protovalidate.ValidationError):
        svc.list_contacts()
    assert stub.calls == []


def test_upsert_contact_calls_stub_when_valid() -> None:
    stub = FakeStub()
    stored = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.upsert_response = contacts_pb2.UpsertContactResponse(
        contact=stored,
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.upsert_contact(contacts_pb2.Contact(name='Alice'))
    assert got == stored
    assert stub.calls


def test_delete_contact_calls_stub_when_valid() -> None:
    stub = FakeStub()
    stored = contacts_pb2.Contact(uuid='a', name='Alice')
    stub.delete_response = contacts_pb2.DeleteContactResponse(
        contact=stored,
    )
    svc = contacts_client.ContactsClient(
        stub=stub, validate=lambda _req: None,
    )
    got = svc.delete_contact('a')
    assert got == stored


if __name__ == '__main__':
    sys.exit(pytest.main([__file__, *sys.argv[1:]]))
```

If `ValidationError([])` cannot be constructed, use the same `_ValidationFailed` subclass as Task 5.

- [ ] **Step 2: Add `client_test` target**

```python
py_test(
    name = "client_test",
    srcs = ["client_test.py"],
    imports = ["."],
    target_compatible_with = _PY_COMPATIBLE,
    deps = [
        ":client",
        "@pypi//pytest",
    ],
)
```

- [ ] **Step 3: Run client tests, expect fail**

```bash
bazel test --test_summary=terse //codelab/proto:client_test
```

Expected: FAIL (`ContactsClient.__init__` does not accept `stub` / `validate`).

- [ ] **Step 4: Implement `ContactsClient` and `main`**

Replace `codelab/proto/client.py`:

```python
"""Client for the contacts gRPC service."""

from collections.abc import Callable
from typing import Protocol
import logging
import sys

import grpc
import protovalidate

from gen import contacts_pb2
from gen import contacts_pb2_grpc

logger = logging.getLogger(__name__)


class ContactsStub(Protocol):
    """gRPC stub surface used by ContactsClient."""

    def ListContacts(
        self, request: contacts_pb2.ContactListRequest,
    ) -> contacts_pb2.ContactListResponse: ...

    def UpsertContact(
        self, request: contacts_pb2.UpsertContactRequest,
    ) -> contacts_pb2.UpsertContactResponse: ...

    def DeleteContact(
        self, request: contacts_pb2.DeleteContactRequest,
    ) -> contacts_pb2.DeleteContactResponse: ...


class ContactsClient:
    """Contacts client."""

    def __init__(
        self,
        channel: grpc.Channel | None = None,
        *,
        stub: ContactsStub | None = None,
        validate: Callable[[object], None] | None = None,
    ) -> None:
        """Initialize the client.

        Args:
            channel: Used to build a stub when stub is omitted.
            stub: Prebuilt stub for tests.
            validate: Request validator. Defaults to protovalidate.validate.

        Raises:
            ValueError: If neither channel nor stub is provided.
        """
        if stub is not None:
            self.stub = stub
        elif channel is not None:
            self.stub = contacts_pb2_grpc.ContactsServiceStub(channel)
        else:
            raise ValueError('channel or stub is required')
        self._validate = (
            validate
            if validate is not None
            else protovalidate.validate
        )

    def list_contacts(
        self, query: str = '',
    ) -> list[contacts_pb2.Contact]:
        """List contacts matching query."""
        request = contacts_pb2.ContactListRequest(query=query)
        self._validate(request)
        return list(self.stub.ListContacts(request).contacts)

    def upsert_contact(
        self, contact: contacts_pb2.Contact,
    ) -> contacts_pb2.Contact:
        """Insert or update a contact."""
        request = contacts_pb2.UpsertContactRequest(contact=contact)
        self._validate(request)
        return self.stub.UpsertContact(request).contact

    def delete_contact(
        self, contact_uuid: str,
    ) -> contacts_pb2.Contact:
        """Delete a contact by uuid."""
        request = contacts_pb2.DeleteContactRequest(uuid=contact_uuid)
        self._validate(request)
        return self.stub.DeleteContact(request).contact


def main() -> None:
    """Run a demo client against localhost:50051."""
    channel = grpc.insecure_channel('localhost:50051')
    svc = ContactsClient(channel)
    print('Upsert contacts:')
    try:
        alice_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Alice',
                email='alice@example.com',
                phone='123-456-7890',
            ),
        )
        print(alice_contact)
    except protovalidate.ValidationError as err:
        logger.error('upsert Alice failed: %s', err.violations)
        print(err.violations)
        alice_contact = None

    try:
        bob_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Bob',
                email='',
                phone='222-333-4444',
            ),
        )
        print(bob_contact)
    except protovalidate.ValidationError as err:
        logger.error('upsert Bob failed: %s', err.violations)
        print(err.violations)
        bob_contact = None

    # Wrong email fails validation
    try:
        wrong_contact = svc.upsert_contact(
            contacts_pb2.Contact(
                name='Wrong',
                email='wrong',
                phone='555-666-7777',
            ),
        )
        print(wrong_contact)
    except protovalidate.ValidationError as err:
        print(err.violations)

    print('List contacts:')
    print(svc.list_contacts())
    print('List contacts with query:')
    print(svc.list_contacts(query='Alice'))
    if alice_contact is not None:
        print('Delete contacts:')
        print(svc.delete_contact(alice_contact.uuid))
    print('List contacts:')
    print(svc.list_contacts())
    if bob_contact is not None:
        print('Delete contacts:')
        print(svc.delete_contact(bob_contact.uuid))
    print('List contacts:')
    print(svc.list_contacts())


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()
```

`main` may print CLI results. Library methods must not print.

- [ ] **Step 5: Annotate `ContactsService` and attach the interceptor in `main`**

Replace `ContactsService` and `main` in `server.py`:

```python
class ContactsService(contacts_pb2_grpc.ContactsServiceServicer):
    """Contacts service implementation."""

    def __init__(self, db: Database) -> None:
        """Initialize the service."""
        self.db = db

    def ListContacts(
        self,
        request: contacts_pb2.ContactListRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.ContactListResponse:
        """List contacts."""
        return contacts_pb2.ContactListResponse(
            contacts=self.db.list_contacts(request.query),
        )

    def UpsertContact(
        self,
        request: contacts_pb2.UpsertContactRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.UpsertContactResponse:
        """Upsert contact."""
        return contacts_pb2.UpsertContactResponse(
            contact=self.db.upsert_contact(request.contact),
        )

    def DeleteContact(
        self,
        request: contacts_pb2.DeleteContactRequest,
        context: grpc.ServicerContext,
    ) -> contacts_pb2.DeleteContactResponse:
        """Delete contact."""
        return contacts_pb2.DeleteContactResponse(
            contact=self.db.delete_contact(request.uuid),
        )


def main() -> None:
    """Start the contacts gRPC server on port 50051."""
    db = Database({})
    grpc_server = grpc.server(
        futures.ThreadPoolExecutor(),
        interceptors=[ValidationInterceptor()],
    )
    contacts_pb2_grpc.add_ContactsServiceServicer_to_server(
        ContactsService(db),
        grpc_server,
    )
    grpc_server.add_insecure_port('[::]:50051')
    grpc_server.start()
    grpc_server.wait_for_termination()


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    main()
```

- [ ] **Step 6: Run proto tests**

```bash
bazel test --test_summary=terse //codelab/proto:all
```

Expected: PASS (`server_test` and `client_test`).

- [ ] **Step 7: Commit**

```bash
git add codelab/proto/client.py codelab/proto/client_test.py \
  codelab/proto/server.py codelab/proto/BUILD.bazel
git commit -m "$(cat <<'EOF'
feat(proto): raise on client validation failure and type the service

ContactsClient injects stub and validate. Invalid requests never hit
the stub. main() prints CLI output and catches ValidationError.
EOF
)"
```

---

### Task 7: mypy on Make, README, full check

**Files:**
- Modify: `pyproject.toml` (add `[tool.mypy]`), `Makefile` (`lint-mypy` on `lint` and `fix`), `README.org`

**Interfaces:**
- Consumes: typed `client.py` / `server.py` / tests from Tasks 4–6
- Produces: `make lint` runs `mypy`; handwritten Python is clean

- [ ] **Step 1: Add mypy config**

Append to `pyproject.toml`:

```toml
[tool.mypy]
python_version = "3.11"
strict = true
mypy_path = "codelab/proto"
files = [
    "codelab/proto/client.py",
    "codelab/proto/server.py",
    "codelab/proto/client_test.py",
    "codelab/proto/server_test.py",
]
exclude = [
    "(^|/)gen/",
    ".*_pb2\\.py$",
    ".*_pb2_grpc\\.py$",
    "^bazel-",
]
```

- [ ] **Step 2: Wire `lint-mypy`**

In `Makefile`:

```makefile
.PHONY: lint-ruff fix-ruff lint-mypy check-uv-lock

lint-ruff:
	ruff check

fix-ruff:
	ruff check --fix

lint-mypy:
	mypy
```

Change:

```makefile
lint: lint-golangci lint-ruff lint-mypy lint-shellcheck check-spacemacs

fix: fix-golangci fix-ruff lint-mypy lint-shellcheck check-spacemacs
```

- [ ] **Step 3: Run ruff and mypy, fix without noqa**

```bash
ruff check
mypy
```

Expected: exit 0. If ruff/mypy fail, fix the handwritten files (imports, unused names, interceptor types). Generated pb2 files stay excluded. Do not add `# noqa` unless a false positive is documented in a comment on that line.

Likely fixes:

- Interceptor tests pass `handler_call_details=None`. If mypy or grpc
  typing rejects that, type the argument as `object`.
- `ContactsStub` Protocol (Task 6) must stay in sync with `FakeStub`.

- [ ] **Step 4: Mention mypy in `README.org`**

Getting Started linters:

```
- =golangci-lint=, =ruff=, =mypy=, =shellcheck=, =semgrep= — linters (run by =make lint= / =make check=).
```

Make table `make lint` row:

```
| =make lint=          | Run golangci-lint, ruff, mypy, shellcheck, and Spacemacs checks. |
```

- [ ] **Step 5: Full verification**

```bash
uv lock --check
ruff check
mypy
bazel test --test_summary=terse //...
make check
```

Expected: all PASS. Include any generated `MODULE.bazel.lock` or Ansible diffs that `make check` regenerates.

- [ ] **Step 6: Commit PR 3 remainder**

```bash
git add pyproject.toml Makefile README.org
git add -u
git commit -m "$(cat <<'EOF'
chore(python): type-check proto sources with mypy on make lint

Strict mypy runs on the handwritten proto modules. Generated pb2
files stay excluded.
EOF
)"
```

---

## Spec coverage

| Spec requirement | Task |
|------------------|------|
| PEP 621 + uv.lock + package = false | 1 |
| Drop poetry.lock, requirements.* | 1 |
| rules_python 2.3.2 + pip.parse(uv_lock) + @pypi | 1 |
| uv lock --check in Make | 1 |
| .python-version 3.11, gitignore .venv | 1 |
| README uv | 1 |
| uv_lock fallback only if demonstrated | 1 step 7 |
| Drop Ansible poetry playbook | 2 |
| gen/__init__.py, BUILD.bazel, PYTHONPATH=. | 3 |
| Termux target_compatible_with | 3 |
| Database new_id injection + tests | 4 |
| Interceptor wrap unary + abort + tests | 5 |
| Client inject stub/validate, raise, tests | 6 |
| Service types, interceptor in main | 6 |
| pytest.main shim | 4, 6 |
| mypy strict + lint-mypy | 7 |
| No parallel uv pytest Make target | all tasks |
| No noqa for proto rewrite | 7 |
| py_binary optional demos | 3 |

## Notes for the executor

- Stay on branch `docs/python-modern-practices` or restack onto a feature branch. Do not merge to main until `make check` passes.
- Task 1 is the risk (rules_python 1.0.0 → 2.3.2). Do not start Task 3 if `bazel test //...` fails after Task 1.
- `make check` is required before asking to land each PR, per repo `CLAUDE.md`.
