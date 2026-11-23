# Standard Checker Baseline Format and CLI Contract

This document specifies the repository-standard grandfather/baseline JSON format, matching and ratcheting semantics, and CLI flag contract for repository checkers and linters.

## Design Goals

1. **Zero external dependencies**: Standard JSON readable and writable with the standard library in Go, Python, shell, and Bazel Starlark.
2. **Hermetic Bazel caching**: Each checker owns an isolated file so updating one checker's baseline does not invalidate other checkers' Bazel test caches.
3. **Deterministic diffs**: Canonical formatting ensures minimal, noise-free diffs across automated updates.
4. **Ratcheting without time-bombs**: Progress is enforced by tightening limits as code improves, not by calendar-based expiration dates.

---

## 1. File Layout Convention

Each checker stores its baseline in a dedicated file under `.baselines/` at the repository root:

```text
.baselines/<checker>.json
```

For example:

- `.baselines/agents-md-size.json`

Keeping one file per checker preserves Bazel test cache isolation (each `bazel test` target declares only its own `.baselines/<checker>.json` in `data`) and allows parallel `--update-baseline` runs across different checkers without file contention or merge conflicts.

---

## 2. JSON Schema (`version: 1`)

### Top-Level Object

| Field | Type | Required | Description |
| --- | --- | --- | --- |
| `version` | integer | yes | Schema version. Must be `1`. Loaders must reject unknown versions. |
| `checker` | string | yes | Canonical checker name (matching `<checker>` in `.baselines/<checker>.json`). |
| `entries` | object | yes | Map of normalized repo-relative target path or glob pattern to entry object. |

Loaders must reject unknown JSON fields at both the top level and entry level (e.g., `DisallowUnknownFields` in Go) so typos such as `limit` or unsupported fields fail fast.

### Entry Object

All fields in an entry object are optional:

| Field | Type | Required | Description |
| --- | --- | --- | --- |
| `limits` | object | no | Map of metric name (`max_*` or `min_*`) to integer (`int64`), e.g. `max_bytes`, `max_cascading_bytes`, `min_doc_bytes`, `max_violations`. |
| `reason` | string | no | Short human-readable explanation for why the baseline entry exists. |
| `issue` | string | no | Optional tracking reference (e.g., `#123` or issue URL). |

An empty entry `{}` (or an entry with no `limits` field) represents a **pure exemption**: the matched target is exempted from the checker regardless of its measured values.

### Why `expires` Is Omitted

Baseline entries intentionally do **not** have an `expires` (or any date-based expiration) field:

- **No calendar-triggered CI failures**: Expiring baselines cause `main` and unrelated PRs to suddenly fail on a future date without any code change.
- **Bazel cacheability**: Hermetic tests must depend only on repository contents, never on wall-clock time.
- **Ratcheting instead of time-bombs**: Checkers enforce forward progress through `--strict-baseline` and `--tighten-baseline` alongside optional `issue` links.

---

## 3. Canonical Formatting

Baseline files must be written in canonical JSON format:

- **Indentation**: 2 spaces per level.
- **Key ordering**: Lexicographically sorted object keys.
- **Omitted empty fields**: Omit empty optional entry fields (`limits`, `reason`, `issue`) so a pure exemption with no metadata serializes as `{}`.
- **Trailing newline**: Single `\n` at the end of the file.

### Examples

#### Numeric Override

```json
{
  "checker": "agents-md-size",
  "entries": {
    "devtools/setup-dev/ansible/AGENTS.md": {
      "issue": "#1069",
      "limits": {
        "max_bytes": 5120,
        "max_cascading_bytes": 18432
      },
      "reason": "Legacy Ansible playbook instructions pending split"
    }
  },
  "version": 1
}
```

#### Pure Exemption

```json
{
  "checker": "agents-md-size",
  "entries": {
    "third_party/generated/AGENTS.md": {}
  },
  "version": 1
}
```

#### Glob Exemption

```json
{
  "checker": "agents-md-size",
  "entries": {
    "**/testdata/**": {
      "reason": "Test fixtures are exempt from size limits"
    }
  },
  "version": 1
}
```

---

## 4. Matching & Ratcheting Rules

### Path Normalization

All target paths and entry keys must be normalized:

- Repo-relative, using forward slashes (`/`).
- No leading `./` and no trailing `/`.
- Cleaned (no redundant `//` or `.` segments).
- Loaders must reject absolute paths (e.g., `/etc/passwd`), parent directory traversal (`..`), and backslashes (`\`).

### Glob Support and Precedence

Entry keys containing `*`, `?`, or `[` are treated as glob patterns supporting:

- `*`: matches zero or more non-`/` characters within a single path segment.
- `?`: matches any single non-`/` character.
- `[...]`: character class matching a single non-`/` character.
- `**`: matches zero or more path segments.

When resolving a target path against `entries`:

1. **Exact path match** always takes precedence over any glob match.
2. Among matching globs, the pattern with the **longest non-wildcard prefix** (number of bytes/characters before the first `*`, `?`, or `[`) wins.
3. Ties in non-wildcard prefix length are broken **lexicographically** (smallest pattern string wins).

### Symmetric Ratcheting (`max_*` and `min_*`)

Checkers evaluate metrics against either an upper bound (`max_*`) or a lower bound (`min_*`):

- **Upper-bound metrics (`max_*`)**, e.g. `max_bytes`, `max_cascading_bytes`, `max_violations`:
  - Effective limit is `entry.limits[metric]` if present, otherwise `defaultMax`.
  - **Fail** if `actual > effectiveMax`.
  - **Stale override** if an exact-path override exists and `actual <= defaultMax` (the target now satisfies the default).
  - **Ratchet opportunity (tighten downward)** if an exact-path override exists and `defaultMax < actual < effectiveMax`: the limit can be tightened from `effectiveMax` down to `actual`.

- **Lower-bound metrics (`min_*`)**, e.g. `min_doc_bytes`:
  - Effective limit is `entry.limits[metric]` if present, otherwise `defaultMin`.
  - **Fail** if `actual < effectiveMin`.
  - **Stale override** if an exact-path override exists and `actual >= defaultMin` (the target now satisfies the default).
  - **Ratchet opportunity (tighten upward)** if an exact-path override exists and `effectiveMin < actual < defaultMin`: the limit can be tightened from `effectiveMin` up to `actual`.

- **Pure exemption** (`limits` absent or empty):
  - Always passes regardless of observed metric values.

### Staleness Conditions

Under `--strict-baseline` (enabled by default), a baseline is stale if any of the following conditions hold:

1. **Deleted target**: An exact-path entry refers to a target that no longer exists.
2. **Satisfies defaults**: An exact-path entry (or one of its `limits` overrides) now passes the checker's default threshold(s).
3. **Looser-than-needed limit**: An exact-path entry has a limit that can be ratcheted closer to the default (`actual < effectiveMax` for `max_*`, or `actual > effectiveMin` for `min_*`).
4. **Unmatched glob**: A glob entry matches zero existing targets.

### Manual Glob and Metadata Preservation

- **Globs are manual**: `--update-baseline` and `--tighten-baseline` must **never** auto-generate glob entries and must **never** rewrite or expand existing glob entries into exact-path entries.
- **Metadata is preserved**: When an entry is kept or ratcheted, existing `reason` and `issue` values must be preserved.

---

## 5. Standard CLI Contract

Checkers supporting baselines must expose the following flags:

| Flag | Default | Description |
| --- | --- | --- |
| `--baseline <path>` | `.baselines/<checker>.json` (if present) | Path to the baseline JSON file. |
| `--update-baseline` | `false` | Rewrite the baseline file: record new violations at their observed values, remove stale exact-path entries, tighten improved limits, and preserve `reason`, `issue`, and manual globs. |
| `--tighten-baseline` | `false` | Rewrite the baseline file to **only** remove stale exact-path entries and tighten looser-than-needed limits toward defaults; **never** add new entries or loosen existing limits. |
| `--strict-baseline` | `true` | Fail the check when stale entries or loosenable (ratchetable) limits exist, even if all targets pass their baselined limits. |

`--update-baseline` and `--tighten-baseline` are mutually exclusive; passing both must return a usage error.

### Exit Codes

| Code | Meaning |
| --- | --- |
| `0` | Pass (all targets within limits, and no staleness issues when `--strict-baseline=true`; or baseline updated/tightened without remaining violations). |
| `1` | Check failure (limit violation, or stale/loosenable baseline entry when `--strict-baseline=true`, or unbaselined violation during `--tighten-baseline`). |
| `2` | Usage, configuration, or runtime error (invalid flags, malformed JSON, unsupported `version`, unnormalized keys, I/O failure). |
