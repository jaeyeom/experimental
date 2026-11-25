"""Starlark macro that writes a pylinters entry manifest for a Bazel package."""

load("@bazel_skylib//rules:write_file.bzl", "write_file")

_RULE_KINDS = {
    "py_binary": "script",
    "py_test": "test",
}

def _strip_label(label, pkg):
    """Return a repo-relative path for a main-repo file label, or None."""
    if not label or label.startswith("@"):
        return None
    if label.startswith("//"):
        rest = label[2:]
        if ":" not in rest:
            return None
        target_pkg, target_name = rest.split(":", 1)
        return "%s/%s" % (target_pkg, target_name) if target_pkg else target_name
    name = label[1:] if label.startswith(":") else label
    return "%s/%s" % (pkg, name) if pkg else name

def _resolve_main(rule, pkg):
    """Resolve the main .py file of a py_binary or py_test rule dict."""
    if rule.get("main_module"):
        return None
    srcs = []
    raw_srcs = rule.get("srcs", ())
    if type(raw_srcs) in ("list", "tuple"):
        for s in raw_srcs:
            p = _strip_label(s, pkg)
            if p:
                srcs.append(p)
    main_attr = rule.get("main")
    if main_attr and type(main_attr) == "string":
        main_path = _strip_label(main_attr, pkg)
        if not main_path:
            return None
        suffix = main_attr.split(":")[-1] if ":" in main_attr else main_attr
    else:
        name = rule.get("name", "")
        main_path = "%s/%s.py" % (pkg, name) if pkg else "%s.py" % name
        suffix = "%s.py" % name

    exact = [p for p in srcs if p == main_path]
    if len(exact) == 1:
        return exact[0]
    trailing = [p for p in srcs if ("/" + p).endswith("/" + suffix)]
    if len(trailing) == 1:
        return trailing[0]
    return None

def py_entries_manifest(name, visibility = None):
    """Write a ``<kind> <repo-relative-path>`` manifest of py_binary/py_test mains.

    Must be called at the end of a BUILD.bazel file after all py_binary and
    py_test rules in the package have been declared.
    """
    pkg = native.package_name()
    pkg_prefix = pkg + "/" if pkg else ""
    pkg_sources = {
        (pkg_prefix + f): True
        for f in native.glob(["**/*.py"], allow_empty = True)
    }
    entries = {}
    for _rule_name, rule in native.existing_rules().items():
        kind = _RULE_KINDS.get(rule.get("kind", ""))
        if not kind:
            continue
        main_path = _resolve_main(rule, pkg)
        if not main_path:
            continue

        # Skip generated entry-point files (e.g., py_console_script_binary).
        if main_path.startswith(pkg_prefix) and main_path not in pkg_sources:
            continue
        entries["%s %s" % (kind, main_path)] = True
    write_file(
        name = name,
        out = name + ".txt",
        content = sorted(entries.keys()),
        visibility = visibility,
    )
