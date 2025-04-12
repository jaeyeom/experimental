workspace(name = "experimental")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Python rules
http_archive(
    name = "rules_python",
    sha256 = "0a8003b044294d7840ac7d9d73eef05d6ceb682d7516781a4ec62eeb34702578",
    strip_prefix = "rules_python-0.24.0",
    url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.24.0.tar.gz",
)

load("@rules_python//python:repositories.bzl", "py_repositories")

py_repositories()

# Python dependencies
load("@rules_python//python:pip.bzl", "pip_parse")

pip_parse(
    name = "pip",
    requirements_lock = "//:requirements.txt",
)

load("@pip//:requirements.bzl", "install_deps")

install_deps()

# Repository dependencies go here
