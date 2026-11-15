"""Rules for org-lint testing.

This module provides macros for generating org-lint test targets.
"""

def org_lint_tests(org_files, data = None):
    """Generate org-lint test targets for the given .org files.

    This macro creates a sh_test target for each .org file that runs
    org-lint on the file. The tests use the --skip-if-no-emacs flag
    to gracefully skip when emacs is not available (e.g., in CI).

    A new .org file is not yet known to version control, so Emacs does
    not follow the runfiles symlink to the source tree. [[file:]] links
    are then resolved inside the test's runfiles. Pass every linked file
    in data. Tracked files still pass without it, because Emacs follows
    those symlinks back to the checkout.

    Usage:
        load("//tools/org-lint:defs.bzl", "org_lint_tests")

        org_lint_tests([
            "README.org",
            "docs.org",
        ])

    Args:
        org_files: List of .org files to lint. Each file should be relative
                   to the current package directory.
        data: Extra runfiles every generated test needs so [[file:]] links
              resolve. Labels may be files in this package or exported
              files in another package. Defaults to none.

    Generated targets:
        For each file "example.org", generates a test target:
        - name: "example_org_lint_test"
        - tags: ["org-lint", "local"]
    """
    if data == None:
        data = []
    for org_file in org_files:
        # Generate test name from filename
        # "README.org" -> "README_org_lint_test"
        # "sub/doc.org" -> "sub_doc_org_lint_test"
        test_name = org_file.replace(".org", "").replace("/", "_").replace("-", "_") + "_org_lint_test"

        # The file under test is always a runfile. Skip a duplicate label
        # when the caller also lists it in data.
        runfiles = [org_file]
        for extra in data:
            if extra != org_file:
                runfiles.append(extra)

        native.sh_test(
            name = test_name,
            srcs = ["//tools/org-lint:test_org_lint.sh"],
            args = [org_file],
            data = runfiles,
            tags = [
                "org-lint",
                "local",
            ],
        )
