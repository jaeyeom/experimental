For important context and instructions, please review the `CLAUDE.md` file before proceeding.

When creating a pull request, omit HTML comments (`<!-- ... -->`) from
`.github/pull_request_template.md` in the submitted body. This repository
squash-merges with the PR title and description as the commit message, so
those comments would appear in `git log`. Delete unused optional sections
instead of leaving them empty. A workflow strips leftover comments from
the stored body as a backstop. Other repositories can call
`jaeyeom/experimental/.github/workflows/strip-pr-html-comments.yml`
pinned to a commit SHA.

## Instructions for Jules (non-local Cloud-based async workflow)

- Run `make check` and resolve all issues.
- After running `make`, which may generate files, ensure you `git add` the generated files.
- Do not commit an empty diff. If the diff is empty, something is wrong or files have not been added.
- Before signaling completion, ensure you `git push` your changes to apply them to the PR.
- Be aware of the correct PR number and do not work on a random one.
