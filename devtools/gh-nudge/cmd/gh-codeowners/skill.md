---
name: gh-codeowners
description: CLI that identifies GitHub CODEOWNERS for files in a repository.
---

# gh-codeowners

`gh-codeowners` resolves code owners for file paths using GitHub's section-based `CODEOWNERS` rules.

Common usage:
- `gh-codeowners file1.go file2.js`
- `gh-codeowners --codeowners-file=/path/to/CODEOWNERS file1.go`
- `git diff --name-only | gh-codeowners`
