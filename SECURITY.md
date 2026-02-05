# Security Policy

## Supported Versions

This is an experimental repository containing various prototypes and tools. There is no formal versioning or release schedule. Security updates are applied to the latest code on the `main` branch.

| Branch | Supported          |
| ------ | ------------------ |
| main   | :white_check_mark: |
| other  | :x:                |

## Reporting a Vulnerability

If you discover a security vulnerability in this repository, please report it responsibly.

### How to Report

1. **Do not** open a public GitHub issue for security vulnerabilities.
2. Send a private report via [GitHub Security Advisories](https://github.com/jaeyeom/experimental/security/advisories/new) or email the maintainer directly.
3. Include as much detail as possible:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

### What to Expect

- **Acknowledgment**: You will receive an acknowledgment within 7 days.
- **Assessment**: The vulnerability will be assessed and prioritized based on severity.
- **Resolution**: A fix will be applied as soon as reasonably possible.
- **Disclosure**: You will be credited in the fix (unless you prefer anonymity).

### Scope

This policy covers code in this repository. Dependencies are managed through their respective maintainers.

## Security Practices

This repository uses:
- Static analysis via `golangci-lint` and `ruff`
- Automated testing via CI
- Dependency scanning where applicable
