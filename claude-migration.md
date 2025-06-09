# Claude Code Migration Guide

## Converting Windsurf Rules to Claude Code

This document explains how to migrate existing Windsurf rules to be compatible with Claude Code.

## Key Differences to Address:

1. **Remove Windsurf-specific metadata**: The YAML frontmatter with `trigger` and other Windsurf-specific fields won't work in Claude Code
2. **Integrate with CLAUDE.md**: Claude Code uses CLAUDE.md for project-specific instructions
3. **Remove context retention requirements**: Rule #11 about random emojis is Windsurf-specific

## Migration Options:

### Option 1: Integrate into CLAUDE.md (Recommended)
Add the relevant rules directly to your existing CLAUDE.md file under new sections:

```markdown
# Development Guidelines

## General Principles
- Practice Test Driven Development
- All exported identifiers should be documented
- Don't log errors and bubble them up
- Use simplest solution over complexity
- Follow SOLID principles
- Keep files under 200-300 lines

## Language-Specific Rules

### Go
- Use log/slog package for logging
- Define interfaces where used, not where implemented
- Clarify failing cases in table-driven tests

### Build Commands
- Run appropriate build/test commands when making changes
- Use `make` if Makefile exists
- Use `bazel run //:format && bazel test //...` for Bazel projects
```

### Option 2: Create Claude Code Rules Directory
Create a `docs/claude/rules/` directory with simplified markdown files without the YAML frontmatter:

```markdown
# Go Development Rules

- Use log/slog package for logging with proper log levels
- Define interfaces where used rather than implemented
- Clarify failing test cases in table-driven tests
```

### Option 3: Reference in CLAUDE.md
Add a reference section in CLAUDE.md:

```markdown
# Additional Guidelines
See docs/windsurf/rules/ for detailed development guidelines (ignore YAML frontmatter)
```

## Current Windsurf Rules Summary:

### General Rules (docs/windsurf/rules/general.md)
- Test Driven Development practices
- Documentation requirements for exported identifiers
- Error handling guidelines
- Code organization principles
- Simplicity and SOLID principles
- File size limits (200-300 lines)
- No mock data in dev/prod

### Go Rules (docs/windsurf/rules/go.md)
- Use log/slog package
- Define interfaces where used
- Clear table-driven test failure messages

### Build Rules
- **Bazel**: Run `bazel run //:format && bazel test //path/to/your/project/...:all`
- **Make**: Run `make` when Makefile exists

### Jira Rules (docs/windsurf/rules/jira.md)
- Use `jira` CLI tool with `--plain` option
- Specific commands for updating descriptions and linking tickets

## Recommendation

**Use Option 1** - integrate the most important rules directly into CLAUDE.md since Claude Code reads this file automatically and it keeps everything centralized.