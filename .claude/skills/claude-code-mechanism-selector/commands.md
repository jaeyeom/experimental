# Slash Commands Reference

## Overview

Slash commands are user-invoked prompts stored as Markdown files. You explicitly type `/command` to trigger them.

## When to Use

- Simple, frequently used prompts
- Quick reminders or templates
- Explicit control over when it runs
- Single-file instructions

## File Locations

| Type | Location | Scope |
|------|----------|-------|
| Project | `.claude/commands/` | Current project |
| Personal | `~/.claude/commands/` | All projects |

## File Format

```markdown
---
allowed-tools: Bash(git:*), Read, Edit
argument-hint: [message]
description: Brief description shown in /help
model: claude-3-5-haiku-20241022
---

Your prompt instructions here.

Use $ARGUMENTS for all arguments.
Use $1, $2, etc. for positional arguments.
```

## Frontmatter Options

| Field | Purpose | Default |
|-------|---------|---------|
| `allowed-tools` | Tools the command can use | Inherits from conversation |
| `argument-hint` | Shows expected arguments | None |
| `description` | Brief description | First line of prompt |
| `model` | Specific model to use | Inherits from conversation |
| `disable-model-invocation` | Prevent SlashCommand tool | false |

## Features

### Arguments
- `$ARGUMENTS` - All arguments as single string
- `$1`, `$2`, etc. - Individual positional arguments

### Bash Execution
Use `!` prefix to run bash before the command:
```markdown
## Context
- Git status: !`git status`
- Current branch: !`git branch --show-current`
```

### File References
Use `@` to include file contents:
```markdown
Review @src/main.ts against @docs/style-guide.md
```

## Examples

### Simple Review Command
```markdown
# .claude/commands/review.md
Review this code for:
- Security vulnerabilities
- Performance issues
- Code style violations
```

### Commit Command with Tools
```markdown
---
allowed-tools: Bash(git add:*), Bash(git commit:*)
description: Create a git commit
---

## Context
- Status: !`git status`
- Diff: !`git diff HEAD`

Create a commit based on these changes.
```

## Namespacing

Subdirectories group commands but don't affect the name:
- `.claude/commands/frontend/component.md` → `/component` (project:frontend)
- `.claude/commands/backend/api.md` → `/api` (project:backend)

## Priority

Project commands override user commands with the same name.
