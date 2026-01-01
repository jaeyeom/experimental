# Subagents Reference

## Overview

Subagents are specialized AI assistants with their own context window, custom system prompts, and configurable tool access. They're invoked automatically or explicitly for task-specific workflows.

## When to Use

- Tasks requiring separate context (prevents main conversation pollution)
- Specialized expertise with detailed instructions
- Different tool permissions than main conversation
- Reusable workflows across projects

## File Locations

| Type | Location | Priority |
|------|----------|----------|
| Project | `.claude/agents/` | Highest |
| CLI | `--agents` flag | Medium |
| Personal | `~/.claude/agents/` | Lowest |

## File Format

```markdown
---
name: your-agent-name
description: When this agent should be invoked
tools: Read, Grep, Glob, Bash  # Optional - inherits all if omitted
model: sonnet                   # Optional - sonnet, opus, haiku, or inherit
permissionMode: default         # Optional - default, acceptEdits, bypassPermissions, plan
skills: skill1, skill2          # Optional - skills to auto-load
---

Your agent's system prompt here.

Include:
- Role and expertise
- Step-by-step process
- Constraints and best practices
```

## Frontmatter Options

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Lowercase letters and hyphens |
| `description` | Yes | When to invoke (include "proactively" for auto-use) |
| `tools` | No | Comma-separated list (inherits all if omitted) |
| `model` | No | `sonnet`, `opus`, `haiku`, or `inherit` |
| `permissionMode` | No | Permission handling mode |
| `skills` | No | Skills to auto-load |

## Built-in Subagents

### Explore
- **Model**: Haiku (fast)
- **Mode**: Read-only
- **Tools**: Glob, Grep, Read, limited Bash
- **Purpose**: Fast codebase search and analysis

### General-purpose
- **Model**: Sonnet
- **Mode**: Read/write
- **Tools**: All tools
- **Purpose**: Complex multi-step tasks

### Plan
- **Model**: Sonnet
- **Mode**: Research only
- **Tools**: Read, Glob, Grep, Bash
- **Purpose**: Codebase research in plan mode

## Invocation

### Automatic
Claude delegates based on task and description. Use phrases like:
- "Use PROACTIVELY"
- "MUST BE USED"

### Explicit
```
Use the code-reviewer subagent to check my changes
Have the debugger subagent investigate this error
```

## Examples

### Code Reviewer
```markdown
---
name: code-reviewer
description: Expert code reviewer. Use proactively after code changes.
tools: Read, Grep, Glob, Bash
model: inherit
---

You are a senior code reviewer.

When invoked:
1. Run git diff to see changes
2. Focus on modified files
3. Begin review immediately

Review checklist:
- Code clarity and readability
- Proper error handling
- No security issues
- Good test coverage

Organize feedback by priority:
- Critical (must fix)
- Warnings (should fix)
- Suggestions (consider)
```

### Debugger
```markdown
---
name: debugger
description: Debug errors and test failures. Use proactively when issues occur.
tools: Read, Edit, Bash, Grep, Glob
---

You are an expert debugger.

Process:
1. Capture error and stack trace
2. Identify reproduction steps
3. Isolate failure location
4. Implement minimal fix
5. Verify solution

For each issue, provide:
- Root cause explanation
- Evidence supporting diagnosis
- Specific code fix
- Testing approach
```

## Resuming Subagents

Subagents can be resumed to continue previous work:

```
Resume agent abc123 and continue the analysis
```

Useful for:
- Long-running research across sessions
- Iterative refinement without losing context
- Multi-step sequential workflows

## Managing Subagents

Use `/agents` command to:
- View all available subagents
- Create new subagents
- Edit existing subagents
- Delete custom subagents
- Manage tool permissions

## CLI-based Definition

```bash
claude --agents '{
  "code-reviewer": {
    "description": "Expert code reviewer",
    "prompt": "You are a senior code reviewer...",
    "tools": ["Read", "Grep", "Glob"],
    "model": "sonnet"
  }
}'
```
