# Claude Code Extensions Directory

This directory contains Claude Code extensions for this project.

## Creating New Extensions

When creating new skills, commands, agents, or hooks, use the **claude-code-mechanism-selector** skill for guidance. It helps choose the right extension type for your use case.

## Directory Structure

```
.claude/
├── skills/       # Complex capabilities with auto-discovery
├── commands/     # User-invoked slash commands (/command)
├── agents/       # Specialized subagents with isolated context
└── settings.local.json  # Local Claude Code settings
```

## Quick Decision Guide

| Question | If YES | If NO |
|----------|--------|-------|
| Must run deterministically (guaranteed)? | **Hooks** | Continue |
| Needs separate context or specialized AI? | **Subagents** | Continue |
| Should activate automatically by context? | **Skills** | **Slash Commands** |

## When to Use Each

- **Hooks**: Deterministic actions (auto-format, logging, blocking edits)
- **Subagents**: Isolated tasks needing custom AI behavior
- **Skills**: Complex capabilities with auto-discovery
- **Slash Commands**: Quick, explicit user-invoked prompts

See `skills/claude-code-mechanism-selector/SKILL.md` for detailed guidance.
