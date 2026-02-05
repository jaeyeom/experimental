# Claude Code Extensions Documentation

This directory contains reference documentation for Claude Code extension mechanisms.

## Contents

- `hooks-guide.md` - Deterministic shell commands at lifecycle events
- `skills.md` - Model-invoked capabilities with auto-discovery
- `slash-commands.md` - User-invoked commands with `/command` syntax
- `sub-agents.md` - Specialized agents with isolated context

## Related Skill

When working with Claude Code extensions, use the **claude-code-mechanism-selector** skill located at `.claude/skills/claude-code-mechanism-selector/SKILL.md`.

### Relationship

| This Directory | The Skill |
|----------------|-----------|
| Reference documentation | Decision framework |
| Detailed how-to guides | Quick selection guide |
| Source material for learning | Actionable recommendations |

The documentation here provides comprehensive details about each mechanism. The skill helps you quickly choose the right mechanism for your use case based on a simple decision tree.

## Quick Decision Guide

Before diving into the docs, ask:

1. **Must it run deterministically?** → Hooks (`hooks-guide.md`)
2. **Needs separate context?** → Subagents (`sub-agents.md`)
3. **Should activate automatically?** → Skills (`skills.md`)
4. **User explicitly invokes it?** → Slash Commands (`slash-commands.md`)
