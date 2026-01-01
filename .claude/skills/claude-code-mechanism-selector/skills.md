# Skills Reference

## Overview

Skills are model-invoked capabilities. Claude automatically discovers and uses them based on your request and the skill's description.

## When to Use

- Complex workflows with multiple steps
- Capabilities requiring scripts or utilities
- Knowledge organized across multiple files
- Team workflows to standardize
- Automatic activation based on context

## File Locations

| Type | Location | Scope |
|------|----------|-------|
| Project | `.claude/skills/skill-name/` | Current project |
| Personal | `~/.claude/skills/skill-name/` | All projects |

## Directory Structure

```
my-skill/
├── SKILL.md          (required - main instructions)
├── reference.md      (optional - detailed docs)
├── examples.md       (optional - usage examples)
├── scripts/
│   └── helper.py     (optional - utilities)
└── templates/
    └── template.txt  (optional - templates)
```

## SKILL.md Format

```yaml
---
name: your-skill-name
description: What it does and when to use it. Include trigger keywords.
allowed-tools: Read, Grep, Glob  # Optional - restricts available tools
---

# Your Skill Name

## Instructions
Step-by-step guidance for Claude.

## Examples
Concrete usage examples.
```

## Frontmatter Options

| Field | Required | Purpose |
|-------|----------|---------|
| `name` | Yes | Lowercase letters, numbers, hyphens (max 64 chars) |
| `description` | Yes | What it does + when to use (max 1024 chars) |
| `allowed-tools` | No | Restrict tools (inherits all if omitted) |

## Description Best Practices

The description is critical for discovery. Include:
- What the skill does
- When Claude should use it
- Trigger keywords users might mention

**Bad**: `description: Helps with documents`

**Good**: `description: Extract text and tables from PDF files, fill forms, merge documents. Use when working with PDF files or when the user mentions PDFs, forms, or document extraction.`

## Examples

### Simple Skill (Single File)
```yaml
---
name: commit-helper
description: Generate commit messages from git diffs. Use when writing commits.
---

# Commit Helper

1. Run `git diff --staged`
2. Generate message with:
   - Summary under 50 chars
   - Detailed description
   - Affected components
```

### Skill with Tool Restrictions
```yaml
---
name: code-reviewer
description: Review code for quality and issues. Use for code review or PR analysis.
allowed-tools: Read, Grep, Glob
---

# Code Reviewer
Read-only code analysis without modifications.
```

### Multi-File Skill
```
pdf-processing/
├── SKILL.md
├── FORMS.md
├── REFERENCE.md
└── scripts/
    └── fill_form.py
```

Reference other files from SKILL.md:
```markdown
For form filling, see [FORMS.md](FORMS.md).
```

Claude loads additional files only when needed (progressive disclosure).

## Testing Skills

Test by asking questions that match your description:
```
Can you help me extract text from this PDF?
```

If the description mentions "PDF files", Claude should automatically use the skill.

## Debugging

1. Check description is specific enough
2. Verify YAML syntax (opening/closing `---`)
3. Confirm file location is correct
4. Run `claude --debug` to see loading errors
