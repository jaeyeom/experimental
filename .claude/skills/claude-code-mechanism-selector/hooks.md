# Hooks Reference

## Overview

Hooks are user-defined shell commands that execute at specific points in Claude Code's lifecycle. They provide **deterministic control** - guaranteed to run every time, not relying on AI decisions.

## When to Use

- Actions that MUST always happen (formatting, logging, validation)
- Security enforcement (block sensitive file edits)
- Notifications (custom alerts when Claude needs input)
- Compliance (audit logging of all commands)
- Automated feedback (lint errors, style violations)

## Configuration Location

Hooks are configured in settings JSON files:
- Project: `.claude/settings.json`
- User: `~/.claude/settings.json`

Use `/hooks` command for interactive configuration.

## Hook Events

| Event | When it Runs | Can Block? |
|-------|--------------|------------|
| `PreToolUse` | Before tool calls | Yes |
| `PostToolUse` | After tool calls complete | No |
| `PermissionRequest` | When permission dialog shows | Yes (allow/deny) |
| `UserPromptSubmit` | When user submits prompt | No |
| `Notification` | When Claude sends notifications | No |
| `Stop` | When Claude finishes responding | No |
| `SubagentStop` | When subagent tasks complete | No |
| `PreCompact` | Before compact operation | No |
| `SessionStart` | When session starts/resumes | No |
| `SessionEnd` | When session ends | No |

## Configuration Format

```json
{
  "hooks": {
    "EventName": [
      {
        "matcher": "ToolPattern",
        "hooks": [
          {
            "type": "command",
            "command": "your-shell-command"
          }
        ]
      }
    ]
  }
}
```

## Matchers

- `Bash` - Match Bash tool only
- `Edit|Write` - Match Edit or Write tools
- `*` - Match all tools
- `""` (empty) - Match all (for events without tools)

## Exit Codes

For `PreToolUse` hooks:
- `0` - Allow tool to proceed
- `2` - Block tool execution (with feedback to Claude)

## Examples

### Auto-format TypeScript
```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | { read f; [[ $f == *.ts ]] && npx prettier --write \"$f\"; }"
          }
        ]
      }
    ]
  }
}
```

### Log All Bash Commands
```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.command' >> ~/.claude/bash-log.txt"
          }
        ]
      }
    ]
  }
}
```

### Block Sensitive File Edits
```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "python3 -c \"import json,sys; d=json.load(sys.stdin); p=d.get('tool_input',{}).get('file_path',''); sys.exit(2 if any(x in p for x in ['.env','secrets','.git/']) else 0)\""
          }
        ]
      }
    ]
  }
}
```

### Custom Notifications
```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "notify-send 'Claude Code' 'Awaiting input'"
          }
        ]
      }
    ]
  }
}
```

### Go Format on Save
```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "jq -r '.tool_input.file_path' | { read f; [[ $f == *.go ]] && gofmt -w \"$f\"; }"
          }
        ]
      }
    ]
  }
}
```

## Input Data

Hooks receive JSON via stdin with context about the event:

```json
{
  "tool_name": "Edit",
  "tool_input": {
    "file_path": "/path/to/file.ts",
    "old_string": "...",
    "new_string": "..."
  }
}
```

Use `jq` to extract fields:
```bash
jq -r '.tool_input.file_path'
jq -r '.tool_input.command'
```

## Security Considerations

- Hooks run with your environment's credentials
- Review all hook implementations before registering
- Be cautious with hooks that process external input
- Don't add sensitive paths to file protection allowlists

## Debugging

1. Use `/hooks` to view current configuration
2. Check `~/.claude/settings.json` for raw config
3. Test hook commands manually with sample JSON input
4. Add logging to hook commands to trace execution
