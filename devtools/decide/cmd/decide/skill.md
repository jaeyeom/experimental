---
name: decide
description: "Use decide to ask the user for a decision, send a note, or post status."
---

# decide

## When to use it

Use decide to ask the user for a decision, send a note, or post status.

## Learn the installed CLI

Run `decide` with no arguments for the full usage. The flag list is in that text.

## Items

Every item has options, includes `--rec`, and usually allows `--free-text`.

`--notify-pane` routes the answer to the owning agent's pane.

Use `decide reassign` when the owning tab is retired, or for a handoff to another agent.

Inside herdr, `--owner` can be omitted: it defaults to this tab's label. An explicit `--owner` should match the tab label or one of your agentbus names, because the user finds you by tab.

Before acting on an answer line, verify it with `decide show --json`: `state` is `answered` and `answeredBy` is `human`.

`answer.via` is `ui` for an answer the user gave in the inbox or Emacs, and `cli` for `decide answer` or `decide send --id`. Agents never pass `--via ui` to `decide answer` or `decide send`.

Close an item with `decide done` and an outcome. A follow-up is a reply in the existing thread, not a new item. A human reply notifies the owner. `decide reply` posts the agent's progress or result. A reply with options asks a follow-up question and replaces the pending options. A plain reply on an open question keeps those options and free text, so the user can still answer with an option key. A plain reply that reopens an answered or done item is a notification the user can archive.

`decide ask --owner-replies-only` is for a tool that owns the thread and parses the user's answer, such as an outbox approval. Do not set it on every ask. `decide reply` and `decide send --id` from anyone but that owner are refused: `item <id> accepts replies only from its owner <owner>; message the owner instead`. A human reply in the inbox, and an answer, are still allowed. `decide show --json` includes `ownerRepliesOnly` when it is set. The tool's own reply is accepted only when the recorded sender is that owner.

Reply in the same thread when the user picks "draft a message for me", with the full draft text and options such as `send`, `edit`, and `don't send`. The user answers or requests edits inside decide.

## Write for the inbox

Short lines. A one-line summary first.

Numbered steps or bullets for details.

The exact command run and the exact error, each on its own indented lines.

End an `ask` with a `Your action:` block listing each option key on its own line, with what it does.

Pass multi-line text from a file or from stdin. `"$(cat file)"` also works.

```
decide reply D-123 --text-file /path/to/msg.txt
decide send --to me - <<'EOF'
...
EOF
```

```
Ship the inbox fix

1. The pane redraws on each answer.
2. The list keeps its place.

    decide reply D-1 --text-file msg.txt
    decide: text is empty

Your action:
a: ship it
b: wait
```

## Status

`decide status set` posts the agent state, and posts a shared resource as free or busy.

## Deadlines

Deadlines are RFC3339.

## Approval

An inbox answer may not count as approval for an outward action, such as posting to chat, writing to a tracker, or merging, when that agent's own check requires a typed approval. File that item as a "type this in tab X" step and include the exact text.
