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

Before acting on an answer line, verify it with `decide show --json`: `state` is `answered` and `answeredBy` is `human`.

Close an item with `decide done` and an outcome. A follow-up is a reply in the existing thread, not a new item. A human reply notifies the owner. `decide reply` posts the agent's progress or result. A reply with options asks a follow-up question. A reply without options is a notification the user can archive.

Reply in the same thread when the user picks "draft a message for me", with the full draft text and options such as `send`, `edit`, and `don't send`. The user answers or requests edits inside decide.

## Status

`decide status set` posts the agent state, and posts a shared resource as free or busy.

## Deadlines

Deadlines are RFC3339.

## Approval

An inbox answer may not count as approval for an outward action, such as posting to chat, writing to a tracker, or merging, when that agent's own check requires a typed approval. File that item as a "type this in tab X" step and include the exact text.
