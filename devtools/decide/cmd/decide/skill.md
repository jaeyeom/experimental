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

Close an item with `decide done` and an outcome. File a new item for a follow-up, and leave the closed item closed.

## Status

`decide status set` posts the agent state, and posts a shared resource as free or busy.

## Deadlines

Deadlines are RFC3339.

## Approval

An inbox answer may not count as approval for an outward action, such as posting to chat, writing to a tracker, or merging, when that agent's own check requires a typed approval. File that item as a "type this in tab X" step and include the exact text.
