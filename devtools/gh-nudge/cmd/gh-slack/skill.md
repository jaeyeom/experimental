---
name: gh-slack
description: CLI for sending Slack messages to GitHub users using gh-nudge user mappings.
---

# gh-slack

`gh-slack` sends Slack messages to GitHub users based on the Slack user and DM mappings in `~/.config/gh-nudge/config.{pkl,yaml}`.

Common usage:
- `gh-slack --to githubuser --message "Hello"`
- `gh-slack --to user1,user2 --template "Hi {name}" --var "name=team"`
