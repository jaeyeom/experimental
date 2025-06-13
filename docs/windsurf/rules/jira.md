---
trigger: model_decision
description: when working on Jira
---

1. Use `jira` CLI tool, where `jira issue COMMAND` provides the most useful
   command and it provides help.
2. Use `--plain` option for non-interactive lists.
3. Use the following commands to update Jira ticket descriptions:

   ```sh
   # For simple descriptions (no escape characters):
   jira issue edit TICKET-123 -b "Simple description"

   # For description with code blocks or complex descriptions, use a file:
   jira issue edit TICKET-123 -b "$(cat description.txt)" --no-input
   ```

   Notes:
   - Use actual newlines, not `\n` escape sequences
   - Jira supports code blocks with `{code:language}...{code}` syntax
   - Common language options: java, go, python, javascript, bash, etc.
4. Use the following command to add ticket dependencies when T-1 blocks T-2:

   ```sh
   jira issue link T-1 T-2 Blocks
   ```
5. If you have resolved a Jira ticket, run `rg TICKET` from the repo root to
   search the ticket in the whole repo. If that's a TODO or FIXME comments, you
   may want to delete them or modify them.
