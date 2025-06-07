---
trigger: model_decision
description: when working on Jira
---

1. Use `jira` CLI tool, where `jira issue` is the most useful command.
2. Use the following command to update Jira description:

   ```sh
   jira issue edit TICKET -b "description"
   ```

   The description does not understand escaped newlines like `\n`. An actual
   newline character should be used in the description.
3. Use the following command to add ticket dependencies when T-1 blocks T-2:

   ```sh
   jira issue link T-1 T-2 Blocks
   ```
