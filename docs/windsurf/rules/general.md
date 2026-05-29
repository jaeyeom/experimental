---
trigger: always_on
---

## General Rules
1. Practice Test Driven Development. Try to understand the desired behavior first. Then, unit tests should be updated according to the new behavior. After that, run the tests to see them fail and then implement the new behavior until the tests pass. Do not fear to delete obsolete test code. You do not need to ask my confirmation to run tests, since frequent test running is a part of the development process. Do not test implementation details.
2. All exported identifiers should be documented and package-level comments should be provided.
3. Do not log errors and bubble them up. That will cause duplicate logs. If an error bubbles up or is returned, it will be handled by the callers.
4. Use org-mode’s TODO feature to track progress in a master epic document and sub documents, listing tasks that can be checked off as the project advances. Keep detailed design documentation separate to maintain readability.
5. Keep the design doc focused on the high-level design instead of adding too many details.
6. Simplicity: "Always prioritize the simplest solution over complexity."
7. No Duplication: "Avoid repeating code; reuse existing functionality when possible."
8. Organization: "Keep files concise, under 200-300 lines; refactor as needed."
9. Principles: "Follow SOLID principles (e.g., single responsibility, dependency inversion) where applicable."
10. Guardrails: "Never use mock data in dev or prod—restrict it to tests."
11. Efficiency: "Optimize outputs to minimize token usage without sacrificing clarity."
12. Before working on the project, read `README`, `README.md` or `README.org` file in the same directory to understand the background of the project.

## API and Configuration Best Practices
1. Verify API documentation and configuration details against authoritative, up-to-date sources before generating code. A real-time search-enabled tool (e.g. Perplexity AI) is one way to do this when the answer is uncertain.
2. If you encounter ambiguous or outdated documentation, prompt the user for confirmation or consult a real-time search tool to clarify.
3. Prefer APIs and configurations that are explicitly supported by up-to-date, publicly available documentation.

## Git Specific Rules
1. Check commit log template with `git config commit.template` before writing a commit log.

## Terminal Tasks
1. When you need to create a temporary file for command-line work, use the `mktemp` command for secure, unique file creation.
