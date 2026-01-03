---
description: Convert Org Mode document to Org Tree Slide format. Usage: /org-to-slide <source.org> [output.org]
argument-hint: "<source.org> [output.org]"
---

Convert the Org Mode document to Org Tree Slide presentation format: $ARGUMENTS

Use the `org-slide-converter` skill for conversion rules and constraints.

## Process

1. **Read** the source document
2. **Analyze** sections that exceed slide constraints (~15 content lines, ~70 char width)
3. **Show** the user which sections need splitting with a brief summary:
   - List sections that will be split
   - Note any tables that need reformatting
   - Highlight code blocks that get their own slides
4. **Ask** for confirmation or adjustments before proceeding
5. **Convert** the document following the skill guidelines
6. **Write** to the output file (or `<source>-slides.org` if not specified)

## Key Constraints

- 12-15 content lines per slide (code blocks may slightly exceed)
- ~70 char prose width, ~80 max for tables/code
- 5-6 space indentation (editor has no left margin)
- Use `Cont'd`, `Cont'd (2)` for continuations
- Convert long paragraphs to bullet points
- Use generous vertical spacing (multiple blank lines in title, between items)
- Preserve specific context info (series name, week number, author email)
