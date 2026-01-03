# Org Tree Slide Converter Skill

Convert Emacs Org Mode documents to Org Tree Slide presentation format.

## When to Use

Use this skill when asked to convert an Org Mode document to slide format, or
when the user mentions "org slide", "tree slide", "presentation", or "convert to
slides".

## Slide Constraints

These constraints are optimized for Org Tree Slide with enlarged/zoomed fonts:

| Constraint              | Value                                    |
|-------------------------|------------------------------------------|
| Content lines per slide | 12-15 lines (excluding blank lines)      |
| Prose width             | ~70 characters (target)                  |
| Max width (tables/code) | ~80 characters                           |
| Indentation             | 5-6 spaces (no left margin in editor)    |
| Continuation pattern    | `Cont'd`, `Cont'd (2)`, `Cont'd (3)` ... |

**Note**: Code blocks may slightly exceed 15 lines if splitting would break
readability. This is acceptable.

## Conversion Rules

### 1. Title Section Transformation

**From:**
```org
#+title: Document Title
#+SUBTITLE: The Subtitle Here
#+AUTHOR: Author Name
```

**To:**
```org
#+title: Document Title/Slides

* Document Title


         *My Cast Series: Week 2*


      /The Subtitle Here/

      /Secondary Subtitle if Present/




      Author Name <email@example.com>




```

**Key points:**
- Use generous vertical spacing (multiple blank lines) for visual breathing room
- Preserve specific context info from the original (e.g., "Week 2", series name)
- Use 6-space indentation for title slide content
- Add blank lines between each element and extra blank lines at the end

### 2. Content Density

- Convert long paragraphs to bullet points
- Use 5-6 space indentation for visual hierarchy (editor has no left margin)
- Add blank lines for breathing room between items
- Keep each point concise and scannable
- Use extra blank lines between major points for visual separation

### 3. Section Splitting

When a section exceeds ~15 content lines, split it:

```org
**** Original Section Title
      [First 12-15 lines of content]

***** Cont'd
      [Next portion]

***** Cont'd (2)
      [More content if needed]
```

**Exception**: Code blocks may slightly exceed 15 lines if they form a logical
unit that shouldn't be split.

### 4. Code Blocks

- Code blocks often get their own continuation slide
- Preserve code formatting exactly
- Add a continuation slide if code + explanation is too long

### 5. Tables

- Reformat tables to fit ~80 char width
- Abbreviate column content if needed
- Use shorter column headers

### 6. Heading Level Mapping

| Original | Slide Format | Purpose              |
|----------|--------------|----------------------|
| `*`      | `*`          | Title slide          |
| `**`     | `**`         | Major section        |
| `***`    | `***`        | Sub-section          |
| `****`   | `****`       | Individual slide     |
| -        | `*****`      | Continuation slides  |

## Interactive Workflow

1. **Analyze**: Read the source document and identify sections that need splitting
2. **Propose**: Show the user which sections will be split and how
3. **Convert**: Transform the document section by section
4. **Review**: Present the output for user feedback
5. **Refine**: Adjust based on user preferences

## Example Transformation

**Original (too long for one slide):**
```org
** Code Review Benefits
Code reviews help catch bugs early. They also spread knowledge across the team.
When done well, reviews improve code quality significantly. The reviewer learns
about new parts of the codebase while the author gets fresh perspectives.
Reviews also enforce coding standards and best practices. They create
documentation through the discussion. Reviews build team cohesion and trust.
Additionally, they reduce the bus factor by ensuring multiple people understand
each change. Reviews can also mentor junior developers effectively.
```

**Converted:**
```org
** Code Review Benefits

      - Catch bugs early

      - Spread knowledge across the team

      - Improve code quality

      - Fresh perspectives for authors

*** Cont'd

      - Enforce coding standards

      - Create documentation through discussion

      - Build team cohesion and trust

      - Reduce bus factor

      - Mentor junior developers
```

Note the 6-space indentation and blank lines between bullet points.

## User Preferences

Ask the user about these preferences if not specified:

1. **Verbosity**: Keep detailed explanations or condense aggressively?
2. **Code handling**: Keep all code or summarize long examples?
3. **Series context**: Is this part of a series? What week/part number?

## Preserving Context

Always preserve specific context information from the original document:

- Series name and part/week number (e.g., "Engineering Culture Series: Week 2")
- Author name and email if present
- Any references to previous or upcoming sessions
- Project-specific terminology and naming

Do NOT generalize specific information like "Week 2" to "Part of a series".
