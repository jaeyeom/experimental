# Code Review Rules and Guidelines

## Overview
This document provides instructions for conducting code reviews on the pennybot repository. Follow these guidelines to ensure consistent, high-quality reviews that align with Bear Robotics' engineering standards.

## Review Process

### 1. Fetching PR Information
When reviewing a PR, use the following commands to gather information:
```bash
# Get PR details including title, body, and author
gh pr view <PR_NUMBER> --json title,body,author

# Get the diff
gh pr diff <PR_NUMBER>

# For large diffs, save to file for analysis
gh pr diff <PR_NUMBER> > /tmp/pr_diff.txt
```

### 2. Review Categories
Based on the repository's code review guide, categorize the PR as:
- **New Code (Default)**: New features or functionality
- **Refactoring**: Code restructuring without behavior changes
- **Bug Fixes**: Fixing existing issues
- **Revert Change**: Reverting previous commits

### 3. Review Focus Areas

#### For All PRs:
- **Commit Message Format**: Must include Jira ticket ID (e.g., [PS-1234], [PREM-294])
- **Code Style**: Follow language-specific style guides (C++, Go, Python, Protobuf)
- **Testing**: Ensure adequate test coverage
- **Documentation**: Update relevant documentation if needed

#### Language-Specific Checks:
- **Go**: Check for proper error handling, goroutine safety, and idiomatic code
- **Python**: Verify PEP 8 compliance, type hints where applicable
- **C++**: Ensure RAII principles, const correctness, and proper memory management
- **Protobuf**: Validate backward compatibility and field numbering

### 4. Common Review Points

#### Code Quality:
- DRY (Don't Repeat Yourself) principle violations
- Proper error handling and propagation
- Resource cleanup (defer statements, RAII)
- Thread safety and race conditions
- Clear variable and function naming

#### Architecture:
- Separation of concerns
- Proper abstraction levels
- Interface design
- Dependency management

#### Performance:
- Efficient algorithms and data structures
- Avoiding unnecessary allocations
- Proper use of concurrency
- Database query optimization

### 5. Creating Review Comments

#### Using GitHub REST API:
```bash
# Create a draft review (pending)
gh api repos/bearrobotics/pennybot/pulls/<PR_NUMBER>/reviews \
  --method POST \
  --input review.json

# Where review.json contains:
{
  "body": "Overall review summary",
  "comments": [
    {
      "path": "path/to/file.go",
      "line": 123,
      "body": "Review comment text"
    }
  ]
}
```

**Note**: Omitting the `event` field creates a draft review. Valid `event` values are:
- `"COMMENT"`: Submit as comments
- `"APPROVE"`: Approve the PR
- `"REQUEST_CHANGES"`: Request changes

#### Line Numbers:
- Use line numbers from the **new file content**, not the diff line numbers
- For moved code, comment on the new location

### 6. Review Comment Templates

#### DRY Violation:
```
The [functionality] logic is duplicated between [location1] and [location2]. Consider extracting this into a separate method/function to follow DRY principles.
```

#### Missing Error Handling:
```
This function doesn't handle the error case when [condition]. Consider adding proper error handling:
- Return the error to the caller
- Set appropriate status/state
- Log the error with context
```

#### TODO Comments:
```
The TODO comment indicates missing [functionality]. This could lead to [consequence]. Consider:
1. Implementing the missing logic
2. Adding a timeout mechanism
3. Creating a follow-up ticket if this is non-critical
```

#### Context Usage:
```
Using context.Background() breaks context propagation. Consider:
- Passing the context from the caller
- Storing context in the struct if needed for callbacks
- Using context.WithTimeout for operations with deadlines
```

### 7. Positive Feedback
Always acknowledge good practices:
- Comprehensive test coverage
- Clear documentation
- Well-structured code
- Performance optimizations
- Security considerations

### 8. Review Checklist

- [ ] PR has appropriate Jira ticket reference
- [ ] Code follows style guidelines
- [ ] Tests are included and pass
- [ ] No obvious security vulnerabilities
- [ ] Performance impact is acceptable
- [ ] Documentation is updated if needed
- [ ] Breaking changes are clearly marked
- [ ] Dependencies are properly managed

## Repository-Specific Rules

### CODEOWNERS
The repository uses CODEOWNERS for automatic reviewer assignment. Respect the ownership structure and involve appropriate teams when needed.

### Readability Reviews
Language-specific readability reviewers are defined in CODEOWNERS:
- Go: go-readability team
- Python: python-readability team
- C++: cpp-readability team

### Pre-submit Checks
Ensure code passes PRESUBMIT.sh checks before approval:
- No disallowed files
- Proper formatting (via bearlinters.yaml)
- Build succeeds
- Tests pass

## Best Practices

1. **Be Constructive**: Focus on improving the code, not criticizing the author
2. **Be Specific**: Provide concrete suggestions and examples
3. **Be Timely**: Review PRs promptly to avoid blocking development
4. **Be Thorough**: But also be pragmatic about minor issues
5. **Be Educational**: Help team members learn and grow

## Common Pitfalls to Avoid

1. **Nitpicking**: Focus on significant issues over style preferences
2. **Scope Creep**: Don't request unrelated improvements
3. **Assumption**: Ask clarifying questions instead of assuming intent
4. **Perfectionism**: Good enough is often better than perfect
5. **Personal Preferences**: Distinguish between team standards and personal style

## Tools and Resources

- GitHub CLI: `gh pr` commands for PR management
- Bear linters: Automated style checking
- Code review guide: `/docs/gdr/docs/eng_docs/guides/processes/code-review-guide.md`
- Style guides: `/docs/gdr/docs/eng_docs/guides/style_guides/`

## Review Metrics

Track these metrics to improve review quality:
- Review turnaround time
- Number of review iterations
- Post-merge defect rate
- Review comment resolution rate

Remember: Code reviews are a collaborative process aimed at improving code quality and sharing knowledge across the team.