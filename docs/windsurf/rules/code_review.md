# Code Review Rules for AI Agent

## Quick Setup
```bash
# Get PR info
gh pr view <PR_NUMBER> --json title,body,author
gh pr diff <PR_NUMBER>
```

## Key Focus Areas

### Code Quality
- **DRY violations**: Extract duplicated code
- **Error handling**: Proper error wrapping, propagation and logging
- **Resource cleanup**: defer/RAII patterns
- **Clear naming**: Functions and variables

### Language-Specific
- **Go**: Error handling, goroutine safety
- **Python**: PEP 8, type hints
- **C++**: RAII, const correctness
- **Protobuf**: Backward compatibility

## Creating Reviews
```bash
# Draft review via API
gh api repos/bearrobotics/pennybot/pulls/<PR_NUMBER>/reviews \
  --method POST \
  --input review.json
```

**Note**: Use line numbers from the **new file**, not diff lines.

## Best Practices
1. Be constructive and specific
2. Focus on significant issues
3. Acknowledge good practices
4. Don't nitpick or scope creep
