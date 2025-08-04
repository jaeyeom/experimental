package models

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// diffOpRegex matches classic diff format operations like "15,17d14" or "30a31,33".
var diffOpRegex = regexp.MustCompile(`^(\d+)(?:,(\d+))?([adc])(\d+)(?:,(\d+))?$`)

// ParseDiffSpec parses a classic diff format specification into LineAdjustment structs.
// Supports formats like:
// - "15,17d14" - Delete lines 15-17
// - "30a31,33" - After line 30, insert lines 31-33
// - "5,7c6,8" - Replace lines 5-7 with lines 6-8
// - "15,17d14;30a31,33" - Multiple operations separated by semicolons.
func ParseDiffSpec(spec string) ([]LineAdjustment, error) {
	if spec == "" {
		return nil, fmt.Errorf("empty diff spec")
	}

	var adjustments []LineAdjustment

	// Split by semicolon for multiple operations
	operations := strings.Split(spec, ";")

	for _, op := range operations {
		op = strings.TrimSpace(op)
		if op == "" {
			continue
		}

		matches := diffOpRegex.FindStringSubmatch(op)
		if matches == nil {
			return nil, fmt.Errorf("invalid diff format: %s", op)
		}

		// Parse line numbers
		oldStart, err := strconv.Atoi(matches[1])
		if err != nil {
			return nil, fmt.Errorf("invalid old start line: %s", matches[1])
		}

		oldEnd := oldStart
		if matches[2] != "" {
			oldEnd, err = strconv.Atoi(matches[2])
			if err != nil {
				return nil, fmt.Errorf("invalid old end line: %s", matches[2])
			}
		}

		operation := matches[3]

		newStart, err := strconv.Atoi(matches[4])
		if err != nil {
			return nil, fmt.Errorf("invalid new start line: %s", matches[4])
		}

		newEnd := newStart
		if matches[5] != "" {
			newEnd, err = strconv.Atoi(matches[5])
			if err != nil {
				return nil, fmt.Errorf("invalid new end line: %s", matches[5])
			}
		}

		operationType, err := ParseOperationType(operation)
		if err != nil {
			return nil, fmt.Errorf("unknown operation: %s", operation)
		}

		adj := LineAdjustment{
			Operation: operationType,
			OldStart:  oldStart,
			OldEnd:    oldEnd,
			NewStart:  newStart,
			NewEnd:    newEnd,
		}

		adjustments = append(adjustments, adj)
	}

	if len(adjustments) == 0 {
		return nil, fmt.Errorf("no valid operations found in diff spec")
	}

	return adjustments, nil
}

// AdjustLineNumber applies a series of line adjustments to a given line number.
// Returns the new line number and whether the line is still valid (not deleted).
func AdjustLineNumber(line int, adjustments []LineAdjustment) (int, bool) {
	currentLine := line

	for _, adj := range adjustments {
		switch adj.Operation {
		case OperationDelete:
			if currentLine >= adj.OldStart && currentLine <= adj.OldEnd {
				// Line is deleted
				return -1, false
			}
			if currentLine > adj.OldEnd {
				// Line is after deletion, shift up
				deletedLines := adj.OldEnd - adj.OldStart + 1
				currentLine -= deletedLines
			}

		case OperationInsert:
			if currentLine > adj.OldStart {
				// Line is after insertion point, shift down
				insertedLines := adj.NewEnd - adj.NewStart + 1
				currentLine += insertedLines
			}

		case OperationChange:
			if currentLine >= adj.OldStart && currentLine <= adj.OldEnd {
				// Line is within changed range
				// Map it proportionally to the new range
				oldOffset := currentLine - adj.OldStart
				newLine := adj.NewStart + oldOffset

				// Make sure we don't exceed the new range
				if newLine > adj.NewEnd {
					newLine = adj.NewEnd
				}
				currentLine = newLine
			} else if currentLine > adj.OldEnd {
				// Line is after change, adjust by difference
				oldLines := adj.OldEnd - adj.OldStart + 1
				newLines := adj.NewEnd - adj.NewStart + 1
				diff := newLines - oldLines
				currentLine += diff
			}
		}
	}

	return currentLine, true
}

// AdjustComment adjusts a comment's line numbers based on the given adjustments.
// Returns whether the comment is still valid (not on deleted lines).
func AdjustComment(comment *Comment, adjustments []LineAdjustment) bool {
	// Save original line numbers if not already saved
	if comment.OriginalLine == 0 {
		comment.OriginalLine = comment.Line
		if comment.StartLine != nil {
			origStart := *comment.StartLine
			comment.OriginalStartLine = &origStart
		}
	}

	// Adjust the main line number
	newLine, isValid := AdjustLineNumber(comment.Line, adjustments)
	if !isValid {
		return false
	}
	comment.Line = newLine

	// Adjust start line for multi-line comments
	if comment.StartLine != nil {
		newStartLine, isStartValid := AdjustLineNumber(*comment.StartLine, adjustments)
		if !isStartValid {
			return false
		}
		comment.StartLine = &newStartLine
	}

	// Update adjustment history
	for _, adj := range adjustments {
		adj.AppliedAt = time.Now()
		comment.AdjustmentHistory = append(comment.AdjustmentHistory, adj)
	}

	return true
}

// ValidateAdjustmentAgainstDiff validates that adjusted comment lines are within diff hunks.
func ValidateAdjustmentAgainstDiff(comment Comment, adjustments []LineAdjustment, diffHunks []DiffHunk) error {
	// First check if file has diff hunks
	hasFileHunks := false
	for _, hunk := range diffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			hasFileHunks = true
			break
		}
	}
	if !hasFileHunks {
		return fmt.Errorf("no diff hunks for file %s", comment.Path)
	}

	// If comment is already adjusted, use its line directly
	// Otherwise, create a copy to test adjustments
	var lineToCheck int
	if len(comment.AdjustmentHistory) > 0 {
		// Comment is already adjusted, just check its current line
		lineToCheck = comment.Line
	} else {
		// Create a copy to test adjustments
		testComment := comment
		if !AdjustComment(&testComment, adjustments) {
			return fmt.Errorf("comment on deleted line")
		}
		lineToCheck = testComment.Line
	}

	// Check if adjusted line is within any diff hunk
	inHunk := false
	for _, hunk := range diffHunks {
		if hunk.File == comment.Path && hunk.Side == comment.Side {
			if lineToCheck >= hunk.StartLine && lineToCheck <= hunk.EndLine {
				inHunk = true
				break
			}
		}
	}

	if !inHunk {
		return fmt.Errorf("adjusted line %d outside diff hunks", lineToCheck)
	}

	return nil
}

// FormatAdjustmentDescription creates a human-readable description of an adjustment.
func (adj LineAdjustment) FormatDescription() string {
	switch adj.Operation {
	case OperationDelete:
		if adj.OldStart == adj.OldEnd {
			return fmt.Sprintf("Delete line %d", adj.OldStart)
		}
		return fmt.Sprintf("Delete lines %d-%d", adj.OldStart, adj.OldEnd)
	case OperationInsert:
		count := adj.NewEnd - adj.NewStart + 1
		if count == 1 {
			return fmt.Sprintf("Insert 1 line after line %d", adj.OldStart)
		}
		return fmt.Sprintf("Insert %d lines after line %d", count, adj.OldStart)
	case OperationChange:
		oldCount := adj.OldEnd - adj.OldStart + 1
		newCount := adj.NewEnd - adj.NewStart + 1
		if oldCount == 1 && newCount == 1 {
			return fmt.Sprintf("Change line %d", adj.OldStart)
		}
		return fmt.Sprintf("Replace lines %d-%d with %d lines", adj.OldStart, adj.OldEnd, newCount)
	default:
		return adj.DiffSpec()
	}
}
