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

// simpleMappingRegex matches simple mapping format operations like "15:-2" or "30:+3".
var simpleMappingRegex = regexp.MustCompile(`^\s*(\d+)\s*:\s*([+-]?\d+)\s*$`)

// relaxedMappingRegex allows more formats for better error messages.
var relaxedMappingRegex = regexp.MustCompile(`^\s*([^:]+)\s*:\s*(.+)\s*$`)

// unifiedDiffHunkHeaderRegex matches unified diff hunk headers like "@@ -1,7 +1,6 @@".
var unifiedDiffHunkHeaderRegex = regexp.MustCompile(`^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@`)

// FormatType represents different diff format types.
type FormatType int

const (
	FormatUnknown FormatType = iota
	FormatClassicDiff
	FormatSimpleMapping
	FormatUnifiedDiff
)

// LineAdjustment represents a line number adjustment operation.
type LineAdjustment struct {
	Operation   OperationType `json:"operation"`   // Type of operation (delete, insert, change)
	OldStart    int           `json:"oldStart"`    // Start line in original file
	OldEnd      int           `json:"oldEnd"`      // End line in original file
	NewStart    int           `json:"newStart"`    // Start line in new file
	NewEnd      int           `json:"newEnd"`      // End line in new file
	AppliedAt   time.Time     `json:"appliedAt"`   // When adjustment was applied
	Description string        `json:"description"` // Human-readable description
}

// DiffSpec returns a classic diff format representation of the adjustment.
// Returns format like "15,17d14" for deletions, "14a15,17" for insertions, "15,17c14,16" for changes.
func (adj LineAdjustment) DiffSpec() string {
	// Validate operation type
	switch adj.Operation {
	case OperationDelete, OperationInsert, OperationChange:
		// Valid operations, continue
	default:
		return ""
	}
	// Format old range
	oldRange := ""
	if adj.OldStart == adj.OldEnd {
		oldRange = fmt.Sprintf("%d", adj.OldStart)
	} else {
		oldRange = fmt.Sprintf("%d,%d", adj.OldStart, adj.OldEnd)
	}
	// Format new range
	newRange := ""
	if adj.NewStart == adj.NewEnd {
		newRange = fmt.Sprintf("%d", adj.NewStart)
	} else {
		newRange = fmt.Sprintf("%d,%d", adj.NewStart, adj.NewEnd)
	}
	// Combine with operation character
	return fmt.Sprintf("%s%s%s", oldRange, adj.Operation, newRange)
}

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
	if comment.OriginalRange == nil {
		origRange := comment.Line
		comment.OriginalRange = &origRange
	}

	// Adjust the end line
	newEndLine, isValid := AdjustLineNumber(comment.Line.EndLine, adjustments)
	if !isValid {
		return false
	}

	// Adjust start line
	newStartLine, isStartValid := AdjustLineNumber(comment.Line.StartLine, adjustments)
	if !isStartValid {
		return false
	}

	// Update the range
	comment.Line = LineRange{StartLine: newStartLine, EndLine: newEndLine}

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
		if hunk.Location.Path == comment.Path && hunk.Side == comment.Side {
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
		lineToCheck = comment.Line.EndLine
	} else {
		// Create a copy to test adjustments
		testComment := comment
		if !AdjustComment(&testComment, adjustments) {
			return fmt.Errorf("comment on deleted line")
		}
		lineToCheck = testComment.Line.EndLine
	}

	// Check if adjusted line is within any diff hunk
	inHunk := false
	for _, hunk := range diffHunks {
		if hunk.Location.Path == comment.Path && hunk.Side == comment.Side {
			if hunk.IsInRange(lineToCheck) {
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

// FormatDescription creates a human-readable description of an adjustment.
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

// DetectFormat auto-detects the format type of a diff specification.
func DetectFormat(diffSpec string) FormatType {
	if diffSpec == "" {
		return FormatUnknown
	}

	// Check for unified diff format first (multi-line format)
	if detectUnifiedDiffFormat(diffSpec) {
		return FormatUnifiedDiff
	}

	// Normalize whitespace and split by semicolon for single-line formats
	operations := strings.Split(diffSpec, ";")

	hasSimpleMapping := false
	hasClassicDiff := false

	for _, op := range operations {
		op = strings.TrimSpace(op)
		if op == "" {
			continue
		}

		// Check for simple mapping format (line:offset)
		switch {
		case simpleMappingRegex.MatchString(op):
			hasSimpleMapping = true
		case diffOpRegex.MatchString(op):
			// Check for classic diff format (line[,line]op[line[,line]])
			hasClassicDiff = true
		default:
			// Invalid format found
			return FormatUnknown
		}
	}

	// Mixed formats are not allowed
	if hasSimpleMapping && hasClassicDiff {
		return FormatUnknown
	}

	if hasSimpleMapping {
		return FormatSimpleMapping
	}

	if hasClassicDiff {
		return FormatClassicDiff
	}

	return FormatUnknown
}

// detectUnifiedDiffFormat checks if the input appears to be a unified diff format.
func detectUnifiedDiffFormat(diffSpec string) bool {
	lines := strings.Split(diffSpec, "\n")

	// Look for unified diff markers
	hasFileHeaders := false
	hasHunkHeaders := false

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		// File headers like "diff --git a/file b/file" or "--- a/file"
		if strings.HasPrefix(line, "diff --git") ||
			strings.HasPrefix(line, "---") ||
			strings.HasPrefix(line, "+++") {
			hasFileHeaders = true
		}

		// Hunk headers like "@@ -1,7 +1,6 @@"
		if unifiedDiffHunkHeaderRegex.MatchString(line) {
			hasHunkHeaders = true
		}

		// If we have both file headers and hunk headers, it's likely unified diff
		if hasFileHeaders && hasHunkHeaders {
			return true
		}
	}

	// Also accept minimal unified diff (just hunk headers without file headers)
	return hasHunkHeaders
}

// ParseSimpleMappingSpec parses a simple mapping format specification into LineAdjustment structs.
// Simple mapping format: "line:offset;line:offset"
// Examples: "15:-2" (delete 2 lines at 15), "30:+3" (insert 3 lines before 30).
//
// IMPORTANT: All line numbers in simple mapping format refer to the ORIGINAL file,
// NOT the current state after previous operations. This means:
//   - "10:-1;20:+2" deletes line 10 from original file, then inserts before original line 20
//   - The operations are applied logically as if viewing the original file
//   - This differs from sequential application where line 20 might have moved after the first operation
func ParseSimpleMappingSpec(spec string) ([]LineAdjustment, error) {
	if spec == "" {
		return nil, fmt.Errorf("empty mapping")
	}

	// First, convert to classic diff format and then parse that
	// This ensures consistent behavior between both formats
	classicDiff, err := ConvertSimpleMappingToClassicDiff(spec)
	if err != nil {
		return nil, err
	}

	if classicDiff == "" {
		return nil, fmt.Errorf("no valid operations found in mapping spec")
	}

	return ParseDiffSpec(classicDiff)
}

// ConvertSimpleMappingToClassicDiff converts simple mapping format to classic diff format.
func ConvertSimpleMappingToClassicDiff(mapping string) (string, error) {
	if mapping == "" {
		return "", fmt.Errorf("empty mapping")
	}

	operations := strings.Split(mapping, ";")
	var parts []string
	lineOffset := 0 // Track cumulative changes to the new file line numbering

	for _, op := range operations {
		op = strings.TrimSpace(op)
		if op == "" {
			continue
		}

		matches := relaxedMappingRegex.FindStringSubmatch(op)
		if matches == nil {
			return "", fmt.Errorf("invalid simple mapping format: %s", op)
		}

		lineNum, err := strconv.Atoi(strings.TrimSpace(matches[1]))
		if err != nil {
			return "", fmt.Errorf("invalid line number: %s", matches[1])
		}
		if lineNum <= 0 {
			return "", fmt.Errorf("line number must be positive: %d", lineNum)
		}

		offset, err := strconv.Atoi(strings.TrimSpace(matches[2]))
		if err != nil {
			return "", fmt.Errorf("invalid offset: %s", matches[2])
		}

		// Skip zero offset operations
		if offset == 0 {
			continue
		}

		var part string

		if offset < 0 {
			// Deletion: "line:-n" means delete n lines starting at line
			deleteCount := -offset

			if deleteCount == 1 {
				// Single line deletion: "15d14"
				part = fmt.Sprintf("%dd%d", lineNum, lineNum+lineOffset-1)
			} else {
				// Range deletion: "15,17d14"
				part = fmt.Sprintf("%d,%dd%d", lineNum, lineNum+deleteCount-1, lineNum+lineOffset-1)
			}

			// Update offset for subsequent operations
			lineOffset -= deleteCount
		} else {
			// Insertion: "line:+n" means insert n lines before line
			insertCount := offset

			if insertCount == 1 {
				// Single line insertion: "29a30"
				part = fmt.Sprintf("%da%d", lineNum-1, lineNum)
			} else {
				// Range insertion: "29a30,32"
				part = fmt.Sprintf("%da%d,%d", lineNum-1, lineNum+lineOffset, lineNum+lineOffset+insertCount-1)
			}

			// Update offset for subsequent operations
			lineOffset += insertCount
		}

		parts = append(parts, part)
	}

	if len(parts) == 0 {
		return "", nil
	}

	return strings.Join(parts, ";"), nil
}

// ParseUnifiedDiffSpec parses a unified diff format specification into LineAdjustment structs.
// Converts unified diff format (git diff output) to classic diff format, then parses that.
func ParseUnifiedDiffSpec(spec string) ([]LineAdjustment, error) {
	classicDiff, err := ConvertUnifiedDiffToClassicDiff(spec)
	if err != nil {
		return nil, err
	}

	if classicDiff == "" {
		return nil, fmt.Errorf("no valid operations found in unified diff")
	}

	return ParseDiffSpec(classicDiff)
}

// ConvertUnifiedDiffToClassicDiff converts a unified diff format to classic diff format.
// This is the main function that implements Phase 3 functionality.
func ConvertUnifiedDiffToClassicDiff(unifiedDiff string) (string, error) {
	multiFileSpecs, err := ConvertUnifiedDiffToMultiFileClassicDiff(unifiedDiff)
	if err != nil {
		return "", err
	}

	// Combine all files into a single spec (for backward compatibility)
	var allOps []string
	for _, spec := range multiFileSpecs {
		if spec.ClassicDiff != "" {
			allOps = append(allOps, spec.ClassicDiff)
		}
	}

	if len(allOps) == 0 {
		return "", fmt.Errorf("no valid operations found in unified diff")
	}

	return strings.Join(allOps, ";"), nil
}

// FileAdjustmentSpec represents the adjustment specification for a single file.
type FileAdjustmentSpec struct {
	FilePath    string
	ClassicDiff string
}

// ConvertUnifiedDiffToMultiFileClassicDiff converts a unified diff to per-file classic diff specs.
// This enables multi-file processing support.
func ConvertUnifiedDiffToMultiFileClassicDiff(unifiedDiff string) ([]FileAdjustmentSpec, error) {
	if unifiedDiff == "" {
		return nil, fmt.Errorf("empty unified diff")
	}

	parser := &unifiedDiffParser{}
	return parser.parse(unifiedDiff)
}

// unifiedDiffParser handles parsing unified diff format into classic diff specifications.
type unifiedDiffParser struct {
	fileSpecs   []FileAdjustmentSpec
	currentFile string
	currentOps  []string
	inHunk      bool
	oldPos      int
	newPos      int
	hunkChanges []hunkChange
}

// hunkChange represents a change within a unified diff hunk.
type hunkChange struct {
	changeType string // "delete", "insert", "context"
	oldLineNum int
	newLineNum int
}

// parse processes the unified diff and returns file adjustment specifications.
func (p *unifiedDiffParser) parse(unifiedDiff string) ([]FileAdjustmentSpec, error) {
	lines := strings.Split(unifiedDiff, "\n")

	for _, line := range lines {
		originalLine := line
		line = strings.TrimSpace(line)

		if line == "" {
			continue
		}

		if err := p.processLine(line, originalLine); err != nil {
			return nil, err
		}
	}

	// Process final hunk and file
	p.finalizeCurrentHunk()
	p.finalizeCurrentFile()

	if len(p.fileSpecs) == 0 {
		return nil, fmt.Errorf("no valid operations found in unified diff")
	}

	return p.fileSpecs, nil
}

// processLine handles a single line from the unified diff.
func (p *unifiedDiffParser) processLine(line, originalLine string) error {
	switch {
	case strings.HasPrefix(line, "diff --git"):
		return p.handleGitDiffHeader(line)
	case strings.HasPrefix(line, "---"), strings.HasPrefix(line, "+++"):
		return p.handleFilePathHeader(line)
	case unifiedDiffHunkHeaderRegex.MatchString(line):
		return p.handleHunkHeader(line)
	case p.inHunk:
		return p.handleHunkContentLine(originalLine)
	default:
		return nil
	}
}

// handleGitDiffHeader processes "diff --git a/file b/file" headers.
func (p *unifiedDiffParser) handleGitDiffHeader(line string) error {
	p.finalizeCurrentHunk()
	p.finalizeCurrentFile()

	// Extract file path from git diff header
	parts := strings.Fields(line)
	if len(parts) >= 4 {
		p.currentFile = strings.TrimPrefix(parts[3], "b/")
	}
	p.inHunk = false
	return nil
}

// handleFilePathHeader processes "--- a/file" or "+++ b/file" headers.
func (p *unifiedDiffParser) handleFilePathHeader(line string) error {
	// Extract file path as fallback
	if p.currentFile == "" && strings.HasPrefix(line, "+++") {
		parts := strings.Fields(line)
		if len(parts) >= 2 {
			p.currentFile = strings.TrimPrefix(parts[1], "b/")
		}
	}
	return nil
}

// handleHunkHeader processes "@@ -old_start,old_count +new_start,new_count @@" headers.
func (p *unifiedDiffParser) handleHunkHeader(line string) error {
	p.finalizeCurrentHunk()

	matches := unifiedDiffHunkHeaderRegex.FindStringSubmatch(line)
	if matches == nil {
		return fmt.Errorf("invalid hunk header: %s", line)
	}

	var err error
	p.oldPos, err = strconv.Atoi(matches[1])
	if err != nil {
		return fmt.Errorf("invalid old start line: %s", matches[1])
	}

	p.newPos, err = strconv.Atoi(matches[3])
	if err != nil {
		return fmt.Errorf("invalid new start line: %s", matches[3])
	}

	p.inHunk = true
	p.hunkChanges = nil
	return nil
}

// handleHunkContentLine processes content lines within a hunk.
func (p *unifiedDiffParser) handleHunkContentLine(originalLine string) error {
	switch {
	case strings.HasPrefix(originalLine, "-"):
		p.hunkChanges = append(p.hunkChanges, hunkChange{"delete", p.oldPos, -1})
		p.oldPos++
	case strings.HasPrefix(originalLine, "+"):
		p.hunkChanges = append(p.hunkChanges, hunkChange{"insert", -1, p.newPos})
		p.newPos++
	case strings.HasPrefix(originalLine, " "):
		p.hunkChanges = append(p.hunkChanges, hunkChange{"context", p.oldPos, p.newPos})
		p.oldPos++
		p.newPos++
	}
	return nil
}

// finalizeCurrentHunk processes the current hunk changes and generates classic diff operations.
func (p *unifiedDiffParser) finalizeCurrentHunk() {
	if len(p.hunkChanges) == 0 {
		return
	}

	// Collect ranges of deletions and insertions
	var deletions []int
	var insertions []int

	for _, change := range p.hunkChanges {
		switch change.changeType {
		case "delete":
			deletions = append(deletions, change.oldLineNum)
		case "insert":
			insertions = append(insertions, change.newLineNum)
		}
	}

	if len(deletions) == 0 && len(insertions) == 0 {
		return
	}

	op := p.generateClassicDiffOp(deletions, insertions)
	if op != "" {
		p.currentOps = append(p.currentOps, op)
	}

	p.hunkChanges = nil
}

// generateClassicDiffOp generates a classic diff operation from deletions and insertions.
func (p *unifiedDiffParser) generateClassicDiffOp(deletions, insertions []int) string {
	switch {
	case len(deletions) > 0 && len(insertions) > 0:
		return p.formatChangeOperation(deletions, insertions)
	case len(deletions) > 0:
		return p.formatDeleteOperation(deletions)
	case len(insertions) > 0:
		return p.formatInsertOperation(insertions)
	default:
		return ""
	}
}

// formatChangeOperation formats a change operation (delete + insert).
func (p *unifiedDiffParser) formatChangeOperation(deletions, insertions []int) string {
	delStart, delEnd := deletions[0], deletions[len(deletions)-1]
	insStart, insEnd := insertions[0], insertions[len(insertions)-1]

	switch {
	case len(deletions) == 1 && len(insertions) == 1:
		return fmt.Sprintf("%dc%d", delStart, insStart)
	case len(insertions) == 1:
		return fmt.Sprintf("%d,%dc%d", delStart, delEnd, insStart)
	case len(deletions) == 1:
		return fmt.Sprintf("%dc%d,%d", delStart, insStart, insEnd)
	default:
		return fmt.Sprintf("%d,%dc%d,%d", delStart, delEnd, insStart, insEnd)
	}
}

// formatDeleteOperation formats a delete operation.
func (p *unifiedDiffParser) formatDeleteOperation(deletions []int) string {
	delStart, delEnd := deletions[0], deletions[len(deletions)-1]
	newLineAfterDel := delStart - 1

	if len(deletions) == 1 {
		return fmt.Sprintf("%dd%d", delStart, newLineAfterDel)
	}
	return fmt.Sprintf("%d,%dd%d", delStart, delEnd, newLineAfterDel)
}

// formatInsertOperation formats an insert operation.
func (p *unifiedDiffParser) formatInsertOperation(insertions []int) string {
	insStart, insEnd := insertions[0], insertions[len(insertions)-1]
	oldLineBeforeIns := insStart - 1

	// Find the old line position by looking at the hunk context
	for _, change := range p.hunkChanges {
		if change.changeType == "context" && change.newLineNum < insStart {
			oldLineBeforeIns = change.oldLineNum
		}
	}

	if len(insertions) == 1 {
		return fmt.Sprintf("%da%d", oldLineBeforeIns, insStart)
	}
	return fmt.Sprintf("%da%d,%d", oldLineBeforeIns, insStart, insEnd)
}

// finalizeCurrentFile adds the current file to the results if it has operations.
func (p *unifiedDiffParser) finalizeCurrentFile() {
	if len(p.currentOps) > 0 {
		fileName := p.currentFile
		if fileName == "" {
			fileName = "file" // Default filename for minimal diffs
		}
		p.fileSpecs = append(p.fileSpecs, FileAdjustmentSpec{
			FilePath:    fileName,
			ClassicDiff: strings.Join(p.currentOps, ";"),
		})
		p.currentOps = nil
	}
}

// FilterUnifiedDiffForFile extracts only the portions of a unified diff that relate to a specific file.
func FilterUnifiedDiffForFile(unifiedDiff, targetFile string) (string, error) {
	multiFileSpecs, err := ConvertUnifiedDiffToMultiFileClassicDiff(unifiedDiff)
	if err != nil {
		return "", err
	}

	for _, spec := range multiFileSpecs {
		if spec.FilePath == targetFile {
			return spec.ClassicDiff, nil
		}
	}

	return "", fmt.Errorf("file %s not found in unified diff", targetFile)
}

// ParseDiffSpecWithAutoDetection parses a diff specification with automatic format detection.
// This is the main entry point that supports all supported formats.
func ParseDiffSpecWithAutoDetection(spec string) ([]LineAdjustment, error) {
	format := DetectFormat(spec)

	switch format {
	case FormatClassicDiff:
		return ParseDiffSpec(spec)
	case FormatSimpleMapping:
		return ParseSimpleMappingSpec(spec)
	case FormatUnifiedDiff:
		return ParseUnifiedDiffSpec(spec)
	default:
		return nil, fmt.Errorf("unknown or invalid diff format: %s", spec)
	}
}
