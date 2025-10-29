package models

import (
	"fmt"
	"sort"
	"strings"
	"time"
	"unicode"
)

// SmartMergeStrategy represents different strategies for merging comments when multiple
// comments end up on the same line after code changes.
//
// Each strategy has different trade-offs:
//
//   - concat: Simple, preserves all content, can be verbose
//   - threeway: Organized by type, better for mixed feedback
//   - manual: Safe for critical issues, requires human intervention
//   - skip: Fast but loses information
type SmartMergeStrategy string

const (
	StrategyConcat        SmartMergeStrategy = "concat"   // Concatenate comments with separator
	StrategyThreeWayMerge SmartMergeStrategy = "threeway" // Attempt intelligent three-way merge
	StrategyManual        SmartMergeStrategy = "manual"   // Require manual resolution
	StrategySkip          SmartMergeStrategy = "skip"     // Skip conflicting comments
)

// MergeConflict represents a conflict when multiple comments end up on the same line
// after line adjustments are applied.
//
// A conflict occurs when:
//
//   - Two or more comments map to the same file:line after adjustments
//   - The comments are from different sources (e.g., different reviewers)
//
// The system analyzes the conflict and suggests a resolution strategy based on:
//
//   - Comment types (bug, style, etc.)
//   - Number of reviewers involved
//   - Severity of issues
//
// Example conflict:
//
//	Location:            FileLocation{Path: "main.go", Lines: LineRange{StartLine: 42, EndLine: 42}}
//	ConflictingComments: ["Use const instead", "Add error handling"]
//	SuggestedStrategy:   StrategyThreeWayMerge
//	SuggestedMerge:      "**Style**: Use const instead\n**Bug**: Add error handling"
type MergeConflict struct {
	Location            FileLocation         `json:"location"`                 // File location where conflict occurs
	ConflictingComments []CommentWithContext `json:"conflictingComments"`      // All comments that collided
	SuggestedStrategy   SmartMergeStrategy   `json:"suggestedStrategy"`        // Recommended resolution strategy
	SuggestedMerge      *Comment             `json:"suggestedMerge,omitempty"` // Pre-generated merged comment (if auto-mergeable)
}

// CommentWithContext includes additional context about a comment for merge decisions.
type CommentWithContext struct {
	Comment   Comment        `json:"comment"`
	Context   CommentContext `json:"context"`
	Reviewer  string         `json:"reviewer"`
	Timestamp time.Time      `json:"timestamp"`
}

// CommentContext provides additional context about the comment.
type CommentContext struct {
	SurroundingCode string      `json:"surroundingCode,omitempty"`
	FunctionName    string      `json:"functionName,omitempty"`
	ClassName       string      `json:"className,omitempty"`
	Keywords        []string    `json:"keywords,omitempty"`
	CommentType     CommentType `json:"commentType"`
}

// CommentType categorizes the type of comment based on its content and intent.
//
// The system uses keyword-based analysis to automatically classify comments.
// This classification influences merge strategy selection:
//
//   - Critical types (bug, security) → may trigger manual review
//   - Different types → may trigger three-way merge
//
// Classification keywords:
//
//   - bug: "bug", "error", "issue", "problem", "broken", "fix", "crash"
//   - security: "security", "vulnerability", "exploit", "unsafe", "danger", "risk"
//   - performance: "performance", "slow", "optimize", "efficient", "memory", "cpu"
//   - style: "style", "format", "indent", "space", "naming", "convention"
//   - documentation: "comment", "doc", "documentation", "explain", "clarify"
//   - question: "?", "why", "how", "what", "question", "unclear"
//   - praise: "good", "great", "excellent", "nice", "well done", "perfect"
//   - nit: "nit", "minor", "nitpick", "small"
//   - suggestion: "suggest", "consider", "maybe", "could", "should", "recommend"
//   - general: default when no keywords match
type CommentType string

const (
	CommentBug           CommentType = "bug"           // Critical: bugs, errors, crashes
	CommentSuggestion    CommentType = "suggestion"    // Improvement suggestions
	CommentQuestion      CommentType = "question"      // Questions about code
	CommentPraise        CommentType = "praise"        // Positive feedback
	CommentNit           CommentType = "nit"           // Minor issues
	CommentSecurity      CommentType = "security"      // Critical: security vulnerabilities
	CommentPerformance   CommentType = "performance"   // Performance concerns
	CommentStyle         CommentType = "style"         // Code style issues
	CommentDocumentation CommentType = "documentation" // Documentation requests
	CommentGeneral       CommentType = "general"       // Uncategorized comments
)

// MergeOptions controls how smart merging is performed.
//
// Options allow fine-grained control over merge behavior:
//
// Strategy: Base strategy to use (can be overridden per conflict)
//
//   - StrategyConcat: Simple concatenation (default, safe)
//   - StrategyThreeWayMerge: Grouped by type (better organization)
//   - StrategyManual: Never auto-merge (always require human review)
//   - StrategySkip: Keep first, discard rest (fast but lossy)
//
// PreserveAttribution: Include reviewer names in merged comments
//
//   - true: "[Alice]: Use const" (default, maintains accountability)
//   - false: "Use const" (cleaner, but loses attribution)
//
// MaxConflicts: Maximum conflicts to process (performance limit)
//
//   - Default: 10 (prevents excessive processing)
//   - 0: No limit (process all conflicts)
//
// Interactive: Whether to prompt user for conflict resolution
//
//   - true: Conflicts flagged for manual review
//   - false: Auto-resolve using suggested strategy (default)
//
// PreferLatest: How to order comments when merging
//
//   - true: Newest first (latest feedback prioritized)
//   - false: Sort by reviewer name (default, consistent ordering)
//
// GroupByType: Add type indicators to merged comments
//
//   - true: "[BUG] Memory leak" (default, adds context)
//   - false: "Memory leak" (cleaner output)
type MergeOptions struct {
	Strategy            SmartMergeStrategy `json:"strategy"`            // Base merge strategy
	PreserveAttribution bool               `json:"preserveAttribution"` // Include "[Reviewer]:" prefix
	MaxConflicts        int                `json:"maxConflicts"`        // Limit conflicts processed (0=unlimited)
	Interactive         bool               `json:"interactive"`         // Prompt for manual resolution
	PreferLatest        bool               `json:"preferLatest"`        // Sort by timestamp vs reviewer name
	GroupByType         bool               `json:"groupByType"`         // Add "[TYPE]" prefix to comments
}

// DefaultMergeOptions returns default merge options.
//
// These defaults balance safety, usability, and performance:
//
//   - Concat strategy: Simple, preserves all content
//   - Attribution enabled: Maintains accountability
//   - Max 10 conflicts: Prevents performance issues
//   - Non-interactive: Auto-resolves for automation
//   - Alphabetical order: Consistent, deterministic
//   - Type grouping: Adds helpful context
func DefaultMergeOptions() MergeOptions {
	return MergeOptions{
		Strategy:            StrategyConcat,
		PreserveAttribution: true,
		MaxConflicts:        10,
		Interactive:         false,
		PreferLatest:        false,
		GroupByType:         true,
	}
}

// SmartCommentMerger handles intelligent merging of comments that end up on the same line.
//
// # Smart Comment Merging Overview
//
// When code changes occur after PR review comments are created, line numbers can shift,
// causing multiple comments to map to the same line. SmartCommentMerger provides intelligent
// strategies to handle such conflicts automatically.
//
// # Problem Statement
//
// During a typical PR review workflow:
//
//  1. Reviewers add comments at specific line numbers
//  2. Author modifies the code (adding/deleting lines)
//  3. Line adjustments cause comments to shift
//  4. Multiple comments may end up on the same line (conflict)
//
// Example scenario:
//
//	Original code:     Reviewer comments:      After deleting lines 10-12:
//	10: func foo() {   (no comment)            10: func foo() {
//	11:   x := 1       Alice: "Use const"      11:   y := 2    <- Both comments now on line 11!
//	12:   // old code  (deleted)                12:   return y
//	13:   y := 2       Bob: "Add validation"   }
//	14:   return y
//	15: }
//
// # Merge Strategies
//
// SmartCommentMerger detects and resolves conflicts using multiple strategies:
//
//  1. **Concat Strategy** (StrategyConcat) - Combines comments with attribution:
//     "[Alice]: Use const\n\n[Bob]: Add validation"
//
//  2. **Three-Way Merge** (StrategyThreeWayMerge) - Groups by comment type:
//     "**Bug**: [Alice] Memory leak here\n**Style**: [Bob] Use camelCase"
//
//  3. **Manual Strategy** (StrategyManual) - Flags conflicts for human review:
//     Used for critical issues (bugs/security) with multiple reviewers
//
//  4. **Skip Strategy** (StrategySkip) - Keeps first comment, discards others:
//     Fast but loses information
//
// # Key Capabilities
//
//   - **Context-Aware**: Analyzes comment content to detect type (bug, security, style, etc.)
//   - **Reviewer Attribution**: Preserves who said what (configurable via MergeOptions)
//   - **Conflict Detection**: Identifies when multiple comments collide on same line
//   - **Automatic Resolution**: Intelligently merges using appropriate strategy
//   - **History Tracking**: Records merge operations in comment adjustment history
//   - **Configurable**: Fine-grained control via MergeOptions
//
// # Strategy Selection Logic
//
// The system automatically suggests the best strategy based on:
//
//   - Same reviewer → Concat (likely follow-up thoughts)
//   - Critical issues (bug/security) + multiple reviewers → Manual (needs human judgment)
//   - Different comment types → Three-Way Merge (organized by category)
//   - Default → Concat (safe fallback)
//
// # Usage Example
//
//	// Create merger with options
//	options := DefaultMergeOptions()
//	merger := NewSmartCommentMerger(options)
//
//	// After code changes, detect conflicts
//	adjustments := []LineAdjustment{
//	    {Operation: OperationDelete, OldStart: 10, OldEnd: 12, ...},
//	}
//	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
//
//	// Resolve conflicts
//	for _, conflict := range conflicts {
//	    resolved, err := merger.ResolveMergeConflict(conflict, conflict.SuggestedStrategy)
//	    if err != nil {
//	        // Handle manual resolution needed
//	    }
//	}
//
// Or use the convenience function ApplySmartMerging() for end-to-end processing.
//
// # Integration
//
// This is used by the gh-pr-review tool when submitting reviews with --smart-merge flag:
//
//	gh pr-review submit --auto-adjust --smart-merge
//
// See: devtools/gh-nudge/internal/prreview/commands.go for integration details.
type SmartCommentMerger struct {
	options MergeOptions
}

// NewSmartCommentMerger creates a new smart comment merger with the specified options.
//
// The options control merge behavior including strategy selection, attribution,
// conflict limits, and presentation format. Use DefaultMergeOptions() for sensible
// defaults that balance safety, usability, and performance.
//
// Example:
//
//	// Use defaults
//	merger := NewSmartCommentMerger(DefaultMergeOptions())
//
//	// Customize options
//	opts := DefaultMergeOptions()
//	opts.Strategy = StrategyThreeWayMerge
//	opts.PreserveAttribution = false
//	merger := NewSmartCommentMerger(opts)
func NewSmartCommentMerger(options MergeOptions) *SmartCommentMerger {
	return &SmartCommentMerger{
		options: options,
	}
}

// DetectMergeConflicts identifies comments that will conflict after line adjustments.
func (m *SmartCommentMerger) DetectMergeConflicts(comments []Comment, adjustments []LineAdjustment) ([]MergeConflict, error) {
	// Apply adjustments to get adjusted line numbers
	adjustedComments := make([]CommentWithContext, 0, len(comments))

	for _, comment := range comments {
		testComment := comment
		if AdjustComment(&testComment, adjustments) {
			ctx := m.analyzeCommentContext(testComment)
			adjustedComments = append(adjustedComments, CommentWithContext{
				Comment:   testComment,
				Context:   ctx,
				Reviewer:  m.extractReviewer(testComment),
				Timestamp: testComment.CreatedAt,
			})
		}
	}

	// Group by line number and file
	// Note: We use GetEndLineLocationKey() to group by end line only,
	// so that multi-line comments ending on the same line are detected as conflicts.
	conflicts := make(map[string][]CommentWithContext)
	for _, commentCtx := range adjustedComments {
		key := commentCtx.Comment.GetEndLineLocationKey()
		conflicts[key] = append(conflicts[key], commentCtx)
	}

	// Identify actual conflicts (multiple comments on same line)
	var mergeConflicts []MergeConflict
	for key, commentList := range conflicts {
		if len(commentList) > 1 {
			conflict := m.createMergeConflict(key, commentList)
			mergeConflicts = append(mergeConflicts, conflict)
		}
	}

	// Limit conflicts if specified
	if m.options.MaxConflicts > 0 && len(mergeConflicts) > m.options.MaxConflicts {
		mergeConflicts = mergeConflicts[:m.options.MaxConflicts]
	}

	return mergeConflicts, nil
}

// createMergeConflict creates a MergeConflict from a location key and list of conflicting comments.
func (m *SmartCommentMerger) createMergeConflict(locationKey string, commentList []CommentWithContext) MergeConflict {
	// Parse location from key
	loc, err := ParseFileLocation(locationKey)
	if err != nil {
		// Fallback to using values from first comment if parsing fails
		loc = commentList[0].Comment.GetLocation()
	}

	// Expand the location to include the widest range if any comment is multi-line
	loc = m.expandLocationForMultiLineComments(loc, commentList)

	conflict := MergeConflict{
		Location:            loc,
		ConflictingComments: commentList,
		SuggestedStrategy:   m.suggestMergeStrategy(commentList),
	}

	// Generate suggested merge if strategy allows
	if conflict.SuggestedStrategy != StrategyManual && conflict.SuggestedStrategy != StrategySkip {
		suggestedMerge, err := m.generateMergedComment(commentList, conflict.SuggestedStrategy)
		if err == nil {
			conflict.SuggestedMerge = suggestedMerge
		}
	}

	return conflict
}

// expandLocationForMultiLineComments expands the location range to include all multi-line comments.
func (m *SmartCommentMerger) expandLocationForMultiLineComments(loc FileLocation, commentList []CommentWithContext) FileLocation {
	for _, commentCtx := range commentList {
		if commentCtx.Comment.IsMultiLine() {
			if commentCtx.Comment.Line.StartLine < loc.Lines.StartLine {
				loc.Lines.StartLine = commentCtx.Comment.Line.StartLine
			}
			if commentCtx.Comment.Line.EndLine > loc.Lines.EndLine {
				loc.Lines.EndLine = commentCtx.Comment.Line.EndLine
			}
		}
	}
	return loc
}

// ResolveMergeConflict resolves a single merge conflict according to the specified strategy.
func (m *SmartCommentMerger) ResolveMergeConflict(conflict MergeConflict, strategy SmartMergeStrategy) (*Comment, error) {
	switch strategy {
	case StrategyConcat:
		return m.generateMergedComment(conflict.ConflictingComments, strategy)
	case StrategyThreeWayMerge:
		return m.performThreeWayMerge(conflict.ConflictingComments)
	case StrategySkip:
		// Return the first comment, skip others
		return &conflict.ConflictingComments[0].Comment, nil
	case StrategyManual:
		return nil, fmt.Errorf("manual resolution required for conflict at %s", conflict.Location)
	default:
		return nil, fmt.Errorf("unknown merge strategy: %s", strategy)
	}
}

// suggestMergeStrategy analyzes the comments and suggests the best merge strategy.
func (m *SmartCommentMerger) suggestMergeStrategy(comments []CommentWithContext) SmartMergeStrategy {
	if len(comments) <= 1 {
		return StrategySkip
	}

	// Analyze comment types and reviewers
	reviewers := make(map[string]bool)
	types := make(map[CommentType]int)
	hasCritical := false

	for _, commentCtx := range comments {
		reviewers[commentCtx.Reviewer] = true
		types[commentCtx.Context.CommentType]++

		// Check for critical comments (bugs, security issues)
		if commentCtx.Context.CommentType == CommentBug ||
			commentCtx.Context.CommentType == CommentSecurity {
			hasCritical = true
		}
	}

	// If same reviewer made multiple comments, prefer concatenation
	if len(reviewers) == 1 {
		return StrategyConcat
	}

	// If critical issues are present, require manual review
	if hasCritical && len(reviewers) > 1 {
		return StrategyManual
	}

	// If comments are of different types, try three-way merge
	if len(types) > 1 {
		return StrategyThreeWayMerge
	}

	// Default to concatenation
	return StrategyConcat
}

// generateMergedComment creates a merged comment from multiple comments.
func (m *SmartCommentMerger) generateMergedComment(comments []CommentWithContext, strategy SmartMergeStrategy) (*Comment, error) {
	if len(comments) == 0 {
		return nil, fmt.Errorf("no comments to merge")
	}

	m.sortComments(comments)
	merged := m.createBaseMergedComment(comments[0].Comment)
	bodyParts := m.generateBodyParts(comments, strategy)
	m.finalizeComment(&merged, bodyParts, comments, strategy)

	return &merged, nil
}

// sortComments sorts comments by timestamp or reviewer name based on options.
func (m *SmartCommentMerger) sortComments(comments []CommentWithContext) {
	if m.options.PreferLatest {
		sort.Slice(comments, func(i, j int) bool {
			return comments[i].Timestamp.After(comments[j].Timestamp)
		})
	} else {
		// Sort by reviewer name for consistency
		sort.Slice(comments, func(i, j int) bool {
			return comments[i].Reviewer < comments[j].Reviewer
		})
	}
}

// createBaseMergedComment creates the base merged comment from the first comment.
func (m *SmartCommentMerger) createBaseMergedComment(baseComment Comment) Comment {
	merged := baseComment
	merged.ID = GenerateCommentID() // Generate new ID for merged comment
	return merged
}

// generateBodyParts generates the body parts based on the merge strategy.
func (m *SmartCommentMerger) generateBodyParts(comments []CommentWithContext, strategy SmartMergeStrategy) []string {
	var bodyParts []string

	switch strategy {
	case StrategyConcat:
		bodyParts = m.generateConcatBodyParts(comments)
	case StrategyThreeWayMerge:
		bodyParts = m.generateThreeWayMergeBodyParts(comments)
	default:
		// Return empty slice for unsupported strategy
		return nil
	}

	return bodyParts
}

// generateConcatBodyParts generates body parts for concatenation strategy.
func (m *SmartCommentMerger) generateConcatBodyParts(comments []CommentWithContext) []string {
	var bodyParts []string
	for _, commentCtx := range comments {
		part := m.formatCommentPart(commentCtx)
		bodyParts = append(bodyParts, part)
	}
	return bodyParts
}

// generateThreeWayMergeBodyParts generates body parts for three-way merge strategy.
func (m *SmartCommentMerger) generateThreeWayMergeBodyParts(comments []CommentWithContext) []string {
	var bodyParts []string
	seenContent := make(map[string]bool)
	for _, commentCtx := range comments {
		content := strings.TrimSpace(commentCtx.Comment.Body)
		if !seenContent[content] {
			var part string
			if m.options.PreserveAttribution && commentCtx.Reviewer != "" {
				part = fmt.Sprintf("[%s]: %s", commentCtx.Reviewer, content)
			} else {
				part = content
			}
			bodyParts = append(bodyParts, part)
			seenContent[content] = true
		}
	}
	return bodyParts
}

// formatCommentPart formats a single comment part with attribution and type.
func (m *SmartCommentMerger) formatCommentPart(commentCtx CommentWithContext) string {
	var part string
	if m.options.PreserveAttribution && commentCtx.Reviewer != "" {
		part = fmt.Sprintf("[%s]: %s", commentCtx.Reviewer, commentCtx.Comment.Body)
	} else {
		part = commentCtx.Comment.Body
	}

	// Add type indicator if grouping by type
	if m.options.GroupByType && commentCtx.Context.CommentType != CommentGeneral {
		part = fmt.Sprintf("[%s] %s", strings.ToUpper(string(commentCtx.Context.CommentType)), part)
	}

	return part
}

// finalizeComment finalizes the merged comment with body and metadata.
func (m *SmartCommentMerger) finalizeComment(merged *Comment, bodyParts []string, comments []CommentWithContext, strategy SmartMergeStrategy) {
	merged.Body = strings.Join(bodyParts, "\n\n")
	m.preserveMultiLineRange(merged, comments)
	m.updateCommentMetadata(merged, comments, strategy)
}

// preserveMultiLineRange preserves multi-line range from any comment.
func (m *SmartCommentMerger) preserveMultiLineRange(merged *Comment, comments []CommentWithContext) {
	for _, commentCtx := range comments {
		// Expand the range to include the widest multi-line range
		//
		// TODO: See if this should be handled by a proper method of LineRange.
		if commentCtx.Comment.Line.StartLine < merged.Line.StartLine {
			merged.Line.StartLine = commentCtx.Comment.Line.StartLine
		}
		if commentCtx.Comment.Line.EndLine > merged.Line.EndLine {
			merged.Line.EndLine = commentCtx.Comment.Line.EndLine
		}
	}
}

// updateCommentMetadata updates the metadata of the merged comment.
func (m *SmartCommentMerger) updateCommentMetadata(merged *Comment, comments []CommentWithContext, strategy SmartMergeStrategy) {
	merged.CreatedAt = time.Now()

	// Add merge history to adjustment history
	mergeAdjustment := LineAdjustment{
		Operation:   OperationChange, // Custom operation type for merge (using change as placeholder)
		AppliedAt:   time.Now(),
		Description: fmt.Sprintf("Merged %d comments using %s strategy", len(comments), strategy),
	}
	merged.AdjustmentHistory = append(merged.AdjustmentHistory, mergeAdjustment)
}

// performThreeWayMerge attempts an intelligent merge of comments.
func (m *SmartCommentMerger) performThreeWayMerge(comments []CommentWithContext) (*Comment, error) {
	// Group comments by type
	typeGroups := make(map[CommentType][]CommentWithContext)
	for _, commentCtx := range comments {
		commentType := commentCtx.Context.CommentType
		typeGroups[commentType] = append(typeGroups[commentType], commentCtx)
	}

	// Process each type group
	var sections []string
	for commentType, group := range typeGroups {
		if len(group) == 1 {
			// Single comment of this type
			commentCtx := group[0]
			var content string
			if m.options.PreserveAttribution && commentCtx.Reviewer != "" {
				content = fmt.Sprintf("[%s]: %s", commentCtx.Reviewer, commentCtx.Comment.Body)
			} else {
				content = commentCtx.Comment.Body
			}

			if commentType != CommentGeneral {
				sections = append(sections, fmt.Sprintf("**%s**: %s", toTitle(string(commentType)), content))
			} else {
				sections = append(sections, content)
			}
		} else {
			// Multiple comments of same type - merge them
			var parts []string
			for _, commentCtx := range group {
				var part string
				if m.options.PreserveAttribution && commentCtx.Reviewer != "" {
					part = fmt.Sprintf("[%s]: %s", commentCtx.Reviewer, commentCtx.Comment.Body)
				} else {
					part = commentCtx.Comment.Body
				}
				parts = append(parts, part)
			}

			if commentType != CommentGeneral {
				sections = append(sections, fmt.Sprintf("**%s**:\n%s", toTitle(string(commentType)), strings.Join(parts, "\n")))
			} else {
				sections = append(sections, strings.Join(parts, "\n"))
			}
		}
	}

	// Create merged comment
	if len(comments) == 0 {
		return nil, fmt.Errorf("no comments to merge")
	}

	merged := comments[0].Comment
	merged.ID = GenerateCommentID()
	merged.Body = strings.Join(sections, "\n\n")
	merged.CreatedAt = time.Now()

	return &merged, nil
}

// analyzeCommentContext extracts context information from a comment.
func (m *SmartCommentMerger) analyzeCommentContext(comment Comment) CommentContext {
	body := strings.ToLower(comment.Body)

	// Simple keyword-based classification
	commentType := CommentGeneral
	var keywords []string

	// Bug-related keywords
	if containsAny(body, []string{"bug", "error", "issue", "problem", "broken", "fix", "crash"}) {
		commentType = CommentBug
		keywords = append(keywords, "bug")
	}

	// Security-related keywords
	if containsAny(body, []string{"security", "vulnerability", "exploit", "unsafe", "danger", "risk"}) {
		commentType = CommentSecurity
		keywords = append(keywords, "security")
	}

	// Performance-related keywords
	if containsAny(body, []string{"performance", "slow", "optimize", "efficient", "memory", "cpu", "speed"}) {
		commentType = CommentPerformance
		keywords = append(keywords, "performance")
	}

	// Style-related keywords
	if containsAny(body, []string{"style", "format", "indent", "space", "naming", "convention"}) {
		commentType = CommentStyle
		keywords = append(keywords, "style")
	}

	// Documentation-related keywords
	if containsAny(body, []string{"comment", "doc", "documentation", "explain", "clarify", "describe"}) {
		commentType = CommentDocumentation
		keywords = append(keywords, "documentation")
	}

	// Question keywords
	if containsAny(body, []string{"?", "why", "how", "what", "question", "unclear"}) {
		commentType = CommentQuestion
		keywords = append(keywords, "question")
	}

	// Praise keywords
	if containsAny(body, []string{"good", "great", "excellent", "nice", "well done", "perfect"}) {
		commentType = CommentPraise
		keywords = append(keywords, "praise")
	}

	// Nit keywords
	if containsAny(body, []string{"nit", "minor", "nitpick", "small"}) {
		commentType = CommentNit
		keywords = append(keywords, "nit")
	}

	// Suggestion keywords
	if containsAny(body, []string{"suggest", "consider", "maybe", "could", "should", "recommend"}) {
		if commentType == CommentGeneral {
			commentType = CommentSuggestion
		}
		keywords = append(keywords, "suggestion")
	}

	return CommentContext{
		CommentType: commentType,
		Keywords:    keywords,
		// TODO: Add code analysis for function/class names
	}
}

// extractReviewer extracts reviewer information from comment metadata.
func (m *SmartCommentMerger) extractReviewer(_ Comment) string {
	// For now, return empty string - this would be extracted from GitHub API
	// In a real implementation, this would come from the comment's author field
	return "reviewer"
}

// containsAny checks if the text contains any of the keywords.
func containsAny(text string, keywords []string) bool {
	for _, keyword := range keywords {
		if strings.Contains(text, keyword) {
			return true
		}
	}
	return false
}

// toTitle converts a string to title case (first letter uppercase).
func toTitle(s string) string {
	if len(s) == 0 {
		return s
	}
	r := []rune(s)
	r[0] = unicode.ToUpper(r[0])
	return string(r)
}

// ApplySmartMerging applies smart merging to a list of comments after line adjustments.
//
// This is the main entry point for the smart merge functionality. It:
//
//  1. Applies line adjustments to comments (shifting lines due to code changes)
//  2. Detects conflicts (multiple comments on same line)
//  3. Automatically resolves conflicts using appropriate strategies
//  4. Returns merged comments and any unresolved conflicts
//
// Parameters:
//
//   - comments: Original review comments with line numbers
//   - adjustments: Line changes (insertions, deletions, modifications)
//   - options: Configuration for merge behavior
//
// Returns:
//
//   - []Comment: Final list with conflicts resolved
//   - []MergeConflict: Conflicts that require manual resolution
//   - error: Any error during processing
//
// Workflow:
//
//  1. Apply adjustments → shift comment line numbers
//  2. Group by (file, line) → identify collisions
//  3. Analyze conflicts → determine best strategy
//  4. Auto-merge resolvable conflicts → combine into single comment
//  5. Flag unresolvable conflicts → return for manual review
//
// Example:
//
//	// After code changes shift lines
//	adjustments := []LineAdjustment{
//	    {Operation: OperationDelete, OldStart: 10, OldEnd: 12},
//	}
//
//	// Apply smart merging
//	merged, conflicts, err := ApplySmartMerging(comments, adjustments, DefaultMergeOptions())
//	if err != nil {
//	    return err
//	}
//
//	// Check for manual review needed
//	if len(conflicts) > 0 {
//	    fmt.Printf("Warning: %d conflicts need manual review\n", len(conflicts))
//	    for _, c := range conflicts {
//	        fmt.Printf("  %s - %d comments\n", c.Location, len(c.ConflictingComments))
//	    }
//	}
//
//	// Submit merged comments
//	submitComments(merged)
//
// Strategy Selection:
//
// The system automatically chooses the best strategy per conflict:
//
//   - Same reviewer → Concat (follow-up thoughts)
//   - Bug/Security + multiple reviewers → Manual (critical, needs human judgment)
//   - Different types → ThreeWayMerge (organized presentation)
//   - Default → Concat (safe fallback)
//
// Conflict Resolution Behavior:
//
//   - Auto-resolvable (concat, threeway): Merged immediately
//   - Manual required: Returned in conflicts list
//   - Interactive mode: All conflicts returned (even auto-resolvable)
//   - Fallback: If merge fails, keeps first comment
func ApplySmartMerging(comments []Comment, adjustments []LineAdjustment, options MergeOptions) ([]Comment, []MergeConflict, error) {
	merger := NewSmartCommentMerger(options)

	// Detect conflicts
	conflicts, err := merger.DetectMergeConflicts(comments, adjustments)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to detect merge conflicts: %w", err)
	}

	if len(conflicts) == 0 {
		// No conflicts, apply adjustments normally
		adjustedComments := make([]Comment, 0, len(comments))
		for _, comment := range comments {
			testComment := comment
			if AdjustComment(&testComment, adjustments) {
				adjustedComments = append(adjustedComments, testComment)
			}
		}
		return adjustedComments, nil, nil
	}

	// Build map of conflicting comment IDs for quick lookup
	conflictingIDs := make(map[string]bool)
	for _, conflict := range conflicts {
		for _, commentCtx := range conflict.ConflictingComments {
			conflictingIDs[commentCtx.Comment.ID] = true
		}
	}

	// Resolve conflicts and build final comment list
	var finalComments []Comment
	var unresolvedConflicts []MergeConflict

	// Add non-conflicting comments
	for _, comment := range comments {
		if !conflictingIDs[comment.ID] {
			testComment := comment
			if AdjustComment(&testComment, adjustments) {
				finalComments = append(finalComments, testComment)
			}
		}
	}

	// Resolve conflicts
	for _, conflict := range conflicts {
		resolved, err := merger.ResolveMergeConflict(conflict, conflict.SuggestedStrategy)
		if err != nil {
			if options.Interactive || conflict.SuggestedStrategy == StrategyManual {
				unresolvedConflicts = append(unresolvedConflicts, conflict)
				continue
			}
			// Fallback: use first comment
			finalComments = append(finalComments, conflict.ConflictingComments[0].Comment)
		} else {
			finalComments = append(finalComments, *resolved)
		}
	}

	return finalComments, unresolvedConflicts, nil
}
