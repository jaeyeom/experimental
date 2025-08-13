package models

import (
	"fmt"
	"sort"
	"strings"
	"time"
	"unicode"
)

// SmartMergeStrategy represents different strategies for merging comments.
type SmartMergeStrategy string

const (
	StrategyConcat        SmartMergeStrategy = "concat"   // Concatenate comments with separator
	StrategyThreeWayMerge SmartMergeStrategy = "threeway" // Attempt intelligent three-way merge
	StrategyManual        SmartMergeStrategy = "manual"   // Require manual resolution
	StrategySkip          SmartMergeStrategy = "skip"     // Skip conflicting comments
)

// MergeConflict represents a conflict when multiple comments end up on the same line.
type MergeConflict struct {
	Line                int                  `json:"line"`
	StartLine           *int                 `json:"startLine,omitempty"`
	File                string               `json:"file"`
	ConflictingComments []CommentWithContext `json:"conflictingComments"`
	SuggestedStrategy   SmartMergeStrategy   `json:"suggestedStrategy"`
	SuggestedMerge      *Comment             `json:"suggestedMerge,omitempty"`
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

// CommentType categorizes the type of comment.
type CommentType string

const (
	CommentBug           CommentType = "bug"
	CommentSuggestion    CommentType = "suggestion"
	CommentQuestion      CommentType = "question"
	CommentPraise        CommentType = "praise"
	CommentNit           CommentType = "nit"
	CommentSecurity      CommentType = "security"
	CommentPerformance   CommentType = "performance"
	CommentStyle         CommentType = "style"
	CommentDocumentation CommentType = "documentation"
	CommentGeneral       CommentType = "general"
)

// MergeOptions controls how smart merging is performed.
type MergeOptions struct {
	Strategy            SmartMergeStrategy `json:"strategy"`
	PreserveAttribution bool               `json:"preserveAttribution"`
	MaxConflicts        int                `json:"maxConflicts"`
	Interactive         bool               `json:"interactive"`
	PreferLatest        bool               `json:"preferLatest"`
	GroupByType         bool               `json:"groupByType"`
}

// DefaultMergeOptions returns default merge options.
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
type SmartCommentMerger struct {
	options MergeOptions
}

// NewSmartCommentMerger creates a new smart comment merger.
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
	conflicts := make(map[string][]CommentWithContext)
	for _, commentCtx := range adjustedComments {
		key := fmt.Sprintf("%s:%d", commentCtx.Comment.Path, commentCtx.Comment.Line)
		conflicts[key] = append(conflicts[key], commentCtx)
	}

	// Identify actual conflicts (multiple comments on same line)
	var mergeConflicts []MergeConflict
	for key, commentList := range conflicts {
		if len(commentList) > 1 {
			parts := strings.Split(key, ":")
			line := commentList[0].Comment.Line
			file := parts[0]

			conflict := MergeConflict{
				Line:                line,
				File:                file,
				ConflictingComments: commentList,
				SuggestedStrategy:   m.suggestMergeStrategy(commentList),
			}

			// Add start line if any comment is multi-line
			for _, commentCtx := range commentList {
				if commentCtx.Comment.StartLine != nil {
					conflict.StartLine = commentCtx.Comment.StartLine
					break
				}
			}

			// Generate suggested merge if strategy allows
			if conflict.SuggestedStrategy != StrategyManual && conflict.SuggestedStrategy != StrategySkip {
				suggestedMerge, err := m.generateMergedComment(commentList, conflict.SuggestedStrategy)
				if err == nil {
					conflict.SuggestedMerge = suggestedMerge
				}
			}

			mergeConflicts = append(mergeConflicts, conflict)
		}
	}

	// Limit conflicts if specified
	if m.options.MaxConflicts > 0 && len(mergeConflicts) > m.options.MaxConflicts {
		mergeConflicts = mergeConflicts[:m.options.MaxConflicts]
	}

	return mergeConflicts, nil
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
		return nil, fmt.Errorf("manual resolution required for conflict at %s:%d", conflict.File, conflict.Line)
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
		if commentCtx.Comment.StartLine != nil && merged.StartLine == nil {
			merged.StartLine = commentCtx.Comment.StartLine
		}
		if commentCtx.Comment.StartLine != nil && merged.StartLine != nil && *commentCtx.Comment.StartLine < *merged.StartLine {
			merged.StartLine = commentCtx.Comment.StartLine
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

// ApplySmartMerging applies smart merging to a list of comments after adjustments.
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
