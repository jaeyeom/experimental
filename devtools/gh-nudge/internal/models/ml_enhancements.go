package models

import (
	"encoding/json"
	"fmt"
	"math"
	"sort"
	"strings"
	"time"
)

// MLConfidenceLevel represents the confidence level for ML-based suggestions.
type MLConfidenceLevel string

const (
	MLConfidenceHigh   MLConfidenceLevel = "high"   // >80% confidence
	MLConfidenceMedium MLConfidenceLevel = "medium" // 60-80% confidence
	MLConfidenceLow    MLConfidenceLevel = "low"    // <60% confidence
)

// PatternLearningEngine learns from user correction patterns to improve future suggestions.
type PatternLearningEngine struct {
	Patterns    []AdjustmentPattern `json:"patterns"`
	Corrections []UserCorrection    `json:"corrections"`
	Preferences UserPreferences     `json:"preferences"`
	Stats       LearningStatistics  `json:"statistics"`
}

// AdjustmentPattern represents a learned pattern for line adjustments.
type AdjustmentPattern struct {
	ID          string                 `json:"id"`
	Context     PatternContext         `json:"context"`
	Adjustment  LineAdjustment         `json:"adjustment"`
	Confidence  float64                `json:"confidence"` // 0.0 to 1.0
	Occurrences int                    `json:"occurrences"`
	LastSeen    time.Time              `json:"lastSeen"`
	Accuracy    float64                `json:"accuracy"` // Success rate when applied
	Metadata    map[string]interface{} `json:"metadata,omitempty"`
}

// PatternContext represents the context in which an adjustment pattern applies.
type PatternContext struct {
	FileType        string   `json:"fileType"`        // .go, .js, .py, etc.
	ChangeType      string   `json:"changeType"`      // function_add, import_change, etc.
	SurroundingCode string   `json:"surroundingCode"` // Code context around the change
	Keywords        []string `json:"keywords"`        // Relevant keywords
	LineDistance    int      `json:"lineDistance"`    // Distance from actual change
	CodeStructure   string   `json:"codeStructure"`   // function, class, method, etc.
}

// UserCorrection represents a correction made by the user to an auto-suggestion.
type UserCorrection struct {
	ID                 string            `json:"id"`
	Timestamp          time.Time         `json:"timestamp"`
	OriginalSuggestion LineAdjustment    `json:"originalSuggestion"`
	UserCorrection     LineAdjustment    `json:"userCorrection"`
	Context            PatternContext    `json:"context"`
	CorrectionType     CorrectionType    `json:"correctionType"`
	Confidence         MLConfidenceLevel `json:"confidence"`
}

// CorrectionType represents the type of correction made by the user.
type CorrectionType string

const (
	CorrectionOffset    CorrectionType = "offset"    // Changed the offset amount
	CorrectionDirection CorrectionType = "direction" // Changed direction (+ to - or vice versa)
	CorrectionLine      CorrectionType = "line"      // Changed the target line
	CorrectionReject    CorrectionType = "reject"    // Rejected the suggestion entirely
	CorrectionAccept    CorrectionType = "accept"    // Accepted suggestion as-is
)

// UserPreferences represents learned user preferences.
type UserPreferences struct {
	PreferredConfidenceThreshold float64                  `json:"preferredConfidenceThreshold"`
	ReviewPatterns               []ReviewPattern          `json:"reviewPatterns"`
	FileTypePreferences          map[string]FileTypePrefs `json:"fileTypePreferences"`
	TeamCollaborationStyle       CollaborationStyle       `json:"teamCollaborationStyle"`
	UpdatedAt                    time.Time                `json:"updatedAt"`
}

// ReviewPattern represents a learned review pattern.
type ReviewPattern struct {
	Pattern       string    `json:"pattern"`
	Frequency     int       `json:"frequency"`
	Effectiveness float64   `json:"effectiveness"`
	LastUsed      time.Time `json:"lastUsed"`
}

// FileTypePrefs represents preferences for a specific file type.
type FileTypePrefs struct {
	AdjustmentTolerance float64 `json:"adjustmentTolerance"`
	PreferredStrategy   string  `json:"preferredStrategy"`
	ContextSensitivity  float64 `json:"contextSensitivity"`
}

// CollaborationStyle represents team collaboration preferences.
type CollaborationStyle struct {
	PreferDetailedComments bool    `json:"preferDetailedComments"`
	CommentMergeStrategy   string  `json:"commentMergeStrategy"`
	ReviewThoroughness     float64 `json:"reviewThoroughness"`
}

// LearningStatistics tracks learning performance metrics.
type LearningStatistics struct {
	TotalSuggestions       int                       `json:"totalSuggestions"`
	AcceptedSuggestions    int                       `json:"acceptedSuggestions"`
	RejectedSuggestions    int                       `json:"rejectedSuggestions"`
	AccuracyRate           float64                   `json:"accuracyRate"`
	ConfidenceDistribution map[MLConfidenceLevel]int `json:"confidenceDistribution"`
	LastUpdated            time.Time                 `json:"lastUpdated"`
	ImprovementRate        float64                   `json:"improvementRate"` // Rate of improvement over time
}

// NewPatternLearningEngine creates a new pattern learning engine.
func NewPatternLearningEngine() *PatternLearningEngine {
	return &PatternLearningEngine{
		Patterns:    make([]AdjustmentPattern, 0),
		Corrections: make([]UserCorrection, 0),
		Preferences: UserPreferences{
			PreferredConfidenceThreshold: 0.7, // Default 70% confidence threshold
			FileTypePreferences:          make(map[string]FileTypePrefs),
			TeamCollaborationStyle: CollaborationStyle{
				PreferDetailedComments: true,
				CommentMergeStrategy:   "concat",
				ReviewThoroughness:     0.8,
			},
			UpdatedAt: time.Now(),
		},
		Stats: LearningStatistics{
			ConfidenceDistribution: make(map[MLConfidenceLevel]int),
			LastUpdated:            time.Now(),
		},
	}
}

// LearnFromCorrection learns from a user correction.
func (e *PatternLearningEngine) LearnFromCorrection(correction UserCorrection) {
	correction.ID = GenerateCommentID()
	correction.Timestamp = time.Now()

	e.Corrections = append(e.Corrections, correction)

	// Update existing patterns or create new ones
	e.updatePatternsFromCorrection(correction)

	// Update statistics
	e.updateStatistics(correction)

	// Update user preferences
	e.updatePreferences(correction)
}

// updatePatternsFromCorrection updates existing patterns based on user correction.
func (e *PatternLearningEngine) updatePatternsFromCorrection(correction UserCorrection) {
	// Find similar patterns
	similarPattern := e.findSimilarPattern(correction.Context, correction.UserCorrection)

	if similarPattern != nil {
		// Update existing pattern
		similarPattern.Occurrences++
		similarPattern.LastSeen = time.Now()

		// Update confidence based on correction type
		switch correction.CorrectionType {
		case CorrectionAccept:
			similarPattern.Confidence = math.Min(1.0, similarPattern.Confidence+0.1)
			similarPattern.Accuracy = (similarPattern.Accuracy*float64(similarPattern.Occurrences-1) + 1.0) / float64(similarPattern.Occurrences)
		case CorrectionReject:
			similarPattern.Confidence = math.Max(0.0, similarPattern.Confidence-0.2)
			similarPattern.Accuracy = (similarPattern.Accuracy*float64(similarPattern.Occurrences-1) + 0.0) / float64(similarPattern.Occurrences)
		default:
			// Partial correction - smaller adjustment
			similarPattern.Confidence = math.Max(0.0, similarPattern.Confidence-0.05)
			similarPattern.Accuracy = (similarPattern.Accuracy*float64(similarPattern.Occurrences-1) + 0.5) / float64(similarPattern.Occurrences)
		}
	} else {
		// Create new pattern from user correction
		newPattern := AdjustmentPattern{
			ID:          GenerateCommentID(),
			Context:     correction.Context,
			Adjustment:  correction.UserCorrection,
			Confidence:  0.6, // Start with medium confidence
			Occurrences: 1,
			LastSeen:    time.Now(),
			Accuracy:    1.0, // Start with perfect accuracy
		}

		e.Patterns = append(e.Patterns, newPattern)
	}

	// Prune old or low-confidence patterns
	e.prunePatterns()
}

// findSimilarPattern finds a pattern similar to the given context and adjustment.
func (e *PatternLearningEngine) findSimilarPattern(context PatternContext, adjustment LineAdjustment) *AdjustmentPattern {
	for i := range e.Patterns {
		pattern := &e.Patterns[i]
		similarity := e.calculateContextSimilarity(pattern.Context, context)

		if similarity > 0.8 && e.adjustmentsAreSimilar(pattern.Adjustment, adjustment) {
			return pattern
		}
	}
	return nil
}

// calculateContextSimilarity calculates similarity between two pattern contexts.
func (e *PatternLearningEngine) calculateContextSimilarity(ctx1, ctx2 PatternContext) float64 {
	similarity := 0.0
	factors := 0

	// File type match
	if ctx1.FileType == ctx2.FileType {
		similarity += 0.3
	}
	factors++

	// Change type match
	if ctx1.ChangeType == ctx2.ChangeType {
		similarity += 0.2
	}
	factors++

	// Code structure match
	if ctx1.CodeStructure == ctx2.CodeStructure {
		similarity += 0.2
	}
	factors++

	// Keywords overlap
	keywordSimilarity := e.calculateKeywordSimilarity(ctx1.Keywords, ctx2.Keywords)
	similarity += keywordSimilarity * 0.2
	factors++

	// Line distance similarity
	distanceDiff := math.Abs(float64(ctx1.LineDistance - ctx2.LineDistance))
	distanceSimilarity := math.Max(0.0, 1.0-distanceDiff/10.0)
	similarity += distanceSimilarity * 0.1
	factors++

	return similarity / float64(factors)
}

// calculateKeywordSimilarity calculates similarity between keyword lists.
func (e *PatternLearningEngine) calculateKeywordSimilarity(keywords1, keywords2 []string) float64 {
	if len(keywords1) == 0 && len(keywords2) == 0 {
		return 1.0
	}
	if len(keywords1) == 0 || len(keywords2) == 0 {
		return 0.0
	}

	// Simple Jaccard similarity
	set1 := make(map[string]bool)
	for _, kw := range keywords1 {
		set1[kw] = true
	}

	intersection := 0
	for _, kw := range keywords2 {
		if set1[kw] {
			intersection++
		}
	}

	union := len(keywords1) + len(keywords2) - intersection
	if union == 0 {
		return 0.0
	}

	return float64(intersection) / float64(union)
}

// adjustmentsAreSimilar checks if two adjustments are similar.
func (e *PatternLearningEngine) adjustmentsAreSimilar(adj1, adj2 LineAdjustment) bool {
	return adj1.Operation == adj2.Operation &&
		math.Abs(float64(adj1.OldStart-adj2.OldStart)) <= 2 &&
		math.Abs(float64(adj1.OldEnd-adj2.OldEnd)) <= 2
}

// SuggestAdjustments suggests adjustments based on learned patterns.
func (e *PatternLearningEngine) SuggestAdjustments(context PatternContext) []AdjustmentSuggestion {
	var suggestions []AdjustmentSuggestion

	for _, pattern := range e.Patterns {
		similarity := e.calculateContextSimilarity(pattern.Context, context)

		if similarity > e.Preferences.PreferredConfidenceThreshold {
			confidence := e.calculateSuggestionConfidence(pattern, similarity)

			suggestion := AdjustmentSuggestion{
				Adjustment: pattern.Adjustment,
				Confidence: confidence,
				Reason:     e.generateReason(pattern, similarity),
				PatternID:  pattern.ID,
				Context:    context,
			}

			suggestions = append(suggestions, suggestion)
		}
	}

	// Sort by confidence descending
	sort.Slice(suggestions, func(i, j int) bool {
		return suggestions[i].Confidence > suggestions[j].Confidence
	})

	// Limit suggestions
	if len(suggestions) > 5 {
		suggestions = suggestions[:5]
	}

	return suggestions
}

// AdjustmentSuggestion represents a suggested adjustment with confidence.
type AdjustmentSuggestion struct {
	Adjustment LineAdjustment    `json:"adjustment"`
	Confidence MLConfidenceLevel `json:"confidence"`
	Reason     string            `json:"reason"`
	PatternID  string            `json:"patternId"`
	Context    PatternContext    `json:"context"`
}

// calculateSuggestionConfidence calculates the confidence level for a suggestion.
func (e *PatternLearningEngine) calculateSuggestionConfidence(pattern AdjustmentPattern, similarity float64) MLConfidenceLevel {
	// Combine pattern confidence, accuracy, and context similarity
	baseConfidence := (pattern.Confidence + pattern.Accuracy + similarity) / 3.0

	// Adjust based on recency
	daysSinceLastSeen := time.Since(pattern.LastSeen).Hours() / 24
	recencyFactor := math.Max(0.5, 1.0-daysSinceLastSeen/30.0) // Decay over 30 days

	finalConfidence := baseConfidence * recencyFactor

	switch {
	case finalConfidence >= 0.8:
		return MLConfidenceHigh
	case finalConfidence >= 0.6:
		return MLConfidenceMedium
	default:
		return MLConfidenceLow
	}
}

// generateReason generates a human-readable reason for the suggestion.
func (e *PatternLearningEngine) generateReason(pattern AdjustmentPattern, similarity float64) string {
	reasons := []string{}

	if pattern.Accuracy > 0.8 {
		reasons = append(reasons, fmt.Sprintf("High accuracy pattern (%.0f%%)", pattern.Accuracy*100))
	}

	if pattern.Occurrences > 5 {
		reasons = append(reasons, fmt.Sprintf("Frequently used pattern (%d times)", pattern.Occurrences))
	}

	if similarity > 0.9 {
		reasons = append(reasons, "Very similar context to previous successful adjustments")
	} else if similarity > 0.8 {
		reasons = append(reasons, "Similar context to previous adjustments")
	}

	if len(reasons) == 0 {
		return "Based on learned patterns"
	}

	return strings.Join(reasons, ", ")
}

// updateStatistics updates learning statistics.
func (e *PatternLearningEngine) updateStatistics(correction UserCorrection) {
	e.Stats.TotalSuggestions++
	e.Stats.LastUpdated = time.Now()

	switch correction.CorrectionType {
	case CorrectionAccept:
		e.Stats.AcceptedSuggestions++
	case CorrectionReject:
		e.Stats.RejectedSuggestions++
	}

	// Update confidence distribution
	e.Stats.ConfidenceDistribution[correction.Confidence]++

	// Recalculate accuracy rate
	if e.Stats.TotalSuggestions > 0 {
		e.Stats.AccuracyRate = float64(e.Stats.AcceptedSuggestions) / float64(e.Stats.TotalSuggestions)
	}

	// Calculate improvement rate (simplified)
	if len(e.Corrections) >= 10 {
		recentCorrections := e.Corrections[len(e.Corrections)-10:]
		recentAccepted := 0
		for _, corr := range recentCorrections {
			if corr.CorrectionType == CorrectionAccept {
				recentAccepted++
			}
		}
		recentAccuracy := float64(recentAccepted) / 10.0

		if e.Stats.AccuracyRate > 0 {
			e.Stats.ImprovementRate = (recentAccuracy - e.Stats.AccuracyRate) / e.Stats.AccuracyRate
		}
	}
}

// updatePreferences updates user preferences based on corrections.
func (e *PatternLearningEngine) updatePreferences(correction UserCorrection) {
	// Update confidence threshold based on user behavior
	switch correction.CorrectionType {
	case CorrectionAccept:
		if correction.Confidence == MLConfidenceLow {
			// User accepted low confidence, maybe lower threshold
			e.Preferences.PreferredConfidenceThreshold = math.Max(0.5, e.Preferences.PreferredConfidenceThreshold-0.01)
		}
	case CorrectionReject:
		if correction.Confidence == MLConfidenceHigh {
			// User rejected high confidence, maybe raise threshold
			e.Preferences.PreferredConfidenceThreshold = math.Min(0.9, e.Preferences.PreferredConfidenceThreshold+0.01)
		}
	}

	// Update file type preferences
	fileType := correction.Context.FileType
	if _, exists := e.Preferences.FileTypePreferences[fileType]; !exists {
		e.Preferences.FileTypePreferences[fileType] = FileTypePrefs{
			AdjustmentTolerance: 0.5,
			PreferredStrategy:   "auto",
			ContextSensitivity:  0.7,
		}
	}

	filePrefs := e.Preferences.FileTypePreferences[fileType]

	// Adjust tolerance based on correction type
	switch correction.CorrectionType {
	case CorrectionAccept:
		filePrefs.AdjustmentTolerance = math.Min(1.0, filePrefs.AdjustmentTolerance+0.05)
	case CorrectionReject:
		filePrefs.AdjustmentTolerance = math.Max(0.1, filePrefs.AdjustmentTolerance-0.05)
	}

	e.Preferences.FileTypePreferences[fileType] = filePrefs
	e.Preferences.UpdatedAt = time.Now()
}

// prunePatterns removes old or low-performing patterns.
func (e *PatternLearningEngine) prunePatterns() {
	// Remove patterns that are too old or have poor accuracy
	var keptPatterns []AdjustmentPattern

	for _, pattern := range e.Patterns {
		daysSinceLastSeen := time.Since(pattern.LastSeen).Hours() / 24

		// Keep pattern if it's recent, has good accuracy, or is frequently used
		if daysSinceLastSeen <= 90 && pattern.Accuracy >= 0.3 && pattern.Occurrences >= 1 {
			keptPatterns = append(keptPatterns, pattern)
		}
	}

	e.Patterns = keptPatterns

	// Limit total patterns to prevent memory bloat
	if len(e.Patterns) > 1000 {
		// Keep top 1000 by confidence * accuracy
		sort.Slice(e.Patterns, func(i, j int) bool {
			scoreI := e.Patterns[i].Confidence * e.Patterns[i].Accuracy
			scoreJ := e.Patterns[j].Confidence * e.Patterns[j].Accuracy
			return scoreI > scoreJ
		})
		e.Patterns = e.Patterns[:1000]
	}
}

// GetLearningInsights provides insights into the learning performance.
func (e *PatternLearningEngine) GetLearningInsights() *LearningInsights {
	insights := &LearningInsights{
		Statistics:        e.Stats,
		TopPatterns:       e.getTopPatterns(10),
		RecentCorrections: e.getRecentCorrections(20),
		Recommendations:   e.generateRecommendations(),
	}

	return insights
}

// LearningInsights provides insights into learning performance and patterns.
type LearningInsights struct {
	Statistics        LearningStatistics  `json:"statistics"`
	TopPatterns       []AdjustmentPattern `json:"topPatterns"`
	RecentCorrections []UserCorrection    `json:"recentCorrections"`
	Recommendations   []string            `json:"recommendations"`
}

// getTopPatterns returns the top patterns by performance.
func (e *PatternLearningEngine) getTopPatterns(limit int) []AdjustmentPattern {
	// Create a copy and sort by performance score
	patterns := make([]AdjustmentPattern, len(e.Patterns))
	copy(patterns, e.Patterns)

	sort.Slice(patterns, func(i, j int) bool {
		scoreI := patterns[i].Confidence * patterns[i].Accuracy * math.Log(float64(patterns[i].Occurrences+1))
		scoreJ := patterns[j].Confidence * patterns[j].Accuracy * math.Log(float64(patterns[j].Occurrences+1))
		return scoreI > scoreJ
	})

	if limit > 0 && limit < len(patterns) {
		return patterns[:limit]
	}
	return patterns
}

// getRecentCorrections returns recent corrections.
func (e *PatternLearningEngine) getRecentCorrections(limit int) []UserCorrection {
	corrections := make([]UserCorrection, len(e.Corrections))
	copy(corrections, e.Corrections)

	sort.Slice(corrections, func(i, j int) bool {
		return corrections[i].Timestamp.After(corrections[j].Timestamp)
	})

	if limit > 0 && limit < len(corrections) {
		return corrections[:limit]
	}
	return corrections
}

// generateRecommendations generates recommendations for improving accuracy.
func (e *PatternLearningEngine) generateRecommendations() []string {
	var recommendations []string

	if e.Stats.AccuracyRate < 0.7 {
		recommendations = append(recommendations, "Consider reviewing and correcting more suggestions to improve accuracy")
	}

	if e.Stats.TotalSuggestions < 50 {
		recommendations = append(recommendations, "More data needed - continue using the system to improve suggestions")
	}

	if len(e.Patterns) < 10 {
		recommendations = append(recommendations, "Limited patterns learned - try using the system across different file types")
	}

	highRejectRate := float64(e.Stats.RejectedSuggestions) / float64(e.Stats.TotalSuggestions)
	if highRejectRate > 0.5 {
		recommendations = append(recommendations, "High rejection rate detected - consider adjusting confidence threshold")
	}

	return recommendations
}

// ExportLearningData exports the learning data for backup or analysis.
func (e *PatternLearningEngine) ExportLearningData() ([]byte, error) {
	data, err := json.MarshalIndent(e, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("failed to marshal learning data: %w", err)
	}
	return data, nil
}

// ImportLearningData imports learning data from backup.
func (e *PatternLearningEngine) ImportLearningData(data []byte) error {
	if err := json.Unmarshal(data, e); err != nil {
		return fmt.Errorf("failed to unmarshal learning data: %w", err)
	}
	return nil
}
