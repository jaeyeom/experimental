package models

import (
	"fmt"
	"sort"
	"strings"
	"time"
)

// UndoOperation represents a single undoable operation.
type UndoOperation struct {
	ID          string                 `json:"id"`
	Type        UndoOperationType      `json:"type"`
	Timestamp   time.Time              `json:"timestamp"`
	Description string                 `json:"description"`
	Repository  Repository             `json:"repository"`
	Identifier  string                 `json:"identifier"` // PR number or branch name
	File        string                 `json:"file,omitempty"`
	Data        map[string]interface{} `json:"data"` // Operation-specific data
}

// UndoOperationType represents the type of operation that can be undone.
type UndoOperationType string

const (
	UndoTypeAdjustment UndoOperationType = "adjustment"
	UndoTypeComment    UndoOperationType = "comment"
	UndoTypeMerge      UndoOperationType = "merge"
	UndoTypeResolve    UndoOperationType = "resolve"
	UndoTypeDelete     UndoOperationType = "delete"
)

// UndoSnapshot represents the state before an operation for undo purposes.
type UndoSnapshot struct {
	Comments          []Comment              `json:"comments"`
	AdjustmentHistory []LineAdjustment       `json:"adjustmentHistory,omitempty"`
	Metadata          map[string]interface{} `json:"metadata,omitempty"`
}

// UndoHistory manages the history of operations for undo functionality.
type UndoHistory struct {
	Operations []UndoOperation `json:"operations"`
	MaxEntries int             `json:"maxEntries"`
}

// NewUndoHistory creates a new undo history manager.
func NewUndoHistory(maxEntries int) *UndoHistory {
	if maxEntries <= 0 {
		maxEntries = 50 // Default max entries
	}
	return &UndoHistory{
		Operations: make([]UndoOperation, 0, maxEntries),
		MaxEntries: maxEntries,
	}
}

// RecordAdjustmentOperation records an adjustment operation for undo.
func (h *UndoHistory) RecordAdjustmentOperation(
	repo Repository,
	identifier string,
	file string,
	adjustments []LineAdjustment,
	beforeComments []Comment,
	afterComments []Comment,
) string {
	op := UndoOperation{
		ID:          GenerateCommentID(),
		Type:        UndoTypeAdjustment,
		Timestamp:   time.Now(),
		Description: fmt.Sprintf("Adjustment operation on %s (%d adjustments)", file, len(adjustments)),
		Repository:  repo,
		Identifier:  identifier,
		File:        file,
		Data: map[string]interface{}{
			"adjustments":    adjustments,
			"beforeComments": beforeComments,
			"afterComments":  afterComments,
		},
	}

	h.addOperation(op)
	return op.ID
}

// RecordCommentOperation records a comment add/edit operation for undo.
func (h *UndoHistory) RecordCommentOperation(
	repo Repository,
	identifier string,
	operationType UndoOperationType,
	comment Comment,
	previousComment *Comment,
) string {
	op := UndoOperation{
		ID:          GenerateCommentID(),
		Type:        operationType,
		Timestamp:   time.Now(),
		Description: fmt.Sprintf("Comment %s on %s:%d", operationType, comment.Path, comment.Line),
		Repository:  repo,
		Identifier:  identifier,
		File:        comment.Path,
		Data: map[string]interface{}{
			"comment": comment,
		},
	}

	if previousComment != nil {
		op.Data["previousComment"] = *previousComment
	}

	h.addOperation(op)
	return op.ID
}

// RecordMergeOperation records a merge operation for undo.
func (h *UndoHistory) RecordMergeOperation(
	repo Repository,
	identifier string,
	file string,
	conflicts []MergeConflict,
	mergedComments []Comment,
	originalComments []Comment,
) string {
	op := UndoOperation{
		ID:          GenerateCommentID(),
		Type:        UndoTypeMerge,
		Timestamp:   time.Now(),
		Description: fmt.Sprintf("Smart merge operation on %s (%d conflicts resolved)", file, len(conflicts)),
		Repository:  repo,
		Identifier:  identifier,
		File:        file,
		Data: map[string]interface{}{
			"conflicts":        conflicts,
			"mergedComments":   mergedComments,
			"originalComments": originalComments,
		},
	}

	h.addOperation(op)
	return op.ID
}

// addOperation adds a new operation to the history, managing size limits.
func (h *UndoHistory) addOperation(op UndoOperation) {
	h.Operations = append(h.Operations, op)

	// Enforce size limit
	if len(h.Operations) > h.MaxEntries {
		// Remove oldest operations
		h.Operations = h.Operations[len(h.Operations)-h.MaxEntries:]
	}
}

// GetOperationByID retrieves an operation by its ID.
func (h *UndoHistory) GetOperationByID(id string) (*UndoOperation, error) {
	for _, op := range h.Operations {
		if op.ID == id || strings.HasPrefix(op.ID, id) {
			return &op, nil
		}
	}
	return nil, fmt.Errorf("operation with ID %s not found", id)
}

// GetOperationsByTimestamp retrieves operations within a time range.
func (h *UndoHistory) GetOperationsByTimestamp(since, until time.Time) []UndoOperation {
	var matching []UndoOperation
	for _, op := range h.Operations {
		if (since.IsZero() || op.Timestamp.After(since)) &&
			(until.IsZero() || op.Timestamp.Before(until)) {
			matching = append(matching, op)
		}
	}
	return matching
}

// GetOperationsByFile retrieves operations for a specific file.
func (h *UndoHistory) GetOperationsByFile(repo Repository, identifier, file string) []UndoOperation {
	var matching []UndoOperation
	for _, op := range h.Operations {
		if op.Repository.Owner == repo.Owner &&
			op.Repository.Name == repo.Name &&
			op.Identifier == identifier &&
			(file == "" || op.File == file) {
			matching = append(matching, op)
		}
	}
	return matching
}

// GetRecentOperations retrieves the most recent operations.
func (h *UndoHistory) GetRecentOperations(limit int) []UndoOperation {
	// Sort by timestamp descending
	sorted := make([]UndoOperation, len(h.Operations))
	copy(sorted, h.Operations)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].Timestamp.After(sorted[j].Timestamp)
	})

	if limit > 0 && limit < len(sorted) {
		return sorted[:limit]
	}
	return sorted
}

// UndoManager provides high-level undo functionality.
type UndoManager struct {
	history *UndoHistory
}

// NewUndoManager creates a new undo manager.
func NewUndoManager(maxHistoryEntries int) *UndoManager {
	return &UndoManager{
		history: NewUndoHistory(maxHistoryEntries),
	}
}

// GetHistory returns the undo history.
func (m *UndoManager) GetHistory() *UndoHistory {
	return m.history
}

// RecordAdjustment records an adjustment operation.
func (m *UndoManager) RecordAdjustment(
	repo Repository,
	identifier string,
	file string,
	adjustments []LineAdjustment,
	beforeComments []Comment,
	afterComments []Comment,
) string {
	return m.history.RecordAdjustmentOperation(repo, identifier, file, adjustments, beforeComments, afterComments)
}

// UndoAdjustment reverses an adjustment operation.
func (m *UndoManager) UndoAdjustment(operationID string) ([]Comment, error) {
	op, err := m.history.GetOperationByID(operationID)
	if err != nil {
		return nil, fmt.Errorf("operation not found: %w", err)
	}

	if op.Type != UndoTypeAdjustment {
		return nil, fmt.Errorf("operation %s is not an adjustment operation", operationID)
	}

	// Extract before state
	beforeComments, ok := op.Data["beforeComments"].([]Comment)
	if !ok {
		return nil, fmt.Errorf("invalid operation data: missing beforeComments")
	}

	return beforeComments, nil
}

// UndoCommentOperation reverses a comment add/edit/delete operation.
func (m *UndoManager) UndoCommentOperation(operationID string) (*CommentUndoResult, error) {
	op, err := m.history.GetOperationByID(operationID)
	if err != nil {
		return nil, fmt.Errorf("operation not found: %w", err)
	}

	result := &CommentUndoResult{
		OperationType: op.Type,
		File:          op.File,
	}

	switch op.Type {
	case UndoTypeComment:
		// For add operations, we need to remove the comment
		comment, ok := op.Data["comment"].(Comment)
		if !ok {
			return nil, fmt.Errorf("invalid operation data: missing comment")
		}
		result.RemovedComment = &comment

		// Check if there was a previous comment (edit operation)
		if prevComment, exists := op.Data["previousComment"]; exists {
			if prev, ok := prevComment.(Comment); ok {
				result.RestoredComment = &prev
			}
		}

	case UndoTypeDelete:
		// For delete operations, we need to restore the comment
		comment, ok := op.Data["comment"].(Comment)
		if !ok {
			return nil, fmt.Errorf("invalid operation data: missing comment")
		}
		result.RestoredComment = &comment

	case UndoTypeResolve:
		// For resolve operations, we need to unresolve the comment
		comment, ok := op.Data["comment"].(Comment)
		if !ok {
			return nil, fmt.Errorf("invalid operation data: missing comment")
		}
		// Restore original status
		comment.Status = StatusUnresolved
		comment.ResolvedAt = nil
		comment.ResolutionReason = ""
		result.RestoredComment = &comment

	default:
		return nil, fmt.Errorf("cannot undo operation type %s", op.Type)
	}

	return result, nil
}

// UndoMergeOperation reverses a merge operation.
func (m *UndoManager) UndoMergeOperation(operationID string) ([]Comment, error) {
	op, err := m.history.GetOperationByID(operationID)
	if err != nil {
		return nil, fmt.Errorf("operation not found: %w", err)
	}

	if op.Type != UndoTypeMerge {
		return nil, fmt.Errorf("operation %s is not a merge operation", operationID)
	}

	// Extract original comments before merge
	originalComments, ok := op.Data["originalComments"].([]Comment)
	if !ok {
		return nil, fmt.Errorf("invalid operation data: missing originalComments")
	}

	return originalComments, nil
}

// CommentUndoResult represents the result of undoing a comment operation.
type CommentUndoResult struct {
	OperationType   UndoOperationType `json:"operationType"`
	File            string            `json:"file"`
	RemovedComment  *Comment          `json:"removedComment,omitempty"`
	RestoredComment *Comment          `json:"restoredComment,omitempty"`
}

// UndoService provides a high-level service for undo operations.
type UndoService struct {
	manager *UndoManager
}

// NewUndoService creates a new undo service.
func NewUndoService() *UndoService {
	return &UndoService{
		manager: NewUndoManager(100), // Default 100 operations
	}
}

// GetManager returns the underlying undo manager.
func (s *UndoService) GetManager() *UndoManager {
	return s.manager
}

// ListRecentOperations lists recent operations for a specific context.
func (s *UndoService) ListRecentOperations(repo Repository, identifier string, limit int) []UndoOperation {
	allOps := s.manager.GetHistory().GetOperationsByFile(repo, identifier, "")

	// Sort by timestamp descending
	sort.Slice(allOps, func(i, j int) bool {
		return allOps[i].Timestamp.After(allOps[j].Timestamp)
	})

	if limit > 0 && limit < len(allOps) {
		return allOps[:limit]
	}
	return allOps
}

// FindOperationsByTimeRange finds operations within a specific time range.
func (s *UndoService) FindOperationsByTimeRange(since time.Time, _ int) []UndoOperation {
	return s.manager.GetHistory().GetOperationsByTimestamp(since, time.Time{})
}

// ValidateUndoOperation validates that an operation can be safely undone.
func (s *UndoService) ValidateUndoOperation(operationID string) error {
	op, err := s.manager.GetHistory().GetOperationByID(operationID)
	if err != nil {
		return err
	}

	// Check if operation is too old (optional safety check)
	if time.Since(op.Timestamp) > 24*time.Hour {
		return fmt.Errorf("operation %s is too old to undo safely (older than 24 hours)", operationID)
	}

	// Check for dependent operations (operations that might depend on this one)
	recentOps := s.manager.GetHistory().GetOperationsByFile(op.Repository, op.Identifier, op.File)
	for _, recentOp := range recentOps {
		if recentOp.Timestamp.After(op.Timestamp) && recentOp.ID != op.ID {
			// There are newer operations on the same file
			// This could potentially cause conflicts
			// TODO: Implement dependency analysis for conflict detection
			return fmt.Errorf("potential conflict detected with operation %s", recentOp.ID)
		}
	}

	return nil
}

// GetUndoPreview provides a preview of what will happen when an operation is undone.
func (s *UndoService) GetUndoPreview(operationID string) (*UndoPreview, error) {
	op, err := s.manager.GetHistory().GetOperationByID(operationID)
	if err != nil {
		return nil, err
	}

	preview := &UndoPreview{
		Operation:   *op,
		CanUndo:     true,
		Warnings:    []string{},
		Description: fmt.Sprintf("Undo %s operation from %s", op.Type, op.Timestamp.Format("2006-01-02 15:04:05")),
	}

	// Check for potential conflicts
	if err := s.ValidateUndoOperation(operationID); err != nil {
		preview.Warnings = append(preview.Warnings, err.Error())
	}

	// Add specific preview information based on operation type
	switch op.Type {
	case UndoTypeAdjustment:
		if beforeComments, ok := op.Data["beforeComments"].([]Comment); ok {
			preview.Description = fmt.Sprintf("Restore %d comments to their previous line positions", len(beforeComments))
		}
	case UndoTypeComment:
		if comment, ok := op.Data["comment"].(Comment); ok {
			preview.Description = fmt.Sprintf("Remove comment from %s:%d", comment.Path, comment.Line)
		}
	case UndoTypeMerge:
		if originalComments, ok := op.Data["originalComments"].([]Comment); ok {
			preview.Description = fmt.Sprintf("Restore %d original comments before merge", len(originalComments))
		}
	}

	return preview, nil
}

// UndoPreview provides information about what will happen when an operation is undone.
type UndoPreview struct {
	Operation   UndoOperation `json:"operation"`
	CanUndo     bool          `json:"canUndo"`
	Warnings    []string      `json:"warnings"`
	Description string        `json:"description"`
}
