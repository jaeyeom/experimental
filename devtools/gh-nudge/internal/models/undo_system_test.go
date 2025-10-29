package models

import (
	"testing"
	"time"
)

// Test fixtures

func createTestRepository() Repository {
	return Repository{
		Owner: "testowner",
		Name:  "testrepo",
	}
}

func createTestUndoOperation(opType UndoOperationType, file string) UndoOperation {
	return UndoOperation{
		ID:          GenerateCommentID(),
		Type:        opType,
		Timestamp:   time.Now(),
		Description: "Test operation",
		Repository:  createTestRepository(),
		Identifier:  "123",
		File:        file,
		Data:        make(map[string]interface{}),
	}
}

// UndoHistory Tests

func TestNewUndoHistory(t *testing.T) {
	t.Run("creates history with specified max entries", func(t *testing.T) {
		history := NewUndoHistory(50)

		if history == nil {
			t.Fatal("Expected non-nil history")
		}
		if history.MaxEntries != 50 {
			t.Errorf("Expected MaxEntries = 50, got %d", history.MaxEntries)
		}
		if len(history.Operations) != 0 {
			t.Errorf("Expected empty operations list, got %d operations", len(history.Operations))
		}
	})

	t.Run("uses default max entries for zero or negative values", func(t *testing.T) {
		tests := []struct {
			name      string
			maxEntry  int
			wantEntry int
		}{
			{"zero", 0, 50},
			{"negative", -10, 50},
		}

		for _, tt := range tests {
			t.Run(tt.name, func(t *testing.T) {
				history := NewUndoHistory(tt.maxEntry)
				if history.MaxEntries != tt.wantEntry {
					t.Errorf("Expected MaxEntries = %d, got %d", tt.wantEntry, history.MaxEntries)
				}
			})
		}
	})
}

func TestUndoHistory_RecordAdjustmentOperation(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()

	adjustments := []LineAdjustment{
		{OldStart: 10, OldEnd: 12, NewStart: 8, NewEnd: 10, Operation: OperationDelete},
		{OldStart: 20, OldEnd: 20, NewStart: 23, NewEnd: 23, Operation: OperationInsert},
	}

	beforeComments := []Comment{
		{ID: "1", Path: "file.go", Line: NewSingleLine(10)},
		{ID: "2", Path: "file.go", Line: NewSingleLine(20)},
	}

	afterComments := []Comment{
		{ID: "1", Path: "file.go", Line: NewSingleLine(8)},
		{ID: "2", Path: "file.go", Line: NewSingleLine(23)},
	}

	t.Run("records adjustment operation successfully", func(t *testing.T) {
		opID := history.RecordAdjustmentOperation(repo, "123", "file.go", adjustments, beforeComments, afterComments)

		if opID == "" {
			t.Fatal("Expected non-empty operation ID")
		}

		if len(history.Operations) != 1 {
			t.Fatalf("Expected 1 operation, got %d", len(history.Operations))
		}

		op := history.Operations[0]
		if op.Type != UndoTypeAdjustment {
			t.Errorf("Expected type %s, got %s", UndoTypeAdjustment, op.Type)
		}
		if op.File != "file.go" {
			t.Errorf("Expected file 'file.go', got '%s'", op.File)
		}

		// Verify data fields
		if _, ok := op.Data["adjustments"]; !ok {
			t.Error("Expected 'adjustments' in operation data")
		}
		if _, ok := op.Data["beforeComments"]; !ok {
			t.Error("Expected 'beforeComments' in operation data")
		}
		if _, ok := op.Data["afterComments"]; !ok {
			t.Error("Expected 'afterComments' in operation data")
		}
	})
}

func TestUndoHistory_RecordCommentOperation(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()

	comment := Comment{
		ID:   "comment1",
		Path: "test.go",
		Line: NewSingleLine(15),
		Body: "Test comment",
	}

	t.Run("records comment addition without previous comment", func(t *testing.T) {
		opID := history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		if opID == "" {
			t.Fatal("Expected non-empty operation ID")
		}

		if len(history.Operations) != 1 {
			t.Fatalf("Expected 1 operation, got %d", len(history.Operations))
		}

		op := history.Operations[0]
		if op.Type != UndoTypeComment {
			t.Errorf("Expected type %s, got %s", UndoTypeComment, op.Type)
		}

		if _, ok := op.Data["previousComment"]; ok {
			t.Error("Expected no 'previousComment' in data")
		}
	})

	t.Run("records comment edit with previous comment", func(t *testing.T) {
		history := NewUndoHistory(10)
		previousComment := Comment{
			ID:   "comment1",
			Path: "test.go",
			Line: NewSingleLine(15),
			Body: "Old comment",
		}

		opID := history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, &previousComment)

		if opID == "" {
			t.Fatal("Expected non-empty operation ID")
		}

		op := history.Operations[0]
		if _, ok := op.Data["previousComment"]; !ok {
			t.Error("Expected 'previousComment' in data")
		}
	})
}

func TestUndoHistory_RecordMergeOperation(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()

	conflicts := []MergeConflict{
		{
			Location: NewFileLocationSingleLine("test.go", 10),
			ConflictingComments: []CommentWithContext{
				{Comment: Comment{ID: "c1", Path: "test.go", Line: NewSingleLine(10)}},
				{Comment: Comment{ID: "c2", Path: "test.go", Line: NewSingleLine(10)}},
			},
			SuggestedStrategy: StrategyConcat,
		},
	}

	mergedComments := []Comment{
		{ID: "merged1", Path: "test.go", Line: NewSingleLine(10), Body: "Merged comment"},
	}

	originalComments := []Comment{
		{ID: "c1", Path: "test.go", Line: NewSingleLine(10), Body: "Comment 1"},
		{ID: "c2", Path: "test.go", Line: NewSingleLine(10), Body: "Comment 2"},
	}

	t.Run("records merge operation successfully", func(t *testing.T) {
		opID := history.RecordMergeOperation(repo, "123", "test.go", conflicts, mergedComments, originalComments)

		if opID == "" {
			t.Fatal("Expected non-empty operation ID")
		}

		if len(history.Operations) != 1 {
			t.Fatalf("Expected 1 operation, got %d", len(history.Operations))
		}

		op := history.Operations[0]
		if op.Type != UndoTypeMerge {
			t.Errorf("Expected type %s, got %s", UndoTypeMerge, op.Type)
		}

		// Verify all required data fields
		requiredFields := []string{"conflicts", "mergedComments", "originalComments"}
		for _, field := range requiredFields {
			if _, ok := op.Data[field]; !ok {
				t.Errorf("Expected '%s' in operation data", field)
			}
		}
	})
}

func TestUndoHistory_SizeLimit(t *testing.T) {
	t.Run("enforces max entries limit", func(t *testing.T) {
		history := NewUndoHistory(3)
		repo := createTestRepository()

		// Add 5 operations
		for i := 0; i < 5; i++ {
			comment := Comment{ID: GenerateCommentID(), Path: "test.go", Line: NewSingleLine(i)}
			history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)
		}

		// Should only keep the last 3
		if len(history.Operations) != 3 {
			t.Errorf("Expected 3 operations after size limit, got %d", len(history.Operations))
		}
	})
}

func TestUndoHistory_GetOperationByID(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()

	comment := Comment{ID: "test", Path: "test.go", Line: NewSingleLine(1)}
	opID := history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

	t.Run("retrieves operation by full ID", func(t *testing.T) {
		op, err := history.GetOperationByID(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}
		if op.ID != opID {
			t.Errorf("Expected ID %s, got %s", opID, op.ID)
		}
	})

	t.Run("retrieves operation by ID prefix", func(t *testing.T) {
		prefix := opID[:8]
		op, err := history.GetOperationByID(prefix)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}
		if op.ID != opID {
			t.Errorf("Expected ID %s, got %s", opID, op.ID)
		}
	})

	t.Run("returns error for non-existent ID", func(t *testing.T) {
		_, err := history.GetOperationByID("nonexistent")
		if err == nil {
			t.Error("Expected error for non-existent ID")
		}
	})
}

func TestUndoHistory_GetOperationsByTimestamp(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()
	_ = repo // Avoid unused variable error

	now := time.Now()

	// Create operations with different timestamps
	op1 := createTestUndoOperation(UndoTypeComment, "test.go")
	op1.Timestamp = now.Add(-2 * time.Hour)
	history.Operations = append(history.Operations, op1)

	op2 := createTestUndoOperation(UndoTypeComment, "test.go")
	op2.Timestamp = now.Add(-1 * time.Hour)
	history.Operations = append(history.Operations, op2)

	op3 := createTestUndoOperation(UndoTypeComment, "test.go")
	op3.Timestamp = now
	history.Operations = append(history.Operations, op3)

	t.Run("retrieves operations within time range", func(t *testing.T) {
		since := now.Add(-90 * time.Minute)
		until := now.Add(10 * time.Minute)

		ops := history.GetOperationsByTimestamp(since, until)

		if len(ops) != 2 {
			t.Errorf("Expected 2 operations, got %d", len(ops))
		}
	})

	t.Run("retrieves all operations with zero times", func(t *testing.T) {
		ops := history.GetOperationsByTimestamp(time.Time{}, time.Time{})

		if len(ops) != 3 {
			t.Errorf("Expected 3 operations, got %d", len(ops))
		}
	})
}

func TestUndoHistory_GetOperationsByFile(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()
	_ = repo // Use repo to avoid unused variable error

	// Add operations for different files
	comment1 := Comment{ID: "1", Path: "file1.go", Line: NewSingleLine(1)}
	history.RecordCommentOperation(repo, "123", UndoTypeComment, comment1, nil)

	comment2 := Comment{ID: "2", Path: "file2.go", Line: NewSingleLine(1)}
	history.RecordCommentOperation(repo, "123", UndoTypeComment, comment2, nil)

	comment3 := Comment{ID: "3", Path: "file1.go", Line: NewSingleLine(2)}
	history.RecordCommentOperation(repo, "123", UndoTypeComment, comment3, nil)

	t.Run("retrieves operations for specific file", func(t *testing.T) {
		ops := history.GetOperationsByFile(repo, "123", "file1.go")

		if len(ops) != 2 {
			t.Errorf("Expected 2 operations for file1.go, got %d", len(ops))
		}
	})

	t.Run("retrieves all operations when file is empty", func(t *testing.T) {
		ops := history.GetOperationsByFile(repo, "123", "")

		if len(ops) != 3 {
			t.Errorf("Expected 3 operations, got %d", len(ops))
		}
	})

	t.Run("filters by repository", func(t *testing.T) {
		otherRepo := Repository{Owner: "other", Name: "repo"}
		ops := history.GetOperationsByFile(otherRepo, "123", "")

		if len(ops) != 0 {
			t.Errorf("Expected 0 operations for different repo, got %d", len(ops))
		}
	})
}

func TestUndoHistory_GetRecentOperations(t *testing.T) {
	history := NewUndoHistory(10)
	repo := createTestRepository()

	now := time.Now()

	// Add operations with different timestamps
	for i := 0; i < 5; i++ {
		comment := Comment{ID: GenerateCommentID(), Path: "test.go", Line: NewSingleLine(i)}
		opID := history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)
		op, _ := history.GetOperationByID(opID)
		op.Timestamp = now.Add(time.Duration(-i) * time.Minute)
	}

	t.Run("retrieves limited number of recent operations", func(t *testing.T) {
		ops := history.GetRecentOperations(3)

		if len(ops) != 3 {
			t.Errorf("Expected 3 operations, got %d", len(ops))
		}

		// Verify they're sorted by timestamp descending
		for i := 1; i < len(ops); i++ {
			if ops[i].Timestamp.After(ops[i-1].Timestamp) {
				t.Error("Expected operations sorted by timestamp descending")
			}
		}
	})

	t.Run("retrieves all operations when limit is zero", func(t *testing.T) {
		ops := history.GetRecentOperations(0)

		if len(ops) != 5 {
			t.Errorf("Expected 5 operations, got %d", len(ops))
		}
	})
}

// UndoManager Tests

func TestNewUndoManager(t *testing.T) {
	t.Run("creates manager with history", func(t *testing.T) {
		manager := NewUndoManager(100)

		if manager == nil {
			t.Fatal("Expected non-nil manager")
		}
		if manager.history == nil {
			t.Fatal("Expected non-nil history")
		}
		if manager.history.MaxEntries != 100 {
			t.Errorf("Expected MaxEntries = 100, got %d", manager.history.MaxEntries)
		}
	})
}

func TestUndoManager_RecordAdjustment(t *testing.T) {
	manager := NewUndoManager(100)
	repo := createTestRepository()

	adjustments := []LineAdjustment{{OldStart: 10, OldEnd: 10, NewStart: 8, NewEnd: 8, Operation: OperationDelete}}
	beforeComments := []Comment{{ID: "1", Path: "test.go", Line: NewSingleLine(10)}}
	afterComments := []Comment{{ID: "1", Path: "test.go", Line: NewSingleLine(8)}}

	t.Run("records adjustment and returns operation ID", func(t *testing.T) {
		opID := manager.RecordAdjustment(repo, "123", "test.go", adjustments, beforeComments, afterComments)

		if opID == "" {
			t.Fatal("Expected non-empty operation ID")
		}

		if len(manager.history.Operations) != 1 {
			t.Errorf("Expected 1 operation in history, got %d", len(manager.history.Operations))
		}
	})
}

func TestUndoManager_UndoAdjustment(t *testing.T) {
	manager := NewUndoManager(100)
	repo := createTestRepository()

	beforeComments := []Comment{
		{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "Original position"},
	}
	afterComments := []Comment{
		{ID: "1", Path: "test.go", Line: NewSingleLine(8), Body: "Original position"},
	}
	adjustments := []LineAdjustment{{OldStart: 10, OldEnd: 10, NewStart: 8, NewEnd: 8, Operation: OperationDelete}}

	opID := manager.RecordAdjustment(repo, "123", "test.go", adjustments, beforeComments, afterComments)

	t.Run("successfully undoes adjustment", func(t *testing.T) {
		restoredComments, err := manager.UndoAdjustment(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if len(restoredComments) != 1 {
			t.Fatalf("Expected 1 restored comment, got %d", len(restoredComments))
		}

		if restoredComments[0].Line != NewSingleLine(10) {
			t.Errorf("Expected restored line 10, got %v", restoredComments[0].Line)
		}
	})

	t.Run("returns error for non-existent operation", func(t *testing.T) {
		_, err := manager.UndoAdjustment("nonexistent")

		if err == nil {
			t.Error("Expected error for non-existent operation")
		}
	})

	t.Run("returns error for wrong operation type", func(t *testing.T) {
		comment := Comment{ID: "test", Path: "test.go", Line: NewSingleLine(1)}
		wrongOpID := manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		_, err := manager.UndoAdjustment(wrongOpID)

		if err == nil {
			t.Error("Expected error for wrong operation type")
		}
	})
}

func TestUndoManager_UndoCommentOperation(t *testing.T) {
	manager := NewUndoManager(100)
	repo := createTestRepository()

	t.Run("undoes comment addition", func(t *testing.T) {
		comment := Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "New comment"}
		opID := manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		result, err := manager.UndoCommentOperation(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if result.OperationType != UndoTypeComment {
			t.Errorf("Expected operation type %s, got %s", UndoTypeComment, result.OperationType)
		}

		if result.RemovedComment == nil {
			t.Fatal("Expected removed comment")
		}

		if result.RestoredComment != nil {
			t.Error("Expected no restored comment for addition")
		}
	})

	t.Run("undoes comment edit", func(t *testing.T) {
		oldComment := Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "Old"}
		newComment := Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "New"}

		opID := manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, newComment, &oldComment)

		result, err := manager.UndoCommentOperation(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if result.RemovedComment == nil {
			t.Error("Expected removed comment for edit")
		}

		if result.RestoredComment == nil {
			t.Fatal("Expected restored comment for edit")
		}

		if result.RestoredComment.Body != "Old" {
			t.Errorf("Expected restored comment body 'Old', got '%s'", result.RestoredComment.Body)
		}
	})

	t.Run("undoes comment deletion", func(t *testing.T) {
		comment := Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "Deleted"}
		opID := manager.history.RecordCommentOperation(repo, "123", UndoTypeDelete, comment, nil)

		result, err := manager.UndoCommentOperation(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if result.RestoredComment == nil {
			t.Fatal("Expected restored comment for deletion")
		}

		if result.RemovedComment != nil {
			t.Error("Expected no removed comment for deletion undo")
		}
	})

	t.Run("undoes comment resolution", func(t *testing.T) {
		now := time.Now()
		comment := Comment{
			ID:               "1",
			Path:             "test.go",
			Line:             NewSingleLine(10),
			Status:           StatusResolved,
			ResolvedAt:       &now,
			ResolutionReason: "Fixed",
		}
		opID := manager.history.RecordCommentOperation(repo, "123", UndoTypeResolve, comment, nil)

		result, err := manager.UndoCommentOperation(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if result.RestoredComment == nil {
			t.Fatal("Expected restored comment")
		}

		if result.RestoredComment.Status != StatusUnresolved {
			t.Errorf("Expected status %s, got %s", StatusUnresolved, result.RestoredComment.Status)
		}

		if result.RestoredComment.ResolvedAt != nil {
			t.Error("Expected ResolvedAt to be nil")
		}

		if result.RestoredComment.ResolutionReason != "" {
			t.Error("Expected empty ResolutionReason")
		}
	})
}

func TestUndoManager_UndoMergeOperation(t *testing.T) {
	manager := NewUndoManager(100)
	repo := createTestRepository()

	originalComments := []Comment{
		{ID: "1", Path: "test.go", Line: NewSingleLine(10), Body: "Comment 1"},
		{ID: "2", Path: "test.go", Line: NewSingleLine(10), Body: "Comment 2"},
	}

	mergedComments := []Comment{
		{ID: "merged", Path: "test.go", Line: NewSingleLine(10), Body: "Merged"},
	}

	conflicts := []MergeConflict{
		{
			Location: NewFileLocationSingleLine("test.go", 10),
			ConflictingComments: []CommentWithContext{
				{Comment: Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10)}},
				{Comment: Comment{ID: "2", Path: "test.go", Line: NewSingleLine(10)}},
			},
		},
	}

	opID := manager.history.RecordMergeOperation(repo, "123", "test.go", conflicts, mergedComments, originalComments)

	t.Run("successfully undoes merge", func(t *testing.T) {
		restoredComments, err := manager.UndoMergeOperation(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if len(restoredComments) != 2 {
			t.Fatalf("Expected 2 restored comments, got %d", len(restoredComments))
		}

		// Verify original comments are restored
		if restoredComments[0].ID != "1" || restoredComments[1].ID != "2" {
			t.Error("Expected original comments to be restored")
		}
	})

	t.Run("returns error for wrong operation type", func(t *testing.T) {
		comment := Comment{ID: "test", Path: "test.go", Line: NewSingleLine(1)}
		wrongOpID := manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		_, err := manager.UndoMergeOperation(wrongOpID)

		if err == nil {
			t.Error("Expected error for wrong operation type")
		}
	})
}

// UndoService Tests

func TestNewUndoService(t *testing.T) {
	t.Run("creates service with default settings", func(t *testing.T) {
		service := NewUndoService()

		if service == nil {
			t.Fatal("Expected non-nil service")
		}

		if service.manager == nil {
			t.Fatal("Expected non-nil manager")
		}

		if service.manager.history.MaxEntries != 100 {
			t.Errorf("Expected default max entries 100, got %d", service.manager.history.MaxEntries)
		}
	})
}

func TestUndoService_ListRecentOperations(t *testing.T) {
	service := NewUndoService()
	repo := createTestRepository()

	// Add multiple operations
	for i := 0; i < 5; i++ {
		comment := Comment{ID: GenerateCommentID(), Path: "test.go", Line: NewSingleLine(i)}
		service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)
		time.Sleep(time.Millisecond) // Ensure different timestamps
	}

	t.Run("lists recent operations with limit", func(t *testing.T) {
		ops := service.ListRecentOperations(repo, "123", 3)

		if len(ops) != 3 {
			t.Errorf("Expected 3 operations, got %d", len(ops))
		}

		// Verify descending order
		for i := 1; i < len(ops); i++ {
			if ops[i].Timestamp.After(ops[i-1].Timestamp) {
				t.Error("Expected operations in descending timestamp order")
			}
		}
	})

	t.Run("lists all operations without limit", func(t *testing.T) {
		ops := service.ListRecentOperations(repo, "123", 0)

		if len(ops) != 5 {
			t.Errorf("Expected 5 operations, got %d", len(ops))
		}
	})
}

func TestUndoService_ValidateUndoOperation(t *testing.T) {
	service := NewUndoService()
	repo := createTestRepository()

	t.Run("validates recent operation", func(t *testing.T) {
		comment := Comment{ID: "1", Path: "test.go", Line: NewSingleLine(1)}
		opID := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		err := service.ValidateUndoOperation(opID)
		if err != nil {
			t.Errorf("Expected no error for recent operation, got %v", err)
		}
	})

	t.Run("rejects operation older than 24 hours", func(t *testing.T) {
		// Create operation with old timestamp directly in the history
		oldOp := UndoOperation{
			ID:          GenerateCommentID(),
			Type:        UndoTypeComment,
			Timestamp:   time.Now().Add(-25 * time.Hour),
			Description: "Old operation",
			Repository:  repo,
			Identifier:  "123",
			File:        "test.go",
			Data: map[string]interface{}{
				"comment": Comment{ID: "2", Path: "test.go", Line: NewSingleLine(2)},
			},
		}
		service.manager.history.Operations = append(service.manager.history.Operations, oldOp)

		err := service.ValidateUndoOperation(oldOp.ID)

		if err == nil {
			t.Error("Expected error for old operation")
		}
	})

	t.Run("detects potential conflicts with newer operations", func(t *testing.T) {
		comment1 := Comment{ID: "3", Path: "conflict.go", Line: NewSingleLine(10)}
		opID1 := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment1, nil)

		time.Sleep(10 * time.Millisecond)

		comment2 := Comment{ID: "4", Path: "conflict.go", Line: NewSingleLine(15)}
		service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment2, nil)

		err := service.ValidateUndoOperation(opID1)

		if err == nil {
			t.Error("Expected error for operation with newer operations on same file")
		}
	})

	t.Run("returns error for non-existent operation", func(t *testing.T) {
		err := service.ValidateUndoOperation("nonexistent")

		if err == nil {
			t.Error("Expected error for non-existent operation")
		}
	})
}

func TestUndoService_GetUndoPreview(t *testing.T) {
	service := NewUndoService()
	repo := createTestRepository()

	t.Run("generates preview for adjustment operation", func(t *testing.T) {
		beforeComments := []Comment{
			{ID: "1", Path: "test.go", Line: NewSingleLine(10)},
			{ID: "2", Path: "test.go", Line: NewSingleLine(20)},
		}
		afterComments := []Comment{
			{ID: "1", Path: "test.go", Line: NewSingleLine(8)},
			{ID: "2", Path: "test.go", Line: NewSingleLine(23)},
		}
		adjustments := []LineAdjustment{{OldStart: 10, OldEnd: 10, NewStart: 8, NewEnd: 8, Operation: OperationDelete}}

		opID := service.manager.RecordAdjustment(repo, "123", "test.go", adjustments, beforeComments, afterComments)

		preview, err := service.GetUndoPreview(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if preview == nil {
			t.Fatal("Expected non-nil preview")
		}

		if preview.Operation.ID != opID {
			t.Error("Expected preview to contain operation details")
		}

		if preview.Description == "" {
			t.Error("Expected non-empty description")
		}
	})

	t.Run("generates preview for comment operation", func(t *testing.T) {
		comment := Comment{ID: "1", Path: "main.go", Line: NewSingleLine(42), Body: "Test"}
		opID := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment, nil)

		preview, err := service.GetUndoPreview(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if preview.Description == "" {
			t.Error("Expected description for comment operation")
		}
	})

	t.Run("generates preview for merge operation", func(t *testing.T) {
		originalComments := []Comment{
			{ID: "1", Path: "test.go", Line: NewSingleLine(10)},
			{ID: "2", Path: "test.go", Line: NewSingleLine(10)},
		}
		mergedComments := []Comment{{ID: "merged", Path: "test.go", Line: NewSingleLine(10)}}
		conflicts := []MergeConflict{{
			Location: NewFileLocationSingleLine("test.go", 10),
			ConflictingComments: []CommentWithContext{
				{Comment: Comment{ID: "1", Path: "test.go", Line: NewSingleLine(10)}},
				{Comment: Comment{ID: "2", Path: "test.go", Line: NewSingleLine(10)}},
			},
		}}

		opID := service.manager.history.RecordMergeOperation(repo, "123", "test.go", conflicts, mergedComments, originalComments)

		preview, err := service.GetUndoPreview(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if preview.Description == "" {
			t.Error("Expected description for merge operation")
		}
	})

	t.Run("includes warnings for operations with conflicts", func(t *testing.T) {
		comment1 := Comment{ID: "1", Path: "warn.go", Line: NewSingleLine(10)}
		opID := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment1, nil)

		// Create a newer operation on same file
		time.Sleep(10 * time.Millisecond)
		comment2 := Comment{ID: "2", Path: "warn.go", Line: NewSingleLine(15)}
		service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment2, nil)

		preview, err := service.GetUndoPreview(opID)
		if err != nil {
			t.Fatalf("Expected no error, got %v", err)
		}

		if len(preview.Warnings) == 0 {
			t.Error("Expected warnings for operation with conflicts")
		}
	})
}

// Edge Cases and Integration Tests

func TestUndoSystem_CompleteWorkflow(t *testing.T) {
	t.Run("complete adjustment workflow", func(t *testing.T) {
		service := NewUndoService()
		repo := createTestRepository()

		// Step 1: Record an adjustment
		beforeComments := []Comment{
			{ID: "1", Path: "workflow.go", Line: NewSingleLine(100), Body: "Original comment"},
		}
		afterComments := []Comment{
			{ID: "1", Path: "workflow.go", Line: NewSingleLine(98), Body: "Original comment"},
		}
		adjustments := []LineAdjustment{{OldStart: 100, OldEnd: 100, NewStart: 98, NewEnd: 98, Operation: OperationDelete}}

		opID := service.manager.RecordAdjustment(repo, "123", "workflow.go", adjustments, beforeComments, afterComments)

		// Step 2: Validate the operation
		if err := service.ValidateUndoOperation(opID); err != nil {
			t.Errorf("Expected valid operation, got error: %v", err)
		}

		// Step 3: Preview the undo
		preview, err := service.GetUndoPreview(opID)
		if err != nil {
			t.Fatalf("Expected successful preview, got error: %v", err)
		}
		if !preview.CanUndo {
			t.Error("Expected operation to be undoable")
		}

		// Step 4: Perform the undo
		restoredComments, err := service.manager.UndoAdjustment(opID)
		if err != nil {
			t.Fatalf("Expected successful undo, got error: %v", err)
		}

		// Step 5: Verify restoration
		if len(restoredComments) != 1 {
			t.Fatalf("Expected 1 restored comment, got %d", len(restoredComments))
		}
		if restoredComments[0].Line != NewSingleLine(100) {
			t.Errorf("Expected line 100, got %v", restoredComments[0].Line)
		}
	})
}

func TestUndoSystem_ConcurrentOperations(t *testing.T) {
	t.Run("handles operations on different files independently", func(t *testing.T) {
		service := NewUndoService()
		repo := createTestRepository()

		// Create operations on different files
		comment1 := Comment{ID: "1", Path: "file1.go", Line: NewSingleLine(10)}
		opID1 := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment1, nil)

		comment2 := Comment{ID: "2", Path: "file2.go", Line: NewSingleLine(10)}
		opID2 := service.manager.history.RecordCommentOperation(repo, "123", UndoTypeComment, comment2, nil)

		// Both should be independently undoable
		if err := service.ValidateUndoOperation(opID1); err != nil {
			t.Errorf("Expected file1.go operation to be valid, got: %v", err)
		}

		if err := service.ValidateUndoOperation(opID2); err != nil {
			t.Errorf("Expected file2.go operation to be valid, got: %v", err)
		}
	})
}
