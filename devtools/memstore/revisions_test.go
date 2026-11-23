package memstore

import (
	"context"
	"strings"
	"testing"
)

func TestRecordRevision(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	ctx := context.Background()

	t.Run("records compact json diff", func(t *testing.T) {
		tx, err := v.db.BeginTx(ctx, nil)
		if err != nil {
			t.Fatal(err)
		}
		diff := map[string]any{
			"key":   "status",
			"value": "active",
		}
		if err := RecordRevision(ctx, tx, "facts", "fact-1", "insert", diff, "alice", "initial load"); err != nil {
			t.Fatalf("RecordRevision: %v", err)
		}
		if err := tx.Commit(); err != nil {
			t.Fatal(err)
		}

		var table, rowID, op, diffJSON, actor, reason string
		var createdAt int64
		err = v.db.QueryRowContext(ctx,
			`SELECT table_name, row_id, op, diff_json, actor, reason, created_at
			FROM revisions WHERE row_id = 'fact-1'`,
		).Scan(&table, &rowID, &op, &diffJSON, &actor, &reason, &createdAt)
		if err != nil {
			t.Fatalf("query revision: %v", err)
		}
		if table != "facts" || rowID != "fact-1" || op != "insert" || actor != "alice" || reason != "initial load" {
			t.Errorf("got (%q, %q, %q, %q, %q)", table, rowID, op, actor, reason)
		}
		if createdAt <= 0 {
			t.Errorf("created_at = %d, want > 0", createdAt)
		}
		if diffJSON != `{"key":"status","value":"active"}` {
			t.Errorf("diff_json = %q, want compact JSON", diffJSON)
		}
		if strings.ContainsAny(diffJSON, " \n\t") {
			t.Errorf("diff_json %q is not compact", diffJSON)
		}
	})

	t.Run("nil diffObj writes empty object", func(t *testing.T) {
		tx, err := v.db.BeginTx(ctx, nil)
		if err != nil {
			t.Fatal(err)
		}
		if err := RecordRevision(ctx, tx, "entities", "e-1", "delete", nil, "bob", "cleanup"); err != nil {
			t.Fatalf("RecordRevision nil: %v", err)
		}
		if err := tx.Commit(); err != nil {
			t.Fatal(err)
		}

		var diffJSON string
		if err := v.db.QueryRowContext(ctx, `SELECT diff_json FROM revisions WHERE row_id = 'e-1'`).Scan(&diffJSON); err != nil {
			t.Fatal(err)
		}
		if diffJSON != "{}" {
			t.Errorf("diff_json = %q, want {}", diffJSON)
		}
	})

	t.Run("invalid op fails check constraint", func(t *testing.T) {
		tx, err := v.db.BeginTx(ctx, nil)
		if err != nil {
			t.Fatal(err)
		}
		defer tx.Rollback() //nolint:errcheck // test cleanup
		if err := RecordRevision(ctx, tx, "facts", "f-bad", "bogus", map[string]string{"a": "b"}, "alice", ""); err == nil {
			t.Fatal("RecordRevision with invalid op succeeded, want error")
		}
	})

	t.Run("unmarshalable diffObj returns error", func(t *testing.T) {
		tx, err := v.db.BeginTx(ctx, nil)
		if err != nil {
			t.Fatal(err)
		}
		defer tx.Rollback() //nolint:errcheck // test cleanup
		if err := RecordRevision(ctx, tx, "facts", "f-ch", "insert", make(chan int), "alice", ""); err == nil {
			t.Fatal("RecordRevision with chan succeeded, want error")
		}
	})
}
