package memstore

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"
)

func ageNow() time.Time { return time.Date(2026, 6, 15, 12, 0, 0, 0, time.UTC) }

func newAgeVault(t *testing.T) *Vault {
	t.Helper()
	v, err := InitVault(VaultConfig{Name: "work", Path: filepath.Join(t.TempDir(), "work.db"), Residency: ResidencyLocalOnly})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { v.Close() })
	v.now = ageNow
	return v
}

func mustExec(t *testing.T, db *sql.DB, q string, args ...any) {
	t.Helper()
	if _, err := db.Exec(q, args...); err != nil {
		t.Fatalf("exec %q: %v", q, err)
	}
}

func seedEntity(t *testing.T, db *sql.DB) {
	t.Helper()
	mustExec(t, db, `INSERT INTO entities (id, kind, slug, title, created_at, updated_at) VALUES ('ent_1','person','a','A',1,1)`)
}

func seedFact(t *testing.T, db *sql.DB, id string, validUntil any) {
	t.Helper()
	mustExec(t, db, `INSERT INTO facts (id, entity_id, key, value_json, source_type, valid_until, created_at, updated_at)
		VALUES (?, 'ent_1', ?, '"v"', 'test', ?, 1, 1)`, id, "k_"+id, validUntil)
	mustExec(t, db, `INSERT INTO fts_docs (doc_id, doc_kind, entity_id, compartment, sensitivity_rank, title, body) VALUES (?, 'fact', 'ent_1', 'general', 1, 'k', 'v')`, id)
}

func seedEvent(t *testing.T, db *sql.DB, id, date, channel, ref, comp string) {
	t.Helper()
	d, err := time.Parse("2006-01-02", date)
	if err != nil {
		t.Fatal(err)
	}
	mustExec(t, db, `INSERT INTO episodic_events (id, event_date, occurred_at, entity_id, compartment, sensitivity, channel, source_ref, summary, reporter, created_at)
		VALUES (?, ?, ?, 'ent_1', ?, 'normal', ?, ?, 'did '||?, 'test', 1)`, id, date, d.Unix(), comp, channel, ref, id)
	mustExec(t, db, `INSERT INTO fts_docs (doc_id, doc_kind, entity_id, compartment, sensitivity_rank, title, body) VALUES (?, 'event', 'ent_1', ?, 1, 'e', 'b')`, id, comp)
}

func seedAgeData(t *testing.T, v *Vault) {
	t.Helper()
	db := v.db
	seedEntity(t, db)
	seedFact(t, db, "f_expired", ageNow().Unix()-1)
	seedFact(t, db, "f_future", ageNow().Unix()+3600)
	seedFact(t, db, "f_forever", nil)
	seedEvent(t, db, "e1", "2026-03-01", "slack", "r1", "general")
	seedEvent(t, db, "e2", "2026-03-10", "github", "r2", "general")
	seedEvent(t, db, "e3", "2026-03-20", "slack", "r3", "general")
	seedEvent(t, db, "e4", "2026-03-05", "slack", "r4", "finance")
	seedEvent(t, db, "e5", "2026-06-01", "slack", "r5", "general")
	old := ageNow().AddDate(0, 0, -100).Unix()
	mustExec(t, db, `INSERT INTO revisions (table_name,row_id,op,diff_json,actor,created_at) VALUES ('facts','x','insert','{}','t',?)`, old)
	mustExec(t, db, `INSERT INTO revisions (table_name,row_id,op,diff_json,actor,created_at) VALUES ('facts','y','insert','{}','t',?)`, ageNow().Unix()-10)
	mustExec(t, db, `INSERT INTO access_log (ts,caller,profile,command) VALUES (?, 'c','p','search')`, old)
	mustExec(t, db, `INSERT INTO access_log (ts,caller,profile,command) VALUES (?, 'c','p','search')`, ageNow().Unix()-10)
}

func dumpTables(t *testing.T, db *sql.DB) string {
	t.Helper()
	var sb strings.Builder
	for _, q := range []string{
		`SELECT id, expired_at FROM facts ORDER BY id`,
		`SELECT id, event_date, occurred_at, IFNULL(entity_id,''), compartment, sensitivity, channel, source_ref, summary, details_md, reporter, IFNULL(compacted_into_id,''), created_at FROM episodic_events ORDER BY id`,
		`SELECT id, table_name, row_id, op, diff_json, actor, reason, created_at FROM revisions ORDER BY id`,
		`SELECT id, ts, command FROM access_log ORDER BY id`,
		`SELECT doc_kind, doc_id FROM fts_docs ORDER BY doc_kind, doc_id`,
	} {
		rows, err := db.Query(q)
		if err != nil {
			t.Fatal(err)
		}
		cols, _ := rows.Columns()
		for rows.Next() {
			vals := make([]any, len(cols))
			ptrs := make([]any, len(cols))
			for i := range vals {
				ptrs[i] = &vals[i]
			}
			if err := rows.Scan(ptrs...); err != nil {
				t.Fatal(err)
			}
			fmt.Fprintln(&sb, vals...)
		}
		rows.Close()
		sb.WriteString("--\n")
	}
	return sb.String()
}

func count(t *testing.T, db *sql.DB, q string, args ...any) int {
	t.Helper()
	var n int
	if err := db.QueryRow(q, args...).Scan(&n); err != nil {
		t.Fatal(err)
	}
	return n
}

func TestAgeCompactsExpiresAndPrunes(t *testing.T) {
	v := newAgeVault(t)
	seedAgeData(t, v)
	ctx := context.Background()

	res, err := v.Age(ctx, AgeOptions{RetentionDays: 30})
	if err != nil {
		t.Fatal(err)
	}
	want := AgeResult{ExpiredFacts: 1, CompactedEvents: 4, Summaries: 2, DeletedRevisions: 1, DeletedAccessRows: 1}
	if res != want {
		t.Errorf("result = %+v, want %+v", res, want)
	}
	db := v.db
	if n := count(t, db, `SELECT COUNT(*) FROM facts WHERE expired_at IS NOT NULL`); n != 1 {
		t.Errorf("expired facts = %d", n)
	}
	if n := count(t, db, `SELECT COUNT(*) FROM fts_docs WHERE doc_id='f_expired'`); n != 0 {
		t.Errorf("expired fact still in fts")
	}
	if n := count(t, db, `SELECT COUNT(*) FROM fts_docs WHERE doc_id IN ('f_future','f_forever')`); n != 2 {
		t.Errorf("live facts removed from fts")
	}
	var id, summary, ref, channel, comp string
	var into sql.NullString
	if err := db.QueryRow(`SELECT id, summary, source_ref, channel, compartment FROM episodic_events
		WHERE reporter = ? AND compartment='general'`, AgeReporter).Scan(&id, &summary, &ref, &channel, &comp); err != nil {
		t.Fatal(err)
	}
	if summary != "3 events, channels: github, slack" || ref != "r3" || channel != "github,slack" {
		t.Errorf("summary = %q ref=%q channel=%q", summary, ref, channel)
	}
	for _, e := range []string{"e1", "e2", "e3"} {
		if err := db.QueryRow(`SELECT compacted_into_id FROM episodic_events WHERE id=?`, e).Scan(&into); err != nil || into.String != id {
			t.Errorf("%s compacted_into = %v (%v), want %s", e, into, err, id)
		}
		if n := count(t, db, `SELECT COUNT(*) FROM fts_docs WHERE doc_id=?`, e); n != 0 {
			t.Errorf("%s still in fts", e)
		}
	}
	if n := count(t, db, `SELECT COUNT(*) FROM fts_docs WHERE doc_id=? AND doc_kind='event'`, id); n != 1 {
		t.Errorf("summary not searchable")
	}
	if n := count(t, db, `SELECT COUNT(*) FROM episodic_events WHERE id='e5' AND compacted_into_id IS NULL`); n != 1 {
		t.Errorf("recent event touched")
	}
	if n := count(t, db, `SELECT COUNT(*) FROM access_log`); n != 1 {
		t.Errorf("access_log rows = %d, want 1 (age must not log)", n)
	}
	if n := count(t, db, `SELECT COUNT(*) FROM revisions WHERE row_id='f_expired' AND op='update' AND created_at=?`, ageNow().Unix()); n != 1 {
		t.Errorf("no fact expiry revision")
	}
	if n := count(t, db, `SELECT COUNT(*) FROM revisions WHERE table_name='episodic_events' AND row_id=? AND op='insert' AND diff_json LIKE '%e3%'`, id); n != 1 {
		t.Errorf("no compaction revision")
	}

	before := dumpTables(t, db)
	res, err = v.Age(ctx, AgeOptions{RetentionDays: 30})
	if err != nil {
		t.Fatal(err)
	}
	if res.Changed() {
		t.Errorf("second run changed %+v", res)
	}
	if after := dumpTables(t, db); after != before {
		t.Errorf("second run mutated DB:\n%s\nvs\n%s", before, after)
	}
}

func TestAgeDryRunLeavesDBUntouched(t *testing.T) {
	v := newAgeVault(t)
	seedAgeData(t, v)
	before := dumpTables(t, v.db)
	res, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30, DryRun: true})
	if err != nil {
		t.Fatal(err)
	}
	want := AgeResult{ExpiredFacts: 1, CompactedEvents: 4, Summaries: 2, DeletedRevisions: 1, DeletedAccessRows: 1}
	if res != want {
		t.Errorf("dry-run result = %+v, want %+v", res, want)
	}
	if after := dumpTables(t, v.db); after != before {
		t.Errorf("dry run mutated DB")
	}
}

func TestAgeDeterministic(t *testing.T) {
	a, b := newAgeVault(t), newAgeVault(t)
	seedAgeData(t, a)
	seedAgeData(t, b)
	for _, v := range []*Vault{a, b} {
		if _, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30}); err != nil {
			t.Fatal(err)
		}
	}
	if da, db := dumpTables(t, a.db), dumpTables(t, b.db); da != db {
		t.Errorf("same input gave different output:\n%s\nvs\n%s", da, db)
	}
}

func TestAgeDrop(t *testing.T) {
	v := newAgeVault(t)
	seedAgeData(t, v)
	res, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30, Drop: true})
	if err != nil {
		t.Fatal(err)
	}
	if res.DroppedEvents != 4 || res.CompactedEvents != 0 || res.Summaries != 0 {
		t.Errorf("result = %+v", res)
	}
	if n := count(t, v.db, `SELECT COUNT(*) FROM episodic_events`); n != 1 {
		t.Errorf("events left = %d, want 1", n)
	}
	if n := count(t, v.db, `SELECT COUNT(*) FROM fts_docs WHERE doc_kind='event'`); n != 1 {
		t.Errorf("event fts docs = %d, want 1", n)
	}
	if n := count(t, v.db, `SELECT COUNT(*) FROM revisions WHERE table_name='episodic_events' AND op='delete'`); n != 4 {
		t.Errorf("delete revisions = %d, want 4", n)
	}
	if res2, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30, Drop: true}); err != nil || res2.Changed() {
		t.Errorf("second drop run = %+v, %v", res2, err)
	}
}

func TestAgeMergesIntoExistingSummary(t *testing.T) {
	v := newAgeVault(t)
	seedAgeData(t, v)
	ctx := context.Background()
	if _, err := v.Age(ctx, AgeOptions{RetentionDays: 30}); err != nil {
		t.Fatal(err)
	}
	seedEvent(t, v.db, "e6", "2026-03-25", "email", "r6", "general")
	res, err := v.Age(ctx, AgeOptions{RetentionDays: 30})
	if err != nil {
		t.Fatal(err)
	}
	if res.Summaries != 1 || res.CompactedEvents != 2 {
		t.Errorf("result = %+v", res)
	}
	var summary, ref string
	if err := v.db.QueryRow(`SELECT summary, source_ref FROM episodic_events WHERE reporter=? AND compartment='general' AND compacted_into_id IS NULL`, AgeReporter).Scan(&summary, &ref); err != nil {
		t.Fatal(err)
	}
	if summary != "4 events, channels: email, github, slack" || ref != "r6" {
		t.Errorf("merged summary = %q ref=%q", summary, ref)
	}
	if n := count(t, v.db, `SELECT COUNT(*) FROM episodic_events e JOIN episodic_events p ON e.compacted_into_id = p.id WHERE p.compacted_into_id IS NOT NULL`); n != 0 {
		t.Errorf("compaction chain depth > 1")
	}
}

func TestAgeLocked(t *testing.T) {
	v := newAgeVault(t)
	unlock, err := lockFile(ageLockPath(v.config))
	if err != nil {
		t.Fatal(err)
	}
	defer unlock()
	if _, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30}); !errors.Is(err, ErrAgeLocked) {
		t.Errorf("err = %v, want ErrAgeLocked", err)
	}
}

func TestAgeRejectsBadRetention(t *testing.T) {
	v := newAgeVault(t)
	if _, err := v.Age(context.Background(), AgeOptions{}); err == nil {
		t.Error("want error for zero retention")
	}
}

func TestAgeKeepsExpiryState(t *testing.T) {
	v := newAgeVault(t)
	seedEntity(t, v.db)
	seedFact(t, v.db, "f_expired", ageNow().Unix()-1)
	if _, err := v.Age(context.Background(), AgeOptions{RetentionDays: 30}); err != nil {
		t.Fatal(err)
	}
	var got []int64
	rows, err := v.db.Query(`SELECT expired_at FROM facts`)
	if err != nil {
		t.Fatal(err)
	}
	defer rows.Close()
	for rows.Next() {
		var x int64
		if err := rows.Scan(&x); err != nil {
			t.Fatal(err)
		}
		got = append(got, x)
	}
	if !reflect.DeepEqual(got, []int64{ageNow().Unix()}) {
		t.Errorf("expired_at = %v", got)
	}
}
