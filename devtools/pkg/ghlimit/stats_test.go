package ghlimit

import (
	"bytes"
	"encoding/json"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestCallStatsRanking(t *testing.T) {
	dir := t.TempDir()
	callLog := filepath.Join(dir, "calls.jsonl")
	waitLog := filepath.Join(dir, "log.jsonl")
	now := time.Unix(1_700_000_000, 0)
	at := func(ago time.Duration) time.Time { return now.Add(-ago) }
	old := []CallEntry{
		{Time: at(3 * time.Hour), Caller: "ancient", Kind: KindRESTWrite, Points: 500},
	}
	rotated := []CallEntry{
		{Time: at(50 * time.Minute), Caller: "prsync", Kind: KindGraphQL, Points: 1, DurationMS: 4000, QueryHash: "aaaaaaaaaaaa", QueryFields: []string{"repository"}},
	}
	current := []CallEntry{
		{Time: at(40 * time.Minute), Caller: "prsync", Kind: KindGraphQL, Points: 1, DurationMS: 4000, QueryHash: "aaaaaaaaaaaa", QueryFields: []string{"repository"}},
		{Time: at(30 * time.Minute), Caller: "gh-nudge", Kind: KindRESTWrite, Points: 5, DurationMS: 100},
		{Time: at(30 * time.Minute), Caller: "gh-nudge", Kind: KindRESTWrite, Points: 5, DurationMS: 100},
		{Time: at(20 * time.Minute), Caller: "gh-nudge", Kind: KindGraphQL, Points: 1, DurationMS: 500, QueryHash: "bbbbbbbbbbbb", QueryFields: []string{"viewer"}},
		{Time: at(19 * time.Minute), Caller: "gh-nudge", Kind: KindGraphQL, Points: 1, DurationMS: 500, QueryHash: "bbbbbbbbbbbb", QueryFields: []string{"viewer"}},
		{Time: at(18 * time.Minute), Caller: "gh-nudge", Kind: KindGraphQL, Points: 1, DurationMS: 500, QueryHash: "bbbbbbbbbbbb", QueryFields: []string{"viewer"}, ExitCode: 1, RateLimited: true},
		{Time: at(10 * time.Minute), Caller: "decide", Kind: KindSubcommand, Cmd: "pr view", Points: 0, CacheHit: true},
	}
	writeAll := func(path string, entries []CallEntry) {
		for _, e := range entries {
			if err := AppendCallLog(path, e, MaxCallLogSize); err != nil {
				t.Fatal(err)
			}
		}
	}
	writeAll(callLog+".1", append(old, rotated...))
	writeAll(callLog, current)
	if err := AppendLog(waitLog, Entry{Time: at(5 * time.Minute), Caller: "prsync", Reason: ReasonPointsBudget, WaitMS: 2000}); err != nil {
		t.Fatal(err)
	}

	s, err := ReadStats(waitLog, callLog, now, time.Hour)
	if err != nil {
		t.Fatal(err)
	}
	if s.Calls != 8 || s.Points != 15 || s.RateLimited != 1 || s.CacheHits != 1 {
		t.Fatalf("totals = calls %d points %d rate-limited %d cache hits %d", s.Calls, s.Points, s.RateLimited, s.CacheHits)
	}
	names := func(rows []StatRow) string {
		var out []string
		for _, r := range rows {
			out = append(out, r.Key)
		}
		return strings.Join(out, ",")
	}
	if got := names(s.CallersByPoints); got != "gh-nudge,prsync,decide" {
		t.Errorf("callers by points = %s", got)
	}
	if s.CallersByPoints[0].Points != 13 {
		t.Errorf("gh-nudge points = %d, want 13", s.CallersByPoints[0].Points)
	}
	if got := names(s.CallersByGraphQLTime); got != "prsync,gh-nudge" {
		t.Errorf("callers by GraphQL time = %s", got)
	}
	if s.CallersByGraphQLTime[0].GraphQLMS != 8000 {
		t.Errorf("prsync GraphQL time = %dms, want 8000", s.CallersByGraphQLTime[0].GraphQLMS)
	}
	if got := names(s.QueriesByTime); got != "aaaaaaaaaaaa,bbbbbbbbbbbb" {
		t.Errorf("queries by time = %s", got)
	}
	if got := names(s.QueriesByCount); got != "bbbbbbbbbbbb,aaaaaaaaaaaa" {
		t.Errorf("queries by count = %s", got)
	}
	if s.QueriesByCount[0].Count != 3 || s.QueriesByCount[0].Fields != "viewer" {
		t.Errorf("top query by count = %+v", s.QueriesByCount[0])
	}
	if len(s.WaitsByReason) != 1 || s.WaitsByReason[0].Key != ReasonPointsBudget || s.WaitsByReason[0].WaitMS != 2000 {
		t.Errorf("waits by reason = %+v", s.WaitsByReason)
	}

	var text bytes.Buffer
	if err := s.WriteText(&text); err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"8 calls", "15 points", "1 rate-limited", "points_budget", "aaaaaaaaaaaa", "repository"} {
		if !strings.Contains(text.String(), want) {
			t.Errorf("text stats missing %q:\n%s", want, text.String())
		}
	}
	raw, err := json.Marshal(s)
	if err != nil {
		t.Fatal(err)
	}
	var back Stats
	if err := json.Unmarshal(raw, &back); err != nil || back.Points != 15 || len(back.QueriesByTime) != 2 {
		t.Fatalf("JSON round trip = %+v, %v", back, err)
	}
}

func TestStatsCountAnomaliesByCaller(t *testing.T) {
	dir := t.TempDir()
	callLog := filepath.Join(dir, "calls.jsonl")
	now := time.Unix(1_700_000_000, 0)
	for _, e := range []CallEntry{
		{Time: now, Caller: "decide", Kind: KindSubcommand, Anomaly: AnomalyEmptyJSON},
		{Time: now, Caller: "decide", Kind: KindSubcommand, Anomaly: AnomalyEmptyJSON},
		{Time: now, Caller: "prsync", Kind: KindGraphQL, Anomaly: AnomalyEmptyJSON},
		{Time: now, Caller: "prsync", Kind: KindGraphQL},
	} {
		if err := AppendCallLog(callLog, e, MaxCallLogSize); err != nil {
			t.Fatal(err)
		}
	}
	s, err := ReadStats(filepath.Join(dir, "log.jsonl"), callLog, now, time.Hour)
	if err != nil {
		t.Fatal(err)
	}
	if s.Anomalies != 3 || len(s.AnomaliesByCaller) != 2 ||
		s.AnomaliesByCaller[0] != (StatRow{Key: "decide", Fields: AnomalyEmptyJSON, Count: 2}) ||
		s.AnomaliesByCaller[1] != (StatRow{Key: "prsync", Fields: AnomalyEmptyJSON, Count: 1}) {
		t.Fatalf("anomalies = %d, by caller %+v", s.Anomalies, s.AnomaliesByCaller)
	}
	var text bytes.Buffer
	if err := s.WriteText(&text); err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"3 anomalies", "anomalies by caller", "decide", AnomalyEmptyJSON} {
		if !strings.Contains(text.String(), want) {
			t.Errorf("text stats missing %q:\n%s", want, text.String())
		}
	}
}
