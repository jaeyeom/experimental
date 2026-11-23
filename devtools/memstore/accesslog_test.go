package memstore

import (
	"context"
	"testing"
	"time"
)

func TestAccessLogFiltersNewestFirst(t *testing.T) {
	v, err := InitVault(testVaultConfig(t))
	if err != nil {
		t.Fatal(err)
	}
	defer v.Close()

	now := time.Date(2026, 3, 10, 9, 30, 0, 0, time.UTC)
	v.now = func() time.Time { return now }
	ctx := context.Background()

	seed := []struct {
		caller   string
		profile  string
		command  string
		entityID string
		reason   string
	}{
		{"ops-agent", "ops", "resolve", "person:ada", ""},
		{"ops-agent", "ops", "get", "person:bob", "prep"},
		{"inbox-agent", "inbox", "search", "person:ada", "reply"},
	}
	for _, s := range seed {
		if err := v.LogAccess(ctx, s.caller, s.profile, s.command, s.entityID, s.reason); err != nil {
			t.Fatalf("LogAccess: %v", err)
		}
		now = now.Add(24 * time.Hour)
	}

	all, err := v.QueryAccessLog(ctx, AccessLogFilter{})
	if err != nil {
		t.Fatalf("QueryAccessLog all: %v", err)
	}
	if len(all) != 3 {
		t.Fatalf("len(all) = %d, want 3", len(all))
	}
	if all[0].Caller != "inbox-agent" || all[0].Profile != "inbox" || all[2].Command != "resolve" {
		t.Errorf("unexpected order/fields: %+v", all)
	}

	tests := []struct {
		name   string
		filter AccessLogFilter
		want   int
	}{
		{
			name:   "by caller",
			filter: AccessLogFilter{Caller: "ops-agent"},
			want:   2,
		},
		{
			name:   "by entityID",
			filter: AccessLogFilter{EntityID: "person:ada"},
			want:   2,
		},
		{
			name:   "by caller and entityID",
			filter: AccessLogFilter{Caller: "ops-agent", EntityID: "person:ada"},
			want:   1,
		},
		{
			name:   "by since",
			filter: AccessLogFilter{Since: time.Date(2026, 3, 11, 0, 0, 0, 0, time.UTC)},
			want:   2,
		},
		{
			name:   "by limit",
			filter: AccessLogFilter{Limit: 2},
			want:   2,
		},
		{
			name:   "by since and limit",
			filter: AccessLogFilter{Since: time.Date(2026, 3, 11, 0, 0, 0, 0, time.UTC), Limit: 1},
			want:   1,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := v.QueryAccessLog(ctx, tc.filter)
			if err != nil {
				t.Fatalf("QueryAccessLog(%+v): %v", tc.filter, err)
			}
			if len(got) != tc.want {
				t.Errorf("got %d rows, want %d", len(got), tc.want)
			}
		})
	}
}
