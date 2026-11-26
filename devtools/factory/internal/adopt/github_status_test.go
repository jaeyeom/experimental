package adopt_test

import (
	"context"
	"path/filepath"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/adopt"
	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestAdoptGitHubNativeStatus(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)

	tests := []struct {
		name      string
		state     string
		wantClaim bool
	}{
		{name: "claims when Todo", state: "Todo", wantClaim: true},
		{name: "skips claim when already In progress", state: "In progress", wantClaim: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			st, err := store.Open(context.Background(), filepath.Join(t.TempDir(), "factory.db"))
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = st.Close() })
			it := source.Item{
				Source: "github:experimental",
				ID:     "acme/widgets#12",
				Title:  "Fix bug",
				State:  tc.state,
			}
			src := &fake.Source{
				SourceName:  "github:experimental",
				Account:     "ada",
				ClaimStatus: config.DefaultGitHubClaimStatus,
				Items:       map[string]source.Item{it.ID: it},
			}
			in := adopt.Input{
				Src: src,
				Cfg: config.Source{
					Kind:        "github",
					Name:        "experimental",
					Repo:        "acme/widgets",
					Workspace:   "widgets",
					ClaimStatus: config.DefaultGitHubClaimStatus,
				},
				Kind:  "github",
				Store: st,
				Snap: ready.Snapshot{
					Items:      map[string]source.Item{it.ID: it},
					Workspaces: []herdr.Workspace{{ID: "wg", Label: "widgets"}},
					Tabs:       []herdr.Tab{{ID: "tg", WorkspaceID: "wg", Label: "#12"}},
					Me:         "ada",
				},
				Hostname: "mac1",
				Now:      func() time.Time { return now },
			}
			if err := adopt.Adopt(context.Background(), in, adopt.Request{ID: it.ID}); err != nil {
				t.Fatalf("Adopt() error = %v", err)
			}
			claimed := false
			for _, call := range src.Calls {
				if call.Method == "Claim" {
					claimed = true
				}
			}
			if claimed != tc.wantClaim {
				t.Fatalf("Claim called = %v, want %v (calls: %+v)", claimed, tc.wantClaim, src.Calls)
			}
		})
	}
}
