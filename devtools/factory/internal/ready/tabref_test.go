package ready

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestTabRefJira(t *testing.T) {
	t.Parallel()

	rules := []config.WorkspaceRule{
		{Label: "smoke-label", Workspace: "smoke"},
		{Parent: "PRJ-2131", Workspace: "parent-ws"},
		{Label: "backend", Workspace: "backend-ws"},
	}
	tests := []struct {
		name      string
		id        string
		parent    string
		labels    []string
		workspace string
		wantWS    string
		wantTab   string
	}{
		{
			name:      "first matching rule wins",
			parent:    "PRJ-2131",
			labels:    []string{"smoke-label"},
			workspace: "factory",
			wantWS:    "smoke",
			wantTab:   "PRJ-1",
		},
		{
			name:      "parent rule in config order",
			parent:    "PRJ-2131",
			labels:    []string{"agent-ready"},
			workspace: "factory",
			wantWS:    "parent-ws",
			wantTab:   "PRJ-1",
		},
		{
			name:      "label rule after a parent miss",
			parent:    "PRJ-9",
			labels:    []string{"agent-ready", "backend"},
			workspace: "factory",
			wantWS:    "backend-ws",
			wantTab:   "PRJ-1",
		},
		{
			name:      "label match is case sensitive",
			parent:    "PRJ-9",
			labels:    []string{"Backend"},
			workspace: "factory",
			wantWS:    "factory",
			wantTab:   "PRJ-1",
		},
		{
			name:      "no rule uses source workspace",
			parent:    "PRJ-9",
			labels:    []string{"agent-ready"},
			workspace: "custom",
			wantWS:    "custom",
			wantTab:   "PRJ-1",
		},
		{
			name:    "empty workspace falls back to factory",
			parent:  "PRJ-9",
			wantWS:  "factory",
			wantTab: "PRJ-1",
		},
		{
			name:    "different parent does not match",
			parent:  "PRJ-2132",
			wantWS:  "factory",
			wantTab: "PRJ-1",
		},
		{
			name:      "blank parent on a label rule does not match a parentless item",
			parent:    "",
			labels:    []string{"agent-ready"},
			workspace: "custom",
			wantWS:    "custom",
			wantTab:   "PRJ-1",
		},
		{
			name:      "tab label is the item id",
			id:        "PRJ-99",
			parent:    "PRJ-2131",
			workspace: "factory",
			wantWS:    "parent-ws",
			wantTab:   "PRJ-99",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			id := tc.id
			if id == "" {
				id = "PRJ-1"
			}
			it := source.Item{ID: id, Parent: tc.parent, Labels: tc.labels}
			cfg := config.Source{Workspace: tc.workspace, WorkspaceRules: rules}
			gotWS, gotTab := TabRef(it, cfg, "jira")
			if gotWS != tc.wantWS || gotTab != tc.wantTab {
				t.Fatalf("TabRef() = (%q, %q), want (%q, %q)", gotWS, gotTab, tc.wantWS, tc.wantTab)
			}
		})
	}
}

func TestTabRefGitHub(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name      string
		id        string
		workspace string
		repo      string
		wantWS    string
		wantTab   string
	}{
		{
			name:      "workspace override",
			id:        "jaeyeom/experimental#57",
			workspace: "custom",
			repo:      "jaeyeom/experimental",
			wantWS:    "custom",
			wantTab:   "#57",
		},
		{
			name:    "repo name when workspace is empty",
			id:      "jaeyeom/experimental#57",
			repo:    "jaeyeom/experimental",
			wantWS:  "experimental",
			wantTab: "#57",
		},
		{
			name:    "repo name is the last path part",
			id:      "acme/widgets#12",
			repo:    "acme/widgets",
			wantWS:  "widgets",
			wantTab: "#12",
		},
		{
			name:      "set workspace skips a bad repo",
			id:        "acme/widgets#12",
			workspace: "custom",
			repo:      "not a repo",
			wantWS:    "custom",
			wantTab:   "#12",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			it := source.Item{ID: tc.id}
			cfg := config.Source{Workspace: tc.workspace, Repo: tc.repo}
			gotWS, gotTab := TabRef(it, cfg, "github")
			if gotWS != tc.wantWS || gotTab != tc.wantTab {
				t.Fatalf("TabRef() = (%q, %q), want (%q, %q)", gotWS, gotTab, tc.wantWS, tc.wantTab)
			}
		})
	}
}

func TestTabRefPanics(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name      string
		kind      string
		id        string
		workspace string
		repo      string
		parts     []string
	}{
		{name: "unknown kind", kind: "nope", id: "PRJ-1", parts: []string{"expected", "jira", "github", "found", "nope"}},
		{name: "github id", kind: "github", id: "PRJ-1", workspace: "custom", parts: []string{"expected", "found", "PRJ-1"}},
		{name: "leading zero", kind: "github", id: "owner/repo#01", repo: "owner/repo", parts: []string{"expected", "found", "owner/repo#01"}},
		{name: "repo without slash", kind: "github", id: "owner/repo#1", repo: "experimental", parts: []string{"expected", "found", "experimental"}},
		{name: "empty repo", kind: "github", id: "owner/repo#1", parts: []string{"expected", "found"}},
		{name: "extra slash", kind: "github", id: "owner/repo#1", repo: "a/b/c", parts: []string{"expected", "found", "a/b/c"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			defer func() {
				msg := recovered(t, recover())
				for _, part := range tc.parts {
					if !strings.Contains(msg, part) {
						t.Fatalf("panic = %q, want it to contain %q", msg, part)
					}
				}
			}()
			TabRef(source.Item{ID: tc.id}, config.Source{Workspace: tc.workspace, Repo: tc.repo}, tc.kind)
			t.Fatal("TabRef() returned, want a panic")
		})
	}
}
