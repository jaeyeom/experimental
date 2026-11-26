package cli

import (
	"net/http"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestBuildSourcesJiraKey(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{{Name: "acme", Kind: "jira"}}}
	got, err := buildSources(cfg, Deps{HTTP: &http.Client{}})
	if err != nil {
		t.Fatalf("buildSources: %v", err)
	}
	src, ok := got["jira:acme"]
	if !ok || len(got) != 1 {
		t.Fatalf("sources = %#v", got)
	}
	if src.Name() != "jira:acme" {
		t.Fatalf("Name = %q, want jira:acme", src.Name())
	}
}

func TestBuildSourcesGitHubKey(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{{
		Name: "experimental",
		Kind: "github",
		Repo: "acme/widgets",
	}}}
	got, err := buildSources(cfg, Deps{Exec: executor.NewMockExecutor()})
	if err != nil {
		t.Fatalf("buildSources: %v", err)
	}
	src, ok := got["github:experimental"]
	if !ok || len(got) != 1 {
		t.Fatalf("sources = %#v", got)
	}
	if src.Name() != "github:experimental" {
		t.Fatalf("Name = %q, want github:experimental", src.Name())
	}
}

func TestBuildSourcesGitHubNeedsExecutor(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{{Name: "experimental", Kind: "github"}}}
	_, err := buildSources(cfg, Deps{HTTP: &http.Client{}})
	if err == nil || !strings.Contains(err.Error(), "found nil") || !strings.Contains(err.Error(), "executor") {
		t.Fatalf("error = %v", err)
	}
}

func TestBuildSourcesJiraAndGitHub(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{
		{Name: "acme", Kind: "jira"},
		{Name: "experimental", Kind: "github", Repo: "acme/widgets"},
	}}
	got, err := buildSources(cfg, Deps{HTTP: &http.Client{}, Exec: executor.NewMockExecutor()})
	if err != nil {
		t.Fatalf("buildSources: %v", err)
	}
	if len(got) != 2 || got["jira:acme"] == nil || got["github:experimental"] == nil {
		t.Fatalf("sources = %#v", got)
	}
	if got["jira:acme"].Name() != "jira:acme" || got["github:experimental"].Name() != "github:experimental" {
		t.Fatalf("names = %q %q", got["jira:acme"].Name(), got["github:experimental"].Name())
	}
}

func TestBuildSourcesUnknownKind(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{{Name: "experimental", Kind: "linear"}}}
	_, err := buildSources(cfg, Deps{HTTP: &http.Client{}, Exec: executor.NewMockExecutor()})
	if err == nil || !strings.Contains(err.Error(), "jira or github") || !strings.Contains(err.Error(), `found "linear"`) {
		t.Fatalf("error = %v", err)
	}
}

func TestBuildSourcesRejectsNilHTTPAndEmptyIdentity(t *testing.T) {
	cfg := &config.Config{Sources: []config.Source{{Name: "acme", Kind: "jira"}}}
	if _, err := buildSources(cfg, Deps{}); err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("nil HTTP error = %v", err)
	}
	empty := &config.Config{Sources: []config.Source{{Name: "acme"}}}
	if _, err := buildSources(empty, Deps{HTTP: &http.Client{}}); err == nil || !strings.Contains(err.Error(), `found kind ""`) {
		t.Fatalf("empty kind error = %v", err)
	}
	if _, err := buildSources(nil, Deps{}); err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("nil config error = %v", err)
	}
}
