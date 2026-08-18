package dispatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

var update = flag.Bool("update", false, "update golden files")

var fixtureNow = time.Date(2026, 1, 1, 9, 0, 0, 0, time.UTC)

func TestRunDryRunGolden(t *testing.T) {
	t.Parallel()

	cfg := config.Defaults()
	doc := goldenScanDoc()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	pre := State{}
	pre.Record("acme/widgets#204", []string{"PRRC_old"}, fixtureNow)
	if err := SaveFile(store.Path, pre); err != nil {
		t.Fatal(err)
	}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}

	gotDoc, err := Run(context.Background(), h, store, cfg, Request{
		Doc: doc,
		PRs: []string{
			"acme/gizmos#50",
			"acme/missing#9",
			"acme/widgets#123",
			"acme/widgets#200",
			"acme/widgets#201",
			"acme/widgets#202",
			"acme/widgets#203",
			"acme/widgets#204",
		},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if !gotDoc.DryRun {
		t.Fatal("dry_run = false, want true")
	}
	if len(gotDoc.Results) != 8 {
		t.Fatalf("len(results) = %d, want 8 (never silently drop)", len(gotDoc.Results))
	}

	got, err := json.MarshalIndent(gotDoc, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	got = append(got, '\n')
	path := filepath.Join("testdata", "golden", "dispatch-dry-run.json")
	if *update {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, got, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("golden mismatch\n got:\n%s\nwant:\n%s", got, want)
	}
}

func TestRunDryRunDoesNotWriteState(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "state.json")
	store := FileStore{Path: path}
	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	cfg := config.Defaults()
	_, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if _, err := os.Stat(path); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("state file written on dry-run: %v", err)
	}
}

func TestRunDryRunDoesNotPoll(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{workingAgent("w2:pX", "w2:tX")}}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if h.n != 1 {
		t.Fatalf("AgentList calls = %d, want 1 (one-shot Check, never poll)", h.n)
	}
	if got.Results[0].Action != ActionWouldDispatch {
		t.Fatalf("action = %q, want would_dispatch (never queued)", got.Results[0].Action)
	}
	if got.Results[0].Detail != "gate currently busy: pane w2:pX working" {
		t.Fatalf("detail = %q", got.Results[0].Detail)
	}
}

func TestRunDryRunNeverQueued(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{
		{workingAgent("w2:pX", "w2:tX")},
		{idleAgent("w2:pX", "w2:tX")},
	}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	for _, r := range got.Results {
		if r.Action == ActionQueued {
			t.Fatalf("dry-run emitted queued: %+v", r)
		}
	}
	if h.n != 1 {
		t.Fatalf("AgentList calls = %d, want 1", h.n)
	}
}

func TestRunDryRunHerdrRequired(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{minErr: herdr.ErrNotInstalled}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	_, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
	}, fixtureNow)
	if !errors.Is(err, herdr.ErrNotInstalled) {
		t.Fatalf("error = %v, want ErrNotInstalled", err)
	}
}

func TestRunRecordsEveryInputPR(t *testing.T) {
	t.Parallel()

	h := &scriptHerdr{lists: [][]herdr.Agent{{idleAgent("w2:pC", "w2:tC")}}}
	cfg := config.Defaults()
	store := FileStore{Path: filepath.Join(t.TempDir(), "state.json")}
	got, err := Run(context.Background(), h, store, cfg, Request{
		Doc: scan.Document{PRs: []scan.PR{fixtureEligiblePR()}},
		PRs: []string{"acme/widgets#123", "acme/missing#1"},
	}, fixtureNow)
	if err != nil {
		t.Fatalf("Run() unexpected error: %v", err)
	}
	if len(got.Results) != 2 {
		t.Fatalf("len(results) = %d, want 2", len(got.Results))
	}
	actions := map[string]string{}
	for _, r := range got.Results {
		actions[r.Repo+"#"+itoa(r.Number)] = r.Action
	}
	if actions["acme/widgets#123"] != ActionWouldDispatch {
		t.Fatalf("actions = %v", actions)
	}
	if actions["acme/missing#1"] != ActionSkippedNotFound {
		t.Fatalf("actions = %v", actions)
	}
}

func goldenScanDoc() scan.Document {
	addr := fixtureEligiblePR()
	addr.Repo = "acme/gizmos"
	addr.Number = 50
	addr.Unaddressed = false
	addr.BlockingComments = nil

	draft := fixtureEligiblePR()
	draft.Number = 200
	draft.IsDraft = true

	noTab := fixtureEligiblePR()
	noTab.Number = 201
	noTab.Tab = nil

	noPane := fixtureEligiblePR()
	noPane.Number = 202
	noPane.Tab.PaneID = nil

	busy := fixtureEligiblePR()
	busy.Number = 203
	busy.Tab.AgentStatus = "working"

	deduped := fixtureEligiblePR()
	deduped.Number = 204
	deduped.BlockingComments[0].CommentID = "PRRC_old"

	return scan.Document{PRs: []scan.PR{
		addr, fixtureEligiblePR(), draft, noTab, noPane, busy, deduped,
	}}
}

func itoa(n int) string {
	return strconv.Itoa(n)
}
