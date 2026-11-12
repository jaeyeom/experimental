package cli

import (
	"bytes"
	"context"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	executor "github.com/jaeyeom/go-cmdexec"
)

// untabbedFake is a fake herdr with no PR tab on any machine. The tab
// a `tab create` makes, w9:t1, runs an idle claude in w9:p1.
func untabbedFake(t *testing.T) *herdrtest.Fake {
	t.Helper()
	f := herdrtest.Install(t)
	agent := `{"pane_id":"w9:p1","tab_id":"w9:t1","agent":"claude","agent_status":"idle","cwd":"/src/widgets"}`
	f.Write("local.tabs", `{"result":{"tabs":[]}}`)
	f.Write("local.agents", `{"result":{"agents":[`+agent+`]}}`)
	f.Write("local.prompt", `{"result":{"agent":`+agent+`}}`)
	return f
}

func runOpenTabsDispatch(t *testing.T, args ...string) (dispatch.Document, string) {
	t.Helper()
	ghBin, _ := fixtureBins(t)
	cfgPath := writeLiveConfig(t, ghBin, "herdr", filepath.Join(t.TempDir(), "state.json"), "open_tabs_cwd=/src/widgets")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), append([]string{"dispatch", "--config", cfgPath}, args...), &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	return decodeDispatch(t, stdout.Bytes()), stderr.String()
}

func TestDispatchOpenTabsFlag(t *testing.T) {
	untabbedFake(t)
	got, _ := runOpenTabsDispatch(t)
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionSkippedNoTab {
		t.Fatalf("without --open-tabs: results = %+v, want skipped_no_tab", got.Results)
	}
	got, _ = runOpenTabsDispatch(t, "--open-tabs")
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionWouldOpenTab {
		t.Fatalf("--open-tabs: results = %+v, want would_open_tab", got.Results)
	}
}

func TestDispatchOpenTabsLive(t *testing.T) {
	f := untabbedFake(t)
	got, _ := runOpenTabsDispatch(t, "--go", "--open-tabs")
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionOpenedTabAndDispatched || got.Results[0].TabID != "w9:t1" {
		t.Fatalf("results = %+v, want opened_tab_and_dispatched in w9:t1", got.Results)
	}
	argv := strings.Join(f.Argv(), "\n")
	for _, want := range []string{"tab create --cwd /src/widgets --label PROJ-123 --no-focus", "agent start pr-proj-123 --kind claude --pane w9:p1", "agent prompt w9:p1 "} {
		if !strings.Contains(argv, want) {
			t.Fatalf("argv = %s, want %q", argv, want)
		}
	}
}
