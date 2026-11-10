package herdr

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
	executor "github.com/jaeyeom/go-cmdexec"
)

func remoteClient(t *testing.T) (*Client, *herdrtest.Fake) {
	t.Helper()
	f := herdrtest.Install(t)
	f.Write("m2.tabs", tabListJSON)
	f.Write("m2.agents", agentListJSON)
	f.Write("m2.prompt", promptSuccessJSON)
	return NewClient(executor.NewBasicExecutor(), "herdr").OnMachine("m2", 2*time.Second), f
}

func TestOnMachineListsStampTheMachine(t *testing.T) {
	c, f := remoteClient(t)
	ctx := context.Background()

	tabs, err := c.TabList(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(tabs) != 1 || tabs[0].TabID != "w2:tC" || tabs[0].Machine != "m2" {
		t.Fatalf("tabs = %+v", tabs)
	}
	agents, err := c.AgentList(ctx)
	if err != nil {
		t.Fatal(err)
	}
	if len(agents) != 1 || agents[0].PaneID != "w2:pC" || agents[0].Machine != "m2" {
		t.Fatalf("agents = %+v", agents)
	}
	if got := f.Argv(); len(got) != 2 || got[0] != "--machine m2 tab list" || got[1] != "--machine m2 agent list" {
		t.Fatalf("argv = %q", got)
	}
	if c.Machine() != "m2" {
		t.Fatalf("Machine() = %q", c.Machine())
	}
}

func TestOnMachinePromptRunsThere(t *testing.T) {
	c, f := remoteClient(t)
	out := c.Prompt(context.Background(), "w2:pC", "fix it", []string{"idle"}, time.Second)
	if out.Status != PromptMatched {
		t.Fatalf("outcome = %+v", out)
	}
	got := f.Argv()
	if len(got) != 1 || !strings.HasPrefix(got[0], "--machine m2 agent prompt w2:pC fix it --wait") {
		t.Fatalf("argv = %q", got)
	}
}

func TestOnMachineUnreachable(t *testing.T) {
	tests := []struct {
		file  string
		cause string
	}{
		{"m2.down", remotecall.CauseHostUnreachable},
		{"m2.locked", remotecall.CauseAuthLocked},
		{"m2.hang", remotecall.CauseAuthLocked},
	}
	for _, tt := range tests {
		t.Run(tt.file, func(t *testing.T) {
			f := herdrtest.Install(t)
			f.Write(tt.file, "")
			c := NewClient(executor.NewBasicExecutor(), "herdr").OnMachine("m2", 300*time.Millisecond)
			start := time.Now()
			_, err := c.TabList(context.Background())
			ue := remotecall.AsUnreachable(err)
			if ue == nil || ue.Cause != tt.cause || ue.Machine != "m2" {
				t.Fatalf("TabList error = %v, want cause %q", err, tt.cause)
			}
			if took := time.Since(start); took > 5*time.Second {
				t.Fatalf("TabList took %s", took)
			}
		})
	}
}

func TestLocalClientHasNoMachine(t *testing.T) {
	t.Parallel()
	if got := NewClient(newHerdrMock(), testHerdrBin).Machine(); got != "" {
		t.Fatalf("Machine() = %q", got)
	}
}
