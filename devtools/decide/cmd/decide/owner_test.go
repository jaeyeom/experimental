package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

const (
	herdrPanesW1P3 = `{"result":{"panes":[{"pane_id":"w1:p3","tab_id":"w1:t2","workspace_id":"w1"}]}}`
	herdrTabsW1T2  = `{"result":{"tabs":[{"tab_id":"w1:t2","label":"review"}]}}`
	whoamiOpsInbox = "ops/inbox\nwhen: reviewing\n"
)

// inHerdrPane runs the test in herdr pane w1:p3 of the tab labelled
// "review", with a fake herdr on PATH.
func inHerdrPane(t *testing.T) *herdrtest.Fake {
	t.Helper()
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("DECIDE_HUB", "")
	t.Setenv("DECIDE_AS_HANDLE", "")
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	herdr := herdrtest.Install(t)
	herdr.Write("local.agents", herdrAgentsW1P3)
	herdr.Write("local.panes", herdrPanesW1P3)
	herdr.Write("local.tabs", herdrTabsW1T2)
	return herdr
}

// fakeWhoami replaces `agentbus whoami` for the rest of the test.
func fakeWhoami(t *testing.T, stdout string, err error) *int {
	t.Helper()
	calls := 0
	old := agentbusWhoami
	agentbusWhoami = func(context.Context) (string, error) {
		calls++
		return stdout, err
	}
	t.Cleanup(func() { agentbusWhoami = old })
	return &calls
}

func relayAsk(extra ...string) []string {
	args := append([]string{"ask", "--kind", "relay", "--option", "a:Alpha", "--option", "b:Beta", "--rec", "a", "--json"}, extra...)
	return append(args, "q")
}

func askItem(t *testing.T, args ...string) (decide.Item, string) {
	t.Helper()
	stdout, stderr, code := runCLI(args...)
	if code != 0 {
		t.Fatalf("%q = %d\n%s", args, code, stderr)
	}
	var item decide.Item
	if err := json.Unmarshal([]byte(stdout), &item); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout)
	}
	return item, stderr
}

func TestAskInHerdrDefaultsOwnerToTabLabel(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, whoamiOpsInbox, nil)
	item, stderr := askItem(t, relayAsk()...)
	if item.Owner != "review" {
		t.Fatalf("owner = %q, want the tab label review", item.Owner)
	}
	if item.NotifyPane != "w1:p3" || item.NotifyMachine != "" {
		t.Fatalf("notify = %q on %q, want w1:p3 locally", item.NotifyPane, item.NotifyMachine)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestAskOutsideHerdrRequiresOwner(t *testing.T) {
	inHerdrPane(t)
	t.Setenv("HERDR_PANE_ID", "")
	fakeWhoami(t, whoamiOpsInbox, nil)
	_, stderr, code := runCLI(relayAsk()...)
	if code != 2 || !strings.Contains(stderr, "decide owner is required") {
		t.Fatalf("ask = %d, stderr %q", code, stderr)
	}
}

func TestAskOwnerMatchingTabOrBusNameDoesNotWarn(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, whoamiOpsInbox, nil)
	for _, owner := range []string{"review", "ops/inbox"} {
		item, stderr := askItem(t, relayAsk("--owner", owner)...)
		if item.Owner != owner || stderr != "" {
			t.Fatalf("--owner %s: owner %q, stderr %q", owner, item.Owner, stderr)
		}
		if item.NotifyPane != "w1:p3" {
			t.Fatalf("--owner %s: notifyPane = %q, want w1:p3", owner, item.NotifyPane)
		}
	}
}

func TestAskOwnerMismatchWarnsAndFiles(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "ops/inbox\nwhen: reviewing\nops/triage\nwhen: triage\n", nil)
	item, stderr := askItem(t, relayAsk("--owner", "ada")...)
	want := `warning: owner "ada" matches neither this tab ("review") nor your agentbus names (ops/inbox, ops/triage); the user finds you by tab` + "\n"
	if stderr != want {
		t.Fatalf("stderr = %q, want %q", stderr, want)
	}
	if item.Owner != "ada" || item.ID == "" {
		t.Fatalf("item = %+v, want filed for ada", item)
	}
}

func TestAskWithoutAgentbusComparesOnlyTheTab(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "", errors.New(`run agentbus: exec: "agentbus": executable file not found in $PATH`))
	if _, stderr := askItem(t, relayAsk("--owner", "review")...); stderr != "" {
		t.Fatalf("tab owner stderr = %q", stderr)
	}
	_, stderr := askItem(t, relayAsk("--owner", "ops/inbox")...)
	if !strings.Contains(stderr, `warning: owner "ops/inbox" matches neither this tab ("review") nor your agentbus names (none)`) {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestAskNoRegisteredNameIsNone(t *testing.T) {
	inHerdrPane(t)
	fakeWhoami(t, "herdr:local/s/w1:p3\n", nil)
	_, stderr := askItem(t, relayAsk("--owner", "herdr:local/s/w1:p3")...)
	if !strings.Contains(stderr, "nor your agentbus names (none)") {
		t.Fatalf("stderr = %q", stderr)
	}
}

func TestParseWhoami(t *testing.T) {
	for _, tc := range []struct {
		in   string
		want []string
	}{
		{"", nil},
		{"herdr:m/s/p\n", nil},
		{"ops/inbox\nwhen: x\n", []string{"ops/inbox"}},
		{"a\nwhen: x\nb\nwhen: y\n", []string{"a", "b"}},
	} {
		if got := parseWhoami(tc.in); !slices.Equal(got, tc.want) {
			t.Fatalf("parseWhoami(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestHubAskSendsTheTabLabelAndBusNames(t *testing.T) {
	fake, _ := setupHub(t, `{"hub": "me@hub", "machine": "m2"}`)
	herdr := herdrtest.Install(t)
	herdr.Write("local.agents", herdrAgentsW1P3)
	herdr.Write("local.panes", herdrPanesW1P3)
	herdr.Write("local.tabs", herdrTabsW1T2)
	t.Setenv("HERDR_PANE_ID", "w1:p3")
	fakeWhoami(t, "ops/inbox\nwhen: x\nops/triage\nwhen: y\n", nil)
	var stderr bytes.Buffer
	if code := run([]string{"ask", "--kind", "relay", "q"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("ask = %d, stderr %q", code, stderr.String())
	}
	env := fake.Lines("decide.env")
	for _, want := range []string{"DECIDE_TAB_LABEL=review", "DECIDE_BUS_NAMES=ops/inbox,ops/triage"} {
		if !slices.Contains(env, want) {
			t.Fatalf("hub env = %q, want %s", env, want)
		}
	}
}

func TestHubUsesTheClientTabLabel(t *testing.T) {
	onHub(t, "herdr:m2/s/p3")
	t.Setenv("HERDR_PANE_ID", "")
	t.Setenv("DECIDE_TAB_LABEL", "review")
	t.Setenv("DECIDE_BUS_NAMES", "ops/inbox,ops/triage")
	calls := fakeWhoami(t, "", errors.New("the hub must not run agentbus"))

	item, stderr := askItem(t, relayAsk()...)
	if item.Owner != "review" || item.NotifyPane != "p3" || item.NotifyMachine != "m2" || stderr != "" {
		t.Fatalf("default owner item = %+v, stderr %q", item, stderr)
	}
	if _, stderr := askItem(t, relayAsk("--owner", "ops/triage")...); stderr != "" {
		t.Fatalf("bus name owner stderr = %q", stderr)
	}
	_, stderr = askItem(t, relayAsk("--owner", "ada")...)
	want := `warning: owner "ada" matches neither this tab ("review") nor your agentbus names (ops/inbox, ops/triage); the user finds you by tab` + "\n"
	if stderr != want {
		t.Fatalf("stderr = %q, want %q", stderr, want)
	}
	if *calls != 0 {
		t.Fatalf("hub ran agentbus whoami %d times", *calls)
	}
}

func TestHubIgnoresTheTabLabelWithoutATrustedCaller(t *testing.T) {
	inHerdrPane(t)
	t.Setenv("HERDR_PANE_ID", "")
	t.Setenv("DECIDE_TAB_LABEL", "review")
	fakeWhoami(t, "", nil)
	_, stderr, code := runCLI(relayAsk()...)
	if code != 2 || !strings.Contains(stderr, "decide owner is required") {
		t.Fatalf("ask = %d, stderr %q", code, stderr)
	}
}
