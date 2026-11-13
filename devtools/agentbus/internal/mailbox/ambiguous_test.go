package mailbox

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/registry"
)

const ambiguousHandle = "herdr:local/s1/w1:p1"

func openAmbiguous(t *testing.T, ad adapter.Adapter) (*Mailbox, *eventlog.Log) {
	t.Helper()
	dir := t.TempDir()
	ads := []adapter.Adapter{ad}
	reg, err := registry.Open(dir, ads, nil)
	if err != nil {
		t.Fatal(err)
	}
	ctx := context.Background()
	if _, err := reg.Register(ctx, []string{"HERDR_PANE_ID=p1"}, "ops/x", "reviews"); err != nil {
		t.Fatal(err)
	}
	log := eventlog.Open(dir)
	box, err := Open(dir, reg, ads, log, 0)
	if err != nil {
		t.Fatal(err)
	}
	return box, log
}

func ambiguousFake() *fake.Adapter {
	return &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ambiguousHandle: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": ambiguousHandle},
		DeliverErr:  errors.New("herdr agent prompt: broken pipe"),
	}
}

func sendAmbiguous(t *testing.T, box *Mailbox) Result {
	t.Helper()
	res, err := box.Send(context.Background(), []string{"HERDR_PANE_ID=p1"}, 0, "ops/x", "hello", Options{})
	if err != nil {
		t.Fatal(err)
	}
	return res
}

func statusOf(t *testing.T, box *Mailbox, id string) string {
	t.Helper()
	got, err := box.Status(context.Background(), id)
	if err != nil {
		t.Fatal(err)
	}
	return got
}

func logText(t *testing.T, log *eventlog.Log) string {
	t.Helper()
	lines, err := log.Read(eventlog.Filter{})
	if err != nil {
		t.Fatal(err)
	}
	return strings.Join(lines, "\n")
}

func TestDeliverErrorConfirmsFromScrollback(t *testing.T) {
	ad := ambiguousFake()
	ad.OnDeliver = func(text string) { ad.Screen = text }
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	if res.State != "held: "+adapter.HoldAmbiguous || ad.Calls != 1 {
		t.Fatalf("after send state %q calls %d", res.State, ad.Calls)
	}
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: delivered\n") || ad.Calls != 1 || ad.Submits != 0 {
		t.Fatalf("status %q calls %d submits %d", got, ad.Calls, ad.Submits)
	}
	text := logText(t, log)
	if !strings.Contains(text, `"reason":"ambiguous"`) || !strings.Contains(text, `"detail":"confirmed-after-ambiguous"`) {
		t.Fatalf("log = %s", text)
	}
	if strings.Contains(text, "transport failed") || strings.Contains(text, "user draft") {
		t.Fatalf("log treated the paste as a miss or a draft: %s", text)
	}
}

func TestFailedResultConfirmsFromScrollback(t *testing.T) {
	ad := ambiguousFake()
	ad.DeliverErr = nil
	ad.Result = map[string]adapter.Result{ambiguousHandle: adapter.ResultFailed}
	ad.OnDeliver = func(text string) { ad.Screen = text }
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	if res.State != "held: "+adapter.HoldAmbiguous {
		t.Fatalf("state = %q", res.State)
	}
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: delivered\n") || ad.Calls != 1 {
		t.Fatalf("status %q calls %d", got, ad.Calls)
	}
	if !strings.Contains(logText(t, log), `"detail":"confirmed-after-ambiguous"`) {
		t.Fatal(logText(t, log))
	}
}

func TestScrollbackHitWinsWhenTheComposerCannotBeParsed(t *testing.T) {
	ad := ambiguousFake()
	ad.OnDeliver = func(text string) {
		ad.Screen = text
		ad.ScreenUnreadable = true
	}
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: delivered\n") || ad.Calls != 1 {
		t.Fatalf("status %q calls %d", got, ad.Calls)
	}
	if !strings.Contains(logText(t, log), `"detail":"confirmed-after-ambiguous"`) {
		t.Fatal(logText(t, log))
	}
}

func TestComposerHoldsOurPasteSoTheRetryPressesEnter(t *testing.T) {
	ad := ambiguousFake()
	ad.OnDeliver = func(text string) { ad.ComposerText = text }
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: delivered\n") || ad.Calls != 1 || ad.Submits != 1 || len(ad.Texts) != 1 {
		t.Fatalf("status %q calls %d submits %d texts %d", got, ad.Calls, ad.Submits, len(ad.Texts))
	}
	if !strings.Contains(logText(t, log), `"detail":"submitted-after-ambiguous"`) {
		t.Fatal(logText(t, log))
	}
	if strings.Contains(logText(t, log), "user draft") {
		t.Fatal(logText(t, log))
	}
}

func TestSubmitErrorHoldsAmbiguousWithoutAnotherPaste(t *testing.T) {
	ad := ambiguousFake()
	ad.SubmitErr = errors.New("enter failed")
	ad.OnDeliver = func(text string) { ad.ComposerText = text }
	box, _ := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: held: "+adapter.HoldAmbiguous+"\n") || ad.Calls != 1 || ad.Submits != 1 {
		t.Fatalf("status %q calls %d submits %d", got, ad.Calls, ad.Submits)
	}
}

func TestNothingOnScreenRetriesThePasteOnce(t *testing.T) {
	ad := ambiguousFake()
	var n int
	ad.OnDeliver = func(string) {
		n++
		if n >= 2 {
			ad.DeliverErr = nil
			ad.Result = map[string]adapter.Result{ambiguousHandle: adapter.ResultAccepted}
		}
	}
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	if ad.Calls != 1 || res.State != "held: "+adapter.HoldAmbiguous {
		t.Fatalf("after send state %q calls %d", res.State, ad.Calls)
	}
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: delivered\n") || ad.Calls != 2 || ad.Submits != 0 || len(ad.Texts) != 2 {
		t.Fatalf("status %q calls %d submits %d texts %d", got, ad.Calls, ad.Submits, len(ad.Texts))
	}
	again := statusOf(t, box, res.ID)
	if !strings.Contains(again, "state: delivered\n") || ad.Calls != 2 {
		t.Fatalf("second status %q calls %d", again, ad.Calls)
	}
	text := logText(t, log)
	if strings.Count(text, `"event":"deliver_attempt"`) != 2 {
		t.Fatalf("log = %s", text)
	}
}

func TestUnreadableScreenAfterAnAmbiguousErrorDoesNotPasteAgain(t *testing.T) {
	ad := ambiguousFake()
	ad.OnDeliver = func(string) { ad.ScreenUnreadable = true }
	box, _ := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: held: "+adapter.HoldAmbiguousUnverified+"\n") || ad.Calls != 1 || ad.Submits != 0 {
		t.Fatalf("status %q calls %d submits %d", got, ad.Calls, ad.Submits)
	}
	again := statusOf(t, box, res.ID)
	if !strings.Contains(again, "state: held: "+adapter.HoldAmbiguousUnverified+"\n") || ad.Calls != 1 {
		t.Fatalf("second status %q calls %d", again, ad.Calls)
	}
}

func TestAnotherEnvelopeOrAUserDraftIsStillADraft(t *testing.T) {
	drafts := []string{
		"[agentbus m-10 from x → y · 09:00]\nno\n",
		"please hold on",
	}
	for _, draft := range drafts {
		t.Run(draft, func(t *testing.T) {
			ad := ambiguousFake()
			ad.OnDeliver = func(string) { ad.ComposerText = draft }
			box, _ := openAmbiguous(t, ad)
			res := sendAmbiguous(t, box)
			got := statusOf(t, box, res.ID)
			if !strings.Contains(got, "state: held: "+adapter.HoldDraft+"\n") || ad.Calls != 1 || ad.Submits != 0 {
				t.Fatalf("status %q calls %d submits %d", got, ad.Calls, ad.Submits)
			}
			again := statusOf(t, box, res.ID)
			if !strings.Contains(again, "state: held: "+adapter.HoldDraft+"\n") || ad.Calls != 1 || ad.Submits != 0 {
				t.Fatalf("second status %q calls %d submits %d", again, ad.Calls, ad.Submits)
			}
		})
	}
}

func TestRefusedTransportStillRetriesThePaste(t *testing.T) {
	ad := ambiguousFake()
	ad.DeliverErr = nil
	ad.Result = map[string]adapter.Result{ambiguousHandle: adapter.ResultRefusedRetryable}
	ad.OnDeliver = func(text string) { ad.Screen = text }
	box, log := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: held: transport refused\n") || ad.Calls != 2 {
		t.Fatalf("status %q calls %d", got, ad.Calls)
	}
	if strings.Contains(logText(t, log), "ambiguous") || strings.Contains(got, "delivered") {
		t.Fatalf("status %q log %s", got, logText(t, log))
	}
}

func TestNoConfirmerHoldsUnverified(t *testing.T) {
	ad := &noProbe{handle: ambiguousHandle}
	box, _ := openAmbiguous(t, ad)
	res := sendAmbiguous(t, box)
	got := statusOf(t, box, res.ID)
	if !strings.Contains(got, "state: held: "+adapter.HoldAmbiguousUnverified+"\n") || ad.calls != 1 {
		t.Fatalf("status %q calls %d", got, ad.calls)
	}
}

func TestHasEnvelopeStopsAtTheIDBoundary(t *testing.T) {
	if hasEnvelope("[agentbus m-10 from a → b]", "m-1") {
		t.Fatal("m-1 matched m-10")
	}
	if !hasEnvelope("[agentbus m-1 from a → b]", "m-1") {
		t.Fatal("m-1 missed its own header")
	}
	if !hasEnvelope("[agentbus m-1]\n", "m-1") {
		t.Fatal("closing bracket was not a boundary")
	}
	if hasEnvelope("agentbus m-1 from a", "m-1") {
		t.Fatal("matched without the header mark")
	}
}

func TestUnverifiedHoldsListsThatReason(t *testing.T) {
	dir := t.TempDir()
	body := `{"messages":[
	  {"id":"m-2","from":"a","to":"ops/late","sentAt":"2026-01-02T00:00:00Z","body":"b","state":"held: ambiguous-unverified","handle":"h"},
	  {"id":"m-1","from":"a","to":"ops/x","sentAt":"2026-01-01T00:00:00Z","body":"a","state":"held: ambiguous","handle":"h"},
	  {"id":"m-3","from":"a","to":"ops/y","sentAt":"2026-01-01T00:00:00Z","body":"c","state":"delivered","handle":"h"},
	  {"id":"m-0","from":"a","to":"ops/early","sentAt":"2026-01-01T00:00:00Z","body":"d","state":"held: ambiguous-unverified","handle":"h"}
	]}`
	if err := os.WriteFile(filepath.Join(dir, "messages.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	lines, err := UnverifiedHolds(dir)
	if err != nil {
		t.Fatal(err)
	}
	want := "ambiguous-unverified: m-0 to ops/early\nambiguous-unverified: m-2 to ops/late"
	if strings.Join(lines, "\n") != want {
		t.Fatalf("lines = %q", lines)
	}
	empty, err := UnverifiedHolds(t.TempDir())
	if err != nil || len(empty) != 0 {
		t.Fatalf("missing file = %q, %v", empty, err)
	}
}

type noProbe struct {
	handle string
	calls  int
}

func (a *noProbe) Name() string { return "herdr" }

func (a *noProbe) Inventory(context.Context) ([]adapter.Agent, error) {
	return []adapter.Agent{{Handle: a.handle, Status: "idle"}}, nil
}

func (a *noProbe) Identify(context.Context, []string) (string, error) {
	return a.handle, nil
}

func (a *noProbe) State(context.Context, string) (adapter.State, error) {
	return adapter.StateIdle, nil
}

func (a *noProbe) ComposerEmpty(context.Context, string) (bool, string, error) {
	return true, "", nil
}

func (a *noProbe) Deliver(context.Context, string, string) (adapter.Result, error) {
	a.calls++
	return adapter.ResultFailed, errors.New("dropped")
}

func (a *noProbe) SelfTest(context.Context) error { return nil }
