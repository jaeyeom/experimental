package decide

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func TestFollowStaleResendsOnceWithTheSameKey(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
	led, clock, logPath := followLedger(t)
	item := answeredRelay(t, led, "w3:p7", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	key := DeliveryKey(&item)

	*clock = clock.Add(29 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if keys := keyLog(t, logPath); len(keys) != 1 {
		t.Fatalf("keys before 30m = %#v", keys)
	}

	*clock = item.AnsweredAt.Add(30 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if keys := keyLog(t, logPath); len(keys) != 2 || keys[0] != key || keys[1] != key {
		t.Fatalf("keys at 30m = %#v, want %q twice", keys, key)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 2 || got.Delivery[1].Outcome != OutcomeResend {
		t.Fatalf("delivery = %+v", got.Delivery)
	}

	*clock = item.AnsweredAt.Add(60 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if keys := keyLog(t, logPath); len(keys) != 2 {
		t.Fatalf("keys at 60m = %#v, want no second re-send", keys)
	}
}

func TestFollowStaleSkipsWhenTheOwnerRepliedOrFinished(t *testing.T) {
	t.Run("replied", func(t *testing.T) {
		f := herdrtest.Install(t)
		fakeHerdrMachine(f, "local")
		f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
		led, clock, logPath := followLedger(t)
		item := answeredRelay(t, led, "w3:p7", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}
		if _, err := led.Reply(item.ID, item.Owner, "on it", nil, false); err != nil {
			t.Fatal(err)
		}
		*clock = clock.Add(time.Hour)
		if err := led.FollowStale(*clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		if keys := keyLog(t, logPath); len(keys) != 1 {
			t.Fatalf("keys after reply = %#v", keys)
		}
	})
	t.Run("done", func(t *testing.T) {
		f := herdrtest.Install(t)
		fakeHerdrMachine(f, "local")
		f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
		led, clock, logPath := followLedger(t)
		item := answeredRelay(t, led, "w3:p7", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}
		if _, err := led.Done(item.ID, "shipped"); err != nil {
			t.Fatal(err)
		}
		*clock = clock.Add(time.Hour)
		if err := led.FollowStale(*clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		if keys := keyLog(t, logPath); len(keys) != 1 {
			t.Fatalf("keys after done = %#v", keys)
		}
	})
	t.Run("read", func(t *testing.T) {
		f := herdrtest.Install(t)
		fakeHerdrMachine(f, "local")
		f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
		led, clock, logPath := followLedger(t)
		item := answeredRelay(t, led, "w3:p7", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}
		marked, err := led.MarkRead(item.ID)
		if err != nil {
			t.Fatal(err)
		}
		if marked.ReadAt == nil || marked.ReadAt.Before(item.AnsweredAt) {
			t.Fatalf("readAt = %v, answeredAt = %v", marked.ReadAt, item.AnsweredAt)
		}
		*clock = clock.Add(time.Hour)
		if err := led.FollowStale(*clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		if keys := keyLog(t, logPath); len(keys) != 1 {
			t.Fatalf("keys after mark read = %#v", keys)
		}
	})
}

func TestFollowStaleMarksOwnerGone(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
	led, clock, logPath := followLedger(t)
	item := answeredRelay(t, led, "w3:p9", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	*clock = clock.Add(30 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if got.FollowUp != FollowUpOwnerGone {
		t.Fatalf("followUp = %q", got.FollowUp)
	}
	if keys := keyLog(t, logPath); len(keys) != 1 {
		t.Fatalf("keys = %#v, want no re-send", keys)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome == OutcomeResend {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
	*clock = clock.Add(time.Minute)
	again, err := led.Answer(item.ID, "b", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if again.FollowUp != "" {
		t.Fatalf("new answer kept followUp %q", again.FollowUp)
	}
}

func TestFollowStaleMarksOwnerWaitingOnADialog(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"blocked"}]}}`)
	led, clock, logPath := followLedger(t)
	item := answeredRelay(t, led, "w3:p7", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	*clock = clock.Add(30 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	got := mustGet(t, led, item.ID)
	if got.FollowUp != FollowUpOwnerWaiting {
		t.Fatalf("followUp = %q", got.FollowUp)
	}
	if keys := keyLog(t, logPath); len(keys) != 1 {
		t.Fatalf("keys = %#v, want no re-send", keys)
	}
}

func TestFollowStaleRemoteOwnerUsesMachine(t *testing.T) {
	dir := t.TempDir()
	installProgram(t, "herdr", remoteHerdrScript(dir))
	writeConfig(t, `{"machine":"hub","sendTemplate":"herdr agent prompt {pane}"}`)
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, t.TempDir(), &clock)
	item := answeredRelay(t, led, "w3:p7", "mac")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	key := DeliveryKey(&item)

	clock = clock.Add(30 * time.Minute)
	if err := led.FollowStale(clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	argv := readText(t, filepath.Join(dir, "argv"))
	for _, want := range []string{
		"--machine mac pane list",
		"--machine mac agent list",
		"--machine mac agent prompt w3:p7",
	} {
		if !strings.Contains(argv, want) {
			t.Fatalf("herdr argv missing %q\n%s", want, argv)
		}
	}
	keys := keyLog(t, filepath.Join(dir, "keys"))
	if len(keys) != 2 || keys[0] != key || keys[1] != key {
		t.Fatalf("keys = %#v, want %q twice", keys, key)
	}
	got := mustGet(t, led, item.ID)
	if len(got.Delivery) != 2 || got.Delivery[1].Outcome != OutcomeResend || got.Delivery[1].Target != "w3:p7@mac" {
		t.Fatalf("delivery = %+v", got.Delivery)
	}

	clock = clock.Add(30 * time.Minute)
	if err := led.FollowStale(clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if again := keyLog(t, filepath.Join(dir, "keys")); len(again) != 2 {
		t.Fatalf("keys at 60m = %#v", again)
	}
}

func TestFollowStaleHonorsStaleAnswerAfter(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	writeConfig(t, `{"notifyTemplate":"notifier","ui":{"staleAnswerAfter":"45m"}}`)
	installProgram(t, "notifier", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "w3:p7", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	clock = item.AnsweredAt.Add(30 * time.Minute)
	if err := led.FollowStale(clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if keys := keyLog(t, logPath); len(keys) != 1 {
		t.Fatalf("keys at 30m with 45m setting = %#v", keys)
	}
	clock = item.AnsweredAt.Add(45 * time.Minute)
	if err := led.FollowStale(clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if keys := keyLog(t, logPath); len(keys) != 2 {
		t.Fatalf("keys at 45m = %#v", keys)
	}
}

func TestStaleAnswerAfterRejectsABadDuration(t *testing.T) {
	writeConfig(t, `{"ui":{"staleAnswerAfter":"soon"}}`)
	if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "staleAnswerAfter") {
		t.Fatalf("err = %v", err)
	}
}

func TestArchiveGraceDefaultsAndRejectsABadDuration(t *testing.T) {
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.ArchiveGrace() != DefaultArchiveGrace || DefaultArchiveGrace != 2*time.Second {
		t.Fatalf("default = %s", cfg.ArchiveGrace())
	}
	writeConfig(t, `{"ui":{"archiveGrace":"5s"}}`)
	cfg, err = LoadConfig()
	if err != nil || cfg.ArchiveGrace() != 5*time.Second {
		t.Fatalf("grace = %s err %v", cfg.ArchiveGrace(), err)
	}
	writeConfig(t, `{"ui":{"archiveGrace":"soon"}}`)
	if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "archiveGrace") {
		t.Fatalf("err = %v", err)
	}
}

func TestBusHealthIntervalDefaultsAndRejectsABadDuration(t *testing.T) {
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.BusHealthInterval() != DefaultBusHealthInterval || DefaultBusHealthInterval != 15*time.Second {
		t.Fatalf("default = %s", cfg.BusHealthInterval())
	}
	writeConfig(t, `{"ui":{"busHealthInterval":"1m"}}`)
	cfg, err = LoadConfig()
	if err != nil || cfg.BusHealthInterval() != time.Minute {
		t.Fatalf("interval = %s err %v", cfg.BusHealthInterval(), err)
	}
	writeConfig(t, `{"ui":{"busHealthInterval":"often"}}`)
	if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "busHealthInterval") {
		t.Fatalf("err = %v", err)
	}
}

func TestFollowStaleNewAnswerResetsTheResend(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
	led, clock, logPath := followLedger(t)
	item := answeredRelay(t, led, "w3:p7", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	*clock = clock.Add(30 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	*clock = clock.Add(time.Minute)
	again, err := led.Answer(item.ID, "b", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.DeliverAnswer(&again, nil); err != nil {
		t.Fatal(err)
	}
	second := DeliveryKey(&again)
	*clock = again.AnsweredAt.Add(30 * time.Minute)
	if err := led.FollowStale(*clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	keys := keyLog(t, logPath)
	if len(keys) != 4 || keys[3] != second || keys[1] == second {
		t.Fatalf("keys = %#v, want a new re-send %q", keys, second)
	}
}

// TestFollowStaleConcurrentPassesResendOnce is a runner and a serve
// session passing at the same moment: two ledger handles on one
// directory. The slow notifier keeps both passes in flight together.
func TestFollowStaleConcurrentPassesResendOnce(t *testing.T) {
	f := herdrtest.Install(t)
	fakeHerdrMachine(f, "local")
	f.Write("local.agents", `{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}`)
	led, clock, logPath := followLedger(t)
	installProgram(t, "notifier", "#!/bin/sh\nsleep 0.3\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\n")
	item := answeredRelay(t, led, "w3:p7", "")
	other := openDeliveryLedger(t, led.dir, clock)
	now := item.AnsweredAt.Add(31 * time.Minute)

	var wg sync.WaitGroup
	errs := make([]error, 2)
	for i, l := range []*Ledger{led, other} {
		wg.Add(1)
		go func() {
			defer wg.Done()
			errs[i] = l.FollowStale(now, nil, nil)
		}()
	}
	wg.Wait()
	for _, err := range errs {
		if err != nil {
			t.Fatal(err)
		}
	}
	if keys := keyLog(t, logPath); len(keys) != 1 {
		t.Fatalf("keys = %#v, want one re-send", keys)
	}
	if got := mustGet(t, led, item.ID); len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeResend {
		t.Fatalf("delivery = %+v", got.Delivery)
	}
}

func TestFollowStaleSkipsPullTargets(t *testing.T) {
	for _, pane := range []string{"pull", "pull:desktop"} {
		t.Run(pane, func(t *testing.T) {
			f := herdrtest.Install(t)
			led, clock, logPath := followLedger(t)
			item := answeredRelay(t, led, pane, "")

			*clock = clock.Add(time.Hour)
			if err := led.FollowStale(*clock, nil, nil); err != nil {
				t.Fatal(err)
			}
			got := mustGet(t, led, item.ID)
			if got.FollowUp != "" {
				t.Fatalf("followUp = %q, want empty", got.FollowUp)
			}
			if len(got.Delivery) != 0 {
				t.Fatalf("delivery = %+v, want empty", got.Delivery)
			}
			if keys := keyLog(t, logPath); len(keys) != 0 {
				t.Fatalf("keys = %#v, want no resend", keys)
			}
			if argv := f.Argv(); len(argv) != 0 {
				t.Fatalf("herdr ran %q, want none", argv)
			}
		})
	}
}

func TestFollowStaleCommandTargetDoesNotCallHerdr(t *testing.T) {
	f := herdrtest.Install(t)
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	script := filepath.Join(dir, "mytool")
	if err := os.WriteFile(script, []byte("#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\n"), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	writeConfig(t, `{"targets":[{"name":"mytool","command":["`+script+`"]}]}`)
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "target:mytool", "")
	if _, err := led.DeliverAnswer(&item, nil); err != nil {
		t.Fatal(err)
	}
	key := DeliveryKey(&item)

	clock = item.AnsweredAt.Add(30 * time.Minute)
	if err := led.FollowStale(clock, nil, nil); err != nil {
		t.Fatal(err)
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q, want none for command target", argv)
	}
	got := mustGet(t, led, item.ID)
	if got.FollowUp != "" {
		t.Fatalf("followUp = %q, want empty", got.FollowUp)
	}
	if keys := keyLog(t, logPath); len(keys) != 2 || keys[0] != key || keys[1] != key {
		t.Fatalf("keys = %#v, want %q twice", keys, key)
	}
}

func followLedger(t *testing.T) (*Ledger, *time.Time, string) {
	t.Helper()
	dir := t.TempDir()
	logPath := filepath.Join(dir, "keys")
	writeConfig(t, `{"notifyTemplate":"notifier"}`)
	installProgram(t, "notifier", "#!/bin/sh\nprintf '%s\\n' \"$DECIDE_DELIVERY_KEY\" >> '"+logPath+"'\n")
	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	return openDeliveryLedger(t, dir, &clock), &clock, logPath
}

func keyLog(t *testing.T, path string) []string {
	t.Helper()
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		t.Fatal(err)
	}
	text := strings.TrimSpace(string(data))
	if text == "" {
		return nil
	}
	return strings.Split(text, "\n")
}

func remoteHerdrScript(dir string) string {
	return `#!/bin/sh
printf '%s\n' "$*" >> '` + dir + `/argv'
if [ -n "$DECIDE_DELIVERY_KEY" ]; then
  printf '%s\n' "$DECIDE_DELIVERY_KEY" >> '` + dir + `/keys'
fi
if [ "$1" = "--machine" ]; then shift 2; fi
case "$1 $2" in
"pane list")
  printf '%s\n' '{"result":{"panes":[{"pane_id":"w3:p7","tab_id":"w3:t2","workspace_id":"w3"}]}}' ;;
"tab list")
  printf '%s\n' '{"result":{"tabs":[{"tab_id":"w3:t2","label":"review"}]}}' ;;
"workspace list")
  printf '%s\n' '{"result":{"workspaces":[{"workspace_id":"w3","label":"experimental"}]}}' ;;
"agent list")
  printf '%s\n' '{"result":{"agents":[{"pane_id":"w3:p7","agent_status":"idle"}]}}' ;;
"agent prompt")
  printf '%s\n' '{"result":{}}' ;;
"machine status")
  printf '%s\n' '[{"id":"id-mac","label":"mac","status":"reachable","error":null}]' ;;
*)
  echo "unexpected $*" >&2
  exit 2 ;;
esac
`
}
