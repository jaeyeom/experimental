package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestDefaultDirUsesXDGStateHome(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "/tmp/xdg-state-home")

	got, err := DefaultDir()
	if err != nil {
		t.Fatalf("DefaultDir() error = %v", err)
	}
	want := filepath.Join("/tmp/xdg-state-home", "decide")
	if got != want {
		t.Fatalf("DefaultDir() = %q, want %q", got, want)
	}
}

func TestDefaultDirFallsBackToLocalState(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "")
	t.Setenv("HOME", "/home/tester")

	got, err := DefaultDir()
	if err != nil {
		t.Fatalf("DefaultDir() error = %v", err)
	}
	want := filepath.Join("/home/tester", ".local", "state", "decide")
	if got != want {
		t.Fatalf("DefaultDir() = %q, want %q", got, want)
	}
}

func TestCreateAssignsSequentialIDsAndWithdrawKeepsTheRecord(t *testing.T) {
	led, _ := openTemp(t)

	first, err := led.Create(&Item{
		Question: "first",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "A", Recommended: true},
			{Key: "b", Label: "B"},
		},
	})
	if err != nil {
		t.Fatalf("Create(first) error = %v", err)
	}
	if first.ID != "D-001" {
		t.Fatalf("first id = %q, want D-001", first.ID)
	}
	if first.State != StateOpen {
		t.Fatalf("first state = %q, want open", first.State)
	}
	if first.CreatedAt.IsZero() {
		t.Fatal("first CreatedAt is zero")
	}

	second, err := led.Create(&Item{Question: "second", Kind: KindRelay, Owner: "bea"})
	if err != nil {
		t.Fatalf("Create(second) error = %v", err)
	}
	if second.ID != "D-002" {
		t.Fatalf("second id = %q, want D-002", second.ID)
	}

	withdrawn, err := led.Withdraw(first.ID, "dropped")
	if err != nil {
		t.Fatalf("Withdraw(D-001) error = %v", err)
	}
	if withdrawn.State != StateWithdrawn {
		t.Fatalf("withdrawn state = %q", withdrawn.State)
	}
	if withdrawn.WithdrawnAt.IsZero() {
		t.Fatal("WithdrawnAt is zero")
	}
	if withdrawn.Question != "first" || withdrawn.Owner != "ada" {
		t.Fatalf("withdrawn record = %+v", withdrawn)
	}

	third, err := led.Create(&Item{Question: "third", Kind: KindTypeIn, TypeText: "ship it", Where: "the box"})
	if err != nil {
		t.Fatalf("Create(third) error = %v", err)
	}
	if third.ID != "D-003" {
		t.Fatalf("id after withdraw = %q, want D-003", third.ID)
	}

	got, err := led.Get("D-001")
	if err != nil {
		t.Fatalf("Get(D-001) error = %v", err)
	}
	if got.State != StateWithdrawn || got.Question != "first" {
		t.Fatalf("stored withdrawn item = %+v", got)
	}
	if _, err := led.Get("D-999"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("Get(missing) error = %v, want ErrNotFound", err)
	}
}

func TestRoundTripEveryField(t *testing.T) {
	led, dir := openTemp(t)

	full := itemForMark("full")
	got, err := led.Create(full)
	if err != nil {
		t.Fatalf("Create(full) error = %v", err)
	}
	full.ID = got.ID
	if full.ID != "D-001" {
		t.Fatalf("id = %q, want D-001", full.ID)
	}
	stored, err := led.Get(full.ID)
	if err != nil {
		t.Fatalf("Get(full) error = %v", err)
	}
	assertItemEqual(t, stored, *full)
	assertItemEqual(t, got, *full)

	plain := itemForMark("plain")
	gotPlain, err := led.Create(plain)
	if err != nil {
		t.Fatalf("Create(plain) error = %v", err)
	}
	plain.ID = gotPlain.ID
	storedPlain, err := led.Get(plain.ID)
	if err != nil {
		t.Fatalf("Get(plain) error = %v", err)
	}
	assertItemEqual(t, storedPlain, *plain)

	data, err := os.ReadFile(filepath.Join(dir, ledgerName)) //nolint:gosec // test ledger
	if err != nil {
		t.Fatalf("read ledger: %v", err)
	}
	for _, needle := range []string{`"freeText": false`, `"unread": false`, `"recommended": false`} {
		if !bytes.Contains(data, []byte(needle)) {
			t.Fatalf("ledger missing %s\n%s", needle, data)
		}
	}
}

func TestCreateRejectsInvalidItems(t *testing.T) {
	led, _ := openTemp(t)
	if _, err := led.Create(nil); err == nil {
		t.Fatal("Create(nil) error = nil")
	}
	if _, err := led.Create(&Item{Kind: "other"}); err == nil {
		t.Fatal("Create(bad kind) error = nil")
	}
	_, err := led.Create(&Item{
		Kind: KindRelay,
		Options: []Option{
			{Key: "a", Label: "A", Recommended: true},
			{Key: "b", Label: "B", Recommended: true},
		},
	})
	if err == nil {
		t.Fatal("Create(two recommended) error = nil")
	}
	items, err := led.List()
	if err != nil {
		t.Fatalf("List() error = %v", err)
	}
	if len(items) != 0 {
		t.Fatalf("len(items) = %d, want 0", len(items))
	}
}

func TestAnswerDoneAndWithdraw(t *testing.T) {
	led, _ := openTemp(t)
	relay, err := led.Create(&Item{
		Question: "pick",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "A", Recommended: true},
			{Key: "b", Label: "B"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(relay.ID, "nope", ViaCLI); err == nil {
		t.Fatal("free text was accepted")
	}
	got, err := led.Answer(relay.ID, "b", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateAnswered || got.Answer == nil || got.Answer.Value != "b" || got.Answer.AnsweredBy != "human" {
		t.Fatalf("answer = %+v", got.Answer)
	}

	openFree, err := led.Create(&Item{Question: "or else", Kind: KindRelay, FreeText: true, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	free, err := led.Answer(openFree.ID, "custom", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if free.Answer == nil || free.Answer.Value != "custom" || free.Answer.AnsweredBy != "human" {
		t.Fatalf("free answer = %+v", free.Answer)
	}
	kept, err := led.Create(&Item{
		Question: "marks",
		Kind:     KindRelay,
		FreeText: true,
		Owner:    "ada",
		Options:  []Option{{Key: "a", Label: "A", Recommended: true}},
	})
	if err != nil {
		t.Fatal(err)
	}
	raw := "  say \"hi\" $(uname)  "
	gotFree, err := led.Answer(kept.ID, raw, ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if gotFree.Answer == nil || gotFree.Answer.Value != raw {
		t.Fatalf("free answer = %q", gotFree.Answer)
	}
	spaced, err := led.Create(&Item{
		Question: "space",
		Kind:     KindRelay,
		FreeText: true,
		Owner:    "ada",
		Options:  []Option{{Key: "a", Label: "A", Recommended: true}},
	})
	if err != nil {
		t.Fatal(err)
	}
	gotSpace, err := led.Answer(spaced.ID, " a ", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if gotSpace.Answer == nil || gotSpace.Answer.Value != " a " {
		t.Fatalf("free answer = %+v, want the untrimmed text", gotSpace.Answer)
	}

	typed, err := led.Create(&Item{Question: "type", Kind: KindTypeIn, TypeText: "ship", Where: "box"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(typed.ID, "ship", ViaCLI); err == nil {
		t.Fatal("type-in answer succeeded")
	}
	done, err := led.Done(typed.ID, "typed it")
	if err != nil {
		t.Fatal(err)
	}
	if done.State != StateDone || done.Outcome != "typed it" || done.TypeText != "ship" || done.Where != "box" {
		t.Fatalf("done = %+v", done)
	}
	if _, err := led.Done(typed.ID, "again"); err == nil {
		t.Fatal("second done succeeded")
	}
	if _, err := led.Withdraw(typed.ID, "nope"); err == nil {
		t.Fatal("withdraw of done succeeded")
	}

	other, err := led.Create(&Item{Question: "drop", Kind: KindRelay, Owner: "bea"})
	if err != nil {
		t.Fatal(err)
	}
	withdrawn, err := led.Withdraw(other.ID, "not now")
	if err != nil {
		t.Fatal(err)
	}
	if withdrawn.State != StateWithdrawn || withdrawn.Reason != "not now" || withdrawn.Question != "drop" {
		t.Fatalf("withdrawn = %+v", withdrawn)
	}
	if _, err := led.Withdraw(other.ID, "again"); err == nil {
		t.Fatal("second withdraw succeeded")
	}
	if _, err := led.Answer(other.ID, "a", ViaCLI); err == nil {
		t.Fatal("answer of withdrawn succeeded")
	}
}

func TestSelectOrdersDeadlineThenNewest(t *testing.T) {
	soon := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	later := time.Date(2026, 12, 2, 0, 0, 0, 0, time.UTC)
	items := []Item{
		{ID: "D-001", State: StateOpen, Question: "later", Deadline: &later, CreatedAt: time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)},
		{ID: "D-002", State: StateOpen, Question: "sooner", Deadline: &soon, CreatedAt: time.Date(2026, 11, 2, 0, 0, 0, 0, time.UTC)},
		{ID: "D-003", State: StateOpen, Question: "older", CreatedAt: time.Date(2026, 11, 3, 0, 0, 0, 0, time.UTC)},
		{ID: "D-004", State: StateOpen, Question: "newer", CreatedAt: time.Date(2026, 11, 4, 0, 0, 0, 0, time.UTC)},
		{ID: "D-005", State: StateAnswered, Question: "hidden", CreatedAt: time.Date(2026, 11, 5, 0, 0, 0, 0, time.UTC)},
	}
	got, err := Select(items, StateOpen)
	if err != nil {
		t.Fatal(err)
	}
	var ids []string
	for _, it := range got {
		ids = append(ids, it.ID)
	}
	want := []string{"D-002", "D-001", "D-004", "D-003"}
	if strings.Join(ids, ",") != strings.Join(want, ",") {
		t.Fatalf("order = %v, want %v", ids, want)
	}
	if _, err := Select(items, "nope"); err == nil {
		t.Fatal("invalid state succeeded")
	}
}

func TestTwoProcessesCreate(t *testing.T) {
	led, dir := openTemp(t)
	held, err := led.acquire()
	if err != nil {
		t.Fatalf("acquire() error = %v", err)
	}
	released := false
	t.Cleanup(func() {
		if !released {
			if err := release(held); err != nil {
				t.Errorf("release() error = %v", err)
			}
		}
	})

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	one := startChild(ctx, t, dir, "one")
	two := startChild(ctx, t, dir, "two")
	waitReady(t, one.ready, two.ready)
	assertBlocked(t, one, two)

	if err := release(held); err != nil {
		t.Fatalf("release() error = %v", err)
	}
	released = true
	waitChild(t, one)
	waitChild(t, two)

	idOne := readResult(t, one.result)
	idTwo := readResult(t, two.result)
	if idOne == idTwo {
		t.Fatalf("both processes got %s", idOne)
	}
	gotIDs := map[string]bool{idOne: true, idTwo: true}
	if !gotIDs["D-001"] || !gotIDs["D-002"] {
		t.Fatalf("ids = %s %s, want D-001 and D-002", idOne, idTwo)
	}

	for _, tc := range []struct {
		mark string
		id   string
	}{
		{mark: "one", id: idOne},
		{mark: "two", id: idTwo},
	} {
		stored, err := led.Get(tc.id)
		if err != nil {
			t.Fatalf("Get(%s) error = %v", tc.id, err)
		}
		want := itemForMark(tc.mark)
		want.ID = tc.id
		assertItemEqual(t, stored, *want)
	}
}

func TestDecideChildCreate(t *testing.T) {
	if os.Getenv("DECIDE_CHILD") != "1" {
		t.Skip("subprocess helper")
	}
	ready := os.Getenv("DECIDE_READY")
	if err := os.WriteFile(ready, []byte("ready\n"), 0o600); err != nil {
		t.Fatalf("write ready: %v", err)
	}
	led, err := Open(os.Getenv("DECIDE_DIR"))
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	got, err := led.Create(itemForMark(os.Getenv("DECIDE_MARK")))
	if err != nil {
		t.Fatalf("Create() error = %v", err)
	}
	if err := os.WriteFile(os.Getenv("DECIDE_RESULT"), []byte(got.ID+"\n"), 0o600); err != nil {
		t.Fatalf("write result: %v", err)
	}
}

type childProc struct {
	done   chan error
	out    *bytes.Buffer
	ready  string
	result string
}

func startChild(ctx context.Context, t *testing.T, dir, mark string) *childProc {
	t.Helper()
	ready := filepath.Join(dir, "ready-"+mark)
	result := filepath.Join(dir, "result-"+mark)
	cmd := exec.CommandContext( //nolint:gosec // re-exec this test binary
		ctx,
		os.Args[0],
		"-test.run=^TestDecideChildCreate$",
		"-test.count=1",
		"-test.timeout=20s",
	)
	cmd.Env = childEnv(dir, mark, ready, result)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Start(); err != nil {
		t.Fatalf("start child %s: %v", mark, err)
	}
	proc := &childProc{
		done:   make(chan error, 1),
		out:    &out,
		ready:  ready,
		result: result,
	}
	go func() { proc.done <- cmd.Wait() }()
	return proc
}

func childEnv(dir, mark, ready, result string) []string {
	env := make([]string, 0, len(os.Environ())+4)
	for _, kv := range os.Environ() {
		if dropChildEnv(kv) {
			continue
		}
		env = append(env, kv)
	}
	return append(env,
		"DECIDE_CHILD=1",
		"DECIDE_DIR="+dir,
		"DECIDE_MARK="+mark,
		"DECIDE_READY="+ready,
		"DECIDE_RESULT="+result,
	)
}

func dropChildEnv(kv string) bool {
	for _, prefix := range []string{
		"XML_OUTPUT_FILE=",
		"COVERAGE_OUTPUT_FILE=",
		"GOCOVERDIR=",
		"TEST_SHARD_STATUS_FILE=",
	} {
		if strings.HasPrefix(kv, prefix) {
			return true
		}
	}
	return false
}

func waitReady(t *testing.T, paths ...string) {
	t.Helper()
	deadline := time.Now().Add(10 * time.Second)
	for _, path := range paths {
		for {
			if _, err := os.Stat(path); err == nil {
				break
			}
			if time.Now().After(deadline) {
				t.Fatalf("timed out waiting for %s", path)
			}
			time.Sleep(5 * time.Millisecond)
		}
	}
}

func assertBlocked(t *testing.T, procs ...*childProc) {
	t.Helper()
	deadline := time.Now().Add(400 * time.Millisecond)
	for time.Now().Before(deadline) {
		for _, proc := range procs {
			select {
			case err := <-proc.done:
				t.Fatalf("child finished while lock held: %v\n%s", err, proc.out.String())
			default:
			}
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func waitChild(t *testing.T, proc *childProc) {
	t.Helper()
	select {
	case err := <-proc.done:
		if err != nil {
			t.Fatalf("child error = %v\n%s", err, proc.out.String())
		}
	case <-time.After(20 * time.Second):
		t.Fatalf("child timed out\n%s", proc.out.String())
	}
}

func readResult(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path) //nolint:gosec // test result file
	if err != nil {
		t.Fatalf("read result: %v", err)
	}
	return strings.TrimSpace(string(data))
}

func TestOpenMigratesItemsWithoutDroppingFields(t *testing.T) {
	dir := t.TempDir()
	created := time.Date(2026, 11, 5, 1, 0, 0, 0, time.UTC)
	answeredAt := time.Date(2026, 11, 5, 2, 0, 0, 0, time.UTC)
	deadline := time.Date(2026, 12, 1, 0, 0, 0, 0, time.UTC)
	kept := Item{
		ID:       "D-001",
		Question: "ship it?",
		Options: []Option{
			{Key: "a", Label: "Yes", Recommended: true},
			{Key: "b", Label: "No"},
		},
		FreeText:   true,
		Owner:      "ada",
		Links:      []string{"https://example.com/a"},
		Deadline:   &deadline,
		Kind:       KindRelay,
		State:      StateAnswered,
		CreatedAt:  created,
		AnsweredAt: answeredAt,
		Answer:     &Answer{Value: "a", AnsweredBy: "human"},
		Unread:     true,
		NotifyPane: "pane-9",
		Outcome:    "kept",
		TypeText:   "exact",
		Where:      "box",
		Reason:     "because",
	}
	plain := Item{
		ID:        "D-002",
		Question:  "only ask",
		Owner:     "bea",
		Kind:      KindTypeIn,
		State:     StateOpen,
		CreatedAt: created,
	}
	already := Item{
		ID:        "D-003",
		Question:  "threaded",
		Owner:     "ada",
		Kind:      KindRelay,
		State:     StateOpen,
		CreatedAt: created,
		Messages:  []Message{{Author: "ada", Time: created, Text: "threaded"}},
	}
	body, err := json.MarshalIndent(store{Next: 4, Items: []Item{kept, plain, already}}, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, ledgerName), append(body, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}

	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Get("D-001")
	if err != nil {
		t.Fatal(err)
	}
	assertItemEqual(t, got, kept)
	if len(got.Messages) != 2 {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if got.Messages[0].Author != "ada" || got.Messages[0].Text != "ship it?" || !got.Messages[0].Time.Equal(created) {
		t.Fatalf("ask message = %+v", got.Messages[0])
	}
	if len(got.Messages[0].Options) != 2 || got.Messages[0].Options[0].Key != "a" || !got.Messages[0].Options[0].Recommended {
		t.Fatalf("ask options = %+v", got.Messages[0].Options)
	}
	if got.Messages[1].Author != "human" || got.Messages[1].Text != "a" || !got.Messages[1].Time.Equal(answeredAt) || len(got.Messages[1].Options) != 0 {
		t.Fatalf("answer message = %+v", got.Messages[1])
	}
	plainGot, err := led.Get("D-002")
	if err != nil {
		t.Fatal(err)
	}
	if plainGot.Question != "only ask" || len(plainGot.Messages) != 1 || plainGot.Messages[0].Author != "bea" || plainGot.Messages[0].Text != "only ask" {
		t.Fatalf("plain = %+v", plainGot)
	}
	threaded, err := led.Get("D-003")
	if err != nil {
		t.Fatal(err)
	}
	if len(threaded.Messages) != 1 || threaded.Messages[0].Text != "threaded" {
		t.Fatalf("existing thread = %+v", threaded.Messages)
	}

	again, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	second, err := again.Get("D-001")
	if err != nil {
		t.Fatal(err)
	}
	if len(second.Messages) != 2 || second.Outcome != "kept" || second.NotifyPane != "pane-9" {
		t.Fatalf("second open = %+v", second)
	}
	data, err := os.ReadFile(filepath.Join(dir, ledgerName)) //nolint:gosec // test ledger
	if err != nil {
		t.Fatal(err)
	}
	for _, needle := range []string{`"outcome": "kept"`, `"notifyPane": "pane-9"`, `"question": "ship it?"`, "https://example.com/a"} {
		if !bytes.Contains(data, []byte(needle)) {
			t.Fatalf("ledger missing %s\n%s", needle, data)
		}
	}
}

func TestAnswerReplyAndComposeKeepTheThread(t *testing.T) {
	led, _ := openTemp(t)
	when := time.Date(2026, 11, 6, 3, 4, 5, 0, time.UTC)
	led.now = func() time.Time { return when }
	item, err := led.Create(&Item{
		Question: "draft?",
		Owner:    "ada",
		Kind:     KindRelay,
		Options: []Option{
			{Key: "draft", Label: "draft a message for me", Recommended: true},
			{Key: "later", Label: "Later"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(item.Messages) != 1 || item.Messages[0].Author != "ada" || item.Messages[0].Text != "draft?" || len(item.Messages[0].Options) != 2 {
		t.Fatalf("ask thread = %+v", item.Messages)
	}
	answered, err := led.Answer(item.ID, "draft", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if answered.State != StateAnswered || answered.Answer == nil || answered.Answer.Value != "draft" {
		t.Fatalf("answer = %+v", answered.Answer)
	}
	if len(answered.Messages) != 2 || answered.Messages[1].Author != "human" || answered.Messages[1].Text != "draft: draft a message for me" {
		t.Fatalf("answer message = %+v", answered.Messages)
	}

	const draft = "Hello \"there\"\nplease send this draft"
	opts := []Option{
		{Key: "send", Label: "send", Recommended: true},
		{Key: "edit", Label: "edit"},
		{Key: "don't send", Label: "don't send"},
	}
	replied, err := led.Reply(item.ID, "", draft, opts, false)
	if err != nil {
		t.Fatal(err)
	}
	if replied.State != StateOpen || !replied.Unread || replied.FreeText {
		t.Fatalf("reply state %s unread %v free %v", replied.State, replied.Unread, replied.FreeText)
	}
	if replied.Question != "draft?" || replied.Answer == nil || replied.Answer.Value != "draft" {
		t.Fatalf("original ask was dropped: %+v", replied)
	}
	if len(replied.Options) != 3 || replied.Options[0].Key != "send" || !replied.Options[0].Recommended || replied.Options[2].Key != "don't send" {
		t.Fatalf("options = %+v", replied.Options)
	}
	last := replied.Messages[len(replied.Messages)-1]
	if last.Author != "ada" || last.Text != draft || len(last.Options) != 3 {
		t.Fatalf("draft message = %+v", last)
	}
	if replied.Messages[0].Options[0].Key != "draft" {
		t.Fatalf("original options = %+v", replied.Messages[0].Options)
	}
	if _, err := led.Answer(item.ID, "nope", ViaCLI); err == nil {
		t.Fatal("free text was accepted")
	}
	sent, err := led.Answer(item.ID, "send", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if sent.State != StateAnswered || sent.Answer == nil || sent.Answer.Value != "send" {
		t.Fatalf("send answer = %+v", sent.Answer)
	}
	human := sent.Messages[len(sent.Messages)-1]
	if human.Author != "human" || human.Text != "send: send" {
		t.Fatalf("send message = %+v", human)
	}

	note, err := led.Reply(item.ID, "", "shipped", nil, false)
	if err != nil {
		t.Fatal(err)
	}
	if note.State != StateOpen || !note.Unread || len(note.Options) != 0 {
		t.Fatalf("notification = state %s unread %v options %+v", note.State, note.Unread, note.Options)
	}
	if note.Messages[len(note.Messages)-1].Text != "shipped" || len(note.Messages[len(note.Messages)-1].Options) != 0 {
		t.Fatalf("notification message = %+v", note.Messages[len(note.Messages)-1])
	}
	done, err := led.Done(item.ID, "sent")
	if err != nil {
		t.Fatal(err)
	}
	if done.State != StateDone || done.Outcome != "sent" {
		t.Fatalf("done = %+v", done)
	}
	edited, err := led.Compose(item.ID, "please change the subject", ViaCLI, "")
	if err != nil {
		t.Fatal(err)
	}
	if edited.State != StateDone || edited.Outcome != "sent" {
		t.Fatalf("compose changed the done thread: %+v", edited)
	}
	if edited.Messages[len(edited.Messages)-1].Author != "human" || edited.Messages[len(edited.Messages)-1].Text != "please change the subject" {
		t.Fatalf("compose message = %+v", edited.Messages[len(edited.Messages)-1])
	}
	back, err := led.Reply(item.ID, "", "more", nil, false)
	if err != nil {
		t.Fatal(err)
	}
	if back.State != StateOpen || !back.Unread || back.Outcome != "sent" {
		t.Fatalf("reopened = state %s unread %v outcome %q", back.State, back.Unread, back.Outcome)
	}

	openItem, err := led.Create(&Item{
		Question: "or else",
		Owner:    "ada",
		Kind:     KindRelay,
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "A", Recommended: true}, {Key: "b", Label: "B"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(openItem.ID, "a", ViaCLI); err != nil {
		t.Fatal(err)
	}
	raw := "  say \"hi\"  "
	freed, err := led.Reply(openItem.ID, "", "type it", []Option{{Key: "a", Label: "A", Recommended: true}, {Key: "b", Label: "B"}}, true)
	if err != nil {
		t.Fatal(err)
	}
	if !freed.FreeText || freed.State != StateOpen {
		t.Fatalf("free reply = %+v", freed)
	}
	typed, err := led.Answer(openItem.ID, raw, ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if typed.Answer == nil || typed.Answer.Value != raw || typed.Messages[len(typed.Messages)-1].Text != raw {
		t.Fatalf("free answer = %+v", typed.Answer)
	}

	withdrawn, err := led.Create(&Item{Question: "stop", Owner: "ada", Kind: KindRelay})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Withdraw(withdrawn.ID, "no"); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(withdrawn.ID, "", "later", nil, false); err == nil {
		t.Fatal("reply on a withdrawn thread was accepted")
	}
	if _, err := led.Compose(withdrawn.ID, "later", ViaCLI, ""); err == nil {
		t.Fatal("compose on a withdrawn thread was accepted")
	}
	if _, err := led.Compose(item.ID, "   ", ViaCLI, ""); err == nil {
		t.Fatal("empty compose was accepted")
	}
}

func TestComposeAnswersAnOpenQuestion(t *testing.T) {
	led, _ := openTemp(t)
	item, err := led.Create(&Item{
		Question: "root ask",
		Owner:    "ada",
		Kind:     KindRelay,
		Options:  []Option{{Key: "a", Label: "Alpha"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	text := "say \"hi\"\nnext"
	got, err := led.Compose(item.ID, text, ViaCLI, "")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateAnswered || got.Answer == nil || got.Answer.Value != text || got.Answer.AnsweredBy != "human" {
		t.Fatalf("answer = %+v state %s", got.Answer, got.State)
	}
	if len(got.Messages) != 2 || got.Messages[0].Text != "root ask" || got.Messages[1].Text != text || got.Messages[1].Author != "human" {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if len(got.Options) != 1 || got.Options[0].Key != "a" {
		t.Fatalf("options = %+v", got.Options)
	}
	later, err := led.Compose(item.ID, "later", ViaCLI, "")
	if err != nil {
		t.Fatal(err)
	}
	if later.State != StateAnswered || later.Answer == nil || later.Answer.Value != text {
		t.Fatalf("second compose changed the answer: %+v state %s", later.Answer, later.State)
	}
	if later.Messages[len(later.Messages)-1].Text != "later" {
		t.Fatalf("second message = %+v", later.Messages)
	}

	note, err := led.Create(&Item{
		Question: "progress",
		Owner:    "ada",
		Kind:     KindRelay,
		State:    StateOpen,
	})
	if err != nil {
		t.Fatal(err)
	}
	noted, err := led.Compose(note.ID, "ping", ViaCLI, "")
	if err != nil {
		t.Fatal(err)
	}
	if noted.State != StateOpen || noted.Answer != nil || noted.Messages[len(noted.Messages)-1].Text != "ping" {
		t.Fatalf("notification = state %s answer %+v messages %+v", noted.State, noted.Answer, noted.Messages)
	}
	if _, err := led.Compose(item.ID, "   ", ViaCLI, ""); err == nil {
		t.Fatal("empty compose was accepted")
	}
	withdrawn, err := led.Withdraw(note.ID, "stop")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Compose(withdrawn.ID, "nope", ViaCLI, ""); err == nil {
		t.Fatal("compose on a withdrawn thread was accepted")
	}
}

func TestLegacyAnswerKeepsThePreviousAnswer(t *testing.T) {
	dir := t.TempDir()
	created := time.Date(2026, 11, 6, 1, 0, 0, 0, time.UTC)
	answeredAt := time.Date(2026, 11, 6, 2, 0, 0, 0, time.UTC)
	item := Item{
		ID:       "D-001",
		Question: "root ask",
		Options: []Option{
			{Key: "send", Label: "Send", Recommended: true},
			{Key: "edit", Label: "Edit"},
		},
		FreeText:   true,
		Owner:      "ada",
		Kind:       KindRelay,
		State:      StateAnswered,
		CreatedAt:  created,
		AnsweredAt: answeredAt,
		Answer:     &Answer{Value: "human note", AnsweredBy: "human"},
	}
	body, err := json.MarshalIndent(store{Next: 2, Items: []Item{item}}, "", "  ")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, ledgerName), append(body, '\n'), 0o600); err != nil {
		t.Fatal(err)
	}
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	led.now = func() time.Time { return answeredAt.Add(time.Hour) }
	got, err := led.Answer("D-001", "send", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Messages) != 3 {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if got.Messages[0].Text != "root ask" || got.Messages[1].Author != "human" || got.Messages[1].Text != "human note" || got.Messages[2].Text != "send: Send" {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if len(got.Messages[0].Options) != 2 || got.Messages[0].Options[0].Key != "send" {
		t.Fatalf("root options = %+v", got.Messages[0].Options)
	}
}

func TestStartThreadStoresTheHumanNote(t *testing.T) {
	led, _ := openTemp(t)
	const note = "hello\nthere"
	got, err := led.StartThread("alpha", "pane-a", note, ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if got.Owner != "alpha" || got.NotifyPane != "pane-a" || got.Question != note || got.State != StateOpen {
		t.Fatalf("thread = %+v", got)
	}
	if len(got.Messages) != 1 || got.Messages[0].Author != "human" || got.Messages[0].Text != note {
		t.Fatalf("message = %+v", got.Messages)
	}
	if _, err := led.StartThread(" ", "pane-a", note, ViaCLI); err == nil {
		t.Fatal("empty owner was accepted")
	}
}

func TestForwardThreadRecordsTheSource(t *testing.T) {
	led, dir := openTemp(t)
	got, err := led.ForwardThread("D-123", "alpha", "pane-a", "fwd note", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if got.ForwardedFrom != "D-123" || got.Owner != "alpha" || got.Question != "fwd note" {
		t.Fatalf("thread = %+v", got)
	}
	stored, err := led.Get(got.ID)
	if err != nil {
		t.Fatal(err)
	}
	if stored.ForwardedFrom != "D-123" {
		t.Fatalf("stored = %+v", stored)
	}
	data, err := os.ReadFile(filepath.Join(dir, ledgerName)) //nolint:gosec // test ledger
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Contains(data, []byte(`"forwardedFrom": "D-123"`)) {
		t.Fatalf("ledger missing forwardedFrom\n%s", data)
	}
	plain, err := led.StartThread("alpha", "pane-a", "plain note", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if plain.ForwardedFrom != "" {
		t.Fatalf("plain thread = %+v", plain)
	}
	data, err = os.ReadFile(filepath.Join(dir, ledgerName)) //nolint:gosec // test ledger
	if err != nil {
		t.Fatal(err)
	}
	if bytes.Count(data, []byte(`"forwardedFrom"`)) != 1 {
		t.Fatalf("plain thread wrote forwardedFrom\n%s", data)
	}
}

func TestForwardNoteNamesTheSource(t *testing.T) {
	note := ForwardNote("D-124", "D-123", "human", "hello")
	if !strings.Contains(note, "Fwd D-123") || !strings.Contains(note, "D-124") || !strings.Contains(note, "hello") {
		t.Fatalf("note = %q", note)
	}
	if strings.Contains(DeliveryNote("D-124", "human", "hello"), "Fwd") {
		t.Fatal("plain note names a source")
	}
}

func TestReassignUpdatesOwnerPaneMachineAndAppendsEvent(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	got, err := led.Reassign(item.ID, Reassign{
		Owner: "bea", SetOwner: true,
		Pane: "p2", SetPane: true, Machine: "mac",
		By: "cara",
	})
	if err != nil {
		t.Fatal(err)
	}
	if got.Owner != "bea" || got.NotifyPane != "p2" || got.NotifyMachine != "mac" || got.NotifyClosed {
		t.Fatalf("reassign = owner %q pane %q machine %q closed %v", got.Owner, got.NotifyPane, got.NotifyMachine, got.NotifyClosed)
	}
	if got.Answer == nil || got.Answer.Value != "a" || got.State != StateAnswered {
		t.Fatalf("answer changed: %+v state %s", got.Answer, got.State)
	}
	if len(got.Messages) != 3 || got.Messages[0].Text != "ship?" || got.Messages[1].Text != "a" {
		t.Fatalf("history = %+v", got.Messages)
	}
	last := got.Messages[2]
	const want = "reassigned: owner ada → bea; notify pane p1 → p2 (by cara)"
	if last.Author != "system" || last.By != "cara" || last.Text != want {
		t.Fatalf("event = %+v, want %q", last, want)
	}
	stored, err := led.Get(item.ID)
	if err != nil || stored.Messages[2].Text != want || stored.NotifyMachine != "mac" {
		t.Fatalf("stored = %+v err %v", stored, err)
	}
}

func TestReassignOneFieldLeavesTheOther(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	ownerOnly, err := led.Reassign(item.ID, Reassign{Owner: "bea", SetOwner: true, By: "cara"})
	if err != nil {
		t.Fatal(err)
	}
	if ownerOnly.Owner != "bea" || ownerOnly.NotifyPane != "p1" || ownerOnly.NotifyMachine != "old" {
		t.Fatalf("owner only = owner %q pane %q machine %q", ownerOnly.Owner, ownerOnly.NotifyPane, ownerOnly.NotifyMachine)
	}
	if ownerOnly.Messages[len(ownerOnly.Messages)-1].Text != "reassigned: owner ada → bea (by cara)" {
		t.Fatalf("owner event = %q", ownerOnly.Messages[len(ownerOnly.Messages)-1].Text)
	}

	paneOnly, err := led.Reassign(item.ID, Reassign{Pane: "p9", SetPane: true, Machine: "mac"})
	if err != nil {
		t.Fatal(err)
	}
	if paneOnly.Owner != "bea" || paneOnly.NotifyPane != "p9" || paneOnly.NotifyMachine != "mac" {
		t.Fatalf("pane only = owner %q pane %q machine %q", paneOnly.Owner, paneOnly.NotifyPane, paneOnly.NotifyMachine)
	}
	if paneOnly.Messages[len(paneOnly.Messages)-1].Text != "reassigned: notify pane p1 → p9" {
		t.Fatalf("pane event = %q", paneOnly.Messages[len(paneOnly.Messages)-1].Text)
	}
}

func TestReassignBusPaneClearsTheMachine(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	got, err := led.Reassign(item.ID, Reassign{Pane: "agentbus:ops", SetPane: true, Machine: "mac"})
	if err != nil {
		t.Fatal(err)
	}
	if got.NotifyPane != "agentbus:ops" || got.NotifyMachine != "" || got.NotifyClosed {
		t.Fatalf("bus = pane %q machine %q closed %v", got.NotifyPane, got.NotifyMachine, got.NotifyClosed)
	}
	cleared, err := led.Reassign(item.ID, Reassign{Pane: "", SetPane: true, Machine: "mac"})
	if err != nil {
		t.Fatal(err)
	}
	if cleared.NotifyPane != "" || cleared.NotifyMachine != "" || !cleared.NotifyClosed {
		t.Fatalf("cleared = pane %q machine %q closed %v", cleared.NotifyPane, cleared.NotifyMachine, cleared.NotifyClosed)
	}
}

func TestReassignRefusesDoneAndWithdrawn(t *testing.T) {
	led, _ := openTemp(t)
	for _, state := range []string{StateDone, StateWithdrawn} {
		item := reassignSeed(t, led)
		if _, err := led.finish(item.ID, state, "stop"); err != nil {
			t.Fatal(err)
		}
		if _, err := led.Reassign(item.ID, Reassign{Owner: "bea", SetOwner: true}); err == nil || !strings.Contains(err.Error(), state) {
			t.Fatalf("%s reassign err = %v", state, err)
		}
		stored, err := led.Get(item.ID)
		if err != nil || stored.Owner != "ada" {
			t.Fatalf("stored owner = %q err %v", stored.Owner, err)
		}
	}
}

func TestReassignRequiresAField(t *testing.T) {
	led, _ := openTemp(t)
	item := reassignSeed(t, led)
	if _, err := led.Reassign(item.ID, Reassign{}); err == nil {
		t.Fatal("reassign with nothing succeeded")
	}
	if _, err := led.Reassign(item.ID, Reassign{SetOwner: true}); err == nil {
		t.Fatal("empty owner succeeded")
	}
}

func reassignSeed(t *testing.T, led *Ledger) Item {
	t.Helper()
	item, err := led.Create(&Item{
		Question:      "ship?",
		Kind:          KindRelay,
		Owner:         "ada",
		NotifyPane:    "p1",
		NotifyMachine: "old",
		State:         StateAnswered,
		Answer:        &Answer{Value: "a", AnsweredBy: "human", Via: ViaCLI},
		Messages: []Message{
			{Author: "ada", Text: "ship?"},
			{Author: "human", Text: "a", Via: ViaCLI},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	return item
}

func openTemp(t *testing.T) (*Ledger, string) {
	t.Helper()
	dir := t.TempDir()
	led, err := Open(dir)
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	return led, dir
}

func itemForMark(mark string) *Item {
	created := time.Date(2026, 11, 5, 1, 0, 0, 0, time.UTC)
	item := &Item{
		Question: "question-" + mark,
		Options: []Option{
			{Key: "a", Label: "alpha-" + mark, Recommended: true},
			{Key: "b", Label: "beta-" + mark, Recommended: false},
		},
		FreeText:  mark == "full" || mark == "one",
		Owner:     "owner-" + mark,
		Links:     []string{"https://example.com/" + mark},
		Kind:      KindRelay,
		State:     StateOpen,
		CreatedAt: created,
		Unread:    mark == "full" || mark == "one",
	}
	if mark == "plain" {
		return item
	}
	deadline := time.Date(2026, 11, 6, 15, 4, 5, 0, time.UTC)
	item.Kind = KindTypeIn
	item.State = StateAnswered
	item.Deadline = &deadline
	item.AnsweredAt = time.Date(2026, 11, 5, 2, 0, 0, 0, time.UTC)
	item.DoneAt = time.Date(2026, 11, 5, 3, 0, 0, 0, time.UTC)
	item.WithdrawnAt = time.Date(2026, 11, 5, 4, 0, 0, 0, time.UTC)
	item.Answer = &Answer{Value: "typed-" + mark, AnsweredBy: "human"}
	item.Reason = "reason-" + mark
	item.Outcome = "outcome-" + mark
	item.NotifyPane = "pane-" + mark
	item.TypeText = "exact-" + mark
	item.Where = "where-" + mark
	item.ForwardedFrom = "D-0" + mark
	return item
}

func assertItemEqual(t *testing.T, got, want Item) {
	t.Helper()
	if got.ID != want.ID || got.Question != want.Question || got.FreeText != want.FreeText ||
		got.Owner != want.Owner || got.Kind != want.Kind || got.State != want.State ||
		got.Unread != want.Unread || got.NotifyPane != want.NotifyPane ||
		got.TypeText != want.TypeText || got.Where != want.Where ||
		got.Reason != want.Reason || got.Outcome != want.Outcome ||
		got.ForwardedFrom != want.ForwardedFrom {
		t.Fatalf("item fields\ngot  %+v\nwant %+v", got, want)
	}
	if !got.CreatedAt.Equal(want.CreatedAt) || !got.AnsweredAt.Equal(want.AnsweredAt) ||
		!got.DoneAt.Equal(want.DoneAt) || !got.WithdrawnAt.Equal(want.WithdrawnAt) {
		t.Fatalf("timestamps\ngot  created=%s answered=%s done=%s withdrawn=%s\nwant created=%s answered=%s done=%s withdrawn=%s",
			got.CreatedAt, got.AnsweredAt, got.DoneAt, got.WithdrawnAt,
			want.CreatedAt, want.AnsweredAt, want.DoneAt, want.WithdrawnAt)
	}
	if !sameTimePtr(got.Deadline, want.Deadline) {
		t.Fatalf("deadline got %v want %v", got.Deadline, want.Deadline)
	}
	if !sameAnswer(got.Answer, want.Answer) {
		t.Fatalf("answer got %+v want %+v", got.Answer, want.Answer)
	}
	if len(got.Options) != len(want.Options) || len(got.Links) != len(want.Links) {
		t.Fatalf("options/links\ngot  %+v %v\nwant %+v %v", got.Options, got.Links, want.Options, want.Links)
	}
	for i := range want.Options {
		if got.Options[i] != want.Options[i] {
			t.Fatalf("option %d got %+v want %+v", i, got.Options[i], want.Options[i])
		}
	}
	for i := range want.Links {
		if got.Links[i] != want.Links[i] {
			t.Fatalf("link %d got %q want %q", i, got.Links[i], want.Links[i])
		}
	}
}

func sameTimePtr(got, want *time.Time) bool {
	if got == nil || want == nil {
		return got == nil && want == nil
	}
	return got.Equal(*want)
}

func sameAnswer(got, want *Answer) bool {
	if got == nil || want == nil {
		return got == nil && want == nil
	}
	return *got == *want
}

func TestSetBusMessageStoresTheMessageID(t *testing.T) {
	led, _ := openTemp(t)
	item, err := led.StartThread("coord/primary", "agentbus:coord/primary", "hi", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.SetBusMessage(item.ID, "m-7f3a")
	if err != nil {
		t.Fatal(err)
	}
	stored, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.BusMessage != "m-7f3a" || stored.BusMessage != "m-7f3a" {
		t.Fatalf("bus message = %q, stored %q", got.BusMessage, stored.BusMessage)
	}
	if _, err := led.SetBusMessage("D-999", "m-1"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("missing id err = %v", err)
	}
}

func TestDeleteRemovesTheItemAndDoesNotReuseItsID(t *testing.T) {
	led, _ := openTemp(t)
	first, err := led.StartThread("alpha", "pane-a", "one", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if err := led.Delete(first.ID); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Get(first.ID); !errors.Is(err, ErrNotFound) {
		t.Fatalf("deleted item err = %v", err)
	}
	second, err := led.StartThread("alpha", "pane-a", "two", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if second.ID == first.ID {
		t.Fatalf("id %s was reused", second.ID)
	}
	if err := led.Delete(first.ID); !errors.Is(err, ErrNotFound) {
		t.Fatalf("second delete err = %v", err)
	}
}

func TestAnswerStoresTheOptionKeyAndLabel(t *testing.T) {
	led, _ := openTemp(t)
	item, err := led.Create(&Item{
		Question: "pick",
		Kind:     KindRelay,
		Owner:    "ada",
		Options:  []Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Answer(item.ID, "a", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Key != "a" || got.Answer.Label != "Alpha" || got.Answer.Value != "a" {
		t.Fatalf("answer = %+v", got.Answer)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Text != "a: Alpha" || last.OptionKey != "a" || last.OptionLabel != "Alpha" {
		t.Fatalf("answer message = %+v", last)
	}
	if text := AnswerText(&got); text != "a: Alpha" {
		t.Fatalf("AnswerText = %q", text)
	}
	replied, err := led.Reply(item.ID, "", "again?", []Option{{Key: "a", Label: "Changed"}}, false)
	if err != nil {
		t.Fatal(err)
	}
	if text := AnswerText(&replied); text != "a: Alpha" {
		t.Fatalf("AnswerText after reply = %q", text)
	}
	if label := AnswerLabel(&replied); label != "Alpha" {
		t.Fatalf("AnswerLabel after reply = %q", label)
	}
}

func TestFreeTextAnswerHasNoKeyOrLabel(t *testing.T) {
	led, _ := openTemp(t)
	item, err := led.Create(&Item{
		Question: "say",
		Kind:     KindRelay,
		Owner:    "ada",
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "Alpha"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Answer(item.ID, "something else", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Key != "" || got.Answer.Label != "" || got.Answer.Value != "something else" {
		t.Fatalf("answer = %+v", got.Answer)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Text != "something else" || last.OptionKey != "" || last.OptionLabel != "" {
		t.Fatalf("answer message = %+v", last)
	}
	if text := AnswerText(&got); text != "something else" {
		t.Fatalf("AnswerText = %q", text)
	}
}

func TestLegacyOptionAnswerRendersTheLabel(t *testing.T) {
	asked := time.Date(2026, 11, 6, 1, 0, 0, 0, time.UTC)
	answeredAt := asked.Add(time.Hour)
	for _, tc := range []struct{ value, want string }{
		{"b", "b: Beta"},
		{"z", "z: Zed"},
		{"c", "c"},
	} {
		// The ledger predates the key and label fields. A later reply
		// replaced the item options, so the asking message wins for b.
		raw := `{"next": 2, "items": [{
  "id": "D-001", "question": "pick", "owner": "ada", "kind": "relay",
  "state": "answered", "freeText": false, "unread": false,
  "createdAt": "` + asked.Format(time.RFC3339) + `",
  "answeredAt": "` + answeredAt.Format(time.RFC3339) + `",
  "options": [{"key": "b", "label": "Later", "recommended": false}, {"key": "z", "label": "Zed", "recommended": false}],
  "messages": [
    {"author": "ada", "time": "` + asked.Format(time.RFC3339) + `", "text": "pick",
     "options": [{"key": "a", "label": "Alpha", "recommended": false}, {"key": "b", "label": "Beta", "recommended": false}]},
    {"author": "human", "time": "` + answeredAt.Format(time.RFC3339) + `", "text": "` + tc.value + `"},
    {"author": "ada", "time": "` + answeredAt.Add(time.Hour).Format(time.RFC3339) + `", "text": "again?",
     "options": [{"key": "b", "label": "Later", "recommended": false}, {"key": "z", "label": "Zed", "recommended": false}]}
  ],
  "answer": {"value": "` + tc.value + `", "answeredBy": "human"}
}]}`
		dir := t.TempDir()
		if err := os.WriteFile(filepath.Join(dir, ledgerName), []byte(raw), 0o600); err != nil {
			t.Fatal(err)
		}
		led, err := Open(dir)
		if err != nil {
			t.Fatal(err)
		}
		got, err := led.Get("D-001")
		if err != nil {
			t.Fatal(err)
		}
		if text := AnswerText(&got); text != tc.want {
			t.Fatalf("AnswerText(%q) = %q, want %q", tc.value, text, tc.want)
		}
	}
}

func TestAnswerRecordsVia(t *testing.T) {
	led, _ := openTemp(t)
	ask := func() string {
		t.Helper()
		it, err := led.Create(&Item{
			Question: "pick",
			Kind:     KindRelay,
			Owner:    "ada",
			FreeText: true,
			Options:  []Option{{Key: "a", Label: "A", Recommended: true}},
		})
		if err != nil {
			t.Fatal(err)
		}
		return it.ID
	}
	for _, via := range []string{ViaUI, ViaCLI} {
		got, err := led.Answer(ask(), "a", via)
		if err != nil {
			t.Fatal(err)
		}
		if got.Answer == nil || got.Answer.Via != via {
			t.Fatalf("answer via %s = %+v", via, got.Answer)
		}
		composed, err := led.Compose(ask(), "typed", via, "")
		if err != nil {
			t.Fatal(err)
		}
		if composed.Answer == nil || composed.Answer.Via != via {
			t.Fatalf("compose via %s = %+v", via, composed.Answer)
		}
	}
	for _, via := range []string{"", "bogus", "UI"} {
		id := ask()
		if _, err := led.Answer(id, "a", via); err == nil {
			t.Fatalf("answer via %q was accepted", via)
		}
		if _, err := led.Compose(id, "typed", via, ""); err == nil {
			t.Fatalf("compose via %q was accepted", via)
		}
	}
}

func TestOldAnswerWithoutViaIsNotUI(t *testing.T) {
	dir := t.TempDir()
	raw := `{"next": 2, "items": [{"id": "D-001", "question": "q", "owner": "ada", "kind": "relay",
  "state": "answered", "answer": {"value": "a", "answeredBy": "human"}}]}`
	if err := os.WriteFile(filepath.Join(dir, ledgerName), []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Get("D-001")
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.Via == ViaUI {
		t.Fatalf("old answer = %+v", got.Answer)
	}
	out, err := json.Marshal(got.Answer)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(out), `"via"`) {
		t.Fatalf("old answer JSON = %s", out)
	}
}

func TestThreadMessagesRecordViaAndBy(t *testing.T) {
	led, _ := openTemp(t)
	it, err := led.Create(&Item{
		Question: "post it?",
		Kind:     KindRelay,
		Owner:    "slackwatch",
		Options:  []Option{{Key: "post", Label: "Post", Recommended: true}, {Key: "skip", Label: "Skip"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(it.ID, "post", ViaUI); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(it.ID, "ops/agent", "posted; anything else?", nil, false); err != nil {
		t.Fatal(err)
	}
	got, err := led.Compose(it.ID, "also tag the channel", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	msgs := got.Messages[1:]
	if len(msgs) != 3 {
		t.Fatalf("messages = %+v", got.Messages)
	}
	if m := msgs[0]; m.Author != "human" || m.OptionKey != "post" || m.Via != ViaUI || m.By != "" {
		t.Fatalf("pick = %+v", m)
	}
	if m := msgs[1]; m.Author != "slackwatch" || m.By != "ops/agent" || m.Via != "" {
		t.Fatalf("reply = %+v", m)
	}
	if m := msgs[2]; m.Author != "human" || m.Text != "also tag the channel" || m.Via != ViaUI || m.By != "" {
		t.Fatalf("follow-up = %+v", m)
	}
	cli, err := led.Compose(it.ID, "from a pane", ViaCLI, "ops/agent")
	if err != nil {
		t.Fatal(err)
	}
	if m := cli.Messages[len(cli.Messages)-1]; m.Via != ViaCLI || m.By != "ops/agent" {
		t.Fatalf("cli compose = %+v", m)
	}
	started, err := led.StartThread("ada", "pane-a", "hi", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	if m := started.Messages[0]; m.Via != ViaCLI {
		t.Fatalf("start = %+v", m)
	}
	if _, err := led.StartThread("ada", "pane-a", "hi", "bogus"); err == nil {
		t.Fatal("start via bogus was accepted")
	}
}

func TestOldMessagesHaveNoViaOrBy(t *testing.T) {
	dir := t.TempDir()
	raw := `{"next": 2, "items": [{"id": "D-001", "question": "q", "owner": "ada", "kind": "relay",
  "state": "answered", "answer": {"value": "a", "answeredBy": "human"},
  "messages": [{"author": "ada", "time": "2026-01-01T00:00:00Z", "text": "q"},
    {"author": "human", "time": "2026-01-01T00:01:00Z", "text": "a: A", "optionKey": "a"}]}]}`
	if err := os.WriteFile(filepath.Join(dir, ledgerName), []byte(raw), 0o600); err != nil {
		t.Fatal(err)
	}
	led, err := Open(dir)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Get("D-001")
	if err != nil {
		t.Fatal(err)
	}
	for _, m := range got.Messages {
		if m.Via != "" || m.By != "" {
			t.Fatalf("old message = %+v", m)
		}
	}
	out, err := json.Marshal(got.Messages)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(out), `"via"`) || strings.Contains(string(out), `"by"`) {
		t.Fatalf("old messages JSON = %s", out)
	}
}

func TestMessageSender(t *testing.T) {
	cases := []struct {
		msg  Message
		want string
	}{
		{Message{Author: "slackwatch", By: "ops/agent"}, "ops/agent"},
		{Message{Author: "ada", By: "ada"}, "ada"},
		{Message{Author: "ada"}, "ada"},
	}
	for _, c := range cases {
		if got := c.msg.Sender(); got != c.want {
			t.Fatalf("Sender(%+v) = %q, want %q", c.msg, got, c.want)
		}
	}
}
