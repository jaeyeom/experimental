package tui

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// gateHub is a hub whose List waits for release and then fails with
// err when it is set.
type gateHub struct {
	*fakeHub
	release     chan struct{}
	finished    chan struct{}
	releaseOnce sync.Once
	finishOnce  sync.Once
	mu          sync.Mutex
	err         error
}

func (g *gateHub) List() ([]decide.Item, error) {
	if g.release != nil {
		<-g.release
	}
	defer g.finishOnce.Do(func() { close(g.finished) })
	g.mu.Lock()
	err := g.err
	g.mu.Unlock()
	if err != nil {
		return nil, err
	}
	return g.fakeHub.List()
}

func (g *gateHub) unblock() {
	g.releaseOnce.Do(func() { close(g.release) })
}

type lockedBuffer struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (b *lockedBuffer) Write(p []byte) (int, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	n, err := b.buf.Write(p)
	if err != nil {
		return n, fmt.Errorf("buffer write: %w", err)
	}
	return n, nil
}

func (b *lockedBuffer) String() string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.buf.String()
}

func waitOutput(t *testing.T, out *lockedBuffer, needle string) {
	t.Helper()
	deadline := time.Now().Add(5 * time.Second)
	for !strings.Contains(out.String(), needle) {
		if time.Now().After(deadline) {
			t.Fatalf("output never showed %q:\n%s", needle, out.String())
		}
		time.Sleep(10 * time.Millisecond)
	}
}

func waitRun(t *testing.T, done <-chan error) {
	t.Helper()
	select {
	case err := <-done:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("inbox did not quit")
	}
}

func gatedHub(t *testing.T, question string, state string) *gateHub {
	t.Helper()
	hubDir := t.TempDir()
	mustCreate(t, openLedger(t, hubDir), &decide.Item{
		Question: question, Owner: "ada", Kind: decide.KindRelay, State: state,
	})
	return &gateHub{
		fakeHub:  &fakeHub{hub: &LocalBackend{Dir: hubDir}},
		release:  make(chan struct{}),
		finished: make(chan struct{}),
	}
}

func TestRunDrawsLoadingBeforeFirstList(t *testing.T) {
	hub := gatedHub(t, "ship the startup fix?", decide.StateOpen)
	r, w := io.Pipe()
	out := &lockedBuffer{}
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{Dir: t.TempDir(), In: r, Out: out, Interval: time.Hour, Backend: hub})
	}()
	waitOutput(t, out, "Loading…")
	if strings.Contains(out.String(), "ship the startup fix?") {
		t.Fatalf("items drew before List returned:\n%s", out.String())
	}
	hub.unblock()
	waitOutput(t, out, "ship the startup fix?")
	if _, err := w.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	waitRun(t, done)
	_ = w.Close()
}

func TestRunQuitsWhileLoading(t *testing.T) {
	hub := gatedHub(t, "never shown", decide.StateOpen)
	defer hub.unblock()
	r, w := io.Pipe()
	defer w.Close()
	out := &lockedBuffer{}
	done := make(chan error, 1)
	go func() {
		done <- Run(Options{Dir: t.TempDir(), In: r, Out: out, Interval: time.Hour, Backend: hub})
	}()
	waitOutput(t, out, "Loading…")
	if _, err := w.Write([]byte{'q'}); err != nil {
		t.Fatal(err)
	}
	waitRun(t, done)
	hub.unblock()
	<-hub.finished
}

func TestFirstListErrorShowsAndRetries(t *testing.T) {
	hub := gatedHub(t, "after the retry", decide.StateOpen)
	hub.release = nil
	hub.err = errors.New("ledger busy")
	m, err := prepare(Options{Dir: t.TempDir(), In: strings.NewReader(""), Out: io.Discard, Backend: hub})
	if err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 100, 20
	m.interval = time.Millisecond

	_, cmd := m.Update(m.listCmd()())
	if m.runErr != nil || !strings.Contains(m.View(), "ledger busy") {
		t.Fatalf("runErr %v, view:\n%s", m.runErr, m.View())
	}
	if _, ok := cmd().(reloadMsg); !ok {
		t.Fatal("a failed first list did not schedule the next tick")
	}

	hub.mu.Lock()
	hub.err = nil
	hub.mu.Unlock()
	_, cmd = m.Update(reloadMsg{})
	m.Update(cmd())
	view := m.View()
	if !strings.Contains(view, "after the retry") || strings.Contains(view, "ledger busy") || strings.Contains(view, "Loading…") {
		t.Fatalf("after the retry:\n%s", view)
	}
}

func TestFirstListLoadsDoneItemsForSearch(t *testing.T) {
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{Question: "open apple", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen})
	mustCreate(t, led, &decide.Item{Question: "done apple", Owner: "ada", Kind: decide.KindRelay, State: decide.StateDone})
	m, err := prepare(Options{Dir: dir, In: strings.NewReader(""), Out: io.Discard})
	if err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 120, 24
	if len(m.allItems) != 0 || !strings.Contains(m.View(), "Loading…") {
		t.Fatalf("prepare loaded the list:\n%s", m.View())
	}
	m.Update(m.listCmd()())
	if m.cursor != 0 || len(m.items) != 1 || m.items[0].Question != "open apple" {
		t.Fatalf("cursor %d items %+v", m.cursor, m.items)
	}
	press(t, m, "/apple")
	assertFolderCount(t, m, folderIndex(t, "Done"), "1/1", false)
}

func TestFolderSwitchWhileLoadingWaitsForTheFirstList(t *testing.T) {
	hub := gatedHub(t, "done pear", decide.StateDone)
	hub.release = nil
	m, err := prepare(Options{Dir: t.TempDir(), In: strings.NewReader(""), Out: io.Discard, Backend: hub})
	if err != nil {
		t.Fatal(err)
	}
	press(t, m, "gd")
	if m.folder != folderIndex(t, "Done") || len(hub.ops) != 0 {
		t.Fatalf("folder %d ops %v", m.folder, hub.ops)
	}
	m.Update(m.listCmd()())
	if len(m.items) != 1 || m.items[0].Question != "done pear" {
		t.Fatalf("items %+v", m.items)
	}
}
