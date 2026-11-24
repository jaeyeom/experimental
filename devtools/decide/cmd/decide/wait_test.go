package main

import (
	"bytes"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// waitTestEvery keeps the poll fast so a test that waits returns quickly.
const waitTestEvery = "5ms"

func TestWaitReturnsAtOnceWhenNotOpen(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "already answered")
	if _, stderr, code := runCLI("answer", id, "a"); code != 0 {
		t.Fatalf("answer = %d\n%s", code, stderr)
	}
	stdout, stderr, code := runCLI("wait", "--timeout", "1s", id)
	if code != 0 || !strings.Contains(stdout, "id: "+id) || !strings.Contains(stdout, "state: answered") {
		t.Fatalf("wait = %d\nstdout:\n%s\nstderr:\n%s", code, stdout, stderr)
	}
}

func TestWaitReturnsOnAnswer(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "answer me later")
	errc := changeLater(func() (string, int) {
		_, stderr, code := runCLI("answer", id, "a")
		return stderr, code
	})
	stdout, stderr, code := runCLI("wait", "--json", "--every", waitTestEvery, "--timeout", "10s", id)
	if msg := <-errc; msg != "" {
		t.Fatal(msg)
	}
	if code != 0 {
		t.Fatalf("wait = %d\n%s", code, stderr)
	}
	got := decodeItem(t, stdout)
	if got.ID != id || got.State != decide.StateAnswered || got.Answer == nil || got.Answer.Value != "a" {
		t.Fatalf("wait --json item = %+v", got)
	}
}

func TestWaitReturnsOnNewMessageWhileOpen(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "reply to me")
	before := len(showJSON(t, id).Messages)
	errc := changeLater(func() (string, int) {
		_, stderr, code := runCLI("reply", id, "One more detail")
		return stderr, code
	})
	stdout, stderr, code := runCLI("wait", "--json", "--every", waitTestEvery, "--timeout", "10s", id)
	if msg := <-errc; msg != "" {
		t.Fatal(msg)
	}
	if code != 0 {
		t.Fatalf("wait = %d\n%s", code, stderr)
	}
	got := decodeItem(t, stdout)
	if got.State != decide.StateOpen || len(got.Messages) != before+1 {
		t.Fatalf("state = %q, messages = %d, want open with %d", got.State, len(got.Messages), before+1)
	}
}

func TestWaitTimesOut(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "nobody answers")
	start := time.Now()
	stdout, stderr, code := runCLI("wait", "--every", waitTestEvery, "--timeout", "50ms", id)
	if code != waitTimeoutExit {
		t.Fatalf("wait = %d, want %d\n%s", code, waitTimeoutExit, stderr)
	}
	if stdout != "" || !strings.Contains(stderr, "timed out") {
		t.Fatalf("stdout = %q, stderr = %q", stdout, stderr)
	}
	if elapsed := time.Since(start); elapsed < 50*time.Millisecond {
		t.Fatalf("returned after %v, before the timeout", elapsed)
	}
}

func TestWaitUsageAndUnknownItem(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	id := askRelay(t, nil, "exists")
	for _, tc := range []struct {
		args []string
		code int
		want string
	}{
		{[]string{"wait"}, 2, "id is required"},
		{[]string{"wait", "--every", "0s", id}, 2, "--every"},
		{[]string{"wait", "--timeout", "-1s", id}, 2, "--timeout"},
		{[]string{"wait", "D-999"}, 1, "D-999"},
	} {
		_, stderr, code := runCLI(tc.args...)
		if code != tc.code || !strings.Contains(stderr, tc.want) {
			t.Errorf("%q = %d, stderr %q; want %d mentioning %q", tc.args, code, stderr, tc.code, tc.want)
		}
	}
}

// changeLater runs change after the wait has started polling and reports a
// failure as a message on the returned channel ("" means success).
func changeLater(change func() (stderr string, code int)) <-chan string {
	errc := make(chan string, 1)
	go func() {
		time.Sleep(50 * time.Millisecond)
		if stderr, code := change(); code != 0 {
			errc <- "change failed: " + stderr
			return
		}
		errc <- ""
	}()
	return errc
}

func TestWaitRunsOnTheHub(t *testing.T) {
	fake, _ := setupHub(t, "")
	var stderr bytes.Buffer
	if code := run([]string{"--remote", "me@hub", "wait", "--json", "--timeout", "1h", "D-1"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("wait = %d, stderr %q", code, stderr.String())
	}
	if got := fake.Lines("decide.args"); !slices.Equal(got, []string{"wait", "--json", "--timeout", "1h", "D-1"}) {
		t.Fatalf("hub args = %q", got)
	}
}
