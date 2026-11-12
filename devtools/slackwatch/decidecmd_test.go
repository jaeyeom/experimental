package slackwatch

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// fakeDecideScript writes a decide stand-in that logs its arguments to
// log and prints a JSON item for ask, show, and done.
func fakeDecideScript(t *testing.T) (script, log string) {
	t.Helper()
	dir := t.TempDir()
	log = filepath.Join(dir, "calls")
	script = filepath.Join(dir, "fake-decide")
	body := `#!/bin/sh
echo "$*" >> '` + log + `'
while [ "$1" = --remote ]; do shift 2; done
case "$1" in
ask) echo '{"id":"D-9","state":"open"}' ;;
show) echo '{"id":"D-9","state":"open"}' ;;
done) echo '{"id":"D-9","state":"done"}' ;;
esac
`
	if err := os.WriteFile(script, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake decide must be executable.
		t.Fatal(err)
	}
	return script, log
}

func TestDecideCommandIsUsed(t *testing.T) {
	script, log := fakeDecideScript(t)
	tests := map[string]struct {
		value  string
		prefix string
	}{
		"path": {`"` + script + `"`, ""},
		"argv": {`["` + script + `", "--remote", "box"]`, "--remote box "},
	}
	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			if err := os.Remove(log); err != nil && !errors.Is(err, os.ErrNotExist) {
				t.Fatal(err)
			}
			cfg, err := LoadConfig(writeFile(t, "config.json", `{"decideCommand": `+tt.value+`}`, 0o600))
			if err != nil {
				t.Fatal(err)
			}
			if path, err := cfg.ResolveDecide(); err != nil || path != script {
				t.Errorf("ResolveDecide = %q, %v, want %q", path, err, script)
			}
			d := DecideCLI{Run: ExecDecide(cfg.DecideArgv())}
			ctx := context.Background()
			if id, err := d.Ask(ctx, "slackwatch", "Post?"); err != nil || id != "D-9" {
				t.Fatalf("Ask = %q, %v", id, err)
			}
			if _, err := d.Show(ctx, "D-9"); err != nil {
				t.Fatal(err)
			}
			if err := d.Done(ctx, "D-9", "posted"); err != nil {
				t.Fatal(err)
			}
			data, err := os.ReadFile(log)
			if err != nil {
				t.Fatal(err)
			}
			lines := strings.Split(strings.TrimSpace(string(data)), "\n")
			want := []string{"ask ", "show --json D-9", "done --json D-9 posted"}
			if len(lines) != len(want) {
				t.Fatalf("calls = %q", lines)
			}
			for i, w := range want {
				if !strings.HasPrefix(lines[i], tt.prefix+w) {
					t.Errorf("call %d = %q, want prefix %q", i, lines[i], tt.prefix+w)
				}
			}
		})
	}
}

func TestDecideCommandDefault(t *testing.T) {
	if got := (Config{}).DecideArgv(); fmt.Sprint(got) != "[decide]" {
		t.Errorf("DecideArgv = %q, want [decide]", got)
	}
}

func TestDecideCommandNotFound(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "no-decide")
	cfg := Config{DecideCommand: Command{missing}}
	if _, err := cfg.ResolveDecide(); err == nil || !strings.Contains(err.Error(), missing) {
		t.Errorf("ResolveDecide = %v, want an error naming %s", err, missing)
	}
	d := DecideCLI{Run: ExecDecide(cfg.DecideArgv())}
	if _, err := d.Show(context.Background(), "D-1"); err == nil || !strings.Contains(err.Error(), "decide show") {
		t.Errorf("Show = %v, want a decide show error", err)
	}
}

func TestDecideCommandRejectsEmpty(t *testing.T) {
	for _, v := range []string{`""`, `[]`, `[""]`, `3`} {
		if _, err := LoadConfig(writeFile(t, "config.json", `{"decideCommand": `+v+`}`, 0o600)); err == nil {
			t.Errorf("decideCommand %s loaded, want an error", v)
		}
	}
}

func TestOutboxPollRecord(t *testing.T) {
	t0 := time.Date(2026, 11, 9, 12, 0, 0, 0, time.UTC)
	var p OutboxPoll
	if p.Failing() {
		t.Error("zero poll is failing")
	}
	p.Record(t0, nil)
	if !p.OK || !p.LastOK.Equal(t0) || p.Failing() {
		t.Errorf("after ok = %+v", p)
	}
	p.Record(t0.Add(time.Minute), errors.New("boom"))
	p.Record(t0.Add(2*time.Minute), errors.New("bang"))
	if p.OK || !p.Failing() || !p.FailingSince.Equal(t0.Add(time.Minute)) || p.LastError != "bang" || !p.LastOK.Equal(t0) {
		t.Errorf("after failures = %+v", p)
	}
	p.Record(t0.Add(3*time.Minute), nil)
	if !p.OK || p.Failing() || !p.FailingSince.IsZero() || p.LastError != "" {
		t.Errorf("after recovery = %+v", p)
	}
}

func TestStatusOutboxPollRoundTrip(t *testing.T) {
	s := openTestStore(t)
	now := time.Now().Truncate(time.Second)
	st := Status{PID: 1, HeartbeatAt: now, HeartbeatInterval: Duration{time.Minute}, Connection: ConnConnected}
	st.OutboxPoll.Record(now, errors.New(`decide show: exec: "decide": executable file not found in $PATH`))
	if err := s.WriteStatus(st); err != nil {
		t.Fatal(err)
	}
	got, err := s.ReadStatus(now)
	if err != nil {
		t.Fatal(err)
	}
	g, w := got.OutboxPoll, st.OutboxPoll
	if g.OK != w.OK || g.LastError != w.LastError || !g.FailingSince.Equal(w.FailingSince) || !g.LastOK.Equal(w.LastOK) || got.Healthy() {
		t.Errorf("read %+v healthy %v, want %+v unhealthy", got.OutboxPoll, got.Healthy(), st.OutboxPoll)
	}
}
