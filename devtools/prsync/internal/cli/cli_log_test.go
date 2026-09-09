package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/version"
	executor "github.com/jaeyeom/go-cmdexec"
)

func TestLogLevelInvalidFlag(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"--log-level", "loud", "version"}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "invalid log level") {
		t.Fatalf("stderr = %q, want invalid log level", stderr.String())
	}
}

func TestResolveLogLevel(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		flag    string
		env     string
		want    slog.Level
		wantErr bool
	}{
		{name: "default info", want: slog.LevelInfo},
		{name: "flag debug", flag: "debug", env: "error", want: slog.LevelDebug},
		{name: "env error", env: "error", want: slog.LevelError},
		{name: "env invalid", env: "loud", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := resolveLogLevel(tc.flag, tc.env)
			if tc.wantErr {
				if err == nil {
					t.Fatal("error = nil, want invalid level")
				}
				return
			}
			if err != nil {
				t.Fatalf("resolveLogLevel() error = %v", err)
			}
			if got != tc.want {
				t.Fatalf("resolveLogLevel() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestVersionWritesRunLogWithoutChangingStdout(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version"}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q, want empty", stderr.String())
	}
	var got map[string]string
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("stdout json: %v\n%s", err, stdout.String())
	}
	if got["version"] != version.Version {
		t.Fatalf("version = %q, want %q", got["version"], version.Version)
	}

	recs := readRunLog(t, state)
	if !logHas(recs, "command") {
		t.Fatalf("missing command in %v", logMsgs(recs))
	}
	exitRec, ok := logByMsg(recs, "exit")
	if !ok {
		t.Fatalf("missing exit in %v", logMsgs(recs))
	}
	if exitRec["code"] != float64(0) {
		t.Fatalf("exit.code = %v, want 0", exitRec["code"])
	}
}

func TestLogLevelDebugWritesCommandRecord(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"--log-level", "debug", "version"}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	recs := readRunLog(t, state)
	if !logHas(recs, "command") {
		t.Fatalf("missing command in %v", logMsgs(recs))
	}
}

func TestLogsPathJSON(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"logs", "--path"}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	var got map[string]string
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	want, err := runlog.FilePath()
	if err != nil {
		t.Fatal(err)
	}
	if got["path"] != want {
		t.Fatalf("path = %q, want %q", got["path"], want)
	}
}

func TestLogsDumpsFile(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)

	var stdout, stderr bytes.Buffer
	if code := Execute(context.Background(), []string{"version"}, &stdout, &stderr, nil); code != ExitOK {
		t.Fatalf("version exit = %d, stderr=%q", code, stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	code := Execute(context.Background(), []string{"logs"}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("logs exit = %d, stderr=%q", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), `"msg":"exit"`) {
		t.Fatalf("logs stdout = %q, want dumped JSONL including exit", stdout.String())
	}
}

func TestDispatchGoLogsLifecycleToStateDir(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	ghBin, herdrBin := fixtureBins(t)
	statePath := filepath.Join(t.TempDir(), "state.json")
	cfgPath := writeLiveConfig(t, ghBin, herdrBin, statePath)
	raw := mustScanJSON(t, stdinEligibleDoc())

	restore := swapStdin(t, string(raw))
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--stdin", "--config", cfgPath, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	got := decodeDispatch(t, stdout.Bytes())
	if len(got.Results) != 1 || got.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("results = %+v, want dispatched", got.Results)
	}

	recs := readRunLog(t, state)
	for _, msg := range []string{"startup", "gate_wait_start", "gate_wait_end", "dispatch_send", "settle", "result", "exit"} {
		if !logHas(recs, msg) {
			t.Fatalf("missing %s in %v", msg, logMsgs(recs))
		}
	}
	start, _ := logByMsg(recs, "startup")
	if start["command"] != "dispatch" {
		t.Fatalf("startup.command = %v, want dispatch", start["command"])
	}
	if start["config_path"] != cfgPath {
		t.Fatalf("startup.config_path = %v, want %s", start["config_path"], cfgPath)
	}
	if start["concurrency_wait_on"] != "any" {
		t.Fatalf("startup.concurrency_wait_on = %v, want any", start["concurrency_wait_on"])
	}
}

func readRunLog(t *testing.T, xdgStateHome string) []map[string]any {
	t.Helper()
	path := filepath.Join(xdgStateHome, "prsync", "prsync.jsonl")
	data, err := os.ReadFile(path) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	var recs []map[string]any
	dec := json.NewDecoder(bytes.NewReader(data))
	for {
		var rec map[string]any
		if err := dec.Decode(&rec); err != nil {
			if err == io.EOF {
				break
			}
			t.Fatalf("decode %s: %v\n%s", path, err, data)
		}
		recs = append(recs, rec)
	}
	return recs
}

func logMsgs(recs []map[string]any) []string {
	out := make([]string, 0, len(recs))
	for _, r := range recs {
		if s, ok := r["msg"].(string); ok {
			out = append(out, s)
		}
	}
	return out
}

func logHas(recs []map[string]any, msg string) bool {
	for _, r := range recs {
		if r["msg"] == msg {
			return true
		}
	}
	return false
}

func logByMsg(recs []map[string]any, msg string) (map[string]any, bool) {
	for _, r := range recs {
		if r["msg"] == msg {
			return r, true
		}
	}
	return nil, false
}
