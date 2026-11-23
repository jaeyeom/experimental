package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

func writeTestFile(t *testing.T, path string, size int) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %s: %v", filepath.Dir(path), err)
	}
	data := bytes.Repeat([]byte("a"), size)
	if err := os.WriteFile(path, data, 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func TestRunVersion(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"--version"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run(--version) = %d, want 0; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "agents-md-size ") {
		t.Errorf("stdout = %q, want prefix 'agents-md-size '", stdout.String())
	}
}

func TestRunHelp(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"--help"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run(--help) = %d, want 0; stderr=%s", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), "-max-bytes") {
		t.Errorf("help output missing -max-bytes: %s", stderr.String())
	}
}

func TestRunMutuallyExclusiveFlags(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := run([]string{"--update-baseline", "--tighten-baseline"}, &stdout, &stderr)
	if code != 2 {
		t.Fatalf("run(both update and tighten) = %d, want 2", code)
	}
	if !strings.Contains(stderr.String(), "mutually exclusive") {
		t.Errorf("stderr = %q, want mutually exclusive message", stderr.String())
	}
}

func TestRunInvalidFlagAndInvalidLimits(t *testing.T) {
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--unknown-flag"}, &stdout, &stderr); code != 2 {
		t.Errorf("run(--unknown-flag) = %d, want 2", code)
	}
	stderr.Reset()
	if code := run([]string{"--max-bytes=0"}, &stdout, &stderr); code != 2 {
		t.Errorf("run(--max-bytes=0) = %d, want 2", code)
	}
	stderr.Reset()
	if code := run([]string{"--max-cascading-bytes=-1"}, &stdout, &stderr); code != 2 {
		t.Errorf("run(--max-cascading-bytes=-1) = %d, want 2", code)
	}
}

func TestRunPassAndViolation(t *testing.T) {
	root := t.TempDir()
	writeTestFile(t, filepath.Join(root, "AGENTS.md"), 100)

	var stdout, stderr bytes.Buffer
	code := run([]string{"--root", root}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("run(pass) = %d, want 0; stderr=%s", code, stderr.String())
	}

	writeTestFile(t, filepath.Join(root, "sub", "AGENTS.md"), 5000)
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"--root", root}, &stdout, &stderr)
	if code != 1 {
		t.Fatalf("run(violation) = %d, want 1; stdout=%s stderr=%s", code, stdout.String(), stderr.String())
	}
	out := stderr.String()
	if !strings.Contains(out, "sub/AGENTS.md") || !strings.Contains(out, "5000") || !strings.Contains(out, "4096") {
		t.Errorf("violation output missing expected details: %s", out)
	}
	if !strings.Contains(out, "--update-baseline") {
		t.Errorf("violation output missing --update-baseline hint: %s", out)
	}
}

func TestRunBaselineWorkflow(t *testing.T) {
	root := t.TempDir()
	writeTestFile(t, filepath.Join(root, "AGENTS.md"), 100)
	writeTestFile(t, filepath.Join(root, "big", "AGENTS.md"), 5000)
	baselinePath := filepath.Join(root, ".baselines", "agents-md-size.json")

	// 1. Update baseline records big/AGENTS.md at 5000.
	var stdout, stderr bytes.Buffer
	code := run([]string{"--root", root, "--update-baseline"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("update-baseline = %d, want 0; stderr=%s", code, stderr.String())
	}

	// Add reason metadata manually to verify preservation.
	bf, err := baseline.Load(baselinePath)
	if err != nil {
		t.Fatalf("load baseline: %v", err)
	}
	entry := bf.Entries["big/AGENTS.md"]
	entry.Reason = "legacy doc"
	bf.Entries["big/AGENTS.md"] = entry
	if err := baseline.Save(baselinePath, bf); err != nil {
		t.Fatalf("save baseline: %v", err)
	}

	// 2. Strict check passes.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"--root", root}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("strict check = %d, want 0; stderr=%s", code, stderr.String())
	}

	// 3. Shrink big/AGENTS.md to 4500 (still > 4096, ratchetable).
	writeTestFile(t, filepath.Join(root, "big", "AGENTS.md"), 4500)
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"--root", root}, &stdout, &stderr)
	if code != 1 {
		t.Fatalf("strict check with ratchetable entry = %d, want 1", code)
	}
	if !strings.Contains(stderr.String(), "--tighten-baseline") {
		t.Errorf("expected --tighten-baseline hint in stderr: %s", stderr.String())
	}

	// Non-strict check passes.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"--root", root, "--strict-baseline=false"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("non-strict check = %d, want 0; stderr=%s", code, stderr.String())
	}

	// 4. Tighten baseline ratchets to 4500 and preserves reason.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"--root", root, "--tighten-baseline"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("tighten-baseline = %d, want 0; stderr=%s", code, stderr.String())
	}
	bf, err = baseline.Load(baselinePath)
	if err != nil {
		t.Fatalf("reload baseline: %v", err)
	}
	if got := bf.Entries["big/AGENTS.md"].Limits["max_bytes"]; got != 4500 {
		t.Errorf("ratcheted max_bytes = %d, want 4500", got)
	}
	if got := bf.Entries["big/AGENTS.md"].Reason; got != "legacy doc" {
		t.Errorf("reason = %q, want %q", got, "legacy doc")
	}
}

func TestRunTightenBaselineWithUnbaselinedViolation(t *testing.T) {
	root := t.TempDir()
	writeTestFile(t, filepath.Join(root, "AGENTS.md"), 5000)

	var stdout, stderr bytes.Buffer
	code := run([]string{"--root", root, "--tighten-baseline"}, &stdout, &stderr)
	if code != 1 {
		t.Fatalf("tighten-baseline with unbaselined violation = %d, want 1", code)
	}
}

func TestRunMalformedBaselineReturnsExit2(t *testing.T) {
	root := t.TempDir()
	writeTestFile(t, filepath.Join(root, "AGENTS.md"), 100)
	badBaseline := filepath.Join(root, "bad.json")
	if err := os.WriteFile(badBaseline, []byte("{not json"), 0o644); err != nil {
		t.Fatal(err)
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"--root", root, "--baseline", badBaseline}, &stdout, &stderr)
	if code != 2 {
		t.Fatalf("malformed baseline = %d, want 2; stderr=%s", code, stderr.String())
	}
}
