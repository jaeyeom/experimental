package jobs

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestPolicyGate(t *testing.T) {
	p, err := ParsePolicy([]byte(`{
  "default": "auto",
  "kinds": {"merge": "auto", "comment": "ask", "push": "off"},
  "floor": ["merge", "push"]
}`))
	if err != nil {
		t.Fatal(err)
	}
	for kind, want := range map[string]Gate{
		"rebase":  GateAuto, // the default
		"comment": GateAsk,
		"merge":   GateAsk, // floor: never auto
		"push":    GateOff, // floor does not loosen off
	} {
		if got := p.Gate(kind); got != want {
			t.Errorf("Gate(%s) = %s, want %s", kind, got, want)
		}
	}
}

func TestPolicyZeroAndNoDefaultAskEveryKind(t *testing.T) {
	if got := (Policy{}).Gate("rebase"); got != GateAsk {
		t.Fatalf("zero policy gate = %s, want ask", got)
	}
	p, err := ParsePolicy([]byte(`{"kinds": {"rebase": "auto"}}`))
	if err != nil {
		t.Fatal(err)
	}
	if p.Gate("rebase") != GateAuto || p.Gate("merge") != GateAsk {
		t.Fatalf("gates = %s %s, want auto ask", p.Gate("rebase"), p.Gate("merge"))
	}
}

func TestParsePolicyRefusesBadValues(t *testing.T) {
	for _, text := range []string{
		`{"default": "yes"}`,
		`{"kinds": {"merge": "sometimes"}}`,
		`{"kinds": {"Merge": "ask"}}`,
		`{"floor": ["Bad Kind"]}`,
		`{"gates": {}}`,
		`not json`,
	} {
		if _, err := ParsePolicy([]byte(text)); err == nil {
			t.Errorf("ParsePolicy(%s) accepted", text)
		}
	}
}

func writePolicy(t *testing.T, mode os.FileMode, text string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "jobs-policy.json")
	if err := os.WriteFile(path, []byte(text), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, mode); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestLoadPolicy(t *testing.T) {
	p, err := LoadPolicy(writePolicy(t, 0o644, `{"default": "auto"}`))
	if err != nil || p.Gate("rebase") != GateAuto {
		t.Fatalf("LoadPolicy = %+v, %v", p, err)
	}
}

func TestLoadPolicyMissingIsErrNoPolicy(t *testing.T) {
	p, err := LoadPolicy(filepath.Join(t.TempDir(), "none.json"))
	if !errors.Is(err, ErrNoPolicy) {
		t.Fatalf("err = %v, want ErrNoPolicy", err)
	}
	if p.Gate("rebase") != GateAsk {
		t.Fatalf("missing policy gate = %s, want ask", p.Gate("rebase"))
	}
}

func TestLoadPolicyRefusesAFileOthersCanWrite(t *testing.T) {
	for _, mode := range []os.FileMode{0o664, 0o646} {
		p, err := LoadPolicy(writePolicy(t, mode, `{"default": "auto"}`))
		if err == nil || !strings.Contains(err.Error(), "writable") {
			t.Fatalf("mode %o: err = %v, want a writable refusal", mode, err)
		}
		if p.Gate("rebase") != GateAsk {
			t.Fatalf("mode %o: refused policy gate = %s, want ask", mode, p.Gate("rebase"))
		}
	}
}

func TestLoadPolicyRefusesADirectory(t *testing.T) {
	if _, err := LoadPolicy(t.TempDir()); err == nil {
		t.Fatal("a directory was accepted as the policy")
	}
}
