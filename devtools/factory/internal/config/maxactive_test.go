package config_test

import (
	"strings"
	"testing"
)

func TestLoadLaneMaxActive(t *testing.T) {
	cfg, err := loadLanes(t, withMacLine("max_active = 5"))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Lanes[0].MaxActive != 0 || cfg.Lanes[1].MaxActive != 5 {
		t.Fatalf("max_active = %d, %d; want 0 (unset) and 5", cfg.Lanes[0].MaxActive, cfg.Lanes[1].MaxActive)
	}
	_, err = loadLanes(t, withMacLine("max_active = 0"))
	want := `lane "mac" field "max_active": expected a positive integer or no key, found 0`
	if err == nil || err.Error() != want {
		t.Fatalf("Load() error = %v, want %q", err, want)
	}
}

func TestOrderedLanes(t *testing.T) {
	cfg, err := loadLanes(t, replaceLine(laneLines(), "default", `default = ["mac"]`))
	if err != nil {
		t.Fatal(err)
	}
	var names []string
	for _, lane := range cfg.OrderedLanes() {
		names = append(names, lane.Name)
	}
	if len(names) != 2 || names[0] != "mac" || names[1] != "linux" {
		t.Fatalf("OrderedLanes = %v, want [mac linux]", names)
	}
}

// withMacLine is laneLines with line added to the mac lane.
func withMacLine(line string) []string {
	var out []string
	for _, l := range laneLines() {
		out = append(out, l)
		if l == "max_workers = 2" {
			out = append(out, line)
		}
	}
	return out
}

func TestLoadLaneExec(t *testing.T) {
	cfg, err := loadLanes(t, withMacLine(`exec = ["ssh", "-o", "BatchMode=yes", "mac1"]`))
	if err != nil {
		t.Fatal(err)
	}
	if got := cfg.Lanes[1].Exec; len(got) != 4 || got[0] != "ssh" || got[3] != "mac1" {
		t.Fatalf("exec = %q", got)
	}
	_, err = loadLanes(t, withMacLine(`exec = ["ssh", ""]`))
	if err == nil || !strings.Contains(err.Error(), `lane "mac" field "exec": expected non-empty entries`) {
		t.Fatalf("empty entry: err = %v", err)
	}
	lines := append([]string(nil), laneLines()...)
	for i, l := range lines {
		if l == "max_workers = 4" {
			lines = append(lines[:i+1], append([]string{`exec = ["ssh", "x"]`}, lines[i+1:]...)...)
			break
		}
	}
	_, err = loadLanes(t, lines)
	if err == nil || !strings.Contains(err.Error(), `lane "linux" field "exec": expected it only on a remote lane`) {
		t.Fatalf("local lane: err = %v", err)
	}
}
