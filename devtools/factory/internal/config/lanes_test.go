package config_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func laneLines() []string {
	return []string{
		"[[lane]]",
		`name = "linux"`,
		`machine = ""`,
		`checkout = "~/linux-checkout"`,
		`max_workers = 4`,
		`labels = ["needs-linux", "high-resource"]`,
		"",
		"[[lane]]",
		`name = "mac"`,
		`machine = "mac1"`,
		`checkout = "~/on-mac"`,
		`max_workers = 2`,
		`labels = ["needs-mac"]`,
		"",
		"[lanes]",
		`default = ["linux", "mac"]`,
		"",
	}
}

func loadLanes(t *testing.T, lines []string) (*config.Config, error) {
	t.Helper()
	f := newFake(t)
	f.dir("/home/u/linux-checkout")
	return f.load(tomlDoc(append(lines, githubLines()...)))
}

func TestLoadLanes(t *testing.T) {
	cfg, err := loadLanes(t, laneLines())
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	want := []config.Lane{
		{Name: "linux", Checkout: "/home/u/linux-checkout", MaxWorkers: 4, Labels: []string{"needs-linux", "high-resource"}},
		// A remote checkout is a path on that machine, so ~ is not expanded here.
		{Name: "mac", Machine: "mac1", Checkout: "~/on-mac", MaxWorkers: 2, Labels: []string{"needs-mac"}},
	}
	if !reflect.DeepEqual(cfg.Lanes, want) {
		t.Fatalf("lanes = %#v", cfg.Lanes)
	}
	if !reflect.DeepEqual(cfg.LaneOrder.Default, []string{"linux", "mac"}) {
		t.Fatalf("default = %#v", cfg.LaneOrder.Default)
	}
	if !cfg.HasLanes() {
		t.Fatal("HasLanes() = false")
	}
}

func TestLoadNoLanes(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if cfg.HasLanes() || cfg.Lanes != nil {
		t.Fatalf("lanes = %#v", cfg.Lanes)
	}
	routed, err := cfg.RouteLanes([]string{"needs-mac"})
	if err != nil || routed != nil {
		t.Fatalf("RouteLanes() = %#v, %v", routed, err)
	}
}

func TestLoadLanesRejected(t *testing.T) {
	tests := []struct {
		name  string
		lines []string
		want  string
	}{
		{
			name:  "lanes table without lanes",
			lines: []string{"[lanes]", `default = ["linux"]`, ""},
			want:  "lanes: expected at least one [[lane]], found none",
		},
		{
			name:  "missing default",
			lines: laneLines()[:len(laneLines())-4],
			want:  "lanes.default: expected at least one lane name, found none",
		},
		{
			name:  "unknown default",
			lines: replaceLine(laneLines(), "default", `default = ["linux", "win"]`),
			want:  `lanes.default: expected a [[lane]] name, found "win"`,
		},
		{
			name:  "duplicate default",
			lines: replaceLine(laneLines(), "default", `default = ["mac", "mac"]`),
			want:  `lanes.default: expected each lane once, found "mac" twice`,
		},
		{
			name:  "missing max_workers",
			lines: without(laneLines(), "max_workers"),
			want:  `lane "linux" field "max_workers": expected a positive integer, found none`,
		},
		{
			name:  "zero max_workers",
			lines: replaceLine(laneLines(), "max_workers", `max_workers = 0`),
			want:  `lane "linux" field "max_workers": expected a positive integer, found 0`,
		},
		{
			name:  "duplicate name",
			lines: replaceLine(laneLines(), "name", `name = "mac"`),
			want:  `lane "mac" field "name": expected a unique name, found duplicate`,
		},
		{
			name:  "label in two lanes",
			lines: replaceLine(laneLines(), "labels", `labels = ["needs-mac"]`),
			want:  `lane "mac" field "labels": expected label "needs-mac" in one lane, found it in linux too`,
		},
		{
			name:  "missing local checkout",
			lines: replaceLine(laneLines(), "checkout", `checkout = "/nowhere"`),
			want:  `lane "linux" field "checkout": expected a directory, found /nowhere`,
		},
		{
			name:  "unknown key",
			lines: append([]string{"[[lane]]", `name = "x"`, `max_workers = 1`, `checkout = "/work/repo"`, `cpu = 4`, ""}, "[lanes]", `default = ["x"]`, ""),
			want:  "unknown key lane.cpu",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := loadLanes(t, tc.lines)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Load() error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestRouteLanes(t *testing.T) {
	cfg, err := loadLanes(t, laneLines())
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	tests := []struct {
		name    string
		labels  []string
		want    []string
		wantErr string
	}{
		{name: "needs-mac goes to mac", labels: []string{"bug", "needs-mac"}, want: []string{"mac"}},
		{name: "needs-linux goes to linux", labels: []string{"needs-linux"}, want: []string{"linux"}},
		{name: "any lane label routes", labels: []string{"high-resource"}, want: []string{"linux"}},
		{name: "unlabeled uses default order", labels: []string{"bug"}, want: []string{"linux", "mac"}},
		{name: "no labels uses default order", want: []string{"linux", "mac"}},
		{
			name:    "conflicting labels",
			labels:  []string{"needs-mac", "needs-linux"},
			wantErr: "lane config: expected item labels to match at most one lane, found linux (needs-linux); mac (needs-mac)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cfg.RouteLanes(tc.labels)
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("RouteLanes() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("RouteLanes() error = %v", err)
			}
			names := make([]string, len(got))
			for i, lane := range got {
				names[i] = lane.Name
			}
			if !reflect.DeepEqual(names, tc.want) {
				t.Fatalf("RouteLanes() = %v, want %v", names, tc.want)
			}
		})
	}
}

func TestPickLane(t *testing.T) {
	cfg, err := loadLanes(t, laneLines())
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	linuxOnly, err := cfg.RouteLanes([]string{"needs-linux"})
	if err != nil {
		t.Fatal(err)
	}
	both, err := cfg.RouteLanes(nil)
	if err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name    string
		value   string
		routed  []config.Lane
		want    string
		wantErr string
	}{
		{name: "lane name", value: "mac", routed: both, want: "mac"},
		{name: "machine", value: "mac1", routed: both, want: "mac"},
		{
			name:    "incompatible with labels",
			value:   "mac",
			routed:  linuxOnly,
			wantErr: "--machine mac: lane mac is not compatible with the item labels; expected one of linux",
		},
		{
			name:    "unknown",
			value:   "win",
			routed:  both,
			wantErr: "--machine win: expected a lane name or a lane machine, found none (lanes: linux, mac)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := cfg.PickLane(tc.value, tc.routed)
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("PickLane() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got.Name != tc.want {
				t.Fatalf("PickLane() = %q, %v, want %q", got.Name, err, tc.want)
			}
		})
	}
}
