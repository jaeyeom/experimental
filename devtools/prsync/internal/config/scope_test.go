package config

import (
	"errors"
	"path/filepath"
	"slices"
	"testing"
)

func TestLoadRunScope(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "machines=m2\ntab_machine=m2\nuntabbed=include\nskip_jobs=\"team-cover, team-cover\"\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.TabMachine != "m2" || got.Untabbed != "include" || !slices.Equal(got.SkipJobs, []string{JobTeamCover}) {
		t.Fatalf("scope = %q %q %q", got.TabMachine, got.Untabbed, got.SkipJobs)
	}
	if !got.IncludeUntabbed() || !got.Skips(JobTeamCover) {
		t.Fatalf("IncludeUntabbed=%v Skips=%v, want both true", got.IncludeUntabbed(), got.Skips(JobTeamCover))
	}
}

func TestLoadRunScopeRejectsUnknownValues(t *testing.T) {
	t.Parallel()

	tests := []struct {
		body string
		key  string
	}{
		{"machines=m2\ntab_machine=m3\n", "tab_machine"},
		{"tab_machine=m2\n", "tab_machine"},
		{"untabbed=maybe\n", "untabbed"},
		{"skip_jobs=nudge\n", "skip_jobs"},
	}
	for _, tc := range tests {
		path := filepath.Join(t.TempDir(), "prsync.config")
		mustWrite(t, path, tc.body)
		var keyErr *KeyError
		if _, err := Load(path); !errors.As(err, &keyErr) || keyErr.Key != tc.key {
			t.Errorf("Load(%q) error = %v, want %s key error", tc.body, err, tc.key)
		}
	}
}

func TestIncludeUntabbedDefaultsToLocal(t *testing.T) {
	t.Parallel()

	tests := []struct {
		machine, untabbed string
		want              bool
	}{
		{"", "", true},
		{"local", "", true},
		{"m2", "", false},
		{"local", "exclude", false},
		{"m2", "include", true},
	}
	for _, tc := range tests {
		cfg := Defaults()
		cfg.Machines = []string{"m2"}
		cfg.TabMachine = tc.machine
		cfg.Untabbed = tc.untabbed
		if got := cfg.IncludeUntabbed(); got != tc.want {
			t.Errorf("IncludeUntabbed(%q, %q) = %v, want %v", tc.machine, tc.untabbed, got, tc.want)
		}
	}
}

func TestLoadMachineCheckouts(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "machines=mac linux2\nmachine_checkouts=\"mac=/Users/me/src/repo, linux2=/home/me/repo\"\ntab_machine=mac\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.MachineCheckouts["mac"] != "/Users/me/src/repo" || got.MachineCheckouts["linux2"] != "/home/me/repo" || len(got.MachineCheckouts) != 2 {
		t.Fatalf("machine_checkouts = %v", got.MachineCheckouts)
	}
	if got.GateMachine() != "mac" {
		t.Fatalf("GateMachine() = %q, want mac", got.GateMachine())
	}
}

func TestGateMachineIsEmptyForLocalRuns(t *testing.T) {
	t.Parallel()

	for _, tm := range []string{"", LocalMachine} {
		cfg := Defaults()
		cfg.Machines = []string{"mac"}
		cfg.TabMachine = tm
		if got := cfg.GateMachine(); got != "" {
			t.Errorf("GateMachine() with tab_machine %q = %q, want empty", tm, got)
		}
	}
}

func TestLoadMachineCheckoutsRejectsBadEntries(t *testing.T) {
	t.Parallel()

	for _, body := range []string{
		"machines=mac\nmachine_checkouts=mac\n",
		"machines=mac\nmachine_checkouts=mac=\n",
		"machines=mac\nmachine_checkouts=mac=src/repo\n",
		"machines=mac\nmachine_checkouts=m3=/src/repo\n",
		"machines=mac\nmachine_checkouts=local=/src/repo\n",
	} {
		path := filepath.Join(t.TempDir(), "prsync.config")
		mustWrite(t, path, body)
		var keyErr *KeyError
		if _, err := Load(path); !errors.As(err, &keyErr) || keyErr.Key != "machine_checkouts" {
			t.Errorf("Load(%q) error = %v, want machine_checkouts key error", body, err)
		}
	}
}
