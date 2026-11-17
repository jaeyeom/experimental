package config

import (
	"errors"
	"path/filepath"
	"testing"
)

func TestLoadJobMachinesAndLanes(t *testing.T) {
	t.Parallel()

	path := filepath.Join(t.TempDir(), "prsync.config")
	mustWrite(t, path, "machines=linux2\njob_machines=\"tiger, linux2=turtle\"\njob_lanes=\"needs-linux=linux2, needs-mac=local\"\n")
	got, err := Load(path)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.QueueMachine(LocalMachine) != "tiger" || got.QueueMachine("linux2") != "turtle" {
		t.Fatalf("job_machines = %v", got.JobMachines)
	}
	if got.JobLanes["needs-linux"] != "linux2" || got.JobLanes["needs-mac"] != LocalMachine || len(got.JobLanes) != 2 {
		t.Fatalf("job_lanes = %v", got.JobLanes)
	}
}

func TestQueueMachineDefaultsToTheHerdrName(t *testing.T) {
	t.Parallel()

	cfg := Defaults()
	if got := cfg.QueueMachine(LocalMachine); got != LocalMachine {
		t.Fatalf("QueueMachine(local) = %q", got)
	}
	if got := cfg.QueueMachine("linux2"); got != "linux2" {
		t.Fatalf("QueueMachine(linux2) = %q", got)
	}
}

func TestLoadJobLanesRejectsBadEntries(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct{ body, key string }{
		{"job_lanes=needs-linux\n", "job_lanes"},
		{"job_lanes=needs-linux=\n", "job_lanes"},
		{"job_lanes=needs-linux=m3\n", "job_lanes"},
		{"job_machines=m3=turtle\n", "job_machines"},
	} {
		path := filepath.Join(t.TempDir(), "prsync.config")
		mustWrite(t, path, tc.body)
		var keyErr *KeyError
		if _, err := Load(path); !errors.As(err, &keyErr) || keyErr.Key != tc.key {
			t.Errorf("Load(%q) error = %v, want %s key error", tc.body, err, tc.key)
		}
	}
}
