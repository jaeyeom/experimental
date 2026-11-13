package diskwatch

import (
	"path/filepath"
	"testing"
	"time"
)

func TestStateRoundTrip(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	path := filepath.Join(dir, "diskwatch", "state.json")
	if _, err := LoadState(path); err != nil {
		t.Fatal(err)
	}
	want := State{HookAt: time.Date(2026, 11, 13, 1, 2, 3, 0, time.UTC)}
	want.put("/", MountState{Armed: false, LastAction: "ok"})
	if err := SaveState(path, want); err != nil {
		t.Fatal(err)
	}
	got, err := LoadState(path)
	if err != nil {
		t.Fatal(err)
	}
	if got.Mounts["/"].Armed || got.Mounts["/"].LastAction != "ok" {
		t.Fatalf("state = %+v", got.Mounts["/"])
	}
	if !got.HookAt.Equal(want.HookAt) {
		t.Fatalf("hook = %s", got.HookAt)
	}
}

func TestDecideHysteresis(t *testing.T) {
	t.Parallel()
	mount := Mount{
		Low:      Amount{IsPercent: true, Percent: 15},
		High:     Amount{IsPercent: true, Percent: 25},
		Urgent:   &Amount{IsPercent: true, Percent: 5},
		Cooldown: 6 * time.Hour,
	}
	now := time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC)
	st := MountState{Armed: true}
	trigger, kind := decide(100, 1000, mount, st, now)
	if !trigger || kind != "low" {
		t.Fatalf("first = %v %s", trigger, kind)
	}
	st.Armed = false
	st.TriggeredAt = now
	if trigger, _ := decide(100, 1000, mount, st, now.Add(time.Hour)); trigger {
		t.Fatal("quiet during cooldown")
	}
	st = rearm(300, 1000, mount, st)
	if !st.Armed {
		t.Fatal("re-arm above high")
	}
	if trigger, _ := decide(100, 1000, mount, st, now.Add(2*time.Hour)); trigger {
		t.Fatal("re-armed mount stays quiet during cooldown")
	}
	trigger, kind = decide(100, 1000, mount, st, now.Add(6*time.Hour+time.Second))
	if !trigger || kind != "low" {
		t.Fatalf("after cooldown = %v %s", trigger, kind)
	}
	st.Armed = false
	st.TriggeredAt = now.Add(6*time.Hour + time.Second)
	trigger, kind = decide(40, 1000, mount, st, now.Add(6*time.Hour+time.Minute))
	if !trigger || kind != "urgent" {
		t.Fatalf("urgent = %v %s", trigger, kind)
	}
}
