package main

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"
)

// leaseNowClock is the fake clock newLeaseWorld gives the daemon, as
// the local HH:MM the CLI prints.
func leaseNowClock() string {
	return time.Date(2026, 11, 10, 9, 0, 0, 0, time.UTC).Local().Format("15:04")
}

func TestLeaseClaimNamesTheBranch(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()

	if out := w.mustLease(t, "p1", "claim", "--for", "40m"); !strings.HasPrefix(out, "granted until ") || !strings.HasSuffix(out, "; checkout on main\n") {
		t.Fatalf("claim = %q", out)
	}
	w.mustLease(t, "p1", "release")

	w.git(t, w.repo, "switch", "-q", "--detach")
	head := w.git(t, w.repo, "rev-parse", "--short", "HEAD")
	if out := w.mustLease(t, "p1", "claim", "--for", "40m"); !strings.HasSuffix(out, "; checkout at "+head+" (detached)\n") {
		t.Fatalf("detached claim = %q", out)
	}
}

func TestLeaseQuickReclaimAfterASwitchingReleaseHints(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	w.mustLease(t, "p1", "release")

	out := w.mustLease(t, "p1", "claim", "--for", "40m", "--json")
	var r struct {
		Result         string `json:"result"`
		Branch         string `json:"branch"`
		PreviousBranch string `json:"previous_branch"` //nolint:tagliatelle // the claim reply shape
	}
	if err := json.Unmarshal([]byte(out), &r); err != nil {
		t.Fatalf("claim --json = %s: %v", out, err)
	}
	if r.Result != "granted" || r.Branch != "main" || r.PreviousBranch != "feature" {
		t.Fatalf("claim --json = %s", out)
	}

	// Text: release again from feature and re-claim.
	w.git(t, w.repo, "switch", "-q", "feature")
	w.mustLease(t, "p1", "release")
	out = w.mustLease(t, "p1", "claim", "--for", "40m")
	if want := "; checkout on main (you were on feature when you released at " + leaseNowClock() + ")\n"; !strings.HasSuffix(out, want) {
		t.Fatalf("re-claim = %q, want suffix %q", out, want)
	}
}

func TestLeaseReclaimWithoutAHint(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()

	// Another holder after a switching release.
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	w.mustLease(t, "p1", "release")
	if out := w.mustLease(t, "p2", "claim", "--for", "40m"); strings.Contains(out, "you were on") {
		t.Fatalf("another holder's claim = %q", out)
	}
	if out := w.mustLease(t, "p2", "claim", "--for", "40m", "--json"); strings.Contains(out, "previous_branch") {
		t.Fatalf("another holder's claim --json = %s", out)
	}
	w.mustLease(t, "p2", "release")

	// A release that did not switch.
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p1", "release")
	if out := w.mustLease(t, "p1", "claim", "--for", "40m"); strings.Contains(out, "you were on") {
		t.Fatalf("re-claim after a release on main = %q", out)
	}
}

func TestLeaseWakeNamesTheBranch(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	w.mustLease(t, "p1", "claim", "--for", "40m")
	w.mustLease(t, "p2", "claim", "--for", "20m")
	w.git(t, w.repo, "switch", "-q", "-c", "feature")
	w.mustLease(t, "p1", "release")

	wake := w.ad.Texts[len(w.ad.Texts)-1]
	if !strings.Contains(wake, "[lease] granted ") || !strings.Contains(wake, "; checkout on main. Release with: ") {
		t.Fatalf("wake = %q", wake)
	}
}

func TestLeaseClaimWhenTheBranchCannotBeRead(t *testing.T) {
	w := newLeaseWorld(t)
	stop := startDaemon(t, w.dirs, w.cfg)
	defer stop()
	orig := currentBranch
	currentBranch = func(string, []string) (string, error) { return "", errors.New("git broke") }
	t.Cleanup(func() { currentBranch = orig })

	out := w.mustLease(t, "p1", "claim", "--for", "40m")
	if !strings.HasPrefix(out, "granted until ") || strings.Contains(out, "checkout") || strings.Count(out, "\n") != 1 {
		t.Fatalf("claim = %q", out)
	}
	if out := w.mustLease(t, "p1", "claim", "--for", "40m", "--json"); strings.Contains(out, `"branch"`) {
		t.Fatalf("claim --json = %s", out)
	}
}
