package lease

import (
	"testing"
	"time"
)

// releaseFrom has caller release key after switching from branch to
// main; an empty from means the release did not switch.
func releaseFrom(t *testing.T, tbl *Table, caller, from string) {
	t.Helper()
	if _, err := tbl.Release(caller, Args{Checkout: key, FromBranch: from, Branch: "main"}, noneGone); err != nil {
		t.Fatal(err)
	}
}

func TestQuickReclaimAfterASwitchingReleaseGetsTheHint(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m", Branch: "feature"})
	releaseFrom(t, tbl, alice, "feature")
	released := c.now

	c.now = t0().Add(time.Hour)
	r := mustClaim(t, tbl, alice, Args{For: "40m", Branch: "main"})
	if r.Result != ResultGranted || r.PreviousBranch != "feature" || r.ReleasedAt != released.Format(time.RFC3339) {
		t.Fatalf("re-claim = %+v", r)
	}

	// The hint is given once: the grant cleared the record.
	releaseFrom(t, tbl, alice, "")
	if r := mustClaim(t, tbl, alice, Args{For: "40m", Branch: "main"}); r.PreviousBranch != "" || r.ReleasedAt != "" {
		t.Fatalf("second re-claim = %+v", r)
	}
}

func TestReclaimGetsNoHint(t *testing.T) {
	for name, tc := range map[string]struct {
		from, branch string
		after        time.Duration
		claimer      string
	}{
		"another holder":     {from: "feature", branch: "main", after: time.Minute, claimer: bob},
		"over an hour later": {from: "feature", branch: "main", after: time.Hour + time.Second, claimer: alice},
		"release kept as is": {from: "", branch: "main", after: time.Minute, claimer: alice},
		"back on the branch": {from: "feature", branch: "feature", after: time.Minute, claimer: alice},
		"branch unknown":     {from: "feature", branch: "", after: time.Minute, claimer: alice},
	} {
		t.Run(name, func(t *testing.T) {
			c := &clock{now: t0()}
			tbl := openTable(t, t.TempDir(), c)
			mustClaim(t, tbl, alice, Args{For: "40m"})
			releaseFrom(t, tbl, alice, tc.from)
			c.now = t0().Add(tc.after)
			r := mustClaim(t, tbl, tc.claimer, Args{For: "40m", Branch: tc.branch})
			if r.Result != ResultGranted || r.PreviousBranch != "" || r.ReleasedAt != "" {
				t.Fatalf("claim = %+v", r)
			}
		})
	}
}

func TestAnotherGrantClearsTheReleaseRecord(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	releaseFrom(t, tbl, alice, "feature")
	mustClaim(t, tbl, bob, Args{For: "5m"})
	releaseFrom(t, tbl, bob, "")
	if r := mustClaim(t, tbl, alice, Args{For: "40m", Branch: "main"}); r.PreviousBranch != "" {
		t.Fatalf("claim after bob's grant = %+v", r)
	}
}

func TestOnBranch(t *testing.T) {
	for branch, want := range map[string]string{
		"main":                  "checkout on main",
		"(detached at 1a2b3c4)": "checkout at 1a2b3c4 (detached)",
		"":                      "",
	} {
		if got := OnBranch(branch); got != want {
			t.Errorf("OnBranch(%q) = %q, want %q", branch, got, want)
		}
	}
}
