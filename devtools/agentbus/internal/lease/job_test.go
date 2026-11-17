package lease

import (
	"testing"
	"time"
)

func TestClaimForJobGrantsOnlyAFreeCheckout(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	r, err := tbl.ClaimForJob(key, "j-42", "runner tiger-runner on tiger")
	if err != nil {
		t.Fatalf("ClaimForJob: %v", err)
	}
	h := r.Lease.Holder
	if r.Result != ResultGranted || r.Lease.State != StateHeld || h == nil || h.Handle != "job:j-42" || h.Why != "runner tiger-runner on tiger" {
		t.Fatalf("reply = %+v holder %+v", r, h)
	}
	if !IsJobHolder(h.Handle) || IsJobHolder(alice) {
		t.Fatal("IsJobHolder does not tell a job from a pane")
	}
	// The same job taking it again is granted: a claim cut short retries.
	if _, err := tbl.ClaimForJob(key, "j-42", "again"); err != nil {
		t.Fatalf("same job again: %v", err)
	}
	if _, err := tbl.ClaimForJob(key, "j-43", "other"); err == nil {
		t.Fatal("a second job took a held checkout")
	}
	// Status names the job.
	st, err := tbl.Status(Args{Checkout: key})
	if err != nil || st.Leases[0].Holder.Handle != JobHolder("j-42") {
		t.Fatalf("status = %+v, %v", st, err)
	}
}

func TestClaimForJobNeverQueues(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	mustClaim(t, tbl, alice, Args{For: "40m"})
	if _, err := tbl.ClaimForJob(key, "j-1", "runner r"); err == nil {
		t.Fatal("job took a checkout alice holds")
	}
	st, _ := tbl.Status(Args{Checkout: key})
	if len(st.Leases[0].Queue) != 0 {
		t.Fatalf("queue = %+v, want the job not queued", st.Leases[0].Queue)
	}
	if _, err := tbl.Pause(Args{Checkout: "local:/other"}); err != nil {
		t.Fatal(err)
	}
	if _, err := tbl.ClaimForJob("local:/other", "j-1", "runner r"); err == nil {
		t.Fatal("job took a checkout the user paused")
	}
}

func TestJobHolderIsNotWatchedOrNotifiedAndReleasesToTheNextWaiter(t *testing.T) {
	c := &clock{now: t0()}
	tbl := openTable(t, t.TempDir(), c)
	if _, err := tbl.ClaimForJob(key, "j-1", "runner r"); err != nil {
		t.Fatal(err)
	}
	r := mustClaim(t, tbl, alice, Args{For: "40m"})
	if r.Result != ResultQueued || r.Waiting != nil {
		t.Fatalf("alice's claim = %+v, want queued with no notice to the job", r)
	}
	c.now = c.now.Add(3 * time.Hour)
	if w := tbl.Watched(); len(w) != 0 {
		t.Fatalf("watched = %+v, want no job holder", w)
	}
	p, err := tbl.Pause(Args{Checkout: key})
	if err != nil || p.Asked != "" {
		t.Fatalf("pause = %+v, %v; want no ask to a job", p, err)
	}
	rel, err := tbl.Release(JobHolder("j-1"), Args{Checkout: key}, func(string) bool { return false })
	if err != nil || rel.Granted != UserHolder {
		t.Fatalf("release = %+v, %v; want the user granted first", rel, err)
	}
}
