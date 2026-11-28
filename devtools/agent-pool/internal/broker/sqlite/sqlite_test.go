package sqlite_test

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/chorong-sh/chorong/pkg/sqlitedb"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/sqlite"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

func openTestStore(t *testing.T) (broker.Store, string) {
	t.Helper()
	dbPath := filepath.Join(t.TempDir(), "broker.db")
	st, err := sqlite.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlite.Open(%q) failed: %v", dbPath, err)
	}
	t.Cleanup(func() {
		if err := st.Close(); err != nil {
			t.Errorf("store.Close() failed: %v", err)
		}
	})
	return st, dbPath
}

func TestTokensCreateAuthRevoke(t *testing.T) {
	ctx := context.Background()
	st, dbPath := openTestStore(t)

	prodPlain, prodRec, err := st.CreateToken(ctx, broker.TokenRoleProducer, "ci-bot", []string{"org/repo-a"})
	if err != nil {
		t.Fatalf("CreateToken(producer) error = %v", err)
	}
	if !strings.HasPrefix(prodPlain, "ap_prod_") {
		t.Fatalf("producer token %q missing ap_prod_ prefix", prodPlain)
	}
	if prodRec.ID == "" || prodRec.Role != broker.TokenRoleProducer || prodRec.Name != "ci-bot" {
		t.Fatalf("unexpected producer record: %+v", prodRec)
	}

	workPlain, workRec, err := st.CreateToken(ctx, broker.TokenRoleWorker, "worker-1", []string{"org/repo-a", "org/repo-b"})
	if err != nil {
		t.Fatalf("CreateToken(worker) error = %v", err)
	}
	if !strings.HasPrefix(workPlain, "ap_work_") {
		t.Fatalf("worker token %q missing ap_work_ prefix", workPlain)
	}

	gotProd, err := st.AuthenticateToken(ctx, prodPlain)
	if err != nil {
		t.Fatalf("AuthenticateToken(producer) error = %v", err)
	}
	if gotProd.ID != prodRec.ID || len(gotProd.AllowedRepos) != 1 || gotProd.AllowedRepos[0] != "org/repo-a" {
		t.Fatalf("AuthenticateToken(producer) = %+v, want ID %q", gotProd, prodRec.ID)
	}

	gotWork, err := st.AuthenticateToken(ctx, workPlain)
	if err != nil {
		t.Fatalf("AuthenticateToken(worker) error = %v", err)
	}
	if gotWork.ID != workRec.ID {
		t.Fatalf("AuthenticateToken(worker) = %+v, want ID %q", gotWork, workRec.ID)
	}

	if _, err := broker.AuthenticateAndAuthorize(ctx, st, prodPlain, broker.TokenRoleProducer, "org/repo-a"); err != nil {
		t.Fatalf("AuthenticateAndAuthorize(allowed) error = %v", err)
	}
	if _, err := broker.AuthenticateAndAuthorize(ctx, st, prodPlain, broker.TokenRoleWorker, "org/repo-a"); !errors.Is(err, broker.ErrUnauthorized) {
		t.Fatalf("AuthenticateAndAuthorize(wrong role) error = %v, want ErrUnauthorized", err)
	}
	if _, err := broker.AuthenticateAndAuthorize(ctx, st, prodPlain, broker.TokenRoleProducer, "org/repo-b"); !errors.Is(err, broker.ErrUnauthorized) {
		t.Fatalf("AuthenticateAndAuthorize(disallowed repo) error = %v, want ErrUnauthorized", err)
	}

	if _, err := st.AuthenticateToken(ctx, "ap_prod_wrong_token"); !errors.Is(err, broker.ErrUnauthorized) {
		t.Fatalf("AuthenticateToken(wrong) error = %v, want ErrUnauthorized", err)
	}

	if err := st.RevokeToken(ctx, prodRec.ID); err != nil {
		t.Fatalf("RevokeToken error = %v", err)
	}
	if _, err := st.AuthenticateToken(ctx, prodPlain); !errors.Is(err, broker.ErrUnauthorized) {
		t.Fatalf("AuthenticateToken(revoked) error = %v, want ErrUnauthorized", err)
	}
	if err := st.RevokeToken(ctx, "nonexistent"); !errors.Is(err, broker.ErrNotFound) {
		t.Fatalf("RevokeToken(nonexistent) error = %v, want ErrNotFound", err)
	}

	listed, err := st.ListTokens(ctx)
	if err != nil {
		t.Fatalf("ListTokens error = %v", err)
	}
	if len(listed) != 2 {
		t.Fatalf("ListTokens len = %d, want 2", len(listed))
	}

	// Verify plaintext tokens are never stored in the SQLite tokens table.
	rawDB, err := sqlitedb.Open(dbPath)
	if err != nil {
		t.Fatalf("sqlitedb.Open error = %v", err)
	}
	defer rawDB.Close()

	rows, err := rawDB.QueryContext(ctx, `SELECT id, role, name, allowed_repos_json, hex(salt), hex(token_hash) FROM tokens`)
	if err != nil {
		t.Fatalf("query tokens table: %v", err)
	}
	defer rows.Close()
	for rows.Next() {
		var id, role, name, repos, saltHex, hashHex string
		if err := rows.Scan(&id, &role, &name, &repos, &saltHex, &hashHex); err != nil {
			t.Fatalf("scan token row: %v", err)
		}
		joined := id + role + name + repos + saltHex + hashHex
		if strings.Contains(joined, prodPlain) || strings.Contains(joined, workPlain) {
			t.Fatalf("plaintext token leaked in tokens table row: %s", joined)
		}
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("iterate tokens rows: %v", err)
	}
}

func TestCatalogUpsertAndGet(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)

	if _, err := st.GetCatalog(ctx, "org/missing"); !errors.Is(err, broker.ErrNotFound) {
		t.Fatalf("GetCatalog(missing) error = %v, want ErrNotFound", err)
	}

	t1 := time.Date(2026, 5, 1, 10, 0, 0, 0, time.UTC)
	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/zeta",
		DefaultBranch: "main",
		CatalogSha:    "sha-1",
		CatalogJSON:   []byte(`{"skills":["a"]}`),
		UpdatedAt:     t1,
	}); err != nil {
		t.Fatalf("UpsertCatalog(zeta) error = %v", err)
	}
	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/alpha",
		DefaultBranch: "main",
		CatalogSha:    "sha-a1",
		CatalogJSON:   []byte(`{"skills":["b"]}`),
		UpdatedAt:     t1,
	}); err != nil {
		t.Fatalf("UpsertCatalog(alpha) error = %v", err)
	}

	t2 := t1.Add(time.Minute)
	if err := st.UpsertCatalog(ctx, broker.CatalogRecord{
		Repo:          "org/zeta",
		DefaultBranch: "main",
		CatalogSha:    "sha-2",
		CatalogJSON:   []byte(`{"skills":["a","c"]}`),
		UpdatedAt:     t2,
	}); err != nil {
		t.Fatalf("UpsertCatalog(zeta update) error = %v", err)
	}

	got, err := st.GetCatalog(ctx, "org/zeta")
	if err != nil {
		t.Fatalf("GetCatalog(zeta) error = %v", err)
	}
	if got.CatalogSha != "sha-2" || string(got.CatalogJSON) != `{"skills":["a","c"]}` {
		t.Fatalf("GetCatalog(zeta) = %+v", got)
	}

	list, err := st.ListCatalogs(ctx)
	if err != nil {
		t.Fatalf("ListCatalogs error = %v", err)
	}
	if len(list) != 2 || list[0].Repo != "org/alpha" || list[1].Repo != "org/zeta" {
		t.Fatalf("ListCatalogs order = %+v, want [org/alpha, org/zeta]", list)
	}
}

func TestWorkerHeartbeatAndActiveFilter(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)

	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)
	rem := 75.5
	if err := st.UpsertWorkerHeartbeat(ctx, broker.WorkerRecord{
		WorkerID:             "w-fresh",
		Alias:                "alice-mac",
		SupportedRepos:       []string{"org/repo"},
		SupportedSkills:      map[string][]string{"org/repo": {"review-pr"}},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 2,
		Quota: protocol.QuotaHealth{
			State:                "healthy",
			FiveHourRemainingPct: &rem,
		},
		LastHeartbeat: now.Add(-10 * time.Second),
	}); err != nil {
		t.Fatalf("UpsertWorkerHeartbeat(fresh) error = %v", err)
	}
	if err := st.UpsertWorkerHeartbeat(ctx, broker.WorkerRecord{
		WorkerID:             "w-stale",
		Alias:                "bob-linux",
		SupportedRepos:       []string{"org/repo"},
		AcceptingJobs:        true,
		ConcurrencyAvailable: 1,
		Quota:                protocol.QuotaHealth{State: "healthy"},
		LastHeartbeat:        now.Add(-2 * time.Minute),
	}); err != nil {
		t.Fatalf("UpsertWorkerHeartbeat(stale) error = %v", err)
	}

	active, err := st.ListActiveWorkers(ctx, 30*time.Second, now)
	if err != nil {
		t.Fatalf("ListActiveWorkers error = %v", err)
	}
	if len(active) != 1 || active[0].WorkerID != "w-fresh" {
		t.Fatalf("ListActiveWorkers = %+v, want only w-fresh", active)
	}
	if active[0].Quota.FiveHourRemainingPct == nil || *active[0].Quota.FiveHourRemainingPct != rem {
		t.Fatalf("Quota round-trip mismatch: %+v", active[0].Quota)
	}
}

func TestEnqueueIdempotencyAndConcurrencyCap(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	j1, dedup, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID:    "req-1",
		IdempotencyKey: "key-1",
		Repo:           "org/repo",
		DefaultBranch:  "main",
		CatalogSha:     "sha-1",
		Skill:          "review-pr",
		ArgsJSON:       []byte(`{"pr":1}`),
		MaxAttempts:    2,
		CreatedAt:      now,
		UpdatedAt:      now,
	}, 2)
	if err != nil {
		t.Fatalf("EnqueueJob(1) error = %v", err)
	}
	if dedup {
		t.Fatalf("EnqueueJob(1) deduplicated = true, want false")
	}

	// Duplicate (requesterID, idempotencyKey) returns existing job.
	j1Dup, dedup, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID:    "req-1",
		IdempotencyKey: "key-1",
		Repo:           "org/repo",
		DefaultBranch:  "main",
		CatalogSha:     "sha-1",
		Skill:          "review-pr",
		MaxAttempts:    2,
		CreatedAt:      now.Add(time.Second),
		UpdatedAt:      now.Add(time.Second),
	}, 2)
	if err != nil {
		t.Fatalf("EnqueueJob(dup) error = %v", err)
	}
	if !dedup || j1Dup.JobID != j1.JobID {
		t.Fatalf("EnqueueJob(dup) = (%+v, %v), want job %q with deduplicated=true", j1Dup, dedup, j1.JobID)
	}

	// Enqueue second active job (hits cap of 2).
	j2, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 1,
		CreatedAt:   now.Add(2 * time.Second),
		UpdatedAt:   now.Add(2 * time.Second),
	}, 2)
	if err != nil {
		t.Fatalf("EnqueueJob(2) error = %v", err)
	}

	// Third concurrent job for req-1 fails with ErrRateLimited.
	if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 1,
		CreatedAt:   now.Add(3 * time.Second),
		UpdatedAt:   now.Add(3 * time.Second),
	}, 2); !errors.Is(err, broker.ErrRateLimited) {
		t.Fatalf("EnqueueJob(3) error = %v, want ErrRateLimited", err)
	}

	// Cancel j2 (terminal state) so req-1 drops below cap and can enqueue again.
	if _, err := st.RequestCancel(ctx, j2.JobID, "req-1", now.Add(4*time.Second)); err != nil {
		t.Fatalf("RequestCancel(j2) error = %v", err)
	}
	if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 1,
		CreatedAt:   now.Add(5 * time.Second),
		UpdatedAt:   now.Add(5 * time.Second),
	}, 2); err != nil {
		t.Fatalf("EnqueueJob(after terminal) error = %v", err)
	}

	pending, err := st.CountPendingJobs(ctx, []string{"org/repo"})
	if err != nil {
		t.Fatalf("CountPendingJobs error = %v", err)
	}
	if pending != 2 {
		t.Fatalf("CountPendingJobs = %d, want 2", pending)
	}
}

func TestClaimLeaseConcurrency(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	const numJobs = 3
	for i := range numJobs {
		if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
			RequesterID: "req-1",
			Repo:        "org/repo",
			Skill:       "review-pr",
			MaxAttempts: 2,
			CreatedAt:   now.Add(time.Duration(i) * time.Second),
			UpdatedAt:   now.Add(time.Duration(i) * time.Second),
		}, 10); err != nil {
			t.Fatalf("EnqueueJob(%d) error = %v", i, err)
		}
	}

	const workers = 8
	var (
		wg      sync.WaitGroup
		mu      sync.Mutex
		claimed = make(map[string]int)
		errs    []error
	)

	for i := range workers {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()
			job, err := st.ClaimLease(ctx, fmt.Sprintf("w-%d", idx), "alias", []string{"org/repo"}, nil, time.Minute, now)
			mu.Lock()
			defer mu.Unlock()
			if err != nil {
				if !errors.Is(err, broker.ErrNoJobAvailable) {
					errs = append(errs, err)
				}
				return
			}
			claimed[job.JobID]++
		}(i)
	}
	wg.Wait()

	if len(errs) > 0 {
		t.Fatalf("unexpected ClaimLease errors: %v", errs)
	}
	if len(claimed) != numJobs {
		t.Fatalf("claimed %d unique jobs, want %d", len(claimed), numJobs)
	}
	for id, count := range claimed {
		if count != 1 {
			t.Errorf("job %q claimed %d times, want 1", id, count)
		}
	}
}

func TestClaimLeaseSkillFilter(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "fix-bug",
		MaxAttempts: 1,
		CreatedAt:   now,
		UpdatedAt:   now,
	}, 10); err != nil {
		t.Fatalf("EnqueueJob(fix-bug) error = %v", err)
	}
	wantJob, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 1,
		CreatedAt:   now.Add(time.Second),
		UpdatedAt:   now.Add(time.Second),
	}, 10)
	if err != nil {
		t.Fatalf("EnqueueJob(review-pr) error = %v", err)
	}

	got, err := st.ClaimLease(ctx, "w-1", "alice", []string{"org/repo"}, map[string][]string{
		"org/repo": {"review-pr"},
	}, time.Minute, now.Add(2*time.Second))
	if err != nil {
		t.Fatalf("ClaimLease error = %v", err)
	}
	if got.JobID != wantJob.JobID || got.Skill != "review-pr" {
		t.Fatalf("ClaimLease = %+v, want review-pr job %q", got, wantJob.JobID)
	}

	// Next claim with same skill restriction should find no job.
	if _, err := st.ClaimLease(ctx, "w-1", "alice", []string{"org/repo"}, map[string][]string{
		"org/repo": {"review-pr"},
	}, time.Minute, now.Add(3*time.Second)); !errors.Is(err, broker.ErrNoJobAvailable) {
		t.Fatalf("second ClaimLease error = %v, want ErrNoJobAvailable", err)
	}
}

func TestRenewLeaseAndCancel(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	queuedJob, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 2,
		CreatedAt:   now,
		UpdatedAt:   now,
	}, 10)
	if err != nil {
		t.Fatalf("EnqueueJob(queued) error = %v", err)
	}

	// Wrong requester cannot cancel.
	if _, err := st.RequestCancel(ctx, queuedJob.JobID, "other-req", now.Add(time.Second)); !errors.Is(err, broker.ErrUnauthorized) {
		t.Fatalf("RequestCancel(wrong requester) error = %v, want ErrUnauthorized", err)
	}

	// RequestCancel on QUEUED immediately transitions to CANCELED.
	canceledQueued, err := st.RequestCancel(ctx, queuedJob.JobID, "req-1", now.Add(time.Second))
	if err != nil {
		t.Fatalf("RequestCancel(queued) error = %v", err)
	}
	if canceledQueued.Status != protocol.JobStatusCanceled {
		t.Fatalf("queued cancel status = %q, want CANCELED", canceledQueued.Status)
	}

	// Enqueue and lease a second job.
	if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 2,
		CreatedAt:   now.Add(2 * time.Second),
		UpdatedAt:   now.Add(2 * time.Second),
	}, 10); err != nil {
		t.Fatalf("EnqueueJob(2) error = %v", err)
	}
	leased, err := st.ClaimLease(ctx, "w-1", "alice", []string{"org/repo"}, nil, time.Minute, now.Add(3*time.Second))
	if err != nil {
		t.Fatalf("ClaimLease error = %v", err)
	}
	if leased.Status != protocol.JobStatusLeased {
		t.Fatalf("ClaimLease status = %q, want LEASED", leased.Status)
	}

	// RenewLease transitions LEASED -> RUNNING and extends expiry.
	renewTime := now.Add(20 * time.Second)
	exp, cancelReq, err := st.RenewLease(ctx, leased.JobID, leased.LeaseToken, protocol.JobProgress{
		TurnsUsed:      3,
		EffectiveUnits: 1200,
		ElapsedMs:      17000,
	}, time.Minute, renewTime)
	if err != nil {
		t.Fatalf("RenewLease(1) error = %v", err)
	}
	if cancelReq {
		t.Fatalf("RenewLease(1) cancelRequested = true, want false")
	}
	if !exp.Equal(renewTime.Add(time.Minute)) {
		t.Fatalf("RenewLease(1) expiresAt = %v, want %v", exp, renewTime.Add(time.Minute))
	}

	running, err := st.GetJob(ctx, leased.JobID)
	if err != nil {
		t.Fatalf("GetJob error = %v", err)
	}
	if running.Status != protocol.JobStatusRunning || running.Progress == nil || running.Progress.TurnsUsed != 3 {
		t.Fatalf("unexpected running job state: %+v", running)
	}

	// RequestCancel on RUNNING sets cancelRequested=true, returned on next RenewLease.
	if _, err := st.RequestCancel(ctx, leased.JobID, "req-1", now.Add(30*time.Second)); err != nil {
		t.Fatalf("RequestCancel(running) error = %v", err)
	}
	_, cancelReq, err = st.RenewLease(ctx, leased.JobID, leased.LeaseToken, protocol.JobProgress{
		TurnsUsed: 4,
	}, time.Minute, now.Add(35*time.Second))
	if err != nil {
		t.Fatalf("RenewLease(2) error = %v", err)
	}
	if !cancelReq {
		t.Fatalf("RenewLease(2) cancelRequested = false, want true")
	}
}

func TestSweepExpiredLeases(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	j, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 2,
		CreatedAt:   now,
		UpdatedAt:   now,
	}, 10)
	if err != nil {
		t.Fatalf("EnqueueJob error = %v", err)
	}

	// Attempt 1 expires -> requeued.
	if _, err := st.ClaimLease(ctx, "w-1", "alice", []string{"org/repo"}, nil, time.Minute, now); err != nil {
		t.Fatalf("ClaimLease(1) error = %v", err)
	}
	requeued, timedOut, err := st.SweepExpiredLeases(ctx, now.Add(time.Minute))
	if err != nil {
		t.Fatalf("SweepExpiredLeases(1) error = %v", err)
	}
	if requeued != 1 || timedOut != 0 {
		t.Fatalf("SweepExpiredLeases(1) = (%d, %d), want (1, 0)", requeued, timedOut)
	}

	afterRequeue, err := st.GetJob(ctx, j.JobID)
	if err != nil {
		t.Fatalf("GetJob(afterRequeue) error = %v", err)
	}
	if afterRequeue.Status != protocol.JobStatusQueued || afterRequeue.LeaseToken != "" {
		t.Fatalf("unexpected job after requeue: %+v", afterRequeue)
	}

	// Attempt 2 (reaches maxAttempts=2) expires -> TIMED_OUT.
	claimed2, err := st.ClaimLease(ctx, "w-2", "bob", []string{"org/repo"}, nil, time.Minute, now.Add(2*time.Minute))
	if err != nil {
		t.Fatalf("ClaimLease(2) error = %v", err)
	}
	if claimed2.Attempts != 2 {
		t.Fatalf("ClaimLease(2) attempts = %d, want 2", claimed2.Attempts)
	}

	requeued, timedOut, err = st.SweepExpiredLeases(ctx, now.Add(3*time.Minute))
	if err != nil {
		t.Fatalf("SweepExpiredLeases(2) error = %v", err)
	}
	if requeued != 0 || timedOut != 1 {
		t.Fatalf("SweepExpiredLeases(2) = (%d, %d), want (0, 1)", requeued, timedOut)
	}

	afterTimeout, err := st.GetJob(ctx, j.JobID)
	if err != nil {
		t.Fatalf("GetJob(afterTimeout) error = %v", err)
	}
	if afterTimeout.Status != protocol.JobStatusTimedOut {
		t.Fatalf("job status after timeout = %q, want TIMED_OUT", afterTimeout.Status)
	}
}

func TestCompleteJobLeaseMismatch(t *testing.T) {
	ctx := context.Background()
	st, _ := openTestStore(t)
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)

	if _, _, err := st.EnqueueJob(ctx, broker.JobRecord{
		RequesterID: "req-1",
		Repo:        "org/repo",
		Skill:       "review-pr",
		MaxAttempts: 2,
		CreatedAt:   now,
		UpdatedAt:   now,
	}, 10); err != nil {
		t.Fatalf("EnqueueJob error = %v", err)
	}
	leased, err := st.ClaimLease(ctx, "w-1", "alice", []string{"org/repo"}, nil, time.Minute, now)
	if err != nil {
		t.Fatalf("ClaimLease error = %v", err)
	}

	// Wrong lease token rejected.
	if _, err := st.CompleteJob(
		ctx, leased.JobID, "wrong-token", protocol.JobStatusSucceeded,
		[]byte(`{"ok":true}`), nil, "", "", now.Add(10*time.Second),
	); !errors.Is(err, broker.ErrLeaseMismatch) {
		t.Fatalf("CompleteJob(wrong token) error = %v, want ErrLeaseMismatch", err)
	}

	// Valid completion succeeds and clears lease token.
	usage := &protocol.UsageReport{
		Provider:       "anthropic",
		Model:          "claude-sonnet",
		EffectiveUnits: 500,
		TurnsUsed:      2,
		DurationMs:     4500,
	}
	completed, err := st.CompleteJob(
		ctx, leased.JobID, leased.LeaseToken, protocol.JobStatusSucceeded,
		[]byte(`{"ok":true}`), usage, "", "", now.Add(15*time.Second),
	)
	if err != nil {
		t.Fatalf("CompleteJob(valid) error = %v", err)
	}
	if completed.Status != protocol.JobStatusSucceeded || completed.LeaseToken != "" {
		t.Fatalf("unexpected completed job: %+v", completed)
	}
	if completed.UsageReport == nil || completed.UsageReport.EffectiveUnits != 500 {
		t.Fatalf("unexpected usage report: %+v", completed.UsageReport)
	}

	// Stale lease token after completion is rejected with ErrLeaseMismatch.
	if _, err := st.CompleteJob(
		ctx, leased.JobID, leased.LeaseToken, protocol.JobStatusFailed,
		nil, nil, protocol.ErrCodeInternal, "boom", now.Add(20*time.Second),
	); !errors.Is(err, broker.ErrLeaseMismatch) {
		t.Fatalf("CompleteJob(stale token) error = %v, want ErrLeaseMismatch", err)
	}
}
