package protocol

import (
	"encoding/json"
	"reflect"
	"testing"
	"time"
)

func TestJobStatusValidAndTerminal(t *testing.T) {
	tests := []struct {
		status   JobStatus
		valid    bool
		terminal bool
	}{
		{status: JobStatusQueued, valid: true, terminal: false},
		{status: JobStatusLeased, valid: true, terminal: false},
		{status: JobStatusRunning, valid: true, terminal: false},
		{status: JobStatusSucceeded, valid: true, terminal: true},
		{status: JobStatusFailed, valid: true, terminal: true},
		{status: JobStatusBudgetExceeded, valid: true, terminal: true},
		{status: JobStatusDLPBlocked, valid: true, terminal: true},
		{status: JobStatusTimedOut, valid: true, terminal: true},
		{status: JobStatusCanceled, valid: true, terminal: true},
		{status: JobStatus("BOGUS"), valid: false, terminal: false},
		{status: JobStatus(""), valid: false, terminal: false},
	}

	for _, tc := range tests {
		t.Run(string(tc.status), func(t *testing.T) {
			if got := tc.status.IsValid(); got != tc.valid {
				t.Errorf("JobStatus(%q).IsValid() = %v, want %v", tc.status, got, tc.valid)
			}
			if got := tc.status.IsTerminal(); got != tc.terminal {
				t.Errorf("JobStatus(%q).IsTerminal() = %v, want %v", tc.status, got, tc.terminal)
			}
		})
	}
}

func TestCanTransitionMatrix(t *testing.T) {
	allStatuses := []JobStatus{
		JobStatusQueued,
		JobStatusLeased,
		JobStatusRunning,
		JobStatusSucceeded,
		JobStatusFailed,
		JobStatusBudgetExceeded,
		JobStatusDLPBlocked,
		JobStatusTimedOut,
		JobStatusCanceled,
		JobStatus("BOGUS"),
	}

	allowed := map[JobStatus]map[JobStatus]bool{
		JobStatusQueued: {
			JobStatusLeased:   true,
			JobStatusCanceled: true,
		},
		JobStatusLeased: {
			JobStatusRunning:        true,
			JobStatusQueued:         true,
			JobStatusTimedOut:       true,
			JobStatusCanceled:       true,
			JobStatusFailed:         true,
			JobStatusDLPBlocked:     true,
			JobStatusBudgetExceeded: true,
			JobStatusSucceeded:      true,
		},
		JobStatusRunning: {
			JobStatusQueued:         true,
			JobStatusSucceeded:      true,
			JobStatusFailed:         true,
			JobStatusBudgetExceeded: true,
			JobStatusDLPBlocked:     true,
			JobStatusTimedOut:       true,
			JobStatusCanceled:       true,
		},
	}

	for _, from := range allStatuses {
		for _, to := range allStatuses {
			want := allowed[from][to]
			if got := CanTransition(from, to); got != want {
				t.Errorf("CanTransition(%q, %q) = %v, want %v", from, to, got, want)
			}
		}
	}
}

func TestAPIErrorError(t *testing.T) {
	tests := []struct {
		name string
		err  *APIError
		want string
	}{
		{
			name: "code and message",
			err:  &APIError{Code: ErrCodeNoCapacity, Message: "no idle workers"},
			want: "NO_CAPACITY: no idle workers",
		},
		{
			name: "code only",
			err:  &APIError{Code: ErrCodeNotFound},
			want: "NOT_FOUND",
		},
		{
			name: "message only",
			err:  &APIError{Message: "something failed"},
			want: "something failed",
		},
		{
			name: "nil error",
			err:  nil,
			want: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.err.Error(); got != tc.want {
				t.Errorf("APIError.Error() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTokenUsageEffectiveUnits(t *testing.T) {
	tests := []struct {
		name  string
		usage TokenUsage
		want  int64
	}{
		{
			name:  "zero",
			usage: TokenUsage{},
			want:  0,
		},
		{
			name: "small values",
			usage: TokenUsage{
				Input:         10,
				CacheRead:     20,
				CacheCreation: 4,
				Output:        2,
			},
			want: 27, // 10 + 1.25*4 + 0.1*20 + 5*2 = 10 + 5 + 2 + 10
		},
		{
			name: "fractional rounding cacheRead 5",
			usage: TokenUsage{
				CacheRead: 5,
			},
			want: 1, // 0.1*5 = 0.5 -> 1
		},
		{
			name: "fractional rounding cacheCreation 3",
			usage: TokenUsage{
				CacheCreation: 3,
			},
			want: 4, // 1.25*3 = 3.75 -> 4
		},
		{
			name: "fractional rounding cacheRead 5 and cacheCreation 3",
			usage: TokenUsage{
				CacheRead:     5,
				CacheCreation: 3,
			},
			want: 4, // 0.5 + 3.75 = 4.25 -> 4
		},
		{
			name: "realistic claude token counts",
			usage: TokenUsage{
				Input:         12000,
				CacheRead:     85000,
				CacheCreation: 15000,
				Output:        3500,
			},
			want: 56750, // 12000 + 18750 + 8500 + 17500
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.usage.EffectiveUnits(); got != tc.want {
				t.Errorf("TokenUsage(%+v).EffectiveUnits() = %d, want %d", tc.usage, got, tc.want)
			}
		})
	}
}

func assertJSONKeys(t *testing.T, raw []byte, wantKeys []string) {
	t.Helper()
	var m map[string]json.RawMessage
	if err := json.Unmarshal(raw, &m); err != nil {
		t.Fatalf("json.Unmarshal into map failed: %v", err)
	}
	if len(m) != len(wantKeys) {
		t.Errorf("JSON object has %d keys (%v), want %d (%v)", len(m), mapKeys(m), len(wantKeys), wantKeys)
	}
	for _, k := range wantKeys {
		if _, ok := m[k]; !ok {
			t.Errorf("missing expected JSON key %q in %s", k, string(raw))
		}
	}
}

func mapKeys(m map[string]json.RawMessage) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys
}

func TestDTOJSONRoundTrip(t *testing.T) {
	now := time.Date(2026, 11, 24, 20, 0, 0, 0, time.UTC)
	later := now.Add(5 * time.Hour)
	weekLater := now.Add(7 * 24 * time.Hour)
	fiveHrPct := 82.5
	sevenDayPct := 64.0

	t.Run("PoolStatusResponse", func(t *testing.T) {
		in := PoolStatusResponse{
			ActiveWorkers:  3,
			PoolHealth:     "healthy",
			QueueDepth:     2,
			SupportedRepos: []string{"org/repo-a"},
			AvailableSkills: []SkillSummary{
				{
					Repo:          "org/repo-a",
					DefaultBranch: "main",
					CatalogSha:    "abc123",
					Skill:         "review",
					Description:   "Review code",
					ArgsSchema:    json.RawMessage(`{"type":"object"}`),
					OutputSchema:  json.RawMessage(`{"type":"object"}`),
				},
			},
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{
			"activeWorkers", "poolHealth", "queueDepth", "supportedRepos", "availableSkills",
		})
		var out PoolStatusResponse
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("DispatchRequest", func(t *testing.T) {
		in := DispatchRequest{
			Repo:           "org/repo-a",
			Skill:          "review",
			Args:           json.RawMessage(`{"pr":42}`),
			IdempotencyKey: "idem-1",
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{"repo", "skill", "args", "idempotencyKey"})
		var out DispatchRequest
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("JobResultResponse", func(t *testing.T) {
		in := JobResultResponse{
			JobID:       "job-1",
			Repo:        "org/repo-a",
			Skill:       "review",
			CatalogSha:  "abc123",
			Status:      JobStatusSucceeded,
			WorkerAlias: "worker-1",
			Deliverable: json.RawMessage(`{"summary":"ok"}`),
			UsageReport: &UsageReport{
				Provider: "anthropic",
				Model:    "claude-sonnet-4-5",
				Tokens: TokenUsage{
					Input:         100,
					CacheRead:     200,
					CacheCreation: 50,
					Output:        40,
				},
				EffectiveUnits: 383,
				TurnsUsed:      3,
				DurationMs:     4500,
			},
			Progress: &JobProgress{
				TurnsUsed:      3,
				EffectiveUnits: 383,
				ElapsedMs:      4500,
			},
			ErrorCode:    ErrCodeDLPBlocked,
			ErrorMessage: "redacted",
			CreatedAt:    now,
			UpdatedAt:    later,
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{
			"jobId", "repo", "skill", "catalogSha", "status", "workerAlias",
			"deliverable", "usageReport", "progress", "errorCode", "errorMessage",
			"createdAt", "updatedAt",
		})
		var out JobResultResponse
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("WorkerHeartbeatRequest", func(t *testing.T) {
		in := WorkerHeartbeatRequest{
			SupportedRepos:       []string{"org/repo-a"},
			SupportedSkills:      map[string][]string{"org/repo-a": {"review"}},
			AcceptingJobs:        true,
			ConcurrencyAvailable: 2,
			Quota: QuotaHealth{
				State:                "green",
				FiveHourRemainingPct: &fiveHrPct,
				SevenDayRemainingPct: &sevenDayPct,
				FiveHourResetsAt:     &later,
				SevenDayResetsAt:     &weekLater,
			},
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{
			"supportedRepos", "supportedSkills", "acceptingJobs", "concurrencyAvailable", "quota",
		})
		var out WorkerHeartbeatRequest
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("LeaseResponse", func(t *testing.T) {
		in := LeaseResponse{
			JobID:           "job-1",
			Repo:            "org/repo-a",
			DefaultBranch:   "main",
			CatalogSha:      "abc123",
			Skill:           "review",
			Args:            json.RawMessage(`{"pr":42}`),
			LeaseToken:      "lease-tok-1",
			LeaseTTLSeconds: 60,
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{
			"jobId", "repo", "defaultBranch", "catalogSha", "skill",
			"args", "leaseToken", "leaseTtlSeconds",
		})
		var out LeaseResponse
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("JobHeartbeatRequest", func(t *testing.T) {
		in := JobHeartbeatRequest{
			LeaseToken: "lease-tok-1",
			Progress: JobProgress{
				TurnsUsed:      2,
				EffectiveUnits: 1200,
				ElapsedMs:      3000,
			},
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{"leaseToken", "progress"})
		var out JobHeartbeatRequest
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})

	t.Run("CompleteJobRequest", func(t *testing.T) {
		in := CompleteJobRequest{
			LeaseToken:  "lease-tok-1",
			Status:      JobStatusSucceeded,
			Deliverable: json.RawMessage(`{"ok":true}`),
			DlpChecked:  true,
			UsageReport: &UsageReport{
				Provider:       "anthropic",
				Model:          "claude-sonnet-4-5",
				Tokens:         TokenUsage{Input: 10, Output: 5},
				EffectiveUnits: 35,
				TurnsUsed:      1,
				DurationMs:     900,
			},
			ErrorCode:    ErrCodeOutputInvalid,
			ErrorMessage: "schema mismatch",
		}
		data, err := json.Marshal(in)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		assertJSONKeys(t, data, []string{
			"leaseToken", "status", "deliverable", "dlpChecked",
			"usageReport", "errorCode", "errorMessage",
		})
		var out CompleteJobRequest
		if err := json.Unmarshal(data, &out); err != nil {
			t.Fatalf("Unmarshal failed: %v", err)
		}
		if !reflect.DeepEqual(out, in) {
			t.Errorf("round-trip mismatch:\ngot:  %+v\nwant: %+v", out, in)
		}
	})
}
