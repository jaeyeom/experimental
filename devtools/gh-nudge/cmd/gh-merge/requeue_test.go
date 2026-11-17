package main

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func TestNormalizeFailureLine(t *testing.T) {
	tests := []struct {
		name string
		line string
		want string
	}{
		{name: "digits", line: "ERROR: 3 of 12 tests failed", want: "ERROR: <n> of <n> tests failed"},
		{name: "rfc3339 timestamp", line: "at 2026-01-02T03:04:05.123Z failed", want: "at <ts> failed"},
		{name: "clock prefix", line: "(12:34:56) ERROR: build failed", want: "(<ts>) ERROR: build failed"},
		{name: "hex", line: "commit abc123def456 and 0x1F2E", want: "commit <hex> and <hex>"},
		{name: "plain word that looks hex", line: "deadbeef accede", want: "deadbeef accede"},
		{name: "absolute path", line: "ERROR: /home/runner/work/x/y.go:12:3: boom", want: "ERROR: <path>:<n>:<n>: boom"},
		{name: "relative and home paths", line: "see ./out/log and ~/cache/z", want: "see <path> and <path>"},
		{name: "bazel target kept", line: "ERROR: //pkg/parse:parse_test failed", want: "ERROR: //pkg/parse:parse_test failed"},
		{name: "url kept apart from paths", line: "see https://ci.example/job/77", want: "see https://ci.example/job/<n>"},
		{name: "whitespace", line: "  ERROR:\tboom   now ", want: "ERROR: boom now"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := normalizeFailureLine(tc.line); got != tc.want {
				t.Fatalf("normalizeFailureLine(%q) = %q, want %q", tc.line, got, tc.want)
			}
		})
	}
}

func TestFailureSignature(t *testing.T) {
	tests := []struct {
		name  string
		found eventEvidence
		want  string
	}{
		{
			name: "job, test and first failure line",
			found: eventEvidence{
				evidence: github.FailureEvidence{JobName: "test"},
				report:   failureReport{Log: "compiling\nERROR: took 12s\nERROR: second\n", Tests: []string{"pkg.TestA", "pkg.TestB"}},
			},
			want: "job=test; test=pkg.TestA; line=ERROR: took <n>s",
		},
		{
			name: "report job name wins and check name is a fallback",
			found: eventEvidence{
				evidence: github.FailureEvidence{Name: "ci", JobName: "other"},
				report:   failureReport{JobName: "test-amd64", Log: "ERROR: boom"},
			},
			want: "job=test-amd64; line=ERROR: boom",
		},
		{
			name: "check name when no job name",
			found: eventEvidence{
				evidence: github.FailureEvidence{Name: "ci"},
				report:   failureReport{Log: "ERROR: boom"},
			},
			want: "job=ci; line=ERROR: boom",
		},
		{
			name: "no failure line and no test is unknown",
			found: eventEvidence{
				evidence: github.FailureEvidence{JobName: "test"},
				report:   failureReport{Log: "compiling\nexit 1\n"},
			},
			want: "",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := failureSignature(queueConfig(), tc.found)
			if err != nil {
				t.Fatalf("failureSignature() error = %v", err)
			}
			if got != tc.want {
				t.Fatalf("failureSignature() = %q, want %q", got, tc.want)
			}
		})
	}
}

// requeuePR has an earlier flake removal R1 on commit s1 and a latest flake
// removal EVT1 on commit s2.
func requeuePR() github.QueuePullRequest {
	pr := readyPR()
	base := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	pr.Events = []github.QueueEvent{
		{ID: "A0", Kind: github.QueueEventAdded, CreatedAt: base.Add(-3 * time.Minute)},
		{ID: "R1", Kind: github.QueueEventRemoved, CreatedAt: base.Add(-2 * time.Minute), Reason: "failed_checks", CommitSHA: "s1"},
		{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: base.Add(-1 * time.Minute)},
		{ID: "EVT1", Kind: github.QueueEventRemoved, CreatedAt: base, Reason: "failed_checks", CommitSHA: "s2"},
	}
	pr.Comments = []string{"RCA R1 class=flake", "RCA EVT1 class=flake"}
	return pr
}

func actionsEvidence(log string) github.FailureEvidence {
	return github.FailureEvidence{
		Name:      "ci",
		TargetURL: "https://github.com/acme/widgets/actions/runs/1/job/2",
		JobName:   "test",
		Log:       log,
	}
}

func sameFailureClient(pr github.QueuePullRequest) *fakeQueue {
	return &fakeQueue{pr: pr, evidenceBySHA: map[string]github.FailureEvidence{
		"s1": actionsEvidence("setup\nERROR: /home/runner/work/a/x.go:12: TestParse failed at 2026-01-01T10:00:00Z (run 0x1f2e3d)\n"),
		"s2": actionsEvidence("setup\nERROR: /tmp/b/x.go:12: TestParse failed at 2026-01-02T11:22:33Z (run 0xa9b8c7)\n"),
	}}
}

const sameFailureSignature = "job=test; line=ERROR: <path>:<n>: TestParse failed at <ts> (run <hex>)"

func TestQueueRefusesIdenticalFailureSignature(t *testing.T) {
	pr := requeuePR()
	client := sameFailureClient(pr)
	var err error
	captureStdout(t, func() {
		err = queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
	})
	want := "refusing to enqueue: same failure as the previous ejection (" + sameFailureSignature + "); likely our-code"
	if err == nil || err.Error() != want {
		t.Fatalf("queuePullRequest() error = %v, want %q", err, want)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d, want 0", client.enqueueCalls)
	}
}

func TestQueueSignatureComparesWithPreviousFlakeOrInfraRemoval(t *testing.T) {
	t.Run("previous removal without a flake or infra RCA is skipped", func(t *testing.T) {
		pr := requeuePR()
		pr.Comments = []string{"RCA R1 class=manual", "RCA EVT1 class=flake"}
		client := sameFailureClient(pr)
		captureStdout(t, func() {
			if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		if client.enqueueCalls != 1 {
			t.Fatalf("enqueueCalls = %d, want 1", client.enqueueCalls)
		}
	})
	t.Run("manual latest removal is not compared", func(t *testing.T) {
		pr := requeuePR()
		pr.Comments = []string{"RCA R1 class=infra", "RCA EVT1 class=manual"}
		client := sameFailureClient(pr)
		captureStdout(t, func() {
			if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
				t.Fatalf("queuePullRequest() error = %v", err)
			}
		})
		if client.enqueueCalls != 1 {
			t.Fatalf("enqueueCalls = %d, want 1", client.enqueueCalls)
		}
	})
}

func TestQueueDifferentFailuresRequeueUpToTheCap(t *testing.T) {
	base := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	events := func(removals int) []github.QueueEvent {
		var out []github.QueueEvent
		at := base
		for i := range removals {
			sha := string(rune('a' + i))
			out = append(out,
				github.QueueEvent{ID: "A" + sha, Kind: github.QueueEventAdded, CreatedAt: at},
				github.QueueEvent{ID: "R" + sha, Kind: github.QueueEventRemoved, CreatedAt: at.Add(time.Minute), Reason: "failed_checks", CommitSHA: sha},
			)
			at = at.Add(2 * time.Minute)
		}
		return out
	}
	run := func(t *testing.T, removals int) (*fakeQueue, error) {
		t.Helper()
		pr := readyPR()
		pr.Events = events(removals)
		bySHA := map[string]github.FailureEvidence{}
		for i := range removals {
			sha := string(rune('a' + i))
			pr.Comments = append(pr.Comments, "RCA R"+sha+" class=flake")
			bySHA[sha] = actionsEvidence("ERROR: Test" + strings.ToUpper(sha) + " failed\n")
		}
		client := &fakeQueue{pr: pr, evidenceBySHA: bySHA}
		var err error
		captureStdout(t, func() {
			err = queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
		})
		return client, err
	}
	// Three removals have two earlier requeues: at the default cap of 2.
	client, err := run(t, 3)
	if err != nil || client.enqueueCalls != 1 {
		t.Fatalf("at the cap: error = %v enqueueCalls = %d, want enqueued", err, client.enqueueCalls)
	}
	client, err = run(t, 4)
	const want = "refusing to enqueue: 3 earlier requeues is greater than max_requeues 2"
	if err == nil || err.Error() != want || client.enqueueCalls != 0 {
		t.Fatalf("over the cap: error = %v enqueueCalls = %d, want %q", err, client.enqueueCalls, want)
	}
}

func TestQueueDryRunJSONRequeueGolden(t *testing.T) {
	tests := []struct {
		name    string
		client  func() *fakeQueue
		golden  string
		result  string
		wantErr bool
	}{
		{
			name:    "refused for the same failure",
			client:  func() *fakeQueue { return sameFailureClient(requeuePR()) },
			golden:  "same_failure.json",
			result:  "refused",
			wantErr: true,
		},
		{
			name: "allowed for a different failure",
			client: func() *fakeQueue {
				client := sameFailureClient(requeuePR())
				client.evidenceBySHA["s2"] = actionsEvidence("ERROR: TestOther failed\n")
				return client
			},
			golden: "different_failure.json",
			result: "would-enqueue",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			withMergeFlags(t, true, false, false)
			jsonOutput = true
			t.Cleanup(func() { jsonOutput = false })
			client := tc.client()
			var err error
			output := captureStdout(t, func() {
				err = queuePullRequest(client, rcaConfig(), "acme/widgets#12", false)
			})
			if (err != nil) != tc.wantErr {
				t.Fatalf("queuePullRequest() error = %v, wantErr %v", err, tc.wantErr)
			}
			start := strings.Index(output, "{\n")
			if start < 0 {
				t.Fatalf("no JSON in output:\n%s", output)
			}
			var got struct {
				Result  string          `json:"result"`
				Requeue json.RawMessage `json:"requeue"`
			}
			if err := json.Unmarshal([]byte(output[start:]), &got); err != nil {
				t.Fatalf("json.Unmarshal() error = %v\n%s", err, output)
			}
			if got.Result != tc.result {
				t.Fatalf("result = %q, want %q", got.Result, tc.result)
			}
			var block bytes.Buffer
			if err := json.Indent(&block, got.Requeue, "", "  "); err != nil {
				t.Fatalf("json.Indent() error = %v", err)
			}
			block.WriteString("\n")
			assertGoldenFile(t, "testdata/requeue/"+tc.golden, block.String())
			if client.enqueueCalls != 0 {
				t.Fatalf("enqueueCalls = %d, want 0", client.enqueueCalls)
			}
		})
	}
}

func TestQueueJSONWithoutRemovalOmitsRequeue(t *testing.T) {
	withMergeFlags(t, true, false, false)
	jsonOutput = true
	t.Cleanup(func() { jsonOutput = false })
	pr := readyPR()
	output := captureStdout(t, func() {
		if err := queuePullRequest(&fakeQueue{pr: pr}, queueConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if strings.Contains(output, `"requeue"`) {
		t.Fatalf("requeue block without a removal:\n%s", output)
	}
}
