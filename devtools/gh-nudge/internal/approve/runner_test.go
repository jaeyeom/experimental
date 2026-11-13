package approve

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

type approval struct {
	ref  github.PullRef
	sha  string
	body string
}

type fakeClient struct {
	viewer     string
	search     []github.PullRef
	searched   int
	prs        map[int]github.ApprovalPullRequest
	loadErr    map[int]error
	heads      map[int]string
	headReads  int
	approveErr error
	approvals  []approval
	markErr    error
	marks      []viewedMark
	diffErr    error
	diffReads  int
	patchReads int
	patchErr   error
	patches    map[int][]github.PullFile
	// diff overrides the default diff of every pull request.
	diff string
	handoffFake
}

func (f *fakeClient) ViewerLogin() (string, error) { return f.viewer, nil }

func (f *fakeClient) SearchUserReviewRequested() ([]github.PullRef, error) {
	f.searched++
	return f.search, nil
}

func (f *fakeClient) LoadApprovalPullRequest(ref github.PullRef) (github.ApprovalPullRequest, error) {
	if err := f.loadErr[ref.Number]; err != nil {
		return github.ApprovalPullRequest{}, err
	}
	pr, ok := f.prs[ref.Number]
	if !ok {
		return github.ApprovalPullRequest{}, errors.New("unexpected pull request")
	}
	return pr, nil
}

func (f *fakeClient) PullRequestHeadSHA(ref github.PullRef) (string, error) {
	f.headReads++
	if sha, ok := f.heads[ref.Number]; ok {
		return sha, nil
	}
	return f.prs[ref.Number].HeadSHA, nil
}

func (f *fakeClient) PullRequestFilePatches(ref github.PullRef) ([]github.PullFile, error) {
	f.patchReads++
	if f.patchErr != nil {
		return nil, f.patchErr
	}
	return f.patches[ref.Number], nil
}

func (f *fakeClient) PullRequestDiff(ref github.PullRef) (string, error) {
	f.diffReads++
	if f.diffErr != nil {
		return "", f.diffErr
	}
	if f.diff != "" {
		return f.diff, nil
	}
	return fmt.Sprintf("diff for #%d\n", ref.Number), nil
}

func (f *fakeClient) ApprovePullRequest(ref github.PullRef, sha, body string) error {
	if f.approveErr != nil {
		return f.approveErr
	}
	f.approvals = append(f.approvals, approval{ref: ref, sha: sha, body: body})
	return nil
}

type viewedMark struct {
	ref  github.PullRef
	id   string
	path string
}

func (f *fakeClient) MarkFileAsViewed(ref github.PullRef, id, path string) error {
	if f.markErr != nil {
		return f.markErr
	}
	f.marks = append(f.marks, viewedMark{ref: ref, id: id, path: path})
	return nil
}

func prNumber(n int) github.ApprovalPullRequest {
	pr := basePR()
	pr.Ref.Number = n
	return pr
}

func newFake(prs ...github.ApprovalPullRequest) *fakeClient {
	fake := &fakeClient{viewer: "me", prs: map[int]github.ApprovalPullRequest{}}
	for _, pr := range prs {
		fake.prs[pr.Ref.Number] = pr
		fake.search = append(fake.search, pr.Ref)
	}
	return fake
}

func fixedNow() time.Time { return time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC) }

type runResult struct {
	out  string
	log  []LogEntry
	err  error
	fake *fakeClient
}

func run(t *testing.T, fake *fakeClient, rules []config.ApproveRule, opts Options) runResult {
	t.Helper()
	return runWith(t, fake, rules, opts, nil)
}

func runWith(t *testing.T, fake *fakeClient, rules []config.ApproveRule, opts Options, command CommandRunner) runResult {
	t.Helper()
	return runConfig(t, fake, config.ApproveConfig{Body: "LGTM", SkipAlreadyApproved: true, Rules: rules}, config.ViewedConfig{}, opts, command)
}

func runConfig(t *testing.T, fake *fakeClient, cfg config.ApproveConfig, viewed config.ViewedConfig, opts Options, command CommandRunner) runResult {
	t.Helper()
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client:     fake,
		Config:     cfg,
		Viewed:     viewed,
		Out:        &out,
		Log:        &logBuf,
		Now:        fixedNow,
		RunCommand: command,
	}
	err := runner.Run(opts)
	var entries []LogEntry
	for _, line := range strings.Split(strings.TrimSpace(logBuf.String()), "\n") {
		if line == "" {
			continue
		}
		var entry LogEntry
		if jsonErr := json.Unmarshal([]byte(line), &entry); jsonErr != nil {
			t.Fatalf("log line %q: %v", line, jsonErr)
		}
		entries = append(entries, entry)
	}
	return runResult{out: out.String(), log: entries, err: err, fake: fake}
}

var dashboardRule = config.ApproveRule{Name: "dashboard", PathsAllMatch: []string{"dash/**"}, NotDraft: true}

func TestRunIncludesIndividualRequestAndIgnoresTeamOnly(t *testing.T) {
	team := prNumber(2)
	team.RequestedUsers = nil
	team.RequestedTeams = []string{"core"}
	res := run(t, newFake(prNumber(1), team), []config.ApproveRule{dashboardRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.log) != 1 || res.log[0].PR != "acme/widgets#1" {
		t.Fatalf("log = %+v", res.log)
	}
	if !strings.Contains(res.out, "acme/widgets#1") || strings.Contains(res.out, "acme/widgets#2") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunDryRunDoesNotApprove(t *testing.T) {
	res := run(t, newFake(prNumber(1)), []config.ApproveRule{dashboardRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 0 || res.fake.headReads != 0 {
		t.Fatalf("dry run approved %+v (head reads %d)", res.fake.approvals, res.fake.headReads)
	}
	if res.log[0].Action != ActionWouldApprove {
		t.Fatalf("action = %q", res.log[0].Action)
	}
	if !strings.Contains(res.out, "dashboard") || !strings.Contains(res.out, "Tidy dashboard") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunApplyApprovesAtEvaluatedHead(t *testing.T) {
	res := run(t, newFake(prNumber(1)), []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 1 {
		t.Fatalf("approvals = %+v", res.fake.approvals)
	}
	got := res.fake.approvals[0]
	if got.sha != "h1" || got.body != "LGTM\n\n<!-- gh-approve: dashboard -->" || got.ref.Number != 1 {
		t.Fatalf("approval = %+v", got)
	}
	if res.log[0].Action != ActionApproved {
		t.Fatalf("action = %q", res.log[0].Action)
	}
}

func TestRunNeverApprovesSelfAuthored(t *testing.T) {
	pr := prNumber(1)
	pr.Author = "Me"
	res := run(t, newFake(pr), []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 0 {
		t.Fatalf("approved own PR: %+v", res.fake.approvals)
	}
	entry := res.log[0]
	if entry.Action != ActionSkipped || entry.Rule == nil || *entry.Rule != "dashboard" || !strings.Contains(entry.Reason, "author") {
		t.Fatalf("entry = %+v", entry)
	}
}

func TestRunSkipsAlreadyApprovedAtHead(t *testing.T) {
	pr := prNumber(1)
	pr.Reviews = append(pr.Reviews, github.ApprovalReview{Author: "me", State: "APPROVED", CommitSHA: "h1"})
	res := run(t, newFake(pr), []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 0 || res.log[0].Action != ActionSkipped || !strings.Contains(res.log[0].Reason, "already approved") {
		t.Fatalf("approvals = %+v log = %+v", res.fake.approvals, res.log)
	}

	older := prNumber(1)
	older.Reviews = append(older.Reviews, github.ApprovalReview{Author: "me", State: "APPROVED", CommitSHA: "h0"})
	res = run(t, newFake(older), []config.ApproveRule{dashboardRule}, Options{})
	if res.log[0].Action != ActionWouldApprove {
		t.Fatalf("approval on older head: action = %q", res.log[0].Action)
	}
}

func TestRunSkipsWhenHeadMoved(t *testing.T) {
	fake := newFake(prNumber(1))
	fake.heads = map[int]string{1: "h2"}
	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(fake.approvals) != 0 {
		t.Fatalf("approved after head moved: %+v", fake.approvals)
	}
	entry := res.log[0]
	if entry.Action != ActionSkipped || !strings.Contains(entry.Reason, "h1") || !strings.Contains(entry.Reason, "h2") {
		t.Fatalf("entry = %+v", entry)
	}
	if !strings.Contains(res.out, "head moved") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunGhErrorStopsThatPROnly(t *testing.T) {
	fake := newFake(prNumber(1), prNumber(2))
	fake.loadErr = map[int]error{1: errors.New("gh: HTTP 502")}
	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{})
	if res.err == nil {
		t.Fatal("Run() error = nil, want an error for the failed PR")
	}
	if len(res.log) != 2 || res.log[0].Action != ActionError || !strings.Contains(res.log[0].Reason, "HTTP 502") ||
		res.log[1].Action != ActionWouldApprove {
		t.Fatalf("log = %+v", res.log)
	}
	if !strings.Contains(res.out, "HTTP 502") {
		t.Fatalf("out = %s", res.out)
	}

	fake = newFake(prNumber(1))
	fake.approveErr = errors.New("gh: HTTP 422")
	res = run(t, fake, []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err == nil || res.log[0].Action != ActionError || !strings.Contains(res.log[0].Reason, "HTTP 422") {
		t.Fatalf("approve error: err = %v log = %+v", res.err, res.log)
	}
}

func TestRunManualShowsFirstFailurePerRule(t *testing.T) {
	pr := prNumber(1)
	pr.Author = "bob"
	rules := []config.ApproveRule{
		dashboardRule,
		{Name: "alice-only", AuthorsAllow: []string{"alice"}, MaxFiles: intPtr(0)},
		{Name: "labelled", LabelsRequired: []string{"missing"}},
	}
	rules[0].PathsAllMatch = []string{"docs/**"}
	res := run(t, newFake(pr), rules, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if res.log[0].Action != ActionManual || res.log[0].Rule != nil {
		t.Fatalf("entry = %+v", res.log[0])
	}
	for _, want := range []string{"manual", "dashboard: paths_all_match", "alice-only: max_files", "labelled: labels_required"} {
		if !strings.Contains(res.out, want) {
			t.Fatalf("out missing %q:\n%s", want, res.out)
		}
	}
	if strings.Contains(res.out, "alice-only: authors_allow") {
		t.Fatalf("out shows more than the first failure:\n%s", res.out)
	}
}

func TestRunAuditLogFields(t *testing.T) {
	manual := prNumber(2)
	manual.Files = append(manual.Files, models.File{Path: "infra/main.tf"})
	res := run(t, newFake(prNumber(1), manual), []config.ApproveRule{dashboardRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.log) != 2 {
		t.Fatalf("log lines = %d, want 2", len(res.log))
	}
	entry := res.log[0]
	if !entry.Time.Equal(fixedNow()) || entry.PR != "acme/widgets#1" || entry.URL != "https://github.com/acme/widgets/pull/1" ||
		entry.HeadSHA != "h1" || entry.Author != "alice" || entry.Rule == nil || *entry.Rule != "dashboard" || entry.Action != ActionWouldApprove {
		t.Fatalf("entry = %+v", entry)
	}
	if len(entry.Rules) != 1 || entry.Rules[0].Rule != "dashboard" || len(entry.Rules[0].Conditions) == 0 {
		t.Fatalf("rules = %+v", entry.Rules)
	}
	if res.log[1].Action != ActionManual || res.log[1].Rule != nil || res.log[1].Rules[0].Matched {
		t.Fatalf("manual entry = %+v", res.log[1])
	}
}

func TestRunAuditLogRawJSONKeys(t *testing.T) {
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client: newFake(prNumber(1)),
		Config: config.ApproveConfig{Body: "LGTM", Rules: []config.ApproveRule{dashboardRule}},
		Out:    &out,
		Log:    &logBuf,
		Now:    fixedNow,
	}
	if err := runner.Run(Options{}); err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{`"time"`, `"pr"`, `"author"`, `"headSha"`, `"rule"`, `"rules"`, `"conditions"`, `"pass"`, `"saw"`, `"action"`} {
		if !strings.Contains(logBuf.String(), key) {
			t.Fatalf("log line missing %s: %s", key, logBuf.String())
		}
	}
	if strings.Count(logBuf.String(), "\n") != 1 {
		t.Fatalf("log = %q, want one line", logBuf.String())
	}
}

func TestRunExplainListsEveryCondition(t *testing.T) {
	pr := prNumber(1)
	pr.UnresolvedThreads = 2
	rules := []config.ApproveRule{
		{Name: "small", MaxChangedLines: intPtr(100), AuthorsDeny: []string{"mallory"}},
		{Name: "labelled", LabelsRequired: []string{"internal-only"}, Checks: config.ChecksGreen, MaxUnresolvedThreads: 5},
	}
	res := run(t, newFake(pr), rules, Options{Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	for _, want := range []string{
		"small", "max_changed_lines", "authors_deny", "alice",
		"labelled", "labels_required", "internal-only", "checks", "max_unresolved_threads", "2",
	} {
		if !strings.Contains(res.out, want) {
			t.Fatalf("explain output missing %q:\n%s", want, res.out)
		}
	}
	if strings.Count(res.out, "max_unresolved_threads") != 2 {
		t.Fatalf("want max_unresolved_threads for both rules:\n%s", res.out)
	}
}

func TestRunSinglePR(t *testing.T) {
	fake := newFake(prNumber(7))
	fake.search = nil
	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{PR: "https://github.com/acme/widgets/pull/7"})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.searched != 0 || len(res.log) != 1 || res.log[0].PR != "acme/widgets#7" {
		t.Fatalf("searched = %d log = %+v", fake.searched, res.log)
	}

	team := prNumber(8)
	team.RequestedUsers = nil
	res = run(t, newFake(team), []config.ApproveRule{dashboardRule}, Options{PR: "acme/widgets#8", Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 0 || res.log[0].Action != ActionSkipped || !strings.Contains(res.log[0].Reason, "individual") {
		t.Fatalf("not requested: log = %+v", res.log)
	}

	if res := run(t, newFake(), nil, Options{PR: "not-a-pr"}); res.err == nil {
		t.Fatal("invalid --pr error = nil")
	}
}

func TestRunSkipsClosedPR(t *testing.T) {
	pr := prNumber(1)
	pr.State = "MERGED"
	res := run(t, newFake(pr), []config.ApproveRule{dashboardRule}, Options{PR: "acme/widgets#1", Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(res.fake.approvals) != 0 || res.log[0].Action != ActionSkipped {
		t.Fatalf("log = %+v", res.log)
	}
}

// fakeAgent is a fake agent command. It records each call and prints stdout.
type fakeAgent struct {
	stdout   string
	code     int
	timedOut bool
	startErr bool
	calls    [][]string
	stdins   [][]byte
}

func (f *fakeAgent) run(_ context.Context, stdin []byte, argv []string) (remotecall.Result, error) {
	f.calls = append(f.calls, argv)
	f.stdins = append(f.stdins, stdin)
	if f.startErr {
		return remotecall.Result{}, errors.New("executable not found")
	}
	return remotecall.Result{Stdout: f.stdout, Code: f.code, TimedOut: f.timedOut}, nil
}

func agentRule(name string) config.ApproveRule {
	rule := dashboardRule
	rule.Name = name
	rule.AgentCommand = []string{"review-agent", "--strict"}
	return rule
}

func TestRunAgentApproveMatchesEvenInDryRun(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve", "reason": "only dashboards"}`}
	fake := newFake(prNumber(1))
	res := runWith(t, fake, []config.ApproveRule{agentRule("agent")}, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 1 || strings.Join(agent.calls[0], " ") != "review-agent --strict" {
		t.Fatalf("agent calls = %q", agent.calls)
	}
	entry := res.log[0]
	if entry.Action != ActionWouldApprove || entry.Rule == nil || *entry.Rule != "agent" {
		t.Fatalf("entry = %+v", entry)
	}
	got := entry.Rules[0].Agent
	if got == nil || got.Decision != AgentApprove || got.Reason != "only dashboards" {
		t.Fatalf("agent result = %+v", got)
	}
	if len(fake.approvals) != 0 {
		t.Fatalf("dry run approved %+v", fake.approvals)
	}

	res = runWith(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, Options{Apply: true}, agent.run)
	if res.err != nil || len(res.fake.approvals) != 1 || res.log[0].Action != ActionApproved {
		t.Fatalf("apply: err = %v log = %+v", res.err, res.log)
	}
}

func TestRunAgentDeferLeavesPRForHuman(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "defer", "reason": "touches billing"}`}
	res := runWith(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, Options{Apply: true}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	entry := res.log[0]
	if entry.Action != ActionManual || entry.Rule != nil || len(res.fake.approvals) != 0 {
		t.Fatalf("entry = %+v approvals = %+v", entry, res.fake.approvals)
	}
	if got := entry.Rules[0].Agent; got == nil || got.Decision != AgentDefer || got.Reason != "touches billing" {
		t.Fatalf("agent result = %+v", got)
	}
	if !strings.Contains(res.out, "agent: agent: defer (fresh): touches billing") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunAgentNotRunWhenAnotherConditionFails(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	pr := prNumber(1)
	pr.Draft = true
	fake := newFake(pr)
	res := runWith(t, fake, []config.ApproveRule{agentRule("agent")}, Options{Explain: true}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 0 || fake.diffReads != 0 {
		t.Fatalf("agent calls = %d diff reads = %d, want none", len(agent.calls), fake.diffReads)
	}
	if res.log[0].Action != ActionManual || res.log[0].Rules[0].Agent != nil {
		t.Fatalf("entry = %+v", res.log[0])
	}
	if first, _ := res.log[0].Rules[0].FirstFailure(); first.Condition != "not_draft" {
		t.Fatalf("first failure = %+v", first)
	}
	if !strings.Contains(res.out, "agent_command: not run") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunAgentNotRunForSkippedPR(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	pr := prNumber(1)
	pr.Author = "me"
	res := runWith(t, newFake(pr), []config.ApproveRule{agentRule("agent")}, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 0 || res.log[0].Action != ActionSkipped {
		t.Fatalf("agent calls = %d log = %+v", len(agent.calls), res.log)
	}
}

func TestRunAgentNotRunAfterEarlierRuleMatched(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	rules := []config.ApproveRule{dashboardRule, agentRule("agent")}
	res := runWith(t, newFake(prNumber(1)), rules, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 0 || res.log[0].Rule == nil || *res.log[0].Rule != "dashboard" {
		t.Fatalf("agent calls = %d log = %+v", len(agent.calls), res.log)
	}
}

func TestRunAgentDiffErrorDefers(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	fake := newFake(prNumber(1))
	fake.diffErr = errors.New("HTTP 406: diff too large")
	res := runWith(t, fake, []config.ApproveRule{agentRule("agent")}, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 0 || res.log[0].Action != ActionManual {
		t.Fatalf("agent calls = %d log = %+v", len(agent.calls), res.log)
	}
	if got := res.log[0].Rules[0].Agent; got == nil || got.Decision != AgentDefer || !strings.Contains(got.Reason, "HTTP 406") {
		t.Fatalf("agent result = %+v", got)
	}
}

func TestRunAgentDiffReadOncePerPR(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "defer", "reason": "no"}`}
	fake := newFake(prNumber(1))
	rules := []config.ApproveRule{agentRule("first"), agentRule("second")}
	res := runWith(t, fake, rules, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	if len(agent.calls) != 2 || fake.diffReads != 1 {
		t.Fatalf("agent calls = %d diff reads = %d", len(agent.calls), fake.diffReads)
	}
}

func TestRunAgentStdinCarriesDiffAndFiles(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve"}`}
	res := runWith(t, newFake(prNumber(1)), []config.ApproveRule{agentRule("agent")}, Options{}, agent.run)
	if res.err != nil {
		t.Fatal(res.err)
	}
	var input AgentInput
	if err := json.Unmarshal(agent.stdins[0], &input); err != nil {
		t.Fatal(err)
	}
	if input.Diff != "diff for #1\n" || len(input.Files) != 1 || input.Files[0].Path != "dash/a.go" ||
		input.Rule != "agent" || input.PR.Number != 1 || input.PR.Repo != "acme/widgets" || input.PR.Author != "alice" ||
		input.PR.URL != "https://github.com/acme/widgets/pull/1" || input.PR.Title != "Tidy dashboard" {
		t.Fatalf("stdin = %s", agent.stdins[0])
	}
}

func TestRunAgentExplainAndLogShowDecisionReasonDuration(t *testing.T) {
	agent := &fakeAgent{stdout: `{"decision": "approve", "reason": "only dashboards"}`}
	var out, logBuf bytes.Buffer
	runner := &Runner{
		Client:     newFake(prNumber(1)),
		Config:     config.ApproveConfig{Body: "LGTM", Rules: []config.ApproveRule{agentRule("agent")}},
		Out:        &out,
		Log:        &logBuf,
		Now:        fixedNow,
		RunCommand: agent.run,
	}
	if err := runner.Run(Options{Explain: true}); err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{`"agent"`, `"decision":"approve"`, `"reason":"only dashboards"`, `"durationMs"`} {
		if !strings.Contains(logBuf.String(), key) {
			t.Fatalf("log missing %s: %s", key, logBuf.String())
		}
	}
	if !strings.Contains(out.String(), "pass agent_command: approve: only dashboards (") {
		t.Fatalf("explain = %s", out.String())
	}
}
