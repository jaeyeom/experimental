package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"net/url"
	"os"
	"os/exec"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
)

const logTailLines = 20

type queueAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	EnqueuePullRequest(nodeID string) error
	OpenDownstack(owner, repo string, number int) ([]github.StackPR, error)
	OpenAbove(owner, repo string, number int) ([]github.StackPR, error)
	InNativeStack(owner, repo string, number int) (bool, error)
	OpenByBase(owner, repo, base string) ([]github.BranchPull, error)
	OpenByHead(owner, repo, head string) ([]github.BranchPull, error)
	FailureEvidence(owner, repo, sha string) (github.FailureEvidence, error)
	LandedFirst(owner, repo, sha string, self int) (github.StackPR, bool, error)
	MergeAsync(owner, repo string, number int, sha string) error
}

type watchAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	removalAPI
}

// removalAPI loads the failing check and, for a merge conflict, the pull
// request that landed first. queue, watch, and rca all use it.
type removalAPI interface {
	FailureEvidence(owner, repo, sha string) (github.FailureEvidence, error)
	landedFirstAPI
}

// landedFirstAPI finds the pull request that landed first on a merge group.
type landedFirstAPI interface {
	LandedFirst(owner, repo, sha string, self int) (github.StackPR, bool, error)
}

type watchOptions struct {
	timeout  time.Duration
	interval time.Duration
	now      func() time.Time
	sleep    func(time.Duration)
	asJSON   bool
	cfg      *config.Config
}

type watchResult struct {
	PullRequest  string `json:"pullRequest"`
	Result       string `json:"result"`
	Config       string `json:"config"`
	SettingsFrom string `json:"settingsFrom"`
	// RCAContext lists the RCA markers other tools wrote for the removal.
	RCAContext []rcaContext `json:"rcaContext,omitempty"`
}

type enqueueKind int

const (
	enqueueDirect enqueueKind = iota
	enqueueAsync
)

type queueJSONResult struct {
	PullRequest      string   `json:"pullRequest"`
	Readiness        []string `json:"readiness"`
	Config           string   `json:"config"`
	BlockMergeLabels []string `json:"blockMergeLabels"`
	RequiredChecks   []string `json:"requiredChecks"`
	Result           string   `json:"result"`
	StackedAbove     []int    `json:"stackedAbove"`
	SettingsFrom     string   `json:"settingsFrom"`
	Gate             string   `json:"gate"`
	// Requeue is the RCA gate's decision for the latest removal.
	Requeue *requeueDecision `json:"requeue,omitempty"`
}

// baseRefusalDoc describes the queue refusal for a pull request that is not in
// a native stack and whose base is not the repository's default branch.
const baseRefusalDoc = "A pull request whose base is not the repository's default branch, and which is not in a native stack, is refused"

// baseRefusalFormat is that refusal. The names come from the pull request
// and from defaultBranchRef, not from a fixed branch name.
const baseRefusalFormat = "base is %s, not %s; retarget to %s first"

func queueCommandHelp() string {
	return "Enqueue one pull request (owner/repo#n, a URL, or a bare number) on the merge queue; refuses a draft. " + baseRefusalDoc + " --when-green waits only for pending required checks (timeout defaults to 2h)."
}

func watchCommandHelp() string {
	return "Wait until each pull request merges, is ejected, or times out (owner/repo#n, a URL, or a bare number). On an ejection, prints the failing step's log tail. --timeout is a Go duration (default 30m). Does not read stdin and does not need a terminal"
}

func timeoutHelp() string {
	return "How long watch waits, and how long queue --when-green waits when set (Go duration). Watch defaults to 30m. queue --when-green defaults to 2h"
}

func whenGreenHelp() string {
	return "Wait for pending or not yet reported required checks on the current head, then enqueue (timeout defaults to 2h)"
}

const (
	defaultWhenGreenTimeout = 2 * time.Hour
	whenGreenInitialDelay   = 15 * time.Second
	whenGreenMaxDelay       = time.Minute
)

var (
	whenGreenTimeout = defaultWhenGreenTimeout
	whenGreenNow     = time.Now
	whenGreenSleep   = time.Sleep
)

const (
	mergeConflictProblem    = "pull request has merge conflicts"
	mergeableUnknownProblem = "mergeable state still UNKNOWN after 60s; GitHub has not computed it yet, retry later"
)

var (
	mergeableNow   = time.Now
	mergeableSleep = time.Sleep
)

// queueMergeablePoll re-reads UNKNOWN after 2s, 4s, 8s, then every 16s,
// for at most 60s in total.
func queueMergeablePoll() mergeablePoll {
	return mergeablePoll{
		first:  2 * time.Second,
		max:    16 * time.Second,
		budget: 60 * time.Second,
		now:    mergeableNow,
		sleep:  mergeableSleep,
	}
}

func queueUsage() string {
	return "queue <owner/repo#n|url|number>"
}

func watchUsage() string {
	return "watch <owner/repo#n|url|number>..."
}

func queuePullRequest(client queueAPI, cfg *config.Config, ref string, includeDownstack bool) error {
	resolved, err := resolvePullRef(ref)
	if err != nil {
		return err
	}
	pr, err := client.LoadPullRequest(resolved)
	if err != nil {
		return fmt.Errorf("load pull request %s: %w", resolved, err)
	}
	return queueLoaded(client, cfg, pr, includeDownstack, true)
}

// queueLoaded runs queue on a pull request that is already loaded. It prints
// the removal evidence only when evidence is true.
func queueLoaded(client queueAPI, cfg *config.Config, pr github.QueuePullRequest, includeDownstack, evidence bool) error {
	cfg, above, baseProblem, err := prepareQueue(client, cfg, pr, evidence)
	if err != nil {
		return err
	}
	problems := queueProblems(pr, cfg, baseProblem)
	if whenGreen && pendingOnly(pr.Checks, cfg.Settings.RequiredChecks, problems) {
		return waitForPendingChecks(client, pr, cfg, above, problems, includeDownstack)
	}
	return finishQueue(client, pr, cfg, above, problems, includeDownstack)
}

func prepareQueue(client queueAPI, cfg *config.Config, pr github.QueuePullRequest, evidence bool) (*config.Config, []int, string, error) {
	cfg = cfg.ForRepository(pr.Ref.Owner, pr.Ref.Repo)
	above, baseProblem, err := reportStackedAbove(client, pr)
	if err != nil {
		return nil, nil, "", err
	}
	if !jsonOutput {
		fmt.Println(stackedAboveLine(above))
	}
	if evidence {
		if err := printRemovalEvidence(client, cfg, pr); err != nil {
			return nil, nil, "", err
		}
	}
	return cfg, above, baseProblem, nil
}

// queuePreflight runs every gate that queue and queue -dry-run share, on the
// same loaded data, before either branches on dryRun. pending is true when
// --when-green should keep waiting because mergeable is still UNKNOWN.
// requeue is the RCA gate's decision, nil when the PR was not removed.
func queuePreflight(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string) (pending bool, requeue *requeueDecision, err error) {
	if !jsonOutput {
		fmt.Println(gateLine(pr, cfg.Settings))
	}
	state, err := queueMergeable(client, pr, len(problems) == 0)
	if err != nil {
		return false, nil, err
	}
	switch {
	case state == "CONFLICTING":
		problems = append(problems, mergeConflictProblem)
	case state == "UNKNOWN" && len(problems) == 0 && !whenGreen:
		problems = append(problems, mergeableUnknownProblem)
	}
	if len(problems) > 0 {
		return false, nil, refuseWithProblems(pr, cfg, above, problems)
	}
	requeue, err = requeueGate(client, pr, cfg)
	if err != nil {
		return false, nil, err
	}
	if requeue != nil && !requeue.Allowed {
		return false, requeue, refuseRequeue(pr, cfg, above, requeue)
	}
	return state == "UNKNOWN", requeue, nil
}

// refuseRequeue reports the RCA gate's refusal, with the decision in JSON.
func refuseRequeue(pr github.QueuePullRequest, cfg *config.Config, above []int, requeue *requeueDecision) error {
	refusal := "refusing to enqueue: " + requeue.Reason
	if jsonOutput {
		if err := writeQueueResultWith(pr, cfg, []string{refusal}, above, "refused", "", requeue); err != nil {
			return err
		}
	}
	return fmt.Errorf("%w: %s", errQueueRefused, requeue.Reason)
}

// errQueueRefused marks a refusal by the queue preflight or the RCA gate, as
// opposed to a failure to decide.
var errQueueRefused = errors.New("refusing to enqueue")

// queueMergeable re-reads an UNKNOWN mergeable state with backoff. It does
// not wait when wait is false, since other problems already refuse.
func queueMergeable(client queueAPI, pr github.QueuePullRequest, wait bool) (string, error) {
	if !wait {
		return pr.Mergeable, nil
	}
	refresh := func(ref string) (string, error) {
		next, err := client.LoadPullRequest(ref)
		if err != nil {
			return "", fmt.Errorf("load pull request %s: %w", ref, err)
		}
		return next.Mergeable, nil
	}
	return resolveMergeable(pr.Mergeable, pr.Ref.Raw, refresh, queueMergeablePoll())
}

func finishQueue(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string, includeDownstack bool) error {
	pending, requeue, err := queuePreflight(client, pr, cfg, above, problems)
	if err != nil {
		return err
	}
	if pending {
		return waitUntilGreen(client, pr, cfg, above, []string{"mergeable state is UNKNOWN"}, includeDownstack, requeue)
	}
	lines := readyLines(pr, cfg.Settings)
	if dryRun {
		return writeQueueResultWith(pr, cfg, lines, above, "would-enqueue", fmt.Sprintf("Would enqueue %s.\n", pr.Ref.Raw), requeue)
	}
	kind, err := enqueuePR(client, pr, includeDownstack)
	if err != nil {
		return err
	}
	text := fmt.Sprintf("Enqueued %s.\n", pr.Ref.Raw)
	if kind == enqueueAsync {
		text = fmt.Sprintf("Enqueued %s with merge-async.\n", pr.Ref.Raw)
	}
	return writeQueueResultWith(pr, cfg, lines, above, "enqueued", text, requeue)
}

func waitForPendingChecks(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string, includeDownstack bool) error {
	// problems are only pending required checks here; the wait handles them.
	_, requeue, err := queuePreflight(client, pr, cfg, above, nil)
	if err != nil {
		return err
	}
	return waitUntilGreen(client, pr, cfg, above, problems, includeDownstack, requeue)
}

// waitUntilGreen polls until the pending problems clear, or reports that a
// dry run would wait.
func waitUntilGreen(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string, includeDownstack bool, requeue *requeueDecision) error {
	if dryRun {
		return writeQueueResultWith(pr, cfg, problems, above, "would-wait", fmt.Sprintf("Would wait for %s.\n", pr.Ref.Raw), requeue)
	}
	return pollUntilGreen(client, pr, cfg, above, includeDownstack)
}

const (
	waitPoll = iota
	waitEnqueue
	waitAbort
)

func pollUntilGreen(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, includeDownstack bool) error {
	head := pr.HeadSHA
	deadline := whenGreenNow().Add(queueWaitTimeout())
	delay := whenGreenInitialDelay
	for {
		if err := sleepUntilPoll(deadline, delay); err != nil {
			return abortTimeout(pr, cfg, above)
		}
		delay = nextWhenGreenDelay(delay)
		next, err := client.LoadPullRequest(pr.Ref.Raw)
		if err != nil {
			if wait, ok := watchTransientWait(err, delay); ok {
				delay = wait
				continue
			}
			return fmt.Errorf("load pull request %s: %w", pr.Ref.Raw, err)
		}
		action, reason, freshAbove, err := classifyReload(client, cfg, next, head)
		if err != nil {
			return err
		}
		if freshAbove != nil {
			above = freshAbove
		}
		switch action {
		case waitEnqueue:
			return finishQueue(client, next, cfg, above, nil, includeDownstack)
		case waitAbort:
			return abortQueue(next, cfg, above, reason)
		}
		pr = next
	}
}

func queueWaitTimeout() time.Duration {
	return whenGreenTimeout
}

func sleepUntilPoll(deadline time.Time, delay time.Duration) error {
	now := whenGreenNow()
	if !now.Before(deadline) {
		return errQueueWaitTimeout
	}
	if delay <= 0 {
		delay = whenGreenInitialDelay
	}
	if remaining := deadline.Sub(now); delay > remaining {
		delay = remaining
	}
	whenGreenSleep(delay)
	if !whenGreenNow().Before(deadline) {
		return errQueueWaitTimeout
	}
	return nil
}

var errQueueWaitTimeout = errors.New("timeout")

func nextWhenGreenDelay(current time.Duration) time.Duration {
	if current <= 0 {
		return whenGreenInitialDelay
	}
	next := current * 2
	if next > whenGreenMaxDelay {
		return whenGreenMaxDelay
	}
	return next
}

func classifyReload(client queueAPI, cfg *config.Config, pr github.QueuePullRequest, head string) (int, string, []int, error) {
	if reason, failed := failedRequiredReason(pr.Checks, cfg.Settings.RequiredChecks); failed {
		return waitAbort, reason, nil, nil
	}
	if pr.HeadSHA != head {
		return waitAbort, "head SHA changed", nil, nil
	}
	if pr.ReviewDecision != "APPROVED" {
		return waitAbort, "pull request is not approved", nil, nil
	}
	if pr.UnresolvedThreads > 0 {
		return waitAbort, "unresolved review thread", nil, nil
	}
	if pr.Mergeable == "CONFLICTING" {
		return waitAbort, mergeConflictProblem, nil, nil
	}
	above, baseProblem, err := reportStackedAbove(client, pr)
	if err != nil {
		return 0, "", nil, err
	}
	problems := queueProblems(pr, cfg, baseProblem)
	if len(problems) == 0 && pr.Mergeable == "UNKNOWN" {
		return waitPoll, "", above, nil
	}
	if len(problems) == 0 {
		return waitEnqueue, "", above, nil
	}
	if pendingOnly(pr.Checks, cfg.Settings.RequiredChecks, problems) {
		return waitPoll, "", above, nil
	}
	return waitAbort, strings.Join(problems, "; "), above, nil
}

// abortTimeout reports a timeout and names the required checks still not
// green on the last loaded snapshot.
func abortTimeout(pr github.QueuePullRequest, cfg *config.Config, above []int) error {
	problems := requiredCheckProblems(pr.Checks, cfg.Settings.RequiredChecks)
	if len(problems) == 0 {
		return abortQueue(pr, cfg, above, "timeout")
	}
	detail := strings.Join(problems, "; ")
	text := fmt.Sprintf("Not enqueueing %s: timeout (%s)\n", pr.Ref.Raw, detail)
	if err := writeQueueResult(pr, cfg, append([]string{"timeout"}, problems...), above, "timeout", text); err != nil {
		return err
	}
	return fmt.Errorf("not enqueueing %s: timeout (%s)", pr.Ref.Raw, detail)
}

func abortQueue(pr github.QueuePullRequest, cfg *config.Config, above []int, reason string) error {
	text := fmt.Sprintf("Not enqueueing %s: %s\n", pr.Ref.Raw, reason)
	if err := writeQueueResult(pr, cfg, []string{reason}, above, reason, text); err != nil {
		return err
	}
	return fmt.Errorf("not enqueueing %s: %s", pr.Ref.Raw, reason)
}

func stackNumbers(prs []github.StackPR) []int {
	numbers := make([]int, 0, len(prs))
	for _, pr := range prs {
		numbers = append(numbers, pr.Number)
	}
	return numbers
}

func stackedAboveLine(numbers []int) string {
	if len(numbers) == 0 {
		return "stacked above: (none)"
	}
	parts := make([]string, len(numbers))
	for i, number := range numbers {
		parts[i] = fmt.Sprintf("#%d", number)
	}
	return "stacked above: " + strings.Join(parts, ", ") + ", retarget needed after merge"
}

func queueProblems(pr github.QueuePullRequest, cfg *config.Config, baseProblem string) []string {
	problems := append(queueConfigProblems(pr, cfg), preflightProblems(pr, cfg.Settings.BlockMergeLabels, cfg.Settings.RequiredChecks)...)
	if baseProblem != "" {
		problems = append(problems, baseProblem)
	}
	return problems
}

func reportStackedAbove(client queueAPI, pr github.QueuePullRequest) ([]int, string, error) {
	above, err := client.OpenAbove(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number)
	if err != nil {
		return nil, "", fmt.Errorf("stacked above for %s: %w", pr.Ref.Raw, err)
	}
	native, err := client.InNativeStack(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number)
	if err != nil {
		return nil, "", fmt.Errorf("stacked above for %s: %w", pr.Ref.Raw, err)
	}
	walked, below, found, err := branchNeighbors(client, pr)
	if err != nil {
		return nil, "", err
	}
	numbers := mergeStackNumbers(stackNumbers(above), walked)
	if !refuseNonNativeBase(pr, native) {
		return numbers, "", nil
	}
	return numbers, baseRefusalText(pr, below, found), nil
}

func mergeStackNumbers(native, walked []int) []int {
	seen := make(map[int]bool, len(native)+len(walked))
	out := make([]int, 0, len(native)+len(walked))
	for _, number := range native {
		if seen[number] {
			continue
		}
		seen[number] = true
		out = append(out, number)
	}
	for _, number := range walked {
		if seen[number] {
			continue
		}
		seen[number] = true
		out = append(out, number)
	}
	return out
}

func refuseNonNativeBase(pr github.QueuePullRequest, native bool) bool {
	if native || pr.BaseRefName == "" || pr.BaseRefName == pr.DefaultBranch {
		return false
	}
	return true
}

func baseRefusalText(pr github.QueuePullRequest, below int, found bool) string {
	msg := fmt.Sprintf(baseRefusalFormat, pr.BaseRefName, pr.DefaultBranch, pr.DefaultBranch)
	if found {
		msg += fmt.Sprintf(" (it sits on #%d)", below)
	}
	return msg
}

func branchNeighbors(client queueAPI, pr github.QueuePullRequest) ([]int, int, bool, error) {
	above, err := branchAbove(client, pr)
	if err != nil {
		return nil, 0, false, err
	}
	below, found, err := branchBelow(client, pr)
	if err != nil {
		return nil, 0, false, err
	}
	return above, below, found, nil
}

func branchAbove(client queueAPI, pr github.QueuePullRequest) ([]int, error) {
	if pr.HeadRefName == "" {
		return nil, nil
	}
	seenNum := map[int]bool{pr.Ref.Number: true}
	seenHead := map[string]bool{}
	var numbers []int
	heads := []string{pr.HeadRefName}
	for len(heads) > 0 {
		head := heads[0]
		heads = heads[1:]
		if seenHead[head] {
			continue
		}
		seenHead[head] = true
		pulls, err := client.OpenByBase(pr.Ref.Owner, pr.Ref.Repo, head)
		if err != nil {
			return nil, fmt.Errorf("stacked above for %s: %w", pr.Ref.Raw, err)
		}
		added, next := openAboveByBranch(pulls, seenNum, seenHead)
		numbers = append(numbers, added...)
		heads = append(heads, next...)
	}
	return numbers, nil
}

func openAboveByBranch(pulls []github.BranchPull, seenNum map[int]bool, seenHead map[string]bool) ([]int, []string) {
	var numbers []int
	var next []string
	for _, item := range pulls {
		if item.State != "open" || seenNum[item.Number] {
			continue
		}
		seenNum[item.Number] = true
		numbers = append(numbers, item.Number)
		if item.HeadRefName != "" && !seenHead[item.HeadRefName] {
			next = append(next, item.HeadRefName)
		}
	}
	return numbers, next
}

func branchBelow(client queueAPI, pr github.QueuePullRequest) (int, bool, error) {
	if pr.BaseRefName == "" || pr.BaseRefName == pr.DefaultBranch {
		return 0, false, nil
	}
	pulls, err := client.OpenByHead(pr.Ref.Owner, pr.Ref.Repo, pr.BaseRefName)
	if err != nil {
		return 0, false, fmt.Errorf("stacked below for %s: %w", pr.Ref.Raw, err)
	}
	for _, item := range pulls {
		if item.Number == pr.Ref.Number || item.State != "open" {
			continue
		}
		return item.Number, true, nil
	}
	return 0, false, nil
}

func queueConfigProblems(pr github.QueuePullRequest, cfg *config.Config) []string {
	if allowNoRequiredChecks {
		return nil
	}
	var problems []string
	if len(cfg.Settings.RequiredChecks) == 0 {
		problems = append(problems, "required_checks is empty")
	}
	ownerRepo := pr.Ref.Owner + "/" + pr.Ref.Repo
	if !configuredRepository(cfg, ownerRepo) {
		problems = append(problems, ownerRepo+" is not a configured repository")
	}
	return problems
}

func configuredRepository(cfg *config.Config, ownerRepo string) bool {
	for _, name := range cfg.GitHub.Repos {
		if cfg.GitHub.Owner+"/"+name == ownerRepo {
			return true
		}
	}
	return false
}

func effectiveConfigLines(cfg *config.Config) []string {
	return []string{
		"config: " + cfg.Path,
		"settings from: " + settingsSource(cfg),
		"block_merge_labels: " + formatNameList(cfg.Settings.BlockMergeLabels),
		"required_checks: " + formatNameList(cfg.Settings.RequiredChecks),
	}
}

func settingsSource(cfg *config.Config) string {
	if cfg == nil || cfg.SettingsFrom == "" {
		return config.SettingsFromGlobal
	}
	return cfg.SettingsFrom
}

func formatNameList(items []string) string {
	if len(items) == 0 {
		return "(none)"
	}
	return strings.Join(items, ", ")
}

func jsonStringList(items []string) []string {
	if items == nil {
		return []string{}
	}
	return items
}

func refuseWithProblems(pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string) error {
	if jsonOutput {
		if err := writeQueueResult(pr, cfg, problems, above, "refused", ""); err != nil {
			return err
		}
		return fmt.Errorf("%w %s", errQueueRefused, pr.Ref.Raw)
	}
	for _, line := range effectiveConfigLines(cfg) {
		fmt.Println(line)
	}
	fmt.Printf("Refusing to enqueue %s:\n", pr.Ref.Raw)
	for _, problem := range problems {
		fmt.Printf("- %s\n", problem)
	}
	return fmt.Errorf("%w %s", errQueueRefused, pr.Ref.Raw)
}

func readyLines(pr github.QueuePullRequest, settings config.SettingsConfig) []string {
	draft := "pull request is not a draft"
	if pr.Draft {
		draft = "pull request is a draft"
	}
	lines := []string{draft, "no block label"}
	if pr.ReviewDecision == "APPROVED" {
		lines = append(lines, "pull request is approved")
	} else {
		lines = append(lines, "pull request is not approved")
	}
	if len(settings.RequiredChecks) == 0 {
		lines = append(lines, "no required checks configured")
	} else {
		for _, name := range settings.RequiredChecks {
			lines = append(lines, fmt.Sprintf("required check %q is green", name))
		}
	}
	if pr.UnresolvedThreads > 0 {
		lines = append(lines, "unresolved review thread")
	} else {
		lines = append(lines, "no unresolved review threads")
	}
	return lines
}

// queueResultSink, when set, receives the queue JSON result instead of
// stdout. handle-ejection uses it to report the queue step in its own JSON.
var queueResultSink func(queueJSONResult)

func writeQueueResult(pr github.QueuePullRequest, cfg *config.Config, lines []string, above []int, result, text string) error {
	return writeQueueResultWith(pr, cfg, lines, above, result, text, nil)
}

// writeQueueResultWith is writeQueueResult with the RCA gate's decision,
// which JSON reports as requeue.
func writeQueueResultWith(pr github.QueuePullRequest, cfg *config.Config, lines []string, above []int, result, text string, requeue *requeueDecision) error {
	if lines == nil {
		lines = []string{}
	}
	if above == nil {
		above = []int{}
	}
	if jsonOutput {
		out := queueJSONResult{
			PullRequest:      pr.Ref.Raw,
			Readiness:        lines,
			Config:           cfg.Path,
			BlockMergeLabels: jsonStringList(cfg.Settings.BlockMergeLabels),
			RequiredChecks:   jsonStringList(cfg.Settings.RequiredChecks),
			Result:           result,
			StackedAbove:     above,
			SettingsFrom:     settingsSource(cfg),
			Gate:             gateLine(pr, cfg.Settings),
			Requeue:          requeue,
		}
		if queueResultSink != nil {
			queueResultSink(out)
			return nil
		}
		// Signatures mask values as <n>, <hex>, <path>, and <ts>; keep them
		// readable instead of escaping < and >.
		enc := json.NewEncoder(os.Stdout)
		enc.SetEscapeHTML(false)
		enc.SetIndent("", "  ")
		if err := enc.Encode(out); err != nil {
			return fmt.Errorf("encode queue result: %w", err)
		}
		return nil
	}
	for _, line := range effectiveConfigLines(cfg) {
		fmt.Println(line)
	}
	for _, line := range lines {
		fmt.Println(line)
	}
	fmt.Print(text)
	return nil
}

func printRemovalEvidence(client removalAPI, cfg *config.Config, pr github.QueuePullRequest) error {
	event, ok := latestQueueEvent(pr.Events)
	if !ok || event.Kind != github.QueueEventRemoved {
		return nil
	}
	_, err := printEventEvidence(client, evidenceOut{cfg: cfg, w: os.Stdout}, pr, event, func() (github.FailureEvidence, error) {
		return client.FailureEvidence(pr.Ref.Owner, pr.Ref.Repo, event.CommitSHA)
	})
	return err
}

// printEventEvidence prints the evidence for one removal event, and for a
// merge conflict the pull request that landed first.
func printEventEvidence(client landedFirstAPI, out evidenceOut, pr github.QueuePullRequest, event github.QueueEvent, fetch func() (github.FailureEvidence, error)) (eventEvidence, error) {
	found, err := out.event(pr, event, fetch)
	if err != nil {
		return eventEvidence{}, err
	}
	if event.CommitSHA == "" || event.Reason != "merge_conflict" {
		return found, nil
	}
	return found, printLandedFirst(client, pr, event.CommitSHA)
}

// eventEvidence is what evidenceOut.event found for one removal event.
type eventEvidence struct {
	evidence github.FailureEvidence
	report   failureReport
	hints    []flakeHint
}

func (e evidenceOut) event(pr github.QueuePullRequest, event github.QueueEvent, fetch func() (github.FailureEvidence, error)) (eventEvidence, error) {
	fmt.Fprintf(e.w, "Removal reason: %s\n", event.Reason)
	if event.CommitSHA == "" {
		return eventEvidence{}, nil
	}
	evidence, err := fetch()
	if err != nil {
		return eventEvidence{}, fmt.Errorf("failure evidence for %s: %w", pr.Ref.Raw, err)
	}
	report, err := e.failure(evidence)
	if err != nil {
		return eventEvidence{}, err
	}
	hints, err := flakeHints(e.cfg, report)
	if err != nil {
		return eventEvidence{}, err
	}
	for _, hint := range hints {
		fmt.Fprintln(e.w, hint.text())
	}
	return eventEvidence{evidence: evidence, report: report, hints: hints}, nil
}

func printLandedFirst(client landedFirstAPI, pr github.QueuePullRequest, sha string) error {
	landed, found, err := client.LandedFirst(pr.Ref.Owner, pr.Ref.Repo, sha, pr.Ref.Number)
	if err != nil {
		return fmt.Errorf("landed first for %s: %w", pr.Ref.Raw, err)
	}
	if !found {
		return nil
	}
	fmt.Printf("Landed first: %s/%s#%d\n", pr.Ref.Owner, pr.Ref.Repo, landed.Number)
	return nil
}

func preflightProblems(pr github.QueuePullRequest, blockLabels, required []string) []string {
	var problems []string
	if pr.Draft {
		problems = append(problems, "pull request is a draft")
	}
	model := models.PullRequest{}
	for _, name := range pr.Labels {
		model.Labels = append(model.Labels, models.Label{Name: name})
	}
	if label, blocked := model.BlocksMerge(blockLabels); blocked {
		problems = append(problems, fmt.Sprintf("label %q blocks merge", label))
	}
	if pr.ReviewDecision != "APPROVED" {
		problems = append(problems, "pull request is not approved")
	}
	problems = append(problems, requiredCheckProblems(pr.Checks, required)...)
	if pr.UnresolvedThreads > 0 {
		problems = append(problems, "unresolved review thread")
	}
	return problems
}

func requiredCheckProblems(checks []github.QueueCheck, required []string) []string {
	if len(required) == 0 {
		return nil
	}
	byName := checksByName(checks)
	var problems []string
	for _, name := range required {
		check, ok := byName[name]
		if !ok {
			problems = append(problems, missingCheckProblem(name))
			continue
		}
		if check.Green {
			continue
		}
		problems = append(problems, requiredCheckProblem(name, check.Conclusion))
	}
	return problems
}

func checksByName(checks []github.QueueCheck) map[string]github.QueueCheck {
	byName := make(map[string]github.QueueCheck, len(checks))
	for _, check := range checks {
		byName[check.Name] = check
	}
	return byName
}

func missingCheckProblem(name string) string {
	return fmt.Sprintf("required check %q is missing", name)
}

func requiredCheckProblem(name, conclusion string) string {
	detail := conclusion
	if detail == "" {
		detail = "not green"
	}
	return fmt.Sprintf("required check %q is not green (%s)", name, detail)
}

// checkConclusionKind classifies a required check conclusion.
// Success is SUCCESS. Failure is FAILURE, CANCELLED, TIMED_OUT, or ACTION_REQUIRED.
// Every other conclusion, including an empty one, is pending.
func checkConclusionKind(conclusion string) string {
	switch strings.ToUpper(strings.TrimSpace(conclusion)) {
	case "SUCCESS":
		return "success"
	case "FAILURE", "CANCELLED", "TIMED_OUT", "ACTION_REQUIRED":
		return "failure"
	default:
		return "pending"
	}
}

// pendingOnly reports whether every problem is a required check that is
// pending or not reported yet, so --when-green can wait for it.
func pendingOnly(checks []github.QueueCheck, required, problems []string) bool {
	if len(problems) == 0 || len(required) == 0 {
		return false
	}
	byName := checksByName(checks)
	pending := make(map[string]struct{}, len(required))
	for _, name := range required {
		check, ok := byName[name]
		if !ok {
			pending[missingCheckProblem(name)] = struct{}{}
			continue
		}
		switch checkConclusionKind(check.Conclusion) {
		case "success":
			continue
		case "failure":
			return false
		default:
			pending[requiredCheckProblem(name, check.Conclusion)] = struct{}{}
		}
	}
	if len(pending) == 0 {
		return false
	}
	for _, problem := range problems {
		if _, ok := pending[problem]; !ok {
			return false
		}
	}
	return true
}

func failedRequiredReason(checks []github.QueueCheck, required []string) (string, bool) {
	byName := checksByName(checks)
	var reasons []string
	for _, name := range required {
		check, ok := byName[name]
		if !ok || checkConclusionKind(check.Conclusion) != "failure" {
			continue
		}
		reasons = append(reasons, fmt.Sprintf("required check %q failed (%s)", name, check.Conclusion))
	}
	if len(reasons) == 0 {
		return "", false
	}
	return strings.Join(reasons, "; "), true
}

// requeueGate decides whether the latest removal may be requeued. It returns
// nil when the latest queue event is not a removal. A refusal is a decision
// with Allowed false; err is only for a failure to decide.
func requeueGate(client removalAPI, pr github.QueuePullRequest, cfg *config.Config) (*requeueDecision, error) {
	settings := cfg.Settings
	event, ok := latestQueueEvent(pr.Events)
	if !ok || event.Kind != github.QueueEventRemoved {
		return nil, nil
	}
	class, matched, err := rcaClass(settings.RCAMarker, event.ID, pr.Comments)
	if err != nil {
		return nil, err
	}
	earlier, err := earlierRequeues(settings.RCAMarker, pr.Comments, pr.Events)
	if err != nil {
		return nil, err
	}
	d := &requeueDecision{Allowed: true, Used: earlier, Limit: settings.RequeueLimit()}
	if matched && class == "our-code" {
		at := event.CreatedAt.Format(time.RFC3339)
		moved, decided := ourCodeHeadMoved(pr, event)
		if !decided {
			return d.refuse("our-code removal; head change could not be decided"), nil
		}
		if !moved {
			return d.refuse(fmt.Sprintf("our-code removal and head %s unchanged since %s", pr.HeadSHA, at)), nil
		}
		d.Reason = fmt.Sprintf("our-code ejection at %s; head %s pushed since", at, pr.HeadSHA)
		fmt.Printf("%s, proceeding\n", d.Reason)
		return d, nil
	}
	if reason := requeueRefusal(class, matched, earlier, d.Limit); reason != "" {
		return d.refuse(reason), nil
	}
	d.Reason = fmt.Sprintf("%s RCA", class)
	if class == "manual" {
		return d, nil
	}
	if err := sameFailure(client, cfg, pr, event, d); err != nil {
		return nil, err
	}
	return d, nil
}

// gateLine names the latest queue event the RCA gate decides from and, for a
// removal, the RCA marker found for it.
func gateLine(pr github.QueuePullRequest, settings config.SettingsConfig) string {
	event, ok := latestQueueEvent(pr.Events)
	if !ok {
		return "latest queue event: none"
	}
	line := fmt.Sprintf("latest queue event: %s %s at %s", event.Kind, event.ID, event.CreatedAt.Format(time.RFC3339))
	if event.Kind != github.QueueEventRemoved {
		return line
	}
	class, matched, err := rcaClass(settings.RCAMarker, event.ID, pr.Comments)
	switch {
	case err != nil:
		return line + "; RCA: " + err.Error()
	case !matched:
		return line + "; RCA: none"
	}
	return fmt.Sprintf("%s; RCA: %s (matched)", line, class)
}

// ourCodeHeadMoved reports whether the PR head changed after an our-code
// removal. beforeCommit is not used: it names the merge-group commit the queue
// tested, not the PR head. A HeadRefForcePushedEvent after the removal counts
// as a change. Otherwise the head pushedDate decides, or committedDate when
// pushedDate is absent. decided is false when no date is available, or when
// the head was committed after the removal but pushed before it.
func ourCodeHeadMoved(pr github.QueuePullRequest, event github.QueueEvent) (moved, decided bool) {
	for _, at := range pr.ForcePushedAt {
		if at.After(event.CreatedAt) {
			return true, true
		}
	}
	committedAfter := pr.HeadCommittedAt.After(event.CreatedAt)
	if !pr.HeadPushedAt.IsZero() {
		pushedAfter := pr.HeadPushedAt.After(event.CreatedAt)
		if committedAfter && !pushedAfter {
			return false, false
		}
		return pushedAfter, true
	}
	if pr.HeadCommittedAt.IsZero() {
		return false, false
	}
	return committedAfter, true
}

// requeueRefusal returns why the RCA class refuses a requeue, or "" when it
// allows one.
func requeueRefusal(class string, matched bool, earlier, limit int) string {
	if !matched {
		return "no RCA comment for that removal"
	}
	if class == "our-code" {
		return "RCA class is our-code"
	}
	if class == "manual" {
		return ""
	}
	if class != "flake" && class != "infra" {
		return fmt.Sprintf("RCA class %q is not flake, infra, or manual", class)
	}
	if earlier > limit {
		return fmt.Sprintf("%d earlier requeues is greater than max_requeues %d", earlier, limit)
	}
	return ""
}

func rcaClass(marker, eventID string, comments []string) (string, bool, error) {
	class, matched, err := rcamarker.Parse(marker, eventID, comments)
	if err != nil {
		return "", false, fmt.Errorf("rca_marker: %w", err)
	}
	return class, matched, nil
}

func earlierRequeues(marker string, comments []string, events []github.QueueEvent) (int, error) {
	sorted := append([]github.QueueEvent(nil), events...)
	sortQueueEvents(sorted)
	seenRemoval := false
	lastRemovalID := ""
	count := 0
	for _, event := range sorted {
		if event.Kind == github.QueueEventRemoved {
			seenRemoval = true
			lastRemovalID = event.ID
			continue
		}
		if event.Kind != github.QueueEventAdded || !seenRemoval {
			continue
		}
		class, matched, err := rcaClass(marker, lastRemovalID, comments)
		if err != nil {
			return 0, err
		}
		if matched && (class == "our-code" || class == "manual") {
			continue
		}
		count++
	}
	return count, nil
}

func latestQueueEvent(events []github.QueueEvent) (github.QueueEvent, bool) {
	if len(events) == 0 {
		return github.QueueEvent{}, false
	}
	sorted := append([]github.QueueEvent(nil), events...)
	sortQueueEvents(sorted)
	return sorted[len(sorted)-1], true
}

func sortQueueEvents(events []github.QueueEvent) {
	sort.SliceStable(events, func(i, j int) bool {
		return events[i].CreatedAt.Before(events[j].CreatedAt)
	})
}

// flakeHint is one flake_signatures pattern, the text it matched, the
// failing output line holding that text, and where that line was found.
type flakeHint struct {
	Pattern  string `json:"pattern"`
	Match    string `json:"match"`
	Line     string `json:"line"`
	Location string `json:"location"`
}

func (h flakeHint) text() string {
	return fmt.Sprintf("Flake hint: pattern %q matched %q (not a verdict)", h.Pattern, h.Match)
}

// flakeHints matches flake_signatures only against the failing output: the
// log lines failure_log_pattern selects and the failing tests' output. Lines
// elsewhere in the log also appear in healthy runs, so they give no hint.
// Each pattern gives at most one hint, for its first matching line.
func flakeHints(cfg *config.Config, report failureReport) ([]flakeHint, error) {
	if len(cfg.Settings.FlakeSignatures) == 0 {
		return nil, nil
	}
	output, err := failingOutput(cfg, report)
	if err != nil {
		return nil, err
	}
	var hints []flakeHint
	for _, pattern := range cfg.Settings.FlakeSignatures {
		re, err := regexp.Compile(pattern)
		if err != nil {
			return nil, fmt.Errorf("flake_signatures %q: %w", pattern, err)
		}
		for _, line := range output {
			if hit := re.FindString(line.text); hit != "" {
				hints = append(hints, flakeHint{Pattern: pattern, Match: hit, Line: line.text, Location: line.location})
				break
			}
		}
	}
	return hints, nil
}

// outputLine is one line of failing output and where it was found.
type outputLine struct {
	text     string
	location string
}

func failingOutput(cfg *config.Config, report failureReport) ([]outputLine, error) {
	logLines, err := failureLogLines(cfg, report.Log)
	if err != nil {
		return nil, err
	}
	var output []outputLine
	for _, line := range logLines {
		output = append(output, outputLine{text: line.text, location: fmt.Sprintf("log line %d", line.number)})
	}
	for _, test := range report.TestOutputs {
		for _, line := range strings.Split(test.Output, "\n") {
			if line != "" {
				output = append(output, outputLine{text: line, location: "test " + test.Name})
			}
		}
	}
	return output, nil
}

func tailText(text string, n int) string {
	if text == "" || n <= 0 {
		return ""
	}
	lines := strings.Split(text, "\n")
	if len(lines) <= n {
		return text
	}
	return strings.Join(lines[len(lines)-n:], "\n")
}

func enqueuePR(client queueAPI, pr github.QueuePullRequest, includeDownstack bool) (enqueueKind, error) {
	err := client.EnqueuePullRequest(pr.NodeID)
	if err == nil {
		return enqueueDirect, nil
	}
	if !mutationRequiresAsyncMerge(err) {
		return 0, fmt.Errorf("enqueue %s: %w", pr.Ref.Raw, err)
	}
	if err := enqueueStack(client, pr, includeDownstack); err != nil {
		return 0, err
	}
	return enqueueAsync, nil
}

func mutationRequiresAsyncMerge(err error) bool {
	msg := strings.ToLower(err.Error())
	if !strings.Contains(msg, "stack") {
		return false
	}
	return strings.Contains(msg, "async") ||
		strings.Contains(msg, "merge-async") ||
		strings.Contains(msg, "stack merge") ||
		strings.Contains(msg, "sequentially")
}

// enqueueStack runs after GitHub asked for a stack merge. Stack metadata can
// lag a retarget, so a pull request on the default branch with no open pull
// request below it is enqueued with merge-async without --include-downstack.
func enqueueStack(client queueAPI, pr github.QueuePullRequest, includeDownstack bool) error {
	below, err := openBelowNumbers(client, pr)
	if err != nil {
		return err
	}
	onDefault := pr.BaseRefName == "" || pr.BaseRefName == pr.DefaultBranch
	if len(below) == 0 && onDefault {
		if !jsonOutput {
			fmt.Printf("no open pull requests below %s; enqueuing with merge-async\n", pr.Ref.Raw)
		}
	} else {
		if !jsonOutput {
			fmt.Printf("Open pull requests below %s:\n", pr.Ref.Raw)
			if len(below) == 0 {
				fmt.Println("(none)")
			}
			for _, number := range below {
				fmt.Printf("- %s/%s#%d\n", pr.Ref.Owner, pr.Ref.Repo, number)
			}
		}
		if !includeDownstack {
			return fmt.Errorf("refusing to enqueue stack %s without --include-downstack", pr.Ref.Raw)
		}
	}
	if err := client.MergeAsync(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, pr.HeadSHA); err != nil {
		return fmt.Errorf("merge-async %s: %w", pr.Ref.Raw, err)
	}
	return nil
}

// openBelowNumbers lists open pull requests below pr from the native stack,
// cross-checked against the open pull request whose head is pr's base branch.
func openBelowNumbers(client queueAPI, pr github.QueuePullRequest) ([]int, error) {
	downstack, err := client.OpenDownstack(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number)
	if err != nil {
		return nil, fmt.Errorf("open downstack for %s: %w", pr.Ref.Raw, err)
	}
	var native []int
	for _, item := range downstack {
		if item.State == "open" {
			native = append(native, item.Number)
		}
	}
	walked, found, err := branchBelow(client, pr)
	if err != nil || !found {
		return native, err
	}
	return mergeStackNumbers(native, []int{walked}), nil
}

var runCommand = defaultRunCommand

func defaultRunCommand(name string, args ...string) (string, error) {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(name, args...) //nolint:gosec // G204: git and gh take argument lists, not a shell
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("run %s: %w", name, err)
	}
	return string(out), nil
}

func resolvePullRef(raw string) (string, error) {
	raw = strings.TrimSpace(raw)
	if _, err := github.ParsePullRef(raw); err == nil {
		return raw, nil
	}
	number, ok := barePullNumber(raw)
	if !ok {
		return "", fmt.Errorf("invalid pull request %q", raw)
	}
	ownerRepo, err := githubOwnerRepo()
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s#%d", ownerRepo, number), nil
}

func barePullNumber(raw string) (int, bool) {
	if raw == "" {
		return 0, false
	}
	for _, r := range raw {
		if r < '0' || r > '9' {
			return 0, false
		}
	}
	number, err := strconv.Atoi(raw)
	if err != nil || number <= 0 || strconv.Itoa(number) != raw {
		return 0, false
	}
	return number, true
}

func githubOwnerRepo() (string, error) {
	if remote, ok := preferredRemoteURL(); ok {
		if owner, repo, parsed := parseGitHubRemote(remote); parsed {
			return owner + "/" + repo, nil
		}
	}
	return ownerRepoFromGH()
}

func preferredRemoteURL() (string, bool) {
	if remote, ok := gitRemoteURL("origin"); ok {
		return remote, true
	}
	out, err := runCommand("git", "remote")
	if err != nil {
		return "", false
	}
	name := firstRemoteName(out)
	if name == "" || name == "origin" {
		return "", false
	}
	return gitRemoteURL(name)
}

func gitRemoteURL(name string) (string, bool) {
	out, err := runCommand("git", "remote", "get-url", name)
	if err != nil {
		return "", false
	}
	out = strings.TrimSpace(out)
	if out == "" {
		return "", false
	}
	return out, true
}

func firstRemoteName(output string) string {
	for _, line := range strings.Split(output, "\n") {
		name := strings.TrimSpace(line)
		if name != "" {
			return name
		}
	}
	return ""
}

func ownerRepoFromGH() (string, error) {
	out, err := runCommand(ghcli.Bin(), "repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner")
	if err != nil {
		return "", fmt.Errorf("resolve repository from gh repo view: %w", err)
	}
	name := strings.TrimSpace(out)
	if _, _, ok := splitOwnerRepo(name); !ok {
		return "", fmt.Errorf("resolve repository from gh repo view: got %q", name)
	}
	return name, nil
}

func parseGitHubRemote(raw string) (string, string, bool) {
	raw = strings.TrimSpace(raw)
	if ownerRepo, ok := strings.CutPrefix(raw, "git@github.com:"); ok {
		ownerRepo = strings.TrimSuffix(strings.TrimSuffix(ownerRepo, "/"), ".git")
		return splitOwnerRepo(ownerRepo)
	}
	parsed, err := url.Parse(raw)
	if err != nil || parsed.Host != "github.com" {
		return "", "", false
	}
	path := strings.Trim(parsed.Path, "/")
	path = strings.TrimSuffix(path, ".git")
	return splitOwnerRepo(path)
}

func splitOwnerRepo(path string) (string, string, bool) {
	owner, repo, ok := strings.Cut(path, "/")
	if !ok || owner == "" || repo == "" || strings.Contains(repo, "/") {
		return "", "", false
	}
	return owner, repo, true
}

var errWatchUsage = errors.New("watch usage")

type watchExitError struct {
	code int
}

func (e *watchExitError) Error() string {
	return fmt.Sprintf("watch status %d", e.code)
}

func watchCommand(client watchAPI, cfg *config.Config, args []string) error {
	refs, timeout, err := parseWatchArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], watchUsage())
		os.Exit(1)
	}
	err = watchPullRequests(client, refs, watchOptions{
		timeout:  timeout,
		interval: defaultWatchInterval,
		asJSON:   jsonOutput,
		cfg:      cfg,
	})
	if errors.Is(err, errWatchUsage) {
		fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], watchUsage())
		os.Exit(1)
	}
	var status *watchExitError
	if errors.As(err, &status) {
		os.Exit(status.code)
	}
	return err
}

func parseWatchArgs(args []string) ([]string, time.Duration, error) {
	timeout := watchTimeout
	var refs []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		value, ok := timeoutArg(arg)
		if ok {
			if value == "" {
				next, exists := argAt(args, i+1)
				if !exists {
					return nil, 0, errWatchUsage
				}
				value = next
				i++
			}
			parsed, err := time.ParseDuration(value)
			if err != nil {
				return nil, 0, errWatchUsage
			}
			timeout = parsed
			continue
		}
		if strings.HasPrefix(arg, "-") {
			return nil, 0, errWatchUsage
		}
		refs = append(refs, arg)
	}
	return refs, timeout, nil
}

func argAt(args []string, i int) (string, bool) {
	if i < 0 || i >= len(args) {
		return "", false
	}
	return args[i], true
}

func timeoutArg(arg string) (string, bool) {
	for _, name := range []string{"--timeout=", "-timeout="} {
		if value, ok := strings.CutPrefix(arg, name); ok {
			return value, true
		}
	}
	if arg == "--timeout" || arg == "-timeout" {
		return "", true
	}
	return "", false
}

func watchExitCode(result string) int {
	switch result {
	case "merged":
		return 0
	case "failed_checks":
		return 2
	case "merge_conflict":
		return 3
	case "manual":
		return 4
	case "timeout":
		return 5
	default:
		return 1
	}
}

func watchPullRequests(client watchAPI, refs []string, opt watchOptions) error {
	if len(refs) == 0 {
		return errWatchUsage
	}
	resolved := make([]string, len(refs))
	for i, ref := range refs {
		got, err := resolvePullRef(ref)
		if err != nil {
			return err
		}
		resolved[i] = got
	}
	refs = resolved
	if opt.now == nil {
		opt.now = time.Now
	}
	if opt.sleep == nil {
		opt.sleep = time.Sleep
	}
	results := make([]watchResult, 0, len(refs))
	var exitCode int
	for _, ref := range refs {
		result, others, err := watchOne(client, ref, opt)
		if err != nil {
			return err
		}
		path, from := watchSettings(opt.cfg, ref)
		results = append(results, watchResult{
			PullRequest:  ref,
			Result:       result,
			Config:       path,
			SettingsFrom: from,
			RCAContext:   others,
		})
		if exitCode == 0 {
			exitCode = watchExitCode(result)
		}
	}
	if err := printWatchResults(results, opt.asJSON); err != nil {
		return err
	}
	if exitCode != 0 {
		return &watchExitError{code: exitCode}
	}
	return nil
}

// watchOne returns the watch result for ref and, for a removal, the RCA
// markers other tools wrote for it.
func watchOne(client watchAPI, ref string, opt watchOptions) (string, []rcaContext, error) {
	deadline := opt.now().Add(opt.timeout)
	for {
		pr, err := client.LoadPullRequest(ref)
		if err != nil {
			if wait, ok := watchTransientWait(err, opt.interval); ok {
				if !opt.now().Before(deadline) {
					return "timeout", nil, nil
				}
				opt.sleep(wait)
				continue
			}
			return "", nil, fmt.Errorf("watch %s: %w", ref, err)
		}
		// isInMergeQueue false is not enough: it flickers during a merge.
		if pr.Merged {
			return "merged", nil, nil
		}
		if event, ok := latestQueueEvent(pr.Events); ok && event.Kind == github.QueueEventRemoved {
			if !opt.asJSON && opt.cfg != nil {
				if err := printRemovalEvidence(client, opt.cfg, pr); err != nil {
					return "", nil, err
				}
			}
			if opt.cfg == nil {
				return event.Reason, nil, nil
			}
			marker := opt.cfg.ForRepository(pr.Ref.Owner, pr.Ref.Repo).Settings.RCAMarker
			others, err := otherRCAs(marker, event.ID, pr.Comments)
			if err != nil {
				return "", nil, err
			}
			return event.Reason, others, nil
		}
		if !opt.now().Before(deadline) {
			return "timeout", nil, nil
		}
		opt.sleep(opt.interval)
	}
}

var watchTransientHTTP = regexp.MustCompile(`(?i)\bhttp\s+50[24]\b`)

func watchTransientWait(err error, interval time.Duration) (time.Duration, bool) {
	if err == nil || !watchTransient(err.Error()) {
		return 0, false
	}
	wait := interval
	if wait <= 0 {
		wait = defaultWatchInterval
	}
	if retryAfter, ok := watchRetryAfter(err.Error()); ok && retryAfter > wait {
		wait = retryAfter
	}
	return wait, true
}

func watchTransient(text string) bool {
	lower := strings.ToLower(text)
	switch {
	case strings.Contains(lower, "secondary rate limit"), strings.Contains(lower, "secondary rate-limit"):
		return true
	case watchTransientHTTP.MatchString(lower):
		return true
	case strings.Contains(lower, "timeout"), strings.Contains(lower, "timed out"):
		return true
	case watchConnectionFailure(lower):
		return true
	default:
		return false
	}
}

// watchConnectionFailure reports a connect or read failure that never reached HTTP.
// DNS, refused, reset, TLS handshake, and EOF errors use the same backoff as HTTP 502.
func watchConnectionFailure(lower string) bool {
	switch {
	case strings.Contains(lower, "no such host"):
		return true
	case strings.Contains(lower, "connection refused"):
		return true
	case strings.Contains(lower, "connection reset"):
		return true
	case strings.Contains(lower, "tls handshake"), strings.Contains(lower, "tls: handshake"):
		return true
	case strings.Contains(lower, "eof"):
		return true
	default:
		return false
	}
}

func watchRetryAfter(text string) (time.Duration, bool) {
	const key = "retry-after"
	i := strings.Index(strings.ToLower(text), key)
	if i < 0 {
		return 0, false
	}
	rest := strings.TrimLeft(strings.TrimSpace(text[i+len(key):]), ":= \t")
	if rest == "" {
		return 0, false
	}
	field := rest
	if cut := strings.IndexAny(rest, " \t\r\n"); cut >= 0 {
		field = rest[:cut]
	}
	seconds, convErr := strconv.Atoi(field)
	if convErr != nil || seconds < 0 {
		return 0, false
	}
	return time.Duration(seconds) * time.Second, true
}

func watchSettings(cfg *config.Config, ref string) (string, string) {
	if cfg == nil {
		return "", config.SettingsFromGlobal
	}
	owner, repo := ownerRepoFromRef(ref)
	_, from := cfg.EffectiveSettings(owner, repo)
	return cfg.Path, from
}

func ownerRepoFromRef(ref string) (string, string) {
	head, _, ok := strings.Cut(ref, "#")
	if !ok {
		return "", ""
	}
	owner, repo, ok := splitOwnerRepo(head)
	if !ok {
		return "", ""
	}
	return owner, repo
}

func ownerRepoFromPullURL(raw string) (string, string, bool) {
	parsed, err := url.Parse(raw)
	if err != nil || parsed.Host == "" {
		return "", "", false
	}
	parts := strings.Split(strings.Trim(parsed.Path, "/"), "/")
	if len(parts) < 2 || parts[0] == "" || parts[1] == "" {
		return "", "", false
	}
	return parts[0], parts[1], true
}

func printWatchResults(results []watchResult, asJSON bool) error {
	if !asJSON {
		for _, result := range results {
			fmt.Printf("%s %s\n", result.PullRequest, result.Result)
		}
		return nil
	}
	encoded, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return fmt.Errorf("encode watch results: %w", err)
	}
	fmt.Printf("%s\n", encoded)
	return nil
}
