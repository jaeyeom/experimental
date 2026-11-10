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
	return "Wait for pending required checks on the current head, then enqueue (timeout defaults to 2h)"
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

func queueUsage() string {
	return "queue <owner/repo#n|url|number>"
}

func watchUsage() string {
	return "watch <owner/repo#n|url|number>..."
}

func queuePullRequest(client queueAPI, cfg *config.Config, ref string, includeDownstack bool) error {
	pr, cfg, above, baseProblem, err := prepareQueue(client, cfg, ref)
	if err != nil {
		return err
	}
	problems := queueProblems(pr, cfg, baseProblem)
	if whenGreen && pendingOnly(pr.Checks, cfg.Settings.RequiredChecks, problems) {
		return waitForPendingChecks(client, pr, cfg, above, problems, includeDownstack)
	}
	return finishQueue(client, pr, cfg, above, problems, includeDownstack)
}

func prepareQueue(client queueAPI, cfg *config.Config, ref string) (github.QueuePullRequest, *config.Config, []int, string, error) {
	resolved, err := resolvePullRef(ref)
	if err != nil {
		return github.QueuePullRequest{}, nil, nil, "", err
	}
	pr, err := client.LoadPullRequest(resolved)
	if err != nil {
		return github.QueuePullRequest{}, nil, nil, "", fmt.Errorf("load pull request %s: %w", resolved, err)
	}
	cfg = cfg.ForRepository(pr.Ref.Owner, pr.Ref.Repo)
	above, baseProblem, err := reportStackedAbove(client, pr)
	if err != nil {
		return github.QueuePullRequest{}, nil, nil, "", err
	}
	if !jsonOutput {
		fmt.Println(stackedAboveLine(above))
	}
	if err := printRemovalEvidence(client, cfg, pr); err != nil {
		return github.QueuePullRequest{}, nil, nil, "", err
	}
	return pr, cfg, above, baseProblem, nil
}

func finishQueue(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string, includeDownstack bool) error {
	if len(problems) > 0 {
		return refuseWithProblems(pr, cfg, above, problems)
	}
	if err := refuseRCA(pr, cfg.Settings); err != nil {
		return err
	}
	lines := readyLines(pr, cfg.Settings)
	if dryRun {
		return writeQueueResult(pr, cfg, lines, above, "would-enqueue", fmt.Sprintf("Would enqueue %s.\n", pr.Ref.Raw))
	}
	kind, err := enqueuePR(client, pr, includeDownstack)
	if err != nil {
		return err
	}
	text := fmt.Sprintf("Enqueued %s.\n", pr.Ref.Raw)
	if kind == enqueueAsync {
		text = fmt.Sprintf("Enqueued %s with merge-async.\n", pr.Ref.Raw)
	}
	return writeQueueResult(pr, cfg, lines, above, "enqueued", text)
}

func waitForPendingChecks(client queueAPI, pr github.QueuePullRequest, cfg *config.Config, above []int, problems []string, includeDownstack bool) error {
	if err := refuseRCA(pr, cfg.Settings); err != nil {
		return err
	}
	if dryRun {
		return writeQueueResult(pr, cfg, problems, above, "would-wait", fmt.Sprintf("Would wait for %s.\n", pr.Ref.Raw))
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
			return abortQueue(pr, cfg, above, "timeout")
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
	above, baseProblem, err := reportStackedAbove(client, pr)
	if err != nil {
		return 0, "", nil, err
	}
	problems := queueProblems(pr, cfg, baseProblem)
	if len(problems) == 0 {
		return waitEnqueue, "", above, nil
	}
	if pendingOnly(pr.Checks, cfg.Settings.RequiredChecks, problems) {
		return waitPoll, "", above, nil
	}
	return waitAbort, strings.Join(problems, "; "), above, nil
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
		return fmt.Errorf("refusing to enqueue %s", pr.Ref.Raw)
	}
	for _, line := range effectiveConfigLines(cfg) {
		fmt.Println(line)
	}
	fmt.Printf("Refusing to enqueue %s:\n", pr.Ref.Raw)
	for _, problem := range problems {
		fmt.Printf("- %s\n", problem)
	}
	return fmt.Errorf("refusing to enqueue %s", pr.Ref.Raw)
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

func writeQueueResult(pr github.QueuePullRequest, cfg *config.Config, lines []string, above []int, result, text string) error {
	if lines == nil {
		lines = []string{}
	}
	if above == nil {
		above = []int{}
	}
	if jsonOutput {
		encoded, err := json.MarshalIndent(queueJSONResult{
			PullRequest:      pr.Ref.Raw,
			Readiness:        lines,
			Config:           cfg.Path,
			BlockMergeLabels: jsonStringList(cfg.Settings.BlockMergeLabels),
			RequiredChecks:   jsonStringList(cfg.Settings.RequiredChecks),
			Result:           result,
			StackedAbove:     above,
			SettingsFrom:     settingsSource(cfg),
		}, "", "  ")
		if err != nil {
			return fmt.Errorf("encode queue result: %w", err)
		}
		fmt.Printf("%s\n", encoded)
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
	fmt.Printf("Removal reason: %s\n", event.Reason)
	if event.CommitSHA == "" {
		return nil
	}
	evidence, err := client.FailureEvidence(pr.Ref.Owner, pr.Ref.Repo, event.CommitSHA)
	if err != nil {
		return fmt.Errorf("failure evidence for %s: %w", pr.Ref.Raw, err)
	}
	logText, err := printFailureEvidence(cfg, evidence)
	if err != nil {
		return err
	}
	hints, err := flakeHints(cfg.Settings.FlakeSignatures, logText)
	if err != nil {
		return err
	}
	for _, hint := range hints {
		fmt.Println(hint)
	}
	if event.Reason != "merge_conflict" {
		return nil
	}
	return printLandedFirst(client, pr, event.CommitSHA)
}

func printLandedFirst(client removalAPI, pr github.QueuePullRequest, sha string) error {
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
			problems = append(problems, fmt.Sprintf("required check %q is missing", name))
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

func pendingOnly(checks []github.QueueCheck, required, problems []string) bool {
	if len(problems) == 0 || len(required) == 0 {
		return false
	}
	byName := checksByName(checks)
	pending := make(map[string]struct{}, len(required))
	for _, name := range required {
		check, ok := byName[name]
		if !ok {
			return false
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

func refuseRCA(pr github.QueuePullRequest, settings config.SettingsConfig) error {
	event, ok := latestQueueEvent(pr.Events)
	if !ok || event.Kind != github.QueueEventRemoved {
		return nil
	}
	class, matched, err := rcaClass(settings.RCAMarker, event.ID, pr.Comments)
	if err != nil {
		return err
	}
	if matched && class == "our-code" {
		at := event.CreatedAt.Format(time.RFC3339)
		moved, decided := ourCodeHeadMoved(pr, event)
		if !decided {
			return errors.New("refusing to enqueue: our-code removal; head change could not be decided")
		}
		if !moved {
			return fmt.Errorf("refusing to enqueue: our-code removal and head %s unchanged since %s", pr.HeadSHA, at)
		}
		fmt.Printf("our-code ejection at %s; head %s pushed since, proceeding\n", at, pr.HeadSHA)
		return nil
	}
	earlier, err := earlierRequeues(settings.RCAMarker, pr.Comments, pr.Events)
	if err != nil {
		return err
	}
	return requeueAllowed(class, matched, earlier, settings.RequeueLimit())
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

func requeueAllowed(class string, matched bool, earlier, limit int) error {
	if !matched {
		return errors.New("refusing to enqueue: no RCA comment for that removal")
	}
	if class == "our-code" {
		return errors.New("refusing to enqueue: RCA class is our-code")
	}
	if class != "flake" && class != "infra" {
		return fmt.Errorf("refusing to enqueue: RCA class %q is not flake or infra", class)
	}
	if earlier > limit {
		return fmt.Errorf("refusing to enqueue: %d earlier requeues is greater than max_requeues %d", earlier, limit)
	}
	return nil
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
		if matched && class == "our-code" {
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

func flakeHints(patterns []string, log string) ([]string, error) {
	var hints []string
	for _, pattern := range patterns {
		re, err := regexp.Compile(pattern)
		if err != nil {
			return nil, fmt.Errorf("flake_signatures %q: %w", pattern, err)
		}
		hit := re.FindString(log)
		if hit == "" {
			continue
		}
		hints = append(hints, fmt.Sprintf("Flake hint: pattern %q matched %q (not a verdict)", pattern, hit))
	}
	return hints, nil
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

func enqueueStack(client queueAPI, pr github.QueuePullRequest, includeDownstack bool) error {
	below, err := client.OpenDownstack(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number)
	if err != nil {
		return fmt.Errorf("open downstack for %s: %w", pr.Ref.Raw, err)
	}
	if !jsonOutput {
		fmt.Printf("Open pull requests below %s:\n", pr.Ref.Raw)
		if len(below) == 0 {
			fmt.Println("(none)")
		}
		for _, item := range below {
			fmt.Printf("- %s/%s#%d\n", pr.Ref.Owner, pr.Ref.Repo, item.Number)
		}
	}
	if !includeDownstack {
		return fmt.Errorf("refusing to enqueue stack %s without --include-downstack", pr.Ref.Raw)
	}
	if err := client.MergeAsync(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, pr.HeadSHA); err != nil {
		return fmt.Errorf("merge-async %s: %w", pr.Ref.Raw, err)
	}
	return nil
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
	out, err := runCommand("gh", "repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner")
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
		result, err := watchOne(client, ref, opt)
		if err != nil {
			return err
		}
		path, from := watchSettings(opt.cfg, ref)
		results = append(results, watchResult{
			PullRequest:  ref,
			Result:       result,
			Config:       path,
			SettingsFrom: from,
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

func watchOne(client watchAPI, ref string, opt watchOptions) (string, error) {
	deadline := opt.now().Add(opt.timeout)
	for {
		pr, err := client.LoadPullRequest(ref)
		if err != nil {
			if wait, ok := watchTransientWait(err, opt.interval); ok {
				if !opt.now().Before(deadline) {
					return "timeout", nil
				}
				opt.sleep(wait)
				continue
			}
			return "", fmt.Errorf("watch %s: %w", ref, err)
		}
		// isInMergeQueue false is not enough: it flickers during a merge.
		if pr.Merged {
			return "merged", nil
		}
		if event, ok := latestQueueEvent(pr.Events); ok && event.Kind == github.QueueEventRemoved {
			if !opt.asJSON && opt.cfg != nil {
				if err := printRemovalEvidence(client, opt.cfg, pr); err != nil {
					return "", err
				}
			}
			return event.Reason, nil
		}
		if !opt.now().Before(deadline) {
			return "timeout", nil
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
