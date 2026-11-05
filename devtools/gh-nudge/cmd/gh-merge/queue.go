package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"regexp"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

const logTailLines = 20

type queueAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	EnqueuePullRequest(nodeID string) error
	OpenDownstack(owner, repo string, number int) ([]github.StackPR, error)
	FailureEvidence(owner, repo, sha string) (github.FailureEvidence, error)
	LandedFirst(owner, repo, sha string, self int) (github.StackPR, bool, error)
	MergeAsync(owner, repo string, number int, sha string) error
}

type watchAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
}

type watchOptions struct {
	timeout  time.Duration
	interval time.Duration
	now      func() time.Time
	sleep    func(time.Duration)
	asJSON   bool
}

type watchResult struct {
	PullRequest string `json:"pullRequest"`
	Result      string `json:"result"`
}

func queuePullRequest(client queueAPI, cfg *config.Config, ref string, includeDownstack bool) error {
	pr, err := client.LoadPullRequest(ref)
	if err != nil {
		return fmt.Errorf("load pull request %s: %w", ref, err)
	}
	if err := printRemovalEvidence(client, cfg, pr); err != nil {
		return err
	}
	if err := refusePreflight(pr, cfg.Settings); err != nil {
		return err
	}
	if err := refuseRCA(pr, cfg.Settings); err != nil {
		return err
	}
	return enqueuePR(client, pr, includeDownstack)
}

func printRemovalEvidence(client queueAPI, cfg *config.Config, pr github.QueuePullRequest) error {
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
	fmt.Printf("Check run: %s\n", evidence.CheckRunURL)
	fmt.Printf("Job: %s\n", evidence.JobURL)
	fmt.Printf("Failing step log tail:\n%s\n", tailText(evidence.Log, logTailLines))
	hints, err := flakeHints(cfg.Settings.FlakeSignatures, evidence.Log)
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

func printLandedFirst(client queueAPI, pr github.QueuePullRequest, sha string) error {
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

func refusePreflight(pr github.QueuePullRequest, settings config.SettingsConfig) error {
	problems := preflightProblems(pr, settings.BlockMergeLabels, settings.RequiredChecks)
	if len(problems) == 0 {
		return nil
	}
	fmt.Printf("Refusing to enqueue %s:\n", pr.Ref.Raw)
	for _, problem := range problems {
		fmt.Printf("- %s\n", problem)
	}
	return fmt.Errorf("refusing to enqueue %s", pr.Ref.Raw)
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
	byName := make(map[string]github.QueueCheck, len(checks))
	for _, check := range checks {
		byName[check.Name] = check
	}
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
		detail := check.Conclusion
		if detail == "" {
			detail = "not green"
		}
		problems = append(problems, fmt.Sprintf("required check %q is not green (%s)", name, detail))
	}
	return problems
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
	earlier := earlierRequeues(pr.Events)
	return requeueAllowed(class, matched, earlier, settings.RequeueLimit())
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
	if marker == "" || !strings.Contains(marker, "{event_id}") || !strings.Contains(marker, "{class}") {
		return "", false, nil
	}
	pattern := regexp.QuoteMeta(marker)
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{event_id}"), regexp.QuoteMeta(eventID))
	pattern = strings.ReplaceAll(pattern, regexp.QuoteMeta("{class}"), `(our-code|flake|infra)`)
	re, err := regexp.Compile(pattern)
	if err != nil {
		return "", false, fmt.Errorf("rca_marker: %w", err)
	}
	found := ""
	for _, comment := range comments {
		match := re.FindStringSubmatch(comment)
		if match == nil {
			continue
		}
		if match[1] == "our-code" {
			return "our-code", true, nil
		}
		found = match[1]
	}
	if found == "" {
		return "", false, nil
	}
	return found, true, nil
}

func earlierRequeues(events []github.QueueEvent) int {
	sorted := append([]github.QueueEvent(nil), events...)
	sortQueueEvents(sorted)
	seenRemoval := false
	count := 0
	for _, event := range sorted {
		if event.Kind == github.QueueEventRemoved {
			seenRemoval = true
			continue
		}
		if event.Kind == github.QueueEventAdded && seenRemoval {
			count++
		}
	}
	return count
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

func enqueuePR(client queueAPI, pr github.QueuePullRequest, includeDownstack bool) error {
	err := client.EnqueuePullRequest(pr.NodeID)
	if err == nil {
		fmt.Printf("Enqueued %s.\n", pr.Ref.Raw)
		return nil
	}
	if !mutationRequiresAsyncMerge(err) {
		return fmt.Errorf("enqueue %s: %w", pr.Ref.Raw, err)
	}
	return enqueueStack(client, pr, includeDownstack)
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
	fmt.Printf("Open pull requests below %s:\n", pr.Ref.Raw)
	if len(below) == 0 {
		fmt.Println("(none)")
	}
	for _, item := range below {
		fmt.Printf("- %s/%s#%d\n", pr.Ref.Owner, pr.Ref.Repo, item.Number)
	}
	if !includeDownstack {
		return fmt.Errorf("refusing to enqueue stack %s without --include-downstack", pr.Ref.Raw)
	}
	if err := client.MergeAsync(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, pr.HeadSHA); err != nil {
		return fmt.Errorf("merge-async %s: %w", pr.Ref.Raw, err)
	}
	fmt.Printf("Enqueued %s with merge-async.\n", pr.Ref.Raw)
	return nil
}

func watchPullRequests(client watchAPI, refs []string, opt watchOptions) error {
	if opt.now == nil {
		opt.now = time.Now
	}
	if opt.sleep == nil {
		opt.sleep = time.Sleep
	}
	results := make([]watchResult, 0, len(refs))
	allMerged := true
	for _, ref := range refs {
		result, err := watchOne(client, ref, opt)
		if err != nil {
			return err
		}
		results = append(results, watchResult{PullRequest: ref, Result: result})
		if result != "merged" {
			allMerged = false
		}
	}
	if err := printWatchResults(results, opt.asJSON); err != nil {
		return err
	}
	if !allMerged {
		return errors.New("not every pull request merged")
	}
	return nil
}

func watchOne(client watchAPI, ref string, opt watchOptions) (string, error) {
	deadline := opt.now().Add(opt.timeout)
	for {
		pr, err := client.LoadPullRequest(ref)
		if err != nil {
			return "", fmt.Errorf("watch %s: %w", ref, err)
		}
		// isInMergeQueue false is not enough: it flickers during a merge.
		if pr.Merged {
			return "merged", nil
		}
		if event, ok := latestQueueEvent(pr.Events); ok && event.Kind == github.QueueEventRemoved {
			return "ejected:" + event.Reason, nil
		}
		if !opt.now().Before(deadline) {
			return "timeout", nil
		}
		opt.sleep(opt.interval)
	}
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
