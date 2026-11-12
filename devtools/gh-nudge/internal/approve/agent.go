package approve

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"slices"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

// Decisions an agent command can print.
const (
	AgentApprove = "approve"
	AgentDefer   = "defer"
)

// agentCondition is the condition name of a rule's agent command.
const agentCondition = "agent_command"

// maxStderrInReason bounds how much of a failed command's stderr is kept.
const maxStderrInReason = 300

// AgentResult is what a rule's agent command decided. A failure is a defer
// with the error as the reason.
type AgentResult struct {
	Decision string `json:"decision"`
	Reason   string `json:"reason"`
	// DurationMs is how long the command ran. On a cache hit, it is the
	// duration of the original run.
	DurationMs int64 `json:"durationMs"`
	// Cache is CacheHit, CacheMiss, or CacheBypass. Empty when the command
	// was not run, such as when the diff could not be read.
	Cache string `json:"agent_cache,omitempty"` //nolint:tagliatelle // key name from issue #688
	// CacheKey is the short hash of the cache key.
	CacheKey string `json:"agent_cache_key,omitempty"` //nolint:tagliatelle // pairs with agent_cache
	// CacheError is set when the decision could not be written to the cache.
	CacheError string `json:"agent_cache_error,omitempty"` //nolint:tagliatelle // pairs with agent_cache
	// CacheAge is how old a cache hit is.
	CacheAge time.Duration `json:"-"`
}

func (a AgentResult) String() string {
	duration := (time.Duration(a.DurationMs) * time.Millisecond).String()
	suffix := fmt.Sprintf("(%s, %s)", duration, cacheNote(a))
	if a.Reason == "" {
		return a.Decision + " " + suffix
	}
	return fmt.Sprintf("%s: %s %s", a.Decision, a.Reason, suffix)
}

// Summary is the one-line agent decision shown in the table, such as
// "agent: approve (cached 2d ago): docs only".
func (a AgentResult) Summary() string {
	line := fmt.Sprintf("agent: %s (%s)", a.Decision, cacheNote(a))
	if a.Reason != "" {
		line += ": " + a.Reason
	}
	return line
}

// AgentPR is the pull request metadata in AgentInput.
type AgentPR struct {
	Repo         string   `json:"repo"`
	Number       int      `json:"number"`
	URL          string   `json:"url"`
	Title        string   `json:"title"`
	Author       string   `json:"author"`
	HeadSHA      string   `json:"headSha"`
	Draft        bool     `json:"draft"`
	Additions    int      `json:"additions"`
	Deletions    int      `json:"deletions"`
	ChangedFiles int      `json:"changedFiles"`
	Labels       []string `json:"labels"`
}

// AgentInput is the JSON object written to an agent command's stdin.
type AgentInput struct {
	// Rule is the name of the rule whose agent command runs.
	Rule  string        `json:"rule"`
	PR    AgentPR       `json:"pr"`
	Files []models.File `json:"files"`
	// FilesError is set when the file list could not be read completely.
	FilesError string `json:"filesError,omitempty"`
	Diff       string `json:"diff"`
}

func newAgentInput(pr github.ApprovalPullRequest, diff string) AgentInput {
	return AgentInput{
		PR: AgentPR{
			Repo:         pr.Ref.Owner + "/" + pr.Ref.Repo,
			Number:       pr.Ref.Number,
			URL:          pr.URL,
			Title:        pr.Title,
			Author:       pr.Author,
			HeadSHA:      pr.HeadSHA,
			Draft:        pr.Draft,
			Additions:    pr.Additions,
			Deletions:    pr.Deletions,
			ChangedFiles: pr.ChangedFiles,
			Labels:       pr.Labels,
		},
		Files:      pr.Files,
		FilesError: pr.FilesErr,
		Diff:       diff,
	}
}

// CommandRunner runs argv with stdin. When ctx ends it kills the command and
// sets Result.TimedOut. A non-nil error means the command did not start.
type CommandRunner func(ctx context.Context, stdin []byte, argv []string) (remotecall.Result, error)

// ExecCommand is the CommandRunner that runs a real process, without a
// shell, in its own process group. A timeout kills the whole group.
func ExecCommand(ctx context.Context, stdin []byte, argv []string) (remotecall.Result, error) {
	var stdout, stderr bytes.Buffer
	res, err := remotecall.RunIO(ctx, bytes.NewReader(stdin), &stdout, &stderr, argv[0], argv[1:]...)
	res.Stdout, res.Stderr = stdout.String(), stderr.String()
	if err != nil {
		return res, fmt.Errorf("start %s: %w", argv[0], err)
	}
	return res, nil
}

// agentStdin is the JSON written to an agent command's stdin. The same input
// always gives the same bytes: fields keep their struct order, and files and
// labels are sorted. input's slices are not modified.
func agentStdin(input AgentInput) ([]byte, error) {
	input.Files = slices.Clone(input.Files)
	sort.SliceStable(input.Files, func(i, j int) bool { return input.Files[i].Path < input.Files[j].Path })
	input.PR.Labels = slices.Clone(input.PR.Labels)
	sort.Strings(input.PR.Labels)
	data, err := json.Marshal(input)
	if err != nil {
		return nil, fmt.Errorf("encode agent input: %w", err)
	}
	return data, nil
}

// runAgent runs rule's agent command with input on stdin. Every failure is a
// defer whose reason is the error. With a cache, an approve or defer the
// command printed is stored, and a stored one is returned without running
// the command. A nil cache or a rule with AgentCacheDisabled bypasses it.
func runAgent(run CommandRunner, cache *AgentCache, rule config.ApproveRule, input AgentInput) AgentResult {
	input.Rule = rule.Name
	stdin, err := agentStdin(input)
	if err != nil {
		return AgentResult{Decision: AgentDefer, Reason: err.Error()}
	}
	if cache == nil || rule.AgentCacheDisabled {
		result, _ := execAgent(run, rule, stdin)
		result.Cache = CacheBypass
		return result
	}
	key := agentCacheKey(stdin, rule.AgentCommand, rule.Name)
	short := key[:shortKeyLen]
	if entry, ok := cache.get(key); ok {
		return AgentResult{
			Decision:   entry.Decision,
			Reason:     entry.Reason,
			DurationMs: entry.DurationMs,
			Cache:      CacheHit,
			CacheKey:   short,
			CacheAge:   cache.Now().Sub(entry.Time),
		}
	}
	result, ok := execAgent(run, rule, stdin)
	result.Cache, result.CacheKey = CacheMiss, short
	if ok {
		entry := agentCacheEntry{Decision: result.Decision, Reason: result.Reason, Time: cache.Now(), DurationMs: result.DurationMs}
		if err := cache.put(key, entry); err != nil {
			result.CacheError = err.Error()
		}
	}
	return result
}

// execAgent runs the command. ok is true only when the command itself
// printed a well-formed approve or defer.
func execAgent(run CommandRunner, rule config.ApproveRule, stdin []byte) (AgentResult, bool) {
	start := time.Now()
	decision, reason, ok := agentDecision(run, rule, stdin)
	return AgentResult{Decision: decision, Reason: reason, DurationMs: time.Since(start).Milliseconds()}, ok
}

func agentDecision(run CommandRunner, rule config.ApproveRule, stdin []byte) (string, string, bool) {
	timeout := rule.AgentTimeout
	if timeout <= 0 {
		timeout = config.DefaultAgentTimeout
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	res, err := run(ctx, stdin, rule.AgentCommand)
	switch {
	case err != nil:
		return AgentDefer, "agent command: " + err.Error(), false
	case res.TimedOut:
		return AgentDefer, fmt.Sprintf("agent command timed out after %s", timeout), false
	case res.Code != 0:
		return AgentDefer, fmt.Sprintf("agent command exited %d%s", res.Code, stderrSuffix(res.Stderr)), false
	}
	var out struct {
		Decision string `json:"decision"`
		Reason   string `json:"reason"`
	}
	if err := json.Unmarshal([]byte(strings.TrimSpace(res.Stdout)), &out); err != nil {
		return AgentDefer, "agent command output is not one JSON object: " + err.Error(), false
	}
	switch out.Decision {
	case AgentApprove, AgentDefer:
		return out.Decision, out.Reason, true
	default:
		return AgentDefer, fmt.Sprintf("agent command decision %q is not approve or defer", out.Decision), false
	}
}

func stderrSuffix(stderr string) string {
	stderr = strings.TrimSpace(stderr)
	if stderr == "" {
		return ""
	}
	if len(stderr) > maxStderrInReason {
		stderr = "..." + stderr[len(stderr)-maxStderrInReason:]
	}
	return ": " + stderr
}
