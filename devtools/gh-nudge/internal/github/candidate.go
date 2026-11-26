package github

import (
	"cmp"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
	"github.com/jaeyeom/experimental/devtools/pkg/ghlimit"
)

// candidateBatchSize caps how many pull requests one batched GraphQL query
// requests for candidate state or PR-by-ref loading.
const candidateBatchSize = 50

// DefaultConcurrency is the bounded concurrency for per-PR gh fallback calls
// when gh-throttle is not constraining concurrency further.
const DefaultConcurrency = 4

// CandidateState holds the mergeable state and status check rollup for one PR.
type CandidateState struct {
	Mergeable         string
	StatusCheckRollup []models.StatusCheck
}

// SuggestedConcurrency returns the concurrency bound for per-PR gh calls,
// respecting gh-throttle's GH_THROTTLE_MAX_CONCURRENT limit (default 1 when
// gh-throttle is the active gh binary).
func SuggestedConcurrency() int {
	return suggestedConcurrency(os.Getenv, exec.LookPath, filepath.EvalSymlinks)
}

func suggestedConcurrency(
	getenv func(string) string,
	lookPath func(string) (string, error),
	evalSymlinks func(string) (string, error),
) int {
	limit := DefaultConcurrency
	if getenv("GH_THROTTLE_OFF") == "1" || getenv("GH_THROTTLE_HELD") != "" {
		return limit
	}
	isThrottle := isThrottledBinary(getenv, lookPath, evalSymlinks)
	if v := getenv(ghlimit.EnvMaxConcurrent); v != "" {
		if cfg, err := ghlimit.ConfigFromEnv(getenv); err == nil && cfg.MaxConcurrent >= 1 {
			return min(limit, cfg.MaxConcurrent)
		}
	}
	if isThrottle {
		return min(limit, ghlimit.DefaultConfig().MaxConcurrent)
	}
	return limit
}

func isThrottledBinary(
	getenv func(string) string,
	lookPath func(string) (string, error),
	evalSymlinks func(string) (string, error),
) bool {
	bin := strings.TrimSpace(getenv(ghcli.BinEnv))
	if bin == "" {
		bin = "gh"
	}
	if filepath.Base(bin) == "gh-throttle" {
		return true
	}
	if lookPath == nil {
		return false
	}
	path, err := lookPath(bin)
	if err != nil {
		return false
	}
	if filepath.Base(path) == "gh-throttle" {
		return true
	}
	if evalSymlinks == nil {
		return false
	}
	resolved, err := evalSymlinks(path)
	return err == nil && filepath.Base(resolved) == "gh-throttle"
}

// CandidateStates loads mergeable state and statusCheckRollup for each PR URL
// in batched GraphQL queries. An empty slice makes no calls.
func (c *Client) CandidateStates(prURLs []string) (map[string]CandidateState, error) {
	return c.CandidateStatesContext(context.Background(), prURLs)
}

// CandidateStatesContext is CandidateStates bounded by ctx.
func (c *Client) CandidateStatesContext(ctx context.Context, prURLs []string) (map[string]CandidateState, error) {
	if len(prURLs) == 0 {
		return map[string]CandidateState{}, nil
	}
	type urlRef struct {
		url string
		ref PullRef
	}
	seen := make(map[string]struct{}, len(prURLs))
	parsed := make([]urlRef, 0, len(prURLs))
	var fallback []string
	for _, u := range prURLs {
		if _, ok := seen[u]; ok {
			continue
		}
		seen[u] = struct{}{}
		owner, repo, number, err := parsePullRequestURL(u)
		if err != nil {
			fallback = append(fallback, u)
			continue
		}
		parsed = append(parsed, urlRef{
			url: u,
			ref: PullRef{Owner: owner, Repo: repo, Number: number, Raw: u},
		})
	}

	out := make(map[string]CandidateState, len(seen))
	for start := 0; start < len(parsed); start += candidateBatchSize {
		end := min(start+candidateBatchSize, len(parsed))
		chunk := parsed[start:end]
		refs := make([]PullRef, len(chunk))
		for i, item := range chunk {
			refs[i] = item.ref
		}
		if err := c.loadCandidateStateBatch(ctx, refs, out); err != nil {
			return nil, err
		}
	}
	for _, u := range fallback {
		mergeable, err := c.MergeableStateContext(ctx, u)
		if err != nil {
			return nil, err
		}
		checks, err := c.StatusChecksContext(ctx, u)
		if err != nil {
			return nil, fmt.Errorf("failed to get status checks for %s: %w", u, err)
		}
		out[u] = CandidateState{Mergeable: mergeable, StatusCheckRollup: checks}
	}
	return out, nil
}

// PullRequestsByRef loads pull requests identified by refs in batched GraphQL
// queries, including mergeable state and statusCheckRollup.
func (c *Client) PullRequestsByRef(refs []PullRef) ([]models.PullRequest, error) {
	return c.PullRequestsByRefContext(context.Background(), refs)
}

// PullRequestsByRefContext is PullRequestsByRef bounded by ctx.
func (c *Client) PullRequestsByRefContext(ctx context.Context, refs []PullRef) ([]models.PullRequest, error) {
	if len(refs) == 0 {
		return nil, nil
	}
	deduped := dedupePullRefs(refs)
	out := make([]models.PullRequest, 0, len(deduped))
	for start := 0; start < len(deduped); start += candidateBatchSize {
		end := min(start+candidateBatchSize, len(deduped))
		batch, err := c.loadPullRequestsBatch(ctx, deduped[start:end])
		if err != nil {
			return nil, err
		}
		out = append(out, batch...)
	}
	return out, nil
}

func dedupePullRefs(refs []PullRef) []PullRef {
	seen := make(map[string]struct{}, len(refs))
	out := make([]PullRef, 0, len(refs))
	for _, ref := range refs {
		key := fmt.Sprintf("%s/%s#%d", strings.ToLower(ref.Owner), strings.ToLower(ref.Repo), ref.Number)
		if _, ok := seen[key]; ok {
			continue
		}
		seen[key] = struct{}{}
		out = append(out, ref)
	}
	return out
}

const rollupGraphQLFields = `commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100){pageInfo{hasNextPage} nodes{__typename ... on CheckRun{name status conclusion startedAt completedAt} ... on StatusContext{context state}}}}}}}`

type candidateBatchAlias struct {
	name string
	key  string
	ref  PullRef
}

type candidateRollupJSON struct {
	Commits struct {
		Nodes []struct {
			Commit struct {
				StatusCheckRollup *struct {
					Contexts struct {
						PageInfo pageInfoJSON         `json:"pageInfo"`
						Nodes    []models.StatusCheck `json:"nodes"`
					} `json:"contexts"`
				} `json:"statusCheckRollup"`
			} `json:"commit"`
		} `json:"nodes"`
	} `json:"commits"`
}

type candidateStateNodeJSON struct {
	Mergeable string `json:"mergeable"`
	candidateRollupJSON
}

type pullRefNodeJSON struct {
	URL         string `json:"url"`
	Title       string `json:"title"`
	IsDraft     bool   `json:"isDraft"`
	Mergeable   string `json:"mergeable"`
	HeadRefName string `json:"headRefName"`
	Labels      struct {
		Nodes []models.Label `json:"nodes"`
	} `json:"labels"`
	ReviewRequests struct {
		Nodes []struct {
			RequestedReviewer *models.ReviewRequest `json:"requestedReviewer"`
		} `json:"nodes"`
	} `json:"reviewRequests"`
	candidateRollupJSON
}

type graphqlBatchEnvelope[T any] struct {
	Data map[string]*struct {
		PullRequest *T `json:"pullRequest"`
	} `json:"data"`
	Errors []struct {
		Type    string            `json:"type"`
		Message string            `json:"message"`
		Path    []json.RawMessage `json:"path"`
	} `json:"errors"`
}

func buildCandidateBatchQuery(refs []PullRef, fields string) (string, []candidateBatchAlias, error) {
	var b strings.Builder
	b.WriteString("query{")
	aliases := make([]candidateBatchAlias, len(refs))
	for i, ref := range refs {
		owner, err := gqlQuote(ref.Owner)
		if err != nil {
			return "", nil, err
		}
		name, err := gqlQuote(ref.Repo)
		if err != nil {
			return "", nil, err
		}
		alias := "p" + strconv.Itoa(i)
		key := ref.Raw
		if key == "" {
			key = ApprovalRefKey(ref)
		}
		aliases[i] = candidateBatchAlias{name: alias, key: key, ref: ref}
		fmt.Fprintf(&b, `%s:repository(owner:%s,name:%s){pullRequest(number:%d){%s}}`,
			alias, owner, name, ref.Number, fields)
	}
	b.WriteString("}")
	return b.String(), aliases, nil
}

func runCandidateBatchQuery[T any](
	ctx context.Context,
	c *Client,
	refs []PullRef,
	fields string,
	action string,
) (graphqlBatchEnvelope[T], []candidateBatchAlias, error) {
	if ctx != nil && ctx.Err() != nil {
		return graphqlBatchEnvelope[T]{}, nil, fmt.Errorf("%s: %w", action, ctx.Err())
	}
	query, aliases, err := buildCandidateBatchQuery(refs, fields)
	if err != nil {
		return graphqlBatchEnvelope[T]{}, nil, err
	}
	body, callErr := c.graphqlQueryContext(ctx, query, nil)
	if strings.TrimSpace(body.stdout) == "" {
		if gErr := graphqlCallError(action, body, callErr); gErr != nil {
			return graphqlBatchEnvelope[T]{}, nil, gErr
		}
		return graphqlBatchEnvelope[T]{}, nil, fmt.Errorf("%s: %w", action, body.parseError(io.ErrUnexpectedEOF))
	}
	var envelope graphqlBatchEnvelope[T]
	if err := body.decode(&envelope); err != nil {
		if gErr := graphqlCallError(action, body, callErr); gErr != nil {
			return graphqlBatchEnvelope[T]{}, nil, gErr
		}
		return graphqlBatchEnvelope[T]{}, nil, fmt.Errorf("%s: %w", action, err)
	}
	if err := checkBatchEnvelopeErrors(envelope, aliases, action, body, callErr); err != nil {
		return graphqlBatchEnvelope[T]{}, nil, err
	}
	return envelope, aliases, nil
}

func checkBatchEnvelopeErrors[T any](
	envelope graphqlBatchEnvelope[T],
	aliases []candidateBatchAlias,
	action string,
	body ghJSONOutput,
	callErr error,
) error {
	known := make(map[string]string, len(aliases))
	for _, a := range aliases {
		known[a.name] = a.key
	}
	if len(envelope.Errors) > 0 {
		item := envelope.Errors[0]
		msg := strings.TrimSpace(item.Message)
		if msg == "" {
			msg = cmp.Or(item.Type, "graphql error")
		}
		alias := pathAlias(item.Path)
		if key, ok := known[alias]; ok {
			return fmt.Errorf("%s %s: %s", action, key, msg)
		}
		return graphqlCallError(action, body, callErr)
	}
	if envelope.Data == nil {
		return graphqlCallError(action, body, callErr)
	}
	return nil
}

func (c *Client) loadCandidateStateBatch(ctx context.Context, refs []PullRef, out map[string]CandidateState) error {
	const action = "failed to get status checks"
	envelope, aliases, err := runCandidateBatchQuery[candidateStateNodeJSON](
		ctx, c, refs, "mergeable "+rollupGraphQLFields, action,
	)
	if err != nil {
		return err
	}
	for _, a := range aliases {
		node := batchPullRequestNode(envelope, a.name)
		if node == nil {
			return fmt.Errorf("%s for %s: pull request not found", action, a.key)
		}
		checks, err := c.resolveBatchChecks(ctx, a.key, node.candidateRollupJSON)
		if err != nil {
			return fmt.Errorf("%s for %s: %w", action, a.key, err)
		}
		out[a.key] = CandidateState{
			Mergeable:         node.Mergeable,
			StatusCheckRollup: checks,
		}
	}
	return nil
}

func (c *Client) loadPullRequestsBatch(ctx context.Context, refs []PullRef) ([]models.PullRequest, error) {
	const fields = "url title isDraft mergeable headRefName " +
		"labels(first:100){nodes{name}} " +
		"reviewRequests(first:100){nodes{requestedReviewer{__typename ... on User{login} ... on Team{name slug}}}} " +
		rollupGraphQLFields
	envelope, aliases, err := runCandidateBatchQuery[pullRefNodeJSON](ctx, c, refs, fields, "load pull request")
	if err != nil {
		return nil, err
	}
	out := make([]models.PullRequest, 0, len(aliases))
	for _, a := range aliases {
		node := batchPullRequestNode(envelope, a.name)
		if node == nil {
			return nil, fmt.Errorf("load pull request %s: not found", a.key)
		}
		prURL := cmp.Or(node.URL, a.ref.URL())
		checks, err := c.resolveBatchChecks(ctx, prURL, node.candidateRollupJSON)
		if err != nil {
			return nil, fmt.Errorf("load pull request %s: %w", a.key, err)
		}
		var reviewRequests []models.ReviewRequest
		for _, rr := range node.ReviewRequests.Nodes {
			if rr.RequestedReviewer != nil {
				reviewRequests = append(reviewRequests, *rr.RequestedReviewer)
			}
		}
		out = append(out, models.PullRequest{
			Title:             node.Title,
			URL:               prURL,
			ReviewRequests:    reviewRequests,
			Mergeable:         node.Mergeable,
			HeadRefName:       node.HeadRefName,
			StatusCheckRollup: checks,
			IsDraft:           node.IsDraft,
			Labels:            node.Labels.Nodes,
		})
	}
	return out, nil
}

func batchPullRequestNode[T any](envelope graphqlBatchEnvelope[T], alias string) *T {
	if envelope.Data == nil || envelope.Data[alias] == nil {
		return nil
	}
	return envelope.Data[alias].PullRequest
}

func (c *Client) resolveBatchChecks(ctx context.Context, prURL string, rollup candidateRollupJSON) ([]models.StatusCheck, error) {
	checks, hasNext := extractRollupChecks(rollup)
	if hasNext {
		// Fall back to gh pr view pagination when >100 check contexts exist.
		c.mu.Lock()
		delete(c.checks, prURL)
		c.mu.Unlock()
		return c.StatusChecksContext(ctx, prURL)
	}
	c.mu.Lock()
	if c.checks == nil {
		c.checks = make(map[string][]models.StatusCheck)
	}
	c.checks[prURL] = checks
	c.mu.Unlock()
	return checks, nil
}

func extractRollupChecks(rollup candidateRollupJSON) ([]models.StatusCheck, bool) {
	if len(rollup.Commits.Nodes) == 0 || rollup.Commits.Nodes[0].Commit.StatusCheckRollup == nil {
		return nil, false
	}
	contexts := rollup.Commits.Nodes[0].Commit.StatusCheckRollup.Contexts
	checks := make([]models.StatusCheck, 0, len(contexts.Nodes))
	for _, node := range contexts.Nodes {
		if node.Name == "" && node.Context == "" {
			continue
		}
		checks = append(checks, node)
	}
	return checks, contexts.PageInfo.HasNextPage
}
