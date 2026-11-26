package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// MinGHVersion is the minimum gh CLI version required for `gh api graphql`.
var MinGHVersion = SemVer{Major: 2, Minor: 0, Patch: 0}

// SemVer is a parsed major.minor.patch version.
type SemVer struct {
	Major int
	Minor int
	Patch int
}

// String formats v as "major.minor.patch".
func (v SemVer) String() string {
	return fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
}

// AtLeast reports whether v is greater than or equal to floor.
func (v SemVer) AtLeast(floor SemVer) bool {
	if v.Major != floor.Major {
		return v.Major > floor.Major
	}
	if v.Minor != floor.Minor {
		return v.Minor > floor.Minor
	}
	return v.Patch >= floor.Patch
}

var ghVersionPattern = regexp.MustCompile(`(?m)\bgh version (\d+)\.(\d+)\.(\d+)`)

// ParseVersion parses the version line from `gh --version`.
func ParseVersion(output string) (SemVer, error) {
	m := ghVersionPattern.FindStringSubmatch(output)
	if len(m) != 4 {
		return SemVer{}, fmt.Errorf("gh version: expected 'gh version X.Y.Z', found %s", quoteSnippet([]byte(output)))
	}
	major, err := strconv.Atoi(m[1])
	if err != nil {
		return SemVer{}, fmt.Errorf("gh version: expected integer major, found %q", m[1])
	}
	minor, err := strconv.Atoi(m[2])
	if err != nil {
		return SemVer{}, fmt.Errorf("gh version: expected integer minor, found %q", m[2])
	}
	patch, err := strconv.Atoi(m[3])
	if err != nil {
		return SemVer{}, fmt.Errorf("gh version: expected integer patch, found %q", m[3])
	}
	return SemVer{Major: major, Minor: minor, Patch: patch}, nil
}

// Version runs `gh --version` and parses its semantic version.
func (c *Client) Version(ctx context.Context) (SemVer, error) {
	if ctx == nil {
		return SemVer{}, errors.New("gh version: expected a context, found nil")
	}
	result, err := c.execute(ctx, "--version")
	if err != nil {
		return SemVer{}, err
	}
	if result.ExitCode != 0 {
		return SemVer{}, ghFailure(result)
	}
	return ParseVersion(result.Output)
}

// PlanningStatus is one native GitHub repository issue status from
// repository.planningStatuses in the GraphQL API.
type PlanningStatus struct {
	ID       string
	Name     string
	Category string
	State    string
}

// planningStatusesQuery reads a repository's native issue planning statuses.
// Verified against GitHub's live GraphQL schema on 2026-11-24.
const planningStatusesQuery = "query($owner:String!,$name:String!){" +
	"repository(owner:$owner,name:$name){" +
	"planningStatuses(first:50){nodes{id name category{name state}}}}}"

// PlanningStatuses returns the native issue planning statuses available on repo.
func (c *Client) PlanningStatuses(ctx context.Context, repo string) ([]PlanningStatus, error) {
	if ctx == nil {
		return nil, errors.New("gh planning statuses: expected a context, found nil")
	}
	if !validRepo(repo) {
		return nil, fmt.Errorf("gh planning statuses: expected repository owner/repo, found %q", repo)
	}
	owner, name, _ := strings.Cut(repo, "/")
	result, err := c.execute(ctx,
		"api", "graphql",
		"-f", "query="+planningStatusesQuery,
		"-f", "owner="+owner,
		"-f", "name="+name,
	)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, ghFailure(result)
	}
	return decodePlanningStatuses([]byte(result.Output))
}

func decodePlanningStatuses(raw []byte) ([]PlanningStatus, error) {
	var top map[string]json.RawMessage
	if err := json.Unmarshal(raw, &top); err != nil {
		return nil, fmt.Errorf("gh planning statuses: expected JSON object, found %s: %w", quoteSnippet(raw), err)
	}
	if errs, ok := top["errors"]; ok {
		if err := graphqlFailure(errs); err != nil {
			return nil, err
		}
	}
	dataObj, err := rawObject(top["data"], "data")
	if err != nil {
		return nil, fmt.Errorf("gh planning statuses: %w", err)
	}
	repoObj, err := rawObject(dataObj["repository"], "repository")
	if err != nil {
		return nil, fmt.Errorf("gh planning statuses: %w", err)
	}
	psObj, err := rawObject(repoObj["planningStatuses"], "planningStatuses")
	if err != nil {
		return nil, fmt.Errorf("gh planning statuses: %w", err)
	}
	nodesRaw, ok := psObj["nodes"]
	if !ok || isNull(nodesRaw) {
		return nil, errors.New("gh planning statuses: expected planningStatuses.nodes array, found missing")
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(nodesRaw, &nodes); err != nil {
		return nil, fmt.Errorf("gh planning statuses: expected planningStatuses.nodes array, found %s", quoteSnippet(nodesRaw))
	}
	out := make([]PlanningStatus, 0, len(nodes))
	for i, nodeRaw := range nodes {
		st, err := decodePlanningStatusNode(nodeRaw)
		if err != nil {
			return nil, fmt.Errorf("gh planning statuses: nodes[%d]: %w", i, err)
		}
		out = append(out, st)
	}
	return out, nil
}

func decodePlanningStatusNode(raw json.RawMessage) (PlanningStatus, error) {
	obj, err := rawObject(raw, "planningStatus")
	if err != nil {
		return PlanningStatus{}, err
	}
	id, err := requiredString(obj, "id")
	if err != nil {
		return PlanningStatus{}, err
	}
	if id == "" {
		return PlanningStatus{}, errors.New("expected id, found empty")
	}
	name, err := requiredString(obj, "name")
	if err != nil {
		return PlanningStatus{}, err
	}
	if name == "" {
		return PlanningStatus{}, errors.New("expected name, found empty")
	}
	catObj, err := rawObject(obj["category"], "category")
	if err != nil {
		return PlanningStatus{}, err
	}
	catName, err := requiredString(catObj, "name")
	if err != nil {
		return PlanningStatus{}, fmt.Errorf("category: %w", err)
	}
	state, err := requiredString(catObj, "state")
	if err != nil {
		return PlanningStatus{}, fmt.Errorf("category: %w", err)
	}
	return PlanningStatus{
		ID:       id,
		Name:     name,
		Category: catName,
		State:    state,
	}, nil
}

func rawObject(raw json.RawMessage, what string) (map[string]json.RawMessage, error) {
	if isNull(raw) {
		return nil, fmt.Errorf("expected %s object, found missing", what)
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return nil, fmt.Errorf("expected %s object, found %s", what, quoteSnippet(raw))
	}
	return obj, nil
}
