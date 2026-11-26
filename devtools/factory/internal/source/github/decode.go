package github

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

type issuePage struct {
	nodes []json.RawMessage
	next  string
}

func decodeIssues(raw []byte) (issuePage, error) {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return issuePage{}, err
	}
	if err := graphqlFailure(top["errors"]); err != nil {
		return issuePage{}, err
	}
	repo, err := dataRepository(top["data"])
	if err != nil {
		return issuePage{}, err
	}
	issuesRaw, ok := repo["issues"]
	if !ok || isMissing(issuesRaw) {
		return issuePage{}, errors.New("expected issues, found missing")
	}
	issues, err := objectOf(issuesRaw, "issues")
	if err != nil {
		return issuePage{}, err
	}
	next, err := decodePageInfo(issues["pageInfo"])
	if err != nil {
		return issuePage{}, err
	}
	nodes, err := nodesOf(issues, "issues")
	if err != nil {
		return issuePage{}, err
	}
	return issuePage{nodes: nodes, next: next}, nil
}

func issueNodeID(raw json.RawMessage) (string, error) {
	obj, err := objectOf(raw, "issue")
	if err != nil {
		return "", err
	}
	nodeID, err := requiredString(obj, "id")
	if err != nil {
		return "", err
	}
	if nodeID == "" {
		return "", errors.New("expected id, found empty")
	}
	return nodeID, nil
}

// decodeMutationIDs reads a mutation payload and requires each idKey's id.
// A GraphQL errors array fails the call even when data is also present.
func decodeMutationIDs(raw []byte, field string, idKeys ...string) error {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return err
	}
	if err := graphqlFailure(top["errors"]); err != nil {
		return err
	}
	dataRaw, ok := top["data"]
	if !ok || isMissing(dataRaw) {
		return errors.New("expected data object, found missing")
	}
	data, err := objectOf(dataRaw, "data")
	if err != nil {
		return err
	}
	payloadRaw, ok := data[field]
	if !ok || isMissing(payloadRaw) {
		return fmt.Errorf("expected %s, found missing", field)
	}
	payload, err := objectOf(payloadRaw, field)
	if err != nil {
		return err
	}
	for _, key := range idKeys {
		if err := nestedID(payload, key); err != nil {
			return fmt.Errorf("%s: %w", field, err)
		}
	}
	return nil
}

func nestedID(obj map[string]json.RawMessage, key string) error {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return fmt.Errorf("expected %s, found missing", key)
	}
	child, err := objectOf(raw, key)
	if err != nil {
		return err
	}
	id, err := requiredString(child, "id")
	if err != nil {
		return fmt.Errorf("%s: %w", key, err)
	}
	if id == "" {
		return fmt.Errorf("expected %s.id, found empty", key)
	}
	return nil
}

func decodeGetIssue(raw []byte) (json.RawMessage, error) {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return nil, err
	}
	notFound, err := notFoundErrors(top["errors"])
	if err != nil {
		return nil, err
	}
	repo, err := dataRepository(top["data"])
	if err != nil {
		return nil, err
	}
	issue, ok := repo["issue"]
	if !ok || isMissing(issue) {
		return nil, source.ErrNotFound
	}
	if notFound {
		return nil, errors.New("github graphql: expected no errors, found NOT_FOUND")
	}
	return issue, nil
}

func dataRepository(raw json.RawMessage) (map[string]json.RawMessage, error) {
	if isMissing(raw) {
		return nil, errors.New("expected data object, found missing")
	}
	data, err := objectOf(raw, "data")
	if err != nil {
		return nil, err
	}
	repoRaw, ok := data["repository"]
	if !ok || isMissing(repoRaw) {
		return nil, errors.New("expected repository, found null")
	}
	return objectOf(repoRaw, "repository")
}

// notFoundErrors reports whether every GraphQL error is NOT_FOUND.
// Any other error is returned. No errors returns false.
func notFoundErrors(raw json.RawMessage) (bool, error) {
	if isMissing(raw) {
		return false, nil
	}
	items, err := errorItems(raw)
	if err != nil || len(items) == 0 {
		return false, err
	}
	msgs := make([]string, 0, len(items))
	onlyNotFound := true
	for i, item := range items {
		msg, typ, err := graphqlError(item)
		if err != nil {
			return false, fmt.Errorf("github graphql: errors[%d]: %w", i, err)
		}
		if typ != "NOT_FOUND" {
			onlyNotFound = false
		}
		msgs = append(msgs, msg)
	}
	if onlyNotFound {
		return true, nil
	}
	return false, fmt.Errorf("github graphql: %s", strings.Join(msgs, "; "))
}

func graphqlFailure(raw json.RawMessage) error {
	if isMissing(raw) {
		return nil
	}
	items, err := errorItems(raw)
	if err != nil || len(items) == 0 {
		return err
	}
	msgs := make([]string, 0, len(items))
	for i, item := range items {
		msg, _, err := graphqlError(item)
		if err != nil {
			return fmt.Errorf("github graphql: errors[%d]: %w", i, err)
		}
		msgs = append(msgs, msg)
	}
	return fmt.Errorf("github graphql: %s", strings.Join(msgs, "; "))
}

func errorItems(raw json.RawMessage) ([]json.RawMessage, error) {
	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err != nil {
		return nil, fmt.Errorf("github graphql: expected errors array, found %s", quoteSnippet(raw))
	}
	if items == nil {
		return nil, errors.New("github graphql: expected errors array, found null")
	}
	return items, nil
}

func graphqlError(raw json.RawMessage) (message, typ string, err error) {
	obj, err := objectOf(raw, "error")
	if err != nil {
		return "", "", err
	}
	message, err = requiredString(obj, "message")
	if err != nil {
		return "", "", err
	}
	if message == "" {
		return "", "", errors.New("expected message, found empty")
	}
	typ, err = optionalString(obj, "type")
	if err != nil {
		return "", "", err
	}
	return message, typ, nil
}

func decodePageInfo(raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "", errors.New("expected pageInfo, found missing")
	}
	obj, err := objectOf(raw, "pageInfo")
	if err != nil {
		return "", err
	}
	hasNext, err := requiredBool(obj, "hasNextPage")
	if err != nil {
		return "", fmt.Errorf("pageInfo: %w", err)
	}
	if !hasNext {
		return "", nil
	}
	cursor, err := requiredString(obj, "endCursor")
	if err != nil {
		return "", fmt.Errorf("pageInfo: %w", err)
	}
	if cursor == "" {
		return "", errors.New("pageInfo: expected endCursor, found empty")
	}
	return cursor, nil
}

func jsonObject(raw []byte, what string) (map[string]json.RawMessage, error) {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return nil, fmt.Errorf("expected %s JSON object, found %s: %w", what, quoteSnippet(raw), err)
	}
	if obj == nil {
		return nil, fmt.Errorf("expected %s JSON object, found null", what)
	}
	return obj, nil
}

func objectOf(raw json.RawMessage, what string) (map[string]json.RawMessage, error) {
	if isMissing(raw) {
		return nil, fmt.Errorf("expected %s, found missing", what)
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return nil, fmt.Errorf("expected %s object, found %s", what, quoteSnippet(raw))
	}
	if obj == nil {
		return nil, fmt.Errorf("expected %s object, found null", what)
	}
	return obj, nil
}

func nodesOf(obj map[string]json.RawMessage, what string) ([]json.RawMessage, error) {
	raw, ok := obj["nodes"]
	if !ok || isMissing(raw) {
		return nil, fmt.Errorf("expected %s.nodes, found missing", what)
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(raw, &nodes); err != nil {
		return nil, fmt.Errorf("expected %s.nodes array, found %s", what, quoteSnippet(raw))
	}
	if nodes == nil {
		return nil, fmt.Errorf("expected %s.nodes array, found null", what)
	}
	return nodes, nil
}

func namedNodes(raw json.RawMessage, what string) ([]json.RawMessage, error) {
	obj, err := objectOf(raw, what)
	if err != nil {
		return nil, err
	}
	return nodesOf(obj, what)
}

func requiredString(obj map[string]json.RawMessage, key string) (string, error) {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return "", fmt.Errorf("expected %s, found missing", key)
	}
	return jsonString(raw, key)
}

func optionalString(obj map[string]json.RawMessage, key string) (string, error) {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return "", nil
	}
	return jsonString(raw, key)
}

func jsonString(raw json.RawMessage, key string) (string, error) {
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return "", fmt.Errorf("expected %s string, found %s", key, quoteSnippet(raw))
	}
	return value, nil
}

func requiredBool(obj map[string]json.RawMessage, key string) (bool, error) {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return false, fmt.Errorf("expected %s, found missing", key)
	}
	var value bool
	if err := json.Unmarshal(raw, &value); err != nil {
		return false, fmt.Errorf("expected %s bool, found %s", key, quoteSnippet(raw))
	}
	return value, nil
}

func requiredPositiveInt(obj map[string]json.RawMessage, key string) (int, error) {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return 0, fmt.Errorf("expected %s, found missing", key)
	}
	var value int
	if err := json.Unmarshal(raw, &value); err != nil {
		return 0, fmt.Errorf("expected %s integer, found %s", key, quoteSnippet(raw))
	}
	if value < 1 {
		return 0, fmt.Errorf("expected %s >= 1, found %d", key, value)
	}
	return value, nil
}

func isMissing(raw json.RawMessage) bool {
	trimmed := bytes.TrimSpace(raw)
	return len(trimmed) == 0 || bytes.Equal(trimmed, []byte("null"))
}

func quoteSnippet(raw []byte) string {
	const maxSnippet = 180
	text := strings.TrimSpace(string(raw))
	if len(text) > maxSnippet {
		text = text[:maxSnippet] + "..."
	}
	return strconv.Quote(text)
}

func validRepo(name string) bool {
	owner, repo, ok := strings.Cut(name, "/")
	if !ok || owner == "" || repo == "" || strings.Contains(repo, "/") {
		return false
	}
	return !strings.ContainsAny(owner, " \t") && !strings.ContainsAny(repo, " \t")
}

// decodePlanningStatuses reads repository.planningStatuses.nodes as a name->id map
// and preserves their display order in names.
func decodePlanningStatuses(raw []byte) (ids map[string]string, names []string, err error) {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return nil, nil, err
	}
	if err := graphqlFailure(top["errors"]); err != nil {
		return nil, nil, err
	}
	repo, err := dataRepository(top["data"])
	if err != nil {
		return nil, nil, err
	}
	psRaw, ok := repo["planningStatuses"]
	if !ok || isMissing(psRaw) {
		return nil, nil, errors.New("expected planningStatuses, found missing")
	}
	nodes, err := namedNodes(psRaw, "planningStatuses")
	if err != nil {
		return nil, nil, err
	}
	ids = make(map[string]string, len(nodes))
	names = make([]string, 0, len(nodes))
	for i, nodeRaw := range nodes {
		obj, err := objectOf(nodeRaw, fmt.Sprintf("planningStatuses[%d]", i))
		if err != nil {
			return nil, nil, err
		}
		id, err := requiredString(obj, "id")
		if err != nil {
			return nil, nil, fmt.Errorf("planningStatuses[%d]: %w", i, err)
		}
		if id == "" {
			return nil, nil, fmt.Errorf("planningStatuses[%d]: expected id, found empty", i)
		}
		name, err := requiredString(obj, "name")
		if err != nil {
			return nil, nil, fmt.Errorf("planningStatuses[%d]: %w", i, err)
		}
		if name == "" {
			return nil, nil, fmt.Errorf("planningStatuses[%d]: expected name, found empty", i)
		}
		ids[name] = id
		names = append(names, name)
	}
	return ids, names, nil
}

// decodeSetIssueStatus validates a setIssueStatus mutation response and checks
// that the returned issue status matches wantStatus.
func decodeSetIssueStatus(raw []byte, wantStatus string) error {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return err
	}
	if err := graphqlFailure(top["errors"]); err != nil {
		return err
	}
	dataRaw, ok := top["data"]
	if !ok || isMissing(dataRaw) {
		return errors.New("expected data object, found missing")
	}
	data, err := objectOf(dataRaw, "data")
	if err != nil {
		return err
	}
	payload, err := objectOf(data["setIssueStatus"], "setIssueStatus")
	if err != nil {
		return err
	}
	issueObj, err := objectOf(payload["issue"], "issue")
	if err != nil {
		return fmt.Errorf("setIssueStatus: %w", err)
	}
	if id, err := requiredString(issueObj, "id"); err != nil {
		return fmt.Errorf("setIssueStatus: issue: %w", err)
	} else if id == "" {
		return errors.New("setIssueStatus: expected issue.id, found empty")
	}
	statusObj, err := objectOf(issueObj["status"], "status")
	if err != nil {
		return fmt.Errorf("setIssueStatus: issue: %w", err)
	}
	got, err := requiredString(statusObj, "name")
	if err != nil {
		return fmt.Errorf("setIssueStatus: issue.status: %w", err)
	}
	if got != wantStatus {
		return fmt.Errorf("setIssueStatus: expected status %q, found %q", wantStatus, got)
	}
	return nil
}

// decodeComments reads the JSON output of `gh issue view --json comments`.
func decodeComments(raw []byte) ([]source.ClaimComment, error) {
	top, err := jsonObject(raw, "comments response")
	if err != nil {
		return nil, err
	}
	listRaw, ok := top["comments"]
	if !ok || isMissing(listRaw) {
		return nil, errors.New("expected comments array, found missing")
	}
	var items []json.RawMessage
	if err := json.Unmarshal(listRaw, &items); err != nil {
		return nil, fmt.Errorf("expected comments array, found %s", quoteSnippet(listRaw))
	}
	if items == nil {
		return nil, errors.New("expected comments array, found null")
	}
	out := make([]source.ClaimComment, 0, len(items))
	for i, itemRaw := range items {
		obj, err := objectOf(itemRaw, fmt.Sprintf("comments[%d]", i))
		if err != nil {
			return nil, err
		}
		body, err := requiredString(obj, "body")
		if err != nil {
			return nil, fmt.Errorf("comments[%d]: %w", i, err)
		}
		createdRaw, err := requiredString(obj, "createdAt")
		if err != nil {
			return nil, fmt.Errorf("comments[%d]: %w", i, err)
		}
		createdAt, err := time.Parse(time.RFC3339Nano, createdRaw)
		if err != nil {
			return nil, fmt.Errorf("comments[%d].createdAt: expected RFC3339 timestamp, found %q", i, createdRaw)
		}
		var author string
		if authorRaw, ok := obj["author"]; ok && !isMissing(authorRaw) {
			authorObj, err := objectOf(authorRaw, fmt.Sprintf("comments[%d].author", i))
			if err != nil {
				return nil, err
			}
			author, err = optionalString(authorObj, "login")
			if err != nil {
				return nil, fmt.Errorf("comments[%d].author: %w", i, err)
			}
		}
		out = append(out, source.ClaimComment{
			Author:    author,
			CreatedAt: createdAt,
			Body:      body,
		})
	}
	return out, nil
}
