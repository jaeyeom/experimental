package gh

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"
)

type searchPage struct {
	prs  []PR
	next string
}

func decodeSearch(raw []byte) (searchPage, error) {
	var top map[string]json.RawMessage
	if err := json.Unmarshal(raw, &top); err != nil {
		return searchPage{}, fmt.Errorf("gh search: expected JSON object, found %s: %w", quoteSnippet(raw), err)
	}
	if errs, ok := top["errors"]; ok {
		if err := graphqlFailure(errs); err != nil {
			return searchPage{}, err
		}
	}
	dataRaw, ok := top["data"]
	if !ok || isNull(dataRaw) {
		return searchPage{}, fmt.Errorf("gh search: expected data object, found missing")
	}
	var data map[string]json.RawMessage
	if err := json.Unmarshal(dataRaw, &data); err != nil {
		return searchPage{}, fmt.Errorf("gh search: expected data object, found %s", quoteSnippet(dataRaw))
	}
	searchRaw, ok := data["search"]
	if !ok || isNull(searchRaw) {
		return searchPage{}, fmt.Errorf("gh search: expected search object, found missing")
	}
	var search map[string]json.RawMessage
	if err := json.Unmarshal(searchRaw, &search); err != nil {
		return searchPage{}, fmt.Errorf("gh search: expected search object, found %s", quoteSnippet(searchRaw))
	}
	next, err := decodePageInfo(search["pageInfo"])
	if err != nil {
		return searchPage{}, err
	}
	prs, err := decodeNodes(search["nodes"])
	if err != nil {
		return searchPage{}, err
	}
	return searchPage{prs: prs, next: next}, nil
}

func graphqlFailure(raw json.RawMessage) error {
	if isNull(raw) {
		return nil
	}
	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err != nil {
		return fmt.Errorf("gh graphql: expected errors array, found %s", quoteSnippet(raw))
	}
	if len(items) == 0 {
		return nil
	}
	msgs := make([]string, 0, len(items))
	for i, item := range items {
		msg, err := graphqlMessage(item)
		if err != nil {
			return fmt.Errorf("gh graphql: errors[%d]: %w", i, err)
		}
		msgs = append(msgs, msg)
	}
	return fmt.Errorf("gh graphql: %s", strings.Join(msgs, "; "))
}

func graphqlMessage(raw json.RawMessage) (string, error) {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return "", fmt.Errorf("expected message, found %s", quoteSnippet(raw))
	}
	msg, err := requiredString(obj, "message")
	if err != nil {
		return "", err
	}
	if msg == "" {
		return "", fmt.Errorf("expected message, found empty")
	}
	return msg, nil
}

func decodePageInfo(raw json.RawMessage) (string, error) {
	if isNull(raw) {
		return "", fmt.Errorf("gh search: expected pageInfo, found missing")
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return "", fmt.Errorf("gh search: expected pageInfo object, found %s", quoteSnippet(raw))
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
		return "", fmt.Errorf("pageInfo: expected endCursor, found empty")
	}
	return cursor, nil
}

func decodeNodes(raw json.RawMessage) ([]PR, error) {
	if isNull(raw) {
		return nil, fmt.Errorf("gh search: expected nodes array, found missing")
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(raw, &nodes); err != nil {
		return nil, fmt.Errorf("gh search: expected nodes array, found %s", quoteSnippet(raw))
	}
	out := make([]PR, 0, len(nodes))
	for i, node := range nodes {
		pr, err := decodeNode(node)
		if err != nil {
			return nil, fmt.Errorf("nodes item %d: %w", i, err)
		}
		out = append(out, pr)
	}
	return out, nil
}

func decodeNode(raw json.RawMessage) (PR, error) {
	if isNull(raw) {
		return PR{}, fmt.Errorf("expected PullRequest, found null")
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return PR{}, fmt.Errorf("expected PullRequest, found %s", quoteSnippet(raw))
	}
	number, err := requiredPositiveInt(obj, "number")
	if err != nil {
		return PR{}, err
	}
	title, err := requiredString(obj, "title")
	if err != nil {
		return PR{}, err
	}
	url, err := requiredString(obj, "url")
	if err != nil {
		return PR{}, err
	}
	draft, err := requiredBool(obj, "isDraft")
	if err != nil {
		return PR{}, err
	}
	merged, err := requiredBool(obj, "merged")
	if err != nil {
		return PR{}, err
	}
	mergedAt, err := decodeMergedAt(obj, merged)
	if err != nil {
		return PR{}, err
	}
	created, closed, err := decodeOpenClose(obj)
	if err != nil {
		return PR{}, err
	}
	repo, err := decodeRepoField(obj, "repository")
	if err != nil {
		return PR{}, err
	}
	head, err := requiredString(obj, "headRefName")
	if err != nil {
		return PR{}, err
	}
	closing, err := decodeClosing(obj["closingIssuesReferences"])
	if err != nil {
		return PR{}, err
	}
	return PR{
		Repo:          repo,
		Number:        number,
		Title:         title,
		URL:           url,
		Draft:         draft,
		Merged:        merged,
		MergedAt:      mergedAt,
		CreatedAt:     created,
		ClosedAt:      closed,
		Head:          head,
		ClosingIssues: closing,
	}, nil
}

func decodeMergedAt(obj map[string]json.RawMessage, merged bool) (time.Time, error) {
	raw, ok := obj["mergedAt"]
	if !ok {
		return time.Time{}, fmt.Errorf("expected mergedAt, found missing")
	}
	if isNull(raw) {
		if merged {
			return time.Time{}, fmt.Errorf("expected mergedAt RFC3339 timestamp, found null")
		}
		return time.Time{}, nil
	}
	text, err := jsonString(raw, "mergedAt")
	if err != nil {
		return time.Time{}, err
	}
	parsed, err := time.Parse(time.RFC3339, text)
	if err != nil {
		parsed, err = time.Parse(time.RFC3339Nano, text)
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("expected mergedAt RFC3339 timestamp, found %q", text)
	}
	return parsed, nil
}

// decodeOpenClose reads createdAt, which must be a timestamp, and closedAt,
// which is null while the pull request is open.
func decodeOpenClose(obj map[string]json.RawMessage) (time.Time, time.Time, error) {
	raw, ok := obj["createdAt"]
	if !ok || isNull(raw) {
		return time.Time{}, time.Time{}, fmt.Errorf("expected createdAt, found missing")
	}
	created, err := decodeStamp(raw, "createdAt")
	if err != nil {
		return time.Time{}, time.Time{}, err
	}
	raw, ok = obj["closedAt"]
	if !ok {
		return time.Time{}, time.Time{}, fmt.Errorf("expected closedAt, found missing")
	}
	if isNull(raw) {
		return created, time.Time{}, nil
	}
	closed, err := decodeStamp(raw, "closedAt")
	if err != nil {
		return time.Time{}, time.Time{}, err
	}
	return created, closed, nil
}

func decodeStamp(raw json.RawMessage, key string) (time.Time, error) {
	text, err := jsonString(raw, key)
	if err != nil {
		return time.Time{}, err
	}
	parsed, err := time.Parse(time.RFC3339Nano, text)
	if err != nil {
		return time.Time{}, fmt.Errorf("expected %s RFC3339 timestamp, found %q", key, text)
	}
	return parsed, nil
}

func decodeClosing(raw json.RawMessage) ([]string, error) {
	if isNull(raw) {
		return nil, fmt.Errorf("expected closingIssuesReferences, found missing")
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return nil, fmt.Errorf("expected closingIssuesReferences object, found %s", quoteSnippet(raw))
	}
	nodesRaw, ok := obj["nodes"]
	if !ok || isNull(nodesRaw) {
		return nil, fmt.Errorf("expected closingIssuesReferences.nodes, found missing")
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(nodesRaw, &nodes); err != nil {
		return nil, fmt.Errorf("expected closingIssuesReferences.nodes array, found %s", quoteSnippet(nodesRaw))
	}
	out := make([]string, 0, len(nodes))
	for i, node := range nodes {
		id, err := decodeClosingID(node)
		if err != nil {
			return nil, fmt.Errorf("closingIssuesReferences.nodes item %d: %w", i, err)
		}
		out = append(out, id)
	}
	return out, nil
}

func decodeClosingID(raw json.RawMessage) (string, error) {
	if isNull(raw) {
		return "", fmt.Errorf("expected issue, found null")
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return "", fmt.Errorf("expected issue object, found %s", quoteSnippet(raw))
	}
	number, err := requiredPositiveInt(obj, "number")
	if err != nil {
		return "", err
	}
	repo, err := decodeRepoField(obj, "repository")
	if err != nil {
		return "", err
	}
	return repo + "#" + strconv.Itoa(number), nil
}

func decodeRepoField(obj map[string]json.RawMessage, key string) (string, error) {
	raw, ok := obj[key]
	if !ok || isNull(raw) {
		return "", fmt.Errorf("expected %s, found missing", key)
	}
	var repoObj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &repoObj); err != nil {
		return "", fmt.Errorf("expected %s object, found %s", key, quoteSnippet(raw))
	}
	name, err := requiredString(repoObj, "nameWithOwner")
	if err != nil {
		return "", fmt.Errorf("%s: %w", key, err)
	}
	if !validRepo(name) {
		return "", fmt.Errorf("expected repository owner/repo, found %q", name)
	}
	return name, nil
}

func requiredString(obj map[string]json.RawMessage, key string) (string, error) {
	raw, ok := obj[key]
	if !ok || isNull(raw) {
		return "", fmt.Errorf("expected %s, found missing", key)
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
	if !ok || isNull(raw) {
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
	if !ok || isNull(raw) {
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

func validRepo(name string) bool {
	owner, repo, ok := strings.Cut(name, "/")
	if !ok || owner == "" || repo == "" || strings.Contains(repo, "/") {
		return false
	}
	return !strings.ContainsAny(owner, " \t") && !strings.ContainsAny(repo, " \t")
}

func isNull(raw json.RawMessage) bool {
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
