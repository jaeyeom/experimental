package gh

import (
	"context"
	"encoding/json"
	"fmt"
	"slices"
	"strconv"
)

const viewFields = "isDraft,labels,reviews,reviewRequests,title,body,baseRefName,url"

// View returns one pull request, including its draft flag, labels, reviewers, and body.
func (c *Client) View(ctx context.Context, repo string, number int) (PRView, error) {
	if !validRepo(repo) {
		return PRView{}, fmt.Errorf("gh pr view: expected repository owner/repo, found %q", repo)
	}
	if number < 1 {
		return PRView{}, fmt.Errorf("gh pr view: expected a pull request number >= 1, found %d", number)
	}
	result, err := c.execute(ctx, "pr", "view", strconv.Itoa(number), "--repo", repo, "--json", viewFields)
	if err != nil {
		return PRView{}, err
	}
	if result.ExitCode != 0 {
		return PRView{}, ghFailure(result)
	}
	view, err := decodeView(repo, number, []byte(result.Output))
	if err != nil {
		return PRView{}, err
	}
	return view, nil
}

func decodeView(repo string, number int, raw []byte) (PRView, error) {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return PRView{}, fmt.Errorf("gh pr view: expected a JSON object, found %s: %w", quoteSnippet(raw), err)
	}
	title, err := requiredString(obj, "title")
	if err != nil {
		return PRView{}, fmt.Errorf("gh pr view: %w", err)
	}
	body, err := decodeBody(obj)
	if err != nil {
		return PRView{}, fmt.Errorf("gh pr view: %w", err)
	}
	url, err := requiredString(obj, "url")
	if err != nil {
		return PRView{}, fmt.Errorf("gh pr view: %w", err)
	}
	draft, err := requiredBool(obj, "isDraft")
	if err != nil {
		return PRView{}, fmt.Errorf("gh pr view: %w", err)
	}
	base, err := requiredString(obj, "baseRefName")
	if err != nil {
		return PRView{}, fmt.Errorf("gh pr view: %w", err)
	}
	labels, err := decodeLabels(obj["labels"])
	if err != nil {
		return PRView{}, err
	}
	reviewers, err := decodeReviewers(obj)
	if err != nil {
		return PRView{}, err
	}
	return PRView{
		Repo: repo, Number: number, Title: title, Body: body, URL: url,
		Draft: draft, Base: base, Labels: labels, Reviewers: reviewers,
	}, nil
}

func decodeBody(obj map[string]json.RawMessage) (string, error) {
	raw, ok := obj["body"]
	if !ok {
		return "", fmt.Errorf("expected body, found missing")
	}
	if isNull(raw) {
		return "", nil
	}
	return jsonString(raw, "body")
}

func decodeLabels(raw json.RawMessage) ([]string, error) {
	nodes, err := objectArray(raw, "labels")
	if err != nil {
		return nil, fmt.Errorf("gh pr view: %w", err)
	}
	out := make([]string, 0, len(nodes))
	for i, node := range nodes {
		name, err := requiredString(node, "name")
		if err != nil {
			return nil, fmt.Errorf("gh pr view: labels[%d]: %w", i, err)
		}
		if name == "" {
			return nil, fmt.Errorf("gh pr view: labels[%d]: expected a name, found empty", i)
		}
		out = append(out, name)
	}
	return out, nil
}

func decodeReviewers(obj map[string]json.RawMessage) ([]string, error) {
	reviews, err := objectArray(obj["reviews"], "reviews")
	if err != nil {
		return nil, fmt.Errorf("gh pr view: %w", err)
	}
	requests, err := objectArray(obj["reviewRequests"], "reviewRequests")
	if err != nil {
		return nil, fmt.Errorf("gh pr view: %w", err)
	}
	seen := map[string]struct{}{}
	var names []string
	add := func(name string) {
		if _, ok := seen[name]; ok {
			return
		}
		seen[name] = struct{}{}
		names = append(names, name)
	}
	for i, review := range reviews {
		name, err := reviewAuthor(review)
		if err != nil {
			return nil, fmt.Errorf("gh pr view: reviews[%d]: %w", i, err)
		}
		add(name)
	}
	for i, request := range requests {
		name, err := requestName(request)
		if err != nil {
			return nil, fmt.Errorf("gh pr view: reviewRequests[%d]: %w", i, err)
		}
		add(name)
	}
	slices.Sort(names)
	return names, nil
}

func reviewAuthor(obj map[string]json.RawMessage) (string, error) {
	raw, ok := obj["author"]
	if !ok {
		return "", fmt.Errorf("expected author, found missing")
	}
	if isNull(raw) {
		return "unknown", nil
	}
	var author map[string]json.RawMessage
	if err := json.Unmarshal(raw, &author); err != nil {
		return "", fmt.Errorf("expected author object, found %s", quoteSnippet(raw))
	}
	login, err := requiredString(author, "login")
	if err != nil {
		return "", err
	}
	if login == "" {
		return "unknown", nil
	}
	return login, nil
}

func requestName(obj map[string]json.RawMessage) (string, error) {
	login, ok, err := optionalString(obj, "login")
	if err != nil {
		return "", err
	}
	if ok && login != "" {
		return login, nil
	}
	name, ok, err := optionalString(obj, "name")
	if err != nil {
		return "", err
	}
	if ok && name != "" {
		return name, nil
	}
	return "", fmt.Errorf("expected login or name, found neither")
}

func optionalString(obj map[string]json.RawMessage, key string) (string, bool, error) {
	raw, ok := obj[key]
	if !ok || isNull(raw) {
		return "", false, nil
	}
	value, err := jsonString(raw, key)
	if err != nil {
		return "", false, err
	}
	return value, true, nil
}

func objectArray(raw json.RawMessage, key string) ([]map[string]json.RawMessage, error) {
	if isNull(raw) {
		return nil, fmt.Errorf("expected %s, found missing", key)
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(raw, &nodes); err != nil {
		return nil, fmt.Errorf("expected %s array, found %s", key, quoteSnippet(raw))
	}
	out := make([]map[string]json.RawMessage, len(nodes))
	for i, node := range nodes {
		if isNull(node) {
			return nil, fmt.Errorf("%s[%d]: expected an object, found null", key, i)
		}
		var obj map[string]json.RawMessage
		if err := json.Unmarshal(node, &obj); err != nil {
			return nil, fmt.Errorf("%s[%d]: expected an object, found %s", key, i, quoteSnippet(node))
		}
		out[i] = obj
	}
	return out, nil
}
