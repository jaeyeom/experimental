package github

import (
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func mapNodes(cfg *config.Source, nodes []json.RawMessage) ([]source.Item, error) {
	items := make([]source.Item, 0, len(nodes))
	for i, node := range nodes {
		if isMissing(node) {
			return nil, fmt.Errorf("issues node %d: expected an issue, found null", i)
		}
		item, err := mapNode(cfg, node)
		if err != nil {
			return nil, err
		}
		items = append(items, item)
	}
	return items, nil
}

func mapNode(cfg *config.Source, raw json.RawMessage) (source.Item, error) {
	obj, id, err := issueIdentity(cfg, raw)
	if err != nil {
		return source.Item{}, err
	}
	item, err := issueText(id, obj)
	if err != nil {
		return source.Item{}, err
	}
	if err := issueRelations(id, obj, &item); err != nil {
		return source.Item{}, err
	}
	fields, err := mapIssueFieldValues(id, obj["issueFieldValues"])
	if err != nil {
		return source.Item{}, err
	}
	item.Source = cfg.Name
	item.ID = id
	if cfg.PriorityField != "" {
		if val, ok := fields.singleSelect[cfg.PriorityField]; ok {
			item.Priority = priorityFromField(id, val, cfg)
		} else {
			item.Priority = priorityOf(item.Labels, cfg.PriorityLabels)
		}
	} else {
		item.Priority = priorityOf(item.Labels, cfg.PriorityLabels)
	}
	// A label whose date does not parse defers nothing; doctor reports it.
	labelNotBefore, _ := source.LabelNotBefore(item.Labels, cfg.DeferLabelPrefix)
	if cfg.DeferField != "" {
		if fieldDate, ok := fields.dates[cfg.DeferField]; ok {
			item.NotBefore = mergeNotBefore(&fieldDate, labelNotBefore)
		} else {
			item.NotBefore = labelNotBefore
		}
	} else {
		item.NotBefore = labelNotBefore
	}
	if cfg.DueField != "" {
		if dueDate, ok := fields.dates[cfg.DueField]; ok {
			d := dueDate
			item.Due = &d
		}
	}
	return item, nil
}

func issueIdentity(cfg *config.Source, raw json.RawMessage) (map[string]json.RawMessage, string, error) {
	if cfg == nil {
		return nil, "", errors.New("github issue: expected a config, found nil")
	}
	if !validRepo(cfg.Repo) {
		return nil, "", fmt.Errorf("github: expected repo owner/name, found %q", cfg.Repo)
	}
	obj, err := objectOf(raw, "issue")
	if err != nil {
		return nil, "", fmt.Errorf("github issue: %w", err)
	}
	number, err := requiredPositiveInt(obj, "number")
	if err != nil {
		return nil, "", fmt.Errorf("github issue: %w", err)
	}
	id := source.GitHubID(cfg.Repo, number)
	if err := requireNodeID(id, obj); err != nil {
		return nil, "", err
	}
	return obj, id, nil
}

func requireNodeID(id string, obj map[string]json.RawMessage) error {
	nodeID, err := requiredString(obj, "id")
	if err != nil {
		return fmt.Errorf("github issue %s: %w", id, err)
	}
	if nodeID == "" {
		return fmt.Errorf("github issue %s: expected id, found empty", id)
	}
	return nil
}

func issueText(id string, obj map[string]json.RawMessage) (source.Item, error) {
	title, err := requiredString(obj, "title")
	if err != nil {
		return source.Item{}, fmt.Errorf("github issue %s: %w", id, err)
	}
	url, err := requiredURL(id, obj)
	if err != nil {
		return source.Item{}, err
	}
	state, done, err := mapState(id, obj)
	if err != nil {
		return source.Item{}, err
	}
	typ, err := mapType(id, obj["issueType"])
	if err != nil {
		return source.Item{}, err
	}
	parent, err := mapParent(id, obj["parent"])
	if err != nil {
		return source.Item{}, err
	}
	return source.Item{
		Title:  title,
		URL:    url,
		Type:   typ,
		State:  state,
		Done:   done,
		Parent: parent,
	}, nil
}

func requiredURL(id string, obj map[string]json.RawMessage) (string, error) {
	url, err := requiredString(obj, "url")
	if err != nil {
		return "", fmt.Errorf("github issue %s: %w", id, err)
	}
	if url == "" {
		return "", fmt.Errorf("github issue %s: expected url, found empty", id)
	}
	return url, nil
}

func issueRelations(id string, obj map[string]json.RawMessage, item *source.Item) error {
	labels, err := mapLabels(id, obj["labels"])
	if err != nil {
		return err
	}
	assignee, err := mapAssignee(id, obj["assignees"])
	if err != nil {
		return err
	}
	blocked, err := mapBlocked(id, obj["blockedBy"])
	if err != nil {
		return err
	}
	created, updated, err := issueTimes(id, obj)
	if err != nil {
		return err
	}
	due, err := mapDue(id, obj["milestone"])
	if err != nil {
		return err
	}
	item.Labels = labels
	item.Assignee = assignee
	item.BlockedBy = blocked
	item.Created = created
	item.Updated = updated
	item.Due = due
	return nil
}

// mapDue reads milestone.dueOn. A missing milestone, a null milestone, or a
// null dueOn leaves Due nil. A present dueOn must be an RFC3339 timestamp.
// The stored value is that timestamp's UTC calendar date.
func mapDue(id string, raw json.RawMessage) (*time.Time, error) {
	if isMissing(raw) {
		return nil, nil
	}
	obj, err := objectOf(raw, "milestone")
	if err != nil {
		return nil, fmt.Errorf("github issue %s: %w", id, err)
	}
	dueRaw, ok := obj["dueOn"]
	if !ok || isMissing(dueRaw) {
		return nil, nil
	}
	text, err := jsonString(dueRaw, "dueOn")
	if err != nil {
		return nil, fmt.Errorf("github issue %s: milestone: %w", id, err)
	}
	return parseGitHubDue(id, text)
}

func parseGitHubDue(id, text string) (*time.Time, error) {
	if text == "" {
		return nil, fmt.Errorf("github issue %s: expected milestone.dueOn RFC3339 timestamp, found empty", id)
	}
	parsed, err := time.Parse(time.RFC3339, text)
	if err != nil {
		parsed, err = time.Parse(time.RFC3339Nano, text)
	}
	if err != nil {
		return nil, fmt.Errorf("github issue %s: expected milestone.dueOn RFC3339 timestamp, found %q", id, text)
	}
	due := utcDate(parsed)
	return &due, nil
}

func utcDate(t time.Time) time.Time {
	t = t.UTC()
	return time.Date(t.Year(), t.Month(), t.Day(), 0, 0, 0, 0, time.UTC)
}

func mapState(id string, obj map[string]json.RawMessage) (string, bool, error) {
	state, err := requiredString(obj, "state")
	if err != nil {
		return "", false, fmt.Errorf("github issue %s: %w", id, err)
	}
	var done bool
	switch state {
	case "OPEN":
		done = false
	case "CLOSED":
		done = true
	default:
		return "", false, fmt.Errorf("github issue %s: expected state OPEN or CLOSED, found %q", id, state)
	}
	statusName, err := mapStatusName(id, obj["status"])
	if err != nil {
		return "", false, err
	}
	if statusName != "" {
		return statusName, done, nil
	}
	return state, done, nil
}

func mapStatusName(id string, raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "", nil
	}
	obj, err := objectOf(raw, "status")
	if err != nil {
		return "", fmt.Errorf("github issue %s: %w", id, err)
	}
	name, err := requiredString(obj, "name")
	if err != nil {
		return "", fmt.Errorf("github issue %s: status: %w", id, err)
	}
	if name == "" {
		return "", fmt.Errorf("github issue %s: expected status.name, found empty", id)
	}
	return name, nil
}

func mapType(id string, raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "Issue", nil
	}
	obj, err := objectOf(raw, "issueType")
	if err != nil {
		return "", fmt.Errorf("github issue %s: %w", id, err)
	}
	name, err := requiredString(obj, "name")
	if err != nil {
		return "", fmt.Errorf("github issue %s: issueType: %w", id, err)
	}
	if name == "" {
		return "", fmt.Errorf("github issue %s: expected issueType.name, found empty", id)
	}
	return name, nil
}

func mapParent(id string, raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "", nil
	}
	obj, err := objectOf(raw, "parent")
	if err != nil {
		return "", fmt.Errorf("github issue %s: %w", id, err)
	}
	number, err := requiredPositiveInt(obj, "number")
	if err != nil {
		return "", fmt.Errorf("github issue %s: parent: %w", id, err)
	}
	repo, err := repoName(obj, "repository")
	if err != nil {
		return "", fmt.Errorf("github issue %s: parent: %w", id, err)
	}
	return source.GitHubID(repo, number), nil
}

func mapLabels(id string, raw json.RawMessage) ([]string, error) {
	nodes, err := namedNodes(raw, "labels")
	if err != nil {
		return nil, fmt.Errorf("github issue %s: %w", id, err)
	}
	if len(nodes) >= maxLabels {
		return nil, fmt.Errorf("github issue %s: expected fewer than %d labels, found %d", id, maxLabels, len(nodes))
	}
	labels := make([]string, 0, len(nodes))
	for i, node := range nodes {
		name, err := labelName(node)
		if err != nil {
			return nil, fmt.Errorf("github issue %s: labels node %d: %w", id, i, err)
		}
		labels = append(labels, name)
	}
	return labels, nil
}

func labelName(raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "", errors.New("expected a label, found null")
	}
	obj, err := objectOf(raw, "label")
	if err != nil {
		return "", err
	}
	name, err := requiredString(obj, "name")
	if err != nil {
		return "", err
	}
	if name == "" {
		return "", errors.New("expected name, found empty")
	}
	return name, nil
}

func mapAssignee(id string, raw json.RawMessage) (string, error) {
	nodes, err := namedNodes(raw, "assignees")
	if err != nil {
		return "", fmt.Errorf("github issue %s: %w", id, err)
	}
	if len(nodes) == 0 {
		return "", nil
	}
	logins := make([]string, 0, len(nodes))
	for i, node := range nodes {
		login, err := assigneeLogin(node)
		if err != nil {
			return "", fmt.Errorf("github issue %s: assignees node %d: %w", id, i, err)
		}
		logins = append(logins, login)
	}
	if len(logins) > 1 {
		slog.Debug("github issue extra assignees", "issue", id, "assignees", logins[1:])
	}
	return logins[0], nil
}

func assigneeLogin(raw json.RawMessage) (string, error) {
	if isMissing(raw) {
		return "", errors.New("expected a user, found null")
	}
	obj, err := objectOf(raw, "assignee")
	if err != nil {
		return "", err
	}
	login, err := requiredString(obj, "login")
	if err != nil {
		return "", err
	}
	if login == "" {
		return "", errors.New("expected login, found empty")
	}
	return login, nil
}

func mapBlocked(id string, raw json.RawMessage) ([]source.BlockRef, error) {
	if isMissing(raw) {
		return nil, fmt.Errorf("github issue %s: expected blockedBy, found missing", id)
	}
	obj, err := objectOf(raw, "blockedBy")
	if err != nil {
		return nil, fmt.Errorf("github issue %s: %w", id, err)
	}
	next, err := blockedPage(id, obj["pageInfo"])
	if err != nil {
		return nil, err
	}
	if next {
		return nil, fmt.Errorf("github issue %s: expected blockedBy to fit in one page, found another page", id)
	}
	nodes, err := nodesOf(obj, "blockedBy")
	if err != nil {
		return nil, fmt.Errorf("github issue %s: %w", id, err)
	}
	if len(nodes) > maxBlocked {
		return nil, fmt.Errorf("github issue %s: expected at most %d blockedBy nodes, found %d", id, maxBlocked, len(nodes))
	}
	if len(nodes) == 0 {
		return nil, nil
	}
	refs := make([]source.BlockRef, 0, len(nodes))
	for i, node := range nodes {
		ref, err := mapBlockRef(node)
		if err != nil {
			return nil, fmt.Errorf("github issue %s: blockedBy node %d: %w", id, i, err)
		}
		refs = append(refs, ref)
	}
	return refs, nil
}

func blockedPage(id string, raw json.RawMessage) (bool, error) {
	hasNext, err := hasNextPage(raw)
	if err != nil {
		return false, fmt.Errorf("github issue %s: blockedBy: %w", id, err)
	}
	return hasNext, nil
}

func hasNextPage(raw json.RawMessage) (bool, error) {
	obj, err := objectOf(raw, "pageInfo")
	if err != nil {
		return false, err
	}
	value, err := requiredBool(obj, "hasNextPage")
	if err != nil {
		return false, fmt.Errorf("pageInfo: %w", err)
	}
	return value, nil
}

func mapBlockRef(raw json.RawMessage) (source.BlockRef, error) {
	if isMissing(raw) {
		return source.BlockRef{}, errors.New("expected an issue, found null")
	}
	obj, err := objectOf(raw, "issue")
	if err != nil {
		return source.BlockRef{}, err
	}
	nodeID, err := requiredString(obj, "id")
	if err != nil {
		return source.BlockRef{}, err
	}
	if nodeID == "" {
		return source.BlockRef{}, errors.New("expected id, found empty")
	}
	number, err := requiredPositiveInt(obj, "number")
	if err != nil {
		return source.BlockRef{}, err
	}
	repo, err := repoName(obj, "repository")
	if err != nil {
		return source.BlockRef{}, err
	}
	return source.BlockRef{ID: source.GitHubID(repo, number), LinkID: nodeID}, nil
}

func repoName(obj map[string]json.RawMessage, key string) (string, error) {
	raw, ok := obj[key]
	if !ok || isMissing(raw) {
		return "", fmt.Errorf("expected %s, found missing", key)
	}
	repoObj, err := objectOf(raw, key)
	if err != nil {
		return "", err
	}
	name, err := requiredString(repoObj, "nameWithOwner")
	if err != nil {
		return "", fmt.Errorf("%s: %w", key, err)
	}
	if !validRepo(name) {
		return "", fmt.Errorf("expected repository owner/name, found %q", name)
	}
	return name, nil
}

func issueTimes(id string, obj map[string]json.RawMessage) (created, updated time.Time, err error) {
	created, err = mapTime(id, "createdAt", obj)
	if err != nil {
		return time.Time{}, time.Time{}, err
	}
	updated, err = mapTime(id, "updatedAt", obj)
	if err != nil {
		return time.Time{}, time.Time{}, err
	}
	return created, updated, nil
}

func mapTime(id, field string, obj map[string]json.RawMessage) (time.Time, error) {
	raw, ok := obj[field]
	if !ok || isMissing(raw) {
		return time.Time{}, fmt.Errorf("github issue %s: expected %s, found missing", id, field)
	}
	text, err := jsonString(raw, field)
	if err != nil {
		return time.Time{}, fmt.Errorf("github issue %s: %w", id, err)
	}
	parsed, err := time.Parse(time.RFC3339, text)
	if err != nil {
		parsed, err = time.Parse(time.RFC3339Nano, text)
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("github issue %s: expected %s RFC3339 timestamp, found %q", id, field, text)
	}
	return parsed, nil
}

func priorityOf(labels []string, table map[string]int) int {
	matched := false
	best := 0
	for _, label := range labels {
		rank, ok := table[label]
		if !ok {
			continue
		}
		if !matched || rank < best {
			matched = true
			best = rank
		}
	}
	if !matched {
		return defaultPriority
	}
	return best
}
