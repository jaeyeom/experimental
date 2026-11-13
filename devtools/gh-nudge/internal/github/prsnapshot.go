package github

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
)

// prSnapshotBatch is the most pull requests one state query asks for.
// A larger request is split so a history run stays within a safe query size.
const prSnapshotBatch = 50

// PRSnapshot is the current state, author, and title of one pull request.
// Unreadable is set when GitHub cannot resolve it (deleted, or no access).
type PRSnapshot struct {
	State      string
	Author     string
	Title      string
	Unreadable bool
}

type prAlias struct {
	name string
	key  string
}

// PullRequestSnapshots loads the state, author, and title of each ref.
// One GraphQL request covers up to prSnapshotBatch pull requests. An empty
// list makes no call. A NOT_FOUND or FORBIDDEN result is Unreadable. Any
// other GraphQL error is returned.
func (c *Client) PullRequestSnapshots(refs []PullRef) (map[string]PRSnapshot, error) {
	out := make(map[string]PRSnapshot, len(refs))
	for start := 0; start < len(refs); start += prSnapshotBatch {
		end := start + prSnapshotBatch
		if end > len(refs) {
			end = len(refs)
		}
		part, err := c.pullRequestSnapshotBatch(refs[start:end])
		if err != nil {
			return nil, err
		}
		for key, snap := range part {
			out[key] = snap
		}
	}
	return out, nil
}

func (c *Client) pullRequestSnapshotBatch(refs []PullRef) (map[string]PRSnapshot, error) {
	query, aliases, err := prSnapshotQuery(refs)
	if err != nil {
		return nil, err
	}
	out, err := c.graphqlQuery(query, nil)
	if err != nil {
		return nil, fmt.Errorf("pull request state: %w", err)
	}
	snaps, err := decodePRSnapshots(out.stdout, aliases)
	if err != nil {
		return nil, err
	}
	return snaps, nil
}

func prSnapshotQuery(refs []PullRef) (string, []prAlias, error) {
	var b strings.Builder
	b.WriteString("query{")
	aliases := make([]prAlias, len(refs))
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
		aliases[i] = prAlias{name: alias, key: pullRefKey(ref)}
		fmt.Fprintf(&b, "%s:repository(owner:%s,name:%s){pullRequest(number:%d){state author{login} title}}",
			alias, owner, name, ref.Number)
	}
	b.WriteString("}")
	return b.String(), aliases, nil
}

func pullRefKey(ref PullRef) string {
	if ref.Raw != "" {
		return ref.Raw
	}
	return fmt.Sprintf("%s/%s#%d", ref.Owner, ref.Repo, ref.Number)
}

func gqlQuote(s string) (string, error) {
	quoted, err := json.Marshal(s)
	if err != nil {
		return "", fmt.Errorf("quote %q for graphql: %w", s, err)
	}
	return string(quoted), nil
}

type prSnapshotNode struct {
	PullRequest *struct {
		State  string `json:"state"`
		Author *struct {
			Login string `json:"login"`
		} `json:"author"`
		Title string `json:"title"`
	} `json:"pullRequest"`
}

func decodePRSnapshots(stdout string, aliases []prAlias) (map[string]PRSnapshot, error) {
	var envelope struct {
		Data   map[string]*prSnapshotNode `json:"data"`
		Errors []struct {
			Type    string            `json:"type"`
			Message string            `json:"message"`
			Path    []json.RawMessage `json:"path"`
		} `json:"errors"`
	}
	if err := json.Unmarshal([]byte(stdout), &envelope); err != nil {
		return nil, fmt.Errorf("pull request state: expected a GraphQL JSON object, found %q", head(stdout))
	}
	known := make(map[string]bool, len(aliases))
	for _, alias := range aliases {
		known[alias.name] = true
	}
	unread := map[string]bool{}
	for _, item := range envelope.Errors {
		msg := strings.TrimSpace(item.Message)
		if msg == "" {
			msg = item.Type
		}
		if msg == "" {
			msg = "graphql error"
		}
		alias := pathAlias(item.Path)
		if (item.Type == "NOT_FOUND" || item.Type == "FORBIDDEN") && known[alias] {
			unread[alias] = true
			continue
		}
		return nil, fmt.Errorf("pull request state: %s", msg)
	}
	if envelope.Data == nil && len(unread) == 0 {
		return nil, fmt.Errorf("pull request state: expected data, found %q", head(stdout))
	}
	out := make(map[string]PRSnapshot, len(aliases))
	for _, alias := range aliases {
		var node *prSnapshotNode
		if envelope.Data != nil {
			node = envelope.Data[alias.name]
		}
		snap, err := snapshotOf(alias.key, node, unread[alias.name])
		if err != nil {
			return nil, err
		}
		out[alias.key] = snap
	}
	return out, nil
}

func snapshotOf(key string, node *prSnapshotNode, unreadable bool) (PRSnapshot, error) {
	if unreadable || node == nil || node.PullRequest == nil {
		return PRSnapshot{Unreadable: true}, nil
	}
	pr := node.PullRequest
	switch pr.State {
	case "OPEN", "CLOSED", "MERGED":
	default:
		return PRSnapshot{}, fmt.Errorf("pull request %s: expected state OPEN, CLOSED, or MERGED, found %q", key, pr.State)
	}
	snap := PRSnapshot{State: pr.State, Title: pr.Title}
	if pr.Author != nil {
		snap.Author = pr.Author.Login
	}
	return snap, nil
}

func pathAlias(path []json.RawMessage) string {
	if len(path) == 0 {
		return ""
	}
	var name string
	if err := json.Unmarshal(path[0], &name); err != nil {
		return ""
	}
	return name
}

func head(s string) string {
	s = strings.TrimSpace(s)
	if len(s) > 120 {
		return s[:120]
	}
	return s
}
