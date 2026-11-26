package gh

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
)

// GraphQL __typename values for supported GitHub repository issue fields.
const (
	IssueFieldTypeSingleSelect = "IssueFieldSingleSelect"
	IssueFieldTypeDate         = "IssueFieldDate"
)

// IssueFieldOption is one selectable option on a single-select issue field.
type IssueFieldOption struct {
	ID   string
	Name string
}

// IssueField is one native GitHub issue field available on a repository.
type IssueField struct {
	ID       string
	Name     string
	TypeName string
	Options  []IssueFieldOption
}

// issueFieldsQuery reads a repository's native issue fields (defined at the
// owner account or organization level). Verified against GitHub's live
// GraphQL schema on 2026-11-24.
const issueFieldsQuery = "query($owner:String!,$name:String!){" +
	"repository(owner:$owner,name:$name){" +
	"issueFields(first:50){nodes{" +
	"__typename " +
	"... on IssueFieldSingleSelect{id name options{id name}} " +
	"... on IssueFieldDate{id name}" +
	"}}}}"

// IssueFields returns the supported native issue fields (single-select and
// date) available on repo. Other field types are skipped.
func (c *Client) IssueFields(ctx context.Context, repo string) ([]IssueField, error) {
	if ctx == nil {
		return nil, errors.New("gh issue fields: expected a context, found nil")
	}
	if !validRepo(repo) {
		return nil, fmt.Errorf("gh issue fields: expected repository owner/repo, found %q", repo)
	}
	owner, name, _ := strings.Cut(repo, "/")
	result, err := c.execute(ctx,
		"api", "graphql",
		"-f", "query="+issueFieldsQuery,
		"-f", "owner="+owner,
		"-f", "name="+name,
	)
	if err != nil {
		return nil, err
	}
	if result.ExitCode != 0 {
		return nil, ghFailure(result)
	}
	return decodeIssueFields([]byte(result.Output))
}

func decodeIssueFields(raw []byte) ([]IssueField, error) {
	var top map[string]json.RawMessage
	if err := json.Unmarshal(raw, &top); err != nil {
		return nil, fmt.Errorf("gh issue fields: expected JSON object, found %s: %w", quoteSnippet(raw), err)
	}
	if errs, ok := top["errors"]; ok {
		if err := graphqlFailure(errs); err != nil {
			return nil, err
		}
	}
	dataObj, err := rawObject(top["data"], "data")
	if err != nil {
		return nil, fmt.Errorf("gh issue fields: %w", err)
	}
	repoObj, err := rawObject(dataObj["repository"], "repository")
	if err != nil {
		return nil, fmt.Errorf("gh issue fields: %w", err)
	}
	if isNull(repoObj["issueFields"]) {
		return nil, nil
	}
	fieldsObj, err := rawObject(repoObj["issueFields"], "issueFields")
	if err != nil {
		return nil, fmt.Errorf("gh issue fields: %w", err)
	}
	nodesRaw, ok := fieldsObj["nodes"]
	if !ok || isNull(nodesRaw) {
		return nil, errors.New("gh issue fields: expected issueFields.nodes array, found missing")
	}
	var nodes []json.RawMessage
	if err := json.Unmarshal(nodesRaw, &nodes); err != nil {
		return nil, fmt.Errorf("gh issue fields: expected issueFields.nodes array, found %s", quoteSnippet(nodesRaw))
	}
	out := make([]IssueField, 0, len(nodes))
	for i, nodeRaw := range nodes {
		field, ok, err := decodeIssueFieldNode(nodeRaw)
		if err != nil {
			return nil, fmt.Errorf("gh issue fields: nodes[%d]: %w", i, err)
		}
		if ok {
			out = append(out, field)
		}
	}
	return out, nil
}

func decodeIssueFieldNode(raw json.RawMessage) (IssueField, bool, error) {
	obj, err := rawObject(raw, "issueField")
	if err != nil {
		return IssueField{}, false, err
	}
	typeName, err := requiredString(obj, "__typename")
	if err != nil {
		return IssueField{}, false, err
	}
	switch typeName {
	case IssueFieldTypeSingleSelect, IssueFieldTypeDate:
	default:
		return IssueField{}, false, nil
	}
	id, err := requiredString(obj, "id")
	if err != nil {
		return IssueField{}, false, err
	}
	if id == "" {
		return IssueField{}, false, errors.New("expected id, found empty")
	}
	name, err := requiredString(obj, "name")
	if err != nil {
		return IssueField{}, false, err
	}
	if name == "" {
		return IssueField{}, false, errors.New("expected name, found empty")
	}
	field := IssueField{ID: id, Name: name, TypeName: typeName}
	if typeName == IssueFieldTypeSingleSelect {
		opts, err := decodeIssueFieldOptions(obj["options"])
		if err != nil {
			return IssueField{}, false, err
		}
		field.Options = opts
	}
	return field, true, nil
}

func decodeIssueFieldOptions(raw json.RawMessage) ([]IssueFieldOption, error) {
	if isNull(raw) {
		return nil, errors.New("expected options array, found missing")
	}
	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err != nil {
		return nil, fmt.Errorf("expected options array, found %s", quoteSnippet(raw))
	}
	out := make([]IssueFieldOption, 0, len(items))
	for i, itemRaw := range items {
		obj, err := rawObject(itemRaw, fmt.Sprintf("options[%d]", i))
		if err != nil {
			return nil, err
		}
		id, err := requiredString(obj, "id")
		if err != nil {
			return nil, fmt.Errorf("options[%d]: %w", i, err)
		}
		if id == "" {
			return nil, fmt.Errorf("options[%d]: expected id, found empty", i)
		}
		name, err := requiredString(obj, "name")
		if err != nil {
			return nil, fmt.Errorf("options[%d]: %w", i, err)
		}
		if name == "" {
			return nil, fmt.Errorf("options[%d]: expected name, found empty", i)
		}
		out = append(out, IssueFieldOption{ID: id, Name: name})
	}
	return out, nil
}
