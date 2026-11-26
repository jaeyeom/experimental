package github

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// issueFieldsQuery and setIssueFieldValueQuery were read from the live GraphQL
// schema on 2026-11-24. SetIssueFieldValueInput accepts issueId and an
// issueFields list of IssueFieldCreateOrUpdateInput items.
const (
	issueFieldsQuery = "query($owner:String!,$name:String!){" +
		"repository(owner:$owner,name:$name){" +
		"issueFields(first:50){nodes{" +
		"__typename " +
		"... on IssueFieldSingleSelect{id name options{id name}} " +
		"... on IssueFieldDate{id name}" +
		"}}}}"

	setIssueFieldValueQuery = "mutation($issueId:ID!,$fieldId:ID!,$optionId:ID!){" +
		"setIssueFieldValue(input:{issueId:$issueId,issueFields:[{fieldId:$fieldId,singleSelectOptionId:$optionId}]}){" +
		"issue{id}}}"
)

const (
	issueFieldTypeSingleSelect      = "IssueFieldSingleSelect"
	issueFieldTypeDate              = "IssueFieldDate"
	issueFieldValueTypeSingleSelect = "IssueFieldSingleSelectValue"
	issueFieldValueTypeDate         = "IssueFieldDateValue"
)

var builtinGitHubPriorityRanks = map[string]int{
	"Urgent": 0,
	"High":   1,
	"Medium": 2,
	"Low":    3,
}

type issueFieldValues struct {
	singleSelect map[string]string
	dates        map[string]time.Time
}

type issueFieldDef struct {
	id          string
	name        string
	typeName    string
	options     map[string]string // option name -> option node ID
	optionNames []string
}

func mapIssueFieldValues(id string, raw json.RawMessage) (issueFieldValues, error) {
	out := issueFieldValues{
		singleSelect: map[string]string{},
		dates:        map[string]time.Time{},
	}
	if isMissing(raw) {
		return out, nil
	}
	nodes, err := namedNodes(raw, "issueFieldValues")
	if err != nil {
		return issueFieldValues{}, fmt.Errorf("github issue %s: %w", id, err)
	}
	for i, nodeRaw := range nodes {
		if isMissing(nodeRaw) {
			continue
		}
		if err := mapOneFieldValue(id, i, nodeRaw, &out); err != nil {
			return issueFieldValues{}, err
		}
	}
	return out, nil
}

func mapOneFieldValue(id string, idx int, raw json.RawMessage, out *issueFieldValues) error {
	obj, err := objectOf(raw, fmt.Sprintf("issueFieldValues[%d]", idx))
	if err != nil {
		return fmt.Errorf("github issue %s: %w", id, err)
	}
	typeName, err := requiredString(obj, "__typename")
	if err != nil {
		return fmt.Errorf("github issue %s: issueFieldValues[%d]: %w", id, idx, err)
	}
	switch typeName {
	case issueFieldValueTypeSingleSelect:
		fieldName, err := nestedFieldName(obj)
		if err != nil {
			return fmt.Errorf("github issue %s: issueFieldValues[%d]: %w", id, idx, err)
		}
		val, err := requiredString(obj, "name")
		if err != nil {
			return fmt.Errorf("github issue %s: issueFieldValues[%d]: %w", id, idx, err)
		}
		if val == "" {
			return fmt.Errorf("github issue %s: issueFieldValues[%d]: expected name, found empty", id, idx)
		}
		out.singleSelect[fieldName] = val
	case issueFieldValueTypeDate:
		fieldName, err := nestedFieldName(obj)
		if err != nil {
			return fmt.Errorf("github issue %s: issueFieldValues[%d]: %w", id, idx, err)
		}
		val, err := requiredString(obj, "value")
		if err != nil {
			return fmt.Errorf("github issue %s: issueFieldValues[%d]: %w", id, idx, err)
		}
		parsed, err := parseGitHubFieldDate(id, fieldName, val)
		if err != nil {
			return err
		}
		out.dates[fieldName] = parsed
	}
	return nil
}

func nestedFieldName(obj map[string]json.RawMessage) (string, error) {
	fieldObj, err := objectOf(obj["field"], "field")
	if err != nil {
		return "", err
	}
	name, err := requiredString(fieldObj, "name")
	if err != nil {
		return "", fmt.Errorf("field: %w", err)
	}
	if name == "" {
		return "", errors.New("expected field.name, found empty")
	}
	return name, nil
}

func parseGitHubFieldDate(id, field, text string) (time.Time, error) {
	if text == "" {
		return time.Time{}, fmt.Errorf("github issue %s field %q: expected YYYY-MM-DD or RFC3339 date, found empty", id, field)
	}
	if parsed, err := time.Parse(time.DateOnly, text); err == nil {
		return time.Date(parsed.Year(), parsed.Month(), parsed.Day(), 0, 0, 0, 0, time.UTC), nil
	}
	parsed, err := time.Parse(time.RFC3339, text)
	if err != nil {
		parsed, err = time.Parse(time.RFC3339Nano, text)
	}
	if err != nil {
		return time.Time{}, fmt.Errorf("github issue %s field %q: expected YYYY-MM-DD or RFC3339 date, found %q", id, field, text)
	}
	return utcDate(parsed), nil
}

func priorityFromField(id, value string, cfg *config.Source) int {
	if rank, ok := cfg.PriorityRanks[value]; ok {
		return rank
	}
	if rank, ok := builtinGitHubPriorityRanks[value]; ok {
		return rank
	}
	if slices.Contains(cfg.KnownPriorities, value) {
		return 2
	}
	slog.Warn("unknown github priority", "issue", id, "priority", value)
	return 2
}

func mergeNotBefore(fieldDate, labelDate *time.Time) *time.Time {
	switch {
	case fieldDate == nil:
		return labelDate
	case labelDate == nil:
		d := *fieldDate
		return &d
	case labelDate.After(*fieldDate):
		return labelDate
	default:
		d := *fieldDate
		return &d
	}
}

func (s *Source) setPriorityField(ctx context.Context, id, optionName string) error {
	fields, err := s.loadIssueFields(ctx, "priority")
	if err != nil {
		return err
	}
	def, ok := fields[s.cfg.PriorityField]
	if !ok || def.typeName != issueFieldTypeSingleSelect {
		return fmt.Errorf(
			"github priority: repository %s has no single-select issue field %q (configure priority_labels for label-based priority)",
			s.cfg.Repo, s.cfg.PriorityField,
		)
	}
	optionID, ok := def.options[optionName]
	if !ok {
		return fmt.Errorf(
			"github priority: expected option in %s field %q (%s), found %q",
			s.cfg.Repo, s.cfg.PriorityField, strings.Join(def.optionNames, ", "), optionName,
		)
	}
	_, nodeID, err := s.fetchIssue(ctx, id)
	if err != nil {
		return fmt.Errorf("github priority: %w", err)
	}
	raw, err := s.run(ctx, setIssueFieldValueArgs(nodeID, def.id, optionID)...)
	if err != nil {
		return fmt.Errorf("github priority: %w", err)
	}
	if err := decodeMutationIDs(raw, "setIssueFieldValue", "issue"); err != nil {
		return fmt.Errorf("github priority: %w", err)
	}
	return nil
}

func (s *Source) loadIssueFields(ctx context.Context, op string) (map[string]issueFieldDef, error) {
	s.mu.Lock()
	if s.fieldsCached {
		fields := s.issueFields
		s.mu.Unlock()
		return fields, nil
	}
	s.mu.Unlock()

	owner, name, err := splitRepo(s.cfg.Repo)
	if err != nil {
		return nil, fmt.Errorf("github %s: %w", op, err)
	}
	raw, err := s.run(ctx, issueFieldsArgs(owner, name)...)
	if err != nil {
		return nil, fmt.Errorf("github %s: %w", op, err)
	}
	fields, err := decodeIssueFields(raw)
	if err != nil {
		return nil, fmt.Errorf("github %s: %w", op, err)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.fieldsCached {
		s.issueFields = fields
		s.fieldsCached = true
	}
	return s.issueFields, nil
}

func decodeIssueFields(raw []byte) (map[string]issueFieldDef, error) {
	top, err := jsonObject(raw, "response")
	if err != nil {
		return nil, err
	}
	if err := graphqlFailure(top["errors"]); err != nil {
		return nil, err
	}
	repo, err := dataRepository(top["data"])
	if err != nil {
		return nil, err
	}
	out := map[string]issueFieldDef{}
	if isMissing(repo["issueFields"]) {
		return out, nil
	}
	nodes, err := namedNodes(repo["issueFields"], "issueFields")
	if err != nil {
		return nil, err
	}
	for i, nodeRaw := range nodes {
		def, ok, err := decodeIssueFieldDef(i, nodeRaw)
		if err != nil {
			return nil, err
		}
		if ok {
			out[def.name] = def
		}
	}
	return out, nil
}

func decodeIssueFieldDef(idx int, raw json.RawMessage) (issueFieldDef, bool, error) {
	obj, err := objectOf(raw, fmt.Sprintf("issueFields[%d]", idx))
	if err != nil {
		return issueFieldDef{}, false, err
	}
	typeName, err := requiredString(obj, "__typename")
	if err != nil {
		return issueFieldDef{}, false, fmt.Errorf("issueFields[%d]: %w", idx, err)
	}
	switch typeName {
	case issueFieldTypeSingleSelect, issueFieldTypeDate:
	default:
		return issueFieldDef{}, false, nil
	}
	id, err := requiredString(obj, "id")
	if err != nil || id == "" {
		return issueFieldDef{}, false, fmt.Errorf("issueFields[%d]: expected non-empty id", idx)
	}
	name, err := requiredString(obj, "name")
	if err != nil || name == "" {
		return issueFieldDef{}, false, fmt.Errorf("issueFields[%d]: expected non-empty name", idx)
	}
	def := issueFieldDef{id: id, name: name, typeName: typeName}
	if typeName == issueFieldTypeSingleSelect {
		opts, names, err := decodeFieldOptions(idx, obj["options"])
		if err != nil {
			return issueFieldDef{}, false, err
		}
		def.options = opts
		def.optionNames = names
	}
	return def, true, nil
}

func decodeFieldOptions(idx int, raw json.RawMessage) (map[string]string, []string, error) {
	if isMissing(raw) {
		return nil, nil, fmt.Errorf("issueFields[%d]: expected options array, found missing", idx)
	}
	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err != nil {
		return nil, nil, fmt.Errorf("issueFields[%d]: expected options array, found %s", idx, quoteSnippet(raw))
	}
	opts := make(map[string]string, len(items))
	names := make([]string, 0, len(items))
	for j, itemRaw := range items {
		obj, err := objectOf(itemRaw, fmt.Sprintf("issueFields[%d].options[%d]", idx, j))
		if err != nil {
			return nil, nil, err
		}
		id, err := requiredString(obj, "id")
		if err != nil || id == "" {
			return nil, nil, fmt.Errorf("issueFields[%d].options[%d]: expected non-empty id", idx, j)
		}
		name, err := requiredString(obj, "name")
		if err != nil || name == "" {
			return nil, nil, fmt.Errorf("issueFields[%d].options[%d]: expected non-empty name", idx, j)
		}
		opts[name] = id
		names = append(names, name)
	}
	return opts, names, nil
}

func issueFieldsArgs(owner, name string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + issueFieldsQuery,
		"-f", "owner=" + owner,
		"-f", "name=" + name,
	}
}

func setIssueFieldValueArgs(issueID, fieldID, optionID string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + setIssueFieldValueQuery,
		"-f", "issueId=" + issueID,
		"-f", "fieldId=" + fieldID,
		"-f", "optionId=" + optionID,
	}
}
