package jira

import (
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

type searchBody struct {
	JQL           string   `json:"jql"`
	Fields        []string `json:"fields"`
	MaxResults    int      `json:"maxResults"`
	NextPageToken string   `json:"nextPageToken,omitempty"`
}

type searchPage struct {
	Issues        []jiraIssue `json:"issues"`
	IsLast        bool        `json:"isLast"`
	NextPageToken string      `json:"nextPageToken"`
}

type jiraIssue struct {
	Key    string     `json:"key"`
	Fields jiraFields `json:"fields"`
	// Raw is every field by id, for the configured defer_field.
	Raw map[string]json.RawMessage `json:"-"`
}

// UnmarshalJSON decodes the typed fields and keeps the raw field map.
func (j *jiraIssue) UnmarshalJSON(data []byte) error {
	var wire struct {
		Key    string          `json:"key"`
		Fields json.RawMessage `json:"fields"`
	}
	if err := json.Unmarshal(data, &wire); err != nil {
		return fmt.Errorf("decode jira issue: %w", err)
	}
	j.Key = wire.Key
	if len(wire.Fields) == 0 || string(wire.Fields) == "null" {
		return nil
	}
	if err := json.Unmarshal(wire.Fields, &j.Fields); err != nil {
		return fmt.Errorf("decode jira issue fields: %w", err)
	}
	if err := json.Unmarshal(wire.Fields, &j.Raw); err != nil {
		return fmt.Errorf("decode jira raw fields: %w", err)
	}
	return nil
}

type jiraFields struct {
	Summary    string          `json:"summary"`
	Status     *jiraStatus     `json:"status"`
	IssueType  *jiraNamed      `json:"issuetype"`
	Parent     *jiraRef        `json:"parent"`
	Labels     []string        `json:"labels"`
	Assignee   *jiraAssignee   `json:"assignee"`
	Priority   *jiraNamed      `json:"priority"`
	IssueLinks []jiraLink      `json:"issuelinks"`
	Created    string          `json:"created"`
	Updated    string          `json:"updated"`
	DueDate    json.RawMessage `json:"duedate"`
}

type jiraStatus struct {
	Name           string        `json:"name"`
	StatusCategory *jiraCategory `json:"statusCategory"`
}

type jiraCategory struct {
	Key string `json:"key"`
}

type jiraNamed struct {
	Name string `json:"name"`
}

type jiraRef struct {
	Key string `json:"key"`
}

type jiraAssignee struct {
	AccountID string `json:"accountId"`
}

type jiraLink struct {
	ID           string    `json:"id"`
	Type         jiraNamed `json:"type"`
	InwardIssue  *jiraRef  `json:"inwardIssue"`
	OutwardIssue *jiraRef  `json:"outwardIssue"`
}

type myselfBody struct {
	AccountID string `json:"accountId"`
	TimeZone  string `json:"timeZone"`
}

type transitionsBody struct {
	Transitions []transitionItem `json:"transitions"`
}

type transitionItem struct {
	ID   string     `json:"id"`
	Name string     `json:"name"`
	To   *jiraNamed `json:"to"`
}

// transitionChoice is one transition's id, its own name, and its target status.
type transitionChoice struct {
	ID   string
	Name string
	To   string
}

const (
	// defaultJiraPriority is an unknown name, and a known_priorities name.
	defaultJiraPriority = 2
	// missingJiraPriority is a missing priority. It sorts with Lowest, after Low.
	missingJiraPriority = 4
)

var priorityRanks = map[string]int{
	"Highest": 0,
	"High":    1,
	"Medium":  2,
	"Low":     3,
	"Lowest":  4,
}

func decodeSearch(raw []byte) (searchPage, error) {
	var probe map[string]json.RawMessage
	if err := json.Unmarshal(raw, &probe); err != nil {
		return searchPage{}, fmt.Errorf("jira search: expected an object, found %s: %w", quoteSnippet(raw), err)
	}
	if _, ok := probe["issues"]; !ok {
		return searchPage{}, fmt.Errorf("jira search: expected issues, found %s", quoteSnippet(raw))
	}
	var page searchPage
	if err := json.Unmarshal(raw, &page); err != nil {
		return searchPage{}, fmt.Errorf("jira search: expected an issues array, found %s: %w", quoteSnippet(raw), err)
	}
	return page, nil
}

func decodeIssue(raw []byte) (jiraIssue, error) {
	var issue jiraIssue
	if err := json.Unmarshal(raw, &issue); err != nil {
		return jiraIssue{}, fmt.Errorf("jira issue: expected an object, found %s: %w", quoteSnippet(raw), err)
	}
	return issue, nil
}

func decodeTransitions(raw []byte) ([]transitionChoice, error) {
	var probe map[string]json.RawMessage
	if err := json.Unmarshal(raw, &probe); err != nil {
		return nil, fmt.Errorf("jira transitions: expected an object, found %s: %w", quoteSnippet(raw), err)
	}
	rawList, ok := probe["transitions"]
	if !ok {
		return nil, fmt.Errorf("jira transitions: expected transitions, found %s", quoteSnippet(raw))
	}
	if strings.TrimSpace(string(rawList)) == "null" {
		return nil, fmt.Errorf("jira transitions: expected an array, found null")
	}
	var body transitionsBody
	if err := json.Unmarshal(raw, &body); err != nil {
		return nil, fmt.Errorf("jira transitions: expected a transitions array, found %s: %w", quoteSnippet(raw), err)
	}
	out := make([]transitionChoice, 0, len(body.Transitions))
	for i, tr := range body.Transitions {
		if tr.To == nil {
			return nil, fmt.Errorf("jira transitions: transitions[%d]: expected to, found missing", i)
		}
		out = append(out, transitionChoice{ID: tr.ID, Name: tr.Name, To: tr.To.Name})
	}
	return out, nil
}

func decodeCreated(raw []byte) (string, error) {
	var probe map[string]json.RawMessage
	if err := json.Unmarshal(raw, &probe); err != nil {
		return "", fmt.Errorf("jira create: expected an object, found %s: %w", quoteSnippet(raw), err)
	}
	rawKey, ok := probe["key"]
	if !ok {
		return "", fmt.Errorf("jira create: expected key, found %s", quoteSnippet(raw))
	}
	var key string
	if err := json.Unmarshal(rawKey, &key); err != nil {
		return "", fmt.Errorf("jira create: expected a key string, found %s", quoteSnippet(rawKey))
	}
	if key == "" {
		return "", errors.New("jira create: expected key, found empty")
	}
	return key, nil
}

func decodeMyself(raw []byte) (myselfBody, error) {
	var body myselfBody
	if err := json.Unmarshal(raw, &body); err != nil {
		return myselfBody{}, fmt.Errorf("jira myself: expected an object, found %s: %w", quoteSnippet(raw), err)
	}
	return body, nil
}

func (s *Source) mapIssues(issues []jiraIssue) ([]source.Item, error) {
	items := make([]source.Item, 0, len(issues))
	for i := range issues {
		item, err := s.mapIssue(issues[i])
		if err != nil {
			return nil, err
		}
		items = append(items, item)
	}
	return items, nil
}

func (s *Source) mapIssue(issue jiraIssue) (source.Item, error) {
	if issue.Key == "" {
		return source.Item{}, errors.New("jira issue: expected key, found empty")
	}
	if issue.Fields.Status == nil {
		return source.Item{}, missingField(issue.Key, "status")
	}
	if issue.Fields.IssueType == nil {
		return source.Item{}, missingField(issue.Key, "issuetype")
	}
	created, err := parseJiraTime(issue.Key, "created", issue.Fields.Created)
	if err != nil {
		return source.Item{}, err
	}
	updated, err := parseJiraTime(issue.Key, "updated", issue.Fields.Updated)
	if err != nil {
		return source.Item{}, err
	}
	blocked, err := blockedBy(issue.Key, s.cfg.BlocksLinkType, issue.Fields.IssueLinks)
	if err != nil {
		return source.Item{}, err
	}
	due, err := parseJiraDue(issue.Key, issue.Fields.DueDate)
	if err != nil {
		return source.Item{}, err
	}
	notBefore, err := s.notBefore(issue)
	if err != nil {
		return source.Item{}, err
	}
	return source.Item{
		Source:    s.cfg.Name,
		ID:        issue.Key,
		Title:     issue.Fields.Summary,
		URL:       browseURL(s.cfg.Site, issue.Key),
		Type:      issue.Fields.IssueType.Name,
		State:     issue.Fields.Status.Name,
		Done:      done(issue.Fields.Status),
		Parent:    parentKey(issue.Fields.Parent),
		Labels:    copyStrings(issue.Fields.Labels),
		Assignee:  assigneeID(issue.Fields.Assignee),
		Priority:  s.mapPriority(issue.Key, issue.Fields.Priority),
		BlockedBy: blocked,
		Created:   created,
		Updated:   updated,
		Due:       due,
		NotBefore: notBefore,
	}, nil
}

// notBefore reads the configured defer_field: a YYYY-MM-DD date or a Jira
// datetime, whose calendar date in its own offset is used. Null, empty,
// and a missing field are no deferral.
func (s *Source) notBefore(issue jiraIssue) (*time.Time, error) {
	field := s.cfg.DeferField
	if field == "" {
		return nil, nil
	}
	raw := issue.Raw[field]
	text := strings.TrimSpace(string(raw))
	if text == "" || text == "null" {
		return nil, nil
	}
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return nil, fmt.Errorf("jira issue %s field %q (defer_field): expected a date or datetime string or null, found %s", issue.Key, field, quoteSnippet(raw))
	}
	if value == "" {
		return nil, nil
	}
	parsed, err := time.Parse(jiraDateLayout, value)
	if err != nil {
		parsed, err = time.Parse(jiraTimeLayout, value)
	}
	if err != nil {
		return nil, fmt.Errorf("jira issue %s field %q (defer_field): expected YYYY-MM-DD or %s, found %q", issue.Key, field, jiraTimeLayout, value)
	}
	date := time.Date(parsed.Year(), parsed.Month(), parsed.Day(), 0, 0, 0, 0, time.UTC)
	return &date, nil
}

// parseJiraDue reads fields.duedate. Null, empty, and a missing field stay nil.
// Any other shape must be YYYY-MM-DD.
func parseJiraDue(key string, raw json.RawMessage) (*time.Time, error) {
	text := strings.TrimSpace(string(raw))
	if text == "" || text == "null" {
		return nil, nil
	}
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return nil, fmt.Errorf("jira issue %s field %q: expected YYYY-MM-DD or null, found %s", key, "duedate", quoteSnippet(raw))
	}
	if value == "" {
		return nil, nil
	}
	parsed, err := time.Parse(jiraDateLayout, value)
	if err != nil {
		return nil, fmt.Errorf("jira issue %s field %q: expected YYYY-MM-DD, found %q", key, "duedate", value)
	}
	due := time.Date(parsed.Year(), parsed.Month(), parsed.Day(), 0, 0, 0, 0, time.UTC)
	return &due, nil
}

func missingField(key, field string) error {
	return fmt.Errorf("jira issue %s field %q: expected an object, found missing", key, field)
}

func parseJiraTime(key, field, value string) (time.Time, error) {
	parsed, err := time.Parse(jiraTimeLayout, value)
	if err != nil {
		return time.Time{}, fmt.Errorf("jira issue %s field %q: expected %s, found %q", key, field, jiraTimeLayout, value)
	}
	return parsed, nil
}

// blockedBy keeps inward Blocks links only.
// On issue X, an inwardIssue Y means X is blocked by Y.
// An outwardIssue means X blocks Y, and this read path ignores it.
func blockedBy(key, linkType string, links []jiraLink) ([]source.BlockRef, error) {
	var refs []source.BlockRef
	for _, link := range links {
		if link.Type.Name != linkType || link.InwardIssue == nil {
			continue
		}
		if link.ID == "" {
			return nil, fmt.Errorf("jira issue %s field %q: expected a link id, found empty", key, "issuelinks.id")
		}
		if link.InwardIssue.Key == "" {
			return nil, fmt.Errorf("jira issue %s field %q: expected an issue key, found empty", key, "issuelinks.inwardIssue.key")
		}
		refs = append(refs, source.BlockRef{ID: link.InwardIssue.Key, LinkID: link.ID})
	}
	return refs, nil
}

func done(status *jiraStatus) bool {
	if status == nil || status.StatusCategory == nil {
		return false
	}
	return status.StatusCategory.Key == "done"
}

func parentKey(parent *jiraRef) string {
	if parent == nil {
		return ""
	}
	return parent.Key
}

func assigneeID(assignee *jiraAssignee) string {
	if assignee == nil {
		return ""
	}
	return assignee.AccountID
}

func (s *Source) mapPriority(key string, priority *jiraNamed) int {
	if priority == nil || priority.Name == "" {
		return missingJiraPriority
	}
	if rank, ok := s.priorityRank(priority.Name); ok {
		return rank
	}
	s.warnPriority(key, priority.Name)
	return defaultJiraPriority
}

// priorityRank reports the rank for a Jira priority name.
// Config ranks override the built-in names. A known extra is rank 2.
func (s *Source) priorityRank(name string) (int, bool) {
	if s.cfg.PriorityRanks != nil {
		if rank, ok := s.cfg.PriorityRanks[name]; ok {
			return rank, true
		}
	}
	if rank, ok := priorityRanks[name]; ok {
		return rank, true
	}
	for _, known := range s.cfg.KnownPriorities {
		if known == name {
			return defaultJiraPriority, true
		}
	}
	return 0, false
}

// warnPriority logs an unknown priority once per name for this source.
func (s *Source) warnPriority(key, name string) {
	s.mu.Lock()
	if s.warned == nil {
		s.warned = map[string]struct{}{}
	}
	if _, ok := s.warned[name]; ok {
		s.mu.Unlock()
		return
	}
	s.warned[name] = struct{}{}
	s.mu.Unlock()
	slog.Warn("unknown jira priority", "issue", key, "priority", name)
}

func browseURL(site, key string) string {
	return strings.TrimRight(site, "/") + "/browse/" + key
}

func copyStrings(in []string) []string {
	if in == nil {
		return nil
	}
	out := make([]string, len(in))
	copy(out, in)
	return out
}

func quoteSnippet(raw []byte) string {
	const maxSnippet = 180
	text := strings.TrimSpace(string(raw))
	if len(text) > maxSnippet {
		text = text[:maxSnippet] + "..."
	}
	return strconv.Quote(text)
}

type parentTypeBody struct {
	Fields *struct {
		IssueType *issueTypeBody `json:"issuetype"`
	} `json:"fields"`
}

type issueTypeBody struct {
	Name           string `json:"name"`
	Subtask        *bool  `json:"subtask"`
	HierarchyLevel *int   `json:"hierarchyLevel"`
}

type projectBody struct {
	IssueTypes *[]issueTypeBody `json:"issueTypes"`
}

// decodeParentType reads fields.issuetype from a parent issue.
// Name, subtask, and hierarchyLevel are all required.
func decodeParentType(parent string, raw []byte) (parentType, error) {
	var body parentTypeBody
	if err := json.Unmarshal(raw, &body); err != nil {
		return parentType{}, fmt.Errorf("jira create: parent %s: expected an object, found %s: %w", parent, quoteSnippet(raw), err)
	}
	if body.Fields == nil || body.Fields.IssueType == nil {
		return parentType{}, fmt.Errorf("jira create: parent %s: expected fields.issuetype, found %s", parent, quoteSnippet(raw))
	}
	it := body.Fields.IssueType
	if it.Name == "" || it.Subtask == nil || it.HierarchyLevel == nil {
		return parentType{}, fmt.Errorf("jira create: parent %s: expected issuetype name, subtask, and hierarchyLevel, found %s", parent, quoteSnippet(raw))
	}
	return parentType{
		name:    it.Name,
		subtask: *it.Subtask || *it.HierarchyLevel < 0,
		level:   *it.HierarchyLevel,
	}, nil
}

// decodeSubtaskTypes returns the names of a project's subtask issue types.
func decodeSubtaskTypes(project string, raw []byte) ([]string, error) {
	var body projectBody
	if err := json.Unmarshal(raw, &body); err != nil {
		return nil, fmt.Errorf("jira create: project %s: expected an object, found %s: %w", project, quoteSnippet(raw), err)
	}
	if body.IssueTypes == nil {
		return nil, fmt.Errorf("jira create: project %s: expected issueTypes, found %s", project, quoteSnippet(raw))
	}
	var names []string
	for _, it := range *body.IssueTypes {
		if it.Name == "" || it.Subtask == nil {
			return nil, fmt.Errorf("jira create: project %s: expected issue type name and subtask, found %s", project, quoteSnippet(raw))
		}
		if *it.Subtask {
			names = append(names, it.Name)
		}
	}
	return names, nil
}

type commentsPage struct {
	comments []source.ClaimComment
	startAt  int
	total    int
}

type jiraCommentsBody struct {
	StartAt  int               `json:"startAt"`
	Total    int               `json:"total"`
	Comments []jiraCommentItem `json:"comments"`
}

type jiraCommentItem struct {
	Author  *jiraAssignee   `json:"author"`
	Body    json.RawMessage `json:"body"`
	Created string          `json:"created"`
}

func decodeComments(key string, raw []byte) (commentsPage, error) {
	var probe map[string]json.RawMessage
	if err := json.Unmarshal(raw, &probe); err != nil {
		return commentsPage{}, fmt.Errorf("jira comments %s: expected an object, found %s: %w", key, quoteSnippet(raw), err)
	}
	rawList, ok := probe["comments"]
	if !ok {
		return commentsPage{}, fmt.Errorf("jira comments %s: expected comments, found %s", key, quoteSnippet(raw))
	}
	if strings.TrimSpace(string(rawList)) == "null" {
		return commentsPage{}, fmt.Errorf("jira comments %s: expected a comments array, found null", key)
	}
	var body jiraCommentsBody
	if err := json.Unmarshal(raw, &body); err != nil {
		return commentsPage{}, fmt.Errorf("jira comments %s: expected a comments array, found %s: %w", key, quoteSnippet(raw), err)
	}
	out := make([]source.ClaimComment, 0, len(body.Comments))
	for i, c := range body.Comments {
		created, err := parseJiraTime(key, fmt.Sprintf("comments[%d].created", i), c.Created)
		if err != nil {
			return commentsPage{}, err
		}
		text, err := decodeCommentBody(key, i, c.Body)
		if err != nil {
			return commentsPage{}, err
		}
		out = append(out, source.ClaimComment{
			Author:    assigneeID(c.Author),
			CreatedAt: created,
			Body:      text,
		})
	}
	return commentsPage{
		comments: out,
		startAt:  body.StartAt,
		total:    body.Total,
	}, nil
}

func decodeCommentBody(key string, idx int, raw json.RawMessage) (string, error) {
	trimmed := strings.TrimSpace(string(raw))
	if trimmed == "" || trimmed == "null" {
		return "", nil
	}
	if trimmed[0] == '"' {
		var plain string
		if err := json.Unmarshal(raw, &plain); err != nil {
			return "", fmt.Errorf("jira comments %s comments[%d].body: expected ADF object or string, found %s: %w", key, idx, quoteSnippet(raw), err)
		}
		return plain, nil
	}
	if trimmed[0] == '{' {
		var doc adfDoc
		if err := json.Unmarshal(raw, &doc); err != nil {
			return "", fmt.Errorf("jira comments %s comments[%d].body: expected ADF object, found %s: %w", key, idx, quoteSnippet(raw), err)
		}
		return adfPlainText(&doc), nil
	}
	return "", fmt.Errorf("jira comments %s comments[%d].body: expected ADF object or string, found %s", key, idx, quoteSnippet(raw))
}
