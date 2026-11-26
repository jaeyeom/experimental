package jira

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"net/http"
	"net/url"
	"slices"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/priority"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	issueLinkPath   = "/rest/api/3/issueLink"
	createIssuePath = "/rest/api/3/issue"
)

// assigneePost is the PUT /assignee body.
// A nil AccountID encodes as JSON null and clears the assignee.
// omitempty would drop the field and leave the assignee in place.
type assigneePost struct {
	AccountID *string `json:"accountId"`
}

type transitionPost struct {
	Transition idOnly `json:"transition"`
}

type commentPost struct {
	Body *adfDoc `json:"body"`
}

type idOnly struct {
	ID string `json:"id"`
}

type priorityPut struct {
	Fields priorityFields `json:"fields"`
}

type priorityFields struct {
	Priority jiraNamed `json:"priority"`
}

type labelUpdatePut struct {
	Update labelUpdateFields `json:"update"`
}

type labelUpdateFields struct {
	Labels []labelOp `json:"labels"`
}

type labelOp struct {
	Add    string `json:"add,omitempty"`
	Remove string `json:"remove,omitempty"`
}

type jiraClaimSettings struct {
	assign         bool
	markInProgress bool
	status         string
	transition     string
	label          string
	legacy         bool
}

func (s *Source) effectiveClaim(opts source.ClaimOptions, forRelease bool) jiraClaimSettings {
	var st jiraClaimSettings
	if s.cfg.Claim == (config.Claim{}) {
		assign := s.cfg.ClaimAssign
		if forRelease {
			assign = true
		}
		st = jiraClaimSettings{
			assign:         assign,
			markInProgress: true,
			status:         s.cfg.ClaimStatus,
			transition:     s.cfg.ClaimTransition,
			label:          s.cfg.ClaimLabel,
			legacy:         true,
		}
	} else {
		status := s.cfg.Claim.Status
		if status == "" {
			status = s.cfg.ClaimStatus
		}
		transition := s.cfg.Claim.Transition
		if transition == "" {
			transition = s.cfg.ClaimTransition
		}
		label := s.cfg.Claim.Label
		if label == "" {
			label = s.cfg.ClaimLabel
		}
		st = jiraClaimSettings{
			assign:         s.cfg.Claim.Assign,
			markInProgress: s.cfg.Claim.MarkInProgress,
			status:         status,
			transition:     transition,
			label:          label,
		}
	}
	if opts.Assign != nil {
		st.assign = *opts.Assign
	}
	if opts.MarkInProgress != nil {
		st.markInProgress = *opts.MarkInProgress
	}
	return st
}

// issueLinkBody is the POST /issueLink body.
// InwardIssue is the blocker. OutwardIssue is the blocked issue.
type issueLinkBody struct {
	Type         jiraNamed `json:"type"`
	InwardIssue  jiraRef   `json:"inwardIssue"`
	OutwardIssue jiraRef   `json:"outwardIssue"`
}

type createEnvelope struct {
	Fields createFields `json:"fields"`
}

type createFields struct {
	Project     jiraRef       `json:"project"`
	IssueType   jiraNamed     `json:"issuetype"`
	Summary     string        `json:"summary"`
	Labels      []string      `json:"labels"`
	Parent      *jiraRef      `json:"parent,omitempty"`
	Description *adfDoc       `json:"description,omitempty"`
	Assignee    *jiraAssignee `json:"assignee,omitempty"`
}

// Claim marks the issue taken using the source's configured [claim] settings.
func (s *Source) Claim(ctx context.Context, key string) error {
	return s.ClaimWithOptions(ctx, key, source.ClaimOptions{})
}

// ClaimWithOptions transitions the issue to claim status (when mark_in_progress
// is enabled and a claim status is configured), adds any configured claim
// label, and assigns the authenticated account (when assign is enabled).
func (s *Source) ClaimWithOptions(ctx context.Context, key string, opts source.ClaimOptions) error {
	if err := requireCtx(ctx, "claim"); err != nil {
		return err
	}
	if err := requireKey(key, "claim"); err != nil {
		return err
	}
	st := s.effectiveClaim(opts, false)
	if st.markInProgress {
		if err := s.markClaimInProgress(ctx, key, st); err != nil {
			return err
		}
	}
	if !st.assign {
		return nil
	}
	account, err := s.Me(ctx)
	if err != nil {
		return fmt.Errorf("jira claim: %w", err)
	}
	return s.putAssignee(ctx, "claim", key, &account)
}

func (s *Source) markClaimInProgress(ctx context.Context, key string, st jiraClaimSettings) error {
	if st.legacy && st.status == "" && st.label == "" {
		return errors.New("jira claim: expected claim_status, found empty")
	}
	if st.status != "" {
		live, err := s.Get(ctx, key)
		if err != nil {
			return fmt.Errorf("jira claim: %w", err)
		}
		if live.ID != key {
			return fmt.Errorf("jira claim: expected key %q, found %q", key, live.ID)
		}
		// Already in claim status: do not post a transition. Label and assignment still run.
		if live.State != st.status {
			if err := s.transitionClaim(ctx, key, st.status, st.transition); err != nil {
				return err
			}
		}
	}
	if st.label != "" {
		return s.updateLabel(ctx, "claim", key, labelOp{Add: st.label})
	}
	return nil
}

// PostClaimComment posts a claim comment on the issue as ADF.
func (s *Source) PostClaimComment(ctx context.Context, key, body string) error {
	if err := requireCtx(ctx, "comment"); err != nil {
		return err
	}
	if err := requireKey(key, "comment"); err != nil {
		return err
	}
	if strings.TrimSpace(body) == "" {
		return errors.New("jira comment: expected a non-empty comment body, found empty")
	}
	return s.postComment(ctx, "comment", key, body)
}

// LatestClaimComment reads the issue's comments and returns the latest one
// whose trimmed plain-text body starts with source.ClaimCommentPrefix, or nil
// when none exists.
func (s *Source) LatestClaimComment(ctx context.Context, key string) (*source.ClaimComment, error) {
	if err := requireCtx(ctx, "comments"); err != nil {
		return nil, err
	}
	if err := requireKey(key, "comments"); err != nil {
		return nil, err
	}
	basePath := "/rest/api/3/issue/" + url.PathEscape(key) + "/comment"
	var latest *source.ClaimComment
	startAt := 0
	for {
		path := basePath
		if startAt > 0 {
			path += "?startAt=" + strconv.Itoa(startAt)
		}
		raw, err := s.do(ctx, http.MethodGet, path, nil)
		if err != nil {
			return nil, fmt.Errorf("jira comments: %w", err)
		}
		page, err := decodeComments(key, raw)
		if err != nil {
			return nil, err
		}
		for i := range page.comments {
			c := page.comments[i]
			if !strings.HasPrefix(strings.TrimSpace(c.Body), source.ClaimCommentPrefix) {
				continue
			}
			if latest == nil || !c.CreatedAt.Before(latest.CreatedAt) {
				cp := c
				latest = &cp
			}
		}
		next := page.startAt + len(page.comments)
		if page.total == 0 || next >= page.total {
			break
		}
		if len(page.comments) == 0 {
			return nil, fmt.Errorf("jira comments %s: expected more comments (total %d, startAt %d), found empty page", key, page.total, page.startAt)
		}
		startAt = next
	}
	return latest, nil
}

// Release mirrors the configured claim settings: when mark_in_progress is
// enabled it moves the issue to the first ready status that has a transition
// and removes any configured claim label; when assign is enabled it clears the
// assignee.
func (s *Source) Release(ctx context.Context, key string) error {
	if err := requireCtx(ctx, "release"); err != nil {
		return err
	}
	if err := requireKey(key, "release"); err != nil {
		return err
	}
	st := s.effectiveClaim(source.ClaimOptions{}, true)
	if st.markInProgress {
		if st.status != "" || len(s.cfg.ReadyStatuses) > 0 {
			if err := s.transitionTo(ctx, "release", key, s.cfg.ReadyStatuses); err != nil {
				return err
			}
		}
		if st.label != "" {
			if err := s.updateLabel(ctx, "release", key, labelOp{Remove: st.label}); err != nil {
				return err
			}
		}
	}
	if !st.assign {
		return nil
	}
	return s.putAssignee(ctx, "release", key, nil)
}

// Finish moves the issue to finish_status.
// An empty finish_status returns source.ErrNotSupported.
func (s *Source) Finish(ctx context.Context, key string) error {
	if s.cfg.FinishStatus == "" {
		return source.ErrNotSupported
	}
	if err := requireCtx(ctx, "finish"); err != nil {
		return err
	}
	if err := requireKey(key, "finish"); err != nil {
		return err
	}
	return s.transitionTo(ctx, "finish", key, []string{s.cfg.FinishStatus})
}

// Close posts comment when non-blank and moves the issue to finish_status.
// The transition is resolved before posting comment so a missing or ambiguous
// transition never leaves an orphaned comment.
// An empty finish_status returns source.ErrNotSupported.
func (s *Source) Close(ctx context.Context, key, comment string) error {
	if s.cfg.FinishStatus == "" {
		return source.ErrNotSupported
	}
	if err := requireCtx(ctx, "close"); err != nil {
		return err
	}
	if err := requireKey(key, "close"); err != nil {
		return err
	}
	path, id, err := s.resolveTransition(ctx, "close", key, []string{s.cfg.FinishStatus})
	if err != nil {
		return err
	}
	if strings.TrimSpace(comment) != "" {
		if err := s.postComment(ctx, "close", key, comment); err != nil {
			return err
		}
	}
	return s.postTransition(ctx, "close", path, id)
}

// SetPriority writes the Jira priority name for rank.
// PUT /rest/api/3/issue/{key} sends fields.priority.name.
func (s *Source) SetPriority(ctx context.Context, key string, rank int) error {
	if err := requireCtx(ctx, "priority"); err != nil {
		return err
	}
	if err := requireKey(key, "priority"); err != nil {
		return err
	}
	name, err := priority.Name("jira", s.cfg, rank)
	if err != nil {
		return fmt.Errorf("jira priority %s: %w", key, err)
	}
	body, err := json.Marshal(priorityPut{Fields: priorityFields{Priority: jiraNamed{Name: name}}})
	if err != nil {
		return fmt.Errorf("jira priority %s: encode request: %w", key, err)
	}
	path := "/rest/api/3/issue/" + url.PathEscape(key)
	if _, err := s.do(ctx, http.MethodPut, path, body); err != nil {
		return fmt.Errorf("jira priority %s: %w", key, err)
	}
	return nil
}

// Link records that blocker blocks blocked.
//
// POST /rest/api/3/issueLink sends type.name = blocks_link_type,
// inwardIssue.key = the blocker, and outwardIssue.key = the blocked issue.
// Jira reads that body as inwardIssue blocks outwardIssue. GET of the blocked
// issue yields inwardIssue = blocker, which Sync and Get store as BlockedBy.
// On issue X, an inwardIssue Y means X is blocked by Y.
func (s *Source) Link(ctx context.Context, blocker, blocked string) error {
	if err := requireCtx(ctx, "link"); err != nil {
		return err
	}
	if blocker == "" {
		return errors.New("jira link: expected a blocker key, found empty")
	}
	if blocked == "" {
		return errors.New("jira link: expected a blocked key, found empty")
	}
	if s.cfg.BlocksLinkType == "" {
		return errors.New("jira link: expected blocks_link_type, found empty")
	}
	body, err := json.Marshal(issueLinkBody{
		Type:         jiraNamed{Name: s.cfg.BlocksLinkType},
		InwardIssue:  jiraRef{Key: blocker},
		OutwardIssue: jiraRef{Key: blocked},
	})
	if err != nil {
		return fmt.Errorf("jira link: encode request: %w", err)
	}
	if _, err := s.do(ctx, http.MethodPost, issueLinkPath, body); err != nil {
		return fmt.Errorf("jira link: %w", err)
	}
	return nil
}

// Unlink removes the blocks link from blocker to blocked.
// The link id is the BlockRef from Get(blocked) whose ID is the blocker.
// No such link returns source.ErrNotFound.
func (s *Source) Unlink(ctx context.Context, blocker, blocked string) error {
	if err := requireCtx(ctx, "unlink"); err != nil {
		return err
	}
	if blocker == "" {
		return errors.New("jira unlink: expected a blocker key, found empty")
	}
	if blocked == "" {
		return errors.New("jira unlink: expected a blocked key, found empty")
	}
	item, err := s.Get(ctx, blocked)
	if err != nil {
		return fmt.Errorf("jira unlink: %w", err)
	}
	ref, err := oneBlockRef(item, blocker, blocked)
	if err != nil {
		return err
	}
	path := issueLinkPath + "/" + url.PathEscape(ref.LinkID)
	if _, err := s.do(ctx, http.MethodDelete, path, nil); err != nil {
		return fmt.Errorf("jira unlink: %w", err)
	}
	return nil
}

// Create files an issue, then links each Blocks and BlockedBy entry.
// A link failure after the issue exists returns that key and an error.
func (s *Source) Create(ctx context.Context, req source.CreateRequest) (string, error) {
	if err := requireCtx(ctx, "create"); err != nil {
		return "", err
	}
	if req.Title == "" {
		return "", errors.New("jira create: expected a summary, found empty")
	}
	for _, label := range req.Labels {
		if label == "" {
			return "", errors.New("jira create: expected a label, found empty")
		}
	}
	if s.cfg.CreateIssueType == "" {
		return "", errors.New("jira create: expected create_issue_type, found empty")
	}
	parent := req.Parent
	if parent == "" {
		parent = s.cfg.DefaultParent
	}
	project, err := s.projectOf(req, parent)
	if err != nil {
		return "", err
	}
	issueType, err := s.createIssueType(ctx, project, parent)
	if err != nil {
		return "", err
	}
	assignee, err := s.createAssignee(ctx)
	if err != nil {
		return "", err
	}
	key, err := s.postCreate(ctx, req, project, parent, issueType, assignee)
	if err != nil {
		return key, err
	}
	if err := s.attachLinks(ctx, key, req); err != nil {
		return key, err
	}
	return key, nil
}

func (s *Source) transitionClaim(ctx context.Context, key, status, claimTransition string) error {
	path := "/rest/api/3/issue/" + url.PathEscape(key) + "/transitions"
	raw, err := s.do(ctx, http.MethodGet, path, nil)
	if err != nil {
		return fmt.Errorf("jira claim: %w", err)
	}
	list, err := decodeTransitions(raw)
	if err != nil {
		return fmt.Errorf("jira claim: %w", err)
	}
	chosen, unused, warn, err := chooseClaimTransition(status, claimTransition, list)
	if err != nil {
		return fmt.Errorf("jira claim %s: %w", key, err)
	}
	if warn {
		warnClaimChoice(key, chosen, unused)
	}
	return s.postTransition(ctx, "claim", path, chosen.ID)
}

func (s *Source) updateLabel(ctx context.Context, op, key string, change labelOp) error {
	body, err := json.Marshal(labelUpdatePut{
		Update: labelUpdateFields{Labels: []labelOp{change}},
	})
	if err != nil {
		return fmt.Errorf("jira %s: encode label update: %w", op, err)
	}
	path := "/rest/api/3/issue/" + url.PathEscape(key)
	if _, err := s.do(ctx, http.MethodPut, path, body); err != nil {
		return fmt.Errorf("jira %s: %w", op, err)
	}
	return nil
}

func (s *Source) transitionTo(ctx context.Context, op, key string, statuses []string) error {
	path, id, err := s.resolveTransition(ctx, op, key, statuses)
	if err != nil {
		return err
	}
	return s.postTransition(ctx, op, path, id)
}

func (s *Source) resolveTransition(ctx context.Context, op, key string, statuses []string) (string, string, error) {
	path := "/rest/api/3/issue/" + url.PathEscape(key) + "/transitions"
	raw, err := s.do(ctx, http.MethodGet, path, nil)
	if err != nil {
		return "", "", fmt.Errorf("jira %s: %w", op, err)
	}
	list, err := decodeTransitions(raw)
	if err != nil {
		return "", "", fmt.Errorf("jira %s: %w", op, err)
	}
	id, err := pickTransition(statuses, list)
	if err != nil {
		return "", "", fmt.Errorf("jira %s %s: %w", op, key, err)
	}
	return path, id, nil
}

func (s *Source) postTransition(ctx context.Context, op, path, id string) error {
	body, err := json.Marshal(transitionPost{Transition: idOnly{ID: id}})
	if err != nil {
		return fmt.Errorf("jira %s: encode transition: %w", op, err)
	}
	if _, err := s.do(ctx, http.MethodPost, path, body); err != nil {
		return fmt.Errorf("jira %s: %w", op, err)
	}
	return nil
}

func (s *Source) postComment(ctx context.Context, op, key, comment string) error {
	doc := adfDescription(comment)
	if doc == nil {
		return nil
	}
	body, err := json.Marshal(commentPost{Body: doc})
	if err != nil {
		return fmt.Errorf("jira %s: encode comment: %w", op, err)
	}
	path := "/rest/api/3/issue/" + url.PathEscape(key) + "/comment"
	if _, err := s.do(ctx, http.MethodPost, path, body); err != nil {
		return fmt.Errorf("jira %s: %w", op, err)
	}
	return nil
}

func (s *Source) putAssignee(ctx context.Context, op, key string, account *string) error {
	body, err := json.Marshal(assigneePost{AccountID: account})
	if err != nil {
		return fmt.Errorf("jira %s: encode assignee: %w", op, err)
	}
	path := "/rest/api/3/issue/" + url.PathEscape(key) + "/assignee"
	if _, err := s.do(ctx, http.MethodPut, path, body); err != nil {
		return fmt.Errorf("jira %s: %w", op, err)
	}
	return nil
}

// chooseClaimTransition selects among transitions whose target is status.
// warn is set only when the smallest id is used. unused are the rest.
func chooseClaimTransition(status, claimTransition string, list []transitionChoice) (transitionChoice, []transitionChoice, bool, error) {
	var landing []transitionChoice
	for _, tr := range list {
		if tr.To == status {
			landing = append(landing, tr)
		}
	}
	if len(landing) == 0 {
		_, err := pickTransition([]string{status}, list)
		return transitionChoice{}, nil, false, err
	}
	if len(landing) == 1 {
		if err := requireTransitionID(status, landing[0]); err != nil {
			return transitionChoice{}, nil, false, err
		}
		return landing[0], nil, false, nil
	}
	var named []transitionChoice
	for _, tr := range landing {
		if tr.Name == status {
			named = append(named, tr)
		}
	}
	if len(named) == 1 {
		if err := requireTransitionID(status, named[0]); err != nil {
			return transitionChoice{}, nil, false, err
		}
		return named[0], nil, false, nil
	}
	if claimTransition != "" {
		chosen, err := matchClaimTransition(status, claimTransition, list, landing)
		return chosen, nil, false, err
	}
	chosen, unused, err := smallestTransition(status, landing)
	if err != nil {
		return transitionChoice{}, nil, false, err
	}
	return chosen, unused, true, nil
}

func matchClaimTransition(status, want string, list, landing []transitionChoice) (transitionChoice, error) {
	var hits []transitionChoice
	for _, tr := range landing {
		if tr.ID == want || tr.Name == want {
			hits = append(hits, tr)
		}
	}
	switch len(hits) {
	case 1:
		if err := requireTransitionID(status, hits[0]); err != nil {
			return transitionChoice{}, err
		}
		return hits[0], nil
	case 0:
		for _, tr := range list {
			if tr.To == status {
				continue
			}
			if tr.ID == want || tr.Name == want {
				return transitionChoice{}, fmt.Errorf("expected claim_transition %q to land in %q, found %s to %q", want, status, quoteChoice(tr), tr.To)
			}
		}
		return transitionChoice{}, fmt.Errorf("expected claim_transition %q to match one transition to %q, found none", want, status)
	default:
		return transitionChoice{}, fmt.Errorf("expected one transition to %q for claim_transition %q, found %d", status, want, len(hits))
	}
}

func smallestTransition(status string, landing []transitionChoice) (transitionChoice, []transitionChoice, error) {
	for _, tr := range landing {
		if err := requireTransitionID(status, tr); err != nil {
			return transitionChoice{}, nil, err
		}
	}
	ordered := append([]transitionChoice(nil), landing...)
	slices.SortStableFunc(ordered, func(a, b transitionChoice) int {
		if lessTransitionID(a.ID, b.ID) {
			return -1
		}
		if lessTransitionID(b.ID, a.ID) {
			return 1
		}
		return 0
	})
	return ordered[0], ordered[1:], nil
}

func requireTransitionID(status string, tr transitionChoice) error {
	if tr.ID == "" {
		return fmt.Errorf("expected a transition id for %q, found empty", status)
	}
	return nil
}

// lessTransitionID compares ids as integers when both are integers.
// Otherwise, and when the integers are equal, it compares the strings.
func lessTransitionID(a, b string) bool {
	ai, aErr := strconv.Atoi(a)
	bi, bErr := strconv.Atoi(b)
	if aErr == nil && bErr == nil && ai != bi {
		return ai < bi
	}
	return a < b
}

func warnClaimChoice(key string, chosen transitionChoice, unused []transitionChoice) {
	parts := make([]string, len(unused))
	for i, tr := range unused {
		parts[i] = quoteChoice(tr)
	}
	slog.Warn("jira claim: chose transition",
		"issue", key,
		"id", chosen.ID,
		"name", chosen.Name,
		"unused", strings.Join(parts, ", "),
	)
}

func quoteChoice(tr transitionChoice) string {
	return strconv.Quote(tr.ID) + " " + strconv.Quote(tr.Name)
}

// pickTransition returns the id of the first wanted status that has exactly
// one transition. The match on to.name is exact. None available lists the
// to.name values that were present.
func pickTransition(want []string, list []transitionChoice) (string, error) {
	ids := make(map[string][]string, len(list))
	names := make([]string, 0, len(list))
	for _, tr := range list {
		names = append(names, tr.To)
		ids[tr.To] = append(ids[tr.To], tr.ID)
	}
	for _, status := range want {
		found := ids[status]
		switch len(found) {
		case 0:
			continue
		case 1:
			if found[0] == "" {
				return "", fmt.Errorf("expected a transition id for %q, found empty", status)
			}
			return found[0], nil
		default:
			return "", fmt.Errorf("expected one transition to %q, found %d", status, len(found))
		}
	}
	return "", fmt.Errorf("expected a transition to one of %s, found %s", quoted(want), quoted(names))
}

func (s *Source) attachLinks(ctx context.Context, key string, req source.CreateRequest) error {
	for _, blocked := range req.Blocks {
		if err := s.Link(ctx, key, blocked); err != nil {
			return missingLinks(key, err)
		}
	}
	for _, blocker := range req.BlockedBy {
		if err := s.Link(ctx, blocker, key); err != nil {
			return missingLinks(key, err)
		}
	}
	return nil
}

func missingLinks(key string, err error) error {
	return fmt.Errorf("jira create: issue %s exists but is missing links: %w", key, err)
}

// projectOf picks the new issue's project: --project, then create_project,
// then the parent's key prefix, then the prefix of the first --blocks or
// --blocked-by key.
func (s *Source) projectOf(req source.CreateRequest, parent string) (string, error) {
	switch {
	case req.Project != "":
		return req.Project, nil
	case s.cfg.CreateProject != "":
		return s.cfg.CreateProject, nil
	case parent != "":
		return keyProject("parent", parent)
	}
	links := append(append([]string(nil), req.Blocks...), req.BlockedBy...)
	if len(links) > 0 {
		return keyProject("linked", links[0])
	}
	return "", fmt.Errorf("jira create: no project for the new item; pass --project KEY or --parent KEY, or set create_project for source jira:%s", s.cfg.Name)
}

// keyProject returns the PROJECT part of a PROJECT-NUMBER key.
func keyProject(what, key string) (string, error) {
	project, rest, ok := strings.Cut(key, "-")
	if !ok || project == "" || rest == "" {
		return "", fmt.Errorf("jira create: expected a %s key PROJECT-NUMBER, found %q", what, key)
	}
	return project, nil
}

// createAssignee returns the account a new issue is assigned to.
// Nil means unassigned.
func (s *Source) createAssignee(ctx context.Context) (*string, error) {
	switch s.cfg.CreateAssignee {
	case config.CreateAssigneeUnassigned:
		return nil, nil
	case config.CreateAssigneeMe:
		account, err := s.Me(ctx)
		if err != nil {
			return nil, fmt.Errorf("jira create: %w", err)
		}
		return &account, nil
	default:
		return nil, fmt.Errorf("jira create: expected create_assignee me or unassigned, found %q", s.cfg.CreateAssignee)
	}
}

// postCreate creates the issue and returns its key.
// When Jira rejects only the assignee field, for example because it is not
// on the create screen, the issue is created without it and then assigned
// with PUT /assignee. A failed assignment after that returns the key.
func (s *Source) postCreate(ctx context.Context, req source.CreateRequest, project, parent, issueType string, assignee *string) (string, error) {
	body, err := createBody(req, project, parent, issueType, assignee)
	if err != nil {
		return "", err
	}
	raw, err := s.do(ctx, http.MethodPost, createIssuePath, body)
	if err != nil && assignee != nil && onlyAssigneeRejected(err) {
		slog.Warn("jira create rejected the assignee field; assigning after create", "error", err)
		key, err := s.postCreate(ctx, req, project, parent, issueType, nil)
		if err != nil {
			return key, err
		}
		if err := s.putAssignee(ctx, "create", key, assignee); err != nil {
			return key, fmt.Errorf("jira create: issue %s exists but is not assigned: %w", key, err)
		}
		return key, nil
	}
	if err != nil {
		return "", err
	}
	return decodeCreated(raw)
}

// onlyAssigneeRejected reports a 400 whose only field error is assignee.
func onlyAssigneeRejected(err error) bool {
	var se *statusError
	if !errors.As(err, &se) || se.status != http.StatusBadRequest {
		return false
	}
	var body struct {
		ErrorMessages []string          `json:"errorMessages"`
		Errors        map[string]string `json:"errors"`
	}
	if json.Unmarshal(se.body, &body) != nil {
		return false
	}
	if len(body.ErrorMessages) != 0 || len(body.Errors) != 1 {
		return false
	}
	_, ok := body.Errors["assignee"]
	return ok
}

func createBody(req source.CreateRequest, project, parent, issueType string, assignee *string) ([]byte, error) {
	fields := createFields{
		Project:     jiraRef{Key: project},
		IssueType:   jiraNamed{Name: issueType},
		Summary:     req.Title,
		Labels:      append([]string{}, req.Labels...),
		Parent:      parentRef(parent),
		Description: adfDescription(req.Body),
	}
	if assignee != nil {
		fields.Assignee = &jiraAssignee{AccountID: *assignee}
	}
	raw, err := json.Marshal(createEnvelope{Fields: fields})
	if err != nil {
		return nil, fmt.Errorf("jira create: encode request: %w", err)
	}
	return raw, nil
}

func parentRef(parent string) *jiraRef {
	if parent == "" {
		return nil
	}
	return &jiraRef{Key: parent}
}

func oneBlockRef(item source.Item, blocker, blocked string) (source.BlockRef, error) {
	var found []source.BlockRef
	for _, ref := range item.BlockedBy {
		if ref.ID == blocker {
			found = append(found, ref)
		}
	}
	switch len(found) {
	case 0:
		return source.BlockRef{}, fmt.Errorf("jira unlink: %s does not block %s: %w", blocker, blocked, source.ErrNotFound)
	case 1:
		if found[0].LinkID == "" {
			return source.BlockRef{}, fmt.Errorf("jira unlink: expected a link id for %s, found empty", blocker)
		}
		return found[0], nil
	default:
		return source.BlockRef{}, fmt.Errorf("jira unlink: expected one link from %s to %s, found %d", blocker, blocked, len(found))
	}
}

func requireCtx(ctx context.Context, op string) error {
	if ctx == nil {
		return fmt.Errorf("jira %s: expected a context, found nil", op)
	}
	return nil
}

func requireKey(key, op string) error {
	if key == "" {
		return fmt.Errorf("jira %s: expected an issue key, found empty", op)
	}
	return nil
}

func quoted(items []string) string {
	if len(items) == 0 {
		return "none"
	}
	parts := make([]string, len(items))
	for i, item := range items {
		parts[i] = strconv.Quote(item)
	}
	return strings.Join(parts, ", ")
}

// createIssueType picks the issue type for a new issue under parent.
// No parent or an epic-level parent uses create_issue_type. A standard
// parent uses a subtask type. A subtask parent is an error, because Jira
// does not allow its children.
func (s *Source) createIssueType(ctx context.Context, project, parent string) (string, error) {
	if parent == "" {
		return s.cfg.CreateIssueType, nil
	}
	pt, err := s.parentIssueType(ctx, parent)
	if err != nil {
		return "", err
	}
	switch {
	case pt.subtask:
		return "", fmt.Errorf("jira create: cannot file under %s (%s): Jira does not allow children of a subtask", parent, pt.name)
	case pt.level >= 1:
		return s.cfg.CreateIssueType, nil
	}
	if s.cfg.CreateSubtaskIssueType != "" {
		return s.cfg.CreateSubtaskIssueType, nil
	}
	return s.projectSubtaskType(ctx, project, parent, pt.name)
}

// parentType is the issue type of a parent issue.
type parentType struct {
	name    string
	subtask bool
	level   int
}

func (s *Source) parentIssueType(ctx context.Context, parent string) (parentType, error) {
	path := "/rest/api/3/issue/" + url.PathEscape(parent) + "?fields=issuetype"
	raw, err := s.do(ctx, http.MethodGet, path, nil)
	if err != nil {
		return parentType{}, fmt.Errorf("jira create: parent %s: %w", parent, err)
	}
	return decodeParentType(parent, raw)
}

func (s *Source) projectSubtaskType(ctx context.Context, project, parent, parentTypeName string) (string, error) {
	path := "/rest/api/3/project/" + url.PathEscape(project)
	raw, err := s.do(ctx, http.MethodGet, path, nil)
	if err != nil {
		return "", fmt.Errorf("jira create: project %s: %w", project, err)
	}
	names, err := decodeSubtaskTypes(project, raw)
	if err != nil {
		return "", err
	}
	switch len(names) {
	case 0:
		return "", fmt.Errorf("jira create: cannot file under %s (%s): project %s has no subtask issue type", parent, parentTypeName, project)
	case 1:
		return names[0], nil
	default:
		return "", fmt.Errorf("jira create: cannot file under %s (%s): expected one subtask issue type in project %s, found %s; set create_subtask_issue_type", parent, parentTypeName, project, quoted(names))
	}
}
