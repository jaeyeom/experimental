package github

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/priority"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// AddBlockedByInput and RemoveBlockedByInput were read from the live schema
// on 2026-11-01. Both have the fields clientMutationId, issueId, and
// blockingIssueId. issueId is the blocked issue and blockingIssueId is the
// blocker. clientMutationId is optional and is omitted.
const (
	addBlockedByQuery = "mutation($input:AddBlockedByInput!){" +
		"addBlockedBy(input:$input){issue{id} blockingIssue{id}}}"

	removeBlockedByQuery = "mutation($input:RemoveBlockedByInput!){" +
		"removeBlockedBy(input:$input){issue{id} blockingIssue{id}}}"
)

// AddSubIssueInput fields read on 2026-11-01 are clientMutationId, issueId,
// subIssueId, subIssueUrl, and replaceParent. issueId is the parent and
// subIssueId is the new issue. The other fields are omitted. A new issue
// has no parent, so replaceParent stays unset.
const addSubIssueQuery = "mutation($input:AddSubIssueInput!){" +
	"addSubIssue(input:$input){issue{id} subIssue{id}}}"

// planningStatusesQuery and setIssueStatusQuery were read from the live schema
// on 2026-11-24. SetIssueStatusInput has clientMutationId, issueId, statusId,
// and duplicateIssueId. clientMutationId and duplicateIssueId are omitted.
const (
	planningStatusesQuery = "query($owner:String!,$name:String!){" +
		"repository(owner:$owner,name:$name){" +
		"planningStatuses(first:50){nodes{id name}}}}"

	setIssueStatusQuery = "mutation($input:SetIssueStatusInput!){" +
		"setIssueStatus(input:$input){issue{id status{name}}}}"
)

const (
	claimLabelDescription = "Work is actively underway"
	claimLabelColor       = "1D76DB"
)

type claimSettings struct {
	assign         bool
	markInProgress bool
	createLabel    bool
	status         string
	label          string
	legacy         bool
}

func (s *Source) effectiveClaim(opts source.ClaimOptions) claimSettings {
	var st claimSettings
	if s.cfg.Claim == (config.Claim{}) {
		st = claimSettings{
			assign:         true,
			markInProgress: true,
			status:         s.cfg.ClaimStatus,
			label:          s.cfg.ClaimLabel,
			legacy:         true,
		}
	} else {
		status := s.cfg.Claim.Status
		if status == "" {
			status = s.cfg.ClaimStatus
		}
		label := s.cfg.Claim.Label
		if label == "" {
			label = s.cfg.ClaimLabel
		}
		st = claimSettings{
			assign:         s.cfg.Claim.Assign,
			markInProgress: s.cfg.Claim.MarkInProgress,
			createLabel:    s.cfg.Claim.CreateLabel,
			status:         status,
			label:          label,
		}
	}
	if opts.Assign != nil {
		st.assign = *opts.Assign
	}
	if opts.MarkInProgress != nil {
		st.markInProgress = *opts.MarkInProgress
	}
	if opts.CreateLabel != nil {
		st.createLabel = *opts.CreateLabel
	}
	return st
}

// Claim marks the issue taken using the source's configured [claim] settings.
func (s *Source) Claim(ctx context.Context, id string) error {
	return s.ClaimWithOptions(ctx, id, source.ClaimOptions{})
}

// ClaimWithOptions assigns @me (when assign is enabled), adds any configured
// claim label, and sets any configured native claim status (when
// mark_in_progress is enabled). Assignment runs before setting status so
// GitHub's auto-transition on assignment does not overwrite the claim status.
// When create_label is enabled and the claim label does not exist yet, it
// creates the label and retries the edit once.
func (s *Source) ClaimWithOptions(ctx context.Context, id string, opts source.ClaimOptions) error {
	if ctx == nil {
		return errors.New("github claim: expected a context, found nil")
	}
	st := s.effectiveClaim(opts)
	if st.legacy && st.markInProgress && st.status == "" && st.label == "" {
		return errors.New("github claim: expected a claim status or claim label, found empty")
	}
	number, err := s.repoIssue(id)
	if err != nil {
		return fmt.Errorf("github claim: %w", err)
	}
	flags := s.claimEditFlags(st, opts.RemoveAssignees)
	if len(flags) > 0 {
		if err := s.runClaimEdit(ctx, number, st, flags); err != nil {
			return err
		}
	}
	if st.markInProgress && st.status != "" {
		if err := s.setStatus(ctx, "claim", id, st.status); err != nil {
			return err
		}
	}
	return nil
}

func (s *Source) runClaimEdit(ctx context.Context, number int, st claimSettings, flags []string) error {
	_, err := s.run(ctx, s.editArgs(number, flags...)...)
	if err == nil {
		return nil
	}
	if st.markInProgress && st.label != "" && missingClaimLabel(err, st.label) {
		if !st.createLabel {
			return fmt.Errorf("github claim: create the %q label in %s first", st.label, s.cfg.Repo)
		}
		if createErr := s.createClaimLabel(ctx, st.label); createErr != nil {
			return fmt.Errorf("github claim: create label %q: %w", st.label, createErr)
		}
		if _, retryErr := s.run(ctx, s.editArgs(number, flags...)...); retryErr != nil {
			return fmt.Errorf("github claim: %w", retryErr)
		}
		return nil
	}
	return fmt.Errorf("github claim: %w", err)
}

func (s *Source) claimEditFlags(st claimSettings, removeAssignees []string) []string {
	var flags []string
	if st.markInProgress && st.label != "" {
		flags = append(flags, "--add-label", st.label)
	}
	if st.assign {
		flags = append(flags, "--add-assignee", "@me")
		s.mu.Lock()
		me := s.login
		cached := s.cached
		s.mu.Unlock()
		seen := make(map[string]struct{}, len(removeAssignees))
		for _, prev := range removeAssignees {
			prev = strings.TrimSpace(prev)
			if prev == "" || prev == "@me" {
				continue
			}
			if cached && strings.EqualFold(prev, me) {
				continue
			}
			key := strings.ToLower(prev)
			if _, dup := seen[key]; dup {
				continue
			}
			seen[key] = struct{}{}
			flags = append(flags, "--remove-assignee", prev)
		}
	}
	return flags
}

func (s *Source) createClaimLabel(ctx context.Context, label string) error {
	_, err := s.run(ctx,
		"label", "create", label,
		"--description", claimLabelDescription,
		"--color", claimLabelColor,
		"-R", s.cfg.Repo,
	)
	return err
}

// PostClaimComment posts a claim comment on the issue.
func (s *Source) PostClaimComment(ctx context.Context, id, body string) error {
	if ctx == nil {
		return errors.New("github comment: expected a context, found nil")
	}
	if strings.TrimSpace(body) == "" {
		return errors.New("github comment: expected a non-empty comment body, found empty")
	}
	number, err := s.repoIssue(id)
	if err != nil {
		return fmt.Errorf("github comment: %w", err)
	}
	if _, err := s.run(ctx, "issue", "comment", strconv.Itoa(number), "-R", s.cfg.Repo, "--body", body); err != nil {
		return fmt.Errorf("github comment: %w", err)
	}
	return nil
}

// LatestClaimComment reads the issue's comments and returns the latest one
// whose trimmed body starts with source.ClaimCommentPrefix, or nil when none
// exists.
func (s *Source) LatestClaimComment(ctx context.Context, id string) (*source.ClaimComment, error) {
	if ctx == nil {
		return nil, errors.New("github comments: expected a context, found nil")
	}
	number, err := s.repoIssue(id)
	if err != nil {
		return nil, fmt.Errorf("github comments: %w", err)
	}
	raw, err := s.run(ctx, "issue", "view", strconv.Itoa(number), "-R", s.cfg.Repo, "--json", "comments")
	if err != nil {
		return nil, fmt.Errorf("github comments: %w", err)
	}
	comments, err := decodeComments(raw)
	if err != nil {
		return nil, fmt.Errorf("github comments: %w", err)
	}
	var latest *source.ClaimComment
	for i := range comments {
		c := comments[i]
		if !strings.HasPrefix(strings.TrimSpace(c.Body), source.ClaimCommentPrefix) {
			continue
		}
		if latest == nil || !c.CreatedAt.Before(latest.CreatedAt) {
			cp := c
			latest = &cp
		}
	}
	return latest, nil
}

// Release mirrors the configured claim settings, removing any configured claim
// label and unassigning @me, and moving any configured native claim status back
// to the first non-OPEN ready_statuses entry (defaulting to Todo).
func (s *Source) Release(ctx context.Context, id string) error {
	if ctx == nil {
		return errors.New("github release: expected a context, found nil")
	}
	st := s.effectiveClaim(source.ClaimOptions{})
	if st.legacy && st.markInProgress && st.status == "" && st.label == "" {
		return errors.New("github release: expected a claim status or claim label, found empty")
	}
	number, err := s.repoIssue(id)
	if err != nil {
		return fmt.Errorf("github release: %w", err)
	}
	var flags []string
	if st.markInProgress && st.label != "" {
		flags = append(flags, "--remove-label", st.label)
	}
	if st.assign {
		flags = append(flags, "--remove-assignee", "@me")
	}
	if len(flags) > 0 {
		if _, err := s.run(ctx, s.editArgs(number, flags...)...); err != nil {
			return fmt.Errorf("github release: %w", err)
		}
	}
	if st.markInProgress && st.status != "" {
		if err := s.setStatus(ctx, "release", id, s.releaseStatus()); err != nil {
			return err
		}
	}
	return nil
}

func (s *Source) releaseStatus() string {
	for _, st := range s.cfg.ReadyStatuses {
		if st != "" && st != config.GitHubStateOpen {
			return st
		}
	}
	return config.DefaultGitHubReleaseStatus
}

func (s *Source) setStatus(ctx context.Context, op, id, target string) error {
	statusID, err := s.planningStatusID(ctx, op, target)
	if err != nil {
		return err
	}
	item, nodeID, err := s.fetchIssue(ctx, id)
	if err != nil {
		return fmt.Errorf("github %s: %w", op, err)
	}
	if item.State == target {
		return nil
	}
	raw, err := s.run(ctx, setIssueStatusArgs(nodeID, statusID)...)
	if err != nil {
		return fmt.Errorf("github %s: %w", op, err)
	}
	if err := decodeSetIssueStatus(raw, target); err != nil {
		return fmt.Errorf("github %s: %w", op, err)
	}
	return nil
}

func (s *Source) planningStatusID(ctx context.Context, op, target string) (string, error) {
	ids, names, err := s.loadPlanningStatuses(ctx, op)
	if err != nil {
		return "", err
	}
	if len(ids) == 0 {
		return "", fmt.Errorf("github %s: repository %s has no native issue planning statuses (set claim_label to use label-based claiming)", op, s.cfg.Repo)
	}
	statusID, ok := ids[target]
	if !ok {
		return "", fmt.Errorf("github %s: expected status in %s (%s), found %q", op, s.cfg.Repo, strings.Join(names, ", "), target)
	}
	return statusID, nil
}

func (s *Source) loadPlanningStatuses(ctx context.Context, op string) (map[string]string, []string, error) {
	s.mu.Lock()
	if s.statusCached {
		ids, names := s.statusIDs, s.statusNames
		s.mu.Unlock()
		return ids, names, nil
	}
	s.mu.Unlock()

	owner, name, err := splitRepo(s.cfg.Repo)
	if err != nil {
		return nil, nil, fmt.Errorf("github %s: %w", op, err)
	}
	raw, err := s.run(ctx, planningStatusesArgs(owner, name)...)
	if err != nil {
		return nil, nil, fmt.Errorf("github %s: %w", op, err)
	}
	ids, names, err := decodePlanningStatuses(raw)
	if err != nil {
		return nil, nil, fmt.Errorf("github %s: %w", op, err)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.statusCached {
		s.statusIDs = ids
		s.statusNames = names
		s.statusCached = true
	}
	return s.statusIDs, s.statusNames, nil
}

func planningStatusesArgs(owner, name string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + planningStatusesQuery,
		"-f", "owner=" + owner,
		"-f", "name=" + name,
	}
}

func setIssueStatusArgs(issueID, statusID string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + setIssueStatusQuery,
		"-f", "input[issueId]=" + issueID,
		"-f", "input[statusId]=" + statusID,
	}
}

// Finish returns nil. A pull request that says Fixes #N closes the issue
// on merge, so there is nothing to do here.
func (*Source) Finish(context.Context, string) error {
	return nil
}

// Close closes the issue and posts comment when non-blank.
func (s *Source) Close(ctx context.Context, id, comment string) error {
	if ctx == nil {
		return errors.New("github close: expected a context, found nil")
	}
	number, err := s.repoIssue(id)
	if err != nil {
		return fmt.Errorf("github close: %w", err)
	}
	if _, err := s.run(ctx, s.closeArgs(number, comment)...); err != nil {
		return fmt.Errorf("github close: %w", err)
	}
	return nil
}

// SetPriority writes the priority for rank. When priority_labels is configured,
// it adds the matching label via `gh issue edit`. Otherwise it sets the native
// single-select issue field configured by priority_field (default "Priority")
// via GraphQL setIssueFieldValue.
// A rank with no single configured name returns an error and does not call gh.
func (s *Source) SetPriority(ctx context.Context, id string, rank int) error {
	if ctx == nil {
		return errors.New("github priority: expected a context, found nil")
	}
	name, err := priority.Name("github", s.cfg, rank)
	if err != nil {
		return fmt.Errorf("github priority %s: %w", id, err)
	}
	if len(s.cfg.PriorityLabels) > 0 {
		number, err := s.repoIssue(id)
		if err != nil {
			return fmt.Errorf("github priority: %w", err)
		}
		if _, err := s.run(ctx, s.editArgs(number, "--add-label", name)...); err != nil {
			return fmt.Errorf("github priority: %w", err)
		}
		return nil
	}
	if s.cfg.PriorityField == "" {
		return fmt.Errorf("github priority %s: expected priority_field or priority_labels, found neither", id)
	}
	return s.setPriorityField(ctx, id, name)
}

// Link records that blocker blocks blocked.
// Node ids come from Get. The mutation variables are issueId (blocked) and
// blockingIssueId (blocker).
func (s *Source) Link(ctx context.Context, blocker, blocked string) error {
	if ctx == nil {
		return errors.New("github link: expected a context, found nil")
	}
	_, blockerNode, err := s.fetchIssue(ctx, blocker)
	if err != nil {
		return fmt.Errorf("github link: %w", err)
	}
	_, blockedNode, err := s.fetchIssue(ctx, blocked)
	if err != nil {
		return fmt.Errorf("github link: %w", err)
	}
	raw, err := s.run(ctx, blockedByArgs(addBlockedByQuery, blockedNode, blockerNode)...)
	if err != nil {
		return fmt.Errorf("github link: %w", err)
	}
	if err := decodeMutationIDs(raw, "addBlockedBy", "issue", "blockingIssue"); err != nil {
		return fmt.Errorf("github link: %w", err)
	}
	return nil
}

// Unlink removes the blocked-by link. No such link is source.ErrNotFound.
// The blocker node id is BlockRef.LinkID from Get(blocked), so a cross-repo
// blocker does not need its own Get.
func (s *Source) Unlink(ctx context.Context, blocker, blocked string) error {
	if ctx == nil {
		return errors.New("github unlink: expected a context, found nil")
	}
	if _, _, err := source.ParseGitHubID(blocker); err != nil {
		return fmt.Errorf("github unlink: blocker: %w", err)
	}
	item, blockedNode, err := s.fetchIssue(ctx, blocked)
	if err != nil {
		return fmt.Errorf("github unlink: %w", err)
	}
	ref, ok := blockRef(item, blocker)
	if !ok {
		return fmt.Errorf("github unlink: %s does not block %s: %w", blocker, blocked, source.ErrNotFound)
	}
	if ref.LinkID == "" {
		return fmt.Errorf("github unlink: expected a blocker node id for %s, found empty", blocker)
	}
	raw, err := s.run(ctx, blockedByArgs(removeBlockedByQuery, blockedNode, ref.LinkID)...)
	if err != nil {
		return fmt.Errorf("github unlink: %w", err)
	}
	if err := decodeMutationIDs(raw, "removeBlockedBy", "issue", "blockingIssue"); err != nil {
		return fmt.Errorf("github unlink: %w", err)
	}
	return nil
}

// Create files an issue, then sets its parent and blocked-by links.
// gh prompts unless --title and --body are both passed, so an empty title
// is rejected and an empty body is still sent. A failure after the issue
// exists returns that id and an error.
func (s *Source) Create(ctx context.Context, req source.CreateRequest) (string, error) {
	if ctx == nil {
		return "", errors.New("github create: expected a context, found nil")
	}
	if req.Title == "" {
		return "", errors.New("github create: expected a title, found empty")
	}
	for _, label := range req.Labels {
		if label == "" {
			return "", errors.New("github create: expected a label, found empty")
		}
	}
	if _, _, err := splitRepo(s.cfg.Repo); err != nil {
		return "", fmt.Errorf("github create: %w", err)
	}
	args, err := createArgs(s.cfg.Repo, s.cfg.CreateAssignee, req)
	if err != nil {
		return "", fmt.Errorf("github create: %w", err)
	}
	raw, err := s.run(ctx, args...)
	if err != nil {
		return "", fmt.Errorf("github create: %w", err)
	}
	number, err := parseCreatedNumber(s.cfg.Repo, string(raw))
	if err != nil {
		return "", fmt.Errorf("github create: %w", err)
	}
	id := source.GitHubID(s.cfg.Repo, number)
	if err := s.attachParent(ctx, id, req.Parent); err != nil {
		return id, err
	}
	if err := s.attachLinks(ctx, id, req); err != nil {
		return id, err
	}
	return id, nil
}

func (s *Source) repoIssue(id string) (int, error) {
	_, _, number, err := s.issueRef(id)
	return number, err
}

func (s *Source) attachParent(ctx context.Context, id, parent string) error {
	if parent == "" {
		return nil
	}
	_, subNode, err := s.fetchIssue(ctx, id)
	if err != nil {
		return parentAttachError(id, parent, err)
	}
	_, parentNode, err := s.fetchIssue(ctx, parent)
	if err != nil {
		return parentAttachError(id, parent, err)
	}
	raw, err := s.run(ctx, addSubIssueArgs(parentNode, subNode)...)
	if err != nil {
		return parentAttachError(id, parent, err)
	}
	if err := decodeMutationIDs(raw, "addSubIssue", "issue", "subIssue"); err != nil {
		return parentAttachError(id, parent, err)
	}
	return nil
}

func parentAttachError(id, parent string, err error) error {
	return fmt.Errorf("github create: issue %s exists, but setting parent %s failed: %s", id, parent, err.Error())
}

func (s *Source) attachLinks(ctx context.Context, id string, req source.CreateRequest) error {
	for _, blocked := range req.Blocks {
		if err := s.Link(ctx, id, blocked); err != nil {
			return missingLinks(id, err)
		}
	}
	for _, blocker := range req.BlockedBy {
		if err := s.Link(ctx, blocker, id); err != nil {
			return missingLinks(id, err)
		}
	}
	return nil
}

func missingLinks(id string, err error) error {
	return fmt.Errorf("github create: issue %s exists but is missing links: %s", id, err.Error())
}

func (s *Source) editArgs(number int, flags ...string) []string {
	args := []string{"issue", "edit", strconv.Itoa(number), "-R", s.cfg.Repo}
	return append(args, flags...)
}

func (s *Source) closeArgs(number int, comment string) []string {
	args := []string{"issue", "close", strconv.Itoa(number), "-R", s.cfg.Repo}
	if strings.TrimSpace(comment) != "" {
		args = append(args, "--comment", comment)
	}
	return args
}

// createArgs builds gh issue create. assignee is config.CreateAssigneeMe,
// which adds --assignee @me, or config.CreateAssigneeUnassigned.
func createArgs(repo, assignee string, req source.CreateRequest) ([]string, error) {
	args := []string{
		"issue", "create",
		"-R", repo,
		"--title", req.Title,
		"--body", req.Body,
	}
	for _, label := range req.Labels {
		args = append(args, "--label", label)
	}
	switch assignee {
	case config.CreateAssigneeMe:
		args = append(args, "--assignee", "@me")
	case config.CreateAssigneeUnassigned:
	default:
		return nil, fmt.Errorf("expected create_assignee me or unassigned, found %q", assignee)
	}
	return args, nil
}

// blockedByArgs passes issueId and blockingIssueId as strings.
// -f does not infer a type, so a node id stays a string.
func blockedByArgs(query, issueID, blockingIssueID string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + query,
		"-f", "input[issueId]=" + issueID,
		"-f", "input[blockingIssueId]=" + blockingIssueID,
	}
}

func addSubIssueArgs(parentNode, subNode string) []string {
	return []string{
		"api", "graphql",
		"-f", "query=" + addSubIssueQuery,
		"-f", "input[issueId]=" + parentNode,
		"-f", "input[subIssueId]=" + subNode,
	}
}

func blockRef(item source.Item, blocker string) (source.BlockRef, bool) {
	for _, ref := range item.BlockedBy {
		if ref.ID == blocker {
			return ref, true
		}
	}
	return source.BlockRef{}, false
}

func missingClaimLabel(err error, label string) bool {
	text := err.Error()
	if !strings.Contains(text, "could not add label:") {
		return false
	}
	single := "'" + label + "' not found"
	double := `"` + label + `" not found`
	return strings.Contains(text, single) || strings.Contains(text, double)
}

func parseCreatedNumber(repo, output string) (int, error) {
	text := strings.TrimSpace(output)
	if text == "" {
		return 0, errors.New("expected an issue URL, found empty")
	}
	if strings.ContainsAny(text, " \t\r\n") {
		return 0, fmt.Errorf("expected one issue URL, found %q", text)
	}
	prefix := "https://github.com/" + repo + "/issues/"
	numberText, ok := strings.CutPrefix(text, prefix)
	if !ok {
		return 0, fmt.Errorf("expected an issue URL %s<number>, found %q", prefix, text)
	}
	_, number, err := source.ParseGitHubID(repo + "#" + numberText)
	if err != nil {
		return 0, fmt.Errorf("expected an issue URL %s<number>, found %q", prefix, text)
	}
	return number, nil
}
