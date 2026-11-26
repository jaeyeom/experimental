package doctor

import (
	"cmp"
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/herdr"
	"github.com/jaeyeom/experimental/devtools/factory/internal/parentclose"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// unknownBlockers reports each blocker id that is not in the mirror.
// A blocker the last sync looked up (Input.Blockers) is reported by what
// that lookup found: a Done blocker is no problem, an open one names its status and
// assignee, a missing one says so, and one that was not checked is info.
func unknownBlockers(in Input) []Finding {
	var out []Finding
	seen := make(map[string]struct{})
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		for _, ref := range it.BlockedBy {
			if _, ok := in.Snapshot.Items[ref.ID]; ok {
				continue
			}
			if !firstEdge(seen, id, ref.ID) {
				continue
			}
			if found, ok := unknownBlocker(in, id, ref.ID); ok {
				out = append(out, found)
			}
		}
	}
	return out
}

func unknownBlocker(in Input, id, blocker string) (Finding, bool) {
	look, ok := in.Blockers[blocker]
	if !ok {
		return finding(
			CheckUnknownBlocker,
			SeverityWarn,
			fmt.Sprintf("%s is blocked by %s, which is not in the mirror", id, blocker),
			"widen jql, or unlink",
			id, blocker,
		), true
	}
	switch look.Status {
	case BlockerFound:
		if look.Item.Done {
			return Finding{}, false
		}
		assignee := look.Item.Assignee
		if assignee == "" {
			assignee = "unassigned"
		}
		return finding(
			CheckUnknownBlocker,
			SeverityWarn,
			fmt.Sprintf("%s is blocked by %s (%s, assignee %s), which is not in the mirror", id, blocker, look.Item.State, assignee),
			"wait for it, widen jql, or unlink",
			id, blocker,
		), true
	case BlockerNotFound:
		return finding(
			CheckUnknownBlocker,
			SeverityWarn,
			fmt.Sprintf("%s is blocked by %s, which was not found or is not visible", id, blocker),
			"check the link, or unlink",
			id, blocker,
		), true
	default:
		return finding(
			CheckUnknownBlocker,
			SeverityInfo,
			fmt.Sprintf("%s is blocked by %s, which is not in the mirror and was not checked: %s", id, blocker, look.Reason),
			"widen jql, or raise external_blocker_lookups",
			id, blocker,
		), true
	}
}

func firstEdge(seen map[string]struct{}, a, b string) bool {
	key := a + "\x00" + b
	if _, ok := seen[key]; ok {
		return false
	}
	seen[key] = struct{}{}
	return true
}

func orphans(in Input) []Finding {
	if in.Kind != "jira" {
		return nil
	}
	var out []Finding
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		if it.Done || it.Parent != "" || skippedType(it.Type, in.Config.SkipTypes) || ignoredOrphan(it, in.Config) {
			continue
		}
		found := finding(CheckOrphan, SeverityWarn, id+" has no parent", "set its parent epic", id)
		if options := orphanOptions(it, in); options != nil {
			found.Suggestion = strings.Join(options, "; ")
			found.Suggestions = options
		}
		out = append(out, found)
	}
	return out
}

// orphanOptions lists the fixes that work when "set its parent epic" may
// not: the orphan's type cannot sit under an epic, or it is linked to an
// open task-level item, its likely umbrella. Nil means the plain suggestion.
func orphanOptions(it source.Item, in Input) []string {
	epicTypes := in.Config.OrphanEpicChildTypes
	if len(epicTypes) == 0 {
		return nil
	}
	umbrella, ok := taskUmbrella(it, in)
	if ok {
		move := "move it under " + umbrella.ID + "'s epic"
		if umbrella.Parent != "" {
			move = "move it under " + umbrella.ID + "'s epic " + umbrella.Parent
		}
		return []string{
			"link it to " + umbrella.ID + " (relates or blocks)",
			move,
			"add " + it.ID + " to orphan_ignore",
		}
	}
	if skippedType(it.Type, epicTypes) {
		return nil
	}
	return []string{
		"link it to its umbrella item (relates or blocks)",
		"move it under that item's epic",
		"add " + it.ID + " to orphan_ignore",
	}
}

// taskUmbrella is the first open mirror item, by id, that blocks or is
// blocked by it and whose type is one that sits under an epic.
func taskUmbrella(it source.Item, in Input) (source.Item, bool) {
	linked := make(map[string]struct{})
	for _, ref := range it.BlockedBy {
		linked[ref.ID] = struct{}{}
	}
	for _, id := range sortedIDs(in.Snapshot.Items) {
		other := in.Snapshot.Items[id]
		for _, ref := range other.BlockedBy {
			if ref.ID == it.ID {
				linked[id] = struct{}{}
			}
		}
	}
	for _, id := range sortedIDs(in.Snapshot.Items) {
		if _, ok := linked[id]; !ok || id == it.ID {
			continue
		}
		other := in.Snapshot.Items[id]
		if !other.Done && skippedType(other.Type, in.Config.OrphanEpicChildTypes) {
			return other, true
		}
	}
	return source.Item{}, false
}

// badDeferLabels reports each open GitHub item with a defer_label_prefix
// label whose date does not parse. Such a label defers nothing.
func badDeferLabels(in Input) []Finding {
	prefix := in.Config.DeferLabelPrefix
	if in.Kind != "github" || prefix == "" {
		return nil
	}
	var out []Finding
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		if it.Done {
			continue
		}
		_, bad := source.LabelNotBefore(it.Labels, prefix)
		for _, label := range bad {
			out = append(out, finding(
				CheckBadDeferLabel,
				SeverityWarn,
				fmt.Sprintf("%s label %q is not %sYYYY-MM-DD, so it defers nothing", id, label, prefix),
				"fix the date or remove the label",
				id,
			))
		}
	}
	return out
}

func ignoredOrphan(it source.Item, cfg config.Source) bool {
	for _, id := range cfg.OrphanIgnore {
		if it.ID == id {
			return true
		}
	}
	for _, label := range it.Labels {
		if skippedType(label, cfg.OrphanIgnoreLabels) {
			return true
		}
	}
	return false
}

func skippedType(typ string, skip []string) bool {
	for _, name := range skip {
		if strings.EqualFold(typ, name) {
			return true
		}
	}
	return false
}

// droppedClaims reports items whose state equals claim_status, with no tab and
// no open pull request. An empty claim_status means the source does not track
// claim status (for example a legacy GitHub source using claim_label only), so
// the check does not apply.
// A parent is skipped: an item with an open child in the mirror, or whose type
// is in claim_parent_types. A claim recorded on a lane in Input.Lanes is
// matched against that lane's tabs, and an unreachable lane gives info.
func droppedClaims(in Input) ([]Finding, error) {
	if in.Config.ClaimStatus == "" {
		return nil, nil
	}
	parents := openParents(in.Snapshot.Items)
	var out []Finding
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		if it.State != in.Config.ClaimStatus {
			continue
		}
		if _, ok := parents[id]; ok || skippedType(it.Type, in.Config.ClaimParentTypes) {
			continue
		}
		if hasOpenPR(it, in) || hasLandedCommit(in, id) {
			continue
		}
		if hasActiveSubagentClaim(in, id) {
			continue
		}
		workspaces, tabs := in.Snapshot.Workspaces, in.Snapshot.Tabs
		if lane, ok := claimLane(in, id); ok {
			view := in.Lanes[lane]
			if view.Unreachable != "" {
				out = append(out, finding(
					CheckDroppedClaim,
					SeverityInfo,
					fmt.Sprintf("%s is %s: lane %s unreachable; tab not checked", id, it.State, lane),
					"run doctor again when the lane answers",
					id,
				))
				continue
			}
			workspaces, tabs = view.Workspaces, view.Tabs
		}
		matched, err := matchTabsIn(it, in, workspaces, tabs)
		if err != nil {
			return nil, err
		}
		if len(matched) > 0 {
			continue
		}
		msg := fmt.Sprintf("%s is %s with no tab and no open PR", id, it.State)
		if isMergeMode(in, id) {
			msg = fmt.Sprintf("%s is %s with no tab and no landed commit", id, it.State)
		}
		out = append(out, finding(
			CheckDroppedClaim,
			SeverityWarn,
			msg,
			fmt.Sprintf("factory release %s or start it", id),
			id,
		))
	}
	return out, nil
}

// hasActiveSubagentClaim reports whether id has an active claim started via
// spawner = "subagent", which runs without a herdr tab.
func hasActiveSubagentClaim(in Input, id string) bool {
	for _, claim := range in.Claims {
		if claim.Source == in.SourceKey && claim.ID == id && claim.State.Active() && claim.Spawner == config.SpawnerSubagent {
			return true
		}
	}
	return false
}

// openParents is the set of ids that some open mirror item names as Parent.
func openParents(items map[string]source.Item) map[string]struct{} {
	out := make(map[string]struct{})
	for _, it := range items {
		if !it.Done && it.Parent != "" {
			out[it.Parent] = struct{}{}
		}
	}
	return out
}

// claimLane is the lane recorded on this source's claim for id, when that
// lane is in Input.Lanes.
func claimLane(in Input, id string) (string, bool) {
	for _, claim := range in.Claims {
		if claim.Source != in.SourceKey || claim.ID != id || claim.Lane == "" {
			continue
		}
		if _, ok := in.Lanes[claim.Lane]; ok {
			return claim.Lane, true
		}
	}
	return "", false
}

func staleTabs(in Input) ([]Finding, error) {
	var out []Finding
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		if !it.Done {
			continue
		}
		tabs, err := matchTabs(it, in)
		if err != nil {
			return nil, err
		}
		slices.SortFunc(tabs, func(a, b herdr.Tab) int { return cmp.Compare(a.ID, b.ID) })
		for _, tab := range tabs {
			if !staleAgent(tab.AgentStatus) {
				continue
			}
			out = append(out, finding(
				CheckStaleTab,
				SeverityWarn,
				fmt.Sprintf("%s is Done and tab %s is %s", id, tab.Label, tab.AgentStatus),
				"close the tab (prsync tabs)",
				id, tab.ID,
			))
		}
	}
	return out, nil
}

func staleAgent(status string) bool {
	return status == herdr.AgentStatusIdle || status == herdr.AgentStatusDone
}

// stalledClaims reports a started claim whose UpdatedAt is older than StallAfter
// and that has no open pull request. UpdatedAt moves when the claim enters
// started, so the age is time in this state, not time since ClaimedAt.
// A claim whose item is already done is not a stuck start. That finding says
// to release the claim. doctor does not release it.
func stalledClaims(in Input) ([]Finding, error) {
	claims := append([]store.Claim(nil), in.Claims...)
	slices.SortFunc(claims, compareClaims)
	var out []Finding
	for _, claim := range claims {
		if claim.Source != in.SourceKey {
			continue
		}
		if err := knownClaimState(claim); err != nil {
			return nil, err
		}
		if claim.State != store.Started {
			continue
		}
		found, ok, err := stalledClaim(in, claim)
		if err != nil {
			return nil, err
		}
		if ok {
			out = append(out, found)
		}
	}
	return out, nil
}

func compareClaims(a, b store.Claim) int {
	if c := a.UpdatedAt.Compare(b.UpdatedAt); c != 0 {
		return c
	}
	if c := cmp.Compare(a.ID, b.ID); c != 0 {
		return c
	}
	return a.ClaimedAt.Compare(b.ClaimedAt)
}

func knownClaimState(claim store.Claim) error {
	if claim.ID == "" {
		return fmt.Errorf("claim %s: expected a non-empty id, found empty", claim.Source)
	}
	switch claim.State {
	case store.Claimed, store.Started, store.HandedOff, store.Done, store.Released, store.Failed:
		return nil
	default:
		return fmt.Errorf(
			"claim %s/%s: expected claimed, started, handed_off, done, released, or failed, found %q",
			claim.Source, claim.ID, claim.State,
		)
	}
}

func stalledClaim(in Input, claim store.Claim) (Finding, bool, error) {
	if claim.UpdatedAt.IsZero() {
		return Finding{}, false, fmt.Errorf("claim %s/%s: expected updated_at, found zero", claim.Source, claim.ID)
	}
	it, ok := in.Snapshot.Items[claim.ID]
	if !ok {
		return Finding{}, false, fmt.Errorf("claim %s/%s: expected an item, found none", claim.Source, claim.ID)
	}
	if in.Now.Sub(claim.UpdatedAt) <= in.StallAfter || hasOpenPR(it, in) {
		return Finding{}, false, nil
	}
	if it.Done {
		return finding(
			CheckStalled,
			SeverityWarn,
			fmt.Sprintf("%s done: release claim", claim.ID),
			"factory settle",
			claim.ID,
		), true, nil
	}
	if hasLandedCommit(in, claim.ID) {
		return finding(
			CheckStalled,
			SeverityWarn,
			fmt.Sprintf("%s landed: settle claim", claim.ID),
			fmt.Sprintf("factory settle %s", claim.ID),
			claim.ID,
		), true, nil
	}
	if claim.Spawner == config.SpawnerSubagent {
		return Finding{}, false, nil
	}
	msg := fmt.Sprintf("%s is started and older than %s with no open PR", claim.ID, in.StallAfter)
	if isMergeMode(in, claim.ID) {
		msg = fmt.Sprintf("%s is started and older than %s with no landed commit", claim.ID, in.StallAfter)
	}
	return finding(
		CheckStalled,
		SeverityWarn,
		msg,
		"look at the tab",
		claim.ID,
	), true, nil
}

func almostReady(in Input) []Finding {
	var out []Finding
	for _, id := range sortedIDs(in.Snapshot.Items) {
		it := in.Snapshot.Items[id]
		missing, ok := onlyMissingRequire(it, in)
		if !ok {
			continue
		}
		out = append(out, finding(
			CheckAlmostReady,
			SeverityInfo,
			fmt.Sprintf("%s is missing %s", id, strings.Join(missing, ", ")),
			"add agent-ready",
			id,
		))
	}
	return out
}

func onlyMissingRequire(it source.Item, in Input) ([]string, bool) {
	res := ready.Evaluate(it, in.Snapshot, in.Config, in.Kind)
	for _, check := range res.Checks {
		if check.Rule == ready.RuleLabels {
			continue
		}
		if !check.Pass {
			return nil, false
		}
	}
	missing := missingRequire(it, in.Config)
	if len(missing) == 0 || hasExcluded(it, in.Config) {
		return nil, false
	}
	return missing, true
}

func missingRequire(it source.Item, cfg config.Source) []string {
	var missing []string
	for _, want := range cfg.RequireLabels {
		if !slices.Contains(it.Labels, want) {
			missing = append(missing, want)
		}
	}
	return missing
}

func hasExcluded(it source.Item, cfg config.Source) bool {
	for _, bad := range cfg.ExcludeLabels {
		if slices.Contains(it.Labels, bad) {
			return true
		}
	}
	return false
}

func hasOpenPR(it source.Item, in Input) bool {
	return len(gh.ForItem(in.Snapshot.OpenPRs, it, in.Config.PRTitlePattern)) > 0
}

func hasLandedCommit(in Input, id string) bool {
	return in.LandedCommits[id]
}

func isMergeMode(in Input, id string) bool {
	for _, claim := range in.Claims {
		if claim.Source == in.SourceKey && claim.ID == id && claim.State.Active() && claim.OutputMode != "" {
			return claim.OutputMode == config.OutputModeMerge
		}
	}
	return in.Config.Output.Mode == config.OutputModeMerge
}

// matchTabs uses the same tab identity as ready.TabRef and ready's tab rule.
// Jira matches the tab label in every workspace. GitHub matches that label in
// every workspace whose label is the TabRef workspace.
func matchTabs(it source.Item, in Input) ([]herdr.Tab, error) {
	return matchTabsIn(it, in, in.Snapshot.Workspaces, in.Snapshot.Tabs)
}

func matchTabsIn(it source.Item, in Input, workspaces []herdr.Workspace, tabs []herdr.Tab) ([]herdr.Tab, error) {
	wsLabel, tabLabel, err := tabRef(it, in.Config, in.Kind)
	if err != nil {
		return nil, err
	}
	switch in.Kind {
	case "jira":
		return tabsWithLabel(tabs, nil, tabLabel), nil
	case "github":
		return tabsWithLabel(tabs, workspaceSet(workspaces, wsLabel), tabLabel), nil
	default:
		return nil, fmt.Errorf("doctor: expected kind jira or github, found %q", in.Kind)
	}
}

func tabRef(it source.Item, cfg config.Source, kind string) (string, string, error) {
	if kind != "jira" && kind != "github" {
		return "", "", fmt.Errorf("doctor: expected kind jira or github, found %q", kind)
	}
	ws, tab := ready.TabRef(it, cfg, kind)
	return ws, tab, nil
}

func workspaceSet(workspaces []herdr.Workspace, label string) map[string]struct{} {
	ids := make(map[string]struct{})
	for _, ws := range workspaces {
		if ws.Label == label {
			ids[ws.ID] = struct{}{}
		}
	}
	return ids
}

func tabsWithLabel(tabs []herdr.Tab, workspaces map[string]struct{}, label string) []herdr.Tab {
	var out []herdr.Tab
	for _, tab := range tabs {
		if tab.Label != label {
			continue
		}
		if workspaces != nil {
			if _, ok := workspaces[tab.WorkspaceID]; !ok {
				continue
			}
		}
		out = append(out, tab)
	}
	return out
}

// duplicateClaims reports any item with more than one active claim row
// (claimed, started, or handed_off) in this source.
func duplicateClaims(in Input) ([]Finding, error) {
	activeByID := make(map[string]int)
	for _, claim := range in.Claims {
		if claim.Source != in.SourceKey {
			continue
		}
		if err := knownClaimState(claim); err != nil {
			return nil, err
		}
		if claim.State.Active() {
			activeByID[claim.ID]++
		}
	}
	var ids []string
	for id, count := range activeByID {
		if count > 1 {
			ids = append(ids, id)
		}
	}
	slices.Sort(ids)
	var out []Finding
	for _, id := range ids {
		out = append(out, finding(
			CheckDuplicateClaim,
			SeverityWarn,
			fmt.Sprintf("%s has %d active claim rows", id, activeByID[id]),
			fmt.Sprintf("factory release %s or close the extra claim", id),
			id,
		))
	}
	return out, nil
}

// closableParents reports each open parent item whose known children (across
// the mirror and finished history for this source) are all done.
func closableParents(in Input) ([]Finding, error) {
	items := make([]source.Item, 0, len(in.Snapshot.Items))
	for _, id := range sortedIDs(in.Snapshot.Items) {
		items = append(items, in.Snapshot.Items[id])
	}
	var finished []store.Finished
	for i := range in.Finished {
		if in.Finished[i].Source == in.SourceKey {
			finished = append(finished, in.Finished[i])
		}
	}
	results, err := parentclose.Find(items, finished)
	if err != nil {
		return nil, fmt.Errorf("doctor: closable-parent: %w", err)
	}
	out := make([]Finding, 0, len(results))
	for _, res := range results {
		out = append(out, finding(
			CheckClosableParent,
			SeverityInfo,
			fmt.Sprintf("%s has all children done (%s)", res.ID, strings.Join(res.DoneChildren, ", ")),
			fmt.Sprintf("factory close %s --apply", res.ID),
			res.ID,
		))
	}
	return out, nil
}
