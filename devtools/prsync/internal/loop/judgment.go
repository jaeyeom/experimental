package loop

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/policy"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// Judgment reason constants emitted in RoundResult.NeedsJudgment.
const (
	ReasonCIFailingUnclassified = "ci_failing_unclassified"
	ReasonNoTab                 = "no_tab"
	ReasonAmbiguousTab          = "ambiguous_tab"
	ReasonTabBlocked            = "tab_blocked"
	ReasonLeaseHolderBlocked    = "lease_holder_blocked"
	ReasonMergeReady            = "merge_ready"
	ReasonRereviewDue           = "rereview_due"
)

// JudgmentItem identifies one (PR, reason) pair that rules could not decide
// automatically and requires a judging agent or human decision.
type JudgmentItem struct {
	Repo    string `json:"repo"`
	PR      int    `json:"pr"`
	HeadSHA string `json:"head_sha"` //nolint:tagliatelle // outbound contract
	Reason  string `json:"reason"`
	Minutes *int   `json:"minutes,omitempty"`
	Detail  string `json:"detail,omitempty"`
}

// JudgmentInput supplies the round context used to derive NeedsJudgment.
type JudgmentInput struct {
	Config          config.Config
	Now             time.Time
	Scan            scan.Document
	Dispatch        dispatch.Document
	CIFixInbox      []dispatch.Item
	BotRereview     []policy.Decision
	RereviewPreview []policy.Decision
	InboxRequests   []dispatch.InboxRequest
	InboxDir        string
	Ledger          *ledger.Store
	BlockedSince    map[string]time.Time
}

// PopulateNeedsJudgment computes NeedsJudgment from res and in, writes the
// normalized list back to res.NeedsJudgment, and sets res.NeedsJudgmentCount.
func PopulateNeedsJudgment(ctx context.Context, res *RoundResult, in JudgmentInput) {
	if res == nil {
		return
	}
	if len(in.Scan.PRs) == 0 && len(res.Scan.PRs) > 0 {
		in.Scan = res.Scan
	}
	if len(in.Dispatch.Results) == 0 && len(res.Dispatch.Results) > 0 {
		in.Dispatch = res.Dispatch
	}
	if len(in.CIFixInbox) == 0 && len(res.CIFixInbox) > 0 {
		in.CIFixInbox = res.CIFixInbox
	}
	if len(in.BotRereview) == 0 && len(res.BotRereview) > 0 {
		in.BotRereview = res.BotRereview
	}
	if len(in.RereviewPreview) == 0 && len(res.RereviewPreview) > 0 {
		in.RereviewPreview = res.RereviewPreview
	}
	if in.Now.IsZero() {
		if !res.EndedAt.IsZero() {
			in.Now = res.EndedAt
		} else {
			in.Now = time.Now().UTC()
		}
	}
	items := EvaluateNeedsJudgment(ctx, in)
	res.NeedsJudgment = items
	res.NeedsJudgmentCount = len(items)
}

// EvaluateNeedsJudgment returns deterministic, deduplicated per-(PR, reason)
// judgment items for the round.
func EvaluateNeedsJudgment(ctx context.Context, in JudgmentInput) []JudgmentItem {
	now := in.Now
	if now.IsZero() {
		now = time.Now().UTC()
	}

	inboxIndex := loadInboxHeadIndex(ctx, in)
	lgr := resolveJudgmentLedger(in)

	c := judgmentCollector{
		seen: make(map[judgmentKey]struct{}),
		out:  []JudgmentItem{},
	}

	byPR := make(map[string]scan.PR, len(in.Scan.PRs))
	for _, pr := range in.Scan.PRs {
		byPR[fmt.Sprintf("%s#%d", pr.Repo, pr.Number)] = pr
		if !isOpenInScopePR(pr) {
			continue
		}
		evaluateScanPRJudgments(ctx, pr, in, inboxIndex, lgr, now, &c)
	}

	evaluateLeaseHolderBlocked(in.Dispatch.Results, byPR, &c)
	evaluateLeaseHolderBlocked(in.CIFixInbox, byPR, &c)
	evaluateRereviewDue(in, byPR, &c)

	sort.Slice(c.out, func(i, j int) bool {
		if c.out[i].Repo != c.out[j].Repo {
			return c.out[i].Repo < c.out[j].Repo
		}
		if c.out[i].PR != c.out[j].PR {
			return c.out[i].PR < c.out[j].PR
		}
		return c.out[i].Reason < c.out[j].Reason
	})
	return c.out
}

func evaluateScanPRJudgments(
	ctx context.Context,
	pr scan.PR,
	in JudgmentInput,
	inboxIndex map[inboxHeadKey]bool,
	lgr *ledger.Store,
	now time.Time,
	c *judgmentCollector,
) {
	if pr.CIState == "failing" && !hasCIFixClassification(ctx, pr, inboxIndex, lgr) {
		c.add(JudgmentItem{
			Repo:    pr.Repo,
			PR:      pr.Number,
			HeadSHA: pr.HeadSHA,
			Reason:  ReasonCIFailingUnclassified,
			Detail:  "failing checks with no ci_fix_inbox hint or ci-fix ledger dispatch for head",
		})
	}

	if pr.Tab == nil {
		if detail, amb := ambiguousTabMatch(pr, in.Scan.Warnings); amb {
			c.add(JudgmentItem{
				Repo:    pr.Repo,
				PR:      pr.Number,
				HeadSHA: pr.HeadSHA,
				Reason:  ReasonAmbiguousTab,
				Detail:  detail,
			})
		} else if pr.TabUnknown == "" && !openedTabInRound(pr, in.Dispatch.Results, in.CIFixInbox) {
			c.add(JudgmentItem{
				Repo:    pr.Repo,
				PR:      pr.Number,
				HeadSHA: pr.HeadSHA,
				Reason:  ReasonNoTab,
			})
		}
	} else if isTabBlocked(pr, in.Dispatch.Results, in.CIFixInbox) {
		mins := blockedMinutesFromMap(in.BlockedSince, pr, now)
		c.add(JudgmentItem{
			Repo:    pr.Repo,
			PR:      pr.Number,
			HeadSHA: pr.HeadSHA,
			Reason:  ReasonTabBlocked,
			Minutes: &mins,
			Detail:  fmt.Sprintf("tab %s blocked (%dm)", blockedTabKey(pr), mins),
		})
	}

	if isMergeReady(pr) {
		c.add(JudgmentItem{
			Repo:    pr.Repo,
			PR:      pr.Number,
			HeadSHA: pr.HeadSHA,
			Reason:  ReasonMergeReady,
		})
	}
}

func evaluateLeaseHolderBlocked(items []dispatch.Item, byPR map[string]scan.PR, c *judgmentCollector) {
	for _, it := range items {
		if !isLeaseHolderBlockedItem(it) {
			continue
		}
		key := fmt.Sprintf("%s#%d", it.Repo, it.Number)
		var headSHA string
		if pr, ok := byPR[key]; ok {
			if !isOpenInScopePR(pr) {
				continue
			}
			headSHA = pr.HeadSHA
		}
		c.add(JudgmentItem{
			Repo:    it.Repo,
			PR:      it.Number,
			HeadSHA: headSHA,
			Reason:  ReasonLeaseHolderBlocked,
			Detail:  it.Detail,
		})
	}
}

func evaluateRereviewDue(in JudgmentInput, byPR map[string]scan.PR, c *judgmentCollector) {
	if in.Config.Policy.BotRereview != config.PolicyOff {
		return
	}
	decisions := in.RereviewPreview
	if len(decisions) == 0 {
		decisions = in.BotRereview
	}
	for _, d := range decisions {
		if d.Kind != policy.KindBotRereview || d.Decision != policy.DecisionDryRun {
			continue
		}
		key := fmt.Sprintf("%s#%d", d.Repo, d.Number)
		headSHA := d.HeadSHA
		if pr, ok := byPR[key]; ok {
			if !isOpenInScopePR(pr) {
				continue
			}
			if headSHA == "" {
				headSHA = pr.HeadSHA
			}
		}
		c.add(JudgmentItem{
			Repo:    d.Repo,
			PR:      d.Number,
			HeadSHA: headSHA,
			Reason:  ReasonRereviewDue,
		})
	}
}

type judgmentKey struct {
	repo   string
	pr     int
	reason string
}

type judgmentCollector struct {
	seen map[judgmentKey]struct{}
	out  []JudgmentItem
}

func (c *judgmentCollector) add(item JudgmentItem) {
	k := judgmentKey{repo: item.Repo, pr: item.PR, reason: item.Reason}
	if _, ok := c.seen[k]; ok {
		return
	}
	c.seen[k] = struct{}{}
	c.out = append(c.out, item)
}

type inboxHeadKey struct {
	repo    string
	pr      int
	headSHA string
}

func loadInboxHeadIndex(ctx context.Context, in JudgmentInput) map[inboxHeadKey]bool {
	index := make(map[inboxHeadKey]bool)
	for _, req := range in.InboxRequests {
		addInboxHeadKey(index, req.Repo, req.PR, req.HeadSHA)
	}
	inboxDir := resolveInboxDir(in)
	if inboxDir == "" {
		return index
	}
	addPendingInboxHeads(ctx, inboxDir, index)
	addDoneInboxHeads(ctx, inboxDir, index)
	return index
}

func resolveInboxDir(in JudgmentInput) string {
	if in.InboxDir != "" {
		return in.InboxDir
	}
	if in.Config.CIFixInbox != "" || in.Config.StateFile != "" {
		return in.Config.CIFixInboxDir()
	}
	return ""
}

func addPendingInboxHeads(ctx context.Context, inboxDir string, index map[inboxHeadKey]bool) {
	pending, err := dispatch.ReadCIFixInbox(inboxDir)
	if err != nil {
		runlog.FromContext(ctx).Warn("needs_judgment_read_inbox", "dir", inboxDir, "error", err.Error())
		return
	}
	for _, entry := range pending {
		if entry.Err != nil {
			continue
		}
		addInboxHeadKey(index, entry.Request.Repo, entry.Request.PR, entry.Request.HeadSHA)
	}
}

func addDoneInboxHeads(ctx context.Context, inboxDir string, index map[inboxHeadKey]bool) {
	done, err := dispatch.ReadDoneCIFixInbox(inboxDir)
	if err != nil {
		runlog.FromContext(ctx).Warn("needs_judgment_read_inbox_done", "dir", inboxDir, "error", err.Error())
		return
	}
	for _, rec := range done {
		addInboxHeadKey(index, rec.Repo, rec.PR, rec.HeadSHA)
	}
}

func addInboxHeadKey(index map[inboxHeadKey]bool, repo string, pr int, headSHA string) {
	if repo != "" && pr > 0 && headSHA != "" {
		index[inboxHeadKey{repo: repo, pr: pr, headSHA: headSHA}] = true
	}
}

func resolveJudgmentLedger(in JudgmentInput) *ledger.Store {
	if in.Ledger != nil {
		return in.Ledger
	}
	if in.Config.Policy.LedgerFileConfigured || in.Config.StateFile != "" {
		if path := in.Config.ResolvedLedgerFile(); path != "" {
			return &ledger.Store{Path: path}
		}
	}
	return nil
}

func hasCIFixClassification(ctx context.Context, pr scan.PR, inboxIndex map[inboxHeadKey]bool, lgr *ledger.Store) bool {
	if pr.HeadSHA != "" && inboxIndex[inboxHeadKey{repo: pr.Repo, pr: pr.Number, headSHA: pr.HeadSHA}] {
		return true
	}
	if lgr == nil || pr.HeadSHA == "" {
		return false
	}
	has, err := lgr.HasDispatched(fmt.Sprintf("%s#%d", pr.Repo, pr.Number), pr.HeadSHA, dispatch.KindCIFix)
	if err != nil {
		runlog.FromContext(ctx).Warn("needs_judgment_ledger", "pr", fmt.Sprintf("%s#%d", pr.Repo, pr.Number), "error", err.Error())
		return false
	}
	return has
}

func isOpenInScopePR(pr scan.PR) bool {
	if pr.State != "" && pr.State != "open" {
		return false
	}
	return pr.OutOfScope == ""
}

func isMergeReady(pr scan.PR) bool {
	return !pr.IsDraft &&
		!pr.Unaddressed &&
		len(pr.ReviewRequests) == 0 &&
		pr.Mergeable == "MERGEABLE" &&
		pr.MergeStateStatus != "DIRTY" &&
		pr.ReviewDecision == "APPROVED" &&
		pr.CIState == "green"
}

func ambiguousTabMatch(pr scan.PR, warnings []string) (string, bool) {
	if pr.Identifier != nil && *pr.Identifier != "" {
		prefix := *pr.Identifier + ": ambiguous herdr tab label "
		for _, w := range warnings {
			if strings.HasPrefix(w, prefix) {
				return w, true
			}
		}
	}
	if pr.TabAmbiguous {
		return "multiple herdr tabs matched", true
	}
	return "", false
}

func openedTabInRound(pr scan.PR, dispatchItems, inboxItems []dispatch.Item) bool {
	return hasPRItemAction(dispatchItems, pr.Repo, pr.Number, dispatch.ActionOpenedTabAndDispatched) ||
		hasPRItemAction(inboxItems, pr.Repo, pr.Number, dispatch.ActionOpenedTabAndDispatched)
}

func hasPRItemAction(items []dispatch.Item, repo string, number int, action string) bool {
	for _, it := range items {
		if it.Repo == repo && it.Number == number && it.Action == action {
			return true
		}
	}
	return false
}

func isLeaseHolderBlockedItem(it dispatch.Item) bool {
	if it.Action != dispatch.ActionGateTimeout && it.Action != dispatch.ActionWouldDispatch {
		return false
	}
	if isNonHolderLeaseDetail(it.Detail) {
		return false
	}
	if it.GateSignal == dispatch.SignalLease {
		return true
	}
	return strings.Contains(it.Detail, "— "+dispatch.SignalLease+" ") ||
		strings.Contains(it.Detail, "("+dispatch.SignalLease+" ")
}

func isNonHolderLeaseDetail(detail string) bool {
	return strings.Contains(detail, "cannot read the lease") ||
		strings.Contains(detail, "not guarded") ||
		strings.Contains(detail, "requires gate_lease")
}
