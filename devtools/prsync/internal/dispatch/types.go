package dispatch

// Action values in a dispatch result.
const (
	ActionWouldDispatch     = "would_dispatch"
	ActionDispatched        = "dispatched"
	ActionDispatchedTimeout = "dispatched_timeout"
	ActionDispatchedBlocked = "dispatched_blocked"
	// ActionDispatchedUnresolved means the same unaddressed comments were
	// already sent and the dedupe window still holds. It does not send again.
	ActionDispatchedUnresolved = "dispatched_unresolved"
	ActionSkippedNoTab         = "skipped_no_tab"
	// ActionSkippedTabUnknown means the PR's tab may be on a herdr machine
	// the scan could not reach. It is not sent this run.
	ActionSkippedTabUnknown = "skipped_tab_unknown"
	// ActionSkippedOutOfScope means the PR belongs to another
	// tab_machine run. Detail says why, such as "tab on local".
	ActionSkippedOutOfScope = "skipped_out_of_scope"
	ActionSkippedNoAgent    = "skipped_no_agent"
	ActionSkippedBusy       = "skipped_busy"
	ActionSkippedDeduped    = "skipped_deduped"
	ActionSkippedDraft      = "skipped_draft"
	ActionSkippedStalled    = "skipped_stalled"
	ActionSkippedAddressed  = "skipped_addressed"
	ActionSkippedNotFound   = "skipped_not_found"
	ActionQueued            = "queued"
	ActionGateTimeout       = "gate_timeout"
	// ActionGateUnsafe means the dispatch checkout is dirty. It does not send.
	ActionGateUnsafe = "gate_unsafe"
	ActionFailed     = "failed"
	// ActionWouldOpenTab is the dry run of opening a tab for an untabbed
	// PR (open_missing_tabs) and dispatching to it.
	ActionWouldOpenTab = "would_open_tab"
	// ActionOpenedTabAndDispatched means prsync opened the PR's tab,
	// started its agent, and dispatched to it.
	ActionOpenedTabAndDispatched = "opened_tab_and_dispatched"
	// ActionOpenTabFailed means opening the tab or starting its agent
	// failed. A tab prsync created is closed again. Detail says why.
	ActionOpenTabFailed = "open_tab_failed"
	// ActionSkippedOpenTabCap means open_tabs_max_per_round tabs were
	// already opened this run.
	ActionSkippedOpenTabCap = "skipped_open_tab_cap"
	// ActionSkippedWaiveDisabled means waive re-posting is turned off or
	// lacks waive_comment_prefix / waive_checks.
	ActionSkippedWaiveDisabled = "skipped_waive_disabled"
	// ActionSkippedNoStaleWaive means the PR has no stale waive to re-post.
	ActionSkippedNoStaleWaive = "skipped_no_stale_waive"
	// ActionSkippedAuthorChanges means the stale waive was followed by author
	// changes, so a human must decide instead of re-posting automatically.
	ActionSkippedAuthorChanges = "skipped_author_changes"
	// ActionSkippedNoFailingWaivedChecks means no check in waive_checks is
	// currently failing on the new head.
	ActionSkippedNoFailingWaivedChecks = "skipped_no_failing_waived_checks"
	// ActionSkippedWaiveCap means waive_max_per_pr re-posts were already made.
	ActionSkippedWaiveCap = "skipped_waive_cap"
)

// Kind values in a dispatch result: which prompt the PR gets.
const (
	KindComment       = "comment"
	KindRebase        = "rebase"
	KindMerge         = "merge"
	KindCIFix         = "ci-fix"
	KindMergeQueueFix = "merge-queue-fix"
)

// Document is the outbound dispatch JSON document.
type Document struct {
	GeneratedAt string      `json:"generated_at"` //nolint:tagliatelle // brief outbound contract
	DryRun      bool        `json:"dry_run"`      //nolint:tagliatelle // brief outbound contract
	Results     []Item      `json:"results"`
	Summary     *RunSummary `json:"summary,omitempty"`
}

// RunSummary summarizes a parallel live dispatch run.
type RunSummary struct {
	MaxInFlight int `json:"max_in_flight"` //nolint:tagliatelle // brief outbound contract
	InFlight    int `json:"in_flight"`     //nolint:tagliatelle // brief outbound contract
	Finished    int `json:"finished"`
	TimedOut    int `json:"timed_out"` //nolint:tagliatelle // brief outbound contract
	Blocked     int `json:"blocked"`
	Failed      int `json:"failed,omitempty"`
	Queued      int `json:"queued,omitempty"`
	Skipped     int `json:"skipped,omitempty"`
}

// Item is one PR's dispatch outcome.
type Item struct {
	Repo   string `json:"repo"`
	Number int    `json:"number"`
	Action string `json:"action"`
	// Kind is the prompt kind: comment, rebase, merge, ci-fix, or
	// merge-queue-fix. Every dispatch result sets it; comment results omit it.
	Kind   string `json:"kind,omitempty"`
	PaneID string `json:"pane_id,omitempty"` //nolint:tagliatelle // brief outbound contract
	// Machine is the saved herdr machine of the pane. Omitted for the
	// local server.
	Machine string `json:"machine,omitempty"`
	// TabID is the tab prsync opened for the PR (open_missing_tabs).
	TabID          string `json:"tab_id,omitempty"`          //nolint:tagliatelle // brief outbound contract
	RenderedPrompt string `json:"rendered_prompt,omitempty"` //nolint:tagliatelle // brief outbound contract
	Detail         string `json:"detail,omitempty"`
	// Woke is set when the pane's agent was hibernated and agentbus
	// woke it before the prompt.
	Woke bool `json:"woke,omitempty"`
}
