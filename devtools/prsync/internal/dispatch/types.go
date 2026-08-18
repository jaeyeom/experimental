package dispatch

// Action values in a dispatch result.
const (
	ActionWouldDispatch     = "would_dispatch"
	ActionDispatched        = "dispatched"
	ActionDispatchedTimeout = "dispatched_timeout"
	ActionDispatchedBlocked = "dispatched_blocked"
	ActionSkippedNoTab      = "skipped_no_tab"
	ActionSkippedNoAgent    = "skipped_no_agent"
	ActionSkippedBusy       = "skipped_busy"
	ActionSkippedDeduped    = "skipped_deduped"
	ActionSkippedDraft      = "skipped_draft"
	ActionSkippedStalled    = "skipped_stalled"
	ActionSkippedAddressed  = "skipped_addressed"
	ActionSkippedNotFound   = "skipped_not_found"
	ActionQueued            = "queued"
	ActionFailed            = "failed"
)

// Document is the outbound dispatch JSON document.
type Document struct {
	GeneratedAt string `json:"generated_at"` //nolint:tagliatelle // brief outbound contract
	DryRun      bool   `json:"dry_run"`      //nolint:tagliatelle // brief outbound contract
	Results     []Item `json:"results"`
}

// Item is one PR's dispatch outcome.
type Item struct {
	Repo           string `json:"repo"`
	Number         int    `json:"number"`
	Action         string `json:"action"`
	PaneID         string `json:"pane_id,omitempty"`         //nolint:tagliatelle // brief outbound contract
	RenderedPrompt string `json:"rendered_prompt,omitempty"` //nolint:tagliatelle // brief outbound contract
	Detail         string `json:"detail,omitempty"`
}
