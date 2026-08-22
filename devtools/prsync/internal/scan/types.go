package scan

// Document is the outbound scan JSON document.
type Document struct {
	GeneratedAt       string   `json:"generated_at"` //nolint:tagliatelle // brief outbound contract
	Author            string   `json:"author"`
	Repos             []string `json:"repos"`
	PRs               []PR     `json:"prs"`                //nolint:tagliatelle // brief outbound contract
	InaccessibleRepos []string `json:"inaccessible_repos"` //nolint:tagliatelle // brief outbound contract
	Warnings          []string `json:"warnings"`
}

// PR is one classified pull request in a scan document.
type PR struct {
	Repo             string    `json:"repo"`
	Number           int       `json:"number"`
	Title            string    `json:"title"`
	URL              string    `json:"url"`
	Identifier       *string   `json:"identifier"`
	Base             string    `json:"base"`
	Head             string    `json:"head"`
	HeadSHA          string    `json:"head_sha"`           //nolint:tagliatelle // brief outbound contract
	MergeStateStatus string    `json:"merge_state_status"` //nolint:tagliatelle // brief outbound contract
	IsDraft          bool      `json:"is_draft"`           //nolint:tagliatelle // brief outbound contract
	ReviewDecision   string    `json:"review_decision"`    //nolint:tagliatelle // brief outbound contract
	ReviewRequests   []string  `json:"review_requests"`    //nolint:tagliatelle // brief outbound contract
	CIState          string    `json:"ci_state"`           //nolint:tagliatelle // brief outbound contract
	Bucket           string    `json:"bucket"`
	Unaddressed      bool      `json:"unaddressed"`
	BlockingComments []Comment `json:"blocking_comments"` //nolint:tagliatelle // brief outbound contract
	Tab              *Tab      `json:"tab"`
}

// Comment is one unresolved review thread whose last author is not the PR author.
type Comment struct {
	ThreadID  string `json:"thread_id"`  //nolint:tagliatelle // brief outbound contract
	CommentID string `json:"comment_id"` //nolint:tagliatelle // brief outbound contract
	Author    string `json:"author"`
	Path      string `json:"path"`
	Line      *int   `json:"line"`
	URL       string `json:"url"`
	Body      string `json:"body"`
}

// Tab is a uniquely matched herdr tab. Nil on the PR means no unique tab.
type Tab struct {
	TabID       string  `json:"tab_id"`       //nolint:tagliatelle // brief outbound contract
	PaneID      *string `json:"pane_id"`      //nolint:tagliatelle // brief outbound contract
	WorkspaceID string  `json:"workspace_id"` //nolint:tagliatelle // brief outbound contract
	Label       string  `json:"label"`
	AgentStatus string  `json:"agent_status"` //nolint:tagliatelle // brief outbound contract
}
