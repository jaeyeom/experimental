// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

import "github.com/apple/pkl-go/pkl"

// gh-approve settings. gh-nudge and gh-merge ignore this section.
type ApproveConfig struct {
	// Approval body. The matching rule name is appended as an HTML comment.
	Body string `pkl:"body"`

	// Skip a pull request the user already approved at the current head.
	SkipAlreadyApproved bool `pkl:"skip_already_approved"`

	// How long a cached agent_command decision is reused.
	AgentCacheTtl pkl.Duration `pkl:"agent_cache_ttl"`

	// Size limit of the agent_command decision cache. The oldest entries are
	// evicted first.
	AgentCacheMaxMb int `pkl:"agent_cache_max_mb"`

	// Rules. A pull request that matches none of them is left for a human.
	Rules []ApproveRule `pkl:"rules"`
}
