// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// GitHub-related configuration
type GitHubConfig struct {
	// Repository owner (organization or user). Defaults to authenticated user.
	Owner *string `pkl:"owner"`

	// List of repository names to monitor
	Repos []string `pkl:"repos"`
}
