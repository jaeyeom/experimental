// Code generated from Pkl module `gh_nudge.Config`. DO NOT EDIT.
package pkl

// One gh-approve "mark viewed" rule. A changed file is marked when the
// repos filter matches and any selector matches: a paths_match glob, or
// codeowners_not_mine when the file's owners include neither the user
// nor a team the user belongs to. paths_none_match keeps a match visible
// for this rule. Another rule can still mark it.
type ViewedRule struct {
	// Rule name. It is shown in --explain and written to the audit log.
	Name string `pkl:"name"`

	// Repositories as owner/name or globs such as "acme/*". Empty matches every repo.
	Repos []string `pkl:"repos"`

	// Globs. A changed file matching any of them is marked viewed.
	PathsMatch []string `pkl:"paths_match"`

	// Globs. A changed file matching any of them is not marked by this rule,
	// even when a selector matches. Another rule can still mark it.
	PathsNoneMatch []string `pkl:"paths_none_match"`

	// Alias of paths_none_match. Setting both on one rule is a config error.
	ExceptPaths []string `pkl:"except_paths"`

	// When true, mark a file whose CODEOWNERS owners include neither the
	// user nor a team the user belongs to. Unreadable CODEOWNERS or team
	// data marks nothing for that pull request.
	CodeownersNotMine bool `pkl:"codeowners_not_mine"`
}
