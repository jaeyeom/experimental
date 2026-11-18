// Package orgctl reads an organization spec rendered from Pkl as JSON.
// The package does not parse Pkl.
package orgctl

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"slices"
)

// Spec is one organization. Field names match `pkl eval -f json`.
type Spec struct {
	Org                string             `json:"org"`
	Machines           map[string]Machine `json:"machines"`
	Rules              []Rule             `json:"rules"`
	Classes            map[string]Class   `json:"classes"`
	Roles              map[string]Role    `json:"roles"`
	Pools              map[string]Pool    `json:"pools"`
	ApprovalWords      []string           `json:"approvalWords"`
	AllowApprovalWords []string           `json:"allowApprovalWords"`
	Scope              *Scope             `json:"scope,omitempty"`
	Jobs               *Jobs              `json:"jobs,omitempty"`
	AgentsMDRoot       string             `json:"agentsMdRoot,omitempty"`
}

// Jobs declares the job runners and where anomaly events go.
// Policy is the path of the user-owned gate policy file. orgctl only
// checks that it exists and never reads it.
type Jobs struct {
	Policy      string            `json:"policy"`
	AnomalyRole string            `json:"anomalyRole"`
	Runners     map[string]Runner `json:"runners"`
}

// Runner is one per-machine jobs runner, keyed in [Jobs.Runners] by the
// name of its unit. See [RunnerUnit].
type Runner struct {
	Machine string   `json:"machine"`
	Kinds   []string `json:"kinds"`
}

// Scope limits tab drift to a set of workspaces.
// An empty Workspaces list reports every workspace.
// A pattern-less pool still hides its whole workspace.
// A patterned pool hides only its member tabs.
type Scope struct {
	Workspaces []string `json:"workspaces"`
}

// Machine is one host the organization runs on.
type Machine struct {
	Host        string `json:"host"`
	HerdrHandle string `json:"herdrHandle"`
	Timezone    string `json:"timezone"`
}

// Rule is one org-wide or class rule.
type Rule struct {
	ID     string `json:"id"`
	Text   string `json:"text"`
	Source string `json:"source"`
}

// Class is one role class, keyed in [Spec.Classes] by its prefix.
type Class struct {
	Description string `json:"description"`
	Rules       []Rule `json:"rules"`
}

// Brief is the note a role reads when it starts.
type Brief struct {
	Skill  string   `json:"skill"`
	File   string   `json:"file"`
	Text   string   `json:"text"`
	Memory []string `json:"memory"`
}

// Schedule is a cron prompt owned by a role.
type Schedule struct {
	ID      string `json:"id"`
	Cron    string `json:"cron"`
	Prompt  string `json:"prompt"`
	Durable bool   `json:"durable"`
}

// Needs lists what a person still has to provide. Names only.
type Needs struct {
	Permissions []string `json:"permissions"`
	Secrets     []string `json:"secrets"`
	Services    []string `json:"services"`
}

// Role is one agent, keyed in [Spec.Roles] by its agentbus name.
// A missing reportsTo means the role is at the top.
type Role struct {
	Machine       string     `json:"machine"`
	Workspace     string     `json:"workspace"`
	Tab           string     `json:"tab"`
	Cwd           string     `json:"cwd"`
	When          string     `json:"when"`
	Agent         string     `json:"agent"`
	Model         string     `json:"model"`
	Effort        string     `json:"effort"`
	Brief         Brief      `json:"brief"`
	Schedules     []Schedule `json:"schedules"`
	ReportsTo     *string    `json:"reportsTo"`
	Decides       []string   `json:"decides"`
	Escalates     []string   `json:"escalates"`
	Owns          []string   `json:"owns"`
	CheckoutLease string     `json:"checkoutLease"`
	Tools         []string   `json:"tools"`
	State         []string   `json:"state"`
	Needs         Needs      `json:"needs"`
	Rules         []string   `json:"rules"`
	Class         string     `json:"class,omitempty"`
}

// Pool is a worker pool. It has no schedules.
// Empty NamePattern and TabPattern exclude every tab in the workspace.
// A set pattern matches only those workers. A role key is never a worker.
type Pool struct {
	Machine     string `json:"machine"`
	Workspace   string `json:"workspace"`
	NamePattern string `json:"namePattern,omitempty"`
	TabPattern  string `json:"tabPattern,omitempty"`
	Starter     string `json:"starter"`
	Max         int    `json:"max"`
	Source      string `json:"source"`
}

// CheckRules rejects duplicate rule IDs across org rules and class rules.
func (s Spec) CheckRules() error {
	seen := make(map[string]string, len(s.Rules))
	for _, rule := range s.Rules {
		if where, dup := seen[rule.ID]; dup {
			return fmt.Errorf("duplicate rule id %q (in %s and org rules)", rule.ID, where)
		}
		seen[rule.ID] = "org rules"
	}
	for _, name := range mapKeys(s.Classes) {
		where := fmt.Sprintf("class %s", name)
		for _, rule := range s.Classes[name].Rules {
			if prev, dup := seen[rule.ID]; dup {
				return fmt.Errorf("duplicate rule id %q (in %s and %s)", rule.ID, prev, where)
			}
			seen[rule.ID] = where
		}
	}
	return nil
}

// CheckClasses rejects a role whose class does not exist in [Spec.Classes].
// Roles without a class are allowed.
func (s Spec) CheckClasses() error {
	for _, name := range mapKeys(s.Roles) {
		className := s.Roles[name].Class
		if className == "" {
			continue
		}
		if _, ok := s.Classes[className]; !ok {
			return fmt.Errorf("role %s: unknown class %q", name, className)
		}
	}
	return nil
}

// CheckPatterns rejects a pool glob that filepath.Match cannot compile.
func (s Spec) CheckPatterns() error {
	names := make([]string, 0, len(s.Pools))
	for name := range s.Pools {
		names = append(names, name)
	}
	slices.Sort(names)
	for _, name := range names {
		pool := s.Pools[name]
		if err := globError(name, "namePattern", pool.NamePattern); err != nil {
			return err
		}
		if err := globError(name, "tabPattern", pool.TabPattern); err != nil {
			return err
		}
	}
	return nil
}

// CheckScope warns when a role or pool workspace is outside a set scope.
// An empty scope is unset and warns about nothing.
func (s Spec) CheckScope() []string {
	if !scopeSet(s.Scope) {
		return nil
	}
	allowed := make(map[string]bool, len(s.Scope.Workspaces))
	for _, workspace := range s.Scope.Workspaces {
		allowed[workspace] = true
	}
	var warnings []string
	for _, name := range mapKeys(s.Roles) {
		if !allowed[s.Roles[name].Workspace] {
			warnings = append(warnings, fmt.Sprintf("warning: role %s workspace %q is outside the scope", name, s.Roles[name].Workspace))
		}
	}
	for _, name := range mapKeys(s.Pools) {
		if !allowed[s.Pools[name].Workspace] {
			warnings = append(warnings, fmt.Sprintf("warning: pool %s workspace %q is outside the scope", name, s.Pools[name].Workspace))
		}
	}
	return warnings
}

func scopeSet(scope *Scope) bool {
	return scope != nil && len(scope.Workspaces) > 0
}

func mapKeys[V any](items map[string]V) []string {
	names := make([]string, 0, len(items))
	for name := range items {
		names = append(names, name)
	}
	slices.Sort(names)
	return names
}

func globError(name, kind, pattern string) error {
	if pattern == "" {
		return nil
	}
	if _, err := filepath.Match(pattern, ""); err != nil {
		return fmt.Errorf("pool %s: %s %q: %w", name, kind, pattern, err)
	}
	return nil
}

// Decode reads a spec from JSON produced by `pkl eval -f json`.
func Decode(data []byte) (Spec, error) {
	var spec Spec
	if err := json.Unmarshal(data, &spec); err != nil {
		return Spec{}, fmt.Errorf("decode org spec: %w", err)
	}
	return spec, nil
}
