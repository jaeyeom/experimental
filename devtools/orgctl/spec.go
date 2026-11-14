// Package orgctl reads an organization spec rendered from Pkl as JSON.
// The package does not parse Pkl.
package orgctl

import (
	"encoding/json"
	"fmt"
)

// Spec is one organization. Field names match `pkl eval -f json`.
type Spec struct {
	Org                string             `json:"org"`
	Machines           map[string]Machine `json:"machines"`
	Rules              []Rule             `json:"rules"`
	Roles              map[string]Role    `json:"roles"`
	Pools              map[string]Pool    `json:"pools"`
	ApprovalWords      []string           `json:"approvalWords"`
	AllowApprovalWords []string           `json:"allowApprovalWords"`
}

// Machine is one host the organization runs on.
type Machine struct {
	Host        string `json:"host"`
	HerdrHandle string `json:"herdrHandle"`
	Timezone    string `json:"timezone"`
}

// Rule is one org-wide rule.
type Rule struct {
	ID     string `json:"id"`
	Text   string `json:"text"`
	Source string `json:"source"`
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
}

// Pool is a worker pool. It has no schedules.
type Pool struct {
	Machine   string `json:"machine"`
	Workspace string `json:"workspace"`
	Starter   string `json:"starter"`
	Max       int    `json:"max"`
	Source    string `json:"source"`
}

// Decode reads a spec from JSON produced by `pkl eval -f json`.
func Decode(data []byte) (Spec, error) {
	var spec Spec
	if err := json.Unmarshal(data, &spec); err != nil {
		return Spec{}, fmt.Errorf("decode org spec: %w", err)
	}
	return spec, nil
}
