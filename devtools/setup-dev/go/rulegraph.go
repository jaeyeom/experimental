package main

import (
	"errors"
	"log/slog"
)

type Rule struct {
	Name       string
	ReferTo    string
	Checked    bool
	Check      func() bool
	Build      func() error
	Deps       []*Rule
	PkgManager PackageManager
}

var ErrRuleNotFound = errors.New("rule not found")

type RuleGraph struct {
	Rules map[string]*Rule
}

func (g *RuleGraph) Ensure(rule *Rule) error {
	r := rule
	for r.ReferTo != "" {
		slog.Info("Referring to", "rule", r.ReferTo)
		rr, ok := g.Rules[r.ReferTo]
		if !ok {
			return ErrRuleNotFound
		}
		r = rr
	}
	if r.Checked {
		slog.Info("Already checked", "rule", r.Name)
		return nil
	}
	defer func() {
		r.Checked = true
	}()
	slog.Info("Ensuring", "rule", r.Name)
	if r.Check != nil && r.Check() {
		slog.Info("Already exists", "rule", r.Name)
		return nil
	}
	for _, dep := range r.Deps {
		if err := g.Ensure(dep); err != nil {
			return err
		}
	}
	if r.Build != nil {
		slog.Info("Building", "rule", r.Name)
		err := r.Build()
		slog.Info("Built", "rule", r.Name, "error", err)
		return err
	}
	slog.Info("No build function", "rule", r.Name)
	return nil
}

// Ref returns a reference to a rule.
func Ref(name string) *Rule {
	return &Rule{ReferTo: name}
}

type RuleOpts func(rule *Rule)

func ApplyOpts(r *Rule, opts []RuleOpts) *Rule {
	for _, opt := range opts {
		opt(r)
	}
	return r
}

// Check returns a RuleOpts that sets the check function of a rule.
func Check(check func() bool) RuleOpts {
	return func(rule *Rule) {
		rule.Check = check
	}
}

// Build returns a RuleOpts that sets the build function of a rule.
func Dep(dep *Rule) RuleOpts {
	return func(rule *Rule) {
		rule.Deps = append(rule.Deps, dep)
	}
}
