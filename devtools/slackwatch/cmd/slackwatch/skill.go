package main

import (
	_ "embed"
	"fmt"
	"io"

	// Renders a Markdown skill, not HTML.
	// nosemgrep: import-text-template
	"text/template"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

//go:embed skill.md.tmpl
var skillTemplateText string

// skillData is what the skill template renders from config.
type skillData struct {
	Outbox   string
	PostMode string
}

// cmdSkill prints the agent skill. It reads only the optional config file.
func cmdSkill(configPath string, args []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintln(stderr, "slackwatch: skill does not take arguments")
		return 2
	}
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	tmpl, err := template.New("skill").Parse(skillTemplateText)
	if err != nil {
		return fail(stderr, fmt.Errorf("parse skill template: %w", err))
	}
	if err := tmpl.Execute(stdout, skillData{Outbox: cfg.Outbox, PostMode: cfg.Post.Mode}); err != nil {
		return fail(stderr, fmt.Errorf("render skill: %w", err))
	}
	return 0
}
