package main

import (
	"bytes"
	_ "embed"
	"fmt"
	"io"
	"strings"

	// Renders a Markdown skill, not HTML.
	// nosemgrep: import-text-template
	"text/template"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

//go:embed skill.md.tmpl
var skillTemplateText string

//go:embed triage.md.tmpl
var triageTemplateText string

// skillData is what the skill template renders from config.
type skillData struct {
	Outbox        string
	PostMode      string
	DecideOwner   string
	InboundTarget string
}

// triageData is what the triage brief renders from config.
type triageData struct {
	Target string
}

func renderSkillString(cfg slackwatch.Config) (string, error) {
	tmpl, err := template.New("skill").Parse(skillTemplateText)
	if err != nil {
		return "", fmt.Errorf("parse skill template: %w", err)
	}
	data := skillData{Outbox: cfg.Outbox, PostMode: cfg.Post.Mode, DecideOwner: cfg.Post.DecideOwnerOrDefault()}
	if cfg.Inbound != nil {
		data.InboundTarget = cfg.Inbound.Target
	}
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, data); err != nil {
		return "", fmt.Errorf("render skill: %w", err)
	}
	return buf.String(), nil
}

func renderTriage(cfg slackwatch.Config, stdout, stderr io.Writer) int {
	if cfg.Inbound == nil || strings.TrimSpace(cfg.Inbound.Target) == "" {
		fmt.Fprintln(stderr, "slackwatch: inbound is not configured")
		return 1
	}
	tmpl, err := template.New("triage").Parse(triageTemplateText)
	if err != nil {
		return fail(stderr, fmt.Errorf("parse triage template: %w", err))
	}
	if err := tmpl.Execute(stdout, triageData{Target: cfg.Inbound.Target}); err != nil {
		return fail(stderr, fmt.Errorf("render triage skill: %w", err))
	}
	return 0
}
