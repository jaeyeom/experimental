package outbox

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestLintFields(t *testing.T) {
	rules, err := BuildLintRules(LintConfig{
		Defaults:      true,
		NameRuleLabel: "internal name",
		NamePattern:   `\bworker/[a-z]+\b`,
		Patterns:      []string{`SECRET-[0-9]+`},
		Phrases:       []string{"quiet rollout"},
	}, []string{"coord/primary"})
	if err != nil {
		t.Fatal(err)
	}

	fields := []TextField{
		{Location: "body", Text: "clean line\nsee D-42 and coord/primary", SplitLines: true},
		{Location: "item:3", Text: "per the user, SECRET-9 in worker/blue quiet rollout"},
	}
	hits := LintFields(fields, rules)
	if len(hits) != 6 {
		t.Fatalf("hits = %v, want 6", hits)
	}
	if hits[0].Location != "body line 2" || hits[0].Match != "D-42" || hits[0].Rule != "decide id" {
		t.Errorf("hit[0] = %+v", hits[0])
	}
	if hits[1].Match != "coord/primary" || hits[1].Rule != "internal name" {
		t.Errorf("hit[1] = %+v", hits[1])
	}
	errMsg := (&LintError{Hits: hits}).Error()
	if !strings.Contains(errMsg, "body line 2") || !strings.Contains(errMsg, "SECRET-9") {
		t.Errorf("LintError = %q", errMsg)
	}
}

func TestShapeMessagePassesDecideShape(t *testing.T) {
	longText := strings.Repeat("word ", 60)
	longURL := "https://example.com/" + strings.Repeat("segment/", 25)
	msg := ShapeMessage(
		"Approve long subject header: "+strings.Repeat("x", 120),
		ShapeBullet("Destination", "#ops-channel (C123)"),
		ShapeLinkLine(longURL),
		ShapeBullet("Notes", longText),
		ShapeBullet("Notes2", longText),
		ShapeBullet("Notes3", longText),
		ShapeBullet("Notes4", longText),
		ShapeBullet("Notes5", longText),
		ShapeBullet("Notes6", longText),
		ShapeBullet("Notes7", longText),
		ShapeBullet("Notes8", longText),
	)
	if err := decide.CheckShape(msg, decide.Config{}.ShapeLimits()); err != nil {
		t.Fatalf("CheckShape(%q) failed: %v", msg, err)
	}
}
