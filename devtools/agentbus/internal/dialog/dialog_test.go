package dialog

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
)

const testRule = "──────────────────────────────"

func claudeIdleScreen(lines ...string) string {
	return "⏺ Done.\n" + testRule + "\n" + strings.Join(lines, "\n") + "\n" + testRule + "\n  [Opus] repo\n"
}

func TestDetectBuiltinsAndCustomRules(t *testing.T) {
	permission := "⏺ Bash(rm -rf build)\n" + testRule + "\n Bash command\n\n   rm -rf build\n\n Do you want to proceed?\n ❯ 1. Yes\n   2. No, and tell Claude what to do differently (esc)\n"
	question := "☐ Scope\n\nWhich packages?\n\n❯ 1. Only agentbus\n  2. Everything\n\nEnter to select · ↑/↓ to navigate · Esc to cancel\n"
	customScreen := "Repository security check\n\nDo you trust the files in this folder? [y/N]\n"

	rules, err := CompileRules([]config.DialogRule{
		{Name: "trust-folder", Pattern: `Do you trust.*\? \[y/N\]`},
	})
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name     string
		kind     string
		screen   string
		extra    []Rule
		wantOK   bool
		wantRule string
		wantText string
	}{
		{
			name:     "claude permission prompt",
			kind:     "claude",
			screen:   permission,
			extra:    rules,
			wantOK:   true,
			wantRule: "permission prompt",
			wantText: "Do you want to proceed?",
		},
		{
			name:     "claude question prompt",
			kind:     "claude",
			screen:   question,
			extra:    rules,
			wantOK:   true,
			wantRule: "question prompt",
			wantText: "Enter to select · ↑/↓ to navigate · Esc to cancel",
		},
		{
			name:   "claude normal idle screen",
			kind:   "claude",
			screen: "Earlier text: Do you want to proceed? Do you trust?\n" + claudeIdleScreen("❯ "),
			extra:  rules,
			wantOK: false,
		},
		{
			name:   "claude draft screen is not a dialog",
			kind:   "claude",
			screen: claudeIdleScreen("❯ typing something"),
			extra:  rules,
			wantOK: false,
		},
		{
			name:     "configured rule matches non-composer screen",
			kind:     "claude",
			screen:   customScreen,
			extra:    rules,
			wantOK:   true,
			wantRule: "trust-folder",
			wantText: "Do you trust the files in this folder? [y/N]",
		},
		{
			name:     "configured rule matches other agent kind",
			kind:     "codex",
			screen:   customScreen,
			extra:    rules,
			wantOK:   true,
			wantRule: "trust-folder",
			wantText: "Do you trust the files in this folder? [y/N]",
		},
		{
			name:   "idle codex screen no false positive",
			kind:   "codex",
			screen: "> \n",
			extra:  rules,
			wantOK: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, ok := Detect(tt.kind, tt.screen, tt.extra)
			if ok != tt.wantOK {
				t.Fatalf("Detect = %+v, %v; want %v", got, ok, tt.wantOK)
			}
			if !tt.wantOK {
				return
			}
			if got.Rule != tt.wantRule || got.Text != tt.wantText {
				t.Fatalf("Detect = %+v; want rule=%q text=%q", got, tt.wantRule, tt.wantText)
			}
		})
	}
}

func TestDetectTruncatesLongLines(t *testing.T) {
	longLine := "Do you want to " + strings.Repeat("x", 200)
	got, ok := Detect("claude", longLine+"\n", nil)
	if !ok {
		t.Fatal("expected match")
	}
	if !strings.HasSuffix(got.Text, "…") || len([]rune(got.Text)) > maxMatchRunes+1 {
		t.Fatalf("truncated text = %q (runes=%d)", got.Text, len([]rune(got.Text)))
	}
}

func TestCompileRulesRejectsInvalidRegex(t *testing.T) {
	if _, err := CompileRules([]config.DialogRule{{Name: "bad", Pattern: "[unclosed"}}); err == nil {
		t.Fatal("expected invalid regex error")
	}
}
