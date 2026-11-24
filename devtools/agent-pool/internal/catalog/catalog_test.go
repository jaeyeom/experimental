package catalog

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const validReviewPRCatalog = `{
  "version": 1,
  "skills": {
    "review-pr": {
      "description": "Review a pull request for bugs and style issues.",
      "mode": "readonly",
      "skillPath": ".claude/skills/review-pr/SKILL.md",
      "argsSchema": {
        "type": "object",
        "additionalProperties": false,
        "required": ["prNumber"],
        "properties": {
          "prNumber": {
            "type": "integer",
            "minimum": 1,
            "maximum": 999999
          },
          "focus": {
            "type": "string",
            "enum": ["security", "correctness", "style"]
          }
        }
      },
      "outputSchema": {
        "type": "object",
        "additionalProperties": false,
        "required": ["summary", "findings"],
        "properties": {
          "summary": {
            "type": "string",
            "maxLength": 2000
          },
          "findings": {
            "type": "array",
            "maxItems": 50,
            "items": {
              "type": "object",
              "additionalProperties": false,
              "required": ["severity", "message"],
              "properties": {
                "severity": {
                  "type": "string",
                  "enum": ["low", "medium", "high"]
                },
                "message": {
                  "type": "string"
                }
              }
            }
          }
        }
      }
    }
  }
}`

func TestParseValidCatalog(t *testing.T) {
	cat, err := Parse([]byte(validReviewPRCatalog))
	if err != nil {
		t.Fatalf("Parse() unexpected error: %v", err)
	}
	if cat.Version != SupportedVersion {
		t.Errorf("Version = %d, want %d", cat.Version, SupportedVersion)
	}
	skill, ok := cat.Skills["review-pr"]
	if !ok {
		t.Fatalf("expected skill 'review-pr' in catalog")
	}
	if skill.Mode != ModeReadOnly {
		t.Errorf("Mode = %q, want %q", skill.Mode, ModeReadOnly)
	}
	if skill.SkillPath != ".claude/skills/review-pr/SKILL.md" {
		t.Errorf("SkillPath = %q, want %q", skill.SkillPath, ".claude/skills/review-pr/SKILL.md")
	}
	if skill.Limits.MaxTurns != DefaultMaxTurns {
		t.Errorf("MaxTurns = %d, want %d", skill.Limits.MaxTurns, DefaultMaxTurns)
	}
	if skill.Limits.MaxEffectiveTokens != DefaultMaxEffectiveTokens {
		t.Errorf("MaxEffectiveTokens = %d, want %d", skill.Limits.MaxEffectiveTokens, DefaultMaxEffectiveTokens)
	}
	if skill.Limits.TimeoutSeconds != DefaultTimeoutSeconds {
		t.Errorf("TimeoutSeconds = %d, want %d", skill.Limits.TimeoutSeconds, DefaultTimeoutSeconds)
	}
	if skill.Limits.MaxDeliverableBytes != DefaultMaxDeliverableBytes {
		t.Errorf("MaxDeliverableBytes = %d, want %d", skill.Limits.MaxDeliverableBytes, DefaultMaxDeliverableBytes)
	}
}

func TestParseLimitsValidation(t *testing.T) {
	tests := []struct {
		name    string
		limits  string
		wantErr bool
	}{
		{
			name:   "valid custom limits",
			limits: `{"maxTurns": 50, "maxEffectiveTokens": 200000, "timeoutSeconds": 600, "maxDeliverableBytes": 65536}`,
		},
		{
			name:    "negative maxTurns",
			limits:  `{"maxTurns": -1}`,
			wantErr: true,
		},
		{
			name:    "maxTurns above ceiling",
			limits:  `{"maxTurns": 101}`,
			wantErr: true,
		},
		{
			name:    "negative maxEffectiveTokens",
			limits:  `{"maxEffectiveTokens": -10}`,
			wantErr: true,
		},
		{
			name:    "timeoutSeconds above ceiling",
			limits:  `{"timeoutSeconds": 1801}`,
			wantErr: true,
		},
		{
			name:    "maxDeliverableBytes above ceiling",
			limits:  `{"maxDeliverableBytes": 1048577}`,
			wantErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			raw := `{
				"version": 1,
				"skills": {
					"review-pr": {
						"description": "Review PR",
						"mode": "readonly",
						"skillPath": ".agent-pool/skills/review-pr/SKILL.md",
						"argsSchema": {"type": "object", "additionalProperties": false, "properties": {}},
						"outputSchema": {"type": "object", "properties": {}},
						"limits": ` + tc.limits + `
					}
				}
			}`
			_, err := Parse([]byte(raw))
			if (err != nil) != tc.wantErr {
				t.Fatalf("Parse() error = %v, wantErr %v", err, tc.wantErr)
			}
		})
	}
}

func TestParseRejectsUnknownVersionAndFields(t *testing.T) {
	tests := []struct {
		name string
		raw  string
	}{
		{
			name: "unsupported version 2",
			raw:  strings.Replace(validReviewPRCatalog, `"version": 1`, `"version": 2`, 1),
		},
		{
			name: "unknown top-level field",
			raw:  strings.Replace(validReviewPRCatalog, `"version": 1,`, `"version": 1, "extraField": true,`, 1),
		},
		{
			name: "unknown skill field",
			raw:  strings.Replace(validReviewPRCatalog, `"mode": "readonly",`, `"mode": "readonly", "unknownKey": 123,`, 1),
		},
		{
			name: "invalid skill name uppercase",
			raw:  strings.Replace(validReviewPRCatalog, `"review-pr":`, `"ReviewPR":`, 1),
		},
		{
			name: "invalid skill name leading hyphen",
			raw:  strings.Replace(validReviewPRCatalog, `"review-pr":`, `"-review-pr":`, 1),
		},
		{
			name: "invalid skill name too long",
			raw:  strings.Replace(validReviewPRCatalog, `"review-pr":`, `"`+strings.Repeat("a", 65)+`":`, 1),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if _, err := Parse([]byte(tc.raw)); err == nil {
				t.Fatalf("Parse() succeeded, want error for %s", tc.name)
			}
		})
	}
}

func TestParseRejectsNonReadOnlyMode(t *testing.T) {
	tests := []struct {
		name string
		mode string
	}{
		{name: "mutating mode", mode: `"mutating"`},
		{name: "empty mode", mode: `""`},
		{name: "arbitrary mode", mode: `"write"`},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			raw := strings.Replace(validReviewPRCatalog, `"mode": "readonly"`, `"mode": `+tc.mode, 1)
			if _, err := Parse([]byte(raw)); err == nil {
				t.Fatalf("Parse() succeeded for mode %s, want error", tc.mode)
			}
		})
	}
}

func TestValidateSkillPath(t *testing.T) {
	tests := []struct {
		name    string
		path    string
		wantErr bool
	}{
		{name: "valid claude skill path", path: ".claude/skills/review-pr/SKILL.md"},
		{name: "valid agent-pool skill path", path: ".agent-pool/skills/review-pr/SKILL.md"},
		{name: "valid nested skill path", path: ".claude/skills/team/review-pr/SKILL.md"},
		{name: "parent traversal", path: "../outside/SKILL.md", wantErr: true},
		{name: "traversal inside prefix", path: ".claude/skills/../../outside/SKILL.md", wantErr: true},
		{name: "absolute path", path: "/etc/passwd", wantErr: true},
		{name: "wrong filename", path: ".claude/skills/foo/bar.sh", wantErr: true},
		{name: "wrong prefix", path: "skills/foo/SKILL.md", wantErr: true},
		{name: "missing skill dir", path: ".claude/skills/SKILL.md", wantErr: true},
		{name: "unnormalized double slash", path: ".claude/skills//foo/SKILL.md", wantErr: true},
		{name: "leading dot slash", path: "./.claude/skills/foo/SKILL.md", wantErr: true},
		{name: "backslash path", path: `.claude\skills\foo\SKILL.md`, wantErr: true},
		{name: "empty path", path: "", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			err := ValidateSkillPath(tc.path)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ValidateSkillPath(%q) error = %v, wantErr %v", tc.path, err, tc.wantErr)
			}
		})
	}

	t.Run("LoadFromDir valid repo", func(t *testing.T) {
		repoRoot := t.TempDir()
		catalogDir := filepath.Join(repoRoot, ".agent-pool")
		skillDir := filepath.Join(repoRoot, ".claude", "skills", "review-pr")
		if err := os.MkdirAll(catalogDir, 0o755); err != nil {
			t.Fatalf("mkdir catalogDir: %v", err)
		}
		if err := os.MkdirAll(skillDir, 0o755); err != nil {
			t.Fatalf("mkdir skillDir: %v", err)
		}
		if err := os.WriteFile(filepath.Join(catalogDir, "catalog.json"), []byte(validReviewPRCatalog), 0o600); err != nil {
			t.Fatalf("write catalog.json: %v", err)
		}
		if err := os.WriteFile(filepath.Join(skillDir, "SKILL.md"), []byte("# Review PR\n"), 0o600); err != nil {
			t.Fatalf("write SKILL.md: %v", err)
		}

		cat, err := LoadFromDir(repoRoot)
		if err != nil {
			t.Fatalf("LoadFromDir() unexpected error: %v", err)
		}
		if _, ok := cat.Skills["review-pr"]; !ok {
			t.Fatalf("expected review-pr skill")
		}
	})

	t.Run("LoadFromDir rejects symlink escaping repo root", func(t *testing.T) {
		outsideDir := t.TempDir()
		outsideFile := filepath.Join(outsideDir, "SKILL.md")
		if err := os.WriteFile(outsideFile, []byte("# Outside\n"), 0o600); err != nil {
			t.Fatalf("write outside file: %v", err)
		}

		repoRoot := t.TempDir()
		catalogDir := filepath.Join(repoRoot, ".agent-pool")
		skillDir := filepath.Join(repoRoot, ".claude", "skills", "review-pr")
		if err := os.MkdirAll(catalogDir, 0o755); err != nil {
			t.Fatalf("mkdir catalogDir: %v", err)
		}
		if err := os.MkdirAll(skillDir, 0o755); err != nil {
			t.Fatalf("mkdir skillDir: %v", err)
		}
		if err := os.WriteFile(filepath.Join(catalogDir, "catalog.json"), []byte(validReviewPRCatalog), 0o600); err != nil {
			t.Fatalf("write catalog.json: %v", err)
		}
		if err := os.Symlink(outsideFile, filepath.Join(skillDir, "SKILL.md")); err != nil {
			t.Fatalf("symlink SKILL.md: %v", err)
		}

		if _, err := LoadFromDir(repoRoot); err == nil {
			t.Fatalf("LoadFromDir() succeeded for escaping symlink, want error")
		}
	})

	t.Run("LoadFromDir rejects missing SKILL.md", func(t *testing.T) {
		repoRoot := t.TempDir()
		catalogDir := filepath.Join(repoRoot, ".agent-pool")
		if err := os.MkdirAll(catalogDir, 0o755); err != nil {
			t.Fatalf("mkdir catalogDir: %v", err)
		}
		if err := os.WriteFile(filepath.Join(catalogDir, "catalog.json"), []byte(validReviewPRCatalog), 0o600); err != nil {
			t.Fatalf("write catalog.json: %v", err)
		}

		if _, err := LoadFromDir(repoRoot); err == nil {
			t.Fatalf("LoadFromDir() succeeded when SKILL.md is missing, want error")
		}
	})
}

func TestBuildPrompt(t *testing.T) {
	skillMD := "# Review PR\nInspect the diff carefully."
	argsJSON := []byte(`{"focus":"security","prNumber":42}`)

	first := BuildPrompt("review-pr", skillMD, argsJSON)
	second := BuildPrompt("review-pr", skillMD, argsJSON)

	if first != second {
		t.Fatalf("BuildPrompt() is not deterministic")
	}

	lower := strings.ToLower(first)
	for _, want := range []string{"untrusted", "exfiltrat", "override"} {
		if !strings.Contains(lower, want) {
			t.Errorf("BuildPrompt() missing security framing keyword %q", want)
		}
	}
	if !strings.Contains(first, "review-pr") {
		t.Errorf("BuildPrompt() missing skill name")
	}
	if !strings.Contains(first, skillMD) {
		t.Errorf("BuildPrompt() missing skill markdown")
	}
	if !strings.Contains(first, string(argsJSON)) {
		t.Errorf("BuildPrompt() missing canonical args JSON")
	}
}
