package catalog

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestArgsSchemaRejectsFreeText(t *testing.T) {
	tests := []struct {
		name       string
		argsSchema string
	}{
		{
			name: "string without enum or pattern+maxLength",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"note": {"type": "string"}
				}
			}`,
		},
		{
			name: "string with pattern only (no maxLength)",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"branch": {"type": "string", "pattern": "^[a-z0-9-]+$"}
				}
			}`,
		},
		{
			name: "string with maxLength only (no pattern)",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"branch": {"type": "string", "maxLength": 64}
				}
			}`,
		},
		{
			name: "string with maxLength > 256",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"branch": {"type": "string", "pattern": "^[a-z0-9-]+$", "maxLength": 257}
				}
			}`,
		},
		{
			name: "string with invalid regex pattern",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"branch": {"type": "string", "pattern": "[unclosed", "maxLength": 64}
				}
			}`,
		},
		{
			name: "additionalProperties true",
			argsSchema: `{
				"type": "object",
				"additionalProperties": true,
				"properties": {
					"prNumber": {"type": "integer"}
				}
			}`,
		},
		{
			name: "additionalProperties omitted",
			argsSchema: `{
				"type": "object",
				"properties": {
					"prNumber": {"type": "integer"}
				}
			}`,
		},
		{
			name: "nested object in argsSchema",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"nested": {
						"type": "object",
						"properties": {}
					}
				}
			}`,
		},
		{
			name: "array in argsSchema",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"properties": {
					"tags": {
						"type": "array",
						"items": {"type": "string"}
					}
				}
			}`,
		},
		{
			name: "unsupported keyword in argsSchema",
			argsSchema: `{
				"type": "object",
				"additionalProperties": false,
				"oneOf": [],
				"properties": {}
			}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			raw := `{
				"version": 1,
				"skills": {
					"test-skill": {
						"description": "Test",
						"mode": "readonly",
						"skillPath": ".claude/skills/test-skill/SKILL.md",
						"argsSchema": ` + tc.argsSchema + `,
						"outputSchema": {"type": "object", "properties": {}}
					}
				}
			}`
			if _, err := Parse([]byte(raw)); err == nil {
				t.Fatalf("Parse() succeeded for %s, want error", tc.name)
			}
		})
	}
}

func TestValidateArgs(t *testing.T) {
	skill := Skill{
		Mode:      ModeReadOnly,
		SkillPath: ".claude/skills/review-pr/SKILL.md",
		ArgsSchema: json.RawMessage(`{
			"type": "object",
			"additionalProperties": false,
			"required": ["prNumber", "branch"],
			"properties": {
				"prNumber": {
					"type": "integer",
					"minimum": 1,
					"maximum": 1000
				},
				"focus": {
					"type": "string",
					"enum": ["security", "correctness", "style"]
				},
				"branch": {
					"type": "string",
					"pattern": "^[a-z0-9][a-z0-9/_-]*$",
					"maxLength": 20
				},
				"strict": {
					"type": "boolean"
				},
				"threshold": {
					"type": "number",
					"minimum": 0.0,
					"maximum": 1.0
				}
			}
		}`),
	}

	t.Run("valid args canonicalized deterministically", func(t *testing.T) {
		raw := json.RawMessage(`{ "strict": true, "prNumber": 42, "branch": "feat/auth", "focus": "security", "threshold": 0.75 }`)
		canonical, err := skill.ValidateArgs(raw)
		if err != nil {
			t.Fatalf("ValidateArgs() unexpected error: %v", err)
		}
		want := `{"branch":"feat/auth","focus":"security","prNumber":42,"strict":true,"threshold":0.75}`
		if string(canonical) != want {
			t.Errorf("canonical = %s, want %s", string(canonical), want)
		}
	})

	tests := []struct {
		name string
		raw  string
	}{
		{
			name: "missing required field",
			raw:  `{"prNumber": 42}`,
		},
		{
			name: "extra unknown field",
			raw:  `{"prNumber": 42, "branch": "main", "extra": "nope"}`,
		},
		{
			name: "float passed for integer",
			raw:  `{"prNumber": 1.5, "branch": "main"}`,
		},
		{
			name: "below minimum",
			raw:  `{"prNumber": 0, "branch": "main"}`,
		},
		{
			name: "above maximum",
			raw:  `{"prNumber": 1001, "branch": "main"}`,
		},
		{
			name: "string exceeding maxLength",
			raw:  `{"prNumber": 42, "branch": "this-branch-name-is-way-too-long"}`,
		},
		{
			name: "non-matching pattern",
			raw:  `{"prNumber": 42, "branch": "INVALID BRANCH!"}`,
		},
		{
			name: "invalid enum value",
			raw:  `{"prNumber": 42, "branch": "main", "focus": "performance"}`,
		},
		{
			name: "wrong type for boolean",
			raw:  `{"prNumber": 42, "branch": "main", "strict": "true"}`,
		},
		{
			name: "threshold above maximum",
			raw:  `{"prNumber": 42, "branch": "main", "threshold": 1.5}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if _, err := skill.ValidateArgs(json.RawMessage(tc.raw)); err == nil {
				t.Fatalf("ValidateArgs(%s) succeeded, want error", tc.raw)
			}
		})
	}
}

func TestValidateOutput(t *testing.T) {
	skill := Skill{
		Mode:      ModeReadOnly,
		SkillPath: ".claude/skills/review-pr/SKILL.md",
		Limits: Limits{
			MaxDeliverableBytes: 512,
		},
		OutputSchema: json.RawMessage(`{
			"type": "object",
			"additionalProperties": false,
			"required": ["summary", "findings"],
			"properties": {
				"summary": {
					"type": "string",
					"minLength": 1,
					"maxLength": 200
				},
				"findings": {
					"type": "array",
					"minItems": 0,
					"maxItems": 2,
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
								"type": "string",
								"minLength": 1
							},
							"line": {
								"type": "integer",
								"minimum": 1
							},
							"note": {
								"type": "null"
							}
						}
					}
				}
			}
		}`),
	}

	t.Run("valid nested findings array", func(t *testing.T) {
		raw := json.RawMessage(`{
			"summary": "Looks good overall.",
			"findings": [
				{"severity": "low", "message": "Minor nit", "line": 12, "note": null}
			]
		}`)
		if err := skill.ValidateOutput(raw); err != nil {
			t.Fatalf("ValidateOutput() unexpected error: %v", err)
		}
	})

	tests := []struct {
		name string
		raw  string
	}{
		{
			name: "maxItems overflow",
			raw: `{
				"summary": "Too many findings",
				"findings": [
					{"severity": "low", "message": "a"},
					{"severity": "low", "message": "b"},
					{"severity": "low", "message": "c"}
				]
			}`,
		},
		{
			name: "missing required nested field",
			raw: `{
				"summary": "Missing message",
				"findings": [
					{"severity": "high"}
				]
			}`,
		},
		{
			name: "maxDeliverableBytes exceeded",
			raw:  `{"summary": "` + strings.Repeat("x", 600) + `", "findings": []}`,
		},
		{
			name: "additionalProperties false violation at root",
			raw:  `{"summary": "ok", "findings": [], "extra": 1}`,
		},
		{
			name: "additionalProperties false violation in nested object",
			raw: `{
				"summary": "ok",
				"findings": [
					{"severity": "low", "message": "a", "unexpected": true}
				]
			}`,
		},
		{
			name: "float for nested integer",
			raw: `{
				"summary": "ok",
				"findings": [
					{"severity": "low", "message": "a", "line": 3.5}
				]
			}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if err := skill.ValidateOutput(json.RawMessage(tc.raw)); err == nil {
				t.Fatalf("ValidateOutput() succeeded for %s, want error", tc.name)
			}
		})
	}
}
