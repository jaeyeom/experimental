package vars

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

func TestParseFlag(t *testing.T) {
	tests := []struct {
		name      string
		input     string
		wantKey   string
		wantValue string
		wantErr   bool
	}{
		{
			name:      "simple key=value",
			input:     "BASE_URL=https://example.com",
			wantKey:   "BASE_URL",
			wantValue: "https://example.com",
		},
		{
			name:      "value with equals sign",
			input:     "QUERY=foo=bar&baz=qux",
			wantKey:   "QUERY",
			wantValue: "foo=bar&baz=qux",
		},
		{
			name:      "empty value",
			input:     "EMPTY=",
			wantKey:   "EMPTY",
			wantValue: "",
		},
		{
			name:      "no equals sign treated as empty value",
			input:     "NOVALUE",
			wantKey:   "NOVALUE",
			wantValue: "",
		},
		{
			name:    "empty key",
			input:   "=value",
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			key, value, err := ParseFlag(tt.input)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseFlag() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err == nil {
				if key != tt.wantKey {
					t.Errorf("ParseFlag() key = %q, want %q", key, tt.wantKey)
				}
				if value != tt.wantValue {
					t.Errorf("ParseFlag() value = %q, want %q", value, tt.wantValue)
				}
			}
		})
	}
}

func TestParseFlags(t *testing.T) {
	tests := []struct {
		name    string
		flags   []string
		want    Vars
		wantErr bool
	}{
		{
			name:  "multiple flags",
			flags: []string{"BASE_URL=https://example.com", "USER=testuser"},
			want: Vars{
				"BASE_URL": "https://example.com",
				"USER":     "testuser",
			},
		},
		{
			name:  "empty flags",
			flags: []string{},
			want:  Vars{},
		},
		{
			name:  "flag without equals treated as empty value",
			flags: []string{"BASE_URL=https://example.com", "DEBUG"},
			want: Vars{
				"BASE_URL": "https://example.com",
				"DEBUG":    "",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseFlags(tt.flags)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseFlags() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err == nil && !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ParseFlags() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestLoadEnvFile(t *testing.T) {
	// Create temp file
	content := `# This is a comment
BASE_URL=https://example.com
USER=testuser

# Another comment
PASSWORD=secret123
`
	tmpDir := t.TempDir()
	envFile := filepath.Join(tmpDir, ".gherun.env")
	if err := os.WriteFile(envFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write temp file: %v", err)
	}

	vars, err := LoadEnvFile(envFile)
	if err != nil {
		t.Fatalf("LoadEnvFile() error = %v", err)
	}

	want := Vars{
		"BASE_URL": "https://example.com",
		"USER":     "testuser",
		"PASSWORD": "secret123",
	}

	if !reflect.DeepEqual(vars, want) {
		t.Errorf("LoadEnvFile() = %v, want %v", vars, want)
	}
}

func TestLoadEnvFile_NonExistent(t *testing.T) {
	_, err := LoadEnvFile("/nonexistent/path/.env")
	if err == nil {
		t.Error("LoadEnvFile() expected error for non-existent file")
	}
}

func TestSubstitute(t *testing.T) {
	tests := []struct {
		name    string
		vars    Vars
		content string
		want    string
		wantErr bool
	}{
		{
			name: "simple substitution",
			vars: Vars{"BASE_URL": "https://example.com"},
			content: `Given I navigate to "{{BASE_URL}}/login"
When I click "Sign In"`,
			want: `Given I navigate to "https://example.com/login"
When I click "Sign In"`,
		},
		{
			name: "multiple substitutions",
			vars: Vars{
				"BASE_URL": "https://example.com",
				"USER":     "testuser@example.com",
			},
			content: `Given I navigate to "{{BASE_URL}}/login"
When I enter "{{USER}}" in the email field`,
			want: `Given I navigate to "https://example.com/login"
When I enter "testuser@example.com" in the email field`,
		},
		{
			name:    "undefined variable",
			vars:    Vars{},
			content: `Given I navigate to "{{BASE_URL}}/login"`,
			wantErr: true,
		},
		{
			name:    "multiple undefined variables",
			vars:    Vars{},
			content: `{{BASE_URL}} and {{USER}}`,
			wantErr: true,
		},
		{
			name:    "no variables",
			vars:    Vars{},
			content: "No variables here",
			want:    "No variables here",
		},
		{
			name:    "repeated variable",
			vars:    Vars{"URL": "https://example.com"},
			content: "{{URL}} and {{URL}} again",
			want:    "https://example.com and https://example.com again",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := tt.vars.Substitute(tt.content)
			if (err != nil) != tt.wantErr {
				t.Errorf("Substitute() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if err == nil && got != tt.want {
				t.Errorf("Substitute() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSubstituteWithDefaults(t *testing.T) {
	vars := Vars{"BASE_URL": "https://example.com"}
	content := `{{BASE_URL}}/login with {{UNDEFINED}}`
	want := "https://example.com/login with "

	got := vars.SubstituteWithDefaults(content)
	if got != want {
		t.Errorf("SubstituteWithDefaults() = %q, want %q", got, want)
	}
}

func TestFindVariables(t *testing.T) {
	content := `Given I navigate to "{{BASE_URL}}/login"
And I log in as "{{USER}}" with password "{{PASSWORD}}"
Then I should see "{{USER}}"` // USER appears twice

	got := FindVariables(content)
	want := []string{"BASE_URL", "USER", "PASSWORD"}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("FindVariables() = %v, want %v", got, want)
	}
}

func TestMerge(t *testing.T) {
	v1 := Vars{"A": "1", "B": "2"}
	v2 := Vars{"B": "overridden", "C": "3"}

	got := Merge(v1, v2)
	want := Vars{"A": "1", "B": "overridden", "C": "3"}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Merge() = %v, want %v", got, want)
	}
}

func TestEnvFileAndFlagsCombined(t *testing.T) {
	// Simulate: gherun --env-file .gherun.env --var BASE_URL=https://override.com
	// Env file has BASE_URL and PASSWORD, flag overrides BASE_URL

	// Create temp env file
	content := `BASE_URL=https://original.com
PASSWORD=secret123
`
	tmpDir := t.TempDir()
	envFile := filepath.Join(tmpDir, ".gherun.env")
	if err := os.WriteFile(envFile, []byte(content), 0o600); err != nil {
		t.Fatalf("Failed to write temp file: %v", err)
	}

	// Load env file
	envVars, err := LoadEnvFile(envFile)
	if err != nil {
		t.Fatalf("LoadEnvFile() error = %v", err)
	}

	// Parse flag that overrides BASE_URL
	flagVars, err := ParseFlags([]string{"BASE_URL=https://override.com"})
	if err != nil {
		t.Fatalf("ParseFlags() error = %v", err)
	}

	// Merge: flags override env file
	combined := Merge(envVars, flagVars)

	want := Vars{
		"BASE_URL": "https://override.com", // Overridden by flag
		"PASSWORD": "secret123",            // From env file
	}

	if !reflect.DeepEqual(combined, want) {
		t.Errorf("Combined vars = %v, want %v", combined, want)
	}

	// Test substitution with combined vars
	content2 := `Navigate to "{{BASE_URL}}" with password "{{PASSWORD}}"`
	result, err := combined.Substitute(content2)
	if err != nil {
		t.Fatalf("Substitute() error = %v", err)
	}

	wantResult := `Navigate to "https://override.com" with password "secret123"`
	if result != wantResult {
		t.Errorf("Substitute() = %q, want %q", result, wantResult)
	}
}
