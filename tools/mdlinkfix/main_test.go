package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestResolveLink(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "slash-commands.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name        string
		cfg         Config
		linkPath    string
		fileDir     string
		wantPath    string
		wantChanged bool
	}{
		{
			name: "absolute URL unchanged",
			cfg: Config{
				BaseURL:     "https://example.com",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "https://google.com",
			fileDir:     tmpDir,
			wantPath:    "https://google.com",
			wantChanged: false,
		},
		{
			name: "anchor link unchanged",
			cfg: Config{
				BaseURL:     "https://example.com",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "#section",
			fileDir:     tmpDir,
			wantPath:    "#section",
			wantChanged: false,
		},
		{
			name: "local file exists",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "/en/slash-commands",
			fileDir:     tmpDir,
			wantPath:    "slash-commands.md",
			wantChanged: true,
		},
		{
			name: "local file does not exist - fallback to URL",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "/en/plugins",
			fileDir:     tmpDir,
			wantPath:    "https://code.claude.com/docs/en/plugins",
			wantChanged: true,
		},
		{
			name: "non-prefixed path with base URL",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "/other/path",
			fileDir:     tmpDir,
			wantPath:    "https://code.claude.com/docs/other/path",
			wantChanged: true,
		},
		{
			name: "mailto link unchanged",
			cfg: Config{
				BaseURL:     "https://example.com",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "mailto:test@example.com",
			fileDir:     tmpDir,
			wantPath:    "mailto:test@example.com",
			wantChanged: false,
		},
		{
			name: "relative path without prefix unchanged",
			cfg: Config{
				BaseURL:     "",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
			},
			linkPath:    "other-file.md",
			fileDir:     tmpDir,
			wantPath:    "other-file.md",
			wantChanged: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPath, gotChanged := ResolveLink(tt.cfg, tt.linkPath, tt.fileDir)
			if gotPath != tt.wantPath {
				t.Errorf("ResolveLink() path = %q, want %q", gotPath, tt.wantPath)
			}
			if gotChanged != tt.wantChanged {
				t.Errorf("ResolveLink() changed = %v, want %v", gotChanged, tt.wantChanged)
			}
		})
	}
}

func TestProcessContent(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "slash-commands.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		BaseURL:     "https://code.claude.com/docs",
		LocalPrefix: "/en",
		SuffixAdd:   ".md",
	}

	tests := []struct {
		name        string
		content     string
		wantContent string
		wantChanges int
	}{
		{
			name:        "single link with local file",
			content:     "Check out [Slash commands](/en/slash-commands) for more info.",
			wantContent: "Check out [Slash commands](slash-commands.md) for more info.",
			wantChanges: 1,
		},
		{
			name:        "single link without local file",
			content:     "Check out [Plugins](/en/plugins) for more info.",
			wantContent: "Check out [Plugins](https://code.claude.com/docs/en/plugins) for more info.",
			wantChanges: 1,
		},
		{
			name:        "multiple links mixed",
			content:     "[Local](/en/slash-commands) and [Remote](/en/plugins)",
			wantContent: "[Local](slash-commands.md) and [Remote](https://code.claude.com/docs/en/plugins)",
			wantChanges: 2,
		},
		{
			name:        "absolute URL unchanged",
			content:     "Visit [Google](https://google.com)",
			wantContent: "Visit [Google](https://google.com)",
			wantChanges: 0,
		},
		{
			name:        "anchor link unchanged",
			content:     "See [Section](#section)",
			wantContent: "See [Section](#section)",
			wantChanges: 0,
		},
		{
			name:        "no links",
			content:     "Just some plain text.",
			wantContent: "Just some plain text.",
			wantChanges: 0,
		},
		{
			name:        "link with model config path",
			content:     "Configure the [model](/en/model-config) here.",
			wantContent: "Configure the [model](https://code.claude.com/docs/en/model-config) here.",
			wantChanges: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotContent, gotChanges := ProcessContent(cfg, tt.content, tmpDir)
			if gotContent != tt.wantContent {
				t.Errorf("ProcessContent() content = %q, want %q", gotContent, tt.wantContent)
			}
			if gotChanges != tt.wantChanges {
				t.Errorf("ProcessContent() changes = %d, want %d", gotChanges, tt.wantChanges)
			}
		})
	}
}

func TestParseEnvFile(t *testing.T) {
	tmpDir := t.TempDir()

	tests := []struct {
		name    string
		content string
		want    map[string]string
	}{
		{
			name: "simple key-value",
			content: `MDLINK_BASE_URL=https://example.com
MDLINK_LOCAL_PREFIX=/en`,
			want: map[string]string{
				"MDLINK_BASE_URL":     "https://example.com",
				"MDLINK_LOCAL_PREFIX": "/en",
			},
		},
		{
			name: "with comments and empty lines",
			content: `# This is a comment
MDLINK_BASE_URL=https://example.com

# Another comment
MDLINK_LOCAL_PREFIX=/en
`,
			want: map[string]string{
				"MDLINK_BASE_URL":     "https://example.com",
				"MDLINK_LOCAL_PREFIX": "/en",
			},
		},
		{
			name: "quoted values",
			content: `MDLINK_BASE_URL="https://example.com"
MDLINK_LOCAL_PREFIX='/en'`,
			want: map[string]string{
				"MDLINK_BASE_URL":     "https://example.com",
				"MDLINK_LOCAL_PREFIX": "/en",
			},
		},
		{
			name: "with spaces around equals",
			content: `MDLINK_BASE_URL = https://example.com
MDLINK_LOCAL_PREFIX = /en`,
			want: map[string]string{
				"MDLINK_BASE_URL":     "https://example.com",
				"MDLINK_LOCAL_PREFIX": "/en",
			},
		},
		{
			name: "direnv export format",
			content: `export MDLINK_BASE_URL=https://example.com
export MDLINK_LOCAL_PREFIX=/en`,
			want: map[string]string{
				"MDLINK_BASE_URL":     "https://example.com",
				"MDLINK_LOCAL_PREFIX": "/en",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			envFile := filepath.Join(tmpDir, ".envrc")
			if err := os.WriteFile(envFile, []byte(tt.content), 0o600); err != nil {
				t.Fatal(err)
			}

			got, err := parseEnvFile(envFile)
			if err != nil {
				t.Fatalf("parseEnvFile() error = %v", err)
			}

			for k, v := range tt.want {
				if got[k] != v {
					t.Errorf("parseEnvFile()[%q] = %q, want %q", k, got[k], v)
				}
			}
		})
	}
}

func TestFindEnvFile(t *testing.T) {
	// Create directory structure: tmpDir/subdir/subsubdir
	tmpDir := t.TempDir()
	subdir := filepath.Join(tmpDir, "subdir")
	subsubdir := filepath.Join(subdir, "subsubdir")
	if err := os.MkdirAll(subsubdir, 0o755); err != nil {
		t.Fatal(err)
	}

	// Create .envrc in tmpDir
	envFile := filepath.Join(tmpDir, ".envrc")
	if err := os.WriteFile(envFile, []byte("MDLINK_BASE_URL=https://example.com"), 0o600); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name       string
		startDir   string
		wantPath   string
		wantEnvDir string
	}{
		{
			name:       "find in same directory",
			startDir:   tmpDir,
			wantPath:   envFile,
			wantEnvDir: tmpDir,
		},
		{
			name:       "find in parent directory",
			startDir:   subdir,
			wantPath:   envFile,
			wantEnvDir: tmpDir,
		},
		{
			name:       "find in grandparent directory",
			startDir:   subsubdir,
			wantPath:   envFile,
			wantEnvDir: tmpDir,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPath, gotEnvDir := findEnvFile(tt.startDir)
			if gotPath != tt.wantPath {
				t.Errorf("findEnvFile() path = %q, want %q", gotPath, tt.wantPath)
			}
			if gotEnvDir != tt.wantEnvDir {
				t.Errorf("findEnvFile() envDir = %q, want %q", gotEnvDir, tt.wantEnvDir)
			}
		})
	}
}

func TestLoadConfig(t *testing.T) {
	tmpDir := t.TempDir()

	// Create .envrc file
	envContent := `MDLINK_BASE_URL=https://example.com/docs
MDLINK_LOCAL_PREFIX=/en
MDLINK_SUFFIX_ADD=.md
`
	envFile := filepath.Join(tmpDir, ".envrc")
	if err := os.WriteFile(envFile, []byte(envContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Clear any existing env vars
	os.Unsetenv("MDLINK_BASE_URL")
	os.Unsetenv("MDLINK_LOCAL_PREFIX")
	os.Unsetenv("MDLINK_SUFFIX_DROP")
	os.Unsetenv("MDLINK_SUFFIX_ADD")

	cfg := LoadConfig(tmpDir)

	if cfg.BaseURL != "https://example.com/docs" {
		t.Errorf("LoadConfig() BaseURL = %q, want %q", cfg.BaseURL, "https://example.com/docs")
	}
	if cfg.LocalPrefix != "/en" {
		t.Errorf("LoadConfig() LocalPrefix = %q, want %q", cfg.LocalPrefix, "/en")
	}
	if cfg.SuffixAdd != ".md" {
		t.Errorf("LoadConfig() SuffixAdd = %q, want %q", cfg.SuffixAdd, ".md")
	}
	if cfg.envDir != tmpDir {
		t.Errorf("LoadConfig() envDir = %q, want %q", cfg.envDir, tmpDir)
	}
}

func TestLoadConfigEnvOverride(t *testing.T) {
	tmpDir := t.TempDir()

	// Create .envrc file
	envContent := `MDLINK_BASE_URL=https://example.com/docs
MDLINK_LOCAL_PREFIX=/en
`
	envFile := filepath.Join(tmpDir, ".envrc")
	if err := os.WriteFile(envFile, []byte(envContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Set env var to override
	os.Setenv("MDLINK_BASE_URL", "https://override.com")
	defer os.Unsetenv("MDLINK_BASE_URL")

	cfg := LoadConfig(tmpDir)

	// Env var should override .env file
	if cfg.BaseURL != "https://override.com" {
		t.Errorf("LoadConfig() BaseURL = %q, want %q", cfg.BaseURL, "https://override.com")
	}
	// .env value should still be used for non-overridden vars
	if cfg.LocalPrefix != "/en" {
		t.Errorf("LoadConfig() LocalPrefix = %q, want %q", cfg.LocalPrefix, "/en")
	}
}

func TestProcessFileWithEnv(t *testing.T) {
	// Create a temporary directory with .envrc and test files
	tmpDir := t.TempDir()

	// Create .envrc file
	envContent := `MDLINK_BASE_URL=https://example.com/docs
MDLINK_LOCAL_PREFIX=/en
MDLINK_SUFFIX_ADD=.md
`
	envFile := filepath.Join(tmpDir, ".envrc")
	if err := os.WriteFile(envFile, []byte(envContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create a local file that should be found
	localFile := filepath.Join(tmpDir, "local-doc.md")
	if err := os.WriteFile(localFile, []byte("# Local"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create the file to process
	inputFile := filepath.Join(tmpDir, "input.md")
	inputContent := `# Test Document

Check out [Local Doc](/en/local-doc) and [Remote Doc](/en/remote-doc).
`
	if err := os.WriteFile(inputFile, []byte(inputContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Clear any existing env vars
	os.Unsetenv("MDLINK_BASE_URL")
	os.Unsetenv("MDLINK_LOCAL_PREFIX")
	os.Unsetenv("MDLINK_SUFFIX_DROP")
	os.Unsetenv("MDLINK_SUFFIX_ADD")

	// Process the file
	*dryRun = false
	*verbose = false
	*validate = false
	if _, err := ProcessFile(inputFile); err != nil {
		t.Fatalf("ProcessFile() error = %v", err)
	}

	// Read the result
	result, err := os.ReadFile(inputFile)
	if err != nil {
		t.Fatal(err)
	}

	expected := `# Test Document

Check out [Local Doc](local-doc.md) and [Remote Doc](https://example.com/docs/en/remote-doc).
`
	if string(result) != expected {
		t.Errorf("ProcessFile() result = %q, want %q", string(result), expected)
	}
}

func TestProcessFileInSubdirectory(t *testing.T) {
	// Create directory structure: tmpDir/.envrc, tmpDir/local.md, tmpDir/subdir/input.md
	tmpDir := t.TempDir()
	subdir := filepath.Join(tmpDir, "subdir")
	if err := os.MkdirAll(subdir, 0o755); err != nil {
		t.Fatal(err)
	}

	// Create .envrc file in tmpDir
	envContent := `MDLINK_BASE_URL=https://example.com/docs
MDLINK_LOCAL_PREFIX=/en
MDLINK_SUFFIX_ADD=.md
`
	envFile := filepath.Join(tmpDir, ".envrc")
	if err := os.WriteFile(envFile, []byte(envContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create a local file in tmpDir that should be found
	localFile := filepath.Join(tmpDir, "local-doc.md")
	if err := os.WriteFile(localFile, []byte("# Local"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Create the file to process in subdir
	inputFile := filepath.Join(subdir, "input.md")
	inputContent := `# Test Document

Check out [Local Doc](/en/local-doc) and [Remote Doc](/en/remote-doc).
`
	if err := os.WriteFile(inputFile, []byte(inputContent), 0o600); err != nil {
		t.Fatal(err)
	}

	// Clear any existing env vars
	os.Unsetenv("MDLINK_BASE_URL")
	os.Unsetenv("MDLINK_LOCAL_PREFIX")
	os.Unsetenv("MDLINK_SUFFIX_DROP")
	os.Unsetenv("MDLINK_SUFFIX_ADD")

	// Process the file
	*dryRun = false
	*verbose = false
	*validate = false
	if _, err := ProcessFile(inputFile); err != nil {
		t.Fatalf("ProcessFile() error = %v", err)
	}

	// Read the result
	result, err := os.ReadFile(inputFile)
	if err != nil {
		t.Fatal(err)
	}

	// The link should be relative from subdir to tmpDir/local-doc.md
	expected := `# Test Document

Check out [Local Doc](../local-doc.md) and [Remote Doc](https://example.com/docs/en/remote-doc).
`
	if string(result) != expected {
		t.Errorf("ProcessFile() result = %q, want %q", string(result), expected)
	}
}

func TestLinkPattern(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  [][]string
	}{
		{
			name:  "simple link",
			input: "[text](url)",
			want:  [][]string{{"[text](url)", "text", "url"}},
		},
		{
			name:  "link with title",
			input: `[text](url "title")`,
			want:  [][]string{{`[text](url "title")`, "text", "url"}},
		},
		{
			name:  "link with path",
			input: "[text](/en/path)",
			want:  [][]string{{"[text](/en/path)", "text", "/en/path"}},
		},
		{
			name:  "link with anchor",
			input: "[text](#anchor)",
			want:  [][]string{{"[text](#anchor)", "text", "#anchor"}},
		},
		{
			name:  "link with path and anchor",
			input: "[text](/en/path#anchor)",
			want:  [][]string{{"[text](/en/path#anchor)", "text", "/en/path#anchor"}},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := linkPattern.FindAllStringSubmatch(tt.input, -1)
			if len(got) != len(tt.want) {
				t.Errorf("FindAllStringSubmatch() got %d matches, want %d", len(got), len(tt.want))
				return
			}
			for i := range got {
				for j := range got[i] {
					if got[i][j] != tt.want[i][j] {
						t.Errorf("match[%d][%d] = %q, want %q", i, j, got[i][j], tt.want[i][j])
					}
				}
			}
		})
	}
}

func TestTryURLToLocal(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "slash-commands.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name        string
		cfg         Config
		linkPath    string
		fileDir     string
		wantPath    string
		wantChanged bool
	}{
		{
			name: "URL converts to local file",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath:    "https://code.claude.com/docs/en/slash-commands",
			fileDir:     tmpDir,
			wantPath:    "slash-commands.md",
			wantChanged: true,
		},
		{
			name: "URL with anchor converts to local file",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath:    "https://code.claude.com/docs/en/slash-commands#section",
			fileDir:     tmpDir,
			wantPath:    "slash-commands.md#section",
			wantChanged: true,
		},
		{
			name: "URL stays when local file does not exist",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath:    "https://code.claude.com/docs/en/plugins",
			fileDir:     tmpDir,
			wantPath:    "https://code.claude.com/docs/en/plugins",
			wantChanged: false,
		},
		{
			name: "URL without local prefix stays unchanged",
			cfg: Config{
				BaseURL:     "https://code.claude.com/docs",
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath:    "https://code.claude.com/docs/other/path",
			fileDir:     tmpDir,
			wantPath:    "https://code.claude.com/docs/other/path",
			wantChanged: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPath, gotChanged := ResolveLink(tt.cfg, tt.linkPath, tt.fileDir)
			if gotPath != tt.wantPath {
				t.Errorf("ResolveLink() path = %q, want %q", gotPath, tt.wantPath)
			}
			if gotChanged != tt.wantChanged {
				t.Errorf("ResolveLink() changed = %v, want %v", gotChanged, tt.wantChanged)
			}
		})
	}
}

func TestValidateLocalLink(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "existing.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		name     string
		cfg      Config
		linkPath string
		fileDir  string
		want     bool
	}{
		{
			name:     "anchor-only link is valid",
			cfg:      Config{},
			linkPath: "#section",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name:     "mailto link is valid",
			cfg:      Config{},
			linkPath: "mailto:test@example.com",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name:     "absolute URL is valid (not validated)",
			cfg:      Config{},
			linkPath: "https://example.com",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name:     "existing relative file is valid",
			cfg:      Config{},
			linkPath: "existing.md",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name:     "non-existing relative file is invalid",
			cfg:      Config{},
			linkPath: "nonexisting.md",
			fileDir:  tmpDir,
			want:     false,
		},
		{
			name: "existing prefixed path is valid",
			cfg: Config{
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath: "/en/existing",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name: "non-existing prefixed path is invalid",
			cfg: Config{
				LocalPrefix: "/en",
				SuffixAdd:   ".md",
				envDir:      tmpDir,
			},
			linkPath: "/en/nonexisting",
			fileDir:  tmpDir,
			want:     false,
		},
		{
			name:     "link with anchor to existing file is valid",
			cfg:      Config{},
			linkPath: "existing.md#section",
			fileDir:  tmpDir,
			want:     true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ValidateLocalLink(tt.cfg, tt.linkPath, tt.fileDir)
			if got != tt.want {
				t.Errorf("ValidateLocalLink() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFindBrokenLinks(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "existing.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		LocalPrefix: "/en",
		SuffixAdd:   ".md",
		envDir:      tmpDir,
	}

	tests := []struct {
		name       string
		content    string
		wantBroken int
		wantPaths  []string
	}{
		{
			name:       "no broken links",
			content:    "Check [existing](existing.md) file.",
			wantBroken: 0,
			wantPaths:  nil,
		},
		{
			name:       "one broken link",
			content:    "Check [nonexisting](nonexisting.md) file.",
			wantBroken: 1,
			wantPaths:  []string{"nonexisting.md"},
		},
		{
			name:       "mixed valid and broken",
			content:    "[Valid](existing.md) and [Invalid](broken.md)",
			wantBroken: 1,
			wantPaths:  []string{"broken.md"},
		},
		{
			name:       "prefixed broken link",
			content:    "[Missing](/en/missing) document.",
			wantBroken: 1,
			wantPaths:  []string{"/en/missing"},
		},
		{
			name:       "absolute URLs are not broken",
			content:    "[External](https://example.com) link.",
			wantBroken: 0,
			wantPaths:  nil,
		},
		{
			name:       "anchor links are not broken",
			content:    "[Section](#section) anchor.",
			wantBroken: 0,
			wantPaths:  nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			broken := FindBrokenLinks(cfg, tt.content, tmpDir, "test.md")
			if len(broken) != tt.wantBroken {
				t.Errorf("FindBrokenLinks() found %d broken links, want %d", len(broken), tt.wantBroken)
			}
			for i, bl := range broken {
				if i < len(tt.wantPaths) && bl.Path != tt.wantPaths[i] {
					t.Errorf("broken[%d].Path = %q, want %q", i, bl.Path, tt.wantPaths[i])
				}
			}
		})
	}
}

func TestProcessContentURLToLocal(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "slash-commands.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		BaseURL:     "https://code.claude.com/docs",
		LocalPrefix: "/en",
		SuffixAdd:   ".md",
		envDir:      tmpDir,
	}

	tests := []struct {
		name        string
		content     string
		wantContent string
		wantChanges int
	}{
		{
			name:        "URL converted to local file",
			content:     "Check [Slash commands](https://code.claude.com/docs/en/slash-commands) for more info.",
			wantContent: "Check [Slash commands](slash-commands.md) for more info.",
			wantChanges: 1,
		},
		{
			name:        "URL with anchor converted to local file",
			content:     "See [Section](https://code.claude.com/docs/en/slash-commands#section) here.",
			wantContent: "See [Section](slash-commands.md#section) here.",
			wantChanges: 1,
		},
		{
			name:        "URL stays when no local file exists",
			content:     "Check [Plugins](https://code.claude.com/docs/en/plugins) for more info.",
			wantContent: "Check [Plugins](https://code.claude.com/docs/en/plugins) for more info.",
			wantChanges: 0,
		},
		{
			name:        "other URLs unchanged",
			content:     "Visit [Google](https://google.com) for search.",
			wantContent: "Visit [Google](https://google.com) for search.",
			wantChanges: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotContent, gotChanges := ProcessContent(cfg, tt.content, tmpDir)
			if gotContent != tt.wantContent {
				t.Errorf("ProcessContent() content = %q, want %q", gotContent, tt.wantContent)
			}
			if gotChanges != tt.wantChanges {
				t.Errorf("ProcessContent() changes = %d, want %d", gotChanges, tt.wantChanges)
			}
		})
	}
}
