package linkfix

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
			if len(gotChanges) != tt.wantChanges {
				t.Errorf("ProcessContent() changes = %d, want %d", len(gotChanges), tt.wantChanges)
			}
		})
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
			got := LinkPattern.FindAllStringSubmatch(tt.input, -1)
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
				BaseDir:     tmpDir,
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
				BaseDir:     tmpDir,
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
				BaseDir:     tmpDir,
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
				BaseDir:     tmpDir,
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
				BaseDir:     tmpDir,
			},
			linkPath: "/en/existing",
			fileDir:  tmpDir,
			want:     true,
		},
		{
			name: "non-existing prefixed path is invalid",
			cfg: Config{
				LocalPrefix: "/en",
				BaseDir:     tmpDir,
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
		BaseDir:     tmpDir,
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

func TestFindBrokenLinksSkipsCodeBlocks(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "existing.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		LocalPrefix: "/en",
		BaseDir:     tmpDir,
	}

	tests := []struct {
		name       string
		content    string
		wantBroken int
		wantPaths  []string
	}{
		{
			name: "link in fenced code block is skipped",
			content: `Some text before.

` + "```" + `
[broken](nonexisting.md)
` + "```" + `

Some text after.`,
			wantBroken: 0,
			wantPaths:  nil,
		},
		{
			name: "link in fenced code block with language is skipped",
			content: `Some text before.

` + "```markdown" + `
[broken](nonexisting.md)
` + "```" + `

Some text after.`,
			wantBroken: 0,
			wantPaths:  nil,
		},
		{
			name: "link outside code block is checked",
			content: `[broken](nonexisting.md)

` + "```" + `
[also broken](another-nonexisting.md)
` + "```" + `

Some text after.`,
			wantBroken: 1,
			wantPaths:  []string{"nonexisting.md"},
		},
		{
			name: "links in tilde fenced code block are skipped",
			content: `Some text before.

~~~
[broken](nonexisting.md)
~~~

Some text after.`,
			wantBroken: 0,
			wantPaths:  nil,
		},
		{
			name: "multiple code blocks",
			content: `[valid outside](existing.md)

` + "```" + `
[broken inside](nonexisting.md)
` + "```" + `

[broken outside](broken.md)

~~~
[another broken inside](another.md)
~~~

[also valid](existing.md)`,
			wantBroken: 1,
			wantPaths:  []string{"broken.md"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			broken := FindBrokenLinks(cfg, tt.content, tmpDir, "test.md")
			if len(broken) != tt.wantBroken {
				t.Errorf("FindBrokenLinks() found %d broken links, want %d", len(broken), tt.wantBroken)
				for _, bl := range broken {
					t.Errorf("  broken link: %s at line %d", bl.Path, bl.Line)
				}
			}
			for i, bl := range broken {
				if i < len(tt.wantPaths) && bl.Path != tt.wantPaths[i] {
					t.Errorf("broken[%d].Path = %q, want %q", i, bl.Path, tt.wantPaths[i])
				}
			}
		})
	}
}

func TestProcessContentSkipsCodeBlocks(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "slash-commands.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		BaseURL:     "https://code.claude.com/docs",
		LocalPrefix: "/en",
	}

	tests := []struct {
		name        string
		content     string
		wantContent string
		wantChanges int
	}{
		{
			name: "link in code block not processed",
			content: `Some text.

` + "```" + `
[Slash commands](/en/slash-commands)
` + "```" + `

End.`,
			wantContent: `Some text.

` + "```" + `
[Slash commands](/en/slash-commands)
` + "```" + `

End.`,
			wantChanges: 0,
		},
		{
			name: "link outside code block is processed",
			content: `[Slash commands](/en/slash-commands)

` + "```" + `
[Slash commands](/en/slash-commands)
` + "```" + `

End.`,
			wantContent: `[Slash commands](slash-commands.md)

` + "```" + `
[Slash commands](/en/slash-commands)
` + "```" + `

End.`,
			wantChanges: 1,
		},
		{
			name: "tilde code block not processed",
			content: `Some text.

~~~
[Slash commands](/en/slash-commands)
~~~

End.`,
			wantContent: `Some text.

~~~
[Slash commands](/en/slash-commands)
~~~

End.`,
			wantChanges: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotContent, gotChanges := ProcessContent(cfg, tt.content, tmpDir)
			if gotContent != tt.wantContent {
				t.Errorf("ProcessContent() content = %q, want %q", gotContent, tt.wantContent)
			}
			if len(gotChanges) != tt.wantChanges {
				t.Errorf("ProcessContent() changes = %d, want %d", len(gotChanges), tt.wantChanges)
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
		BaseDir:     tmpDir,
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
			if len(gotChanges) != tt.wantChanges {
				t.Errorf("ProcessContent() changes = %d, want %d", len(gotChanges), tt.wantChanges)
			}
		})
	}
}

func TestLinkChange(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "doc.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	cfg := Config{
		BaseURL:     "https://example.com/docs",
		LocalPrefix: "/en",
	}

	content := `Line 1
[Doc](/en/doc) on line 2
Line 3
[Plugin](/en/plugin) on line 4`

	_, changes := ProcessContent(cfg, content, tmpDir)

	if len(changes) != 2 {
		t.Fatalf("ProcessContent() got %d changes, want 2", len(changes))
	}

	// First change
	if changes[0].OldPath != "/en/doc" {
		t.Errorf("changes[0].OldPath = %q, want %q", changes[0].OldPath, "/en/doc")
	}
	if changes[0].NewPath != "doc.md" {
		t.Errorf("changes[0].NewPath = %q, want %q", changes[0].NewPath, "doc.md")
	}
	if changes[0].Line != 2 {
		t.Errorf("changes[0].Line = %d, want %d", changes[0].Line, 2)
	}

	// Second change
	if changes[1].OldPath != "/en/plugin" {
		t.Errorf("changes[1].OldPath = %q, want %q", changes[1].OldPath, "/en/plugin")
	}
	if changes[1].NewPath != "https://example.com/docs/en/plugin" {
		t.Errorf("changes[1].NewPath = %q, want %q", changes[1].NewPath, "https://example.com/docs/en/plugin")
	}
	if changes[1].Line != 4 {
		t.Errorf("changes[1].Line = %d, want %d", changes[1].Line, 4)
	}
}

func TestSuffixAddDefault(t *testing.T) {
	// Create a temporary directory with test files
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "doc.md")
	if err := os.WriteFile(testFile, []byte("# Test"), 0o600); err != nil {
		t.Fatal(err)
	}

	// Config without SuffixAdd should default to ".md"
	cfg := Config{
		LocalPrefix: "/en",
	}

	gotPath, gotChanged := ResolveLink(cfg, "/en/doc", tmpDir)
	if gotPath != "doc.md" {
		t.Errorf("ResolveLink() path = %q, want %q", gotPath, "doc.md")
	}
	if !gotChanged {
		t.Errorf("ResolveLink() changed = %v, want %v", gotChanged, true)
	}
}
