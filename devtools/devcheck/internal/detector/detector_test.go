package detector

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestProjectDetector_Detect(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "project_detector_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name              string
		files             []string
		expectedLanguages []config.Language
		expectedBuild     config.BuildSystem
		expectedHasGit    bool
	}{
		{
			name:              "go project with bazel",
			files:             []string{"go.mod", "main.go", "MODULE.bazel", "BUILD.bazel"},
			expectedLanguages: []config.Language{config.LanguageGo},
			expectedBuild:     config.BuildSystemBazel,
			expectedHasGit:    false,
		},
		{
			name:              "python project with make",
			files:             []string{"pyproject.toml", "main.py", "Makefile"},
			expectedLanguages: []config.Language{config.LanguagePython},
			expectedBuild:     config.BuildSystemMake,
			expectedHasGit:    false,
		},
		{
			name:              "mixed project with git",
			files:             []string{"go.mod", "main.go", "pyproject.toml", "script.py", ".git/HEAD"},
			expectedLanguages: []config.Language{config.LanguageGo, config.LanguagePython},
			expectedBuild:     config.BuildSystemNone,
			expectedHasGit:    true,
		},
		{
			name:              "typescript project",
			files:             []string{"tsconfig.json", "package.json", "src/main.ts"},
			expectedLanguages: []config.Language{config.LanguageTypeScript},
			expectedBuild:     config.BuildSystemNone,
			expectedHasGit:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test directory
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create test files and directories
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				dir := filepath.Dir(filePath)
				if dir != testDir {
					err := os.MkdirAll(dir, 0o755)
					if err != nil {
						t.Fatal(err)
					}
				}

				if filepath.Base(file) == "HEAD" {
					// Create git HEAD file
					err := os.WriteFile(filePath, []byte("ref: refs/heads/main"), 0o600)
					if err != nil {
						t.Fatal(err)
					}
				} else {
					err := os.WriteFile(filePath, []byte("test content"), 0o600)
					if err != nil {
						t.Fatal(err)
					}
				}
			}

			detector := NewProjectDetector()
			result, err := detector.Detect(testDir)
			if err != nil {
				t.Errorf("Detect() error = %v", err)
				return
			}

			// Check root path
			if result.RootPath != testDir {
				t.Errorf("Expected RootPath %s, got %s", testDir, result.RootPath)
			}

			// Check build system
			if result.BuildSystem != tt.expectedBuild {
				t.Errorf("Expected BuildSystem %v, got %v", tt.expectedBuild, result.BuildSystem)
			}

			// Check languages
			if len(result.Languages) != len(tt.expectedLanguages) {
				t.Errorf("Expected %d languages, got %d: %v", len(tt.expectedLanguages), len(result.Languages), result.Languages)
			} else {
				expectedMap := make(map[config.Language]bool)
				for _, lang := range tt.expectedLanguages {
					expectedMap[lang] = true
				}

				for _, lang := range result.Languages {
					if !expectedMap[lang] {
						t.Errorf("Unexpected language detected: %v", lang)
					}
					delete(expectedMap, lang)
				}

				if len(expectedMap) > 0 {
					t.Errorf("Missing expected languages: %v", expectedMap)
				}
			}

			// Check git detection
			if result.HasGit != tt.expectedHasGit {
				t.Errorf("Expected HasGit %v, got %v", tt.expectedHasGit, result.HasGit)
			}

			// Check that tools are populated
			if len(result.Tools) == 0 {
				t.Error("Expected tools to be populated")
			}

			// Check that detection time is set
			if result.DetectionTime.IsZero() {
				t.Error("Expected DetectionTime to be set")
			}
		})
	}
}

func TestProjectDetector_SupportedLanguages(t *testing.T) {
	detector := NewProjectDetector()

	languages := detector.SupportedLanguages()

	expected := []config.Language{
		config.LanguageGo,
		config.LanguagePython,
		config.LanguageTypeScript,
		config.LanguageJavaScript,
	}

	if len(languages) != len(expected) {
		t.Errorf("Expected %d supported languages, got %d", len(expected), len(languages))
	}

	expectedMap := make(map[config.Language]bool)
	for _, lang := range expected {
		expectedMap[lang] = true
	}

	for _, lang := range languages {
		if !expectedMap[lang] {
			t.Errorf("Unexpected supported language: %v", lang)
		}
	}
}

func TestProjectDetector_SupportedBuildSystems(t *testing.T) {
	detector := NewProjectDetector()

	buildSystems := detector.SupportedBuildSystems()

	expected := []config.BuildSystem{
		config.BuildSystemBazel,
		config.BuildSystemMake,
	}

	if len(buildSystems) != len(expected) {
		t.Errorf("Expected %d supported build systems, got %d", len(expected), len(buildSystems))
	}

	expectedMap := make(map[config.BuildSystem]bool)
	for _, bs := range expected {
		expectedMap[bs] = true
	}

	for _, bs := range buildSystems {
		if !expectedMap[bs] {
			t.Errorf("Unexpected supported build system: %v", bs)
		}
	}
}

func TestProjectDetector_DetectRecordsGolangciConfig(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "golangci_config_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	for _, file := range []string{"go.mod", "main.go", ".golangci.yml"} {
		if err := os.WriteFile(filepath.Join(tempDir, file), []byte("test content"), 0o600); err != nil {
			t.Fatal(err)
		}
	}

	result, err := NewProjectDetector().Detect(tempDir)
	if err != nil {
		t.Fatalf("Detect() error = %v", err)
	}

	got, ok := result.ConfigFiles["golangci-lint"]
	if !ok {
		t.Fatalf("Detect() ConfigFiles = %v, want golangci-lint config recorded", result.ConfigFiles)
	}
	if got != ".golangci.yml" {
		t.Errorf("Detect() ConfigFiles[golangci-lint] = %q, want %q", got, ".golangci.yml")
	}
}

func TestProjectDetector_DetectGitWorktreeFile(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "git_worktree_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	for _, file := range []string{"go.mod", "main.go"} {
		if err := os.WriteFile(filepath.Join(tempDir, file), []byte("test content"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(filepath.Join(tempDir, ".git"), []byte("gitdir: /path/to/main/.git/worktrees/feature"), 0o600); err != nil {
		t.Fatal(err)
	}

	result, err := NewProjectDetector().Detect(tempDir)
	if err != nil {
		t.Fatalf("Detect() error = %v", err)
	}
	if !result.HasGit {
		t.Error("Detect() HasGit = false, want true for a .git worktree file")
	}
}

func TestProjectDetector_DetectWithLocationPriority(t *testing.T) {
	tempDir, err := os.MkdirTemp("", "location_priority_test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tempDir)

	tests := []struct {
		name        string
		files       []string
		expected    config.BuildSystem
		description string
	}{
		{
			name: "bazel_in_root_make_in_subdirectory",
			files: []string{
				"MODULE.bazel",
				"main.go",
				"src/Makefile",
			},
			expected:    config.BuildSystemBazel,
			description: "Should detect Bazel when MODULE.bazel is in root despite Makefile in subdirectory",
		},
		{
			name: "make_in_root_bazel_in_subdirectory",
			files: []string{
				"Makefile",
				"main.go",
				"third_party/MODULE.bazel",
			},
			expected:    config.BuildSystemMake,
			description: "Should detect Make when Makefile is in root despite MODULE.bazel in subdirectory",
		},
		{
			name: "both_in_subdirectories_prefers_make_without_bazel_tools",
			files: []string{
				"main.go",
				"src/MODULE.bazel",
				"test/Makefile",
			},
			expected:    config.BuildSystemMake,
			description: "Should prefer Make when Bazel and Make are at the same depth but Bazel format/lint targets are missing",
		},
		{
			name: "make_shallow_bazel_deep",
			files: []string{
				"src/Makefile",
				"src/third_party/vendor/MODULE.bazel",
				"main.go",
			},
			expected:    config.BuildSystemMake,
			description: "Should detect Make when it's in shallower directory than Bazel",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test directory
			testDir := filepath.Join(tempDir, tt.name)
			err := os.MkdirAll(testDir, 0o755)
			if err != nil {
				t.Fatal(err)
			}

			// Create test files and directories
			for _, file := range tt.files {
				filePath := filepath.Join(testDir, file)
				dir := filepath.Dir(filePath)
				if dir != testDir {
					err := os.MkdirAll(dir, 0o755)
					if err != nil {
						t.Fatal(err)
					}
				}

				err := os.WriteFile(filePath, []byte("test content"), 0o600)
				if err != nil {
					t.Fatal(err)
				}
			}

			detector := NewProjectDetector()
			result, err := detector.Detect(testDir)
			if err != nil {
				t.Errorf("Detect() error = %v", err)
				return
			}

			if result.BuildSystem != tt.expected {
				t.Errorf("%s: expected build system %v, got %v",
					tt.description, tt.expected, result.BuildSystem)
			}
		})
	}
}

func TestProjectDetector_DetectPrefersMakeWhenBazelFormatLintMissing(t *testing.T) {
	withBinsOnPath(t, "make", "bazel")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"go.mod":       "module example",
		"main.go":      "package main",
		"MODULE.bazel": "module(name = \"example\")",
		"BUILD.bazel":  "alias(name = \"format\", actual = \"//tools/format:format\")\n",
		"Makefile":     "format:\nlint:\ntest:\n",
	})

	result, err := NewProjectDetector().Detect(dir)
	if err != nil {
		t.Fatalf("Detect() error = %v", err)
	}
	if result.BuildSystem != config.BuildSystemMake {
		t.Errorf("BuildSystem = %v, want make when //tools:format and //tools:lint are missing", result.BuildSystem)
	}
	assertFirstTool(t, result, config.ToolTypeFormat, "make format")
	assertFirstTool(t, result, config.ToolTypeLint, "make lint")
	assertFirstTool(t, result, config.ToolTypeTest, "make test")
}

func TestProjectDetector_DetectUsesBazelWhenFormatAndLintTargetsExist(t *testing.T) {
	withBinsOnPath(t, "make", "bazel")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"go.mod":            "module example",
		"main.go":           "package main",
		"MODULE.bazel":      "module(name = \"example\")",
		"Makefile":          "format:\nlint:\ntest:\n",
		"tools/BUILD.bazel": "sh_binary(name = \"format\", srcs = [\"format.sh\"])\nsh_binary(name = \"lint\", srcs = [\"lint.sh\"])\n",
	})

	result, err := NewProjectDetector().Detect(dir)
	if err != nil {
		t.Fatalf("Detect() error = %v", err)
	}
	if result.BuildSystem != config.BuildSystemBazel {
		t.Errorf("BuildSystem = %v, want bazel when //tools:format and //tools:lint exist", result.BuildSystem)
	}
	assertFirstTool(t, result, config.ToolTypeFormat, "bazel run //tools:format")
	assertFirstTool(t, result, config.ToolTypeLint, "bazel run //tools:lint")
}

func TestProjectDetector_DetectTSOnlyTreeDoesNotListJavaScript(t *testing.T) {
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"tsconfig.json": `{"compilerOptions":{}}`,
		"src/main.ts":   "export const x = 1;",
		"package.json":  `{"name":"app","scripts":{"test":"jest"}}`,
	})

	result, err := NewProjectDetector().Detect(dir)
	if err != nil {
		t.Fatalf("Detect() error = %v", err)
	}
	for _, lang := range result.Languages {
		if lang == config.LanguageJavaScript {
			t.Fatalf("Languages = %v, want no javascript for a TS-only tree", result.Languages)
		}
	}
	if !hasLanguage(result.Languages, config.LanguageTypeScript) {
		t.Errorf("Languages = %v, want typescript", result.Languages)
	}
}

func TestBazelDetector_GetToolsOnlyListsExistingTargets(t *testing.T) {
	withBinsOnPath(t, "bazel")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"BUILD.bazel": "alias(name = \"format\", actual = \":fmt\")\n",
	})

	tools := NewBazelDetector().GetTools(dir)
	assertContainsTool(t, tools[config.ToolTypeFormat], "bazel run //:format")
	assertNotContainsTool(t, tools[config.ToolTypeFormat], "bazel run //tools:format")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "bazel run //tools:lint")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "bazel run //:lint")
}

func TestMakeDetector_GetToolsOnlyListsExistingTargets(t *testing.T) {
	withBinsOnPath(t, "make")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"Makefile": "format:\ntest:\n",
	})

	tools := NewMakeDetector().GetTools(dir)
	assertContainsTool(t, tools[config.ToolTypeFormat], "make format")
	assertContainsTool(t, tools[config.ToolTypeTest], "make test")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "make lint")
}

func withBinsOnPath(t *testing.T, names ...string) {
	t.Helper()
	binDir := t.TempDir()
	for _, name := range names {
		writeExecutable(t, filepath.Join(binDir, name))
	}
	t.Setenv("PATH", binDir)
}

func writeTree(t *testing.T, dir string, files map[string]string) {
	t.Helper()
	for name, content := range files {
		path := filepath.Join(dir, name)
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
	}
}

func assertFirstTool(t *testing.T, result *config.ProjectConfig, toolType config.ToolType, want string) {
	t.Helper()
	got := result.Tools[toolType]
	if len(got) == 0 || got[0] != want {
		t.Errorf("Tools[%s] first = %v, want %q", toolType, got, want)
	}
}

func assertContainsTool(t *testing.T, tools []string, want string) {
	t.Helper()
	for _, tool := range tools {
		if tool == want {
			return
		}
	}
	t.Errorf("tools %v, want to contain %q", tools, want)
}

func assertNotContainsTool(t *testing.T, tools []string, want string) {
	t.Helper()
	for _, tool := range tools {
		if tool == want {
			t.Errorf("tools %v, want not to contain %q", tools, want)
			return
		}
	}
}
