package detector

import (
	"path/filepath"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

// languageProfile is the table row for a supported language.
type languageProfile struct {
	language    config.Language
	configKeys  []string
	extraConfig func(*ScanResult, map[string]string)
	tools       func(root string, scan *ScanResult) map[config.ToolType][]config.Tool
}

var languageProfiles = []languageProfile{
	{
		language:   config.LanguageGo,
		configKeys: []string{"golangci-lint"},
		tools:      goTools,
	},
	{
		language:   config.LanguagePython,
		configKeys: []string{"ruff"},
		tools:      pythonTools,
	},
	{
		language:    config.LanguageTypeScript,
		configKeys:  []string{"eslint", "biome", "prettier"},
		extraConfig: recordTypeScriptConfig,
		tools:       jsTools,
	},
	{
		language:   config.LanguageJavaScript,
		configKeys: []string{"eslint", "biome", "prettier"},
		tools:      jsTools,
	},
}

func goTools(_ string, _ *ScanResult) map[config.ToolType][]config.Tool {
	return map[config.ToolType][]config.Tool{
		config.ToolTypeFormat: availableTools(
			config.Tool{Command: "gofumpt"},
			config.Tool{Command: "gofmt"},
		),
		config.ToolTypeLint: availableTools(
			config.Tool{Command: "golangci-lint"},
			config.Tool{Command: "unnecessary-interface-assertion-linter"},
			config.Tool{Command: "go", Args: []string{"vet"}},
		),
		config.ToolTypeTest: availableTools(
			config.Tool{Command: "go", Args: []string{"test"}},
		),
	}
}

func pythonTools(_ string, scan *ScanResult) map[config.ToolType][]config.Tool {
	format := []config.Tool{
		{Command: "ruff", Args: []string{"format"}},
		{Command: "black"},
	}
	lint := []config.Tool{
		{Command: "ruff", Args: []string{"check"}},
		{Command: "flake8"},
	}
	test := []config.Tool{
		{Command: "pytest"},
		{Command: "python3", Args: []string{"-m", "unittest"}},
		{Command: "python", Args: []string{"-m", "unittest"}},
	}
	if scan != nil && scan.HasFile("uv.lock") {
		format = append(format, config.Tool{Command: "uv", Args: []string{"run", "--no-sync", "ruff", "format"}})
		lint = append(lint, config.Tool{Command: "uv", Args: []string{"run", "--no-sync", "ruff", "check"}})
		test = append(test, config.Tool{Command: "uv", Args: []string{"run", "--no-sync", "pytest"}})
	}
	return map[config.ToolType][]config.Tool{
		config.ToolTypeFormat: availableTools(format...),
		config.ToolTypeLint:   availableTools(lint...),
		config.ToolTypeTest:   availableTools(test...),
	}
}

func jsTools(rootPath string, scan *ScanResult) map[config.ToolType][]config.Tool {
	var format, lint, test []config.Tool
	scripts, pkgRelDir := loadNpmScripts(rootPath, scan)
	toolDir := ""
	if pkgRelDir != "" && pkgRelDir != "." {
		toolDir = pkgRelDir
	}
	pm := detectJSPackageManager(rootPath, pkgRelDir, scan)
	if _, ok := scripts["format"]; ok {
		format = append(format, config.Tool{Command: pm, Args: []string{"run", "format"}, Dir: toolDir})
	}
	if _, ok := scripts["lint"]; ok {
		lint = append(lint, config.Tool{Command: pm, Args: []string{"run", "lint"}, Dir: toolDir})
	}
	if _, ok := scripts["test"]; ok {
		test = append(test, config.Tool{Command: pm, Args: []string{"test"}, Dir: toolDir})
	}
	hasBiome := scan != nil && (scan.HasFile("biome.json") || scan.HasFile("biome.jsonc"))
	if hasBiome {
		format = append(format, config.Tool{Command: "biome", Args: []string{"format", "--write"}})
		lint = append(lint, config.Tool{Command: "biome", Args: []string{"lint"}})
	}
	format = append(format, config.Tool{Command: "prettier"})
	lint = append(lint, config.Tool{Command: "eslint"})
	if _, ok := scripts["test"]; !ok {
		if hasLockfile(rootPath, pkgRelDir, scan, "bun.lock", "bun.lockb") {
			test = append(test, config.Tool{Command: "bun", Args: []string{"test"}, Dir: toolDir})
		}
		test = append(test, config.Tool{Command: "jest"}, config.Tool{Command: "mocha"})
	}
	return map[config.ToolType][]config.Tool{
		config.ToolTypeFormat: availableTools(format...),
		config.ToolTypeLint:   availableTools(lint...),
		config.ToolTypeTest:   availableTools(test...),
	}
}

func recordTypeScriptConfig(scan *ScanResult, files map[string]string) {
	if scan.HasFile("tsconfig.json") {
		for _, f := range scan.Files {
			if filepath.Base(f) == "tsconfig.json" {
				files["typescript"] = f
				return
			}
		}
	}
}
