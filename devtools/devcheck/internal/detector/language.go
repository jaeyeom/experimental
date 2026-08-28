package detector

import (
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
		configKeys:  []string{"eslint"},
		extraConfig: recordTypeScriptConfig,
		tools:       jsTools,
	},
	{
		language:   config.LanguageJavaScript,
		configKeys: []string{"eslint"},
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
		),
		config.ToolTypeTest: availableTools(
			config.Tool{Command: "go", Args: []string{"test"}},
		),
	}
}

func pythonTools(_ string, _ *ScanResult) map[config.ToolType][]config.Tool {
	return map[config.ToolType][]config.Tool{
		config.ToolTypeFormat: availableTools(
			config.Tool{Command: "ruff", Args: []string{"format"}},
			config.Tool{Command: "black"},
		),
		config.ToolTypeLint: availableTools(
			config.Tool{Command: "ruff", Args: []string{"check"}},
			config.Tool{Command: "flake8"},
		),
		config.ToolTypeTest: availableTools(
			config.Tool{Command: "pytest"},
			config.Tool{Command: "python", Args: []string{"-m", "unittest"}},
		),
	}
}

func jsTools(rootPath string, scan *ScanResult) map[config.ToolType][]config.Tool {
	var format, lint, test []config.Tool
	scripts := loadNpmScripts(rootPath, scan)
	if _, ok := scripts["format"]; ok {
		format = append(format, config.Tool{Command: "npm", Args: []string{"run", "format"}})
	}
	if _, ok := scripts["lint"]; ok {
		lint = append(lint, config.Tool{Command: "npm", Args: []string{"run", "lint"}})
	}
	if _, ok := scripts["test"]; ok {
		test = append(test, config.Tool{Command: "npm", Args: []string{"test"}})
	}
	format = append(format, config.Tool{Command: "prettier"})
	lint = append(lint, config.Tool{Command: "eslint"})
	if _, ok := scripts["test"]; !ok {
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
		files["typescript"] = "tsconfig.json"
	}
}
