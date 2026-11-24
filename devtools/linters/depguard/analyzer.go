// Package depguard provides a go/analysis analyzer wrapping github.com/OpenPeeDeeP/depguard/v2
// with the repository's Base and Main import rules.
package depguard

import (
	"fmt"
	"strings"

	"github.com/OpenPeeDeeP/depguard/v2"
	"golang.org/x/tools/go/analysis"
)

// Analyzer checks imports against the Base (deny list) and Main (strict allow
// list for non-test files) rules.
var Analyzer = &analysis.Analyzer{
	Name: "depguard",
	Doc:  "checks that imports are in a list of acceptable packages",
	Run:  run,
}

func settings() *depguard.LinterSettings {
	return &depguard.LinterSettings{
		"Base": {
			ListMode: "lax",
			Files:    []string{"$all"},
			Deny: map[string]string{
				"log$":      "use log/slog instead",
				"io/ioutil": "replaced by io and os packages since Go 1.16: https://tip.golang.org/doc/go1.16#ioutil",
				"syscall":   "deprecated since Go 1.4, use golang.org/x/sys instead",
			},
		},
		"Main": {
			ListMode: "strict",
			Files:    []string{"$all", "!$test"},
			Allow: []string{
				"github.com/OpenPeeDeeP/depguard",
				"github.com/apple/pkl-go",
				"github.com/bmatcuk/doublestar",
				"github.com/charmbracelet/bubbles",
				"github.com/charmbracelet/bubbletea",
				"github.com/charmbracelet/lipgloss",
				"github.com/fsnotify/fsnotify",
				"github.com/google/renameio/v2",
				"github.com/google/uuid",
				"github.com/jaeyeom/experimental",
				"github.com/jaeyeom/go-cmdexec",
				"github.com/jaeyeom/sugo",
				"github.com/bkielbasa/cyclop",
				"github.com/go-critic/go-critic",
				"github.com/kisielk/errcheck",
				"github.com/ldez/tagliatelle",
				"github.com/mattn/go-sqlite3",
				"github.com/muesli/termenv",
				"github.com/pelletier/go-toml/v2",
				"github.com/slack-go/slack",
				"github.com/spf13/cobra",
				"github.com/spf13/viper",
				"github.com/tetafro/godot",
				"github.com/tomarrell/wrapcheck/v2",
				"golang.org/x/image",
				"golang.org/x/sys",
				"golang.org/x/tools",
				"gopkg.in/yaml.v3",
				"modernc.org/sqlite",
			},
			Deny: map[string]string{
				"github.com/leanovate/gopter": "gopter is for test only",
				"github.com/sirupsen/logrus":  "use log/slog instead",
			},
		},
		"Test": {
			ListMode: "lax",
			Files:    []string{"$test"},
			Deny: map[string]string{
				"github.com/stretchr/testify": "Please use standard library for tests",
			},
		},
	}
}

// isStd reports whether an import path belongs to the Go standard library.
// depguard's $gostd macro reads GOROOT/src, which is absent in the nogo
// sandbox, so standard library imports are exempted from Main here instead.
func isStd(path string) bool {
	first, _, _ := strings.Cut(path, "/")
	return !strings.Contains(first, ".")
}

// allowStd drops Main-list diagnostics for standard library imports.
func allowStd(d analysis.Diagnostic) bool {
	const prefix = "import '"
	rest, ok := strings.CutPrefix(d.Message, prefix)
	if !ok {
		return false
	}
	path, tail, ok := strings.Cut(rest, "'")
	return ok && isStd(path) && strings.HasPrefix(tail, " is not allowed from list 'Main'")
}

func run(pass *analysis.Pass) (any, error) {
	a, err := depguard.NewAnalyzer(settings())
	if err != nil {
		return nil, fmt.Errorf("depguard: %w", err)
	}
	filtered := *pass
	filtered.Report = func(d analysis.Diagnostic) {
		if !allowStd(d) {
			pass.Report(d)
		}
	}
	res, err := a.Run(&filtered)
	if err != nil {
		return nil, fmt.Errorf("depguard: %w", err)
	}
	return res, nil
}
