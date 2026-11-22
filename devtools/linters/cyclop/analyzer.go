// Package cyclop provides a go/analysis analyzer wrapping github.com/bkielbasa/cyclop.
package cyclop

import (
	"fmt"

	"github.com/bkielbasa/cyclop/pkg/analyzer"
	"golang.org/x/tools/go/analysis"
)

// Analyzer reports functions whose cyclomatic complexity exceeds 15.
var Analyzer = &analysis.Analyzer{
	Name: "cyclop",
	Doc:  "checks function and package cyclomatic complexity",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	a := analyzer.NewAnalyzer()
	if err := a.Flags.Set("maxComplexity", "15"); err != nil {
		return nil, fmt.Errorf("cyclop: %w", err)
	}
	res, err := a.Run(pass)
	if err != nil {
		return nil, fmt.Errorf("cyclop: %w", err)
	}
	return res, nil
}
