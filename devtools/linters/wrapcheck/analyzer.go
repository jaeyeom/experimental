// Package wrapcheck provides a go/analysis analyzer wrapping github.com/tomarrell/wrapcheck/v2/wrapcheck.
package wrapcheck

import (
	"fmt"

	"github.com/tomarrell/wrapcheck/v2/wrapcheck"
	"golang.org/x/tools/go/analysis"
)

// Analyzer checks that errors returned from external packages are wrapped.
var Analyzer = &analysis.Analyzer{
	Name: "wrapcheck",
	Doc:  "check that errors returned from external packages are wrapped",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	res, err := wrapcheck.NewAnalyzer(wrapcheck.NewDefaultConfig()).Run(pass)
	if err != nil {
		return nil, fmt.Errorf("wrapcheck: %w", err)
	}
	return res, nil
}
