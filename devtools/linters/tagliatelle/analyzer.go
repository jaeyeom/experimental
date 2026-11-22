// Package tagliatelle provides a go/analysis analyzer wrapping github.com/ldez/tagliatelle.
package tagliatelle

import (
	"fmt"

	"github.com/ldez/tagliatelle"
	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/inspect"
)

// Analyzer checks struct tags against case conventions (json: camel, yaml: snake).
var Analyzer = &analysis.Analyzer{
	Name:     "tagliatelle",
	Doc:      "Checks the struct tags.",
	Requires: []*analysis.Analyzer{inspect.Analyzer},
	Run:      run,
}

func run(pass *analysis.Pass) (any, error) {
	a := tagliatelle.New(tagliatelle.Config{
		Base: tagliatelle.Base{
			Rules: map[string]string{
				"json": "camel",
				"yaml": "snake",
			},
		},
	})
	res, err := a.Run(pass)
	if err != nil {
		return nil, fmt.Errorf("tagliatelle: %w", err)
	}
	return res, nil
}
