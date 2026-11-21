// Package gochecknoinits provides a go/analysis analyzer that reports init functions.
package gochecknoinits

import (
	"go/ast"

	"golang.org/x/tools/go/analysis"
)

// Analyzer reports init functions in Go files.
var Analyzer = &analysis.Analyzer{
	Name: "gochecknoinits",
	Doc:  "check that no init functions are present in Go code",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	for _, file := range pass.Files {
		if ast.IsGenerated(file) {
			continue
		}
		for _, decl := range file.Decls {
			funcDecl, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			if funcDecl.Name.Name == "init" && funcDecl.Recv.NumFields() == 0 {
				pass.Reportf(funcDecl.Pos(), "don't use init function")
			}
		}
	}
	return nil, nil
}
