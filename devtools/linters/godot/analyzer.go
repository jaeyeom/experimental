// Package godot provides a go/analysis analyzer wrapping github.com/tetafro/godot.
package godot

import (
	"fmt"
	"go/ast"

	"github.com/tetafro/godot"
	"golang.org/x/tools/go/analysis"
)

// Analyzer checks that top-level declaration comments end with a period.
var Analyzer = &analysis.Analyzer{
	Name: "godot",
	Doc:  "check if comments end in a period",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	settings := godot.Settings{
		Scope:   godot.DeclScope,
		Period:  true,
		Capital: false,
	}
	for _, file := range pass.Files {
		if ast.IsGenerated(file) {
			continue
		}
		issues, err := godot.Run(file, pass.Fset, settings)
		if err != nil {
			return nil, fmt.Errorf("godot: %w", err)
		}
		tf := pass.Fset.File(file.Pos())
		for _, iss := range issues {
			pos := file.Pos()
			if tf != nil && iss.Pos.Offset >= 0 && iss.Pos.Offset <= tf.Size() {
				pos = tf.Pos(iss.Pos.Offset)
			}
			pass.Reportf(pos, "%s", iss.Message)
		}
	}
	return nil, nil
}
