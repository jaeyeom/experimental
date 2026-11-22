// Package gocritic provides a go/analysis analyzer wrapping github.com/go-critic/go-critic.
package gocritic

import (
	"fmt"
	"go/types"
	"runtime"

	_ "github.com/go-critic/go-critic/checkers" // Registers the checkers.
	"github.com/go-critic/go-critic/linter"
	"golang.org/x/tools/go/analysis"
)

// enabledChecks lists the default go-critic checks (all checks that are not
// experimental, opinionated, performance, or security). They are listed
// explicitly so a go-critic upgrade does not change the set silently.
// ruleguard is not in the default set and is not included.
var enabledChecks = map[string]bool{
	"appendAssign":      true,
	"badCond":           true,
	"captLocal":         true,
	"caseOrder":         true,
	"codegenComment":    true,
	"commentFormatting": true,
	"defaultCaseOrder":  true,
	"deprecatedComment": true,
	"dupBranchBody":     true,
	"dupCase":           true,
	"dupSubExpr":        true,
	"elseif":            true,
	"exitAfterDefer":    true,
	"flagName":          true,
	"ifElseChain":       true,
	"mapKey":            true,
	"newDeref":          true,
	"singleCaseSwitch":  true,
	"sloppyTypeAssert":  true,
	"typeSwitchVar":     true,
	"underef":           true,
	"unlambda":          true,
}

// Analyzer runs the default go-critic checks.
var Analyzer = &analysis.Analyzer{
	Name: "gocritic",
	Doc:  "Provides diagnostics that check for bugs, performance and style issues.",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	ctx := linter.NewContext(pass.Fset, types.SizesFor("gc", runtime.GOARCH))
	ctx.SetPackageInfo(pass.TypesInfo, pass.Pkg)

	var checkers []*linter.Checker
	for _, info := range linter.GetCheckersInfo() {
		if !enabledChecks[info.Name] {
			continue
		}
		c, err := linter.NewChecker(ctx, info)
		if err != nil {
			return nil, fmt.Errorf("gocritic: %w", err)
		}
		checkers = append(checkers, c)
	}

	for _, f := range pass.Files {
		for _, c := range checkers {
			for _, warn := range c.Check(f) {
				pass.Report(analysis.Diagnostic{
					Pos:      warn.Pos,
					Category: c.Info.Name,
					Message:  fmt.Sprintf("%s: %s", c.Info.Name, warn.Text),
				})
			}
		}
	}
	return nil, nil
}
