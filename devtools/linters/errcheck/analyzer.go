// Package errcheck provides a go/analysis analyzer wrapping github.com/kisielk/errcheck
// with std-error-handling exclusions.
package errcheck

import (
	"fmt"
	"go/token"
	"regexp"

	upstream "github.com/kisielk/errcheck/errcheck"
	"golang.org/x/tools/go/analysis"
)

// stdErrorHandlingRegexp excludes common standard library error returns
// (stdout/stderr writes, Close/Flush, os.Remove, Printf/Println, Setenv).
var stdErrorHandlingRegexp = regexp.MustCompile(
	`(?i)^((os\.)?std(out|err)\..*|.*Close|.*Flush|os\.Remove(All)?|.*print(f|ln)?|os\.(Un)?Setenv)$`,
)

// Analyzer reports unchecked errors, excluding common standard library error
// returns.
var Analyzer = &analysis.Analyzer{
	Name: "errcheck",
	Doc:  "check for unchecked errors",
	Run:  run,
}

func run(pass *analysis.Pass) (any, error) {
	passCopy := *pass
	passCopy.Report = func(analysis.Diagnostic) {}

	res, err := upstream.Analyzer.Run(&passCopy)
	if err != nil {
		return nil, fmt.Errorf("errcheck: %w", err)
	}
	result, ok := res.(upstream.Result)
	if !ok {
		return nil, nil
	}

	fileByName := make(map[string]*token.File, len(pass.Files))
	for _, f := range pass.Files {
		if tf := pass.Fset.File(f.Pos()); tf != nil {
			fileByName[tf.Name()] = tf
		}
	}

	for _, ue := range result.UncheckedErrors {
		name := ue.SelectorName
		if name == "" {
			name = ue.FuncName
		}
		if name != "" && stdErrorHandlingRegexp.MatchString(name) {
			continue
		}

		pos := token.NoPos
		if tf := fileByName[ue.Pos.Filename]; tf != nil && ue.Pos.Offset >= 0 && ue.Pos.Offset <= tf.Size() {
			pos = tf.Pos(ue.Pos.Offset)
		}

		msg := "unchecked error"
		if name != "" {
			msg = fmt.Sprintf("Error return value of `%s` is not checked", name)
		}
		pass.Report(analysis.Diagnostic{
			Pos:      pos,
			Message:  msg,
			Category: "errcheck",
		})
	}
	return nil, nil
}
