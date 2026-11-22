package wrapcheck_test

import (
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/internal/analysistestenv"
	"github.com/jaeyeom/experimental/devtools/linters/wrapcheck"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	testdata := analysistestenv.Prepare(t, filepath.Join("devtools", "linters", "wrapcheck"), filepath.Join("src", "a", "a.go"))
	analysistest.Run(analysistestenv.Reporter{T: t}, testdata, wrapcheck.Analyzer, "a")
}
