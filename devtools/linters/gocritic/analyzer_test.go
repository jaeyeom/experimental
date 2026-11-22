package gocritic_test

import (
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/gocritic"
	"github.com/jaeyeom/experimental/devtools/linters/internal/analysistestenv"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	testdata := analysistestenv.Prepare(t, filepath.Join("devtools", "linters", "gocritic"), filepath.Join("src", "a", "a.go"))
	analysistest.Run(analysistestenv.Reporter{T: t}, testdata, gocritic.Analyzer, "a")
}
