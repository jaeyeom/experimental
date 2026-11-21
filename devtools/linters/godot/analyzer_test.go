package godot_test

import (
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/linters/godot"
	"github.com/jaeyeom/experimental/devtools/linters/internal/analysistestenv"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	testdata := analysistestenv.Prepare(t, filepath.Join("devtools", "linters", "godot"), filepath.Join("src", "a", "a.go"))
	analysistest.Run(analysistestenv.Reporter{T: t}, testdata, godot.Analyzer, "a")
}
