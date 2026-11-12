package gh

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"
)

// knownAPIWrites are the `gh api` endpoints this package writes to with
// -f/-F fields and the default POST. There are none today.
var knownAPIWrites = []string{}

// TestAPIFieldCallsNameMethod guards against `gh api ENDPOINT -f k=v`
// meant as a read: with fields and no --method, gh sends a POST, which a
// GET-only endpoint such as search/issues answers with 404. Every argument
// list in this package that holds "api" and -f/-F must be `api graphql`,
// name a method, or be a known write.
func TestAPIFieldCallsNameMethod(t *testing.T) {
	t.Parallel()

	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	fset := token.NewFileSet()
	checked := 0
	for _, name := range files {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(fset, name, nil, 0)
		if err != nil {
			t.Fatal(err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			var args []ast.Expr
			switch node := n.(type) {
			case *ast.CallExpr:
				args = node.Args
			case *ast.CompositeLit:
				args = node.Elts
			default:
				return true
			}
			lits := stringLits(args)
			hasFields := slices.Contains(lits, "-f") || slices.Contains(lits, "-F")
			if !hasFields || !slices.Contains(lits, "api") {
				return true
			}
			checked++
			if slices.Contains(lits, "graphql") || slices.Contains(lits, "--method") || slices.Contains(lits, "-X") {
				return true
			}
			for _, endpoint := range knownAPIWrites {
				if slices.Contains(lits, endpoint) {
					return true
				}
			}
			t.Errorf("%s: gh api with -f/-F and no --method sends a POST; add --method GET for a read", fset.Position(n.Pos()))
			return true
		})
	}
	if checked == 0 {
		t.Fatal("found no gh api call with -f/-F; the check is not looking at the sources")
	}
}

func stringLits(exprs []ast.Expr) []string {
	var out []string
	for _, expr := range exprs {
		lit, ok := expr.(*ast.BasicLit)
		if !ok || lit.Kind != token.STRING {
			continue
		}
		s, err := strconv.Unquote(lit.Value)
		if err != nil {
			continue
		}
		out = append(out, s)
	}
	return out
}
