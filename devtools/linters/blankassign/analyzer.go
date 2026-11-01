// Package blankassign reports package-level blank assignments whose
// initializers are effectful.
//
// A value spec is reported when every name is "_", an initializer is
// present, and that initializer contains a function call or a channel
// receive. Type conversions are not calls. Calls inside a function literal
// are not counted: that body does not run while the initializer is evaluated.
//
// The -allowlist flag names package functions as import/path.Func. It
// defaults to empty. A listed function is ignored only when every argument
// is a constant value or an identifier bound to a const. Method calls are
// never ignored. Callees are matched with type information, so an import
// alias still matches.
package blankassign

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/inspect"
	"golang.org/x/tools/go/ast/inspector"
)

// diagnostic is the message reported on each effectful blank value spec.
const diagnostic = "package-level blank assignment is effectful"

// Analyzer reports package-level blank assignments with effectful initializers.
var Analyzer = newAnalyzer()

func newAnalyzer() *analysis.Analyzer {
	a := &analysis.Analyzer{
		Name:     "blankassign",
		Doc:      "report package-level blank assignments with effectful initializers",
		Requires: []*analysis.Analyzer{inspect.Analyzer},
		Run:      run,
	}
	a.Flags.String("allowlist", "", "comma-separated import/path.Func calls to ignore when every argument is constant")
	return a
}

func run(pass *analysis.Pass) (any, error) {
	allow, err := allowlist(pass)
	if err != nil {
		return nil, err
	}
	insp, ok := pass.ResultOf[inspect.Analyzer].(*inspector.Inspector)
	if !ok {
		return nil, fmt.Errorf("blankassign: missing inspect result")
	}
	insp.WithStack([]ast.Node{(*ast.GenDecl)(nil)}, func(n ast.Node, push bool, stack []ast.Node) bool {
		if !push {
			return false
		}
		decl, ok := n.(*ast.GenDecl)
		if !ok || decl.Tok != token.VAR || !packageLevel(stack) {
			return false
		}
		for _, spec := range decl.Specs {
			reportSpec(pass, spec, allow)
		}
		return false
	})
	return nil, nil
}

func allowlist(pass *analysis.Pass) (map[string]struct{}, error) {
	raw := ""
	if flag := pass.Analyzer.Flags.Lookup("allowlist"); flag != nil {
		raw = flag.Value.String()
	}
	parsed, err := parseAllowlist(raw)
	if err != nil {
		return nil, fmt.Errorf("blankassign: %w", err)
	}
	return parsed, nil
}

func parseAllowlist(raw string) (map[string]struct{}, error) {
	out := make(map[string]struct{})
	if strings.TrimSpace(raw) == "" {
		return out, nil
	}
	for _, part := range strings.Split(raw, ",") {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		if !validAllowlistEntry(part) {
			return nil, fmt.Errorf("invalid allowlist entry %q", part)
		}
		out[part] = struct{}{}
	}
	return out, nil
}

func validAllowlistEntry(entry string) bool {
	dot := strings.LastIndex(entry, ".")
	if dot <= 0 || dot == len(entry)-1 {
		return false
	}
	name := entry[dot+1:]
	if strings.Contains(name, "/") || !token.IsIdentifier(name) {
		return false
	}
	return true
}

func packageLevel(stack []ast.Node) bool {
	if len(stack) < 2 {
		return false
	}
	_, ok := stack[len(stack)-2].(*ast.File)
	return ok
}

func reportSpec(pass *analysis.Pass, spec ast.Spec, allow map[string]struct{}) {
	value, ok := spec.(*ast.ValueSpec)
	if !ok || !allBlank(value.Names) || len(value.Values) == 0 {
		return
	}
	if !effectful(pass.TypesInfo, value.Values, allow) {
		return
	}
	pass.Reportf(value.Pos(), diagnostic)
}

func allBlank(names []*ast.Ident) bool {
	if len(names) == 0 {
		return false
	}
	for _, name := range names {
		if name.Name != "_" {
			return false
		}
	}
	return true
}

func effectful(info *types.Info, values []ast.Expr, allow map[string]struct{}) bool {
	if info == nil {
		return false
	}
	for _, value := range values {
		if exprEffectful(info, value, allow) {
			return true
		}
	}
	return false
}

func exprEffectful(info *types.Info, root ast.Expr, allow map[string]struct{}) bool {
	found := false
	ast.Inspect(root, func(n ast.Node) bool {
		if found || n == nil {
			return false
		}
		switch n := n.(type) {
		case *ast.FuncLit:
			// The literal is a value. Calls in its body do not run yet.
			return false
		case *ast.CallExpr:
			if conversion(info, n) {
				return true
			}
			if quietAllowlisted(info, n, allow) {
				return false
			}
			found = true
			return false
		case *ast.UnaryExpr:
			if n.Op == token.ARROW {
				found = true
				return false
			}
		}
		return true
	})
	return found
}

func conversion(info *types.Info, call *ast.CallExpr) bool {
	fun := ast.Unparen(call.Fun)
	tv, ok := info.Types[fun]
	if !ok {
		tv, ok = info.Types[call.Fun]
	}
	return ok && tv.IsType()
}

func quietAllowlisted(info *types.Info, call *ast.CallExpr, allow map[string]struct{}) bool {
	if len(allow) == 0 {
		return false
	}
	fn := calledFunc(info, call)
	if fn == nil || fn.Pkg() == nil {
		return false
	}
	sig, ok := fn.Type().(*types.Signature)
	if !ok || sig.Recv() != nil {
		return false
	}
	if _, ok := allow[fn.Pkg().Path()+"."+fn.Name()]; !ok {
		return false
	}
	return constArgs(info, call)
}

func calledFunc(info *types.Info, call *ast.CallExpr) *types.Func {
	switch fun := ast.Unparen(call.Fun).(type) {
	case *ast.Ident:
		fn, _ := info.ObjectOf(fun).(*types.Func)
		return fn
	case *ast.SelectorExpr:
		fn, _ := info.ObjectOf(fun.Sel).(*types.Func)
		return fn
	default:
		return nil
	}
}

func constArgs(info *types.Info, call *ast.CallExpr) bool {
	for _, arg := range call.Args {
		if !constArg(info, arg) {
			return false
		}
	}
	return true
}

func constArg(info *types.Info, expr ast.Expr) bool {
	expr = ast.Unparen(expr)
	if tv, ok := info.Types[expr]; ok && tv.Value != nil {
		return true
	}
	ident, ok := expr.(*ast.Ident)
	if !ok {
		return false
	}
	_, ok = info.ObjectOf(ident).(*types.Const)
	return ok
}
