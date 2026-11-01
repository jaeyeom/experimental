// Package pkgvar reports package-level variables that should be constants
// and package-level initializers that do work at import time.
//
// An unexported variable is reported when its initializer is a Go constant
// expression and the package neither assigns to it nor takes its address.
// Exported variables are not reported: another package can assign to them,
// and -ldflags -X requires a string variable.
//
// An initializer is reported when it contains a call or a channel receive,
// unless every such call is pure and at least one name keeps the result.
// A same-package function is pure when its body does not mutate package-level
// variables, take their addresses, start a goroutine, defer, send, or receive,
// and every call in that body is pure. An imported function is pure only when
// it is allowlisted. The default allowlist is errors.New, fmt.Errorf, and
// regexp.MustCompile. A listed function also requires pure arguments.
// Builtins len, cap, complex, real, imag, min, max, new, and make are pure.
// panic, print, and println are not. Method calls are never allowlisted.
// Calls inside a function literal count only when that literal is called.
package pkgvar

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

const (
	diagConst   = "unexported package-level variable can be a constant"
	diagDiscard = "package-level initializer discards its result"
	diagImpure  = "package-level initializer calls an impure function or receives from a channel"

	defaultAllowlist = "errors.New,fmt.Errorf,regexp.MustCompile"
)

// Analyzer reports package-level variables that should not be variables.
var Analyzer = newAnalyzer()

func newAnalyzer() *analysis.Analyzer {
	a := &analysis.Analyzer{
		Name:     "pkgvar",
		Doc:      "report package-level variables that should be constants or that run at import time",
		Requires: []*analysis.Analyzer{inspect.Analyzer},
		Run:      run,
	}
	a.Flags.String("allowlist", defaultAllowlist, "comma-separated import/path.Func calls that stay pure when every argument is pure")
	return a
}

func run(pass *analysis.Pass) (any, error) {
	allow, err := allowlist(pass)
	if err != nil {
		return nil, err
	}
	insp, ok := pass.ResultOf[inspect.Analyzer].(*inspector.Inspector)
	if !ok {
		return nil, fmt.Errorf("pkgvar: missing inspect result")
	}
	c := &checker{
		pass:      pass,
		allow:     allow,
		decls:     funcDecls(pass),
		memo:      map[*types.Func]bool{},
		visiting:  map[*types.Func]bool{},
		assigned:  map[*types.Var]bool{},
		addressed: map[*types.Var]bool{},
	}
	c.collectMutations()
	insp.WithStack([]ast.Node{(*ast.GenDecl)(nil)}, func(n ast.Node, push bool, stack []ast.Node) bool {
		if !push {
			return false
		}
		decl, ok := n.(*ast.GenDecl)
		if !ok || decl.Tok != token.VAR || !packageLevel(stack) {
			return false
		}
		for _, spec := range decl.Specs {
			value, ok := spec.(*ast.ValueSpec)
			if !ok {
				continue
			}
			c.reportSpec(value)
		}
		return false
	})
	return nil, nil
}

type effect int

const (
	effectNone effect = iota
	effectCall
	effectImpure
)

func (e effect) join(other effect) effect {
	if other > e {
		return other
	}
	return e
}

type checker struct {
	pass      *analysis.Pass
	allow     map[string]struct{}
	decls     map[*types.Func]*ast.FuncDecl
	memo      map[*types.Func]bool
	visiting  map[*types.Func]bool
	assigned  map[*types.Var]bool
	addressed map[*types.Var]bool
}

func (c *checker) reportSpec(spec *ast.ValueSpec) {
	c.reportConst(spec)
	eff := effectNone
	for _, value := range spec.Values {
		if isGoConst(c.pass.TypesInfo, value) {
			continue
		}
		eff = eff.join(c.exprEffect(value))
	}
	switch eff {
	case effectImpure:
		c.pass.Reportf(spec.Pos(), diagImpure)
	case effectCall:
		if !keepsResult(spec.Names) {
			c.pass.Reportf(spec.Pos(), diagDiscard)
		}
	}
}

func (c *checker) reportConst(spec *ast.ValueSpec) {
	if len(spec.Values) != len(spec.Names) {
		return
	}
	for i, name := range spec.Names {
		if !isGoConst(c.pass.TypesInfo, spec.Values[i]) {
			continue
		}
		obj, _ := c.pass.TypesInfo.Defs[name].(*types.Var)
		if obj == nil || obj.Parent() != c.pass.Pkg.Scope() || ast.IsExported(name.Name) {
			continue
		}
		if c.assigned[obj] || c.addressed[obj] {
			continue
		}
		c.pass.Reportf(name.Pos(), diagConst)
	}
}

func keepsResult(names []*ast.Ident) bool {
	for _, name := range names {
		if name.Name != "_" {
			return true
		}
	}
	return false
}

func (c *checker) exprEffect(root ast.Expr) effect {
	if root == nil || isGoConst(c.pass.TypesInfo, root) {
		return effectNone
	}
	eff := effectNone
	ast.Inspect(root, func(n ast.Node) bool {
		if n == nil || eff == effectImpure {
			return false
		}
		switch n := n.(type) {
		case *ast.FuncLit:
			return false
		case *ast.CallExpr:
			eff = eff.join(c.callEffect(n))
			return false
		case *ast.UnaryExpr:
			if n.Op == token.ARROW {
				eff = effectImpure
				return false
			}
		}
		return true
	})
	return eff
}

func (c *checker) callEffect(call *ast.CallExpr) effect {
	eff := effectNone
	for _, arg := range call.Args {
		eff = eff.join(c.exprEffect(arg))
	}
	if eff == effectImpure || c.conversion(call) {
		return eff
	}
	if !c.calleePure(call) {
		return effectImpure
	}
	return eff.join(effectCall)
}

func (c *checker) conversion(call *ast.CallExpr) bool {
	fun := ast.Unparen(call.Fun)
	tv, ok := c.pass.TypesInfo.Types[fun]
	if !ok {
		tv, ok = c.pass.TypesInfo.Types[call.Fun]
	}
	return ok && tv.IsType()
}

func (c *checker) calleePure(call *ast.CallExpr) bool {
	fun := ast.Unparen(call.Fun)
	if lit, ok := fun.(*ast.FuncLit); ok {
		return c.blockPure(lit.Body)
	}
	switch obj := objectOf(c.pass.TypesInfo, fun).(type) {
	case *types.Builtin:
		return pureBuiltin(obj.Name())
	case *types.Func:
		if obj.Pkg() == c.pass.Pkg {
			return c.funcPure(obj)
		}
		sig, _ := obj.Type().(*types.Signature)
		if sig != nil && sig.Recv() != nil {
			return false
		}
		if obj.Pkg() == nil {
			return pureBuiltin(obj.Name())
		}
		_, ok := c.allow[obj.Pkg().Path()+"."+obj.Name()]
		return ok
	default:
		return false
	}
}

func (c *checker) funcPure(fn *types.Func) bool {
	if pure, ok := c.memo[fn]; ok {
		return pure
	}
	if c.visiting[fn] {
		return true
	}
	decl := c.decls[fn]
	if decl == nil || decl.Body == nil {
		c.memo[fn] = false
		return false
	}
	c.visiting[fn] = true
	pure := c.blockPure(decl.Body)
	delete(c.visiting, fn)
	c.memo[fn] = pure
	return pure
}

func (c *checker) blockPure(body *ast.BlockStmt) bool {
	if body == nil {
		return false
	}
	pure := true
	ast.Inspect(body, func(n ast.Node) bool {
		if !pure || n == nil {
			return false
		}
		var descend bool
		pure, descend = c.stepPure(n)
		return pure && descend
	})
	return pure
}

// stepPure reports whether n keeps the enclosing function pure and whether
// its children still need to be walked. Function literals are skipped because
// an uncalled literal does not run.
func (c *checker) stepPure(n ast.Node) (bool, bool) {
	switch n := n.(type) {
	case *ast.FuncLit:
		return true, false
	case *ast.GoStmt, *ast.DeferStmt, *ast.SendStmt:
		return false, false
	case *ast.CallExpr:
		return c.callEffect(n) != effectImpure, false
	case *ast.UnaryExpr:
		if c.impureUnary(n) {
			return false, false
		}
		return true, true
	case *ast.AssignStmt:
		return !c.assignsPkg(n.Lhs), true
	case *ast.IncDecStmt:
		return c.pkgVar(n.X) == nil, true
	case *ast.RangeStmt:
		return !c.assignsPkg(rangeLHS(n)), true
	default:
		return true, true
	}
}

func (c *checker) impureUnary(n *ast.UnaryExpr) bool {
	if n.Op == token.ARROW {
		return true
	}
	return n.Op == token.AND && c.pkgVar(n.X) != nil
}

func (c *checker) collectMutations() {
	for _, file := range c.pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.AssignStmt:
				c.noteAssign(n.Lhs)
			case *ast.IncDecStmt:
				c.noteAssign([]ast.Expr{n.X})
			case *ast.RangeStmt:
				c.noteAssign(rangeLHS(n))
			case *ast.UnaryExpr:
				if n.Op == token.AND {
					if v := c.pkgVar(n.X); v != nil {
						c.addressed[v] = true
					}
				}
			}
			return true
		})
	}
}

func (c *checker) noteAssign(lhs []ast.Expr) {
	for _, expr := range lhs {
		if v := c.pkgVar(expr); v != nil {
			c.assigned[v] = true
		}
	}
}

func (c *checker) assignsPkg(lhs []ast.Expr) bool {
	for _, expr := range lhs {
		if c.pkgVar(expr) != nil {
			return true
		}
	}
	return false
}

func (c *checker) pkgVar(expr ast.Expr) *types.Var {
	var obj types.Object
	switch expr := ast.Unparen(expr).(type) {
	case *ast.Ident:
		obj = c.pass.TypesInfo.ObjectOf(expr)
	case *ast.SelectorExpr:
		obj = c.pass.TypesInfo.ObjectOf(expr.Sel)
	case *ast.IndexExpr:
		return c.pkgVar(expr.X)
	case *ast.SliceExpr:
		return c.pkgVar(expr.X)
	default:
		return nil
	}
	v, ok := obj.(*types.Var)
	if !ok || v.Parent() != c.pass.Pkg.Scope() {
		return nil
	}
	return v
}

func rangeLHS(stmt *ast.RangeStmt) []ast.Expr {
	var lhs []ast.Expr
	if stmt.Key != nil {
		lhs = append(lhs, stmt.Key)
	}
	if stmt.Value != nil {
		lhs = append(lhs, stmt.Value)
	}
	return lhs
}

func isGoConst(info *types.Info, expr ast.Expr) bool {
	if info == nil || expr == nil {
		return false
	}
	tv, ok := info.Types[expr]
	return ok && tv.Value != nil
}

func pureBuiltin(name string) bool {
	switch name {
	case "len", "cap", "complex", "real", "imag", "min", "max", "new", "make":
		return true
	default:
		return false
	}
}

func objectOf(info *types.Info, fun ast.Expr) types.Object {
	switch fun := ast.Unparen(fun).(type) {
	case *ast.Ident:
		return info.ObjectOf(fun)
	case *ast.SelectorExpr:
		return info.ObjectOf(fun.Sel)
	default:
		return nil
	}
}

func funcDecls(pass *analysis.Pass) map[*types.Func]*ast.FuncDecl {
	out := map[*types.Func]*ast.FuncDecl{}
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			fnDecl, ok := decl.(*ast.FuncDecl)
			if !ok || fnDecl.Name == nil {
				continue
			}
			fn, _ := pass.TypesInfo.Defs[fnDecl.Name].(*types.Func)
			if fn != nil {
				out[fn] = fnDecl
			}
		}
	}
	return out
}

func packageLevel(stack []ast.Node) bool {
	if len(stack) < 2 {
		return false
	}
	_, ok := stack[len(stack)-2].(*ast.File)
	return ok
}

func allowlist(pass *analysis.Pass) (map[string]struct{}, error) {
	raw := defaultAllowlist
	if flag := pass.Analyzer.Flags.Lookup("allowlist"); flag != nil {
		raw = flag.Value.String()
	}
	parsed, err := parseAllowlist(raw)
	if err != nil {
		return nil, fmt.Errorf("pkgvar: %w", err)
	}
	return parsed, nil
}

func parseAllowlist(raw string) (map[string]struct{}, error) {
	out := map[string]struct{}{}
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
	return !strings.Contains(name, "/") && token.IsIdentifier(name)
}
