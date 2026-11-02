// Package pkgvar reports package-level variables that should be constants
// and package-level initializers that do work at import time.
//
// A package-level variable, exported or not, is reported when its initializer
// is a Go constant expression and the package neither assigns to it nor takes
// its address. An assignment or address-take in another package still leaves
// the finding. A string kept for -ldflags -X is suppressed on that declaration
// with a nolint comment and a reason.
//
// An initializer is reported when it contains a call or a channel receive,
// unless every such call is pure and at least one name keeps the result.
// A same-package function is pure when its body does not mutate package-level
// variables, take their addresses, start a goroutine, defer, send, or receive,
// and every call in that body is pure. A pointer-receiver method or method
// value takes the address of the variable it is selected from. Selector,
// pointer, index, and slice expressions are followed to that variable.
// Ranging over a channel receives from it. Ranging over an iterator calls it.
// An imported function is pure only when it is allowlisted. The default
// allowlist is errors.New, fmt.Errorf, and regexp.MustCompile. A listed
// function also requires pure arguments.
// Builtins len, cap, complex, real, imag, min, max, new, and make are pure.
// panic, print, and println are not. Method calls are never allowlisted.
// Calls inside a function literal count only when that literal is called.
package pkgvar

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"regexp"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/passes/inspect"
	"golang.org/x/tools/go/ast/inspector"
	"golang.org/x/tools/go/types/typeutil"
)

const (
	diagConst   = "package-level variable can be a constant"
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
	// optHits counts optimistic results for functions still on the visiting stack.
	optHits int
}

func (c *checker) reportSpec(spec *ast.ValueSpec) {
	if specSuppressed(spec) {
		return
	}
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

var (
	nolintLinters = regexp.MustCompile(`^//\s*nolint:([A-Za-z0-9_,-]+)`)
	nolintReason  = regexp.MustCompile(`\s//\s*(\S.*)`)
	wantMarker    = regexp.MustCompile(`^want\b`)
)

// specSuppressed reports whether spec.Comment suppresses pkgvar.
// A trailing comment is one Go comment: the second // is text, not another
// comment. spec.Doc and comments on any other line are ignored. A reason
// whose first word is the analysistest marker want does not suppress.
// wanted does, because the word boundary does not split it.
func specSuppressed(spec *ast.ValueSpec) bool {
	if spec.Comment == nil {
		return false
	}
	for _, comment := range spec.Comment.List {
		if commentSuppressesPkgvar(comment.Text) {
			return true
		}
	}
	return false
}

func commentSuppressesPkgvar(text string) bool {
	loc := nolintLinters.FindStringSubmatchIndex(text)
	if loc == nil {
		return false
	}
	listed := false
	for _, part := range strings.Split(text[loc[2]:loc[3]], ",") {
		if strings.TrimSpace(part) == "pkgvar" {
			listed = true
			break
		}
	}
	if !listed {
		return false
	}
	reason := nolintReason.FindStringSubmatch(text[loc[1]:])
	if reason == nil {
		return false
	}
	return !wantMarker.MatchString(reason[1])
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
		if obj == nil || obj.Parent() != c.pass.Pkg.Scope() {
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
	return c.walkEffect(root, false)
}

// argEffect is exprEffect for a call argument. A pointer-receiver method
// value, or an address of a package-level variable, makes that argument
// impure. The same expression as a whole initializer is not a call, so
// exprEffect leaves it alone.
func (c *checker) argEffect(root ast.Expr) effect {
	return c.walkEffect(root, true)
}

func (c *checker) walkEffect(root ast.Expr, addr bool) effect {
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
			if c.unaryImpure(n, addr) {
				eff = effectImpure
				return false
			}
		case *ast.SelectorExpr:
			if addr && c.methodAddrVar(n) != nil {
				eff = effectImpure
				return false
			}
		}
		return true
	})
	return eff
}

func (c *checker) unaryImpure(n *ast.UnaryExpr, addr bool) bool {
	if n.Op == token.ARROW {
		return true
	}
	return addr && n.Op == token.AND && c.rootPkgVar(n.X) != nil
}

func (c *checker) callEffect(call *ast.CallExpr) effect {
	eff := c.funValueEffect(call.Fun)
	if c.methodAddrVar(call.Fun) != nil {
		eff = effectImpure
	}
	for _, arg := range call.Args {
		eff = eff.join(c.argEffect(arg))
	}
	if eff == effectImpure || c.conversion(call) {
		return eff
	}
	if !c.calleePure(call) {
		return effectImpure
	}
	return eff.join(effectCall)
}

// funValueEffect is the effect of evaluating the callee expression, not of
// invoking it. A pure method does not hide an impure receiver or index.
func (c *checker) funValueEffect(fun ast.Expr) effect {
	switch fun := ast.Unparen(fun).(type) {
	case *ast.Ident, *ast.FuncLit:
		return effectNone
	case *ast.SelectorExpr:
		return c.exprEffect(fun.X)
	case *ast.IndexExpr:
		eff := c.funValueEffect(fun.X)
		if !isType(c.pass.TypesInfo, fun.Index) {
			eff = eff.join(c.exprEffect(fun.Index))
		}
		return eff
	case *ast.IndexListExpr:
		return c.funValueEffect(fun.X)
	default:
		return c.exprEffect(fun)
	}
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
	if lit, ok := ast.Unparen(call.Fun).(*ast.FuncLit); ok {
		return c.blockPure(lit.Body)
	}
	switch obj := typeutil.Callee(c.pass.TypesInfo, call).(type) {
	case *types.Builtin:
		return pureBuiltin(obj.Name())
	case *types.Func:
		fn := obj.Origin()
		if fn.Pkg() == c.pass.Pkg {
			return c.funcPure(fn)
		}
		sig, _ := fn.Type().(*types.Signature)
		if sig != nil && sig.Recv() != nil {
			return false
		}
		if fn.Pkg() == nil {
			return pureBuiltin(fn.Name())
		}
		_, ok := c.allow[fn.Pkg().Path()+"."+fn.Name()]
		return ok
	default:
		return false
	}
}

func (c *checker) funcPure(fn *types.Func) bool {
	fn = fn.Origin()
	if pure, ok := c.memo[fn]; ok {
		return pure
	}
	if c.visiting[fn] {
		c.optHits++
		return true
	}
	decl := c.decls[fn]
	if decl == nil || decl.Body == nil {
		c.memo[fn] = false
		return false
	}
	c.visiting[fn] = true
	hits := c.optHits
	pure := c.blockPure(decl.Body)
	delete(c.visiting, fn)
	if !pure {
		c.memo[fn] = false
		return false
	}
	// A pure answer that used a function still on the stack is incomplete.
	// Leave it unmemoized so a later impure result can be seen. Memoize when
	// the cycle has finished and nothing impure was found.
	if c.optHits > hits && len(c.visiting) > 0 {
		return true
	}
	c.memo[fn] = true
	return true
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
		return c.rootPkgVar(n.X) == nil, true
	case *ast.SelectorExpr:
		if c.methodAddrVar(n) != nil {
			return false, false
		}
		return true, true
	case *ast.RangeStmt:
		if c.rangeImpure(n) {
			return false, false
		}
		return true, true
	default:
		return true, true
	}
}

func (c *checker) impureUnary(n *ast.UnaryExpr) bool {
	if n.Op == token.ARROW {
		return true
	}
	return n.Op == token.AND && c.rootPkgVar(n.X) != nil
}

func (c *checker) rangeImpure(stmt *ast.RangeStmt) bool {
	if c.assignsPkg(rangeLHS(stmt)) {
		return true
	}
	tv, ok := c.pass.TypesInfo.Types[stmt.X]
	if !ok || tv.Type == nil {
		return false
	}
	switch t := tv.Type.Underlying().(type) {
	case *types.Chan:
		return true
	case *types.Signature:
		return isIterator(t) && c.iterEffect(stmt.X) == effectImpure
	default:
		return false
	}
}

func (c *checker) iterEffect(expr ast.Expr) effect {
	if lit, ok := ast.Unparen(expr).(*ast.FuncLit); ok {
		if c.blockPure(lit.Body) {
			return effectCall
		}
		return effectImpure
	}
	fn := funcObject(c.pass.TypesInfo, expr)
	if fn != nil && c.funcPure(fn) {
		return effectCall
	}
	return effectImpure
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
					c.noteAddress(n.X)
				}
			case *ast.SelectorExpr:
				if v := c.methodAddrVar(n); v != nil && v.Pkg() == c.pass.Pkg {
					c.addressed[v] = true
				}
			}
			return true
		})
	}
}

// noteAddress records an explicit address-take of a variable in this package.
func (c *checker) noteAddress(expr ast.Expr) {
	v := c.rootPkgVar(expr)
	if v != nil && v.Pkg() == c.pass.Pkg {
		c.addressed[v] = true
	}
}

// methodAddrVar is the package-level variable a pointer-receiver method or
// method value is selected from. Selection.Indirect is false for an
// addressable value whose address is taken implicitly, so the method's
// receiver type is what decides. Value receivers and locals return nil.
func (c *checker) methodAddrVar(expr ast.Expr) *types.Var {
	sel, ok := ast.Unparen(expr).(*ast.SelectorExpr)
	if !ok {
		return nil
	}
	selection := c.pass.TypesInfo.Selections[sel]
	if selection == nil || !pointerRecv(selection) {
		return nil
	}
	switch selection.Kind() {
	case types.MethodVal, types.MethodExpr:
		return c.rootPkgVar(sel.X)
	default:
		return nil
	}
}

func pointerRecv(selection *types.Selection) bool {
	fn, ok := selection.Obj().(*types.Func)
	if !ok {
		return false
	}
	sig, ok := fn.Type().(*types.Signature)
	if !ok || sig.Recv() == nil {
		return false
	}
	_, isPtr := sig.Recv().Type().Underlying().(*types.Pointer)
	return isPtr
}

func (c *checker) noteAssign(lhs []ast.Expr) {
	for _, expr := range lhs {
		v := c.rootPkgVar(expr)
		if v != nil && v.Pkg() == c.pass.Pkg {
			c.assigned[v] = true
		}
	}
}

func (c *checker) assignsPkg(lhs []ast.Expr) bool {
	for _, expr := range lhs {
		if c.rootPkgVar(expr) != nil {
			return true
		}
	}
	return false
}

// rootPkgVar follows selectors, pointers, indexes, and slices to a
// package-level variable. The variable may belong to any package: purity
// cares about every package variable, while the constant rule records only
// variables declared in the package under analysis.
func (c *checker) rootPkgVar(expr ast.Expr) *types.Var {
	switch expr := ast.Unparen(expr).(type) {
	case *ast.Ident:
		return pkgLevelVar(c.pass.TypesInfo.ObjectOf(expr))
	case *ast.SelectorExpr:
		if v := pkgLevelVar(c.pass.TypesInfo.ObjectOf(expr.Sel)); v != nil {
			return v
		}
		return c.rootPkgVar(expr.X)
	case *ast.StarExpr:
		return c.rootPkgVar(expr.X)
	case *ast.IndexExpr:
		return c.rootPkgVar(expr.X)
	case *ast.IndexListExpr:
		return c.rootPkgVar(expr.X)
	case *ast.SliceExpr:
		return c.rootPkgVar(expr.X)
	default:
		return nil
	}
}

func pkgLevelVar(obj types.Object) *types.Var {
	v, ok := obj.(*types.Var)
	if !ok || v.Pkg() == nil || v.Parent() != v.Pkg().Scope() {
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

func funcObject(info *types.Info, expr ast.Expr) *types.Func {
	switch expr := ast.Unparen(expr).(type) {
	case *ast.Ident:
		fn, _ := info.ObjectOf(expr).(*types.Func)
		return fn
	case *ast.SelectorExpr:
		fn, _ := info.ObjectOf(expr.Sel).(*types.Func)
		return fn
	case *ast.IndexExpr:
		if isType(info, expr.Index) {
			return funcObject(info, expr.X)
		}
	case *ast.IndexListExpr:
		return funcObject(info, expr.X)
	}
	return nil
}

func isType(info *types.Info, expr ast.Expr) bool {
	tv, ok := info.Types[expr]
	return ok && tv.IsType()
}

// isIterator reports the Go range-over-function shape: one yield parameter
// of func() bool, func(V) bool, or func(K, V) bool, and no results.
func isIterator(sig *types.Signature) bool {
	if sig.Params().Len() != 1 || sig.Results().Len() != 0 {
		return false
	}
	yield, ok := sig.Params().At(0).Type().Underlying().(*types.Signature)
	if !ok || yield.Results().Len() != 1 {
		return false
	}
	basic, ok := yield.Results().At(0).Type().Underlying().(*types.Basic)
	if !ok || basic.Kind() != types.Bool {
		return false
	}
	return yield.Params().Len() <= 2
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
				out[fn.Origin()] = fnDecl
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
