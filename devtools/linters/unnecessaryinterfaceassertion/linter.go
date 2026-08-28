// Package unnecessaryinterfaceassertion flags compile-time interface
// assertions that are unnecessary because the interface has a single
// implementation and is not used elsewhere.
package unnecessaryinterfaceassertion

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/packages"
)

// ToolName is the command name reported on each issue.
const ToolName = "unnecessary-interface-assertion-linter"

// Issue is a finding in config.Issue JSON shape so devcheck can unmarshal it.
type Issue struct {
	FilePath string `json:"filePath"`
	Line     int    `json:"line"`
	Column   int    `json:"column"`
	Severity string `json:"severity"`
	Message  string `json:"message"`
	ToolName string `json:"toolName"`
}

// Linter flags unnecessary compile-time interface assertions.
type Linter struct {
	// Dir is the directory to use for loading packages. If empty, uses current directory.
	Dir string
}

// New creates a Linter.
func New() *Linter {
	return &Linter{}
}

// Lint runs the linter on the given paths.
func (l *Linter) Lint(paths []string) ([]Issue, error) {
	pkgs, err := l.loadPackages(paths)
	if err != nil {
		return nil, err
	}

	for _, pkg := range pkgs {
		for _, pkgErr := range pkg.Errors {
			return nil, fmt.Errorf("package %s has error: %w", pkg.PkgPath, pkgErr)
		}
	}

	interfaces, allNamedTypes := collectTypes(pkgs)
	implementations := findImplementations(interfaces, allNamedTypes)
	usages := collectInterfaceUsages(pkgs)

	return findUnnecessaryAssertions(pkgs, implementations, usages), nil
}

func (l *Linter) loadPackages(paths []string) ([]*packages.Package, error) {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedDeps | packages.NeedTypes | packages.NeedTypesSizes |
			packages.NeedSyntax | packages.NeedTypesInfo,
		Dir: l.Dir,
	}
	pkgs, err := packages.Load(cfg, paths...)
	if err != nil {
		return nil, fmt.Errorf("failed to load packages: %w", err)
	}
	return pkgs, nil
}

func collectTypes(pkgs []*packages.Package) ([]*types.TypeName, []*types.Named) {
	var interfaces []*types.TypeName
	var allNamedTypes []*types.Named

	for _, pkg := range pkgs {
		if pkg.Types == nil {
			continue
		}
		scope := pkg.Types.Scope()
		for _, name := range scope.Names() {
			obj := scope.Lookup(name)
			tn, ok := obj.(*types.TypeName)
			if !ok || tn.IsAlias() {
				continue
			}
			named, ok := tn.Type().(*types.Named)
			if !ok {
				continue
			}
			allNamedTypes = append(allNamedTypes, named)
			if _, ok := named.Underlying().(*types.Interface); ok {
				interfaces = append(interfaces, tn)
			}
		}
	}
	return interfaces, allNamedTypes
}

func findImplementations(interfaces []*types.TypeName, allNamedTypes []*types.Named) map[*types.TypeName][]types.Type {
	implementations := make(map[*types.TypeName][]types.Type)

	for _, typ := range allNamedTypes {
		for _, ifaceTypeName := range interfaces {
			if typ.Obj() == ifaceTypeName {
				continue
			}
			iface, ok := ifaceTypeName.Type().Underlying().(*types.Interface)
			if !ok {
				continue
			}
			if types.Implements(typ, iface) {
				implementations[ifaceTypeName] = append(implementations[ifaceTypeName], typ)
			}
			addPointerImpl(implementations, ifaceTypeName, iface, typ)
		}
	}
	return implementations
}

func addPointerImpl(implementations map[*types.TypeName][]types.Type, ifaceTypeName *types.TypeName, iface *types.Interface, typ *types.Named) {
	ptr := types.NewPointer(typ)
	if !types.Implements(ptr, iface) {
		return
	}
	for _, existing := range implementations[ifaceTypeName] {
		if types.Identical(existing, ptr) {
			return
		}
	}
	implementations[ifaceTypeName] = append(implementations[ifaceTypeName], ptr)
}

func collectInterfaceUsages(pkgs []*packages.Package) map[types.Object][]token.Pos {
	usages := make(map[types.Object][]token.Pos)
	for _, pkg := range pkgs {
		info := pkg.TypesInfo
		if info == nil {
			continue
		}
		for id, obj := range info.Uses {
			tn, ok := obj.(*types.TypeName)
			if !ok || tn.IsAlias() {
				continue
			}
			if _, ok := tn.Type().Underlying().(*types.Interface); !ok {
				continue
			}
			usages[obj] = append(usages[obj], id.Pos())
		}
	}
	return usages
}

func findUnnecessaryAssertions(pkgs []*packages.Package, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos) []Issue {
	var issues []Issue
	for _, pkg := range pkgs {
		info := pkg.TypesInfo
		if info == nil {
			continue
		}
		for _, file := range pkg.Syntax {
			issues = append(issues, findAssertionsInFile(pkg, file, info, implementations, usages)...)
		}
	}
	return issues
}

func findAssertionsInFile(pkg *packages.Package, file *ast.File, info *types.Info, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos) []Issue {
	var issues []Issue
	ast.Inspect(file, func(n ast.Node) bool {
		decl, ok := n.(*ast.GenDecl)
		if !ok || decl.Tok != token.VAR {
			return true
		}
		for _, spec := range decl.Specs {
			if issue := checkVarSpec(pkg, spec, info, implementations, usages, n); issue != nil {
				issues = append(issues, *issue)
			}
		}
		return true
	})
	return issues
}

func checkVarSpec(pkg *packages.Package, spec ast.Spec, info *types.Info, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos, n ast.Node) *Issue {
	valueSpec, ok := spec.(*ast.ValueSpec)
	if !ok {
		return nil
	}
	if !isBlankIdentifier(valueSpec) {
		return nil
	}
	if valueSpec.Type == nil {
		return nil
	}

	typeName := getTypeName(info, valueSpec.Type)
	if typeName == nil {
		return nil
	}

	// Heuristic 1: The interface has only one implementation.
	if len(implementations[typeName]) != 1 {
		return nil
	}

	// Heuristic 2: The interface is only used in this assertion.
	typeObj := getTypeObject(info, valueSpec.Type)
	if typeObj == nil {
		return nil
	}
	if len(usages[typeObj]) > 1 {
		return nil
	}

	pos := pkg.Fset.Position(n.Pos())
	return &Issue{
		FilePath: pos.Filename,
		Line:     pos.Line,
		Column:   pos.Column,
		Severity: "warning",
		Message:  "Unnecessary interface assertion",
		ToolName: ToolName,
	}
}

func isBlankIdentifier(valueSpec *ast.ValueSpec) bool {
	return len(valueSpec.Names) == 1 && valueSpec.Names[0].Name == "_"
}

func getTypeName(info *types.Info, typeExpr ast.Expr) *types.TypeName {
	obj := info.TypeOf(typeExpr)
	if obj == nil {
		return nil
	}
	named, ok := obj.(*types.Named)
	if !ok {
		return nil
	}
	return named.Obj()
}

func getTypeObject(info *types.Info, typeExpr ast.Expr) types.Object {
	var typeIdent *ast.Ident
	switch t := typeExpr.(type) {
	case *ast.Ident:
		typeIdent = t
	case *ast.SelectorExpr:
		typeIdent = t.Sel
	}
	if typeIdent == nil {
		return nil
	}
	typeObj := info.Uses[typeIdent]
	if typeObj == nil {
		typeObj = info.Defs[typeIdent]
	}
	return typeObj
}
