// Package detector provides the main project detection functionality.
package detector

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/packages"
)

// UnnecessaryInterfaceAssertionLinter lints for unnecessary interface assertions.
type UnnecessaryInterfaceAssertionLinter struct {
	// Dir is the directory to use for loading packages. If empty, uses current directory.
	Dir string
}

// NewUnnecessaryInterfaceAssertionLinter creates a new UnnecessaryInterfaceAssertionLinter.
func NewUnnecessaryInterfaceAssertionLinter() *UnnecessaryInterfaceAssertionLinter {
	return &UnnecessaryInterfaceAssertionLinter{}
}

// Lint runs the linter on the given paths.
func (l *UnnecessaryInterfaceAssertionLinter) Lint(paths []string) ([]string, error) {
	pkgs, err := l.loadPackages(paths)
	if err != nil {
		return nil, err
	}

	// Check for package errors.
	for _, pkg := range pkgs {
		for _, err := range pkg.Errors {
			return nil, fmt.Errorf("package %s has error: %v", pkg.PkgPath, err)
		}
	}

	interfaces, allNamedTypes := collectTypes(pkgs)
	implementations := findImplementations(interfaces, allNamedTypes)
	usages := collectInterfaceUsages(pkgs)

	return findUnnecessaryAssertions(pkgs, implementations, usages), nil
}

func (l *UnnecessaryInterfaceAssertionLinter) loadPackages(paths []string) ([]*packages.Package, error) {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedTypes | packages.NeedTypesSizes |
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
			// Skip if this is the interface type itself.
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

func findUnnecessaryAssertions(pkgs []*packages.Package, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos) []string {
	var issues []string
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

func findAssertionsInFile(pkg *packages.Package, file *ast.File, info *types.Info, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos) []string {
	var issues []string
	ast.Inspect(file, func(n ast.Node) bool {
		decl, ok := n.(*ast.GenDecl)
		if !ok || decl.Tok != token.VAR {
			return true
		}
		for _, spec := range decl.Specs {
			if issue := checkVarSpec(pkg, spec, info, implementations, usages, n); issue != "" {
				issues = append(issues, issue)
			}
		}
		return true
	})
	return issues
}

func checkVarSpec(pkg *packages.Package, spec ast.Spec, info *types.Info, implementations map[*types.TypeName][]types.Type, usages map[types.Object][]token.Pos, n ast.Node) string {
	valueSpec, ok := spec.(*ast.ValueSpec)
	if !ok {
		return ""
	}
	if !isBlankIdentifier(valueSpec) {
		return ""
	}
	if valueSpec.Type == nil {
		return ""
	}

	typeName := getTypeName(info, valueSpec.Type)
	if typeName == nil {
		return ""
	}

	// Heuristic 1: The interface has only one implementation.
	if len(implementations[typeName]) != 1 {
		return ""
	}

	// Heuristic 2: The interface is only used in this assertion.
	typeObj := getTypeObject(info, valueSpec.Type)
	if typeObj == nil {
		return ""
	}
	if len(usages[typeObj]) > 1 {
		return ""
	}

	return fmt.Sprintf("Unnecessary interface assertion at %s", pkg.Fset.Position(n.Pos()))
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
