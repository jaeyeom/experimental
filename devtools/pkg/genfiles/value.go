package genfiles

import (
	"path"
	"strings"
)

// VarNames are the variables a value can take from a matched file.
var VarNames = []string{"path", "dir", "name", "stem"}

// Value is one argument, working directory or environment value. Exactly one
// form is set: a literal (IsLit), a variable (Var) or a concatenation (Join).
// A literal is used byte for byte and is never parsed or interpreted.
type Value struct {
	Lit   string
	IsLit bool
	Var   string
	Join  []Value
}

// Expand returns v's text for the matched file at path (repo-relative,
// /-separated). The result is a single argument; nothing splits or
// interprets it.
func (v Value) Expand(path string) string {
	switch {
	case v.IsLit:
		return v.Lit
	case v.Var != "":
		return Vars(path)[v.Var]
	}
	var b strings.Builder
	for _, part := range v.Join {
		b.WriteString(part.Expand(path))
	}
	return b.String()
}

// Vars returns the variables for the matched file at p: path (p itself), dir
// (its directory, "." at the root), name (its base name) and stem (the name
// minus its last extension; a name that is all extension, like .bashrc, is
// its own stem).
func Vars(p string) map[string]string {
	name := path.Base(p)
	stem := name
	if ext := path.Ext(name); ext != name {
		stem = strings.TrimSuffix(name, ext)
	}
	return map[string]string{
		"path": p,
		"dir":  path.Dir(p),
		"name": name,
		"stem": stem,
	}
}
