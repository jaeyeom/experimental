package genfiles

import (
	"errors"
	"fmt"
	"path"
	"regexp"
	"slices"
	"strings"

	"gopkg.in/yaml.v3"
)

// Entry is one entry of a generated-files list.
type Entry struct {
	Pattern string
	Comment string
	// Run holds the commands that regenerate the files, run in order with no
	// shell. Nil when the files have no automated command.
	Run [][]Value
	// Dir is the working directory, repo-relative. Nil means the repo root.
	Dir *Value
	Env []EnvVar
	// Line is the entry's line in the list file.
	Line int
}

// EnvVar is one environment variable added to the commands' environment.
type EnvVar struct {
	Name  string
	Value Value
}

var (
	entryKeys = []string{"pattern", "comment", "run", "dir", "env"}
	shells    = []string{"sh", "bash", "zsh", "dash", "ksh"}
	cFlagRE   = regexp.MustCompile(`^-[A-Za-z]*c[A-Za-z]*$`)
	envNameR  = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)
)

// Parse reads a generated-files list strictly: an unknown key, a value of the
// wrong YAML type, an alias, a custom tag, an invalid pattern or an unsafe
// command is an error. Nothing is defaulted. All problems found are returned
// joined, each prefixed with its line.
func Parse(data []byte) ([]Entry, error) {
	seq, err := topSequence(data)
	if err != nil {
		return nil, err
	}
	p := &parser{}
	seen := map[string]bool{}
	var entries []Entry
	for i, n := range seq.Content {
		p.entry = i + 1
		e, ok := p.parseEntry(n)
		if !ok {
			continue
		}
		if seen[e.Pattern] {
			p.errorf(n, "duplicate pattern %q", e.Pattern)
			continue
		}
		seen[e.Pattern] = true
		entries = append(entries, e)
	}
	if len(p.errs) > 0 {
		return nil, errors.Join(p.errs...)
	}
	return entries, nil
}

// ParsePatterns reads only the patterns of a generated-files list, ignoring
// every other key. Each entry still needs a valid string pattern. It is for
// consumers that only match paths and must keep working with lists written
// in an older or newer shape.
func ParsePatterns(data []byte) ([]string, error) {
	if empty, err := emptyDocument(data); err != nil || empty {
		return []string{}, err
	}
	seq, err := topSequence(data)
	if err != nil {
		return nil, err
	}
	p := &parser{}
	patterns := make([]string, 0, len(seq.Content))
	for i, n := range seq.Content {
		p.entry = i + 1
		if n.Kind != yaml.MappingNode {
			p.errorf(n, "expected a mapping, found %s", kindName(n))
			continue
		}
		var pn *yaml.Node
		dup := false
		for j := 0; j+1 < len(n.Content); j += 2 {
			if n.Content[j].Value != "pattern" {
				continue
			}
			if pn != nil {
				p.errorf(n.Content[j], "duplicate key %q", "pattern")
				dup = true
			}
			pn = n.Content[j+1]
		}
		if dup {
			continue
		}
		if pn == nil {
			p.errorf(n, "missing required key %q", "pattern")
			continue
		}
		if pat, ok := p.pattern(pn); ok {
			patterns = append(patterns, pat)
		}
	}
	if len(p.errs) > 0 {
		return nil, errors.Join(p.errs...)
	}
	return patterns, nil
}

// emptyDocument reports whether data holds no YAML content (empty or only
// comments).
func emptyDocument(data []byte) (bool, error) {
	var doc yaml.Node
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return false, fmt.Errorf("parse yaml: %w", err)
	}
	return doc.Kind == 0 || len(doc.Content) == 0, nil
}

func topSequence(data []byte) (*yaml.Node, error) {
	var doc yaml.Node
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("parse yaml: %w", err)
	}
	if doc.Kind != yaml.DocumentNode || len(doc.Content) != 1 {
		return nil, errors.New("expected a list of entries, found an empty document")
	}
	seq := doc.Content[0]
	if seq.Kind != yaml.SequenceNode {
		return nil, fmt.Errorf("line %d: expected a list of entries, found %s", seq.Line, kindName(seq))
	}
	return seq, nil
}

type parser struct {
	entry int
	errs  []error
}

func (p *parser) errorf(n *yaml.Node, format string, args ...any) {
	p.errs = append(p.errs, fmt.Errorf("line %d: entry %d: %s", n.Line, p.entry, fmt.Sprintf(format, args...)))
}

func (p *parser) parseEntry(n *yaml.Node) (Entry, bool) {
	e := Entry{Line: n.Line}
	if p.customTag(n, "") {
		return e, false
	}
	if n.Kind != yaml.MappingNode {
		p.errorf(n, "expected a mapping, found %s", kindName(n))
		return e, false
	}
	before := len(p.errs)
	f := p.fields(n)
	if pn, ok := f["pattern"]; ok {
		e.Pattern, _ = p.pattern(pn)
	} else {
		p.errorf(n, "missing required key %q", "pattern")
	}
	if c, ok := f["comment"]; ok {
		e.Comment, _ = p.str(c, "comment")
	}
	if r, ok := f["run"]; ok {
		e.Run = p.run(r)
	}
	dirNode, envNode := f["dir"], f["env"]
	switch {
	case (dirNode != nil || envNode != nil) && f["run"] == nil:
		p.errorf(n, "dir and env need run")
	case e.Run != nil:
		if dirNode != nil {
			e.Dir = p.dir(dirNode)
		}
		if envNode != nil {
			e.Env = p.env(envNode)
		}
	}
	return e, len(p.errs) == before
}

// fields returns an entry's values by key, reporting unknown and duplicate
// keys.
func (p *parser) fields(n *yaml.Node) map[string]*yaml.Node {
	f := map[string]*yaml.Node{}
	for j := 0; j+1 < len(n.Content); j += 2 {
		key := n.Content[j]
		switch {
		case !slices.Contains(entryKeys, key.Value):
			p.errorf(key, "unknown key %q; allowed: %s", key.Value, strings.Join(entryKeys, ", "))
		case f[key.Value] != nil:
			p.errorf(key, "duplicate key %q", key.Value)
		default:
			f[key.Value] = n.Content[j+1]
		}
	}
	return f
}

func (p *parser) pattern(n *yaml.Node) (string, bool) {
	s, ok := p.str(n, "pattern")
	if !ok {
		return "", false
	}
	if err := ValidatePattern(s); err != nil {
		p.errorf(n, "%v", err)
		return "", false
	}
	return s, true
}

// str returns n as a string. Only a YAML string is accepted: a plain scalar
// YAML resolves to a number, bool or null is an error, not text.
func (p *parser) str(n *yaml.Node, where string) (string, bool) {
	if typ := p.badScalar(n, where); typ != "" {
		p.errorf(n, "%s: expected a string, found %s", where, typ)
		return "", false
	}
	return n.Value, n.Kind == yaml.ScalarNode
}

// customTag reports, and returns true for, a custom (non-core) YAML tag on n.
func (p *parser) customTag(n *yaml.Node, where string) bool {
	if !strings.HasPrefix(n.Tag, "!") || strings.HasPrefix(n.Tag, "!!") {
		return false
	}
	if where == "" {
		p.errorf(n, "custom tag %q is not supported", n.Tag)
	} else {
		p.errorf(n, "%s: custom tag %q is not supported", where, n.Tag)
	}
	return true
}

// badScalar reports why n is not a string scalar ("" when it is). Aliases and
// custom tags are reported directly.
func (p *parser) badScalar(n *yaml.Node, where string) string {
	switch {
	case n.Kind == yaml.AliasNode:
		p.errorf(n, "%s: aliases are not supported", where)
		return ""
	case p.customTag(n, where):
		return ""
	case n.Kind == yaml.ScalarNode && n.ShortTag() == "!!str":
		return ""
	}
	return kindName(n)
}

func (p *parser) run(n *yaml.Node) [][]Value {
	if p.customTag(n, "run") {
		return nil
	}
	if n.Kind != yaml.SequenceNode {
		p.errorf(n, "run: expected a list of commands, found %s", kindName(n))
		return nil
	}
	if len(n.Content) == 0 {
		p.errorf(n, "run: must not be empty; omit run for a file with no command")
		return nil
	}
	cmds := make([][]Value, 0, len(n.Content))
	for i, c := range n.Content {
		where := fmt.Sprintf("run[%d]", i)
		if p.customTag(c, where) {
			continue
		}
		if c.Kind != yaml.SequenceNode {
			p.errorf(c, "%s: expected a list of arguments, found %s", where, kindName(c))
			continue
		}
		if len(c.Content) == 0 {
			p.errorf(c, "%s: a command must not be empty", where)
			continue
		}
		argv := make([]Value, 0, len(c.Content))
		for j, a := range c.Content {
			v, ok := p.value(a, fmt.Sprintf("%s[%d]", where, j))
			if ok {
				argv = append(argv, v)
			}
		}
		if len(argv) != len(c.Content) {
			continue
		}
		p.checkCommand(c, where, argv)
		cmds = append(cmds, argv)
	}
	return cmds
}

// checkCommand enforces that the program, and a shell's -c script, never come
// from a file name.
func (p *parser) checkCommand(c *yaml.Node, where string, argv []Value) {
	if !argv[0].IsLit {
		p.errorf(c.Content[0], "%s[0]: the program must be a literal string", where)
		return
	}
	if !slices.Contains(shells, path.Base(argv[0].Lit)) {
		return
	}
	if j, flag := shellScriptIndex(argv); j > 0 && !argv[j].IsLit {
		p.errorf(c.Content[j], "%s[%d]: the script after %s must be a literal string; pass file values as positional parameters", where, j, flag)
	}
}

// shellScriptIndex returns the index of the argument a shell runs as its -c
// script, and the -c flag itself; 0 when there is no -c flag. The script is
// the first operand after the options: "--" ends options, -o/+o/-O/+O take
// an argument, and any other -x or +x is a flag. A non-literal argument in
// option position is returned as the script, because the shell could read
// it as one.
func shellScriptIndex(argv []Value) (int, string) {
	flag := ""
	for j := 1; j < len(argv); j++ {
		a := argv[j]
		if !a.IsLit {
			if flag != "" {
				return j, flag
			}
			continue
		}
		switch shellArgKind(a.Lit) {
		case argEndOfOptions:
			if flag == "" || j+1 >= len(argv) {
				return 0, ""
			}
			return j + 1, flag
		case argOptionWithValue:
			j++ // after -c the option's value must be literal too
			if flag != "" && j < len(argv) && !argv[j].IsLit {
				return j, flag
			}
		case argCFlag:
			flag = a.Lit
		case argOperand:
			if flag == "" {
				return 0, "" // a script file; its arguments follow
			}
			return j, flag
		}
	}
	return 0, ""
}

type shellArg int

const (
	argOperand shellArg = iota
	argFlag
	argCFlag
	argOptionWithValue
	argEndOfOptions
)

func shellArgKind(s string) shellArg {
	switch {
	case s == "--":
		return argEndOfOptions
	case s == "-o" || s == "+o" || s == "-O" || s == "+O":
		return argOptionWithValue
	case cFlagRE.MatchString(s):
		return argCFlag
	case strings.HasPrefix(s, "-") || strings.HasPrefix(s, "+"):
		return argFlag
	}
	return argOperand
}

func (p *parser) value(n *yaml.Node, where string) (Value, bool) {
	if n.Kind == yaml.MappingNode || n.Kind == yaml.SequenceNode {
		if p.customTag(n, where) {
			return Value{}, false
		}
	}
	if n.Kind == yaml.MappingNode {
		return p.valueMapping(n, where)
	}
	if typ := p.badScalar(n, where); typ != "" {
		p.errorf(n, "%s: expected a string or a {var}/{join} mapping, found %s", where, typ)
		return Value{}, false
	} else if n.Kind != yaml.ScalarNode {
		return Value{}, false
	}
	return Value{Lit: n.Value, IsLit: true}, true
}

func (p *parser) valueMapping(n *yaml.Node, where string) (Value, bool) {
	if len(n.Content) != 2 {
		p.errorf(n, "%s: a value mapping has exactly one key, var or join", where)
		return Value{}, false
	}
	key, val := n.Content[0], n.Content[1]
	switch key.Value {
	case "var":
		name, ok := p.str(val, where+": var")
		if !ok {
			return Value{}, false
		}
		if !slices.Contains(VarNames, name) {
			p.errorf(val, "%s: unknown var %q; expected one of %s", where, name, strings.Join(VarNames, ", "))
			return Value{}, false
		}
		return Value{Var: name}, true
	case "join":
		if p.customTag(val, where+": join") {
			return Value{}, false
		}
		if val.Kind != yaml.SequenceNode {
			p.errorf(val, "%s: join: expected a list of values, found %s", where, kindName(val))
			return Value{}, false
		}
		parts := make([]Value, 0, len(val.Content))
		for i, part := range val.Content {
			v, ok := p.value(part, fmt.Sprintf("%s.join[%d]", where, i))
			if !ok {
				return Value{}, false
			}
			parts = append(parts, v)
		}
		return Value{Join: parts}, true
	}
	p.errorf(key, "%s: unknown value key %q; expected var or join", where, key.Value)
	return Value{}, false
}

func (p *parser) dir(n *yaml.Node) *Value {
	v, ok := p.value(n, "dir")
	if !ok {
		return nil
	}
	if v.IsLit {
		switch {
		case strings.HasPrefix(v.Lit, "/"):
			p.errorf(n, "dir: %q must be relative to the repository root", v.Lit)
			return nil
		case v.Lit == "" || path.Clean(v.Lit) == ".." || strings.HasPrefix(path.Clean(v.Lit), "../"):
			p.errorf(n, "dir: %q must stay inside the repository", v.Lit)
			return nil
		}
	}
	return &v
}

func (p *parser) env(n *yaml.Node) []EnvVar {
	if p.customTag(n, "env") {
		return nil
	}
	if n.Kind != yaml.MappingNode {
		p.errorf(n, "env: expected a mapping, found %s", kindName(n))
		return nil
	}
	vars := make([]EnvVar, 0, len(n.Content)/2)
	seen := map[string]bool{}
	for j := 0; j+1 < len(n.Content); j += 2 {
		key, val := n.Content[j], n.Content[j+1]
		if seen[key.Value] {
			p.errorf(key, "env: duplicate variable %q", key.Value)
			continue
		}
		seen[key.Value] = true
		if !envNameR.MatchString(key.Value) {
			p.errorf(key, "env: invalid variable name %q", key.Value)
			continue
		}
		if v, ok := p.value(val, "env."+key.Value); ok {
			vars = append(vars, EnvVar{Name: key.Value, Value: v})
		}
	}
	return vars
}

func kindName(n *yaml.Node) string {
	switch n.Kind {
	case yaml.SequenceNode:
		return "list"
	case yaml.MappingNode:
		return "mapping"
	case yaml.AliasNode:
		return "alias"
	case yaml.ScalarNode:
		return strings.TrimPrefix(n.ShortTag(), "!!")
	}
	return "nothing"
}
