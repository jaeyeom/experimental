package orgctl

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
)

// Change kinds.
const (
	ChangeAdd    = "add"
	ChangeRemove = "remove"
	ChangeModify = "change"
)

// Change is one difference between two specs. Scope is org, role,
// pool, rule (an org rule), or class. Name is the role, pool, rule id,
// or class. Field is empty when the whole thing was added or removed.
// For a list item, Item is set and Old or New holds the item. For a
// long text, Long is set, Old and New hold the whole texts, and Added
// and Gone count the words added and removed.
type Change struct {
	Kind  string `json:"kind"`
	Scope string `json:"scope"`
	Name  string `json:"name"`
	Field string `json:"field,omitempty"`
	Old   string `json:"old,omitempty"`
	New   string `json:"new,omitempty"`
	Item  bool   `json:"item,omitempty"`
	Long  bool   `json:"long,omitempty"`
	Added int    `json:"addedWords,omitempty"`
	Gone  int    `json:"removedWords,omitempty"`
	hunks []string
}

// DiffSpecs lists the changes from before to after: the org name, roles,
// pools, org rules, and classes with their rules. Roles, pools, and
// classes are matched by key, rules and schedules by id, so moving one
// is not a change. A list that is only reordered is not a change.
func DiffSpecs(before, after Spec) []Change {
	var out []Change
	if before.Org != after.Org {
		out = append(out, Change{Kind: ChangeModify, Scope: "org", Name: "org", Field: "name", Old: before.Org, New: after.Org})
	}
	out = append(out, diffKeyed("role", before.Roles, after.Roles, diffRole)...)
	out = append(out, diffKeyed("pool", before.Pools, after.Pools, diffPool)...)
	out = append(out, diffRules("rule", "", before.Rules, after.Rules)...)
	out = append(out, diffKeyed("class", before.Classes, after.Classes, diffClass)...)
	return out
}

func diffKeyed[V any](scope string, before, after map[string]V, each func(name string, a, b V) []Change) []Change {
	names := mapKeys(before)
	for _, name := range mapKeys(after) {
		if _, ok := before[name]; !ok {
			names = append(names, name)
		}
	}
	slices.Sort(names)
	var out []Change
	for _, name := range names {
		a, inOld := before[name]
		b, inNew := after[name]
		switch {
		case !inOld:
			out = append(out, Change{Kind: ChangeAdd, Scope: scope, Name: name})
		case !inNew:
			out = append(out, Change{Kind: ChangeRemove, Scope: scope, Name: name})
		default:
			out = append(out, each(name, a, b)...)
		}
	}
	return out
}

// fieldDiff collects the changes of one role, pool, or class.
type fieldDiff struct {
	scope, name string
	out         []Change
}

func (d *fieldDiff) scalar(field, a, b string) {
	if a != b {
		d.out = append(d.out, Change{Kind: ChangeModify, Scope: d.scope, Name: d.name, Field: field, Old: a, New: b})
	}
}

func (d *fieldDiff) list(field string, a, b []string) {
	for _, item := range b {
		if !slices.Contains(a, item) {
			d.out = append(d.out, Change{Kind: ChangeAdd, Scope: d.scope, Name: d.name, Field: field, New: item, Item: true})
		}
	}
	for _, item := range a {
		if !slices.Contains(b, item) {
			d.out = append(d.out, Change{Kind: ChangeRemove, Scope: d.scope, Name: d.name, Field: field, Old: item, Item: true})
		}
	}
}

func (d *fieldDiff) text(field, a, b string) {
	if a == b {
		return
	}
	added, gone, hunks := wordDiff(a, b)
	d.out = append(d.out, Change{
		Kind: ChangeModify, Scope: d.scope, Name: d.name, Field: field, Old: a, New: b,
		Long: true, Added: added, Gone: gone, hunks: hunks,
	})
}

func optional(s *string) string {
	if s == nil {
		return ""
	}
	return *s
}

func diffRole(name string, a, b Role) []Change {
	d := fieldDiff{scope: "role", name: name}
	d.scalar("machine", a.Machine, b.Machine)
	d.scalar("workspace", a.Workspace, b.Workspace)
	d.scalar("tab", a.Tab, b.Tab)
	d.scalar("cwd", a.Cwd, b.Cwd)
	d.scalar("agent", a.Agent, b.Agent)
	d.scalar("model", a.Model, b.Model)
	d.scalar("effort", a.Effort, b.Effort)
	d.scalar("reportsTo", optional(a.ReportsTo), optional(b.ReportsTo))
	d.scalar("checkoutLease", a.CheckoutLease, b.CheckoutLease)
	d.scalar("class", a.Class, b.Class)
	d.text("when", a.When, b.When)
	d.scalar("brief.skill", a.Brief.Skill, b.Brief.Skill)
	d.scalar("brief.file", a.Brief.File, b.Brief.File)
	d.text("brief.text", a.Brief.Text, b.Brief.Text)
	d.list("brief.memory", a.Brief.Memory, b.Brief.Memory)
	d.list("decides", a.Decides, b.Decides)
	d.list("escalates", a.Escalates, b.Escalates)
	d.list("owns", a.Owns, b.Owns)
	d.list("tools", a.Tools, b.Tools)
	d.list("state", a.State, b.State)
	d.list("rules", a.Rules, b.Rules)
	d.list("needs.permissions", a.Needs.Permissions, b.Needs.Permissions)
	d.list("needs.secrets", a.Needs.Secrets, b.Needs.Secrets)
	d.list("needs.services", a.Needs.Services, b.Needs.Services)
	d.schedules(a.Schedules, b.Schedules)
	return d.out
}

func (d *fieldDiff) schedules(a, b []Schedule) {
	byID := func(list []Schedule) map[string]Schedule {
		m := make(map[string]Schedule, len(list))
		for _, s := range list {
			m[s.ID] = s
		}
		return m
	}
	before, after := byID(a), byID(b)
	for _, s := range b {
		if _, ok := before[s.ID]; !ok {
			d.out = append(d.out, Change{Kind: ChangeAdd, Scope: d.scope, Name: d.name, Field: "schedule", New: s.ID, Item: true})
		}
	}
	for _, s := range a {
		if _, ok := after[s.ID]; !ok {
			d.out = append(d.out, Change{Kind: ChangeRemove, Scope: d.scope, Name: d.name, Field: "schedule", Old: s.ID, Item: true})
		}
	}
	for _, s := range a {
		n, ok := after[s.ID]
		if !ok {
			continue
		}
		prefix := "schedule " + s.ID + " "
		d.scalar(prefix+"cron", s.Cron, n.Cron)
		d.scalar(prefix+"durable", strconv.FormatBool(s.Durable), strconv.FormatBool(n.Durable))
		d.text(prefix+"prompt", s.Prompt, n.Prompt)
	}
}

func diffPool(name string, a, b Pool) []Change {
	d := fieldDiff{scope: "pool", name: name}
	d.scalar("machine", a.Machine, b.Machine)
	d.scalar("workspace", a.Workspace, b.Workspace)
	d.scalar("namePattern", a.NamePattern, b.NamePattern)
	d.scalar("tabPattern", a.TabPattern, b.TabPattern)
	d.scalar("starter", a.Starter, b.Starter)
	d.scalar("max", strconv.Itoa(a.Max), strconv.Itoa(b.Max))
	d.scalar("source", a.Source, b.Source)
	return d.out
}

func diffClass(name string, a, b Class) []Change {
	d := fieldDiff{scope: "class", name: name}
	d.text("description", a.Description, b.Description)
	return append(d.out, diffRules("class", name, a.Rules, b.Rules)...)
}

// diffRules matches rules by id. An org rule's Scope is rule and its
// Name the id; a class rule's Scope is class, Name the class, and Field
// "rule <id>" (with " text" or " source" when that changed).
func diffRules(scope, class string, a, b []Rule) []Change {
	byID := func(list []Rule) map[string]Rule {
		m := make(map[string]Rule, len(list))
		for _, r := range list {
			m[r.ID] = r
		}
		return m
	}
	before, after := byID(a), byID(b)
	var out []Change
	add := func(kind, id string) {
		if class == "" {
			out = append(out, Change{Kind: kind, Scope: scope, Name: id})
			return
		}
		c := Change{Kind: kind, Scope: scope, Name: class, Field: "rule", Item: true}
		if kind == ChangeAdd {
			c.New = id
		} else {
			c.Old = id
		}
		out = append(out, c)
	}
	for _, r := range b {
		if _, ok := before[r.ID]; !ok {
			add(ChangeAdd, r.ID)
		}
	}
	for _, r := range a {
		if _, ok := after[r.ID]; !ok {
			add(ChangeRemove, r.ID)
		}
	}
	for _, r := range a {
		n, ok := after[r.ID]
		if !ok {
			continue
		}
		d := fieldDiff{scope: scope, name: r.ID}
		prefix := ""
		if class != "" {
			d.name = class
			prefix = "rule " + r.ID + " "
		}
		d.text(prefix+"text", r.Text, n.Text)
		d.scalar(prefix+"source", r.Source, n.Source)
		out = append(out, d.out...)
	}
	return out
}

// maxHunks is how many word-diff hunks a long text shows without --full.
const maxHunks = 3

// Lines is the change as text. A long text prints a summary line, then
// its word-diff hunks indented, at most maxHunks unless full is set.
func (c Change) Lines(full bool) []string {
	head := c.head()
	if !c.Long {
		return []string{head}
	}
	lines := []string{fmt.Sprintf("%s changed (+%d -%d words)", head, c.Added, c.Gone)}
	hunks := c.hunks
	if !full && len(hunks) > maxHunks {
		more := len(hunks) - maxHunks
		hunks = append(slices.Clone(hunks[:maxHunks]), fmt.Sprintf("… %d more (--full shows all)", more))
	}
	for _, h := range hunks {
		lines = append(lines, "    "+h)
	}
	return lines
}

func (c Change) head() string {
	label := c.Scope + " " + c.Name
	switch c.Scope {
	case "role":
		label = c.Name
	case "org":
		label = "org"
	}
	switch {
	case c.Field == "" && c.Kind == ChangeAdd:
		return "+ " + c.Scope + " " + c.Name + " (new)"
	case c.Field == "" && c.Kind == ChangeRemove:
		return "- " + c.Scope + " " + c.Name + " (removed)"
	case c.Item && c.Kind == ChangeAdd:
		return "~ " + label + ": + " + c.Field + " " + strconv.Quote(c.New)
	case c.Item && c.Kind == ChangeRemove:
		return "~ " + label + ": - " + c.Field + " " + strconv.Quote(c.Old)
	case c.Long:
		return "~ " + label + ": " + c.Field
	default:
		return fmt.Sprintf("~ %s: %s %s -> %s", label, c.Field, shown(c.Old), shown(c.New))
	}
}

func shown(s string) string {
	if s == "" {
		return "(none)"
	}
	if strings.ContainsAny(s, " \t\n\"") {
		return strconv.Quote(s)
	}
	return s
}

// maxDiffCells bounds the word-diff table; larger texts are summarized
// by counts alone.
const maxDiffCells = 4_000_000

// diffContext is the unchanged words shown on each side of a hunk.
const diffContext = 3

// wordDiff counts the words added and removed between a and b and
// renders each changed run as one hunk: a few words of context, then
// [-removed-] and {+added+}.
func wordDiff(a, b string) (added, gone int, hunks []string) {
	x, y := strings.Fields(a), strings.Fields(b)
	if len(x)*len(y) > maxDiffCells {
		return len(y), len(x), nil
	}
	ops := lcsOps(x, y)
	for i := 0; i < len(ops); {
		if ops[i].kind == ' ' {
			i++
			continue
		}
		j := i
		var del, ins []string
		for j < len(ops) && ops[j].kind != ' ' {
			if ops[j].kind == '-' {
				del = append(del, ops[j].word)
			} else {
				ins = append(ins, ops[j].word)
			}
			j++
		}
		gone += len(del)
		added += len(ins)
		hunks = append(hunks, hunk(ops, i, j, del, ins))
		i = j
	}
	return added, gone, hunks
}

func hunk(ops []wordOp, start, end int, del, ins []string) string {
	var parts []string
	before := contextWords(ops, start-1, -1)
	if len(before) > 0 {
		parts = append(parts, strings.Join(before, " "))
	}
	if len(del) > 0 {
		parts = append(parts, "[-"+strings.Join(del, " ")+"-]")
	}
	if len(ins) > 0 {
		parts = append(parts, "{+"+strings.Join(ins, " ")+"+}")
	}
	if after := contextWords(ops, end, 1); len(after) > 0 {
		parts = append(parts, strings.Join(after, " "))
	}
	return strings.Join(parts, " ")
}

// contextWords collects up to diffContext unchanged words from i in
// direction step, stopping at the next change, in reading order.
func contextWords(ops []wordOp, i, step int) []string {
	var words []string
	for ; i >= 0 && i < len(ops) && ops[i].kind == ' ' && len(words) < diffContext; i += step {
		words = append(words, ops[i].word)
	}
	if step < 0 {
		slices.Reverse(words)
	}
	return words
}

type wordOp struct {
	kind byte // ' ', '-', '+'
	word string
}

// lcsOps is the word edit script from x to y by longest common
// subsequence.
func lcsOps(x, y []string) []wordOp {
	n, m := len(x), len(y)
	table := make([][]int, n+1)
	for i := range table {
		table[i] = make([]int, m+1)
	}
	for i := n - 1; i >= 0; i-- {
		for j := m - 1; j >= 0; j-- {
			if x[i] == y[j] {
				table[i][j] = table[i+1][j+1] + 1
			} else {
				table[i][j] = max(table[i+1][j], table[i][j+1])
			}
		}
	}
	var ops []wordOp
	i, j := 0, 0
	for i < n && j < m {
		switch {
		case x[i] == y[j]:
			ops = append(ops, wordOp{' ', x[i]})
			i++
			j++
		case table[i+1][j] >= table[i][j+1]:
			ops = append(ops, wordOp{'-', x[i]})
			i++
		default:
			ops = append(ops, wordOp{'+', y[j]})
			j++
		}
	}
	for ; i < n; i++ {
		ops = append(ops, wordOp{'-', x[i]})
	}
	for ; j < m; j++ {
		ops = append(ops, wordOp{'+', y[j]})
	}
	return ops
}
