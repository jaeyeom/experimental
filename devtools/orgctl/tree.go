package orgctl

import (
	"fmt"
	"slices"
	"strings"
)

// TreeNode is one line of the org chart. Kind is root, role, or pool.
type TreeNode struct {
	Label    string      `json:"label"`
	Kind     string      `json:"kind"`
	Max      int         `json:"max,omitempty"`
	Live     *TreeLive   `json:"live,omitempty"`
	Children []*TreeNode `json:"children"`
}

// TreeLive is a role's live state: alive, hibernated, dead, missing,
// another agentbus liveness value, or "not read". Model is the running
// agent's model when known.
type TreeLive struct {
	State string `json:"state"`
	Model string `json:"model,omitempty"`
}

// TreeOptions shapes the chart.
//
// RootLabel names the operator line; empty means "user". Root, when
// set, starts the chart at that role instead. Depth stops below that
// depth, where the root is 0; a negative Depth has no limit. A nil Live
// reads nothing live.
type TreeOptions struct {
	RootLabel string
	Root      string
	Depth     int
	Live      *Live
}

// CheckReports rejects a reportsTo that names no role and a reportsTo
// cycle. The cycle error prints its path, such as "a → b → a".
func (s Spec) CheckReports() error {
	for _, name := range roleNames(s) {
		boss := s.Roles[name].ReportsTo
		if boss != nil {
			if _, ok := s.Roles[*boss]; !ok {
				return fmt.Errorf("role %s: reportsTo unknown role %q", name, *boss)
			}
		}
	}
	for _, name := range roleNames(s) {
		if cycle := s.reportsCycle(name); cycle != nil {
			return fmt.Errorf("reportsTo cycle: %s", strings.Join(cycle, " → "))
		}
	}
	return nil
}

// reportsCycle follows reportsTo from start and returns the cycle it
// enters, starting and ending at the first repeated role, or nil.
func (s Spec) reportsCycle(start string) []string {
	var path []string
	seen := map[string]int{}
	for name := start; ; {
		if i, ok := seen[name]; ok {
			return append(path[i:], name)
		}
		seen[name] = len(path)
		path = append(path, name)
		boss := s.Roles[name].ReportsTo
		if boss == nil {
			return nil
		}
		if _, ok := s.Roles[*boss]; !ok {
			return nil
		}
		name = *boss
	}
}

// BuildTree builds the org chart: the operator, then the roles with no
// reportsTo, each with its reports and the pools it starts. Siblings are
// roles by name, then pools by name.
func BuildTree(s Spec, opts TreeOptions) (*TreeNode, error) {
	if err := s.CheckReports(); err != nil {
		return nil, err
	}
	reports := map[string][]string{}
	for _, name := range roleNames(s) {
		boss := ""
		if b := s.Roles[name].ReportsTo; b != nil {
			boss = *b
		}
		reports[boss] = append(reports[boss], name)
	}
	pools := map[string][]string{}
	for _, name := range mapKeys(s.Pools) {
		starter := s.Pools[name].Starter
		if _, ok := s.Roles[starter]; !ok {
			starter = ""
		}
		pools[starter] = append(pools[starter], name)
	}
	b := treeBuilder{spec: s, opts: opts, reports: reports, pools: pools}
	if opts.Root != "" {
		if _, ok := s.Roles[opts.Root]; !ok {
			return nil, fmt.Errorf("unknown role %q", opts.Root)
		}
		return b.role(opts.Root, 0), nil
	}
	label := opts.RootLabel
	if label == "" {
		label = "user"
	}
	root := &TreeNode{Label: label, Kind: "root"}
	root.Children = b.children("", 0)
	return root, nil
}

type treeBuilder struct {
	spec    Spec
	opts    TreeOptions
	reports map[string][]string
	pools   map[string][]string
}

func (b treeBuilder) role(name string, depth int) *TreeNode {
	node := &TreeNode{Label: name, Kind: "role", Live: b.live(name)}
	node.Children = b.children(name, depth)
	return node
}

// children lists the reports and pools under parent ("" is the root).
func (b treeBuilder) children(parent string, depth int) []*TreeNode {
	nodes := []*TreeNode{}
	if b.opts.Depth >= 0 && depth >= b.opts.Depth {
		return nodes
	}
	for _, name := range b.reports[parent] {
		nodes = append(nodes, b.role(name, depth+1))
	}
	for _, name := range b.pools[parent] {
		nodes = append(nodes, &TreeNode{Label: name, Kind: "pool", Max: b.spec.Pools[name].Max, Children: []*TreeNode{}})
	}
	return nodes
}

func (b treeBuilder) live(name string) *TreeLive {
	live := b.opts.Live
	if live == nil {
		return nil
	}
	if live.NamesNotRead {
		return &TreeLive{State: "not read"}
	}
	i := slices.IndexFunc(live.Names, func(n LiveName) bool { return n.Name == name })
	if i < 0 {
		return &TreeLive{State: "missing"}
	}
	state := live.Names[i].Liveness
	switch state {
	case "gone", "stale":
		state = "dead"
	case "":
		state = "alive"
	}
	out := &TreeLive{State: state}
	if agent, ok := liveAgent(*live, name); ok {
		out.Model = agent.Model
	}
	return out
}

// RenderTree prints the chart in tree(1) style. ascii uses |--, |, and
// `-- instead of box-drawing characters. Lines are never cut.
func RenderTree(root *TreeNode, ascii bool) string {
	var sb strings.Builder
	sb.WriteString(nodeLine(root) + "\n")
	writeChildren(&sb, root.Children, "", ascii)
	return sb.String()
}

func writeChildren(sb *strings.Builder, nodes []*TreeNode, prefix string, ascii bool) {
	tee, last, bar, gap := "├── ", "└── ", "│   ", "    "
	if ascii {
		tee, last, bar = "|-- ", "`-- ", "|   "
	}
	for i, node := range nodes {
		branch, next := tee, bar
		if i == len(nodes)-1 {
			branch, next = last, gap
		}
		sb.WriteString(prefix + branch + nodeLine(node) + "\n")
		writeChildren(sb, node.Children, prefix+next, ascii)
	}
}

func nodeLine(n *TreeNode) string {
	switch {
	case n.Kind == "pool":
		return fmt.Sprintf("[pool] %s (max %d)", n.Label, n.Max)
	case n.Live == nil:
		return n.Label
	case n.Live.Model != "":
		return fmt.Sprintf("%s (%s, %s)", n.Label, n.Live.State, n.Live.Model)
	default:
		return fmt.Sprintf("%s (%s)", n.Label, n.Live.State)
	}
}
