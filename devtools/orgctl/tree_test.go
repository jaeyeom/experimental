package orgctl

import (
	"encoding/json"
	"strings"
	"testing"
)

const treeSpecJSON = `{
  "roles": {
    "desk/lead":  {"model": "opus"},
    "desk/clerk": {"reportsTo": "desk/lead"},
    "desk/audit": {"reportsTo": "desk/lead"},
    "desk/intern": {"reportsTo": "desk/clerk"},
    "ops/oncall": {}
  },
  "pools": {
    "typists": {"starter": "desk/lead", "max": 3}
  }
}`

func treeSpec(t *testing.T) Spec {
	t.Helper()
	spec, err := Decode([]byte(treeSpecJSON))
	if err != nil {
		t.Fatal(err)
	}
	return spec
}

func renderTree(t *testing.T, spec Spec, opts TreeOptions, ascii bool) string {
	t.Helper()
	root, err := BuildTree(spec, opts)
	if err != nil {
		t.Fatal(err)
	}
	return RenderTree(root, ascii)
}

func TestTreeUnicode(t *testing.T) {
	got := renderTree(t, treeSpec(t), TreeOptions{Depth: -1}, false)
	want := `user
├── desk/lead
│   ├── desk/audit
│   ├── desk/clerk
│   │   └── desk/intern
│   └── [pool] typists (max 3)
└── ops/oncall
`
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestTreeASCIIAndRootLabel(t *testing.T) {
	got := renderTree(t, treeSpec(t), TreeOptions{Depth: -1, RootLabel: "operator"}, true)
	want := "operator\n" +
		"|-- desk/lead\n" +
		"|   |-- desk/audit\n" +
		"|   |-- desk/clerk\n" +
		"|   |   `-- desk/intern\n" +
		"|   `-- [pool] typists (max 3)\n" +
		"`-- ops/oncall\n"
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestTreeRootRole(t *testing.T) {
	got := renderTree(t, treeSpec(t), TreeOptions{Depth: -1, Root: "desk/clerk"}, false)
	want := "desk/clerk\n└── desk/intern\n"
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestTreeUnknownRootRole(t *testing.T) {
	_, err := BuildTree(treeSpec(t), TreeOptions{Depth: -1, Root: "desk/nobody"})
	if err == nil || !strings.Contains(err.Error(), `unknown role "desk/nobody"`) {
		t.Fatalf("err = %v", err)
	}
}

func TestTreeDepth(t *testing.T) {
	got := renderTree(t, treeSpec(t), TreeOptions{Depth: 1}, false)
	want := "user\n├── desk/lead\n└── ops/oncall\n"
	if got != want {
		t.Fatalf("depth 1 got\n%s", got)
	}
	got = renderTree(t, treeSpec(t), TreeOptions{Depth: 0}, false)
	if got != "user\n" {
		t.Fatalf("depth 0 got\n%s", got)
	}
}

func TestTreeJSON(t *testing.T) {
	root, err := BuildTree(treeSpec(t), TreeOptions{Depth: 1, Root: "desk/lead"})
	if err != nil {
		t.Fatal(err)
	}
	data, err := json.Marshal(root)
	if err != nil {
		t.Fatal(err)
	}
	want := `{"label":"desk/lead","kind":"role","children":[` +
		`{"label":"desk/audit","kind":"role","children":[]},` +
		`{"label":"desk/clerk","kind":"role","children":[]},` +
		`{"label":"typists","kind":"pool","max":3,"children":[]}]}`
	if string(data) != want {
		t.Fatalf("got  %s\nwant %s", data, want)
	}
}

func TestTreeLive(t *testing.T) {
	spec, err := Decode([]byte(`{"roles":{
		"a/up":{},"b/sleep":{},"c/down":{},"d/gone":{},"e/none":{},"f/far":{}}}`))
	if err != nil {
		t.Fatal(err)
	}
	live := Live{
		Names: []LiveName{
			{Name: "a/up", Liveness: "alive"},
			{Name: "b/sleep", Liveness: "hibernated"},
			{Name: "c/down", Liveness: "dead"},
			{Name: "d/gone", Liveness: "gone"},
			{Name: "f/far", Liveness: "unreachable"},
		},
		Agents: []LiveAgent{{Name: "a/up", Model: "opus"}},
	}
	got := renderTree(t, spec, TreeOptions{Depth: -1, Live: &live}, false)
	want := `user
├── a/up (alive, opus)
├── b/sleep (hibernated)
├── c/down (dead)
├── d/gone (dead)
├── e/none (missing)
└── f/far (unreachable)
`
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}

	root, err := BuildTree(spec, TreeOptions{Depth: -1, Live: &live})
	if err != nil {
		t.Fatal(err)
	}
	if l := root.Children[0].Live; l == nil || l.State != "alive" || l.Model != "opus" {
		t.Fatalf("json live = %+v", l)
	}
}

func TestTreeLiveNamesNotRead(t *testing.T) {
	spec, err := Decode([]byte(`{"roles":{"a/up":{}}}`))
	if err != nil {
		t.Fatal(err)
	}
	got := renderTree(t, spec, TreeOptions{Depth: -1, Live: &Live{NamesNotRead: true}}, false)
	if got != "user\n└── a/up (not read)\n" {
		t.Fatalf("got\n%s", got)
	}
}

func TestCheckReportsCycle(t *testing.T) {
	spec, err := Decode([]byte(`{"roles":{
		"a":{"reportsTo":"b"},"b":{"reportsTo":"c"},"c":{"reportsTo":"a"},"top":{}}}`))
	if err != nil {
		t.Fatal(err)
	}
	err = spec.CheckReports()
	if err == nil || !strings.Contains(err.Error(), "reportsTo cycle: a → b → c → a") {
		t.Fatalf("err = %v", err)
	}
	if _, err := BuildTree(spec, TreeOptions{Depth: -1}); err == nil || !strings.Contains(err.Error(), "a → b → c → a") {
		t.Fatalf("tree err = %v", err)
	}
}

func TestCheckReportsUnknownManager(t *testing.T) {
	spec, err := Decode([]byte(`{"roles":{"a":{"reportsTo":"ghost"}}}`))
	if err != nil {
		t.Fatal(err)
	}
	if err := spec.CheckReports(); err == nil || !strings.Contains(err.Error(), `role a: reportsTo unknown role "ghost"`) {
		t.Fatalf("err = %v", err)
	}
}

func TestTreeSiblingOrderStable(t *testing.T) {
	first := renderTree(t, treeSpec(t), TreeOptions{Depth: -1}, false)
	for range 20 {
		if got := renderTree(t, treeSpec(t), TreeOptions{Depth: -1}, false); got != first {
			t.Fatalf("order changed:\n%s\nvs\n%s", got, first)
		}
	}
}
