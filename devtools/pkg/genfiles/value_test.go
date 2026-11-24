package genfiles

import (
	"reflect"
	"testing"
)

func TestVars(t *testing.T) {
	tests := []struct {
		path string
		want map[string]string
	}{
		{"a/b/foo.tar.gz", map[string]string{"path": "a/b/foo.tar.gz", "dir": "a/b", "name": "foo.tar.gz", "stem": "foo.tar"}},
		{"Makefile", map[string]string{"path": "Makefile", "dir": ".", "name": "Makefile", "stem": "Makefile"}},
		{"x/.bashrc", map[string]string{"path": "x/.bashrc", "dir": "x", "name": ".bashrc", "stem": ".bashrc"}},
	}
	for _, tt := range tests {
		if got := Vars(tt.path); !reflect.DeepEqual(got, tt.want) {
			t.Errorf("Vars(%q) = %v, want %v", tt.path, got, tt.want)
		}
	}
}

func TestValueExpand(t *testing.T) {
	v := Value{Join: []Value{
		{Lit: "tools/sim/xacro/", IsLit: true},
		{Var: "stem"},
		{Lit: ".xacro", IsLit: true},
	}}
	if got := v.Expand("tools/sim/urdf/body.urdf"); got != "tools/sim/xacro/body.xacro" {
		t.Errorf("Expand = %q", got)
	}
	// A literal is used byte for byte, braces and all.
	lit := Value{Lit: "{path}; rm -rf ~", IsLit: true}
	if got := lit.Expand("x"); got != "{path}; rm -rf ~" {
		t.Errorf("literal Expand = %q", got)
	}
	// A file name is one value, never split or interpreted.
	if got := (Value{Var: "path"}).Expand("a/x;rm -rf ~.lock"); got != "a/x;rm -rf ~.lock" {
		t.Errorf("var Expand = %q", got)
	}
}
