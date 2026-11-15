package orgctl

import (
	"path/filepath"
	"testing"
)

func TestExpandPath(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	got, err := ExpandPath("~/proj")
	if err != nil {
		t.Fatal(err)
	}
	if got != filepath.Join(home, "proj") {
		t.Fatalf("tilde = %s", got)
	}
	got, err = ExpandPath("~")
	if err != nil || got != home {
		t.Fatalf("~ = %s, %v", got, err)
	}
	got, err = ExpandPath("/abs/proj")
	if err != nil || got != "/abs/proj" {
		t.Fatalf("absolute = %s, %v", got, err)
	}
	got, err = ExpandPath("proj")
	if err != nil || got != "proj" {
		t.Fatalf("relative = %s, %v", got, err)
	}
}

func TestCheckCwd(t *testing.T) {
	bad := Spec{Roles: map[string]Role{
		"desk/clerk": {Cwd: "proj"},
		"abs/one":    {Cwd: "/proj"},
	}}
	err := bad.CheckCwd()
	if err == nil || err.Error() != "role desk/clerk: cwd must be absolute or start with ~" {
		t.Fatalf("err = %v", err)
	}
	ok := Spec{Roles: map[string]Role{
		"desk/clerk": {Cwd: "~/proj"},
		"abs/one":    {Cwd: "/proj"},
		"bare/home":  {Cwd: "~"},
		"unset/one":  {},
	}}
	if err := ok.CheckCwd(); err != nil {
		t.Fatal(err)
	}
}
