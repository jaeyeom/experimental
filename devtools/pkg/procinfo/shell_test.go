package procinfo

import "testing"

func TestIsShell(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		p    Process
		want bool
	}{
		{Process{Name: "zsh", Argv: []string{"-zsh"}}, true},
		{Process{Name: "-bash", Argv: []string{"-bash"}}, true},
		{Process{Name: "fish", Argv: []string{"/opt/homebrew/bin/fish", "-l"}}, true},
		{Process{Argv: []string{"/bin/sh"}}, true},
		{Process{Name: "2.1.319", Argv: []string{"claude", "--resume"}}, false},
		{Process{Name: "vim", Argv: []string{"vim", "x.go"}}, false},
		{Process{}, false},
	} {
		if got := IsShell(tc.p); got != tc.want {
			t.Errorf("IsShell(%+v) = %v, want %v", tc.p, got, tc.want)
		}
	}
}
