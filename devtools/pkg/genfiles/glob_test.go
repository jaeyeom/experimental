package genfiles

import "testing"

func TestMatch(t *testing.T) {
	tests := []struct {
		pattern, path string
		want          bool
	}{
		{"Cargo.lock", "Cargo.lock", true},
		{"Cargo.lock", "a/b/Cargo.lock", true},
		{"Cargo.lock", "Cargo.lockx", false},
		{"/Cargo.lock", "Cargo.lock", true},
		{"/Cargo.lock", "a/Cargo.lock", false},
		{"**/pnpm-lock.yaml", "pnpm-lock.yaml", true},
		{"**/pnpm-lock.yaml", "x/y/pnpm-lock.yaml", true},
		{"a/*.go", "a/x.go", true},
		{"a/*.go", "a/b/x.go", false},
		{"a/*.go", "b/a/x.go", false},
		{"a/**/b", "a/b", true},
		{"a/**/b", "a/x/y/b", true},
		{"a/**", "a/x/y", true},
		{"a/**", "a", false},
		{"?.go", "x.go", true},
		{"?.go", "xy.go", false},
		{"*.go", "deep/dir/x.go", true},
		{"*.pkl.go", "a/Config.pkl.go", true},
		{"a.b", "aXb", false},
		{"a/[x", "a/[x", false}, // invalid pattern never matches
	}
	for _, tt := range tests {
		if got := Match(tt.pattern, tt.path); got != tt.want {
			t.Errorf("Match(%q, %q) = %v, want %v", tt.pattern, tt.path, got, tt.want)
		}
	}
}

func TestValidatePattern(t *testing.T) {
	for _, ok := range []string{"Cargo.lock", "/uv.lock", "**/x.json", "a/**", "a/*/b?.go"} {
		if err := ValidatePattern(ok); err != nil {
			t.Errorf("ValidatePattern(%q) = %v, want nil", ok, err)
		}
	}
	for _, bad := range []string{"", "!x", "a/[ab].go", "a]", `a\b`, `"a"`, "a b", "a\tb", "dir/", "/"} {
		if err := ValidatePattern(bad); err == nil {
			t.Errorf("ValidatePattern(%q) = nil, want error", bad)
		}
	}
}
