package notify

import (
	"context"
	"slices"
	"testing"
)

func TestBuildCommandPlaceholdersAndPositional(t *testing.T) {
	vars := []Var{
		{Placeholder: "{rule}", EnvKey: "TEST_RULE", Positional: true, Value: "github-token"},
		{Placeholder: "{prefix}", EnvKey: "TEST_PREFIX", Positional: true, Value: "ghp_****"},
		{Placeholder: "{extra}", EnvKey: "TEST_EXTRA", Positional: false, Value: "opt"},
	}
	baseEnv := []string{"PATH=/usr/bin"}

	t.Run("placeholder substitution", func(t *testing.T) {
		argv, env := BuildCommand([]string{"notify-send", "{rule}", "seen {prefix}"}, baseEnv, vars)
		wantArgv := []string{"notify-send", "github-token", "seen ghp_****"}
		if !slices.Equal(argv, wantArgv) {
			t.Fatalf("argv = %v, want %v", argv, wantArgv)
		}
		wantEnv := []string{"PATH=/usr/bin", "TEST_RULE=github-token", "TEST_PREFIX=ghp_****", "TEST_EXTRA=opt"}
		if !slices.Equal(env, wantEnv) {
			t.Fatalf("env = %v, want %v", env, wantEnv)
		}
	})

	t.Run("positional fallback", func(t *testing.T) {
		argv, _ := BuildCommand([]string{"my-notify"}, baseEnv, vars)
		wantArgv := []string{"my-notify", "github-token", "ghp_****"}
		if !slices.Equal(argv, wantArgv) {
			t.Fatalf("argv = %v, want %v", argv, wantArgv)
		}
	})
}

func TestExec(t *testing.T) {
	if err := Exec(context.Background(), nil, nil); err == nil {
		t.Fatal("expected error for empty argv")
	}
	if err := Exec(context.Background(), []string{"true"}, []string{"PATH=/usr/bin:/bin"}); err != nil {
		t.Fatalf("Exec(true) = %v", err)
	}
	if err := Exec(context.Background(), []string{"false"}, []string{"PATH=/usr/bin:/bin"}); err == nil {
		t.Fatal("expected error for false command")
	}
}
