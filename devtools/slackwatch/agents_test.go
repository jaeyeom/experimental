package slackwatch

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

func TestAgentbusNames(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "agentbus")
	body := "#!/bin/sh\necho '[{\"name\":\"alpha\"},{\"name\":\"\"},{\"name\":\"beta\"}]'\n"
	if err := os.WriteFile(script, []byte(body), 0o700); err != nil { //nolint:gosec // G306: the fake agentbus must be executable.
		t.Fatal(err)
	}
	t.Setenv("PATH", dir)
	got, err := AgentbusNames(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(got, []string{"alpha", "beta"}) {
		t.Errorf("names = %q", got)
	}
}

func TestAgentbusNamesDown(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	if _, err := AgentbusNames(context.Background()); err == nil {
		t.Fatal("missing agentbus succeeded")
	}
}
