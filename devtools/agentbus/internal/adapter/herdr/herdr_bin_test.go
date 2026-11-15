package herdr

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

func TestHerdrEnvRunsWhenPathIsEmpty(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "herdr")
	body := "#!/bin/sh\necho '{\"result\":{\"agents\":[]}}'\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil { //nolint:gosec // G306: the fake herdr has to be executable
		t.Fatal(err)
	}
	t.Setenv("PATH", "")
	t.Setenv("AGENTBUS_HERDR", script)
	if err := (&Adapter{}).SelfTest(context.Background()); err != nil {
		t.Fatal(err)
	}
}

func TestHerdrPathOverridesTheEnvBin(t *testing.T) {
	dir := t.TempDir()
	bad := filepath.Join(dir, "bad")
	good := filepath.Join(dir, "good")
	if err := os.WriteFile(bad, []byte("#!/bin/sh\nexit 1\n"), 0o755); err != nil { //nolint:gosec // G306: the fake herdr has to be executable
		t.Fatal(err)
	}
	body := "#!/bin/sh\necho '{\"result\":{\"agents\":[]}}'\n"
	if err := os.WriteFile(good, []byte(body), 0o755); err != nil { //nolint:gosec // G306: the fake herdr has to be executable
		t.Fatal(err)
	}
	t.Setenv("PATH", "")
	t.Setenv("AGENTBUS_HERDR", bad)
	ad := &Adapter{HerdrPath: good}
	if err := ad.SelfTest(context.Background()); err != nil {
		t.Fatal(err)
	}
}
