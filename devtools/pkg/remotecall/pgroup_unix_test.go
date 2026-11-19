//go:build unix

package remotecall

import (
	"context"
	"fmt"
	"os"
	"strings"
	"testing"

	"golang.org/x/sys/unix"
)

// sessionHelperEnv makes the test binary print whether it leads its own
// session, which means it has no controlling terminal.
const sessionHelperEnv = "REMOTECALL_SESSION_HELPER"

func TestMain(m *testing.M) {
	if os.Getenv(sessionHelperEnv) == "1" {
		sid, err := unix.Getsid(0)
		fmt.Println(err == nil && sid == os.Getpid())
		os.Exit(0)
	}
	os.Exit(m.Run())
}

func TestRunIONoTerminalStartsANewSession(t *testing.T) {
	env := append(os.Environ(), sessionHelperEnv+"=1")
	var stdout, stderr strings.Builder
	res, err := RunIOEnvNoTerminal(context.Background(), env, nil, &stdout, &stderr, os.Args[0])
	if err != nil || res.Code != 0 {
		t.Fatalf("RunIOEnvNoTerminal = %+v, %v; stderr %q", res, err, stderr.String())
	}
	if got := strings.TrimSpace(stdout.String()); got != "true" {
		t.Fatalf("child leads its own session = %s, want true", got)
	}

	stdout.Reset()
	if _, err := RunIOEnv(context.Background(), env, nil, &stdout, &stderr, os.Args[0]); err != nil {
		t.Fatal(err)
	}
	if got := strings.TrimSpace(stdout.String()); got != "false" {
		t.Fatalf("RunIOEnv child leads its own session = %s, want false", got)
	}
}
