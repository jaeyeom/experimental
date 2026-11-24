//go:build !unix

package sandbox

import (
	"fmt"
	"os/exec"
	"time"
)

func configureProcessGroup(*exec.Cmd) {}

// Terminate kills the child process on non-Unix platforms.
func (p *Process) Terminate(time.Duration) error {
	if p.exited() {
		return nil
	}
	if err := p.cmd.Process.Kill(); err != nil {
		return fmt.Errorf("kill process %d: %w", p.pid, err)
	}
	<-p.done
	return nil
}

func processAlive(int) bool {
	return false
}
