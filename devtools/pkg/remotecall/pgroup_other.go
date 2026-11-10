//go:build !unix

package remotecall

import "os/exec"

// setProcessGroup leaves cmd alone. A cancel kills only the process.
func setProcessGroup(*exec.Cmd) {}
