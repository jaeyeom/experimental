package main

import (
	"fmt"
	"os"
	"os/exec"
)

func CheckDir(dir string) func() bool {
	return func() bool {
		_, err := os.Stat(dir)
		return err == nil
	}
}

func EmacsCurrentVersion(cmd *exec.Cmd) string {
	if cmd == nil {
		cmd = exec.Command("emacs", "--version")
	}
	out, err := cmd.Output()
	if err != nil {
		return ""
	}
	var version string
	fmt.Sscanf(string(out), "GNU Emacs %s\n", &version)
	return version
}

func CheckEmacsVersion(version string) func() bool {
	return func() bool {
		return EmacsCurrentVersion(nil) == version
	}
}
