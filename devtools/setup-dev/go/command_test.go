package main

import (
	"os/exec"
	"testing"
)

func TestRun(t *testing.T) {
	cmd := exec.Command("echo", "Hello, world!")
	err := Run(cmd)
	if err != nil {
		t.Fatal(err)
	}
}
