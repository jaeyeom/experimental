package main

import (
	"fmt"
	"log"
	"os/exec"
)

func Run(cmd *exec.Cmd) error {
	log.Println("Running command:", cmd)
	out, err := cmd.CombinedOutput()
	log.Println("Output:", string(out))
	if err != nil {
		return fmt.Errorf("command execution failed: %w", err)
	}
	return nil
}
