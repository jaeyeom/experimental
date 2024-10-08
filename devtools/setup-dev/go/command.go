package main

import (
	"log"
	"os/exec"
)

func Run(cmd *exec.Cmd) error {
	log.Println("Running command:", cmd)
	out, err := cmd.CombinedOutput()
	log.Println("Output:", string(out))
	return err
}
