package main

import (
	_ "embed"
	"fmt"
	"io"
)

//go:embed skill.md
var skillText string

func cmdSkill(args []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		return fail(stderr, fmt.Errorf("decide skill does not take arguments"), 2)
	}
	if _, err := io.WriteString(stdout, skillText); err != nil {
		return fail(stderr, err, 1)
	}
	return 0
}
