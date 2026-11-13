package main

import (
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
)

// cliStdin is the reader for a text argument of "-". Tests replace it.
var cliStdin io.Reader = os.Stdin

var (
	errTextSources = errors.New("decide text comes from the argument, --text-file, or -, not more than one")
	errTextEmpty   = errors.New("decide text is empty")
)

func textStdin() io.Reader {
	if cliStdin == nil {
		return os.Stdin
	}
	return cliStdin
}

// pullTextFile removes one --text-file path. A second one is a usage error.
// Arguments after -- are left alone.
func pullTextFile(args []string) (string, []string, error) {
	var file string
	seen := false
	out := make([]string, 0, len(args))
	for i := 0; i < len(args); i++ {
		a := args[i]
		if a == "--" {
			out = append(out, args[i:]...)
			break
		}
		path, eq := strings.CutPrefix(a, "--text-file=")
		if !eq && a != "--text-file" {
			out = append(out, a)
			continue
		}
		if seen {
			return "", nil, fmt.Errorf("--text-file was given twice")
		}
		seen = true
		if !eq {
			if i+1 >= len(args) || args[i+1] == "--" {
				return "", nil, fmt.Errorf("--text-file needs a path")
			}
			i++
			path = args[i]
		}
		file = path
	}
	return file, out, nil
}

// splitFlags separates flags from positionals. A valued flag consumes the
// next argument unless it already contains '='. "-" is a positional.
func splitFlags(args []string, valued map[string]bool) (flags, pos []string) {
	for i := 0; i < len(args); i++ {
		a := args[i]
		if a == "--" {
			pos = append(pos, args[i+1:]...)
			break
		}
		if a == "-" || !strings.HasPrefix(a, "-") {
			pos = append(pos, a)
			continue
		}
		flags = append(flags, a)
		name := strings.TrimLeft(a, "-")
		if name == "" || strings.Contains(name, "=") {
			continue
		}
		if valued[name] && i+1 < len(args) {
			i++
			flags = append(flags, args[i])
		}
	}
	return flags, pos
}

// bodyFrom reads the only positional, --text-file, or "-". A missing
// positional keeps the decide <missing> is required error.
func bodyFrom(args []string, file, missing string) (string, error) {
	if file != "" && len(args) > 0 {
		return "", errTextSources
	}
	if file != "" {
		return readTrimmed(file, false)
	}
	if len(args) != 1 {
		return "", fmt.Errorf("decide %s is required", missing)
	}
	if args[0] == "-" {
		return readTrimmed("", true)
	}
	if strings.TrimSpace(args[0]) == "" {
		return "", fmt.Errorf("decide %s is required", missing)
	}
	return trimBody(args[0])
}

// pairBody reads an id plus text from the positional, --text-file, or "-".
func pairBody(args []string, first, second, file string) (string, string, error) {
	if file != "" {
		return pairFile(args, first, second, file)
	}
	if len(args) == 2 && args[1] == "-" {
		if strings.TrimSpace(args[0]) == "" {
			return "", "", fmt.Errorf("decide %s and %s are required", first, second)
		}
		text, err := readTrimmed("", true)
		return args[0], text, err
	}
	id, text, err := twoArgs(args, first, second)
	if err != nil {
		return "", "", err
	}
	text, err = trimBody(text)
	if err != nil {
		return "", "", fmt.Errorf("decide %s and %s are required", first, second)
	}
	return id, text, nil
}

func pairFile(args []string, first, second, file string) (string, string, error) {
	if len(args) != 1 || strings.TrimSpace(args[0]) == "" {
		if len(args) > 1 {
			return "", "", errTextSources
		}
		return "", "", fmt.Errorf("decide %s and %s are required", first, second)
	}
	text, err := readTrimmed(file, false)
	if err != nil {
		return "", "", err
	}
	return args[0], text, nil
}

// resourceArgs reads free|busy and an optional note. The note is optional
// only when no file and no "-" are given.
func resourceArgs(args []string, file string) (string, string, error) {
	if len(args) < 1 || len(args) > 2 || strings.TrimSpace(args[0]) == "" {
		return "", "", fmt.Errorf("decide status set --resource requires free or busy")
	}
	if file != "" && len(args) > 1 {
		return "", "", errTextSources
	}
	if file == "" && len(args) == 1 {
		return args[0], "", nil
	}
	if file == "" && args[1] != "-" {
		note, err := trimBody(args[1])
		if err != nil {
			return "", "", err
		}
		return args[0], note, nil
	}
	note, err := readTrimmed(file, file == "")
	if err != nil {
		return "", "", err
	}
	return args[0], note, nil
}

func readTrimmed(file string, dash bool) (string, error) {
	raw, err := readSource(file, dash)
	if err != nil {
		return "", err
	}
	return trimBody(string(raw))
}

func readSource(file string, dash bool) ([]byte, error) {
	if file != "" {
		raw, err := os.ReadFile(file)
		if err != nil {
			return nil, fmt.Errorf("decide text file: %w", err)
		}
		return raw, nil
	}
	if !dash {
		return nil, errTextEmpty
	}
	raw, err := io.ReadAll(textStdin())
	if err != nil {
		return nil, fmt.Errorf("decide text: %w", err)
	}
	return raw, nil
}

func trimBody(raw string) (string, error) {
	text := strings.TrimRight(raw, "\r\n")
	if text == "" {
		return "", errTextEmpty
	}
	return text, nil
}

// expandHubText reads --text-file or a positional "-" locally and replaces
// it with that text. Args with neither are returned unchanged.
func expandHubText(args []string) ([]string, error) {
	if !hubTextCommand(args) {
		return args, nil
	}
	file, stripped, err := pullTextFile(args)
	if err != nil {
		return nil, err
	}
	if file == "" && !textDash(stripped) {
		return args, nil
	}
	return rebuildTextArgs(stripped, file)
}

func hubTextCommand(args []string) bool {
	if len(args) == 0 {
		return false
	}
	switch args[0] {
	case "ask", "reply", "send", "done", "withdraw":
		return true
	case "status":
		return len(args) > 1 && args[1] == "set"
	default:
		return false
	}
}

func textDash(args []string) bool {
	flags, pos := splitFlags(args[textPrefix(args):], textValued(args))
	idx := textIndex(args, flags)
	return len(pos) > idx && pos[idx] == "-"
}

func textPrefix(args []string) int {
	if args[0] == "status" {
		return 2
	}
	return 1
}

func textIndex(args, flags []string) int {
	if textNeedsID(args[0]) {
		return 1
	}
	if args[0] == "status" && flagNamed(flags, "resource") && !flagNamed(flags, "agent") {
		return 1
	}
	return 0
}

func textNeedsID(cmd string) bool {
	return cmd == "reply" || cmd == "done" || cmd == "withdraw"
}

func textValued(args []string) map[string]bool {
	switch args[0] {
	case "ask":
		return map[string]bool{
			"owner": true, "kind": true, "option": true, "rec": true,
			"deadline": true, "link": true, "notify-pane": true,
			"type-text": true, "where": true,
		}
	case "send":
		return map[string]bool{"to": true, "id": true, "via": true}
	case "reply":
		return map[string]bool{"option": true, "rec": true}
	case "status":
		return map[string]bool{"agent": true, "resource": true, "state": true}
	default:
		return nil
	}
}

func flagNamed(flags []string, name string) bool {
	prefix := "--" + name
	for _, f := range flags {
		if f == prefix || strings.HasPrefix(f, prefix+"=") {
			return true
		}
	}
	return false
}

func rebuildTextArgs(args []string, file string) ([]string, error) {
	n := textPrefix(args)
	flags, pos := splitFlags(args[n:], textValued(args))
	fixed, text, err := hubBody(args, flags, pos, file)
	if err != nil {
		return nil, err
	}
	out := make([]string, 0, n+len(flags)+len(fixed)+1)
	out = append(out, args[:n]...)
	out = append(out, flags...)
	out = append(out, fixed...)
	if text != "" {
		out = append(out, text)
	}
	return out, nil
}

func hubBody(args []string, flags, pos []string, file string) ([]string, string, error) {
	if args[0] == "status" && flagNamed(flags, "resource") && !flagNamed(flags, "agent") {
		availability, note, err := resourceArgs(pos, file)
		if err != nil {
			return nil, "", err
		}
		return []string{availability}, note, nil
	}
	if textNeedsID(args[0]) {
		id, text, err := pairBody(pos, "id", textLabel(args[0]), file)
		if err != nil {
			return nil, "", err
		}
		return []string{id}, text, nil
	}
	label := "text"
	if args[0] == "ask" {
		label = "question"
	}
	text, err := bodyFrom(pos, file, label)
	if err != nil {
		return nil, "", err
	}
	return nil, text, nil
}

func textLabel(cmd string) string {
	switch cmd {
	case "done":
		return "outcome"
	case "withdraw":
		return "reason"
	default:
		return "text"
	}
}
