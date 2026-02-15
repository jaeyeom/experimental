// Package main provides a CLI tool for rendering ASCII art boxes with
// sections and connectors.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/text/box"
	"github.com/jaeyeom/experimental/text/canvas"
)

// stringSlice implements flag.Value for repeated string flags.
type stringSlice []string

func (s *stringSlice) String() string { return strings.Join(*s, ", ") }
func (s *stringSlice) Set(val string) error {
	*s = append(*s, val)
	return nil
}

func main() {
	if err := run(os.Args[1:], os.Stdin, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func run(args []string, stdin io.Reader, stdout io.Writer) error {
	var (
		sections  stringSlice
		connector string
		border    string
		padding   int
		width     int
		prefix    string
	)

	fs := flag.NewFlagSet("box-renderer", flag.ContinueOnError)
	fs.SetOutput(stdout)
	fs.Var(&sections, "section", "Section text, | separates lines. Repeatable.")
	fs.StringVar(&connector, "connector", "│|▼", "Connector chars between sections, | separates lines")
	fs.StringVar(&border, "border", "single", "Border style: single, double, rounded, ascii")
	fs.IntVar(&padding, "padding", 2, "Left/right inner padding")
	fs.IntVar(&width, "width", 0, "Box width (0 = auto)")
	fs.StringVar(&prefix, "prefix", "", "Comment prefix for each line")

	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("parsing flags: %w", err)
	}

	// If no --section flags, read from stdin
	if len(sections) == 0 {
		var err error
		sections, err = readSectionsFromReader(stdin)
		if err != nil {
			return fmt.Errorf("reading stdin: %w", err)
		}
	}

	if len(sections) == 0 {
		return fmt.Errorf("no sections provided; use --section flags or pipe text via stdin")
	}

	style, err := parseBorderStyle(border)
	if err != nil {
		return err
	}

	b := box.New().
		WithStyle(style).
		WithPadding(padding).
		WithWidth(width).
		WithPrefix(prefix).
		WithConnector(strings.Split(connector, "|")...)

	for _, s := range sections {
		b.AddSection(strings.Split(s, "|")...)
	}

	fmt.Fprintln(stdout, b.Render())
	return nil
}

// readSectionsFromReader reads sections from a reader. Sections are separated
// by lines containing only "---".
func readSectionsFromReader(r io.Reader) ([]string, error) {
	var sections []string
	var current []string

	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "---" {
			if len(current) > 0 {
				sections = append(sections, strings.Join(current, "|"))
				current = nil
			}
			continue
		}
		current = append(current, line)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("scanning input: %w", err)
	}
	if len(current) > 0 {
		sections = append(sections, strings.Join(current, "|"))
	}
	return sections, nil
}

func parseBorderStyle(s string) (canvas.BorderStyle, error) {
	switch strings.ToLower(s) {
	case "single":
		return canvas.BorderSingle, nil
	case "double":
		return canvas.BorderDouble, nil
	case "rounded":
		return canvas.BorderRounded, nil
	case "ascii":
		return canvas.BorderASCII, nil
	default:
		return 0, fmt.Errorf("unknown border style %q; use single, double, rounded, or ascii", s)
	}
}
