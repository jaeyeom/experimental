// Package main provides the command-line interface for splitting markdown files.
package main

import (
	"flag"
	"log/slog"
	"os"

	"github.com/jaeyeom/experimental/text/markdown/split"
)

func main() {
	inputFile := flag.String("input", "", "Input markdown file to split")
	outputDir := flag.String("output", "", "Output directory for split files")
	flag.Parse()

	if *inputFile == "" || *outputDir == "" {
		flag.Usage()
		os.Exit(1)
	}

	if err := split.Markdown(*inputFile, *outputDir); err != nil {
		slog.Error("Failed to split markdown", "error", err)
		os.Exit(1)
	}
}
