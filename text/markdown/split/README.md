# Markdown Splitter

A utility tool to split large markdown files into smaller parts based on
headers. This is particularly useful when working with large technical
documentation that needs to be processed by LLM models.

## Features

- Splits markdown files based on header levels (#, ##, ###, etc.)
- Generates a table of contents file with links to all sections
- Uses base36 naming scheme (0-9A-Z) for better organization
- Sanitizes filenames for cross-platform compatibility
- Preserves markdown formatting
- Creates clean, organized output directory structure
- Efficient processing using Go's concurrent features

## Installation

```bash
go install github.com/jaeyeom/experimental/text/markdown/split/cmd/split@latest
```

Or build from a checkout of this repository:

```bash
go build -o split ./text/markdown/split/cmd/split
```

## Usage

Both flags are required; there is no default output directory.

```bash
split -input=input_file.md -output=output_directory
```

### Arguments

- `-input`: The markdown file you want to split (required)
- `-output`: Output directory for split files (required)

## Output Structure

The tool generates:
1. A table of contents file named `000_table_of_contents.md`
2. Individual markdown files for each section using a base36 naming scheme:
   - First level headers: `100_`, `200_`, `300_`, etc.
   - Second level headers: `110_`, `120_`, `130_`, etc.
   - Third level headers: `111_`, `112_`, `113_`, etc.

This naming scheme allows for up to 36 sections per level (using 0-9 and A-Z).

## Running Tests

From this directory:

```bash
go test ./...
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
