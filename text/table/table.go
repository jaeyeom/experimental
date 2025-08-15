// Package table provides a general-purpose table formatting tool with flexible
// column management, configurable width handling, and cell content wrapping/clipping options.
//
// This package offers both fixed-width and unlimited-width table rendering modes,
// with support for automatic terminal width detection, text wrapping within cells,
// and various text alignment options.
//
// Basic usage:
//
//	table := table.NewTable().
//		WithWidth(table.WidthFixed, 80).
//		WithCellMode(table.CellClip).
//		AddColumn("ID", 8, table.AlignLeft).
//		AddColumn("Name", 0, table.AlignLeft)
//
//	table.AddRow("1", "John Doe")
//	table.AddRow("2", "Jane Smith")
//
//	fmt.Print(table.Render())
//
// Alternatively, use the fluent builder interface:
//
//	table := table.NewBuilder().
//		Width(table.WidthAuto).
//		Wrapping(table.CellWrap).
//		Column("ID", 8, table.AlignLeft).
//		Column("Name", 0, table.AlignLeft).
//		Row("1", "John Doe").
//		Row("2", "Jane Smith").
//		Build()
package table

import (
	"os"
	"strconv"
	"strings"
)

// Alignment represents text alignment options.
type Alignment int

const (
	AlignLeft Alignment = iota
	AlignCenter
	AlignRight
)

// WidthMode represents table width handling modes.
type WidthMode int

const (
	WidthFixed WidthMode = iota
	WidthUnlimited
	WidthAuto
)

// CellMode represents cell content handling modes.
type CellMode int

const (
	CellWrap CellMode = iota
	CellClip
)

// Column represents a table column configuration.
type Column struct {
	Header    string
	Width     int // 0 for auto-sizing
	Alignment Alignment
}

// Config holds table configuration options.
type Config struct {
	Width         int
	WidthMode     WidthMode
	CellMode      CellMode
	SeparatorChar string
	PaddingChar   string
}

// Table represents a configurable table renderer.
type Table struct {
	config  Config
	columns []Column
	rows    [][]string
}

// NewTable creates a new table with default configuration.
func NewTable() *Table {
	return &Table{
		config: Config{
			Width:         120,
			WidthMode:     WidthAuto,
			CellMode:      CellWrap,
			SeparatorChar: " | ",
			PaddingChar:   " ",
		},
		columns: make([]Column, 0),
		rows:    make([][]string, 0),
	}
}

// WithWidth sets the table width mode and value.
func (t *Table) WithWidth(mode WidthMode, width ...int) *Table {
	t.config.WidthMode = mode
	if mode == WidthFixed && len(width) > 0 {
		t.config.Width = width[0]
	} else if mode == WidthAuto {
		t.config.Width = getTerminalWidth()
	}
	return t
}

// WithCellMode sets the cell content handling mode.
func (t *Table) WithCellMode(mode CellMode) *Table {
	t.config.CellMode = mode
	return t
}

// WithSeparator sets the column separator.
func (t *Table) WithSeparator(sep string) *Table {
	t.config.SeparatorChar = sep
	return t
}

// AddColumn adds a new column to the table.
func (t *Table) AddColumn(header string, width int, alignment Alignment) *Table {
	t.columns = append(t.columns, Column{
		Header:    header,
		Width:     width,
		Alignment: alignment,
	})
	return t
}

// AddRow adds a new data row to the table.
func (t *Table) AddRow(cells ...string) *Table {
	// Pad row to match number of columns
	row := make([]string, len(t.columns))
	copy(row, cells)
	t.rows = append(t.rows, row)
	return t
}

// Render generates the formatted table string.
func (t *Table) Render() string {
	if len(t.columns) == 0 {
		return ""
	}

	// Calculate column widths
	columnWidths := t.calculateColumnWidths()

	var result strings.Builder

	// Render header
	t.renderRow(&result, t.getHeaderRow(), columnWidths)

	// Render separator
	t.renderSeparator(&result, columnWidths)

	// Render data rows
	for _, row := range t.rows {
		t.renderRow(&result, row, columnWidths)
		if t.config.CellMode == CellWrap {
			t.renderSeparator(&result, columnWidths)
		}
	}

	return result.String()
}

// getHeaderRow returns the header row data.
func (t *Table) getHeaderRow() []string {
	headers := make([]string, len(t.columns))
	for i, col := range t.columns {
		headers[i] = col.Header
	}
	return headers
}

// calculateColumnWidths determines the actual width for each column.
func (t *Table) calculateColumnWidths() []int {
	widths := make([]int, len(t.columns))

	if t.config.WidthMode == WidthUnlimited {
		// For unlimited width, use specified widths or calculate from content
		for i, col := range t.columns {
			if col.Width > 0 {
				widths[i] = col.Width
			} else {
				widths[i] = t.calculateContentWidth(i)
			}
		}
		return widths
	}

	// Calculate fixed/auto width distribution
	totalWidth := t.config.Width
	separatorWidth := len(t.config.SeparatorChar) * (len(t.columns) - 1)
	availableWidth := totalWidth - separatorWidth

	if availableWidth < len(t.columns) {
		availableWidth = len(t.columns) // Minimum width
	}

	// First pass: assign fixed widths
	fixedWidth := 0
	autoColumns := 0

	for i, col := range t.columns {
		if col.Width > 0 {
			widths[i] = col.Width
			fixedWidth += col.Width
		} else {
			autoColumns++
		}
	}

	// Second pass: distribute remaining width among auto columns
	remainingWidth := availableWidth - fixedWidth
	if autoColumns > 0 && remainingWidth > 0 {
		baseWidth := remainingWidth / autoColumns
		extraWidth := remainingWidth % autoColumns

		for i, col := range t.columns {
			if col.Width == 0 {
				widths[i] = baseWidth
				if extraWidth > 0 {
					widths[i]++
					extraWidth--
				}
			}
		}
	}

	// Ensure minimum width of 1 for each column
	for i := range widths {
		if widths[i] < 1 {
			widths[i] = 1
		}
	}

	return widths
}

// calculateContentWidth calculates width needed for column content.
func (t *Table) calculateContentWidth(columnIndex int) int {
	maxWidth := len(t.columns[columnIndex].Header)

	for _, row := range t.rows {
		if columnIndex < len(row) {
			cellWidth := len(row[columnIndex])
			if cellWidth > maxWidth {
				maxWidth = cellWidth
			}
		}
	}

	return maxWidth
}

// renderRow renders a single table row.
func (t *Table) renderRow(result *strings.Builder, row []string, widths []int) {
	if t.config.CellMode == CellClip {
		t.renderSingleLineRow(result, row, widths)
		return
	}

	// Handle multi-line wrapping
	cellLines := make([][]string, len(row))
	maxLines := 1

	for i, cell := range row {
		if i < len(widths) {
			cellLines[i] = wrapText(cell, widths[i])
			if len(cellLines[i]) > maxLines {
				maxLines = len(cellLines[i])
			}
		}
	}

	// Render each line
	for lineIndex := 0; lineIndex < maxLines; lineIndex++ {
		var lineBuilder strings.Builder
		for i, col := range t.columns {
			if i > 0 {
				lineBuilder.WriteString(t.config.SeparatorChar)
			}

			var cellContent string
			if lineIndex < len(cellLines[i]) {
				cellContent = cellLines[i][lineIndex]
			}

			formattedCell := t.formatCell(cellContent, widths[i], col.Alignment)
			lineBuilder.WriteString(formattedCell)
		}
		result.WriteString(strings.TrimRight(lineBuilder.String(), " \t"))
		result.WriteString("\n")
	}
}

// renderSingleLineRow renders a row with clipped content.
func (t *Table) renderSingleLineRow(result *strings.Builder, row []string, widths []int) {
	var lineBuilder strings.Builder
	for i, col := range t.columns {
		if i > 0 {
			lineBuilder.WriteString(t.config.SeparatorChar)
		}

		cellContent := ""
		if i < len(row) {
			cellContent = row[i]
		}

		// Clip content if necessary
		if len(cellContent) > widths[i] {
			if widths[i] > 3 {
				cellContent = cellContent[:widths[i]-3] + "..."
			} else {
				cellContent = cellContent[:widths[i]]
			}
		}

		formattedCell := t.formatCell(cellContent, widths[i], col.Alignment)
		lineBuilder.WriteString(formattedCell)
	}
	result.WriteString(strings.TrimRight(lineBuilder.String(), " \t"))
	result.WriteString("\n")
}

// formatCell formats cell content with alignment and padding.
func (t *Table) formatCell(content string, width int, alignment Alignment) string {
	if len(content) >= width {
		return content
	}

	padding := width - len(content)

	switch alignment {
	case AlignLeft:
		return content + strings.Repeat(t.config.PaddingChar, padding)
	case AlignRight:
		return strings.Repeat(t.config.PaddingChar, padding) + content
	case AlignCenter:
		leftPad := padding / 2
		rightPad := padding - leftPad
		return strings.Repeat(t.config.PaddingChar, leftPad) + content + strings.Repeat(t.config.PaddingChar, rightPad)
	default:
		return content + strings.Repeat(t.config.PaddingChar, padding)
	}
}

// renderSeparator renders the table row separator.
func (t *Table) renderSeparator(result *strings.Builder, widths []int) {
	var lineBuilder strings.Builder
	for i, width := range widths {
		if i > 0 {
			lineBuilder.WriteString(strings.Repeat("-", len(t.config.SeparatorChar)))
		}
		lineBuilder.WriteString(strings.Repeat("-", width))
	}
	result.WriteString(strings.TrimRight(lineBuilder.String(), " \t"))
	result.WriteString("\n")
}

// getTerminalWidth detects terminal width from environment.
func getTerminalWidth() int {
	if os.Getenv("COLUMNS") != "" {
		if width, err := strconv.Atoi(os.Getenv("COLUMNS")); err == nil && width > 0 {
			return width
		}
	}
	return 120 // Default width
}

// wrapText wraps text to fit within the specified width, breaking at word boundaries when possible.
func wrapText(text string, width int) []string {
	if width <= 0 {
		return []string{text}
	}

	var lines []string
	words := strings.Fields(text)

	if len(words) == 0 {
		return []string{""}
	}

	currentLine := ""
	for _, word := range words {
		switch {
		case len(word) > width:
			// Add current line if it exists
			if currentLine != "" {
				lines = append(lines, currentLine)
				currentLine = ""
			}

			// Break the long word into chunks
			for len(word) > width {
				lines = append(lines, word[:width])
				word = word[width:]
			}

			// Set remaining part as current line
			if word != "" {
				currentLine = word
			}
		case currentLine == "":
			currentLine = word
		case len(currentLine)+1+len(word) <= width:
			currentLine += " " + word
		default:
			lines = append(lines, currentLine)
			currentLine = word
		}
	}

	if currentLine != "" {
		lines = append(lines, currentLine)
	}

	if len(lines) == 0 {
		return []string{""}
	}

	return lines
}

// Builder provides a fluent interface for table construction.
type Builder struct {
	table *Table
}

// NewBuilder creates a new table builder.
func NewBuilder() *Builder {
	return &Builder{table: NewTable()}
}

// Width sets the table width mode.
func (b *Builder) Width(mode WidthMode, width ...int) *Builder {
	b.table.WithWidth(mode, width...)
	return b
}

// Wrapping sets cell wrapping mode.
func (b *Builder) Wrapping(mode CellMode) *Builder {
	b.table.WithCellMode(mode)
	return b
}

// Column adds a column to the table.
func (b *Builder) Column(header string, width int, alignment Alignment) *Builder {
	b.table.AddColumn(header, width, alignment)
	return b
}

// Row adds a data row to the table.
func (b *Builder) Row(cells ...string) *Builder {
	b.table.AddRow(cells...)
	return b
}

// Build returns the constructed table.
func (b *Builder) Build() *Table {
	return b.table
}
