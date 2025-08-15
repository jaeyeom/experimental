package table

import (
	"fmt"
	"time"
)

func ExampleTable_basic() {
	// Basic table with fixed width and clipped cells
	table := NewTable().
		WithWidth(WidthFixed, 50).
		WithCellMode(CellClip).
		AddColumn("ID", 4, AlignRight).
		AddColumn("Name", 15, AlignLeft).
		AddColumn("Status", 10, AlignCenter)

	table.AddRow("1", "John Smith", "Active")
	table.AddRow("2", "Jane Doe", "Inactive")
	table.AddRow("123", "Bob Johnson Jr.", "Pending")

	fmt.Print(table.Render())
}

func ExampleTable_wrapping() {
	// Table with text wrapping enabled
	table := NewTable().
		WithWidth(WidthFixed, 40).
		WithCellMode(CellWrap).
		AddColumn("Name", 12, AlignLeft).
		AddColumn("Description", 0, AlignLeft)

	table.AddRow("Service A", "This is a very long description that will be wrapped across multiple lines")
	table.AddRow("Service B", "Short desc")

	fmt.Print(table.Render())
}

func ExampleTable_unlimited() {
	// Unlimited width table - expands to fit content
	table := NewTable().
		WithWidth(WidthUnlimited).
		AddColumn("Timestamp", 20, AlignLeft).
		AddColumn("Level", 8, AlignCenter).
		AddColumn("Message", 0, AlignLeft)

	now := time.Now()
	table.AddRow(now.Format("2006-01-02 15:04:05"), "INFO", "Application started successfully")
	table.AddRow(now.Add(time.Minute).Format("2006-01-02 15:04:05"), "WARN", "Low disk space")
	table.AddRow(now.Add(2*time.Minute).Format("2006-01-02 15:04:05"), "ERROR", "Database connection failed")

	fmt.Print(table.Render())
}

func ExampleBuilder() {
	// Using the fluent builder interface
	table := NewBuilder().
		Width(WidthAuto). // Uses terminal width
		Wrapping(CellWrap).
		Column("ID", 8, AlignLeft).
		Column("File Path", 30, AlignLeft).
		Column("Size", 10, AlignRight).
		Column("Modified", 16, AlignLeft).
		Row("1", "/home/user/documents/report.pdf", "2.5 MB", "2023-12-01").
		Row("2", "/home/user/projects/myapp/src/main.go", "15.2 KB", "2023-12-02").
		Row("3", "/home/user/downloads/very-long-filename-that-might-wrap.zip", "150 MB", "2023-12-03").
		Build()

	fmt.Print(table.Render())
}

func ExampleTable_customSeparator() {
	// Table with custom separator
	table := NewTable().
		WithSeparator(" │ ").
		WithWidth(WidthFixed, 40).
		AddColumn("Name", 10, AlignLeft).
		AddColumn("Value", 15, AlignRight).
		AddColumn("Unit", 8, AlignCenter)

	table.AddRow("CPU", "85.5", "%")
	table.AddRow("Memory", "4.2", "GB")
	table.AddRow("Disk", "120.8", "GB")

	fmt.Print(table.Render())
}

func ExampleTable_alignment() {
	// Demonstrate different alignment options
	table := NewTable().
		WithWidth(WidthFixed, 60).
		WithCellMode(CellClip).
		AddColumn("Left", 15, AlignLeft).
		AddColumn("Center", 15, AlignCenter).
		AddColumn("Right", 15, AlignRight)

	table.AddRow("Short", "Medium text", "Very long content here")
	table.AddRow("A", "B", "C")
	table.AddRow("Test data", "Centered", "Right aligned")

	fmt.Print(table.Render())
}
