# text/canvas

A 2D text canvas package for Go that provides arbitrary text positioning and common layout patterns.

## Installation

```go
import "github.com/jaeyeom/experimental/text/canvas"
```

## Core Concepts

### Canvas

The `Canvas` is a 2D grid of runes that can be manipulated and rendered as text.

```go
c := canvas.New(40, 10)
c.SetChar(0, 0, 'H')
c.SetChar(1, 0, 'i')
fmt.Println(c.Render())
```

### Renderable Interface

All layout types implement the `Renderable` interface, enabling composition and nesting:

```go
type Renderable interface {
    RenderTo(cs CharSetter)
    GetID() string
    GetBounds() Rectangle
}
```

## Layout Types

### Grid

Grid-based layouts with configurable cell size and gaps.

```go
grid := canvas.Grid("data", 2, 3).
    At(canvas.Position{X: 0, Y: 0}).
    WithCellSize(8, 1).
    WithGap(1, 0).
    SetCellText(0, 0, "Name").
    SetCellText(0, 1, "Age").
    SetCellText(0, 2, "City").
    SetCellText(1, 0, "Alice").
    SetCellText(1, 1, "30").
    SetCellText(1, 2, "NYC")

c := canvas.New(30, 5)
grid.RenderTo(c)
fmt.Println(c.Render())
```

Output:
```
Name     Age      City
Alice    30       NYC
```

### Stack

Arranges children vertically or horizontally with optional gaps and alignment.

```go
// Vertical stack
vstack := canvas.NewStack("menu", canvas.DirectionVertical).
    WithGap(0).
    AddText("1", "1. New Game").
    AddText("2", "2. Load Game").
    AddText("3", "3. Exit")

// Horizontal stack
hstack := canvas.NewStack("buttons", canvas.DirectionHorizontal).
    WithGap(2).
    AddText("ok", "[OK]").
    AddText("cancel", "[Cancel]")
```

Output (vertical):
```
1. New Game
2. Load Game
3. Exit
```

Output (horizontal):
```
[OK]  [Cancel]
```

### Border

Wraps content with decorative borders. Supports multiple styles and titles.

```go
content := canvas.NewTextBlock("msg", "Hello, World!", canvas.Position{})
border := canvas.NewBorder("box", content, canvas.BorderSingle)

c := canvas.New(20, 5)
border.RenderTo(c)
fmt.Println(c.Render())
```

Output:
```
┌─────────────┐
│Hello, World!│
└─────────────┘
```

#### Border Styles

| Style | Characters |
|-------|------------|
| `BorderSingle` | `┌─┐│└┘` |
| `BorderDouble` | `╔═╗║╚╝` |
| `BorderRounded` | `╭─╮│╰╯` |
| `BorderASCII` | `+-+\|` |

#### With Title

```go
border := canvas.NewBorder("alert", content, canvas.BorderDouble).
    WithTitle("Alert", canvas.TitleTopCenter)
```

Output:
```
╔════════Alert═════════╗
║Important message here║
╚══════════════════════╝
```

### BorderLayout

Divides space into five regions: North, South, East, West, and Center.

```go
layout := canvas.NewBorderLayout("app", 30, 5).
    WithGap(1).
    SetRegionText(canvas.RegionNorth, "=== Header ===").
    SetRegionText(canvas.RegionSouth, "Status: Ready").
    SetRegionText(canvas.RegionWest, "Menu").
    SetRegionText(canvas.RegionCenter, "Main Content")
```

Output:
```
=== Header ===

Menu Main Content

Status: Ready
```

### FlowLayout

Arranges children in a flowing manner, automatically wrapping to new lines.

```go
flow := canvas.NewFlowLayout("tags", 20).
    WithGap(1, 0).
    AddText("t1", "[go]", 4).
    AddText("t2", "[rust]", 6).
    AddText("t3", "[python]", 8).
    AddText("t4", "[js]", 4)
```

Output:
```
[go] [rust] [python]
[js]
```

## Convenience Functions

### Center

Creates centered text within a specified width.

```go
block := canvas.CenterAt("title", "Hello", 15, canvas.Position{X: 0, Y: 0})
```

Output:
```
     Hello
```

### Title

Creates a title with optional underline.

```go
title := canvas.NewTitle("header", "My Application").
    At(canvas.Position{X: 0, Y: 0}).
    WithUnderline(canvas.LineStyleDouble)
```

Output:
```
My Application
==============
```

### Margin

Wraps content with margin spacing.

```go
content := canvas.NewTextBlock("inner", "Hello", canvas.Position{})
margin := canvas.NewMargin("wrapper", content, 1, 2, 1, 2) // top, right, bottom, left
```

### AddBorder (Shorthand)

Quick way to create bordered text.

```go
border := canvas.AddBorder("note", "Quick note", canvas.BorderRounded)
```

Output:
```
╭──────────╮
│Quick note│
╰──────────╯
```

## Nesting Layouts

All layout types implement `Renderable`, so they can be nested:

```go
// Create bordered boxes
box1 := canvas.AddBorder("b1", "Box 1", canvas.BorderSingle)
box2 := canvas.AddBorder("b2", "Box 2", canvas.BorderSingle)
box3 := canvas.AddBorder("b3", "Box 3", canvas.BorderSingle)

// Stack them horizontally
stack := canvas.NewStack("boxes", canvas.DirectionHorizontal).
    WithGap(1).
    Add(box1).
    Add(box2).
    Add(box3)

c := canvas.New(35, 5)
stack.RenderTo(c)
fmt.Println(c.Render())
```

Output:
```
┌─────┐ ┌─────┐ ┌─────┐
│Box 1│ │Box 2│ │Box 3│
└─────┘ └─────┘ └─────┘
```

## Complete Example: Dashboard

```go
func CreateDashboard() string {
    // Create header
    header := canvas.NewTitle("header", "System Dashboard").
        At(canvas.Position{X: 0, Y: 0}).
        WithUnderline(canvas.LineStyleDouble).
        WithWidth(40)

    // Create status grid
    statusGrid := canvas.Grid("status", 2, 2).
        At(canvas.Position{X: 0, Y: 3}).
        WithCellSize(15, 1).
        WithGap(2, 0).
        SetCellText(0, 0, "CPU: 45%").
        SetCellText(0, 1, "Memory: 2.1GB").
        SetCellText(1, 0, "Disk: 120GB").
        SetCellText(1, 1, "Network: OK")

    // Create a bordered message box
    msgContent := canvas.NewTextBlock("msg", "All systems operational", canvas.Position{})
    messageBox := canvas.NewBorder("msgbox", msgContent, canvas.BorderRounded).
        At(canvas.Position{X: 0, Y: 6}).
        WithTitle("Status", canvas.TitleTopLeft)

    // Render everything to canvas
    c := canvas.New(45, 12)
    header.RenderTo(c)
    statusGrid.RenderTo(c)
    messageBox.RenderTo(c)

    return c.Render()
}
```

Output:
```
System Dashboard
========================================

CPU: 45%         Memory: 2.1GB
Disk: 120GB      Network: OK

╭─Status────────────────╮
│All systems operational│
╰───────────────────────╯
```

## Type Reference

### Direction

```go
const (
    DirectionVertical   Direction = iota
    DirectionHorizontal
)
```

### BorderStyle

```go
const (
    BorderNone    BorderStyle = iota
    BorderSingle
    BorderDouble
    BorderRounded
    BorderASCII
)
```

### TitlePosition

```go
const (
    TitleTopLeft      TitlePosition = iota
    TitleTopCenter
    TitleTopRight
    TitleBottomLeft
    TitleBottomCenter
    TitleBottomRight
)
```

### Region (for BorderLayout)

```go
const (
    RegionNorth  Region = iota
    RegionSouth
    RegionEast
    RegionWest
    RegionCenter
)
```

### Spacing

```go
type Spacing struct {
    Top, Right, Bottom, Left int
}

// Constructors
canvas.NewSpacing(all int) Spacing
canvas.NewSpacingSymmetric(vertical, horizontal int) Spacing
canvas.NewSpacingTRBL(top, right, bottom, left int) Spacing
```
