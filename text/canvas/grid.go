package canvas

import "strconv"

// GridLayout provides a grid-based layout system.
// Cells are arranged in rows and columns with configurable size and gap.
type GridLayout struct {
	id         string
	rows, cols int
	cellWidth  int
	cellHeight int
	position   Position
	cells      map[[2]int]Renderable // [row, col] -> content
	gapH, gapV int                   // Horizontal and vertical gap between cells
}

// Grid creates a new GridLayout with the specified dimensions.
func Grid(id string, rows, cols int) *GridLayout {
	return &GridLayout{
		id:         id,
		rows:       rows,
		cols:       cols,
		cellWidth:  10,
		cellHeight: 1,
		position:   Position{X: 0, Y: 0},
		cells:      make(map[[2]int]Renderable),
		gapH:       0,
		gapV:       0,
	}
}

// At positions the grid at specific coordinates.
func (g *GridLayout) At(pos Position) *GridLayout {
	g.position = pos
	return g
}

// WithCellSize sets uniform cell dimensions.
func (g *GridLayout) WithCellSize(width, height int) *GridLayout {
	g.cellWidth = width
	g.cellHeight = height
	return g
}

// WithGap sets spacing between cells.
func (g *GridLayout) WithGap(horizontal, vertical int) *GridLayout {
	g.gapH = horizontal
	g.gapV = vertical
	return g
}

// SetCell places a Renderable in a specific cell.
// Row and column are 0-indexed.
func (g *GridLayout) SetCell(row, col int, content Renderable) *GridLayout {
	if row >= 0 && row < g.rows && col >= 0 && col < g.cols {
		g.cells[[2]int{row, col}] = content
	}
	return g
}

// SetCellText is a convenience for placing text in a cell.
func (g *GridLayout) SetCellText(row, col int, text string) *GridLayout {
	id := g.id + "-cell-" + strconv.Itoa(row) + "-" + strconv.Itoa(col)
	block := NewTextBlockWithWidth(id, text, Position{X: 0, Y: 0}, g.cellWidth)
	return g.SetCell(row, col, block)
}

// GetID returns the unique identifier for this grid.
func (g *GridLayout) GetID() string {
	return g.id
}

// GetBounds returns the rectangular bounds that this grid occupies.
func (g *GridLayout) GetBounds() Rectangle {
	totalWidth := g.cols*g.cellWidth + (g.cols-1)*g.gapH
	totalHeight := g.rows*g.cellHeight + (g.rows-1)*g.gapV

	if g.cols <= 0 {
		totalWidth = 0
	}
	if g.rows <= 0 {
		totalHeight = 0
	}

	return Rectangle{
		X:      g.position.X,
		Y:      g.position.Y,
		Width:  totalWidth,
		Height: totalHeight,
	}
}

// getCellPosition returns the top-left position of a cell.
func (g *GridLayout) getCellPosition(row, col int) Position {
	x := g.position.X + col*(g.cellWidth+g.gapH)
	y := g.position.Y + row*(g.cellHeight+g.gapV)
	return Position{X: x, Y: y}
}

// RenderTo renders all cells in the grid to the CharSetter.
func (g *GridLayout) RenderTo(cs CharSetter) {
	for key, content := range g.cells {
		row, col := key[0], key[1]
		cellPos := g.getCellPosition(row, col)
		contentBounds := content.GetBounds()

		// Create offset CharSetter to position content in cell
		offsetCS := &offsetCharSetter{
			target:  cs,
			offsetX: cellPos.X - contentBounds.X,
			offsetY: cellPos.Y - contentBounds.Y,
		}
		content.RenderTo(offsetCS)
	}
}
