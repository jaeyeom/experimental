package canvas

// FlowLayout arranges children in a flowing manner, wrapping to new lines
// when the maximum width is exceeded.
type FlowLayout struct {
	id        string
	position  Position
	maxWidth  int
	children  []Renderable
	gapH      int // Horizontal gap between children
	gapV      int // Vertical gap between rows
	alignment Alignment
}

// NewFlowLayout creates a new FlowLayout with specified maximum width.
func NewFlowLayout(id string, maxWidth int) *FlowLayout {
	return &FlowLayout{
		id:        id,
		position:  Position{X: 0, Y: 0},
		maxWidth:  maxWidth,
		children:  make([]Renderable, 0),
		gapH:      1,
		gapV:      0,
		alignment: AlignLeft,
	}
}

// At positions the layout at specific coordinates.
func (fl *FlowLayout) At(pos Position) *FlowLayout {
	fl.position = pos
	return fl
}

// WithGap sets spacing between children.
func (fl *FlowLayout) WithGap(horizontal, vertical int) *FlowLayout {
	fl.gapH = horizontal
	fl.gapV = vertical
	return fl
}

// WithAlignment sets how items align within their row.
func (fl *FlowLayout) WithAlignment(align Alignment) *FlowLayout {
	fl.alignment = align
	return fl
}

// Add appends a Renderable to the flow.
func (fl *FlowLayout) Add(content Renderable) *FlowLayout {
	fl.children = append(fl.children, content)
	return fl
}

// AddText is a convenience for adding text blocks with specific width.
func (fl *FlowLayout) AddText(id, text string, width int) *FlowLayout {
	block := NewTextBlockWithWidth(id, text, Position{X: 0, Y: 0}, width)
	fl.children = append(fl.children, block)
	return fl
}

// GetID returns the unique identifier for this layout.
func (fl *FlowLayout) GetID() string {
	return fl.id
}

// layoutInfo holds information about how children are arranged in rows.
type layoutInfo struct {
	rows       [][]int // Each row contains indices of children
	rowHeights []int   // Height of each row
	rowWidths  []int   // Total width used by each row
}

// calculateLayout determines how children are arranged in rows.
func (fl *FlowLayout) calculateLayout() layoutInfo {
	info := layoutInfo{
		rows:       make([][]int, 0),
		rowHeights: make([]int, 0),
		rowWidths:  make([]int, 0),
	}

	if len(fl.children) == 0 {
		return info
	}

	currentRow := make([]int, 0)
	currentRowWidth := 0
	currentRowHeight := 0

	for i, child := range fl.children {
		bounds := child.GetBounds()
		childWidth := bounds.Width

		// Check if adding this child would exceed maxWidth
		newWidth := currentRowWidth + childWidth
		if len(currentRow) > 0 {
			newWidth += fl.gapH
		}

		if len(currentRow) > 0 && newWidth > fl.maxWidth {
			// Start a new row
			info.rows = append(info.rows, currentRow)
			info.rowHeights = append(info.rowHeights, currentRowHeight)
			info.rowWidths = append(info.rowWidths, currentRowWidth)

			currentRow = make([]int, 0)
			currentRowWidth = 0
			currentRowHeight = 0
		}

		// Add child to current row
		if len(currentRow) > 0 {
			currentRowWidth += fl.gapH
		}
		currentRow = append(currentRow, i)
		currentRowWidth += childWidth
		if bounds.Height > currentRowHeight {
			currentRowHeight = bounds.Height
		}
	}

	// Add the last row
	if len(currentRow) > 0 {
		info.rows = append(info.rows, currentRow)
		info.rowHeights = append(info.rowHeights, currentRowHeight)
		info.rowWidths = append(info.rowWidths, currentRowWidth)
	}

	return info
}

// GetBounds returns the rectangular bounds that this layout occupies.
func (fl *FlowLayout) GetBounds() Rectangle {
	info := fl.calculateLayout()

	if len(info.rows) == 0 {
		return Rectangle{X: fl.position.X, Y: fl.position.Y, Width: 0, Height: 0}
	}

	totalHeight := 0
	maxWidth := 0

	for i, rowHeight := range info.rowHeights {
		totalHeight += rowHeight
		if i > 0 {
			totalHeight += fl.gapV
		}
		if info.rowWidths[i] > maxWidth {
			maxWidth = info.rowWidths[i]
		}
	}

	return Rectangle{
		X:      fl.position.X,
		Y:      fl.position.Y,
		Width:  maxWidth,
		Height: totalHeight,
	}
}

// RenderTo renders all children in the flow layout to the CharSetter.
func (fl *FlowLayout) RenderTo(cs CharSetter) {
	info := fl.calculateLayout()

	if len(info.rows) == 0 {
		return
	}

	currentY := fl.position.Y

	for rowIdx, row := range info.rows {
		// Calculate row offset for alignment
		var rowStartX int
		switch fl.alignment {
		case AlignCenter:
			rowStartX = fl.position.X + (fl.maxWidth-info.rowWidths[rowIdx])/2
		case AlignRight:
			rowStartX = fl.position.X + fl.maxWidth - info.rowWidths[rowIdx]
		default: // AlignLeft
			rowStartX = fl.position.X
		}

		currentX := rowStartX
		rowHeight := info.rowHeights[rowIdx]

		for _, childIdx := range row {
			child := fl.children[childIdx]
			childBounds := child.GetBounds()

			// Center child vertically within the row
			childOffsetY := (rowHeight - childBounds.Height) / 2

			offsetCS := &offsetCharSetter{
				target:  cs,
				offsetX: currentX - childBounds.X,
				offsetY: currentY + childOffsetY - childBounds.Y,
			}
			child.RenderTo(offsetCS)

			currentX += childBounds.Width + fl.gapH
		}

		currentY += rowHeight + fl.gapV
	}
}
