package canvas

// Stack arranges children in a vertical or horizontal stack.
// Children are rendered sequentially with optional gap between them.
type Stack struct {
	id        string
	direction Direction
	position  Position
	children  []Renderable
	gap       int
	alignment Alignment
}

// NewStack creates a new Stack with the specified direction.
func NewStack(id string, direction Direction) *Stack {
	return &Stack{
		id:        id,
		direction: direction,
		position:  Position{X: 0, Y: 0},
		children:  make([]Renderable, 0),
		gap:       0,
		alignment: AlignLeft,
	}
}

// At positions the stack at specific coordinates.
func (s *Stack) At(pos Position) *Stack {
	s.position = pos
	return s
}

// WithGap sets spacing between children.
func (s *Stack) WithGap(gap int) *Stack {
	s.gap = gap
	return s
}

// WithAlignment sets cross-axis alignment for children.
// For vertical stacks, this aligns horizontally.
// For horizontal stacks, this aligns vertically.
func (s *Stack) WithAlignment(align Alignment) *Stack {
	s.alignment = align
	return s
}

// Add appends a Renderable to the stack.
func (s *Stack) Add(content Renderable) *Stack {
	s.children = append(s.children, content)
	return s
}

// AddText is a convenience for adding text blocks.
func (s *Stack) AddText(id, text string) *Stack {
	block := NewTextBlock(id, text, Position{X: 0, Y: 0})
	s.children = append(s.children, block)
	return s
}

// AddTextWithWidth adds text with specific width for wrapping.
func (s *Stack) AddTextWithWidth(id, text string, width int) *Stack {
	block := NewTextBlockWithWidth(id, text, Position{X: 0, Y: 0}, width)
	s.children = append(s.children, block)
	return s
}

// GetID returns the unique identifier for this stack.
func (s *Stack) GetID() string {
	return s.id
}

// GetBounds returns the rectangular bounds that this stack occupies.
func (s *Stack) GetBounds() Rectangle {
	if len(s.children) == 0 {
		return Rectangle{X: s.position.X, Y: s.position.Y, Width: 0, Height: 0}
	}

	totalWidth := 0
	totalHeight := 0

	switch s.direction {
	case DirectionVertical:
		maxWidth := 0
		for i, child := range s.children {
			bounds := child.GetBounds()
			if bounds.Width > maxWidth {
				maxWidth = bounds.Width
			}
			totalHeight += bounds.Height
			if i > 0 {
				totalHeight += s.gap
			}
		}
		totalWidth = maxWidth

	case DirectionHorizontal:
		maxHeight := 0
		for i, child := range s.children {
			bounds := child.GetBounds()
			if bounds.Height > maxHeight {
				maxHeight = bounds.Height
			}
			totalWidth += bounds.Width
			if i > 0 {
				totalWidth += s.gap
			}
		}
		totalHeight = maxHeight
	}

	return Rectangle{
		X:      s.position.X,
		Y:      s.position.Y,
		Width:  totalWidth,
		Height: totalHeight,
	}
}

// RenderTo renders all children in the stack to the CharSetter.
func (s *Stack) RenderTo(cs CharSetter) {
	if len(s.children) == 0 {
		return
	}

	// Calculate total bounds for alignment
	bounds := s.GetBounds()

	currentX := s.position.X
	currentY := s.position.Y

	for i, child := range s.children {
		childBounds := child.GetBounds()

		// Calculate aligned position based on direction and alignment
		var offsetX, offsetY int

		switch s.direction {
		case DirectionVertical:
			// Align horizontally (cross-axis)
			switch s.alignment {
			case AlignCenter:
				offsetX = (bounds.Width - childBounds.Width) / 2
			case AlignRight:
				offsetX = bounds.Width - childBounds.Width
			default: // AlignLeft
				offsetX = 0
			}
			offsetY = 0

		case DirectionHorizontal:
			// Align vertically (cross-axis)
			offsetX = 0
			switch s.alignment {
			case AlignCenter:
				offsetY = (bounds.Height - childBounds.Height) / 2
			case AlignRight: // Using AlignRight as bottom alignment
				offsetY = bounds.Height - childBounds.Height
			default: // AlignLeft as top alignment
				offsetY = 0
			}
		}

		// Create offset CharSetter for this child
		offsetCS := &offsetCharSetter{
			target:  cs,
			offsetX: currentX + offsetX - childBounds.X,
			offsetY: currentY + offsetY - childBounds.Y,
		}
		child.RenderTo(offsetCS)

		// Move to next position
		if s.direction == DirectionVertical {
			currentY += childBounds.Height + s.gap
		} else {
			currentX += childBounds.Width + s.gap
		}

		// Skip gap calculation for last child (already handled in loop)
		_ = i
	}
}
