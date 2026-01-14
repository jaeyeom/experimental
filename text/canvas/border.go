package canvas

// Border wraps content with a decorative border.
type Border struct {
	id       string
	content  Renderable
	style    BorderStyle
	position Position
	title    string
	titlePos TitlePosition
	padding  Spacing
	width    int // Explicit width, 0 means auto-calculate from content
	height   int // Explicit height, 0 means auto-calculate from content
}

// NewBorder creates a bordered container for content.
func NewBorder(id string, content Renderable, style BorderStyle) *Border {
	return &Border{
		id:       id,
		content:  content,
		style:    style,
		position: Position{X: 0, Y: 0},
		title:    "",
		titlePos: TitleTopLeft,
		padding:  Spacing{Top: 0, Right: 0, Bottom: 0, Left: 0},
		width:    0,
		height:   0,
	}
}

// NewBorderWithSize creates a bordered container with explicit dimensions.
func NewBorderWithSize(id string, style BorderStyle, width, height int) *Border {
	return &Border{
		id:       id,
		content:  nil,
		style:    style,
		position: Position{X: 0, Y: 0},
		title:    "",
		titlePos: TitleTopLeft,
		padding:  Spacing{Top: 0, Right: 0, Bottom: 0, Left: 0},
		width:    width,
		height:   height,
	}
}

// At positions the border at specific coordinates.
func (b *Border) At(pos Position) *Border {
	b.position = pos
	return b
}

// WithTitle adds a title to the border.
func (b *Border) WithTitle(title string, pos TitlePosition) *Border {
	b.title = title
	b.titlePos = pos
	return b
}

// WithPadding sets internal padding between border and content.
func (b *Border) WithPadding(padding Spacing) *Border {
	b.padding = padding
	return b
}

// WithPaddingUniform sets uniform internal padding on all sides.
func (b *Border) WithPaddingUniform(padding int) *Border {
	b.padding = NewSpacing(padding)
	return b
}

// WithContent sets the content to be wrapped.
func (b *Border) WithContent(content Renderable) *Border {
	b.content = content
	return b
}

// GetID returns the unique identifier for this border.
func (b *Border) GetID() string {
	return b.id
}

// getContentBounds returns the bounds of the content area.
func (b *Border) getContentBounds() Rectangle {
	if b.content != nil {
		return b.content.GetBounds()
	}
	return Rectangle{X: 0, Y: 0, Width: 0, Height: 0}
}

// GetBounds returns the rectangular bounds that this border occupies.
func (b *Border) GetBounds() Rectangle {
	var innerWidth, innerHeight int

	if b.width > 0 && b.height > 0 {
		// Explicit size
		return Rectangle{
			X:      b.position.X,
			Y:      b.position.Y,
			Width:  b.width,
			Height: b.height,
		}
	}

	// Auto-calculate from content
	contentBounds := b.getContentBounds()
	innerWidth = contentBounds.Width + b.padding.Left + b.padding.Right
	innerHeight = contentBounds.Height + b.padding.Top + b.padding.Bottom

	// Add 2 for the border itself (1 char on each side)
	return Rectangle{
		X:      b.position.X,
		Y:      b.position.Y,
		Width:  innerWidth + 2,
		Height: innerHeight + 2,
	}
}

// getTitlePosition calculates the position for the title based on title position setting.
func (b *Border) getTitlePosition(bounds Rectangle, titleLen int) (int, int) {
	var titleX, titleY int

	switch b.titlePos {
	case TitleTopLeft:
		titleX = bounds.X + 2
		titleY = bounds.Y
	case TitleTopCenter:
		titleX = bounds.X + (bounds.Width-titleLen)/2
		titleY = bounds.Y
	case TitleTopRight:
		titleX = bounds.X + bounds.Width - titleLen - 2
		titleY = bounds.Y
	case TitleBottomLeft:
		titleX = bounds.X + 2
		titleY = bounds.Y + bounds.Height - 1
	case TitleBottomCenter:
		titleX = bounds.X + (bounds.Width-titleLen)/2
		titleY = bounds.Y + bounds.Height - 1
	case TitleBottomRight:
		titleX = bounds.X + bounds.Width - titleLen - 2
		titleY = bounds.Y + bounds.Height - 1
	}

	return titleX, titleY
}

// renderTitle draws the title on the border.
func (b *Border) renderTitle(cs CharSetter, bounds Rectangle) {
	if b.title == "" {
		return
	}

	titleRunes := []rune(b.title)
	titleLen := len(titleRunes)
	titleX, titleY := b.getTitlePosition(bounds, titleLen)

	// Ensure title stays within bounds
	titleX = max(titleX, bounds.X+1)
	if titleX+titleLen > bounds.X+bounds.Width-1 {
		titleLen = bounds.X + bounds.Width - 1 - titleX
	}

	for i := 0; i < titleLen && i < len(titleRunes); i++ {
		cs.SetChar(titleX+i, titleY, titleRunes[i])
	}
}

// RenderTo renders the border and its content to the CharSetter.
func (b *Border) RenderTo(cs CharSetter) {
	if b.style == BorderNone {
		// No border, just render content if present
		if b.content != nil {
			offsetCS := &offsetCharSetter{
				target:  cs,
				offsetX: b.position.X + b.padding.Left - b.getContentBounds().X,
				offsetY: b.position.Y + b.padding.Top - b.getContentBounds().Y,
			}
			b.content.RenderTo(offsetCS)
		}
		return
	}

	bounds := b.GetBounds()
	chars := GetBoxChars(b.style)

	// Draw corners
	cs.SetChar(bounds.X, bounds.Y, chars.TopLeft)
	cs.SetChar(bounds.X+bounds.Width-1, bounds.Y, chars.TopRight)
	cs.SetChar(bounds.X, bounds.Y+bounds.Height-1, chars.BottomLeft)
	cs.SetChar(bounds.X+bounds.Width-1, bounds.Y+bounds.Height-1, chars.BottomRight)

	// Draw top and bottom edges
	for x := bounds.X + 1; x < bounds.X+bounds.Width-1; x++ {
		cs.SetChar(x, bounds.Y, chars.Horizontal)
		cs.SetChar(x, bounds.Y+bounds.Height-1, chars.Horizontal)
	}

	// Draw left and right edges
	for y := bounds.Y + 1; y < bounds.Y+bounds.Height-1; y++ {
		cs.SetChar(bounds.X, y, chars.Vertical)
		cs.SetChar(bounds.X+bounds.Width-1, y, chars.Vertical)
	}

	// Draw title
	b.renderTitle(cs, bounds)

	// Render content if present
	if b.content != nil {
		contentBounds := b.getContentBounds()
		offsetCS := &offsetCharSetter{
			target:  cs,
			offsetX: bounds.X + 1 + b.padding.Left - contentBounds.X,
			offsetY: bounds.Y + 1 + b.padding.Top - contentBounds.Y,
		}
		b.content.RenderTo(offsetCS)
	}
}

// AddBorder is a convenience function to create a bordered text block.
func AddBorder(id string, text string, style BorderStyle) *Border {
	textBlock := NewTextBlock(id+"-content", text, Position{X: 0, Y: 0})
	return NewBorder(id, textBlock, style)
}
