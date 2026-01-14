package canvas

// Center creates a TextBlock that centers text within the given width.
// The text block is positioned at (0, 0) by default.
func Center(id string, text string, width int) TextBlock {
	return TextBlock{
		ID:       id,
		Text:     text,
		Position: Position{X: 0, Y: 0},
		Width:    width,
		WrapMode: WrapBasic,
		Align:    AlignCenter,
	}
}

// CenterAt creates a centered TextBlock at a specific position.
func CenterAt(id string, text string, width int, pos Position) TextBlock {
	return TextBlock{
		ID:       id,
		Text:     text,
		Position: pos,
		Width:    width,
		WrapMode: WrapBasic,
		Align:    AlignCenter,
	}
}

// MarginWrapper wraps a Renderable with margin spacing.
// The margin adds offset to the content's position without changing its size.
type MarginWrapper struct {
	id      string
	content Renderable
	margin  Spacing
}

// NewMargin creates a MarginWrapper with specified margins.
func NewMargin(id string, content Renderable, top, right, bottom, left int) *MarginWrapper {
	return &MarginWrapper{
		id:      id,
		content: content,
		margin:  Spacing{Top: top, Right: right, Bottom: bottom, Left: left},
	}
}

// NewMarginUniform creates a MarginWrapper with uniform margins on all sides.
func NewMarginUniform(id string, content Renderable, margin int) *MarginWrapper {
	return &MarginWrapper{
		id:      id,
		content: content,
		margin:  NewSpacing(margin),
	}
}

// GetID returns the unique identifier for this margin wrapper.
func (m *MarginWrapper) GetID() string {
	return m.id
}

// GetBounds returns the rectangular bounds including the margin.
func (m *MarginWrapper) GetBounds() Rectangle {
	contentBounds := m.content.GetBounds()
	return Rectangle{
		X:      contentBounds.X - m.margin.Left,
		Y:      contentBounds.Y - m.margin.Top,
		Width:  contentBounds.Width + m.margin.Left + m.margin.Right,
		Height: contentBounds.Height + m.margin.Top + m.margin.Bottom,
	}
}

// RenderTo renders the wrapped content with margin offset applied.
func (m *MarginWrapper) RenderTo(cs CharSetter) {
	// Create an offset CharSetter that applies the margin
	offsetCS := &offsetCharSetter{
		target:  cs,
		offsetX: m.margin.Left,
		offsetY: m.margin.Top,
	}
	m.content.RenderTo(offsetCS)
}

// offsetCharSetter applies position offset when setting characters.
type offsetCharSetter struct {
	target           CharSetter
	offsetX, offsetY int
}

// SetChar sets a character at the offset position.
func (ocs *offsetCharSetter) SetChar(x, y int, r rune) bool {
	return ocs.target.SetChar(x+ocs.offsetX, y+ocs.offsetY, r)
}

// Title represents a title element with optional underline.
type Title struct {
	id         string
	text       string
	position   Position
	underline  bool
	underStyle LineStyle
	width      int
}

// NewTitle creates a new title element.
func NewTitle(id string, text string) *Title {
	return &Title{
		id:         id,
		text:       text,
		position:   Position{X: 0, Y: 0},
		underline:  false,
		underStyle: LineStyleSingle,
		width:      len([]rune(text)),
	}
}

// At positions the title at specific coordinates.
func (t *Title) At(pos Position) *Title {
	t.position = pos
	return t
}

// WithUnderline adds an underline to the title.
func (t *Title) WithUnderline(style LineStyle) *Title {
	t.underline = true
	t.underStyle = style
	return t
}

// WithWidth sets the width for the title (affects underline length).
func (t *Title) WithWidth(width int) *Title {
	t.width = width
	return t
}

// GetID returns the unique identifier for this title.
func (t *Title) GetID() string {
	return t.id
}

// GetBounds returns the rectangular bounds that this title occupies.
func (t *Title) GetBounds() Rectangle {
	height := 1
	if t.underline {
		height = 2
	}
	return Rectangle{
		X:      t.position.X,
		Y:      t.position.Y,
		Width:  t.width,
		Height: height,
	}
}

// RenderTo renders the title to any CharSetter implementation.
func (t *Title) RenderTo(cs CharSetter) {
	// Render the title text
	for i, r := range []rune(t.text) {
		cs.SetChar(t.position.X+i, t.position.Y, r)
	}

	// Render underline if enabled
	if t.underline {
		line := Line{
			ID:        t.id + "-underline",
			Start:     Position{X: t.position.X, Y: t.position.Y + 1},
			Length:    t.width,
			Direction: LineDirectionHorizontal,
			Style:     t.underStyle,
		}
		line.RenderTo(cs)
	}
}
