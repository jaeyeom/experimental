package canvas

// BorderLayout divides space into five regions: North, South, East, West, Center.
// North and South span the full width, East and West span the remaining height,
// and Center fills the remaining space.
type BorderLayout struct {
	id       string
	position Position
	width    int
	height   int
	regions  map[Region]Renderable
	gap      int // Gap between regions
}

// NewBorderLayout creates a new BorderLayout with specified dimensions.
func NewBorderLayout(id string, width, height int) *BorderLayout {
	return &BorderLayout{
		id:       id,
		position: Position{X: 0, Y: 0},
		width:    width,
		height:   height,
		regions:  make(map[Region]Renderable),
		gap:      0,
	}
}

// At positions the layout at specific coordinates.
func (bl *BorderLayout) At(pos Position) *BorderLayout {
	bl.position = pos
	return bl
}

// WithGap sets spacing between regions.
func (bl *BorderLayout) WithGap(gap int) *BorderLayout {
	bl.gap = gap
	return bl
}

// SetRegion places content in a specific region.
func (bl *BorderLayout) SetRegion(region Region, content Renderable) *BorderLayout {
	bl.regions[region] = content
	return bl
}

// SetRegionText is a convenience for placing text in a region.
func (bl *BorderLayout) SetRegionText(region Region, text string) *BorderLayout {
	id := bl.id + "-" + bl.regionName(region)
	block := NewTextBlock(id, text, Position{X: 0, Y: 0})
	bl.regions[region] = block
	return bl
}

// regionName returns a string name for a region.
func (bl *BorderLayout) regionName(region Region) string {
	switch region {
	case RegionNorth:
		return "north"
	case RegionSouth:
		return "south"
	case RegionEast:
		return "east"
	case RegionWest:
		return "west"
	case RegionCenter:
		return "center"
	default:
		return "unknown"
	}
}

// GetID returns the unique identifier for this layout.
func (bl *BorderLayout) GetID() string {
	return bl.id
}

// GetBounds returns the rectangular bounds that this layout occupies.
func (bl *BorderLayout) GetBounds() Rectangle {
	return Rectangle{
		X:      bl.position.X,
		Y:      bl.position.Y,
		Width:  bl.width,
		Height: bl.height,
	}
}

// getRegionBounds calculates the actual bounds for each region.
func (bl *BorderLayout) getRegionBounds() map[Region]Rectangle {
	bounds := make(map[Region]Rectangle)

	// Calculate heights of north and south regions
	northHeight := 0
	if north, ok := bl.regions[RegionNorth]; ok {
		northHeight = north.GetBounds().Height
	}

	southHeight := 0
	if south, ok := bl.regions[RegionSouth]; ok {
		southHeight = south.GetBounds().Height
	}

	// Calculate widths of east and west regions
	westWidth := 0
	if west, ok := bl.regions[RegionWest]; ok {
		westWidth = west.GetBounds().Width
	}

	eastWidth := 0
	if east, ok := bl.regions[RegionEast]; ok {
		eastWidth = east.GetBounds().Width
	}

	// Calculate remaining center dimensions
	centerTop := bl.position.Y + northHeight
	if northHeight > 0 {
		centerTop += bl.gap
	}

	centerBottom := bl.position.Y + bl.height - southHeight
	if southHeight > 0 {
		centerBottom -= bl.gap
	}

	centerLeft := bl.position.X + westWidth
	if westWidth > 0 {
		centerLeft += bl.gap
	}

	centerRight := bl.position.X + bl.width - eastWidth
	if eastWidth > 0 {
		centerRight -= bl.gap
	}

	middleHeight := max(centerBottom-centerTop, 0)
	centerWidth := max(centerRight-centerLeft, 0)

	// North: full width, at top
	if northHeight > 0 {
		bounds[RegionNorth] = Rectangle{
			X:      bl.position.X,
			Y:      bl.position.Y,
			Width:  bl.width,
			Height: northHeight,
		}
	}

	// South: full width, at bottom
	if southHeight > 0 {
		bounds[RegionSouth] = Rectangle{
			X:      bl.position.X,
			Y:      bl.position.Y + bl.height - southHeight,
			Width:  bl.width,
			Height: southHeight,
		}
	}

	// West: remaining height, at left
	if westWidth > 0 {
		bounds[RegionWest] = Rectangle{
			X:      bl.position.X,
			Y:      centerTop,
			Width:  westWidth,
			Height: middleHeight,
		}
	}

	// East: remaining height, at right
	if eastWidth > 0 {
		bounds[RegionEast] = Rectangle{
			X:      bl.position.X + bl.width - eastWidth,
			Y:      centerTop,
			Width:  eastWidth,
			Height: middleHeight,
		}
	}

	// Center: remaining space
	if _, ok := bl.regions[RegionCenter]; ok {
		bounds[RegionCenter] = Rectangle{
			X:      centerLeft,
			Y:      centerTop,
			Width:  centerWidth,
			Height: middleHeight,
		}
	}

	return bounds
}

// RenderTo renders all regions to the CharSetter.
func (bl *BorderLayout) RenderTo(cs CharSetter) {
	regionBounds := bl.getRegionBounds()

	for region, content := range bl.regions {
		bounds, ok := regionBounds[region]
		if !ok {
			continue
		}

		contentBounds := content.GetBounds()

		// Create offset CharSetter to position content in region
		offsetCS := &offsetCharSetter{
			target:  cs,
			offsetX: bounds.X - contentBounds.X,
			offsetY: bounds.Y - contentBounds.Y,
		}
		content.RenderTo(offsetCS)
	}
}
