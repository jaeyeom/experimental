package canvas

// Direction specifies the stacking direction for layout containers.
type Direction int

const (
	// DirectionVertical stacks children from top to bottom.
	DirectionVertical Direction = iota
	// DirectionHorizontal stacks children from left to right.
	DirectionHorizontal
)

// BorderStyle specifies the visual style for borders.
type BorderStyle int

const (
	// BorderNone renders no border.
	BorderNone BorderStyle = iota
	// BorderSingle uses single-line box drawing characters.
	BorderSingle
	// BorderDouble uses double-line box drawing characters.
	BorderDouble
	// BorderRounded uses rounded corner box drawing characters.
	BorderRounded
	// BorderASCII uses ASCII characters (+, -, |).
	BorderASCII
)

// TitlePosition specifies where titles appear on a border.
type TitlePosition int

const (
	// TitleTopLeft places the title at the top-left of the border.
	TitleTopLeft TitlePosition = iota
	// TitleTopCenter places the title at the top-center of the border.
	TitleTopCenter
	// TitleTopRight places the title at the top-right of the border.
	TitleTopRight
	// TitleBottomLeft places the title at the bottom-left of the border.
	TitleBottomLeft
	// TitleBottomCenter places the title at the bottom-center of the border.
	TitleBottomCenter
	// TitleBottomRight places the title at the bottom-right of the border.
	TitleBottomRight
)

// Spacing represents margin or padding values for all four sides.
type Spacing struct {
	Top, Right, Bottom, Left int
}

// NewSpacing creates a Spacing with all sides set to the same value.
func NewSpacing(all int) Spacing {
	return Spacing{Top: all, Right: all, Bottom: all, Left: all}
}

// NewSpacingSymmetric creates a Spacing with symmetric vertical and horizontal values.
func NewSpacingSymmetric(vertical, horizontal int) Spacing {
	return Spacing{Top: vertical, Right: horizontal, Bottom: vertical, Left: horizontal}
}

// NewSpacingTRBL creates a Spacing with explicit top, right, bottom, left values.
func NewSpacingTRBL(top, right, bottom, left int) Spacing {
	return Spacing{Top: top, Right: right, Bottom: bottom, Left: left}
}

// Region identifies a region in a BorderLayout.
type Region int

const (
	// RegionNorth is the top region of a BorderLayout.
	RegionNorth Region = iota
	// RegionSouth is the bottom region of a BorderLayout.
	RegionSouth
	// RegionEast is the right region of a BorderLayout.
	RegionEast
	// RegionWest is the left region of a BorderLayout.
	RegionWest
	// RegionCenter is the center region of a BorderLayout.
	RegionCenter
)
