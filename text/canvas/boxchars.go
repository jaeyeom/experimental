package canvas

// BoxChars holds the characters used for drawing borders.
type BoxChars struct {
	TopLeft     rune
	TopRight    rune
	BottomLeft  rune
	BottomRight rune
	Horizontal  rune
	Vertical    rune
	TeeLeft     rune // T pointing left (right side of box)
	TeeRight    rune // T pointing right (left side of box)
	TeeTop      rune // T pointing up (bottom of box)
	TeeBottom   rune // T pointing down (top of box)
	Cross       rune
}

// SingleBoxChars uses single-line Unicode box drawing characters.
var SingleBoxChars = BoxChars{
	TopLeft:     '┌',
	TopRight:    '┐',
	BottomLeft:  '└',
	BottomRight: '┘',
	Horizontal:  '─',
	Vertical:    '│',
	TeeLeft:     '┤',
	TeeRight:    '├',
	TeeTop:      '┴',
	TeeBottom:   '┬',
	Cross:       '┼',
}

// DoubleBoxChars uses double-line Unicode box drawing characters.
var DoubleBoxChars = BoxChars{
	TopLeft:     '╔',
	TopRight:    '╗',
	BottomLeft:  '╚',
	BottomRight: '╝',
	Horizontal:  '═',
	Vertical:    '║',
	TeeLeft:     '╣',
	TeeRight:    '╠',
	TeeTop:      '╩',
	TeeBottom:   '╦',
	Cross:       '╬',
}

// RoundedBoxChars uses rounded corner Unicode box drawing characters.
var RoundedBoxChars = BoxChars{
	TopLeft:     '╭',
	TopRight:    '╮',
	BottomLeft:  '╰',
	BottomRight: '╯',
	Horizontal:  '─',
	Vertical:    '│',
	TeeLeft:     '┤',
	TeeRight:    '├',
	TeeTop:      '┴',
	TeeBottom:   '┬',
	Cross:       '┼',
}

// ASCIIBoxChars uses ASCII characters for maximum compatibility.
var ASCIIBoxChars = BoxChars{
	TopLeft:     '+',
	TopRight:    '+',
	BottomLeft:  '+',
	BottomRight: '+',
	Horizontal:  '-',
	Vertical:    '|',
	TeeLeft:     '+',
	TeeRight:    '+',
	TeeTop:      '+',
	TeeBottom:   '+',
	Cross:       '+',
}

// GetBoxChars returns the box drawing characters for a given border style.
func GetBoxChars(style BorderStyle) BoxChars {
	switch style {
	case BorderSingle:
		return SingleBoxChars
	case BorderDouble:
		return DoubleBoxChars
	case BorderRounded:
		return RoundedBoxChars
	case BorderASCII:
		return ASCIIBoxChars
	default:
		return SingleBoxChars
	}
}
