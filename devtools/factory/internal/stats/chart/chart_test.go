package chart

import (
	"slices"
	"testing"
)

func TestStack(t *testing.T) {
	shaded := []Part{{Fill: '#', Shade: '█'}, {Fill: '=', Shade: '▒'}}
	tests := []struct {
		name   string
		values []float64
		parts  []Part
		scale  float64
		width  int
		style  Style
		want   []string
	}{
		{name: "zero scale", values: []float64{0, 0}, scale: 0, width: 4, want: []string{"", ""}},
		{name: "full", values: []float64{5}, scale: 5, width: 3, want: []string{"███"}},
		{name: "scaled to the largest", values: []float64{2}, scale: 8, width: 12, want: []string{"███"}},
		{name: "eighths", values: []float64{1}, scale: 16, width: 12, want: []string{"▊"}},
		{name: "tiny part gets a cell", values: []float64{1, 999}, scale: 1000, width: 12, want: []string{"▏", "███████████"}},
		{name: "never wider than width", values: []float64{1, 1, 1}, scale: 1.5, width: 4, want: []string{"█", "█", "██"}},
		{name: "more parts than cells", values: []float64{1, 1, 1}, scale: 3, width: 2, want: []string{"▋", "▋", ""}},
		{name: "shades without color", values: []float64{3, 1}, parts: shaded, scale: 4, width: 4, want: []string{"███", "▒"}},
		{name: "blocks with color", values: []float64{3, 1}, parts: shaded, scale: 8, width: 4, style: Style{Color: true}, want: []string{"█▌", "▌"}},
		{name: "ascii", values: []float64{1, 1}, parts: shaded, scale: 2, width: 4, style: Style{ASCII: true}, want: []string{"##", "=="}},
		{name: "no width", values: []float64{1}, scale: 1, width: 0, want: []string{""}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			parts := make([]Part, len(tc.values))
			for i, v := range tc.values {
				if i < len(tc.parts) {
					parts[i] = tc.parts[i]
				}
				parts[i].Value = v
			}
			got := Stack(parts, tc.scale, tc.width, tc.style)
			if !slices.Equal(got, tc.want) {
				t.Fatalf("Stack = %q, want %q", got, tc.want)
			}
			n := 0
			for i, seg := range got {
				n += len([]rune(seg))
				if tc.values[i] > 0 && seg == "" && tc.width >= len(tc.values) && tc.scale > 0 {
					t.Fatalf("part %d is above zero but empty: %q", i, got)
				}
			}
			if n > max(tc.width, 0) {
				t.Fatalf("Stack used %d cells, width %d", n, tc.width)
			}
		})
	}
}

func TestSparkline(t *testing.T) {
	if got := Sparkline([]float64{0, 1, 2, 4}, Style{}); got != "▁▃▅█" {
		t.Fatalf("Sparkline = %q", got)
	}
	if got := Sparkline([]float64{0, 1, 2, 4}, Style{ASCII: true}); got != "_.=#" {
		t.Fatalf("ascii Sparkline = %q", got)
	}
	if got := Sparkline([]float64{0, 0}, Style{}); got != "▁▁" {
		t.Fatalf("zero Sparkline = %q", got)
	}
}

func TestBraille(t *testing.T) {
	if got := Braille([][]float64{{0, 0}}, nil, 1, 1, Style{}); !slices.Equal(got, []string{"⣀"}) {
		t.Fatalf("flat zero = %q", got)
	}
	if got := Braille([][]float64{{1, 1}}, nil, 1, 1, Style{}); !slices.Equal(got, []string{"⠉"}) {
		t.Fatalf("flat max = %q", got)
	}
	if got := Braille([][]float64{{1, 1}}, nil, 1, 1, Style{ASCII: true}); !slices.Equal(got, []string{"*"}) {
		t.Fatalf("ascii = %q", got)
	}
	// A falling line over 2 rows: the top-left and bottom-right are set.
	got := Braille([][]float64{{1, 0}}, nil, 2, 2, Style{ASCII: true})
	if len(got) != 2 || got[0][0] != '*' || got[1][1] != '*' {
		t.Fatalf("falling = %q", got)
	}
	if got := Braille(nil, nil, 0, 2, Style{}); got != nil {
		t.Fatalf("no width = %q", got)
	}
}

func TestVBars(t *testing.T) {
	if got := VBars([]float64{1, 2}, 1, Style{}); !slices.Equal(got, []string{"▄█"}) {
		t.Fatalf("VBars = %q", got)
	}
	got := VBars([]float64{0, 2}, 2, Style{ASCII: true})
	if !slices.Equal(got, []string{" #", " #"}) {
		t.Fatalf("ascii VBars = %q", got)
	}
}
