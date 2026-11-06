//go:build !darwin && !linux

package tui

import "io"

func termRows(io.Reader) int {
	return 0
}
