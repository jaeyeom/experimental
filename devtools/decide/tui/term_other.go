//go:build !darwin && !linux

package tui

import "io"

func makeRaw(io.Reader) (func(), bool, error) {
	return func() {}, false, nil
}

func termRows(io.Reader) int {
	return 0
}
