//go:build !darwin && !linux

package tui

import "io"

func makeRaw(io.Reader) (func(), error) {
	return func() {}, nil
}
