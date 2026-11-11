//go:build !darwin && !linux

package tui

import (
	"io"
	"os"
	"time"
)

func termRows(io.Reader) int {
	return 0
}

// inputReady never waits here, so a split Ctrl+Enter sequence passes
// through as it arrived.
func inputReady(*os.File) func(time.Duration) bool {
	return nil
}
