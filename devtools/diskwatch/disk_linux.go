//go:build linux

package diskwatch

import "fmt"

func blockSize(bsize int64) (uint64, error) {
	if bsize <= 0 {
		return 0, fmt.Errorf("invalid block size")
	}
	return uint64(bsize), nil //nolint:gosec // G115: bsize is positive and a block size fits
}
