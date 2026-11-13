//go:build darwin

package diskwatch

import "fmt"

func blockSize(bsize uint32) (uint64, error) {
	if bsize == 0 {
		return 0, fmt.Errorf("invalid block size")
	}
	return uint64(bsize), nil
}
