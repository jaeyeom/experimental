//go:build linux || darwin

package healthsnap

import (
	"fmt"

	"golang.org/x/sys/unix"
)

func statDiskUsage(path string) (uint64, uint64, error) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return 0, 0, fmt.Errorf("statfs %s: %w", path, err)
	}
	bsize, err := diskBlockSize(st.Bsize)
	if err != nil {
		return 0, 0, fmt.Errorf("statfs %s: %w", path, err)
	}
	return st.Bavail * bsize, st.Blocks * bsize, nil
}
