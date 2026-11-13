//go:build linux || darwin

package diskwatch

import (
	"fmt"

	"golang.org/x/sys/unix"
)

func statUsage(path string) (Usage, error) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return Usage{}, fmt.Errorf("statfs %s: %w", path, err)
	}
	bsize, err := blockSize(st.Bsize)
	if err != nil {
		return Usage{}, fmt.Errorf("statfs %s: %w", path, err)
	}
	return Usage{Free: st.Bavail * bsize, Total: st.Blocks * bsize}, nil
}

func (osLister) lstat(path string) (nodeInfo, error) {
	var st unix.Stat_t
	if err := unix.Lstat(path, &st); err != nil {
		return nodeInfo{}, fmt.Errorf("lstat %s: %w", path, err)
	}
	return nodeInfo{
		isDir: st.Mode&unix.S_IFMT == unix.S_IFDIR,
		size:  st.Size,
		dev:   uint64(st.Dev), //nolint:gosec // G115: a device id is non-negative
	}, nil
}

// Stat reads free and total bytes for path.
func Stat(path string) (Usage, error) {
	return statUsage(path)
}
