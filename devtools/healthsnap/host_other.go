//go:build !linux && !darwin

package healthsnap

import (
	"context"
	"errors"
	"fmt"
)

func readHostLoad(context.Context) (LoadStats, error) {
	return LoadStats{}, errors.New("host load checks support linux and darwin")
}

func readHostMemory(context.Context) (MemoryStats, error) {
	return MemoryStats{}, errors.New("host memory checks support linux and darwin")
}

func statDiskUsage(path string) (uint64, uint64, error) {
	return 0, 0, fmt.Errorf("statfs %s: host disk checks support linux and darwin", path)
}
