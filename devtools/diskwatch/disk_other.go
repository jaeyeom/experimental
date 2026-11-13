//go:build !linux && !darwin

package diskwatch

import "fmt"

func statUsage(path string) (Usage, error) {
	return Usage{}, fmt.Errorf("statfs %s: diskwatch supports linux and darwin", path)
}

func (osLister) lstat(path string) (nodeInfo, error) {
	return nodeInfo{}, fmt.Errorf("lstat %s: diskwatch supports linux and darwin", path)
}

// Stat reads free and total bytes for path.
func Stat(path string) (Usage, error) {
	return statUsage(path)
}
