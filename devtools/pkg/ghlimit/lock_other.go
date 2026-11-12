//go:build !darwin && !linux

package ghlimit

import "os"

// tryLock always succeeds where flock is not wired up, so calls run
// without the cross-process limit.
func tryLock(*os.File) (bool, error) { return true, nil }
