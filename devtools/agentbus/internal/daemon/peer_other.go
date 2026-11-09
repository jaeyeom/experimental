//go:build !darwin && !linux

package daemon

import "errors"

func peerPIDFromFD(int) (int, error) {
	return 0, errors.New("peer pid is not supported")
}
