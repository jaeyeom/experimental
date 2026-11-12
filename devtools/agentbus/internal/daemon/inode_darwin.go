//go:build darwin

package daemon

import "golang.org/x/sys/unix"

func statInode(st unix.Stat_t) inode {
	// Dev is int32. The bit pattern is the device id, including a high bit.
	return inode{dev: uint64(uint32(st.Dev)), ino: st.Ino} //nolint:gosec // G115: device id, not a length
}
