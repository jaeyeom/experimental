//go:build linux

package daemon

import "golang.org/x/sys/unix"

func statInode(st unix.Stat_t) inode {
	return inode{dev: st.Dev, ino: st.Ino}
}
