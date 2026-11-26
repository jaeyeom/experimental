//go:build !unix

package term

import "os"

// Width reports no terminal where the size cannot be read.
func Width(*os.File) (int, bool) {
	return 0, false
}
