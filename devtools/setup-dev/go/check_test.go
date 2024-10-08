package main

import (
	"fmt"
	"os/exec"
)

func ExampleEmacsCurrentVersion() {
	version := EmacsCurrentVersion(exec.Command("echo", `GNU Emacs 29.4
Copyright (C) 2024 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
`))
	fmt.Println(version)
	// Output: 29.4
}
