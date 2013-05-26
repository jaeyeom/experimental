// Binary clock_main starts a classic bell clock. It rings for every
// hour, for example, 3 times at 3 o'clock. To ring the bell, it runs
// an external shell command, there should be a separate binary to
// ring the bell.
package main

import (
	"jaeyeom/toy/clock"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
)

var (
	bellCommand = flag.String(
		"bell_command",
		os.Getenv("HOME") + "/bin/bell",
		"Bell command with number of rings as the 1st arg.")
)

// Bell rings the bell times times.
func Bell(times int) error {
	log.Println("Ring the bell times:", times)
	return exec.Command(*bellCommand, fmt.Sprint(times)).Run()
}

func main() {
	flag.Parse()
	log.Fatal(clock.Clock{BellHandler: Bell}.Serve())
}
