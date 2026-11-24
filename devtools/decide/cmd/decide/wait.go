package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// waitTimeoutExit is the exit code when --timeout passes first, as timeout(1) uses.
const waitTimeoutExit = 124

// defaultWaitEvery is how often decide wait rereads the ledger.
const defaultWaitEvery = 2 * time.Second

var errWaitTimeout = errors.New("timed out")

// cmdWait is `decide wait`: it blocks until item id is no longer open or
// gets a new message, then prints it as decide show does. An agent with no
// herdr pane to be notified in runs it in the background and wakes when it
// exits.
func cmdWait(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("wait", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print JSON")
	timeout := fs.Duration("timeout", 0, "give up after this long and exit 124; 0 waits forever")
	every := fs.Duration("every", defaultWaitEvery, "how often to reread the item")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, err := oneArg(fs.Args(), "id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *every <= 0 {
		return fail(stderr, fmt.Errorf("--every must be positive"), 2)
	}
	if *timeout < 0 {
		return fail(stderr, fmt.Errorf("--timeout must not be negative"), 2)
	}
	led, err := openLedger()
	if err != nil {
		return fail(stderr, err, 1)
	}
	item, err := waitForChange(func() (decide.Item, error) { return led.Get(id) }, *every, *timeout)
	if errors.Is(err, errWaitTimeout) {
		return fail(stderr, fmt.Errorf("wait %s: timed out after %v", id, *timeout), waitTimeoutExit)
	}
	if err != nil {
		return fail(stderr, err, 1)
	}
	if *jsonOut {
		return writeShowJSON(id, stdout, stderr)
	}
	writeShow(stdout, &item)
	return 0
}

// waitForChange polls get every interval until the item is not open or has
// more messages than at the first read. A zero timeout never times out.
func waitForChange(get func() (decide.Item, error), every, timeout time.Duration) (decide.Item, error) {
	item, err := get()
	if err != nil {
		return decide.Item{}, err
	}
	seen := len(item.Messages)
	var deadline <-chan time.Time
	if timeout > 0 {
		t := time.NewTimer(timeout)
		defer t.Stop()
		deadline = t.C
	}
	tick := time.NewTicker(every)
	defer tick.Stop()
	for item.State == decide.StateOpen && len(item.Messages) == seen {
		select {
		case <-deadline:
			return decide.Item{}, errWaitTimeout
		case <-tick.C:
		}
		if item, err = get(); err != nil {
			return decide.Item{}, err
		}
	}
	return item, nil
}
