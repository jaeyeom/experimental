package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func runDashboard(args []string, store quotapace.Store, e env) (int, error) {
	fs := newFlags("dashboard", e)
	v := addViewFlags(fs, true)
	watchFlag := fs.String("watch", "", "refresh interval, such as 30s (empty runs once)")
	if err := parse(fs, args); err != nil {
		return 0, err
	}
	var watchEvery time.Duration
	if *watchFlag != "" && *watchFlag != "0" {
		d, err := quotapace.ParseDuration(*watchFlag)
		if err != nil {
			return 0, usageErr("--watch: %v", err)
		}
		watchEvery = d
	}
	stale, err := quotapace.ParseDuration(*v.stale)
	if err != nil {
		return 0, usageErr("--stale: %v", err)
	}
	opts := quotapace.DashboardOptions{
		Stale:   stale,
		Profile: *v.profile,
	}
	if watchEvery == 0 {
		return 0, renderDashboard(store, v, opts, e)
	}
	return 0, watchDashboard(store, v, opts, watchEvery, e)
}

func renderDashboard(store quotapace.Store, v viewFlags, opts quotapace.DashboardOptions, e env) error {
	snap, err := v.snapshot(store, e.now())
	if err != nil {
		return err
	}
	fmt.Fprint(e.stdout, snap.Dashboard(opts))
	for _, ws := range snap.Windows() {
		for _, w := range ws.Warnings {
			fmt.Fprintf(e.stderr, "warning: %s %s: %s\n", ws.Profile, ws.Window, w)
		}
	}
	return nil
}

func watchDashboard(store quotapace.Store, v viewFlags, opts quotapace.DashboardOptions, every time.Duration, e env) error {
	ctx := e.ctx
	if ctx == nil {
		ctx = context.Background()
	}
	after := e.after
	if after == nil {
		after = time.After
	}
	tty := isTTY(e.stdout)
	first := true
	for {
		if !first {
			if tty {
				fmt.Fprint(e.stdout, "\x1b[H\x1b[2J")
			} else {
				fmt.Fprintln(e.stdout)
			}
		}
		first = false
		if err := renderDashboard(store, v, opts, e); err != nil {
			return err
		}
		select {
		case <-ctx.Done():
			return nil
		case <-after(every):
		}
	}
}

func isTTY(w any) bool {
	f, ok := w.(*os.File)
	if !ok {
		return false
	}
	info, err := f.Stat()
	if err != nil {
		return false
	}
	return info.Mode()&os.ModeCharDevice != 0
}
