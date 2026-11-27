package main

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/quotapace"
)

func runGate(args []string, store quotapace.Store, e env) (int, error) {
	fs := newFlags("gate", e)
	profile := fs.String("profile", "", "profile (reads only this profile)")
	window := fs.String("window", "7d", "window, such as 7d or 5h")
	minGap := fs.Float64("min-gap", 0, "minimum headroom gap in percentage points")
	staleFlag := fs.String("stale", "6h", "fail closed when reading is older than this (0 disables)")
	asJSON := fs.Bool("json", false, "print gate result as JSON")
	watch := fs.Bool("watch", false, "poll periodically and notify when gap crosses --min-gap")
	everyFlag := fs.String("every", "1m", "poll interval in --watch mode")
	notifyCmd := fs.String("notify-cmd", "", "command run on crossing with gate JSON on stdin (default $QUOTAPACE_NOTIFY_CMD)")
	if err := parse(fs, args); err != nil {
		return 0, err
	}
	if *profile == "" {
		return 0, usageErr("gate needs --profile")
	}
	if err := quotapace.CheckProfileName(*profile); err != nil {
		return 0, usageErr("%v", err)
	}
	if _, err := quotapace.WindowLength(*window); err != nil {
		return 0, usageErr("--window: %v", err)
	}
	var stale time.Duration
	if *staleFlag != "0" {
		d, err := quotapace.ParseDuration(*staleFlag)
		if err != nil {
			return 0, usageErr("--stale: %v", err)
		}
		stale = d
	}
	every, err := quotapace.ParseDuration(*everyFlag)
	if err != nil {
		return 0, usageErr("--every: %v", err)
	}

	opts := quotapace.GateOptions{
		Profile: *profile,
		Window:  *window,
		MinGap:  *minGap,
		Stale:   stale,
	}
	if !*watch {
		now := e.now()
		res, err := store.Gate(opts, now)
		if err != nil {
			return 0, fmt.Errorf("gate: %w", err)
		}
		if err := writeGateOutput(e, res, now, *asJSON); err != nil {
			return 0, err
		}
		return res.Exit, nil
	}

	cmd := *notifyCmd
	if cmd == "" {
		cmd = e.getenv("QUOTAPACE_NOTIFY_CMD")
	}
	argv := strings.Fields(cmd)
	return watchGate(store, opts, every, argv, *asJSON, e)
}

func watchGate(store quotapace.Store, opts quotapace.GateOptions, every time.Duration, argv []string, asJSON bool, e env) (int, error) {
	ctx := e.ctx
	if ctx == nil {
		ctx = context.Background()
	}
	after := e.after
	if after == nil {
		after = time.After
	}
	var watcher quotapace.GateWatcher
	for {
		now := e.now()
		res, err := store.Gate(opts, now)
		if err != nil {
			return 0, fmt.Errorf("gate: %w", err)
		}
		if watcher.Step(res) {
			if err := writeGateOutput(e, res, now, asJSON); err != nil {
				return 0, err
			}
			if len(argv) > 0 {
				payload, err := json.Marshal(res)
				if err != nil {
					return 0, fmt.Errorf("encode gate: %w", err)
				}
				if err := e.notify(argv, string(payload)+"\n"); err != nil {
					return 0, err
				}
			}
		}
		select {
		case <-ctx.Done():
			return 0, nil
		case <-after(every):
		}
	}
}

func writeGateOutput(e env, res quotapace.GateResult, now time.Time, asJSON bool) error {
	if asJSON {
		data, err := json.MarshalIndent(res, "", "  ")
		if err != nil {
			return fmt.Errorf("encode gate: %w", err)
		}
		fmt.Fprintln(e.stdout, string(data))
		return nil
	}
	fmt.Fprintln(e.stdout, res.Line(now))
	return nil
}
