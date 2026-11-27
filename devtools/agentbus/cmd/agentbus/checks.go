package main

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
)

func isCheckCommand(name string) bool {
	return name == "held" || name == "asks" || name == "dialogs"
}

func cmdErrorCode(args []string) int {
	if len(args) > 0 && isCheckCommand(args[0]) {
		return 2
	}
	return 1
}

type stringListFlag []string

func (s *stringListFlag) String() string { return strings.Join(*s, ",") }

func (s *stringListFlag) Set(val string) error {
	val = strings.TrimSpace(val)
	if val == "" {
		return errors.New("value must not be empty")
	}
	*s = append(*s, val)
	return nil
}

func parseCheckDuration(raw string) (string, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return "", nil
	}
	d, ok := parseDays(raw)
	if !ok {
		return "", errors.New("want a non-negative duration like 30m, 6h, or 2d")
	}
	return d.String(), nil
}

func cmdHeld(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("held", flag.ContinueOnError)
	fs.SetOutput(stderr)
	olderThan := fs.String("older-than", "", "only include messages held for at least this duration (e.g. 10m, 2h, 1d)")
	to := fs.String("to", "", "filter by recipient name or handle")
	asJSON := fs.Bool("json", false, "print a JSON array of held messages")
	var reasons, notReasons stringListFlag
	fs.Var(&reasons, "reason", "include only messages with this reason code (repeatable)")
	fs.Var(&notReasons, "not-reason", "exclude messages with this reason code (repeatable)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "agentbus held: unexpected arguments")
		return 2
	}
	dur, err := parseCheckDuration(*olderThan)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus held: --older-than %q: %s\n", *olderThan, err)
		return 2
	}
	resp, err := dialBus(ctx, env, daemon.Request{
		Op:         "held",
		OlderThan:  dur,
		Reasons:    reasons,
		NotReasons: notReasons,
		To:         *to,
	})
	if err != nil {
		fmt.Fprintf(stderr, "agentbus held: %s\n", err)
		return 2
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus held: %s\n", resp.Error)
		return 2
	}
	items := resp.HeldMessages
	if *asJSON {
		if items == nil {
			items = []mailbox.HeldMessage{}
		}
		if err := json.NewEncoder(stdout).Encode(items); err != nil {
			fmt.Fprintf(stderr, "agentbus held: %s\n", err)
			return 2
		}
	} else {
		for _, it := range items {
			fmt.Fprintf(stdout, "%s from %s to %s held: %s [%s] since %s (age %s)\n",
				it.ID, it.From, it.To, it.Reason, it.ReasonCode, it.HeldSince, it.Age)
		}
	}
	if len(items) > 0 {
		return 1
	}
	return 0
}

func cmdAsks(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("asks", flag.ContinueOnError)
	fs.SetOutput(stderr)
	unanswered := fs.String("unanswered", "", "only include asks unanswered for at least this duration (e.g. 10m, 2h, 1d)")
	from := fs.String("from", "", "filter by sender name or handle")
	to := fs.String("to", "", "filter by recipient name or handle")
	asJSON := fs.Bool("json", false, "print a JSON array of unanswered asks")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "agentbus asks: unexpected arguments")
		return 2
	}
	dur, err := parseCheckDuration(*unanswered)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus asks: --unanswered %q: %s\n", *unanswered, err)
		return 2
	}
	resp, err := dialBus(ctx, env, daemon.Request{
		Op:        "asks",
		OlderThan: dur,
		From:      *from,
		To:        *to,
	})
	if err != nil {
		fmt.Fprintf(stderr, "agentbus asks: %s\n", err)
		return 2
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus asks: %s\n", resp.Error)
		return 2
	}
	items := resp.Asks
	if *asJSON {
		if items == nil {
			items = []mailbox.Ask{}
		}
		if err := json.NewEncoder(stdout).Encode(items); err != nil {
			fmt.Fprintf(stderr, "agentbus asks: %s\n", err)
			return 2
		}
	} else {
		for _, a := range items {
			fmt.Fprintf(stdout, "%s from %s to %s sent %s (age %s) [%s]: %s\n",
				a.ID, a.From, a.To, a.Sent, a.Age, a.Rule, a.FirstLine)
		}
	}
	if len(items) > 0 {
		return 1
	}
	return 0
}

func cmdDialogs(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("dialogs", flag.ContinueOnError)
	fs.SetOutput(stderr)
	machine := fs.String("machine", "", "filter by machine name (e.g. local, linux2)")
	asJSON := fs.Bool("json", false, "print a JSON array of active dialogs")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if len(fs.Args()) != 0 {
		fmt.Fprintln(stderr, "agentbus dialogs: unexpected arguments")
		return 2
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus dialogs: %s\n", err)
		return 2
	}
	resp, err := dialBus(ctx, env, daemon.Request{
		Op:          "dialogs",
		Machine:     *machine,
		DialogRules: cfg.DialogRules,
	})
	if err != nil {
		fmt.Fprintf(stderr, "agentbus dialogs: %s\n", err)
		return 2
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus dialogs: %s\n", resp.Error)
		return 2
	}
	items := resp.Dialogs
	if *asJSON {
		if items == nil {
			items = []daemon.DialogEntry{}
		}
		if err := json.NewEncoder(stdout).Encode(items); err != nil {
			fmt.Fprintf(stderr, "agentbus dialogs: %s\n", err)
			return 2
		}
	} else {
		for _, d := range items {
			who := cmp.Or(d.Name, d.Handle)
			fmt.Fprintf(stdout, "%s/%s (%s) [%s]: %s\n", d.Machine, d.Pane, who, d.Rule, d.Text)
		}
	}
	if len(items) > 0 {
		return 1
	}
	return 0
}
