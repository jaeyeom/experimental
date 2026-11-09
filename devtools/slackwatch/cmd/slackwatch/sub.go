package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
)

// resolveChannel looks up a channel id by name with conversations.list.
// Tests replace it with a fake.
var resolveChannel = func(cfg slackwatch.Config, name string) (string, error) {
	if cfg.TokenFile == "" {
		return "", fmt.Errorf("resolving #%s needs tokenFile; pass the channel id instead", name)
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return "", err //nolint:wrapcheck // Already descriptive.
	}
	tokens, err := slackwatch.LoadTokens(cfg.TokenFile)
	if err != nil {
		return "", err //nolint:wrapcheck // Already descriptive.
	}
	return slackReader{api: slack.New(tokens.UserToken)}.ChannelID(context.Background(), name)
}

// channelArg returns the channel id for an id or a #name.
func channelArg(cfg slackwatch.Config, arg string) (string, error) {
	if name, ok := strings.CutPrefix(arg, "#"); ok {
		return resolveChannel(cfg, name)
	}
	return arg, nil
}

// withChannel parses a single <channel> argument, opens the store, and
// resolves the channel.
func withChannel(configPath, cmd string, pos []string, stderr io.Writer, f func(*slackwatch.Store, string) error) int {
	if len(pos) != 1 {
		return usageErr(stderr, fmt.Errorf("%s needs one channel id or #name", cmd))
	}
	cfg, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	channel, err := channelArg(cfg, pos[0])
	if err != nil {
		return fail(stderr, err)
	}
	if err := f(store, channel); err != nil {
		return fail(stderr, err)
	}
	return 0
}

func cmdSub(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("sub", stderr)
	var s slackwatch.Sub
	fs.StringVar(&s.Owner, "owner", "", "owner name, resolved through config targets (required)")
	fs.StringVar(&s.Purpose, "purpose", "", "why the channel is followed")
	until := fs.String("until", "", "end the subscription at this date")
	fs.BoolVar(&s.IncludeSelf, "include-self", false, "also notify the token owner's own messages")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if s.Owner == "" {
		return usageErr(stderr, errors.New("sub needs --owner"))
	}
	now := time.Now()
	if *until != "" {
		if s.Until, err = parseUntil(*until, now); err != nil {
			return usageErr(stderr, err)
		}
	}
	return withChannel(configPath, "sub", pos, stderr, func(store *slackwatch.Store, channel string) error {
		s.Channel = channel
		got, created, err := store.AddSub(s, now)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		verb := "updated"
		if created {
			verb = "created"
		}
		fmt.Fprintf(stdout, "subscription %d %s: %s for %s\n", got.ID, verb, got.Channel, got.Owner)
		return nil
	})
}

func cmdUnsub(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("unsub", stderr)
	owner := fs.String("owner", "", "only this owner's subscription")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	return withChannel(configPath, "unsub", pos, stderr, func(store *slackwatch.Store, channel string) error {
		n, err := store.RemoveSubs(channel, *owner)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		if n == 0 {
			return fmt.Errorf("no subscription to %s", channel)
		}
		fmt.Fprintf(stdout, "removed %d subscriptions to %s\n", n, channel)
		return nil
	})
}

func cmdSubs(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("subs", stderr)
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 0 {
		return usageErr(stderr, errors.New("subs takes no arguments"))
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	subs, err := store.ListSubs()
	if err != nil {
		return fail(stderr, err)
	}
	if *asJSON {
		return writeJSON(stdout, stderr, subs)
	}
	tw := tabwriter.NewWriter(stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tCHANNEL\tOWNER\tPURPOSE\tUNTIL\tINCLUDE SELF")
	for _, s := range subs {
		until := ""
		if !s.Until.IsZero() {
			until = s.Until.Local().Format(time.RFC3339)
		}
		self := ""
		if s.IncludeSelf {
			self = "yes"
		}
		fmt.Fprintf(tw, "%d\t%s\t%s\t%s\t%s\t%s\n", s.ID, s.Channel, s.Owner, s.Purpose, until, self)
	}
	if err := tw.Flush(); err != nil {
		return fail(stderr, err)
	}
	return 0
}
