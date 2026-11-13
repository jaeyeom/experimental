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
)

// waitFor sleeps out a Slack rate limit. Tests replace it.
var waitFor = slackwatch.Wait

// channelLookup resolves names through the state cache and conversations.list.
func channelLookup(cfg slackwatch.Config, store *slackwatch.Store, stderr io.Writer) slackwatch.ChannelLookup {
	lookup := slackwatch.ChannelLookup{
		Store: store, List: tokenChannelList{cfg: cfg}, Now: time.Now,
		TTL: cfg.ChannelCacheTTL.Duration, MaxWait: cfg.ChannelLookupWait.Duration, Sleep: waitFor,
	}
	if stderr != nil {
		lookup.OnWait = func(d time.Duration) {
			fmt.Fprintf(stderr, "rate limited, waiting %s…\n", d)
		}
	}
	return lookup
}

// tokenChannelList lists conversations with the user token.
type tokenChannelList struct {
	cfg slackwatch.Config
}

func (t tokenChannelList) ListChannels(ctx context.Context, cursor string) (slackwatch.ChannelPage, error) {
	api, err := readClient(t.cfg)
	if err != nil {
		return slackwatch.ChannelPage{}, err
	}
	page, err := slackReader{api: api}.ListChannels(ctx, cursor)
	if err != nil {
		return slackwatch.ChannelPage{}, err //nolint:wrapcheck // Already descriptive.
	}
	return page, nil
}

// resolveChannel looks up a channel id by name. A fresh cache hit makes
// no API call. Tests replace it with a fake.
var resolveChannel = func(cfg slackwatch.Config, store *slackwatch.Store, name string, stderr io.Writer) (string, error) {
	if cfg.TokenFile == "" {
		return "", fmt.Errorf("resolving #%s needs tokenFile; pass the channel id instead", name)
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return "", err //nolint:wrapcheck // Already descriptive.
	}
	return channelLookup(cfg, store, stderr).ChannelID(context.Background(), name) //nolint:wrapcheck // Already names the channel and the fix.
}

// channelArg returns the channel id for an id or a #name.
func channelArg(cfg slackwatch.Config, store *slackwatch.Store, arg string, stderr io.Writer) (string, error) {
	if name, ok := strings.CutPrefix(arg, "#"); ok {
		return resolveChannel(cfg, store, name, stderr)
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
	channel, err := channelArg(cfg, store, pos[0], stderr)
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
	unchecked := fs.Bool("owner-unchecked", false, "accept an owner that is not a known target or agent")
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
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	if err := checkOwner(cfg, s.Owner, *unchecked, stderr); err != nil {
		return fail(stderr, err)
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
