package slackwatch

import (
	"errors"
	"fmt"
	"net/url"
	"regexp"
	"strconv"
	"strings"
)

// PostRef names a Slack post or a watch. Either WatchID is set, or Channel
// and TS are. ThreadTS is the parent thread when the post is known to be a
// thread reply; it is empty for a top-level post or when unknown.
type PostRef struct {
	WatchID  int64
	Channel  string
	TS       string
	ThreadTS string
}

var (
	channelRE   = regexp.MustCompile(`^[A-Z0-9]+$`)
	tsRE        = regexp.MustCompile(`^[0-9]+\.[0-9]+$`)
	permalinkRE = regexp.MustCompile(`^/archives/([A-Z0-9]+)/p([0-9]{7,})$`)
)

// ParsePost parses the <post> arguments of a CLI command: a permalink, a
// channel and ts pair, or a watch id.
func ParsePost(args []string) (PostRef, error) {
	switch len(args) {
	case 1:
		if id, err := strconv.ParseInt(args[0], 10, 64); err == nil {
			if id <= 0 {
				return PostRef{}, fmt.Errorf("invalid watch id %q", args[0])
			}
			return PostRef{WatchID: id}, nil
		}
		return parsePermalink(args[0])
	case 2:
		if !channelRE.MatchString(args[0]) {
			return PostRef{}, fmt.Errorf("invalid Slack channel id %q", args[0])
		}
		if !tsRE.MatchString(args[1]) {
			return PostRef{}, fmt.Errorf("invalid Slack ts %q", args[1])
		}
		return PostRef{Channel: args[0], TS: args[1]}, nil
	default:
		return PostRef{}, errors.New("want a permalink, a channel and ts, or a watch id")
	}
}

// parsePermalink parses https://<team>.slack.com/archives/<channel>/p<digits>
// with an optional thread_ts query for replies.
func parsePermalink(s string) (PostRef, error) {
	u, err := url.Parse(s)
	if err != nil || u.Host == "" {
		return PostRef{}, fmt.Errorf("not a Slack permalink, channel and ts, or watch id: %q", s)
	}
	m := permalinkRE.FindStringSubmatch(u.Path)
	if m == nil {
		return PostRef{}, fmt.Errorf("not a Slack message permalink: %q", s)
	}
	digits := m[2]
	p := PostRef{Channel: m[1], TS: digits[:len(digits)-6] + "." + digits[len(digits)-6:]}
	if thread := u.Query().Get("thread_ts"); thread != "" && thread != p.TS {
		if !tsRE.MatchString(thread) {
			return PostRef{}, fmt.Errorf("invalid thread_ts in permalink: %q", s)
		}
		p.ThreadTS = thread
	}
	return p, nil
}

// CompareTS compares two Slack timestamps numerically and returns -1, 0,
// or 1. Both must be of the form seconds.micros.
func CompareTS(a, b string) int {
	as, af, _ := strings.Cut(a, ".")
	bs, bf, _ := strings.Cut(b, ".")
	if c := compareDigits(as, bs); c != 0 {
		return c
	}
	// Fractions compare as decimals: pad to equal length.
	for len(af) < len(bf) {
		af += "0"
	}
	for len(bf) < len(af) {
		bf += "0"
	}
	return strings.Compare(af, bf)
}

func compareDigits(a, b string) int {
	a = strings.TrimLeft(a, "0")
	b = strings.TrimLeft(b, "0")
	if len(a) != len(b) {
		if len(a) < len(b) {
			return -1
		}
		return 1
	}
	return strings.Compare(a, b)
}
