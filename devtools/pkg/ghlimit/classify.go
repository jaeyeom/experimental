package ghlimit

import (
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"
)

// mutatingSubcommands are the gh commands, by group, that write to
// GitHub. A group mapped to nil mutates with every subcommand.
var mutatingSubcommands = map[string][]string{
	"pr":    {"review", "edit", "merge", "close", "comment", "ready"},
	"issue": {"create", "edit", "comment", "close"},
	"label": nil,
}

// IsMutation reports whether gh args write to GitHub, so they must keep
// the mutation gap. args exclude the gh program name.
func IsMutation(args []string) bool {
	if len(args) == 0 {
		return false
	}
	if args[0] == "api" {
		return apiMutates(args[1:])
	}
	subs, ok := mutatingSubcommands[args[0]]
	if !ok {
		return false
	}
	if subs == nil {
		return true
	}
	return len(args) > 1 && slices.Contains(subs, args[1])
}

// apiFlags maps the gh api flags that take a value to what the value
// is: "method", "field", or "" for a value that only needs skipping so
// it is not read as the endpoint.
var apiFlags = map[string]string{
	"-X": "method", "--method": "method",
	"-f": "field", "-F": "field", "--field": "field", "--raw-field": "field",
	"-H": "", "--header": "", "-q": "", "--jq": "", "-t": "", "--template": "",
	"--cache": "", "--hostname": "", "--input": "", "-p": "", "--preview": "",
}

// apiCall is what IsMutation needs from gh api args.
type apiCall struct {
	method   string
	fields   []string
	endpoint string
}

func (c *apiCall) set(kind, value string) {
	switch kind {
	case "method":
		c.method = strings.ToUpper(value)
	case "field":
		c.fields = append(c.fields, value)
	}
}

// parseAPIArgs reads the method, the -f/-F field values and the
// endpoint from gh api args.
func parseAPIArgs(args []string) apiCall {
	var c apiCall
	for i := 0; i < len(args); i++ {
		arg := args[i]
		name, value, hasValue := splitFlag(arg)
		if kind, ok := apiFlags[name]; ok {
			if !hasValue && i+1 < len(args) {
				i++
				value = args[i]
			}
			c.set(kind, value)
			continue
		}
		// A short flag with its value attached, such as -XPOST.
		if len(arg) > 2 && arg[0] == '-' && arg[1] != '-' {
			c.set(apiFlags[arg[:2]], arg[2:])
			continue
		}
		if !strings.HasPrefix(arg, "-") && c.endpoint == "" {
			c.endpoint = arg
		}
	}
	return c
}

// apiMutates classifies gh api args: an explicit write method, fields
// without a method (gh then POSTs), or a graphql mutation query.
func apiMutates(args []string) bool {
	c := parseAPIArgs(args)
	if c.endpoint == "graphql" {
		for _, f := range c.fields {
			if query, ok := strings.CutPrefix(f, "query="); ok {
				return strings.HasPrefix(strings.TrimSpace(query), "mutation")
			}
		}
		return false
	}
	switch c.method {
	case "POST", "PATCH", "PUT", "DELETE":
		return true
	case "":
		return len(c.fields) > 0
	default:
		return false
	}
}

// splitFlag splits --name=value into its name and value. A short flag
// or a flag without "=" is returned whole.
func splitFlag(arg string) (name, value string, hasValue bool) {
	if strings.HasPrefix(arg, "--") {
		if n, v, ok := strings.Cut(arg, "="); ok {
			return n, v, true
		}
	}
	return arg, "", false
}

// Redact keeps the command and subcommand and every flag name of args
// and replaces every other value with "_", so a log line shows what kind
// of call it was without its content.
func Redact(args []string) []string {
	out := make([]string, len(args))
	words, afterFlag := 0, false
	for i, arg := range args {
		isFlag := strings.HasPrefix(arg, "-") && len(arg) > 1
		switch {
		case isFlag:
			if name, _, ok := splitFlag(arg); ok {
				out[i] = name + "=_"
			} else {
				out[i] = arg
			}
		case words < 2 && !afterFlag:
			out[i] = arg
			words++
		default:
			out[i] = "_"
			words = 2
		}
		afterFlag = isFlag
	}
	return out
}

// UsesStdin reports whether gh args tell gh to read stdin ("-", "--x=-"
// or a field "@-").
func UsesStdin(args []string) bool {
	for _, arg := range args {
		if arg == "-" || strings.HasSuffix(arg, "=-") || strings.HasSuffix(arg, "@-") {
			return true
		}
	}
	return false
}

var (
	rateLimitText = regexp.MustCompile(`(?i)rate limit|abuse detection`)
	httpLimitCode = regexp.MustCompile(`\bHTTP(/[\d.]+)?\s+(403|429)\b`)
	retryAfter    = regexp.MustCompile(`(?im)^retry-after:\s*(\d+)\s*$`)
	rateReset     = regexp.MustCompile(`(?im)^x-ratelimit-reset:\s*(\d+)\s*$`)
)

// RateLimited reports whether a failed gh call hit a primary or
// secondary rate limit. An HTTP 403 or 429 counts only for gh api, whose
// other commands also report a 403 for missing permissions.
func RateLimited(args []string, stderr, stdout string) bool {
	if rateLimitText.MatchString(stderr) {
		return true
	}
	return len(args) > 0 && args[0] == "api" &&
		(httpLimitCode.MatchString(stderr) || httpLimitCode.MatchString(stdout))
}

// RetryAfter is the wait a Retry-After or x-ratelimit-reset header in
// gh output asks for, as gh api -i prints them.
func RetryAfter(output string, now time.Time) (time.Duration, bool) {
	if m := retryAfter.FindStringSubmatch(output); m != nil {
		if secs, err := strconv.ParseInt(m[1], 10, 64); err == nil {
			return time.Duration(secs) * time.Second, true
		}
	}
	if m := rateReset.FindStringSubmatch(output); m != nil {
		if epoch, err := strconv.ParseInt(m[1], 10, 64); err == nil {
			return time.Unix(epoch, 0).Sub(now), true
		}
	}
	return 0, false
}
