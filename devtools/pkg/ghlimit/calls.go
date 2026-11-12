package ghlimit

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"
)

// Call kinds, as GitHub's secondary rate limits tell them apart.
const (
	KindGraphQL         = "graphql"
	KindGraphQLMutation = "graphql-mutation"
	KindRESTGet         = "rest-get"
	KindRESTWrite       = "rest-write"
	// KindSubcommand is any gh command but gh api, such as gh pr view.
	// Most of them use GraphQL.
	KindSubcommand = "gh-subcommand"
)

// Secondary-limit points of a read and of a write.
const (
	readPoints  = 1
	writePoints = 5
)

// CallInfo is how a gh call counts against the secondary limits.
type CallInfo struct {
	Kind string
	// Cmd is the command and subcommand of a KindSubcommand call, such
	// as "pr view".
	Cmd    string
	Points int
	// QueryHash is a short hash of a gh api graphql query, and
	// QueryFields its top-level field names.
	QueryHash   string
	QueryFields []string
	// query is the raw query text.
	query string
}

// GraphQL reports whether the call spends the GraphQL time budget. A
// gh subcommand counts, since most of them use GraphQL.
func (c CallInfo) GraphQL() bool {
	return c.Kind == KindGraphQL || c.Kind == KindGraphQLMutation || c.Kind == KindSubcommand
}

// AsksRateLimit reports whether the call is a GraphQL query that asks
// for rateLimit, so its stdout is worth keeping for ParseRateLimit.
func (c CallInfo) AsksRateLimit() bool {
	return strings.Contains(c.query, "rateLimit")
}

// durationKey names the calls whose durations predict each other: one
// query hash, or one gh subcommand.
func (c CallInfo) durationKey() string {
	if c.QueryHash != "" {
		return "q:" + c.QueryHash
	}
	return c.Kind + ":" + c.Cmd
}

// ClassifyCall tells the kind and the points of gh args, which exclude
// the gh program name: a GraphQL query costs 1 and a mutation 5, a REST
// GET 1 and a REST write 5, and any other gh command 1 or, when it
// mutates, 5.
func ClassifyCall(args []string) CallInfo {
	if len(args) > 0 && args[0] == "api" {
		call := parseAPIArgs(args[1:])
		if call.endpoint == "graphql" {
			info := CallInfo{Kind: KindGraphQL, Points: readPoints}
			if apiMutates(args[1:]) {
				info.Kind, info.Points = KindGraphQLMutation, writePoints
			}
			for _, f := range call.fields {
				if q, ok := strings.CutPrefix(f, "query="); ok {
					info.query = q
					info.QueryHash = hashHex(strings.Join(strings.Fields(q), " "))[:12]
					info.QueryFields = topLevelFields(q)
				}
			}
			return info
		}
		if apiMutates(args[1:]) {
			return CallInfo{Kind: KindRESTWrite, Points: writePoints}
		}
		return CallInfo{Kind: KindRESTGet, Points: readPoints}
	}
	info := CallInfo{Kind: KindSubcommand, Points: readPoints}
	var words []string
	for _, a := range args {
		if len(words) == 2 || (len(words) > 0 && strings.HasPrefix(a, "-")) {
			break
		}
		words = append(words, a)
	}
	info.Cmd = strings.Join(words, " ")
	if IsMutation(args) {
		info.Points = writePoints
	}
	return info
}

// topLevelFields lists the field names (not aliases) in the selection
// set of the first operation of a GraphQL document. Fragment spreads,
// arguments, directives, strings and comments are skipped.
func topLevelFields(q string) []string {
	toks := gqlTokens(q)
	open := slices.Index(toks, "{")
	if open < 0 || (open > 0 && toks[0] == "fragment") {
		return nil
	}
	var fields []string
	depth := 1
	for i := open + 1; i < len(toks) && depth > 0; i++ {
		t := toks[i]
		switch {
		case t == "{":
			depth++
		case t == "}":
			depth--
		case depth != 1, t == ":":
		case t == "." || t == "@":
			i = skipSpreadOrDirective(toks, i)
		case i+1 < len(toks) && toks[i+1] == ":":
			i++ // an alias; the field name follows the colon
		default:
			fields = append(fields, t)
		}
	}
	return fields
}

// skipSpreadOrDirective returns the index of the last token of the
// fragment spread ("...F", "... on T") or directive ("@d") that starts
// at toks[i].
func skipSpreadOrDirective(toks []string, i int) int {
	for i < len(toks) && (toks[i] == "." || toks[i] == "@") {
		i++
	}
	if i < len(toks) && toks[i] == "on" {
		i++
	}
	if i >= len(toks) || toks[i] == "{" {
		return i - 1
	}
	return i
}

// gqlTokens splits a GraphQL document into names and the punctuation
// { } : @ and ., dropping comments, strings and parenthesized arguments.
func gqlTokens(q string) []string {
	var toks []string
	parens := 0
	for i := 0; i < len(q); i++ {
		ch := q[i]
		switch {
		case ch == '#':
			i = skipUntil(q, i, '\n')
		case ch == '"':
			i = skipUntil(q, i, '"')
		case ch == '(':
			parens++
		case ch == ')':
			parens--
		case parens > 0:
		case isNameByte(ch):
			j := i
			for j < len(q) && isNameByte(q[j]) {
				j++
			}
			toks = append(toks, q[i:j])
			i = j - 1
		case strings.IndexByte("{}:@.", ch) >= 0:
			toks = append(toks, string(ch))
		}
	}
	return toks
}

// skipUntil returns the index of the next end byte after q[i], skipping
// backslash escapes, or len(q).
func skipUntil(q string, i int, end byte) int {
	for i++; i < len(q) && q[i] != end; i++ {
		if q[i] == '\\' {
			i++
		}
	}
	return i
}

func isNameByte(c byte) bool {
	return c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'
}

// maxRateLimitParse is the largest stdout ParseRateLimit reads.
const maxRateLimitParse = 1 << 20

// ParseRateLimit reads data.rateLimit.cost and .remaining from a gh api
// graphql response of at most maxRateLimitParse bytes.
func ParseRateLimit(stdout []byte) (cost, remaining int, ok bool) {
	if len(stdout) == 0 || len(stdout) > maxRateLimitParse {
		return 0, 0, false
	}
	var resp struct {
		Data struct {
			RateLimit *struct {
				Cost      int `json:"cost"`
				Remaining int `json:"remaining"`
			} `json:"rateLimit"`
		} `json:"data"`
	}
	if json.Unmarshal(stdout, &resp) != nil || resp.Data.RateLimit == nil {
		return 0, 0, false
	}
	return resp.Data.RateLimit.Cost, resp.Data.RateLimit.Remaining, true
}

// CallEntry is one line of the call log: one gh run, or one read the
// cache served.
type CallEntry struct {
	Time       time.Time `json:"time"`
	DurationMS int64     `json:"durationMs"`
	PID        int       `json:"pid"`
	Kind       string    `json:"kind"`
	Cmd        string    `json:"cmd,omitempty"`
	// Points is what the call costs under the secondary-limit model; 0
	// for a cache hit.
	Points int `json:"points"`
	// Caller is the first ancestor of gh-throttle that is not a shell,
	// CallerChain the names from its parent up to that ancestor, and Cwd
	// the working directory.
	Caller      string   `json:"caller"`
	CallerChain []string `json:"callerChain,omitempty"`
	Cwd         string   `json:"cwd,omitempty"`
	// Args are the gh arguments with values redacted.
	Args        string   `json:"args"`
	QueryHash   string   `json:"queryHash,omitempty"`
	QueryFields []string `json:"queryFields,omitempty"`
	ExitCode    int      `json:"exitCode"`
	RateLimited bool     `json:"rateLimited,omitempty"`
	CacheHit    bool     `json:"cacheHit,omitempty"`
	// RateLimitCost and RateLimitRemaining are the response's
	// rateLimit { cost remaining } when the query asked for it.
	RateLimitCost      *int `json:"rateLimitCost,omitempty"`
	RateLimitRemaining *int `json:"rateLimitRemaining,omitempty"`
}

// GraphQLTime is how long the call spent on GraphQL, for the stats.
func (e CallEntry) GraphQLTime() time.Duration {
	if e.CacheHit || !(CallInfo{Kind: e.Kind}).GraphQL() {
		return 0
	}
	return time.Duration(e.DurationMS) * time.Millisecond
}

// newCallEntry is the entry for args classified as info, without the
// caller fields.
func newCallEntry(args []string, info CallInfo, start time.Time) CallEntry {
	return CallEntry{
		Time:        start,
		Kind:        info.Kind,
		Cmd:         info.Cmd,
		Points:      info.Points,
		Args:        strings.Join(Redact(args), " "),
		QueryHash:   info.QueryHash,
		QueryFields: info.QueryFields,
	}
}

// MaxCallLogSize is the size at which the call log is rotated.
const MaxCallLogSize = 20 << 20

// AppendCallLog appends e as one JSON line to path, creating its
// directory. When the line would take the file past maxSize, the file
// first moves to path.1, replacing the one there, so at most two files
// are kept.
func AppendCallLog(path string, e CallEntry, maxSize int64) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create log dir: %w", err)
	}
	line, err := json.Marshal(e)
	if err != nil {
		return fmt.Errorf("encode call entry: %w", err)
	}
	line = append(line, '\n')
	if info, err := os.Stat(path); err == nil && info.Size()+int64(len(line)) > maxSize {
		if err := os.Rename(path, path+".1"); err != nil {
			return fmt.Errorf("rotate call log: %w", err)
		}
	}
	return appendLine(path, line)
}

func appendLine(path string, line []byte) error {
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("open log: %w", err)
	}
	if _, err := f.Write(line); err != nil {
		_ = f.Close()
		return fmt.Errorf("write log: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close log: %w", err)
	}
	return nil
}
