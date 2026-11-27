package ghlimit

import (
	"bytes"
	"strings"
)

// paginateEstimateBytes is the assumed page size when --jq or
// --template transforms stdout so JSON value boundaries no longer match
// response pages.
const paginateEstimateBytes = 32 << 10

type pageCounter struct {
	slurp          bool
	includeHeaders bool
	transformed    bool

	depth      int
	inString   bool
	escaped    bool
	topValues  int
	slurpElems int

	httpCount   int
	atLineStart bool
	linePrefix  [5]byte
	prefixLen   int
}

func newPageCounter(args []string) *pageCounter {
	return &pageCounter{
		slurp:          hasArgFlag(args, "--slurp"),
		includeHeaders: hasArgFlag(args, "-i", "--include"),
		transformed:    hasArgFlag(args, "-q", "--jq", "-t", "--template"),
		atLineStart:    true,
	}
}

func hasArgFlag(args []string, names ...string) bool {
	for _, a := range args {
		if a == "--" {
			return false
		}
		name, _, _ := splitFlag(a)
		for _, want := range names {
			if name == want {
				return true
			}
			if len(want) == 2 && strings.HasPrefix(a, want) && a != "-" {
				return true
			}
		}
	}
	return false
}

func (c *pageCounter) Write(p []byte) {
	if c.includeHeaders {
		c.writeHeaders(p)
		return
	}
	if c.transformed {
		return
	}
	c.writeJSON(p)
}

func (c *pageCounter) writeHeaders(p []byte) {
	for _, b := range p {
		if b == '\n' {
			c.atLineStart = true
			c.prefixLen = 0
			continue
		}
		if !c.atLineStart {
			continue
		}
		if c.prefixLen < len(c.linePrefix) {
			c.linePrefix[c.prefixLen] = b
			c.prefixLen++
			if c.prefixLen == len(c.linePrefix) {
				if bytes.Equal(c.linePrefix[:], []byte("HTTP/")) {
					c.httpCount++
				}
				c.atLineStart = false
			}
		}
	}
}

func (c *pageCounter) writeJSON(p []byte) {
	for _, b := range p {
		if c.inString {
			c.stepString(b)
			continue
		}
		switch b {
		case '"':
			c.inString = true
		case '{', '[':
			if c.depth == 0 {
				c.topValues++
			} else if c.slurp && c.depth == 1 {
				c.slurpElems++
			}
			c.depth++
		case '}', ']':
			if c.depth > 0 {
				c.depth--
			}
		}
	}
}

func (c *pageCounter) stepString(b byte) {
	if c.escaped {
		c.escaped = false
		return
	}
	if b == '\\' {
		c.escaped = true
		return
	}
	if b == '"' {
		c.inString = false
	}
}

func (c *pageCounter) Count(totalBytes int64) int {
	if c.includeHeaders {
		return c.httpCount
	}
	if c.transformed {
		if totalBytes <= 0 {
			return 0
		}
		return int((totalBytes + paginateEstimateBytes - 1) / paginateEstimateBytes)
	}
	if c.slurp {
		return c.slurpElems
	}
	return c.topValues
}

// countPaginatedPages returns the number of pages a --paginate call
// fetched, always at least 1.
func countPaginatedPages(args []string, res Result) int {
	if res.Pages > 0 {
		return res.Pages
	}
	pc := newPageCounter(args)
	sample := res.StdoutHead
	if res.StdoutKept {
		sample = res.Stdout
	}
	pc.Write(sample)
	n := pc.Count(res.StdoutLen)
	if !res.StdoutKept && !pc.transformed && len(res.StdoutHead) > 0 && res.StdoutLen > int64(len(res.StdoutHead)) && n > 0 {
		n = int((int64(n) * res.StdoutLen) / int64(len(res.StdoutHead)))
	}
	return max(1, n)
}
