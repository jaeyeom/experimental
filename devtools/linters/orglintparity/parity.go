// Package orglintparity compares findings of a Go org linter with those of
// Emacs org-lint. Both sides are normalized to "file:line: checker: message".
package orglintparity

import (
	"bufio"
	"fmt"
	"io"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// Finding is one normalized lint finding.
type Finding struct {
	File    string
	Line    int
	Checker string
	Message string
}

func (f Finding) String() string {
	return fmt.Sprintf("%s:%d: %s: %s", f.File, f.Line, f.Checker, f.Message)
}

var findingRE = regexp.MustCompile(`^(.+?):(\d+): ([A-Za-z0-9$-]+): (.*)$`)

// Parse reads normalized findings, skipping blank lines.
func Parse(r io.Reader) ([]Finding, error) {
	var out []Finding
	sc := bufio.NewScanner(r)
	for sc.Scan() {
		line := sc.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}
		m := findingRE.FindStringSubmatch(line)
		if m == nil {
			return nil, fmt.Errorf("not a normalized finding: %q", line)
		}
		n, _ := strconv.Atoi(m[2])
		out = append(out, Finding{m[1], n, m[3], m[4]})
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read findings: %w", err)
	}
	return out, nil
}

func sorted(fs []Finding) []Finding {
	out := append([]Finding(nil), fs...)
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i], out[j]
		if a.File != b.File {
			return a.File < b.File
		}
		if a.Line != b.Line {
			return a.Line < b.Line
		}
		if a.Checker != b.Checker {
			return a.Checker < b.Checker
		}
		return a.Message < b.Message
	})
	return out
}

// Format renders findings sorted, one per line.
func Format(fs []Finding) string {
	var sb strings.Builder
	for _, f := range sorted(fs) {
		sb.WriteString(f.String())
		sb.WriteByte('\n')
	}
	return sb.String()
}

// LineDiff is a finding both sides report, on different lines.
type LineDiff struct{ Want, Got Finding }

// Report is the difference between Emacs (want) and the Go linter (got).
type Report struct {
	Missed    []Finding // Emacs reports, Go does not
	Extra     []Finding // Go reports, Emacs does not
	LineDiffs []LineDiff
}

// Empty reports whether both sides agree.
func (r Report) Empty() bool {
	return len(r.Missed) == 0 && len(r.Extra) == 0 && len(r.LineDiffs) == 0
}

func (r Report) String() string {
	var sb strings.Builder
	for _, f := range r.Missed {
		fmt.Fprintf(&sb, "MISSED %s\n", f)
	}
	for _, f := range r.Extra {
		fmt.Fprintf(&sb, "EXTRA  %s\n", f)
	}
	for _, d := range r.LineDiffs {
		fmt.Fprintf(&sb, "LINE   %s (go: line %d)\n", d.Want, d.Got.Line)
	}
	return sb.String()
}

// Diff compares findings. Exact matches cancel out; leftovers with the same
// file, checker and message pair up as line differences.
func Diff(want, got []Finding) Report {
	exact := map[Finding]int{}
	for _, f := range got {
		exact[f]++
	}
	var wantLeft []Finding
	for _, f := range sorted(want) {
		if exact[f] > 0 {
			exact[f]--
			continue
		}
		wantLeft = append(wantLeft, f)
	}
	var gotLeft []Finding
	for _, f := range sorted(got) {
		if exact[f] > 0 {
			exact[f]--
			gotLeft = append(gotLeft, f)
		}
	}
	type key struct{ file, checker, msg string }
	pool := map[key][]Finding{}
	for _, f := range gotLeft {
		k := key{f.File, f.Checker, f.Message}
		pool[k] = append(pool[k], f)
	}
	var r Report
	for _, w := range wantLeft {
		k := key{w.File, w.Checker, w.Message}
		if g := pool[k]; len(g) > 0 {
			r.LineDiffs = append(r.LineDiffs, LineDiff{w, g[0]})
			pool[k] = g[1:]
			continue
		}
		r.Missed = append(r.Missed, w)
	}
	for _, g := range pool {
		r.Extra = append(r.Extra, g...)
	}
	r.Extra = sorted(r.Extra)
	return r
}

// GoldenPath returns the golden file next to a fixture .org file.
func GoldenPath(org string) string {
	return strings.TrimSuffix(org, ".orgtest") + ".golden"
}
