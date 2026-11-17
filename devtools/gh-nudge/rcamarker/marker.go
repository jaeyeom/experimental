// Package rcamarker renders and parses the gh-merge rca_marker template.
//
// A marker may carry optional trailing " key=value" tokens between the last
// placeholder and the rest of the template, for example
// "<!-- gh-merge-rca EVT class=flake commit=abc job=build -->".
// Values contain no spaces and no "-->". Unknown keys are ignored.
package rcamarker

import (
	"fmt"
	"regexp"
	"strings"
)

// Keys a marker may carry after its placeholders.
const (
	// KeyTool names the tool that wrote the marker. Absent means gh-merge.
	KeyTool = "tool"
	// KeyCommit is the merge-group commit the queue tested.
	KeyCommit = "commit"
	// KeyJob is the failed job.
	KeyJob = "job"
	// KeyTest is the first failing test.
	KeyTest = "test"
	// KeySupersedes is the id of an earlier RCA comment this marker corrects.
	KeySupersedes = "supersedes"
)

// GHMerge is the tool of a marker without a tool= key.
const GHMerge = "gh-merge"

var knownKeys = map[string]bool{KeyTool: true, KeyCommit: true, KeyJob: true, KeyTest: true, KeySupersedes: true}

const (
	classPattern = `our-code|flake|infra|manual`
	keysGroup    = `(?P<keys>(?: [A-Za-z][A-Za-z0-9_-]*=\S+)*)`
)

// Marker is one RCA marker found in a comment.
type Marker struct {
	EventID string
	Class   string
	// Keys holds the known trailing keys. Unknown keys are dropped.
	Keys map[string]string
}

// Tool returns the tool that wrote the marker.
func (m Marker) Tool() string {
	if tool := m.Keys[KeyTool]; tool != "" {
		return tool
	}
	return GHMerge
}

// Ours reports whether gh-merge wrote the marker.
func (m Marker) Ours() bool {
	return m.Tool() == GHMerge
}

// Key is one trailing key=value token to render.
type Key struct {
	Name  string
	Value string
}

// Render substitutes {event_id} and {class} in marker and adds keys after
// the last placeholder. A key with an empty value is skipped. Whitespace in
// a value becomes "_", and "-->" becomes "--".
func Render(marker, eventID, class string, keys ...Key) string {
	var tokens strings.Builder
	for _, key := range keys {
		value := cleanValue(key.Value)
		if key.Name == "" || value == "" {
			continue
		}
		tokens.WriteString(" " + key.Name + "=" + value)
	}
	cut := tailStart(marker)
	line := marker[:cut] + tokens.String() + marker[cut:]
	line = strings.ReplaceAll(line, "{event_id}", eventID)
	return strings.ReplaceAll(line, "{class}", class)
}

func cleanValue(value string) string {
	value = strings.Join(strings.Fields(value), "_")
	for strings.Contains(value, "-->") {
		value = strings.ReplaceAll(value, "-->", "--")
	}
	return value
}

// Parse reports the RCA class gh-merge decides from for eventID in comments,
// oldest first. Markers from other tools are ignored. A marker with
// supersedes= replaces every earlier marker for the event. Among the rest,
// our-code wins over the other classes; otherwise the last class is used.
// matched is false when marker is empty, lacks a placeholder, or no comment matches.
func Parse(marker, eventID string, comments []string) (string, bool, error) {
	markers, err := Find(marker, eventID, comments)
	if err != nil {
		return "", false, err
	}
	found, ourCode := "", false
	for _, m := range markers {
		if !m.Ours() {
			continue
		}
		if _, ok := m.Keys[KeySupersedes]; ok {
			ourCode = false
		}
		ourCode = ourCode || m.Class == "our-code"
		found = m.Class
	}
	if ourCode {
		return "our-code", true, nil
	}
	return found, found != "", nil
}

// Others returns the markers for eventID written by other tools, oldest
// first. They are context only: Parse never counts them.
func Others(marker, eventID string, comments []string) ([]Marker, error) {
	markers, err := Find(marker, eventID, comments)
	if err != nil {
		return nil, err
	}
	var others []Marker
	for _, m := range markers {
		if !m.Ours() {
			others = append(others, m)
		}
	}
	return others, nil
}

// Find returns the first marker for eventID in each comment, oldest first,
// from any tool.
func Find(marker, eventID string, comments []string) ([]Marker, error) {
	if !markerReady(marker) {
		return nil, nil
	}
	re, err := compile(marker, regexp.QuoteMeta(eventID))
	if err != nil {
		return nil, err
	}
	var markers []Marker
	for _, comment := range comments {
		if m, ok := match(re, comment); ok {
			markers = append(markers, m)
		}
	}
	return markers, nil
}

// Match reports the first marker in comment. The event id is the text
// captured for {event_id}. Class is our-code, flake, infra, or manual.
func Match(marker, comment string) (Marker, bool, error) {
	if !markerReady(marker) {
		return Marker{}, false, nil
	}
	re, err := compile(marker, `.+?`)
	if err != nil {
		return Marker{}, false, err
	}
	m, ok := match(re, comment)
	return m, ok, nil
}

func match(re *regexp.Regexp, comment string) (Marker, bool) {
	found := re.FindStringSubmatch(comment)
	if found == nil {
		return Marker{}, false
	}
	m := Marker{
		EventID: found[re.SubexpIndex("event")],
		Class:   found[re.SubexpIndex("class")],
		Keys:    map[string]string{},
	}
	for _, token := range strings.Fields(found[re.SubexpIndex("keys")]) {
		name, value, _ := strings.Cut(token, "=")
		if strings.Contains(value, "-->") {
			return Marker{}, false
		}
		if knownKeys[name] {
			m.Keys[name] = value
		}
	}
	return m, true
}

// compile turns marker into a regular expression. Trailing keys go between
// the last placeholder and the rest of the template. When the template ends
// with a placeholder, the marker must end its line, so a stray word after
// it does not match.
func compile(marker, eventPattern string) (*regexp.Regexp, error) {
	cut := tailStart(marker)
	head := regexp.QuoteMeta(marker[:cut])
	head = replaceFirst(head, regexp.QuoteMeta("{event_id}"), `(?P<event>`+eventPattern+`)`, `(?:`+eventPattern+`)`)
	head = replaceFirst(head, regexp.QuoteMeta("{class}"), `(?P<class>`+classPattern+`)`, `(?:`+classPattern+`)`)
	tail := marker[cut:]
	pattern := head + keysGroup + regexp.QuoteMeta(tail)
	if tail == "" {
		pattern = `(?m)` + pattern + `[ \t]*\r?$`
	}
	re, err := regexp.Compile(pattern)
	if err != nil {
		return nil, fmt.Errorf("compile rca marker: %w", err)
	}
	return re, nil
}

// replaceFirst replaces the first old in s with first and the others with rest.
func replaceFirst(s, old, first, rest string) string {
	before, after, ok := strings.Cut(s, old)
	if !ok {
		return s
	}
	return before + first + strings.ReplaceAll(after, old, rest)
}

// tailStart returns the index just past the last placeholder in marker.
func tailStart(marker string) int {
	return max(placeholderEnd(marker, "{event_id}"), placeholderEnd(marker, "{class}"))
}

func placeholderEnd(marker, placeholder string) int {
	at := strings.LastIndex(marker, placeholder)
	if at < 0 {
		return 0
	}
	return at + len(placeholder)
}

func markerReady(marker string) bool {
	return marker != "" && strings.Contains(marker, "{event_id}") && strings.Contains(marker, "{class}")
}
