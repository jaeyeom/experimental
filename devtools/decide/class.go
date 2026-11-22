package decide

import (
	"fmt"
	"strconv"
	"strings"
)

const (
	// ClassDecision is a choice with a reason that should guide later choices.
	ClassDecision = "decision"
	// ClassApproval is a yes/no/edit check on one exact action or text.
	ClassApproval = "approval"
	// ClassRequest is a request from the user to an agent, tracked to done.
	ClassRequest = "request"
	// ClassFact is a question for a fact only the user knows.
	ClassFact = "fact"
	// ClassInbound is someone else's ask relayed to the user.
	ClassInbound = "inbound"
	// ClassTask is work only the user can do.
	ClassTask = "task"
	// ClassNote is an FYI, report, or alert (no options, archived to close).
	ClassNote = "note"
)

type classSpec struct {
	name   string
	prefix string
}

var classSpecs = []classSpec{
	{name: ClassDecision, prefix: "D"},
	{name: ClassApproval, prefix: "A"},
	{name: ClassRequest, prefix: "R"},
	{name: ClassFact, prefix: "Q"},
	{name: ClassInbound, prefix: "I"},
	{name: ClassTask, prefix: "T"},
	{name: ClassNote, prefix: "N"},
}

// ParseClass parses a class name or single-letter prefix (case-insensitive)
// and returns the canonical class name.
func ParseClass(raw string) (string, error) {
	s := strings.TrimSpace(raw)
	if s == "" {
		return "", fmt.Errorf("decide class is empty")
	}
	lower := strings.ToLower(s)
	upper := strings.ToUpper(s)
	for _, spec := range classSpecs {
		if lower == spec.name || upper == spec.prefix {
			return spec.name, nil
		}
	}
	return "", fmt.Errorf("decide class %q is invalid", raw)
}

// ClassPrefix returns the single-letter ID prefix for class.
// An empty class defaults to "D" (decision).
func ClassPrefix(class string) (string, error) {
	if class == "" {
		return "D", nil
	}
	for _, spec := range classSpecs {
		if class == spec.name {
			return spec.prefix, nil
		}
	}
	return "", fmt.Errorf("decide class %q is invalid", class)
}

// FormatID formats item number num with the prefix for class.
func FormatID(class string, num int) string {
	prefix, err := ClassPrefix(class)
	if err != nil {
		prefix = "D"
	}
	return fmt.Sprintf("%s-%03d", prefix, num)
}

// EffectiveClass returns it.Class, or ClassDecision for legacy items without a class.
func (it *Item) EffectiveClass() string {
	if it == nil || it.Class == "" {
		return ClassDecision
	}
	return it.Class
}

func isValidPrefix(p string) bool {
	upper := strings.ToUpper(p)
	for _, spec := range classSpecs {
		if upper == spec.prefix {
			return true
		}
	}
	return false
}

func isApprovalOptionKey(key string) bool {
	return key == "yes" || key == "no" || key == "edit"
}

func parseID(id string) (int, bool) {
	numStr := id
	if prefix, rest, hasDash := strings.Cut(id, "-"); hasDash {
		if !isValidPrefix(prefix) || rest == "" {
			return 0, false
		}
		numStr = rest
	}
	if numStr == "" {
		return 0, false
	}
	n, err := strconv.Atoi(numStr)
	if err != nil || n < 1 {
		return 0, false
	}
	return n, true
}
