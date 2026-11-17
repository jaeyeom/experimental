// Package ghoutbox holds GitHub pull request comments and reviews that
// coding agents draft until the user approves the exact text in decide.
package ghoutbox

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"unicode/utf8"
)

// Draft kinds. KindThread starts one discussion thread per line comment.
// The others are a review in the main conversation.
const (
	KindThread         = "thread"
	KindComment        = "comment"
	KindRequestChanges = "request-changes"
	KindApprove        = "approve"
)

// LineComment is the text for one line of the pull request diff.
type LineComment struct {
	Path string `json:"path"`
	Line int    `json:"line"`
	Text string `json:"text"`
}

// Where is path:line.
func (c LineComment) Where() string {
	return c.Path + ":" + strconv.Itoa(c.Line)
}

// Draft is what an agent submits. Body and line texts are kept byte for
// byte.
type Draft struct {
	Repo  string        `json:"repo"`
	PR    int           `json:"pr"`
	Kind  string        `json:"kind"`
	Body  string        `json:"body,omitempty"`
	Lines []LineComment `json:"lines,omitempty"`
	Owner string        `json:"owner"`
	Ref   string        `json:"ref,omitempty"`
	// Edit is the id of an existing review comment (KindThread) or
	// review (a review kind) to replace.
	Edit string `json:"edit,omitempty"`
}

// UsageError is a draft the command line got wrong.
type UsageError struct{ msg string }

func (e *UsageError) Error() string { return e.msg }

func usagef(format string, args ...any) error {
	return &UsageError{msg: fmt.Sprintf(format, args...)}
}

var (
	repoPattern = regexp.MustCompile(`^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$`)
	idPattern   = regexp.MustCompile(`^[0-9]+$`)
)

// ParseLineSpec splits path:line=file. The path may contain ':' and the
// file may contain '='.
func ParseLineSpec(spec string) (path string, line int, file string, err error) {
	left, file, ok := strings.Cut(spec, "=")
	if !ok || file == "" {
		return "", 0, "", usagef("--line %q: want path:line=text-file", spec)
	}
	i := strings.LastIndex(left, ":")
	if i <= 0 {
		return "", 0, "", usagef("--line %q: want path:line=text-file", spec)
	}
	line, err = strconv.Atoi(left[i+1:])
	if err != nil || line <= 0 {
		return "", 0, "", usagef("--line %q: line must be a positive number", spec)
	}
	return left[:i], line, file, nil
}

// ValidateText checks the kind rules and the text, without the repo,
// pull request, and owner. It sets an empty Kind to KindThread when the
// draft has lines and no body.
func (d *Draft) ValidateText() error {
	if d.Kind == "" {
		if d.Body != "" && len(d.Lines) == 0 {
			return usagef("a body needs a destination: give a line with --line path:line=text-file, " +
				"or choose a review with --kind comment, --kind request-changes or --kind approve")
		}
		d.Kind = KindThread
	}
	if err := d.checkKind(); err != nil {
		return err
	}
	if err := d.checkEdit(); err != nil {
		return err
	}
	return d.checkTexts()
}

// Validate checks the whole draft. See ValidateText.
func (d *Draft) Validate() error {
	if !repoPattern.MatchString(d.Repo) {
		return usagef("--repo %q: want owner/name", d.Repo)
	}
	if d.PR <= 0 {
		return usagef("--pr must be a positive pull request number")
	}
	if strings.TrimSpace(d.Owner) == "" {
		return usagef("--owner is required: the agent that gets the outcome")
	}
	return d.ValidateText()
}

func (d *Draft) checkKind() error {
	switch d.Kind {
	case KindThread:
		if d.Body != "" {
			return usagef("a discussion thread has no body: put the text in --line, or choose a review --kind")
		}
		if len(d.Lines) == 0 {
			return usagef("a discussion thread needs at least one --line path:line=text-file")
		}
	case KindComment, KindRequestChanges:
		if d.Body == "" && len(d.Lines) == 0 {
			return usagef("a %s review needs a body or a --line", d.Kind)
		}
	case KindApprove:
	default:
		return usagef("--kind %q: want thread, comment, request-changes or approve", d.Kind)
	}
	return nil
}

func (d *Draft) checkEdit() error {
	if d.Edit == "" {
		return nil
	}
	if !idPattern.MatchString(d.Edit) {
		return usagef("--edit %q: want the numeric id of a comment or review", d.Edit)
	}
	if d.Kind == KindThread && len(d.Lines) != 1 {
		return usagef("--edit of a discussion thread comment takes exactly one --line")
	}
	if d.Kind != KindThread && len(d.Lines) != 0 {
		return usagef("--edit of a review takes no --line; give the new body with --body-file")
	}
	if d.Kind != KindThread && strings.TrimSpace(d.Body) == "" {
		return usagef("--edit of a review needs the new body in --body-file")
	}
	return nil
}

func (d *Draft) checkTexts() error {
	if err := checkText("body", d.Body, true); err != nil {
		return err
	}
	for _, c := range d.Lines {
		if err := checkText(c.Where(), c.Text, false); err != nil {
			return err
		}
	}
	return nil
}

func checkText(where, text string, mayBeEmpty bool) error {
	if !utf8.ValidString(text) {
		return usagef("%s is not valid UTF-8", where)
	}
	if !mayBeEmpty && strings.TrimSpace(text) == "" {
		return usagef("%s is empty", where)
	}
	return nil
}

// reviewEvents maps a review kind to its GitHub event.
var reviewEvents = map[string]string{
	KindComment:        "COMMENT",
	KindRequestChanges: "REQUEST_CHANGES",
	KindApprove:        "APPROVE",
}

// reviewWords is a review kind in plain words.
var reviewWords = map[string]string{
	KindComment:        "comment",
	KindRequestChanges: "request changes",
	KindApprove:        "approve",
}

// Destinations names where the draft goes in plain words, one entry per
// discussion thread or a single review.
func (d Draft) Destinations() []string {
	if d.Kind == KindThread {
		var out []string
		for _, c := range d.Lines {
			where := fmt.Sprintf("discussion thread on %s line %d", c.Path, c.Line)
			if d.Edit != "" {
				where = fmt.Sprintf("edit of comment %s in the %s", d.Edit, where)
			}
			out = append(out, where)
		}
		return out
	}
	if d.Edit != "" {
		return []string{fmt.Sprintf("edit of review %s (main conversation)", d.Edit)}
	}
	return []string{"review (main conversation): " + reviewWords[d.Kind]}
}

// Target is owner/name#N.
func (d Draft) Target() string {
	return d.Repo + "#" + strconv.Itoa(d.PR)
}
