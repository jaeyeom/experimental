package ghask

import (
	"errors"
	"fmt"
	"net/url"
	"os"
	"regexp"
	"strings"
	"unicode"
)

// Size caps on request fields, in bytes. A request over a cap gets 413.
const (
	MaxSelectionBytes = 4 << 10
	MaxQuestionBytes  = 2 << 10
	MaxPathBytes      = 1 << 10
	MaxPageURLBytes   = 2 << 10
	// MaxBodyBytes caps the whole JSON body.
	MaxBodyBytes = 16 << 10
)

var (
	repoPattern = regexp.MustCompile(`^[A-Za-z0-9_.][A-Za-z0-9_.-]*/[A-Za-z0-9_.][A-Za-z0-9_.-]*$`)
	shaPattern  = regexp.MustCompile(`^[0-9a-fA-F]{7,64}$`)
)

// AskRequest is the JSON body of POST /ask.
type AskRequest struct {
	Repo      string `json:"repo"`
	PR        int    `json:"pr"`
	HeadSHA   string `json:"head_sha"` //nolint:tagliatelle // the /ask JSON API uses snake_case
	Path      string `json:"path"`
	Side      string `json:"side"`
	StartLine int    `json:"start_line"` //nolint:tagliatelle // the /ask JSON API uses snake_case
	EndLine   int    `json:"end_line"`   //nolint:tagliatelle // the /ask JSON API uses snake_case
	Selection string `json:"selection"`
	Question  string `json:"question"`
	Deep      bool   `json:"deep"`
	PageURL   string `json:"page_url"` //nolint:tagliatelle // the /ask JSON API uses snake_case
}

type tooLargeError struct {
	field string
	limit int
}

func (e *tooLargeError) Error() string {
	return fmt.Sprintf("%s is over %d bytes", e.field, e.limit)
}

func isTooLarge(err error) bool {
	var tl *tooLargeError
	return errors.As(err, &tl)
}

// Validate checks every field. Size errors are reported first, so an
// oversized request always gets 413.
func (r AskRequest) Validate() error {
	if err := r.checkSizes(); err != nil {
		return err
	}
	if !repoPattern.MatchString(r.Repo) {
		return errors.New("repo must be owner/name")
	}
	if r.PR <= 0 {
		return errors.New("pr must be positive")
	}
	if !shaPattern.MatchString(r.HeadSHA) {
		return errors.New("head_sha must be a hex commit id")
	}
	if r.Path == "" || strings.ContainsFunc(r.Path, unicode.IsControl) {
		return errors.New("path must be non-empty with no control characters")
	}
	if r.Side != "LEFT" && r.Side != "RIGHT" {
		return errors.New("side must be LEFT or RIGHT")
	}
	if r.StartLine <= 0 || r.EndLine < r.StartLine {
		return errors.New("start_line and end_line must form a range starting at 1 or later")
	}
	if strings.TrimSpace(r.Question) == "" {
		return errors.New("question is empty")
	}
	return checkPageURL(r.PageURL)
}

func checkPageURL(raw string) error {
	u, err := url.Parse(raw)
	if err != nil || u.Scheme != "https" || u.Host != "github.com" || strings.ContainsFunc(raw, unicode.IsControl) {
		return errors.New("page_url must be an https://github.com/ link")
	}
	return nil
}

func (r AskRequest) checkSizes() error {
	for _, c := range []struct {
		field string
		value string
		limit int
	}{
		{"selection", r.Selection, MaxSelectionBytes},
		{"question", r.Question, MaxQuestionBytes},
		{"path", r.Path, MaxPathBytes},
		{"page_url", r.PageURL, MaxPageURLBytes},
	} {
		if len(c.value) > c.limit {
			return &tooLargeError{field: c.field, limit: c.limit}
		}
	}
	return nil
}

// RenderMessage is the plain text handed to ask_command in a file.
func RenderMessage(r AskRequest) string {
	deep := "no"
	if r.Deep {
		deep = "yes"
	}
	var b strings.Builder
	fmt.Fprintf(&b, "[gh-ask] PR %d · %s:%d-%d (%s) · deep=%s\n", r.PR, r.Path, r.StartLine, r.EndLine, r.Side, deep)
	writeBlock(&b, "Question", r.Question)
	writeBlock(&b, "Code", r.Selection)
	writeBlock(&b, "Link", r.PageURL)
	return b.String()
}

func writeBlock(b *strings.Builder, title, body string) {
	b.WriteString("\n" + title + ":\n")
	b.WriteString(body)
	if !strings.HasSuffix(body, "\n") {
		b.WriteString("\n")
	}
}

// writeMessageFile writes text to a new mode-0600 file in dir and returns
// its path.
func writeMessageFile(dir, text string) (string, error) {
	f, err := os.CreateTemp(dir, "gh-ask-*.txt")
	if err != nil {
		return "", fmt.Errorf("create message file: %w", err)
	}
	path := f.Name()
	fail := func(err error) (string, error) {
		_ = f.Close()
		_ = os.Remove(path)
		return "", fmt.Errorf("write message file: %w", err)
	}
	if err := f.Chmod(0o600); err != nil {
		return fail(err)
	}
	if _, err := f.WriteString(text); err != nil {
		return fail(err)
	}
	if err := f.Close(); err != nil {
		_ = os.Remove(path)
		return "", fmt.Errorf("write message file: %w", err)
	}
	return path, nil
}
