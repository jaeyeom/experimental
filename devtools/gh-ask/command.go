package ghask

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"regexp"
	"strings"
	"time"
	"unicode"
)

const (
	maxStdoutBytes = 64 << 10
	maxStderrBytes = 4 << 10
	maxIDBytes     = 256
	waitDelay      = 5 * time.Second
)

var placeholderPattern = regexp.MustCompile(`\{[^{}]*\}`)

// expandArgv replaces {name} in each template argument with vars[name].
// It makes one pass, so a substituted value is never expanded again. An
// unknown placeholder is an error.
func expandArgv(tmpl []string, vars map[string]string) ([]string, error) {
	out := make([]string, len(tmpl))
	var unknown string
	for i, arg := range tmpl {
		out[i] = placeholderPattern.ReplaceAllStringFunc(arg, func(m string) string {
			value, ok := vars[m[1:len(m)-1]]
			if !ok && unknown == "" {
				unknown = m
			}
			return value
		})
	}
	if unknown != "" {
		return nil, fmt.Errorf("unknown placeholder %s", unknown)
	}
	return out, nil
}

// limitedBuffer keeps the first limit bytes and drops the rest, so a
// chatty command cannot grow server memory.
type limitedBuffer struct {
	bytes.Buffer
	limit int
}

func (b *limitedBuffer) Write(p []byte) (int, error) {
	if room := b.limit - b.Len(); room > 0 {
		if len(p) > room {
			b.Buffer.Write(p[:room])
		} else {
			b.Buffer.Write(p)
		}
	}
	return len(p), nil
}

// runCommand runs argv without a shell and with no stdin. It returns
// stdout, and stderr for logging.
func runCommand(ctx context.Context, argv []string, timeout time.Duration) ([]byte, string, error) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	// argv comes from the user's config. A request fills only {pr} (an
	// int), {repo} (in the allowlist) and {file} (a server temp path).
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: see above
	stdout := &limitedBuffer{limit: maxStdoutBytes}
	stderr := &limitedBuffer{limit: maxStderrBytes}
	cmd.Stdout = stdout
	cmd.Stderr = stderr
	cmd.WaitDelay = waitDelay
	if err := cmd.Run(); err != nil {
		if ctx.Err() != nil {
			return nil, stderr.String(), fmt.Errorf("command timed out after %v", timeout)
		}
		return nil, stderr.String(), fmt.Errorf("command failed: %w", err)
	}
	return stdout.Bytes(), stderr.String(), nil
}

// extractID returns the first match of re in out: its first group when
// re has one, otherwise the whole match.
func extractID(re *regexp.Regexp, out []byte) (string, bool) {
	m := re.FindSubmatch(out)
	if m == nil {
		return "", false
	}
	id := m[0]
	if len(m) > 1 {
		id = m[1]
	}
	s := strings.TrimSpace(string(id))
	if s == "" || len(s) > maxIDBytes || strings.ContainsFunc(s, unicode.IsControl) {
		return "", false
	}
	return s, true
}
