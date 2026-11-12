package github

import (
	"encoding/json"
	"fmt"
	"log/slog"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/ghlimit"
)

// emptyStdoutRetryDelay is the wait before the one retry of a gh JSON
// command that exited 0 with empty stdout.
const emptyStdoutRetryDelay = 2 * time.Second

// parseErrorHeadBytes caps how much of stdout a parse error quotes.
const parseErrorHeadBytes = 200

// ghJSONOutput is the captured output of one gh run whose stdout is JSON.
type ghJSONOutput struct {
	args   []string
	stdout string
	stderr string
}

// runGHJSON runs gh with args and captures stdout and stderr separately.
// A non-zero exit returns the error and is not retried here. An exit 0
// with blank stdout is logged at warning level and retried once after
// emptyStdoutRetryDelay; the second result is returned as is.
func (c *Client) runGHJSON(args ...string) (ghJSONOutput, error) {
	out, err := c.runGHOnce(args)
	if err != nil || strings.TrimSpace(out.stdout) != "" {
		return out, err
	}
	c.warnf("gh exited 0 with empty stdout; retrying once",
		"command", out.command(),
		"delay", emptyStdoutRetryDelay,
		"stderr", firstLine(out.stderr))
	c.sleepFor(emptyStdoutRetryDelay)
	return c.runGHOnce(args)
}

func (c *Client) runGHOnce(args []string) (ghJSONOutput, error) {
	stdout, stderr, err := c.executeSeparated("gh", args...)
	return ghJSONOutput{args: args, stdout: stdout, stderr: stderr}, err
}

func (c *Client) warnf(msg string, args ...any) {
	if c.warn != nil {
		c.warn(msg, args...)
		return
	}
	slog.Warn(msg, args...)
}

func (c *Client) sleepFor(d time.Duration) {
	if c.sleep != nil {
		c.sleep(d)
		return
	}
	time.Sleep(d)
}

// decode unmarshals stdout into v. A failure returns parseError.
func (o ghJSONOutput) decode(v any) error {
	if err := json.Unmarshal([]byte(o.stdout), v); err != nil {
		return o.parseError(err)
	}
	return nil
}

// parseError wraps err with the redacted command, the exit code, the
// stdout length and head, and the first stderr line. Output is parsed only
// after gh exits 0, so the exit code is always 0.
func (o ghJSONOutput) parseError(err error) error {
	head := ""
	if o.stdout != "" {
		h := o.stdout
		if len(h) > parseErrorHeadBytes {
			h = h[:parseErrorHeadBytes]
		}
		head = fmt.Sprintf(", head %q", h)
	}
	return fmt.Errorf("parse gh %s output: %w (exit 0, stdout %d bytes%s, stderr: %q)",
		o.command(), err, len(o.stdout), head, firstLine(o.stderr))
}

func (o ghJSONOutput) command() string {
	return strings.Join(ghlimit.Redact(o.args), " ")
}

func firstLine(s string) string {
	line, _, _ := strings.Cut(strings.TrimSpace(s), "\n")
	return strings.TrimSpace(line)
}
