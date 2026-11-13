package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
)

func cmdSend(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	parsed, err := parseSend(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return 2
	}
	body, code := readMessageBody("send", parsed.body, parsed.file, stdin, stderr)
	if code != 0 {
		return code
	}
	return finishSend(ctx, env, daemon.Request{
		Op: "send", Name: parsed.target, Body: body, Env: env,
		TTL: parsed.ttl, Wait: parsed.wait, ClientID: parsed.clientID,
		LogName: parsed.target,
	}, parsed.asJSON, stdout, stderr)
}

func cmdReply(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	parsed, err := parseReply(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus reply: %s\n", err)
		return 2
	}
	body, code := readMessageBody("reply", parsed.body, parsed.file, stdin, stderr)
	if code != 0 {
		return code
	}
	return finishSend(ctx, env, daemon.Request{
		Op: "reply", Name: parsed.id, Body: body, Env: env, ClientID: parsed.clientID,
		LogName: replyRecipient(ctx, env, parsed.id),
	}, parsed.asJSON, stdout, stderr)
}

// readMessageBody resolves one body. file wins over body. An empty body
// reads stdin. The caller still rejects an empty result.
func readMessageBody(cmd, body, file string, stdin io.Reader, stderr io.Writer) (string, int) {
	if file != "" {
		data, err := os.ReadFile(file)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus %s: %s\n", cmd, err)
			return "", 1
		}
		return string(data), 0
	}
	if body != "" {
		return body, 0
	}
	if stdin == nil {
		fmt.Fprintf(stderr, "agentbus %s: message body is required\n", cmd)
		return "", 2
	}
	data, err := io.ReadAll(stdin)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus %s: %s\n", cmd, err)
		return "", 1
	}
	return string(data), 0
}

// replyRecipient is the original sender, for a lost reply's log hint.
// A lookup that fails leaves the hint with only the client id.
func replyRecipient(ctx context.Context, env []string, id string) string {
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "show", Name: id}, io.Discard)
	if code != 0 || !resp.OK {
		return ""
	}
	for _, line := range strings.Split(resp.Text, "\n") {
		name, ok := strings.CutPrefix(line, "from: ")
		if ok && name != "" {
			return name
		}
	}
	return ""
}

func cmdShow(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	return cmdMessageText(ctx, args, env, "show", stdout, stderr)
}

func cmdThread(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	return cmdMessageText(ctx, args, env, "thread", stdout, stderr)
}

func cmdStatus(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	return cmdMessageText(ctx, args, env, "status", stdout, stderr)
}

func cmdMessageText(ctx context.Context, args, env []string, op string, stdout, stderr io.Writer) int {
	if len(args) != 1 || strings.HasPrefix(args[0], "-") {
		fmt.Fprintf(stderr, "agentbus %s: usage: agentbus %s <id>\n", op, op)
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: op, Name: args[0]}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	fmt.Fprint(stdout, resp.Text)
	return 0
}

func finishSend(ctx context.Context, env []string, req daemon.Request, asJSON bool, stdout, stderr io.Writer) int {
	if req.Body == "" {
		fmt.Fprintf(stderr, "agentbus: message body is required\n")
		return 2
	}
	if req.ClientID == "" {
		id, err := localClientID(env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
		req.ClientID = id
	}
	resp, code := callDaemon(ctx, env, req, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	return printAccepted(stdout, stderr, resp.Name, resp.State, asJSON, resp.Duplicate)
}

// localClientID is the retry key for a local send or reply. An explicit
// --client-id is kept. Otherwise it is <machine>:<ulid>, reused if the
// daemon was not listening and the same request is dialed again.
func localClientID(env []string) (string, error) {
	machine := ""
	if cfg, err := hubSettings(env); err == nil {
		machine = cfg.Machine
	}
	return mintClientID(machine)
}

func printAccepted(stdout, stderr io.Writer, id, state string, asJSON, dup bool) int {
	if asJSON {
		err := json.NewEncoder(stdout).Encode(struct {
			ID        string `json:"id"`
			State     string `json:"state"`
			Duplicate bool   `json:"duplicate,omitempty"`
		}{ID: id, State: state, Duplicate: dup})
		if err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
	} else if _, err := fmt.Fprintf(stdout, "%s %s\n", id, state); err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	noteHeld(stderr, state)
	return 0
}

func noteHeld(stderr io.Writer, state string) {
	switch state {
	case "held: " + mailbox.HoldHibernated:
		fmt.Fprintf(stderr, "agentbus: the agent is hibernated; the message is queued until it wakes\n")
	case "held: " + mailbox.HoldWaking:
		fmt.Fprintf(stderr, "agentbus: the agent is hibernated; waking it, and the message goes out once it is ready\n")
	case "held: " + mailbox.HoldWakeFailed:
		fmt.Fprintf(stderr, "agentbus: the agent is hibernated and its last wake failed; the message is queued until a wake succeeds\n")
	}
}

type sendParse struct {
	target   string
	body     string
	file     string
	ttl      string
	clientID string
	wait     bool
	asJSON   bool
}

func parseSend(args []string) (sendParse, error) {
	left, literal := splitDoubleDash(args)
	wait, left, err := takeBool(left, "--wait")
	if err != nil {
		return sendParse{}, err
	}
	asJSON, left, err := takeBool(left, "--json")
	if err != nil {
		return sendParse{}, err
	}
	ttl, left, ttlFound, err := takeFlag(left, "--ttl")
	if err != nil {
		return sendParse{}, err
	}
	if ttlFound {
		if err := positiveTTL(ttl); err != nil {
			return sendParse{}, err
		}
	}
	clientID, left, idFound, err := takeFlag(left, "--client-id")
	if err != nil {
		return sendParse{}, err
	}
	if idFound {
		if err := mailbox.ValidClientID(clientID); err != nil {
			return sendParse{}, fmt.Errorf("%w", err)
		}
	}
	msg, err := parsePositionals(left, literal, "usage: agentbus send <name|handle> [<body>] [--file <path>] [--ttl <duration>] [--wait] [--client-id <id>] [--json]")
	if err != nil {
		return sendParse{}, err
	}
	return sendParse{
		target: msg.recipient, body: msg.body, file: msg.file, ttl: ttl,
		clientID: clientID, wait: wait, asJSON: asJSON,
	}, nil
}

func positiveTTL(raw string) error {
	d, err := time.ParseDuration(strings.TrimSpace(raw))
	if err != nil || d <= 0 {
		return errors.New("--ttl needs a positive duration, for example 30m")
	}
	return nil
}

func takeBool(args []string, flagName string) (bool, []string, error) {
	var rest []string
	found := false
	for _, arg := range args {
		if arg != flagName {
			rest = append(rest, arg)
			continue
		}
		if found {
			return true, nil, fmt.Errorf("%s given twice", flagName)
		}
		found = true
	}
	return found, rest, nil
}

type replyParse struct {
	id       string
	body     string
	file     string
	clientID string
	asJSON   bool
}

// messageBody is one recipient and one body source. An empty body and
// file means the caller reads stdin.
type messageBody struct {
	recipient string
	body      string
	file      string
}

func parseReply(args []string) (replyParse, error) {
	left, literal := splitDoubleDash(args)
	asJSON, left, err := takeBool(left, "--json")
	if err != nil {
		return replyParse{}, err
	}
	clientID, left, found, err := takeFlag(left, "--client-id")
	if err != nil {
		return replyParse{}, err
	}
	if found {
		if err := mailbox.ValidClientID(clientID); err != nil {
			return replyParse{}, fmt.Errorf("%w", err)
		}
	}
	msg, err := parsePositionals(left, literal, "usage: agentbus reply <id> [<body>] [--file <path>] [--client-id <id>] [--json]")
	if err != nil {
		return replyParse{}, err
	}
	return replyParse{
		id: msg.recipient, body: msg.body, file: msg.file,
		clientID: clientID, asJSON: asJSON,
	}, nil
}

// splitDoubleDash ends flag parsing. Words after "--" are body text.
func splitDoubleDash(args []string) (left, literal []string) {
	for i, arg := range args {
		if arg == "--" {
			return args[:i], args[i+1:]
		}
	}
	return args, nil
}

// parsePositionals reads the recipient and exactly one body source:
// --file, stdin, or the remaining words. A token that starts with "--"
// before the body starts is an unknown flag.
func parsePositionals(left, literal []string, usage string) (messageBody, error) {
	file, rest, found, err := takeFlag(left, "--file")
	if err != nil {
		return messageBody{}, err
	}
	if len(rest) < 1 {
		return messageBody{}, errors.New(usage)
	}
	if strings.HasPrefix(rest[0], "--") {
		return messageBody{}, fmt.Errorf("unknown flag %s", rest[0])
	}
	if strings.HasPrefix(rest[0], "-") {
		return messageBody{}, errors.New(usage)
	}
	words, stdin, err := leadingBody(rest[1:])
	if err != nil {
		return messageBody{}, err
	}
	if stdin && len(literal) > 0 {
		return messageBody{}, errors.New("pass the body as an argument or -, not both")
	}
	words = append(words, literal...)
	if found {
		if strings.TrimSpace(file) == "" {
			return messageBody{}, errors.New("--file needs a path")
		}
		if len(words) > 0 || stdin {
			return messageBody{}, errors.New("pass the body as an argument or --file, not both")
		}
		return messageBody{recipient: rest[0], file: file}, nil
	}
	if stdin || len(words) == 0 {
		return messageBody{recipient: rest[0]}, nil
	}
	return messageBody{recipient: rest[0], body: strings.Join(words, " ")}, nil
}

// leadingBody returns the body words before a "--" separator.
// A sole "-" selects stdin. An unknown "--" flag before any word is an error.
func leadingBody(args []string) ([]string, bool, error) {
	if len(args) == 1 && args[0] == "-" {
		return nil, true, nil
	}
	var words []string
	for _, arg := range args {
		if len(words) == 0 && strings.HasPrefix(arg, "--") {
			return nil, false, fmt.Errorf("unknown flag %s", arg)
		}
		words = append(words, arg)
	}
	return words, false, nil
}
