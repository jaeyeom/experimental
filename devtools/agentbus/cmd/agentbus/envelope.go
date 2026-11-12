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
	body := parsed.body
	if parsed.file != "" {
		data, readErr := os.ReadFile(parsed.file)
		if readErr != nil {
			fmt.Fprintf(stderr, "agentbus send: %s\n", readErr)
			return 1
		}
		body = string(data)
	}
	if body == "" {
		if stdin == nil {
			fmt.Fprintf(stderr, "agentbus send: message body is required\n")
			return 2
		}
		data, readErr := io.ReadAll(stdin)
		if readErr != nil {
			fmt.Fprintf(stderr, "agentbus send: %s\n", readErr)
			return 1
		}
		body = string(data)
	}
	return finishSend(ctx, env, daemon.Request{
		Op: "send", Name: parsed.target, Body: body, Env: env,
		TTL: parsed.ttl, Wait: parsed.wait, ClientID: parsed.clientID,
	}, parsed.asJSON, stdout, stderr)
}

func cmdReply(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	id, body, clientID, asJSON, err := parseReply(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus reply: %s\n", err)
		return 2
	}
	return finishSend(ctx, env, daemon.Request{Op: "reply", Name: id, Body: body, Env: env, ClientID: clientID}, asJSON, stdout, stderr)
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
	wait, rest, err := takeBool(args, "--wait")
	if err != nil {
		return sendParse{}, err
	}
	asJSON, rest, err := takeBool(rest, "--json")
	if err != nil {
		return sendParse{}, err
	}
	ttl, rest, ttlFound, err := takeFlag(rest, "--ttl")
	if err != nil {
		return sendParse{}, err
	}
	if ttlFound {
		if err := positiveTTL(ttl); err != nil {
			return sendParse{}, err
		}
	}
	clientID, rest, idFound, err := takeFlag(rest, "--client-id")
	if err != nil {
		return sendParse{}, err
	}
	if idFound {
		if err := mailbox.ValidClientID(clientID); err != nil {
			return sendParse{}, fmt.Errorf("%w", err)
		}
	}
	target, body, file, err := parseSendArgs(rest)
	if err != nil {
		return sendParse{}, err
	}
	return sendParse{
		target: target, body: body, file: file, ttl: ttl,
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

func parseSendArgs(args []string) (string, string, string, error) {
	file, rest, found, err := takeFlag(args, "--file")
	if err != nil {
		return "", "", "", err
	}
	if len(rest) < 1 || strings.HasPrefix(rest[0], "-") {
		return "", "", "", errors.New("usage: agentbus send <name|handle> [<body>] [--file <path>] [--ttl <duration>] [--wait] [--client-id <id>] [--json]")
	}
	if found {
		if strings.TrimSpace(file) == "" {
			return "", "", "", errors.New("--file needs a path")
		}
		if len(rest) != 1 {
			return "", "", "", errors.New("pass the body as an argument or --file, not both")
		}
		return rest[0], "", file, nil
	}
	if len(rest) == 1 {
		return rest[0], "", "", nil
	}
	return rest[0], strings.Join(rest[1:], " "), "", nil
}

func parseReply(args []string) (string, string, string, bool, error) {
	asJSON, rest, err := takeBool(args, "--json")
	if err != nil {
		return "", "", "", false, err
	}
	clientID, rest, found, err := takeFlag(rest, "--client-id")
	if err != nil {
		return "", "", "", false, err
	}
	if found {
		if err := mailbox.ValidClientID(clientID); err != nil {
			return "", "", "", false, fmt.Errorf("%w", err)
		}
	}
	if len(rest) < 2 || strings.HasPrefix(rest[0], "-") {
		return "", "", "", false, errors.New("usage: agentbus reply <id> <body> [--client-id <id>] [--json]")
	}
	return rest[0], strings.Join(rest[1:], " "), clientID, asJSON, nil
}
