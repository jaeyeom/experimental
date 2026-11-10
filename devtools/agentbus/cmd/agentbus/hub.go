package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// identifyTimeout bounds the local herdr call that names this pane.
const identifyTimeout = 5 * time.Second

// hubEnv overrides the config's hub for one shell.
const hubEnv = "AGENTBUS_HUB"

// identityCommands identify the caller, so hub mode passes the caller's
// handle on this machine to the hub.
var identityCommands = map[string]bool{
	"register": true,
	"describe": true,
	"whoami":   true,
	"release":  true,
	"send":     true,
	"reply":    true,
}

// localCommands run on this machine even in hub mode.
var localCommands = map[string]bool{
	"install-hooks": true,
}

// stdinCommands read stdin, so hub mode forwards it.
var stdinCommands = map[string]bool{
	"send":        true,
	"hook":        true,
	"prompt-hook": true,
}

// hookCommands exit 0 when the hub cannot be reached, as they do when
// the local bus is down: a down hub must not block a tool call or prompt.
var hookCommands = map[string]bool{
	"hook":        true,
	"prompt-hook": true,
}

// hubSettings reads the hub target and this machine's label. An empty
// target means hub mode is off.
func hubSettings(env []string) (config.Config, error) {
	var cfg config.Config
	// Without HOME there is no config file to read.
	if _, err := config.Path(env); err == nil {
		if cfg, err = config.Load(env); err != nil {
			return config.Config{}, fmt.Errorf("config: %w", err)
		}
	}
	if hub := strings.TrimSpace(envGet(env, hubEnv)); hub != "" {
		if strings.HasPrefix(hub, "-") {
			return config.Config{}, fmt.Errorf("%s must not start with -", hubEnv)
		}
		cfg.Hub = hub
	}
	return cfg, nil
}

// runOnHub runs one agentbus command on cfg.Hub over ssh. It never
// touches a local daemon or local state.
func runOnHub(ctx context.Context, cfg config.Config, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	name := ""
	if len(args) > 0 {
		name = args[0]
	}
	if name == "install" {
		fmt.Fprintf(stderr, "agentbus install: the daemon runs on the hub %s; run agentbus install there\n", cfg.Hub)
		return 1
	}
	call := hubcall.Call{Args: args, Stdout: stdout, Stderr: stderr}
	// Only these read stdin. ssh would otherwise drain the caller's stdin.
	if stdinCommands[name] {
		call.Stdin = stdin
	}
	if hookCommands[name] {
		call.Stderr = io.Discard
	}
	if name == "send" {
		var code int
		if call, code = sendFileOnStdin(call, stderr); code != 0 {
			return code
		}
	}
	if identityCommands[name] && envGet(env, "HERDR_PANE_ID") != "" {
		caller, err := callerHandle(ctx, cfg, env)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus: %s\n", err)
			return 1
		}
		call.Env = []string{herdr.AsHandleEnv + "=" + caller}
	}
	hub := hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}
	code, err := hub.Run(ctx, call)
	if err == nil {
		return code
	}
	if hookCommands[name] {
		return 0
	}
	fmt.Fprintf(stderr, "agentbus: %s\n", err)
	return code
}

// sendFileOnStdin reads a --file path here and sends its content on
// stdin, so the hub never needs the path.
func sendFileOnStdin(call hubcall.Call, stderr io.Writer) (hubcall.Call, int) {
	_, _, file, _, _, err := parseSend(call.Args[1:])
	if err != nil || file == "" {
		return call, 0
	}
	_, rest, _, _ := takeFlag(call.Args[1:], "--file")
	data, err := os.ReadFile(file)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return call, 1
	}
	call.Args = append([]string{"send"}, rest...)
	call.Stdin = strings.NewReader(string(data))
	return call, 0
}

// callerHandle is herdr:<machine>/<session>/<pane> for this pane.
func callerHandle(ctx context.Context, cfg config.Config, env []string) (string, error) {
	if cfg.Machine == "" {
		path, _ := config.Path(env)
		return "", fmt.Errorf("hub mode needs machine = \"<label>\" in %s, this machine's herdr label on the hub", path)
	}
	ctx, cancel := context.WithTimeout(ctx, identifyTimeout)
	defer cancel()
	local, err := herdr.LocalHandle(ctx, env)
	if err != nil {
		return "", fmt.Errorf("identify this pane: %w", err)
	}
	caller, err := herdr.OnMachine(local, cfg.Machine)
	if err != nil {
		return "", fmt.Errorf("identify this pane: %w", err)
	}
	return caller, nil
}
