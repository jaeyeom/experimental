package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func cmdRegister(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	name, when, err := parseRegister(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus register: %s\n", err)
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "register", Name: name, When: when, Env: env}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	fmt.Fprintf(stdout, "registered %s\n", name)
	return 0
}

func cmdDescribe(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	when, err := parseDescribe(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus describe: %s\n", err)
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "describe", When: when, Env: env}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	for _, name := range strings.Split(resp.Name, "\n") {
		if name == "" {
			continue
		}
		fmt.Fprintf(stdout, "described %s\n", name)
	}
	return 0
}

func cmdWhoami(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintf(stderr, "agentbus whoami: unexpected arguments\n")
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "whoami", Env: env}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	if len(resp.Names) == 0 {
		fmt.Fprintln(stdout, resp.Handle)
		return 0
	}
	for _, name := range resp.Names {
		fmt.Fprintf(stdout, "%s\nwhen: %s\n", name.Name, name.When)
	}
	return 0
}

func cmdRelease(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	name, to, err := parseRelease(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus release: %s\n", err)
		return 2
	}
	resp, code := callDaemon(ctx, env, daemon.Request{Op: "release", Name: name, To: to, Env: env}, stderr)
	if code != 0 {
		return code
	}
	if !resp.OK {
		fmt.Fprintf(stderr, "agentbus: %s\n", resp.Error)
		return 1
	}
	if resp.Handle == "" {
		fmt.Fprintf(stdout, "released %s\n", name)
		return 0
	}
	fmt.Fprintf(stdout, "handed off %s to %s\n", name, resp.Handle)
	return 0
}

func callDaemon(ctx context.Context, env []string, req daemon.Request, stderr io.Writer) (daemon.Response, int) {
	if hubRouted(ctx) {
		// The hub's daemon: never start or fall back to a local one.
		resp, err := dialBus(ctx, env, req)
		if err != nil {
			if _, ok := tunnelFrom(ctx); ok {
				fmt.Fprintf(stderr, "agentbus: hub tunnel: %s\n", err)
			} else {
				fmt.Fprintf(stderr, "agentbus: %s\n", err)
			}
			return daemon.Response{}, 1
		}
		return resp, 0
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return daemon.Response{}, 1
	}
	resp, err := dialDaemon(ctx, dirs.SocketPath, req)
	if err == nil {
		return resp, 0
	}
	exe, err2 := os.Executable()
	if err2 != nil {
		fmt.Fprintf(stderr, "agentbus: cannot find this executable: %s\n", err2)
		return daemon.Response{}, 1
	}
	bin, err2 := client.DaemonBinBeside(exe)
	if err2 != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err2)
		return daemon.Response{}, 1
	}
	if err2 = client.Ensure(ctx, dirs, bin, env); err2 != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err2)
		return daemon.Response{}, 1
	}
	resp, err = dialDaemon(ctx, dirs.SocketPath, req)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return daemon.Response{}, 1
	}
	return resp, 0
}

func parseRegister(args []string) (string, string, error) {
	when, rest, found, err := takeFlag(args, "--when")
	if err != nil {
		return "", "", err
	}
	if !found || strings.TrimSpace(when) == "" {
		return "", "", errors.New("--when is required")
	}
	if len(rest) != 1 || strings.HasPrefix(rest[0], "-") {
		return "", "", errors.New("usage: agentbus register <name> --when <text>")
	}
	return rest[0], when, nil
}

func parseDescribe(args []string) (string, error) {
	when, rest, found, err := takeFlag(args, "--when")
	if err != nil {
		return "", err
	}
	if !found || strings.TrimSpace(when) == "" {
		return "", errors.New("--when is required")
	}
	if len(rest) != 0 {
		return "", errors.New("usage: agentbus describe --when <text>")
	}
	return when, nil
}

func parseRelease(args []string) (string, string, error) {
	to, rest, _, err := takeFlag(args, "--to")
	if err != nil {
		return "", "", err
	}
	if len(rest) != 1 || strings.HasPrefix(rest[0], "-") {
		return "", "", errors.New("usage: agentbus release <name> [--to <claimer>]")
	}
	return rest[0], to, nil
}

func takeFlag(args []string, flagName string) (string, []string, bool, error) {
	var rest []string
	var val string
	found := false
	for i := 0; i < len(args); i++ {
		arg := args[i]
		if arg == flagName {
			if found {
				return "", nil, true, fmt.Errorf("%s given twice", flagName)
			}
			if i+1 >= len(args) {
				return "", nil, true, fmt.Errorf("%s needs a value", flagName)
			}
			found = true
			val = args[i+1]
			i++
			continue
		}
		prefix := flagName + "="
		if strings.HasPrefix(arg, prefix) {
			if found {
				return "", nil, true, fmt.Errorf("%s given twice", flagName)
			}
			found = true
			val = strings.TrimPrefix(arg, prefix)
			continue
		}
		rest = append(rest, arg)
	}
	return val, rest, found, nil
}
