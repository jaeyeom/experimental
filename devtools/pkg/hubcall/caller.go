package hubcall

import (
	"fmt"
	"strings"
)

// LocalMachine is the machine part of a handle on the local herdr server.
const LocalMachine = "local"

// Handle is the herdr handle herdr:<machine>/<session>/<pane>.
func Handle(machine, session, pane string) string {
	return "herdr:" + machine + "/" + session + "/" + pane
}

// ParseHandle splits a herdr handle into its machine, session and pane.
func ParseHandle(raw string) (machine, session, pane string, err error) {
	rest, ok := strings.CutPrefix(raw, "herdr:")
	if !ok {
		return "", "", "", fmt.Errorf("handle %q is not a herdr handle", raw)
	}
	machine, rest, ok = strings.Cut(rest, "/")
	if !ok || machine == "" {
		return "", "", "", fmt.Errorf("handle %q is missing a machine", raw)
	}
	session, pane, ok = strings.Cut(rest, "/")
	if !ok || session == "" || pane == "" {
		return "", "", "", fmt.Errorf("handle %q is missing a session or pane", raw)
	}
	return machine, session, pane, nil
}

// RemoteCaller accepts raw only as a herdr handle on a machine other
// than local: a caller that reached the hub from another machine.
func RemoteCaller(raw string) (string, error) {
	machine, _, _, err := ParseHandle(raw)
	if err != nil {
		return "", err
	}
	if machine == LocalMachine {
		return "", fmt.Errorf("handle %q must name a remote machine", raw)
	}
	return raw, nil
}

// OnMachine rewrites a herdr:local/... handle to herdr:<machine>/....
func OnMachine(raw, machine string) (string, error) {
	m, session, pane, err := ParseHandle(raw)
	if err != nil {
		return "", err
	}
	if m != LocalMachine {
		return "", fmt.Errorf("handle %q is not local", raw)
	}
	return Handle(machine, session, pane), nil
}

// TrustedCaller returns the hub-mode caller handle in env[key], or ""
// when key is unset. The caller side of a hub call sets key on the
// remote command. The hub trusts it only when SSH_CONNECTION shows the
// command came over ssh and the handle names a remote machine. Any SSH
// login as the same user can set it; that is the user account's trust
// boundary.
func TrustedCaller(env []string, key string) (string, error) {
	raw := envValue(env, key)
	if raw == "" {
		return "", nil
	}
	if envValue(env, "SSH_CONNECTION") == "" {
		return "", fmt.Errorf("%s is only trusted over ssh, from a hub-mode call", key)
	}
	if _, err := AnonCaller(raw); err == nil {
		return raw, nil
	}
	if _, err := RemoteCaller(raw); err != nil {
		return "", fmt.Errorf("%s: %w", key, err)
	}
	return raw, nil
}

// AnonCaller accepts raw only as anon:<host>-<pid>, the identity a
// hub-mode client stored for a sender that is not a herdr pane.
func AnonCaller(raw string) (string, error) {
	rest, ok := strings.CutPrefix(raw, "anon:")
	if !ok {
		return "", fmt.Errorf("handle %q is not an anon handle", raw)
	}
	dash := strings.LastIndex(rest, "-")
	if dash <= 0 || dash == len(rest)-1 {
		return "", fmt.Errorf("handle %q is not an anon handle", raw)
	}
	for _, r := range rest[dash+1:] {
		if r < '0' || r > '9' {
			return "", fmt.Errorf("handle %q is not an anon handle", raw)
		}
	}
	return raw, nil
}

func envValue(env []string, key string) string {
	for _, entry := range env {
		name, value, ok := strings.Cut(entry, "=")
		if ok && name == key {
			return value
		}
	}
	return ""
}
