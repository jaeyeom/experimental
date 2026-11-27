package config

import (
	"fmt"
	"strings"
	"time"
	"unicode"
)

// Defaults for loop pre-round gate and pause settings.
const (
	defaultLoopPreRoundTimeout = 30 * time.Second
	defaultLoopGatePause       = 5 * time.Minute
	defaultLoopGatePauseMax    = time.Hour
)

func applyLoopKey(cfg *Config, key, val string) error {
	switch key {
	case "loop_pre_round_cmd":
		argv, err := splitArgv(key, val)
		if err != nil {
			return err
		}
		cfg.LoopPreRoundCmd = argv
		return nil
	case "loop_pre_round_timeout":
		return applyPositiveDuration(&cfg.LoopPreRoundTimeout, key, val)
	case "loop_gate_pause":
		return applyPositiveDuration(&cfg.LoopGatePause, key, val)
	case "loop_gate_pause_max":
		return applyPositiveDuration(&cfg.LoopGatePauseMax, key, val)
	case "loop_wake_agentbus_name":
		cfg.LoopWakeAgentbusName = strings.TrimSpace(val)
		return nil
	default:
		return nil
	}
}

func validateLoopConfig(c Config) error {
	if c.AgentbusBin == "" {
		return &KeyError{Key: "agentbus_bin", Reason: "must be non-empty"}
	}
	if c.LoopPreRoundTimeout <= 0 {
		return &KeyError{Key: "loop_pre_round_timeout", Reason: "must be > 0"}
	}
	if c.LoopGatePause <= 0 {
		return &KeyError{Key: "loop_gate_pause", Reason: "must be > 0"}
	}
	if c.LoopGatePauseMax <= 0 {
		return &KeyError{Key: "loop_gate_pause_max", Reason: "must be > 0"}
	}
	return nil
}

func applyPositiveDuration(dst *time.Duration, key, val string) error {
	d, err := time.ParseDuration(strings.TrimSpace(val))
	if err != nil || d <= 0 {
		return &KeyError{Key: key, Reason: fmt.Sprintf("invalid duration %q; want a positive Go duration", val)}
	}
	*dst = d
	return nil
}

// splitArgv parses a direct argv command string with single/double quote and
// backslash support without invoking a shell.
func splitArgv(key, val string) ([]string, error) {
	var args []string
	for i := 0; i < len(val); {
		for i < len(val) && unicode.IsSpace(rune(val[i])) {
			i++
		}
		if i >= len(val) {
			break
		}
		tok, next, err := readArgvToken(key, val, i)
		if err != nil {
			return nil, err
		}
		args = append(args, tok)
		i = next
	}
	return args, nil
}

func readArgvToken(key, val string, i int) (string, int, error) {
	var cur strings.Builder
	for i < len(val) && !unicode.IsSpace(rune(val[i])) {
		switch val[i] {
		case '\'':
			next, err := readSingleQuoted(key, val, i+1, &cur)
			if err != nil {
				return "", 0, err
			}
			i = next
		case '"':
			next, err := readDoubleQuoted(key, val, i+1, &cur)
			if err != nil {
				return "", 0, err
			}
			i = next
		case '\\':
			i++
			if i < len(val) {
				cur.WriteByte(val[i])
				i++
			} else {
				cur.WriteByte('\\')
			}
		default:
			cur.WriteByte(val[i])
			i++
		}
	}
	return cur.String(), i, nil
}

func readSingleQuoted(key, val string, i int, cur *strings.Builder) (int, error) {
	for i < len(val) {
		if val[i] == '\'' {
			return i + 1, nil
		}
		cur.WriteByte(val[i])
		i++
	}
	return 0, &KeyError{Key: key, Reason: "unterminated quote"}
}

func readDoubleQuoted(key, val string, i int, cur *strings.Builder) (int, error) {
	for i < len(val) {
		switch val[i] {
		case '"':
			return i + 1, nil
		case '\\':
			if i+1 < len(val) && (val[i+1] == '"' || val[i+1] == '\\') {
				cur.WriteByte(val[i+1])
				i += 2
				continue
			}
			cur.WriteByte(val[i])
			i++
		default:
			cur.WriteByte(val[i])
			i++
		}
	}
	return 0, &KeyError{Key: key, Reason: "unterminated quote"}
}
