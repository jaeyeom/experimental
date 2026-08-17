package config

import (
	"bufio"
	"fmt"
	"io"
	"regexp"
	"strings"
)

var keyPattern = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// Parse reads a shell-sourceable KEY=VALUE file.
func Parse(r io.Reader) (map[string]string, error) {
	out := make(map[string]string)
	sc := bufio.NewScanner(r)
	line := 0
	for sc.Scan() {
		line++
		raw := sc.Text()
		trimmed := strings.TrimSpace(raw)
		if trimmed == "" || strings.HasPrefix(trimmed, "#") {
			continue
		}
		key, val, ok := strings.Cut(raw, "=")
		if !ok {
			return nil, &KeyError{Line: line, Reason: "missing '='"}
		}
		key = strings.TrimSpace(key)
		if key == "" {
			return nil, &KeyError{Line: line, Reason: "empty key"}
		}
		if !keyPattern.MatchString(key) {
			return nil, &KeyError{Key: key, Line: line, Reason: "invalid key"}
		}
		out[key] = unquote(strings.TrimSpace(val))
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("read config: %w", err)
	}
	return out, nil
}

func unquote(v string) string {
	if len(v) < 2 {
		return v
	}
	if v[0] == '"' && v[len(v)-1] == '"' {
		return unescapeDouble(v[1 : len(v)-1])
	}
	if v[0] == '\'' && v[len(v)-1] == '\'' {
		return v[1 : len(v)-1]
	}
	return v
}

func unescapeDouble(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); i++ {
		if s[i] == '\\' && i+1 < len(s) && (s[i+1] == '"' || s[i+1] == '\\') {
			b.WriteByte(s[i+1])
			i++
			continue
		}
		b.WriteByte(s[i])
	}
	return b.String()
}
