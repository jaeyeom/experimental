package decide

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sync"

	"github.com/google/renameio/v2"
)

// backedUp holds the config paths this process has copied to .bak.
var (
	backedUpMu sync.Mutex
	backedUp   = map[string]bool{} //nolint:pkgvar // one backup per config per process
)

// jsonMember is one key of a JSON object, kept in file order.
type jsonMember struct {
	key   string
	value json.RawMessage
}

// SetAvatarRule writes name's exact-match avatar rule to this machine's
// config and returns the config's avatar rules after the write. rule
// carries Builtin, File, or Hide; a zero rule removes name's exact rules.
// A new rule goes first. Other rules, keys, and their order are kept.
//
// A config that does not parse or is not writable is left unchanged.
// The write goes to a temp file in the same directory, then a rename;
// a symlinked config has its target replaced. The first write in a
// process copies the original to config.json.bak.
func SetAvatarRule(name string, rule AvatarRule) ([]AvatarRule, error) {
	path, ok := configPath()
	if !ok {
		return nil, fmt.Errorf("decide config: no home directory")
	}
	target, data, perm, err := readConfigForWrite(path)
	if err != nil {
		return nil, err
	}
	if _, err := parseConfig(data, filepath.Dir(path)); err != nil {
		return nil, err
	}
	src := data
	if src == nil {
		src = []byte("{}")
	}
	out, err := editAvatars(src, name, rule)
	if err != nil {
		return nil, fmt.Errorf("edit decide config: %w", err)
	}
	cfg, err := parseConfig(out, filepath.Dir(path))
	if err != nil {
		return nil, err
	}
	if data != nil {
		if err := backupConfig(path, data, perm); err != nil {
			return nil, err
		}
	}
	if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
		return nil, fmt.Errorf("write decide config: %w", err)
	}
	if err := renameio.WriteFile(target, out, perm, renameio.WithTempDir(filepath.Dir(target))); err != nil {
		return nil, fmt.Errorf("write decide config: %w", err)
	}
	return cfg.Avatars, nil
}

// readConfigForWrite resolves path's symlinks and reads the file. A
// missing file has nil data. A file this process cannot write is an
// error.
func readConfigForWrite(path string) (target string, data []byte, perm fs.FileMode, err error) {
	target, err = filepath.EvalSymlinks(path)
	if errors.Is(err, fs.ErrNotExist) {
		if _, lerr := os.Lstat(path); lerr == nil {
			return "", nil, 0, fmt.Errorf("decide config %s: %w", path, err) // a dangling link
		}
		return path, nil, 0o600, nil
	}
	if err != nil {
		return "", nil, 0, fmt.Errorf("decide config: %w", err)
	}
	f, err := os.OpenFile(target, os.O_RDWR, 0) //nolint:gosec // G304: the user's own config
	if err != nil {
		return "", nil, 0, fmt.Errorf("decide config is not writable: %w", err)
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return "", nil, 0, fmt.Errorf("decide config: %w", err)
	}
	data, err = io.ReadAll(f)
	if err != nil {
		return "", nil, 0, fmt.Errorf("read decide config: %w", err)
	}
	return target, data, info.Mode().Perm(), nil
}

// backupConfig copies data to path.bak once per process.
func backupConfig(path string, data []byte, perm fs.FileMode) error {
	backedUpMu.Lock()
	defer backedUpMu.Unlock()
	if backedUp[path] {
		return nil
	}
	bak := path + ".bak"
	if err := renameio.WriteFile(bak, data, perm, renameio.WithTempDir(filepath.Dir(bak))); err != nil {
		return fmt.Errorf("back up decide config: %w", err)
	}
	backedUp[path] = true
	return nil
}

// editAvatars returns the config src with name's exact rule set to rule,
// indented by two spaces.
func editAvatars(src []byte, name string, rule AvatarRule) ([]byte, error) {
	top, err := parseObject(src)
	if err != nil {
		return nil, err
	}
	i := memberIndex(top, "avatars")
	var rules []json.RawMessage
	if i >= 0 {
		if err := json.Unmarshal(top[i].value, &rules); err != nil {
			return nil, fmt.Errorf("avatars: %w", err)
		}
	}
	rules, err = setExactRule(rules, name, rule)
	if err != nil {
		return nil, err
	}
	if rules == nil {
		rules = []json.RawMessage{}
	}
	value, err := marshalPlain(rules)
	if err != nil {
		return nil, err
	}
	if i >= 0 {
		top[i].value = value
	} else {
		top = append(top, jsonMember{"avatars", value})
	}
	compact, err := writeObject(top)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	if err := json.Indent(&out, compact, "", "  "); err != nil {
		return nil, fmt.Errorf("indent: %w", err)
	}
	out.WriteByte('\n')
	return out.Bytes(), nil
}

// setExactRule updates the first rule whose match is name, or puts a new
// one first. A zero rule drops every rule whose match is name.
func setExactRule(rules []json.RawMessage, name string, rule AvatarRule) ([]json.RawMessage, error) {
	remove := rule.File == "" && rule.Builtin == "" && !rule.Hide
	var out []json.RawMessage
	done := false
	for _, raw := range rules {
		obj, err := parseObject(raw)
		if err != nil {
			return nil, fmt.Errorf("avatars: %w", err)
		}
		var match string
		if j := memberIndex(obj, "match"); j >= 0 {
			_ = json.Unmarshal(obj[j].value, &match)
		}
		if match != name || done {
			out = append(out, raw)
			continue
		}
		if remove {
			continue
		}
		edited, err := withChoice(obj, rule)
		if err != nil {
			return nil, err
		}
		out = append(out, edited)
		done = true
	}
	if remove || done {
		return out, nil
	}
	match, err := marshalPlain(name)
	if err != nil {
		return nil, err
	}
	fresh, err := withChoice([]jsonMember{{"match", match}}, rule)
	if err != nil {
		return nil, err
	}
	return append([]json.RawMessage{fresh}, out...), nil
}

// withChoice replaces obj's file, builtin, and hide with rule's.
func withChoice(obj []jsonMember, rule AvatarRule) (json.RawMessage, error) {
	kept := obj[:0:0]
	for _, m := range obj {
		if m.key != "file" && m.key != "builtin" && m.key != "hide" {
			kept = append(kept, m)
		}
	}
	add := func(key string, v any) error {
		raw, err := marshalPlain(v)
		if err != nil {
			return err
		}
		kept = append(kept, jsonMember{key, raw})
		return nil
	}
	var err error
	switch {
	case rule.Hide:
		err = add("hide", true)
	case rule.Builtin != "":
		err = add("builtin", rule.Builtin)
	default:
		err = add("file", rule.File)
	}
	if err != nil {
		return nil, err
	}
	return writeObject(kept)
}

// parseObject splits a JSON object into its members in order. Anything
// after the object is an error.
func parseObject(data []byte) ([]jsonMember, error) {
	dec := json.NewDecoder(bytes.NewReader(data))
	tok, err := dec.Token()
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	if d, ok := tok.(json.Delim); !ok || d != '{' {
		return nil, fmt.Errorf("parse: not a JSON object")
	}
	var members []jsonMember
	for dec.More() {
		tok, err := dec.Token()
		if err != nil {
			return nil, fmt.Errorf("parse: %w", err)
		}
		key, _ := tok.(string)
		var value json.RawMessage
		if err := dec.Decode(&value); err != nil {
			return nil, fmt.Errorf("parse %q: %w", key, err)
		}
		members = append(members, jsonMember{key, value})
	}
	if _, err := dec.Token(); err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return nil, fmt.Errorf("parse: data after the object")
	}
	return members, nil
}

func memberIndex(members []jsonMember, key string) int {
	for i, m := range members {
		if m.key == key {
			return i
		}
	}
	return -1
}

// writeObject is members as one compact JSON object.
func writeObject(members []jsonMember) (json.RawMessage, error) {
	var b bytes.Buffer
	b.WriteByte('{')
	for i, m := range members {
		if i > 0 {
			b.WriteByte(',')
		}
		key, err := marshalPlain(m.key)
		if err != nil {
			return nil, err
		}
		b.Write(key)
		b.WriteByte(':')
		if err := json.Compact(&b, m.value); err != nil {
			return nil, fmt.Errorf("compact %q: %w", m.key, err)
		}
	}
	b.WriteByte('}')
	return b.Bytes(), nil
}

// marshalPlain is json.Marshal without HTML escaping.
func marshalPlain(v any) (json.RawMessage, error) {
	var b bytes.Buffer
	enc := json.NewEncoder(&b)
	enc.SetEscapeHTML(false)
	if err := enc.Encode(v); err != nil {
		return nil, fmt.Errorf("encode: %w", err)
	}
	return bytes.TrimRight(b.Bytes(), "\n"), nil
}
