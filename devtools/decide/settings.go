package decide

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/google/renameio/v2"
)

// SetConfigValue updates one top-level or nested dot-separated key (such as
// "messageAvatars" or "ui.answerAction") in this machine's config.json and
// returns the validated Config after the write.
//
// Unknown keys and existing key order are preserved. A config that fails to
// parse or validate, or is not writable, is left unchanged. The first write
// in a process copies the original file to config.json.bak. The write is atomic
// via a temp file and rename in the same directory.
func SetConfigValue(dotKey string, value any) (Config, error) {
	path, ok := configPath()
	if !ok {
		return Config{}, fmt.Errorf("decide config: no home directory")
	}
	return SetConfigValueAt(path, dotKey, value)
}

// SetConfigValueAt updates one top-level or nested dot-separated key in path
// and returns the validated Config after the write.
func SetConfigValueAt(path, dotKey string, value any) (Config, error) {
	parts := strings.Split(dotKey, ".")
	for _, p := range parts {
		if p == "" {
			return Config{}, fmt.Errorf("decide config: invalid key %q", dotKey)
		}
	}
	target, data, perm, err := readConfigForWrite(path)
	if err != nil {
		return Config{}, err
	}
	if _, err := parseConfig(data, filepath.Dir(path)); err != nil {
		return Config{}, err
	}
	src := data
	if src == nil {
		src = []byte("{}")
	}
	out, err := editNestedMember(src, parts, value)
	if err != nil {
		return Config{}, fmt.Errorf("edit decide config: %w", err)
	}
	cfg, err := parseConfig(out, filepath.Dir(path))
	if err != nil {
		return Config{}, err
	}
	if data != nil {
		if err := backupConfig(path, data, perm); err != nil {
			return Config{}, err
		}
	}
	if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
		return Config{}, fmt.Errorf("write decide config: %w", err)
	}
	if err := renameio.WriteFile(target, out, perm, renameio.WithTempDir(filepath.Dir(target))); err != nil {
		return Config{}, fmt.Errorf("write decide config: %w", err)
	}
	return cfg, nil
}

// editNestedMember returns src with the nested key path set to value, indented
// by two spaces with a trailing newline. Other keys and their order are kept.
func editNestedMember(src []byte, path []string, value any) ([]byte, error) {
	raw, err := marshalPlain(value)
	if err != nil {
		return nil, err
	}
	compact, err := setNestedObject(src, path, raw)
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

func setNestedObject(src []byte, path []string, raw json.RawMessage) (json.RawMessage, error) {
	members, err := parseObject(src)
	if err != nil {
		return nil, err
	}
	key := path[0]
	idx := memberIndex(members, key)
	if len(path) == 1 {
		if idx >= 0 {
			members[idx].value = raw
		} else {
			members = append(members, jsonMember{key: key, value: raw})
		}
		return writeObject(members)
	}
	var childSrc []byte
	if idx >= 0 {
		childSrc = members[idx].value
	} else {
		childSrc = []byte("{}")
	}
	childVal, err := setNestedObject(childSrc, path[1:], raw)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", key, err)
	}
	if idx >= 0 {
		members[idx].value = childVal
	} else {
		members = append(members, jsonMember{key: key, value: childVal})
	}
	return writeObject(members)
}
