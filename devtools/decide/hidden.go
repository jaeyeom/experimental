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

// SetHiddenContacts replaces the hiddenContacts display key and returns
// the names stored. Blank names are dropped. Other keys and their order
// are kept. A config that does not parse or is not writable is left
// unchanged. The write matches SetAvatarRule: a temp file, one backup,
// then a rename, including through a symlink.
func SetHiddenContacts(names []string) ([]string, error) {
	names = normalizeHidden(names)
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
	out, err := editHidden(src, names)
	if err != nil {
		return nil, fmt.Errorf("edit decide config: %w", err)
	}
	if _, err := parseConfig(out, filepath.Dir(path)); err != nil {
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
	return names, nil
}

// normalizeHidden trims names, drops blanks, and keeps the first of
// each name.
func normalizeHidden(names []string) []string {
	seen := map[string]bool{}
	out := make([]string, 0, len(names))
	for _, name := range names {
		name = strings.TrimSpace(name)
		if name == "" || seen[name] {
			continue
		}
		seen[name] = true
		out = append(out, name)
	}
	return out
}

// editHidden returns src with hiddenContacts set to names.
func editHidden(src []byte, names []string) ([]byte, error) {
	top, err := parseObject(src)
	if err != nil {
		return nil, err
	}
	value, err := marshalPlain(names)
	if err != nil {
		return nil, err
	}
	if i := memberIndex(top, "hiddenContacts"); i >= 0 {
		top[i].value = value
	} else {
		top = append(top, jsonMember{key: "hiddenContacts", value: value})
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
