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
	if err := writeConfigMember("hiddenContacts", names); err != nil {
		return nil, err
	}
	return names, nil
}

// SetWebConfig replaces the top-level "web" config key after backing up
// config.json to config.json.bak.
func SetWebConfig(web WebConfig) error {
	return writeConfigMember("web", web)
}

// SetWebConfigAt replaces the top-level "web" config key in path after backing
// up path to path.bak.
func SetWebConfigAt(path string, web WebConfig) error {
	return writeConfigMemberAt(path, "web", web)
}

// writeConfigMember replaces one top-level config key. The write matches
// SetAvatarRule: a temp file, one backup, then a rename, including
// through a symlink.
func writeConfigMember(key string, value any) error {
	path, ok := configPath()
	if !ok {
		return fmt.Errorf("decide config: no home directory")
	}
	return writeConfigMemberAt(path, key, value)
}

func writeConfigMemberAt(path, key string, value any) error {
	target, data, perm, err := readConfigForWrite(path)
	if err != nil {
		return err
	}
	if _, err := parseConfig(data, filepath.Dir(path)); err != nil {
		return err
	}
	src := data
	if src == nil {
		src = []byte("{}")
	}
	out, err := editMember(src, key, value)
	if err != nil {
		return fmt.Errorf("edit decide config: %w", err)
	}
	if _, err := parseConfig(out, filepath.Dir(path)); err != nil {
		return err
	}
	if data != nil {
		if err := backupConfig(path, data, perm); err != nil {
			return err
		}
	}
	if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
		return fmt.Errorf("write decide config: %w", err)
	}
	if err := renameio.WriteFile(target, out, perm, renameio.WithTempDir(filepath.Dir(target))); err != nil {
		return fmt.Errorf("write decide config: %w", err)
	}
	return nil
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

// editMember returns src with key set to value. Other keys and their
// order are kept.
func editMember(src []byte, key string, value any) ([]byte, error) {
	top, err := parseObject(src)
	if err != nil {
		return nil, err
	}
	raw, err := marshalPlain(value)
	if err != nil {
		return nil, err
	}
	if i := memberIndex(top, key); i >= 0 {
		top[i].value = raw
	} else {
		top = append(top, jsonMember{key: key, value: raw})
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
