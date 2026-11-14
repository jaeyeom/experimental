package decide

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
)

// MaxHubAvatarSize caps one avatar file the hub sends a client.
const MaxHubAvatarSize = 256 << 10

// HubAvatarPrefix starts the File of an avatar rule that came from the
// hub. The client renders it from the bytes the hub sent, never from
// its own disk.
const HubAvatarPrefix = "hub:"

// Sources of an effective config key.
const (
	SourceLocal = "local"
	SourceHub   = "hub"
)

// DisplayKeys are the config keys a hub shares with its clients. A key
// not listed here is never exported.
func DisplayKeys() []string {
	return []string{"avatarDefault", "avatarMono", "avatars", "contactAliases", "hiddenContacts", "hyperlinks", "recipients", "recipientsTimeout", "targets"}
}

// copyDisplayKey sets dst's field for display key key from src.
func copyDisplayKey(key string, dst, src *Config) {
	switch key {
	case "avatarDefault":
		dst.AvatarDefault = src.AvatarDefault
	case "avatarMono":
		dst.AvatarMono = src.AvatarMono
	case "avatars":
		dst.Avatars = src.Avatars
	case "contactAliases":
		dst.ContactAliases = src.ContactAliases
	case "hiddenContacts":
		dst.HiddenContacts = src.HiddenContacts
	case "hyperlinks":
		dst.Hyperlinks = src.Hyperlinks
	case "recipients":
		dst.Recipients = src.Recipients
	case "recipientsTimeout":
		dst.RecipientsTimeout = src.RecipientsTimeout
	case "targets":
		dst.Targets = src.Targets
	}
}

// ConfigFile is the parsed config and its top-level keys as written.
type ConfigFile struct {
	Config Config
	Keys   map[string]json.RawMessage
}

// LoadConfigFile reads the decide config file with its keys. A missing
// file is an empty config with no keys.
func LoadConfigFile() (ConfigFile, error) {
	cfg, err := LoadConfig()
	if err != nil {
		return ConfigFile{}, err
	}
	keys := map[string]json.RawMessage{}
	path, ok := configPath()
	if !ok {
		return ConfigFile{Config: cfg, Keys: keys}, nil
	}
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own config
	if errors.Is(err, fs.ErrNotExist) {
		return ConfigFile{Config: cfg, Keys: keys}, nil
	}
	if err != nil {
		return ConfigFile{}, fmt.Errorf("read decide config: %w", err)
	}
	members, err := parseObject(data)
	if err != nil {
		return ConfigFile{}, fmt.Errorf("decide config: %w", err)
	}
	for _, m := range members {
		keys[m.key] = m.value
	}
	return ConfigFile{Config: cfg, Keys: keys}, nil
}

// AvatarFile is one hub avatar file: its bytes, or why the hub could
// not read it.
type AvatarFile struct {
	Data  []byte `json:"data,omitempty"`
	Error string `json:"error,omitempty"`
}

// DisplaySettings are a hub's display keys as written, and the avatar
// files its rules name. Each file rule's File is a HubAvatarPrefix name
// in Files, so no hub path leaves the hub.
type DisplaySettings struct {
	Keys  map[string]json.RawMessage `json:"keys,omitempty"`
	Files map[string]AvatarFile      `json:"files,omitempty"`
}

// Display is f's display settings for a client, with avatar files read
// here.
func (f ConfigFile) Display() DisplaySettings {
	out := DisplaySettings{Keys: map[string]json.RawMessage{}}
	for _, key := range DisplayKeys() {
		raw, ok := f.Keys[key]
		if !ok {
			continue
		}
		if key != "avatars" {
			out.Keys[key] = raw
			continue
		}
		rules := slices.Clone(f.Config.Avatars)
		for i, r := range rules {
			if r.File == "" {
				continue
			}
			name := fmt.Sprintf("%s%d%s", HubAvatarPrefix, i, filepath.Ext(r.File))
			if out.Files == nil {
				out.Files = map[string]AvatarFile{}
			}
			out.Files[name] = readHubAvatar(r.File)
			rules[i].File = name
		}
		data, err := marshalPlain(rules)
		if err != nil {
			continue
		}
		out.Keys[key] = data
	}
	return out
}

// readHubAvatar reads path for a client. Its error never names the path.
func readHubAvatar(path string) AvatarFile {
	f, err := os.Open(path) //nolint:gosec // G304: the avatar path is operator config
	if errors.Is(err, fs.ErrNotExist) {
		return AvatarFile{Error: "file not found on the hub"}
	}
	if err != nil {
		return AvatarFile{Error: "file unreadable on the hub"}
	}
	defer f.Close()
	data, err := io.ReadAll(io.LimitReader(f, MaxHubAvatarSize+1))
	if err != nil {
		return AvatarFile{Error: "file unreadable on the hub"}
	}
	if len(data) > MaxHubAvatarSize {
		return AvatarFile{Error: "file on the hub is larger than 256 KB"}
	}
	return AvatarFile{Data: data}
}

// Effective is the local config with the hub's display settings for
// the keys it does not set. Sources and Values give each key's origin
// and value as written. HubKeys are the display keys the hub sent;
// Overrides are those the local config sets. Files are the hub's
// avatar files when its avatars are used.
type Effective struct {
	Config    Config
	Sources   map[string]string
	Values    map[string]json.RawMessage
	HubKeys   []string
	Overrides []string
	Files     map[string]AvatarFile
}

// MergeDisplay merges hub into local, one top-level key at a time: a
// key the local config sets keeps its local value. Keys off the
// DisplayKeys allowlist are ignored.
func MergeDisplay(local ConfigFile, hub DisplaySettings) (Effective, error) {
	eff := Effective{Config: local.Config, Sources: map[string]string{}, Values: map[string]json.RawMessage{}}
	for k, v := range local.Keys {
		eff.Sources[k] = SourceLocal
		eff.Values[k] = v
	}
	used := map[string]json.RawMessage{}
	for _, key := range DisplayKeys() {
		raw, ok := hub.Keys[key]
		if !ok {
			continue
		}
		eff.HubKeys = append(eff.HubKeys, key)
		if _, set := local.Keys[key]; set {
			eff.Overrides = append(eff.Overrides, key)
			continue
		}
		used[key] = raw
	}
	if len(used) == 0 {
		return eff, nil
	}
	data, err := json.Marshal(used)
	if err != nil {
		return Effective{}, fmt.Errorf("hub display settings: %w", err)
	}
	var hc Config
	if err := json.Unmarshal(data, &hc); err != nil {
		return Effective{}, fmt.Errorf("hub display settings: %w", err)
	}
	if err := checkAvatarRules(hc.Avatars); err != nil {
		return Effective{}, fmt.Errorf("hub display settings: %w", err)
	}
	if _, err := parseRecipientsTimeout(hc.RecipientsTimeout); err != nil {
		return Effective{}, fmt.Errorf("hub display settings: %w", err)
	}
	for key, raw := range used {
		copyDisplayKey(key, &eff.Config, &hc)
		eff.Sources[key] = SourceHub
		eff.Values[key] = raw
	}
	if _, ok := used["avatars"]; ok {
		eff.Files = hub.Files
	}
	return eff, nil
}

// Summary is one line naming how many display keys the hub sent and
// which the local config overrides.
func (e Effective) Summary() string {
	overrides := "none"
	if len(e.Overrides) > 0 {
		overrides = strings.Join(e.Overrides, ", ")
	}
	return fmt.Sprintf("display settings: hub (%d keys), local overrides: %s", len(e.HubKeys), overrides)
}
