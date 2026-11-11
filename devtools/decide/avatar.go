package decide

import (
	"os"
	"path"
	"path/filepath"
	"strings"
)

// AvatarMonoHide is the avatarMono value that hides avatars without color.
const AvatarMonoHide = "hide"

// AvatarRule maps a TUI name to an avatar file. Match is an exact name or a
// path.Match glob. LoadConfig makes File absolute: a leading "~" is the
// home directory and a relative path is relative to the config file.
type AvatarRule struct {
	Match string `json:"match"`
	File  string `json:"file"`
}

// MatchAvatar returns the rule for name. An exact match wins over a glob.
// Otherwise the first matching glob wins.
func MatchAvatar(rules []AvatarRule, name string) (AvatarRule, bool) {
	for _, r := range rules {
		if r.Match == name {
			return r, true
		}
	}
	for _, r := range rules {
		if ok, err := path.Match(r.Match, name); err == nil && ok {
			return r, true
		}
	}
	return AvatarRule{}, false
}

func resolveAvatarFiles(rules []AvatarRule, configDir string) {
	for i := range rules {
		rules[i].File = resolveAvatarFile(rules[i].File, configDir)
	}
}

func resolveAvatarFile(file, configDir string) string {
	if file == "~" || strings.HasPrefix(file, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return file
		}
		return filepath.Join(home, file[1:])
	}
	if file == "" || filepath.IsAbs(file) {
		return file
	}
	return filepath.Join(configDir, file)
}
