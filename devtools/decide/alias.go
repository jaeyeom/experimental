package decide

import "strings"

// SetContactAliases replaces the contactAliases display key and returns
// the pairs stored. Blank keys and values are dropped. Other keys and
// their order are kept. A config that does not parse or is not writable
// is left unchanged.
func SetContactAliases(aliases map[string]string) (map[string]string, error) {
	aliases = normalizeAliases(aliases)
	if err := writeConfigMember("contactAliases", aliases); err != nil {
		return nil, err
	}
	return aliases, nil
}

// normalizeAliases trims pairs and drops blanks. The result is never nil.
func normalizeAliases(aliases map[string]string) map[string]string {
	out := map[string]string{}
	for name, agent := range aliases {
		name = strings.TrimSpace(name)
		agent = strings.TrimSpace(agent)
		if name == "" || agent == "" {
			continue
		}
		out[name] = agent
	}
	return out
}
