package decide

// SetFavoriteContacts replaces the favoriteContacts display key and
// returns the names stored, in the order given. Blank and repeated names
// are dropped. Other keys and their order are kept. A config that does
// not parse or is not writable is left unchanged.
func SetFavoriteContacts(names []string) ([]string, error) {
	names = normalizeHidden(names)
	if err := writeConfigMember("favoriteContacts", names); err != nil {
		return nil, err
	}
	return names, nil
}
