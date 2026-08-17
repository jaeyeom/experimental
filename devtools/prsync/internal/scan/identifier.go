package scan

import "regexp"

// ExtractID returns the first title_id_pattern match, or nil if none.
func ExtractID(title string, re *regexp.Regexp) *string {
	if re == nil {
		return nil
	}
	loc := re.FindStringIndex(title)
	if loc == nil {
		return nil
	}
	s := title[loc[0]:loc[1]]
	return &s
}
