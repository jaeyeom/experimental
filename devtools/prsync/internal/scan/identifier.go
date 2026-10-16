package scan

import (
	"regexp"
	"strconv"
	"strings"
)

// prNumberLabelRe matches a tab label that is only "#N", with no leading zero.
// "#1#2" and "#42 extra" are not pull-request numbers.
var prNumberLabelRe = regexp.MustCompile(`^#([1-9][0-9]*)$`)

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

// prNumberLabel reports the pull request number when label is exactly "#N".
func prNumberLabel(label string) (int, bool) {
	match := prNumberLabelRe.FindStringSubmatch(strings.TrimSpace(label))
	if match == nil {
		return 0, false
	}
	number, err := strconv.Atoi(match[1])
	if err != nil || number <= 0 {
		return 0, false
	}
	return number, true
}
