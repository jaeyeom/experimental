package orgctl

import (
	"crypto/sha256"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
)

const (
	// BriefOK means the file matches the rendered brief.
	BriefOK = "ok"
	// BriefEdited means the file exists and differs from the render.
	BriefEdited = "edited in place"
	// BriefMissing means the brief file is absent.
	BriefMissing = "missing"
)

// Render builds the brief file body from brief.text and the org rules
// that apply to the role. A nil role.Rules list means every org rule.
// Rules are ordered by id. Line endings are LF.
func Render(role Role, org Spec) string {
	var b strings.Builder
	text := strings.TrimRight(normalizeNewlines(role.Brief.Text), "\n")
	if text != "" {
		b.WriteString(text)
		b.WriteByte('\n')
	}
	for _, rule := range applicableRules(role, org) {
		if b.Len() > 0 {
			b.WriteByte('\n')
		}
		b.WriteString(rule.ID)
		b.WriteByte('\n')
		b.WriteString(strings.TrimRight(normalizeNewlines(rule.Text), "\n"))
		b.WriteByte('\n')
	}
	return b.String()
}

// Hash is the sha256 of Render.
func Hash(role Role, org Spec) [32]byte {
	return sha256.Sum256([]byte(Render(role, org)))
}

// Compare hashes the rendered brief against the file at brief.file.
// A relative path is joined to dir, the role cwd. The file is opened
// read-only. v1 never writes it.
func Compare(role Role, org Spec, dir string) (string, error) {
	path := briefPath(dir, role)
	if path == "" {
		return BriefMissing, nil
	}
	data, err := readBrief(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return BriefMissing, nil
		}
		return "", err
	}
	if sha256.Sum256(data) == Hash(role, org) {
		return BriefOK, nil
	}
	return BriefEdited, nil
}

func briefPath(dir string, role Role) string {
	file := role.Brief.File
	if file == "" {
		return ""
	}
	if filepath.IsAbs(file) {
		return file
	}
	return filepath.Join(dir, file)
}

func readBrief(path string) ([]byte, error) {
	f, err := os.Open(path) //nolint:gosec // G304: the role brief path, read-only
	if err != nil {
		return nil, fmt.Errorf("open brief: %w", err)
	}
	data, err := io.ReadAll(f)
	cerr := f.Close()
	if err != nil {
		return nil, fmt.Errorf("read brief: %w", err)
	}
	if cerr != nil {
		return nil, fmt.Errorf("close brief: %w", cerr)
	}
	return data, nil
}

func applicableRules(role Role, org Spec) []Rule {
	byID := make(map[string]Rule, len(org.Rules))
	for _, rule := range org.Rules {
		byID[rule.ID] = rule
	}
	ids := role.Rules
	if ids == nil {
		ids = make([]string, len(org.Rules))
		for i, rule := range org.Rules {
			ids[i] = rule.ID
		}
	}
	rules := make([]Rule, 0, len(ids))
	seen := map[string]bool{}
	for _, id := range ids {
		rule, ok := byID[id]
		if !ok || seen[id] {
			continue
		}
		seen[id] = true
		rules = append(rules, rule)
	}
	slices.SortFunc(rules, func(a, b Rule) int {
		return strings.Compare(a.ID, b.ID)
	})
	return rules
}

func normalizeNewlines(s string) string {
	s = strings.ReplaceAll(s, "\r\n", "\n")
	return strings.ReplaceAll(s, "\r", "\n")
}
