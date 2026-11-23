package memstore

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// ContactsAttributes holds read-only enrichment attributes fetched from
// devtools/contacts for a linked person entity.
type ContactsAttributes struct {
	Team            string
	Role            string
	Timezone        string
	Handles         []string
	OpenCommitments []string
}

// loadContactsAttributes reads handles, team, role, timezone, and open
// commitments for contactsRef from the contacts store configured at
// contactsConfigPath without mutating the contacts database.
func loadContactsAttributes(contactsConfigPath, contactsRef string, getenv func(string) string, now func() time.Time) (ContactsAttributes, bool, error) {
	if contactsConfigPath == "" || contactsRef == "" {
		return ContactsAttributes{}, false, nil
	}
	if getenv == nil {
		getenv = func(string) string { return "" }
	}
	if now == nil {
		now = time.Now
	}

	cfg, err := contacts.LoadConfig(contactsConfigPath, getenv)
	if err != nil {
		return ContactsAttributes{}, false, fmt.Errorf("load contacts config %q: %w", contactsConfigPath, err)
	}
	store, err := contacts.Open(cfg.DataDir, now)
	if err != nil {
		return ContactsAttributes{}, false, fmt.Errorf("open contacts store %q: %w", cfg.DataDir, err)
	}
	defer store.Close()

	person, err := store.Get(contactsRef)
	if errors.Is(err, contacts.ErrNotFound) {
		return ContactsAttributes{}, false, nil
	}
	if err != nil {
		return ContactsAttributes{}, false, fmt.Errorf("get contact %q: %w", contactsRef, err)
	}

	commitments, err := store.Commitments(contacts.CommitmentFilter{
		PersonID: contactsRef,
		Open:     true,
	})
	if err != nil {
		return ContactsAttributes{}, false, fmt.Errorf("list commitments for %q: %w", contactsRef, err)
	}

	handles := make([]string, 0, len(person.Handles))
	for _, h := range person.Handles {
		handles = append(handles, h.String())
	}
	slices.Sort(handles)

	openCommitments := make([]string, 0, len(commitments))
	for _, c := range commitments {
		openCommitments = append(openCommitments, formatCommitment(c))
	}

	return ContactsAttributes{
		Team:            person.Team,
		Role:            person.Role,
		Timezone:        person.TZ,
		Handles:         handles,
		OpenCommitments: openCommitments,
	}, true, nil
}

func formatCommitment(c contacts.Commitment) string {
	var b strings.Builder
	fmt.Fprintf(&b, "[%s] %s", c.Direction, c.Text)
	if c.Due != "" {
		fmt.Fprintf(&b, " (due %s)", c.Due)
	}
	return b.String()
}
