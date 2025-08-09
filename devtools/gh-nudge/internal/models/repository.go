package models

import "fmt"

// Repository represents a GitHub repository with owner and name.
type Repository struct {
	Owner string `json:"owner"`
	Name  string `json:"name"`
}

// NewRepository creates a new Repository instance.
func NewRepository(owner, name string) Repository {
	return Repository{
		Owner: owner,
		Name:  name,
	}
}

// String returns the repository in "owner/name" format.
func (r Repository) String() string {
	return fmt.Sprintf("%s/%s", r.Owner, r.Name)
}

// IsValid returns true if both owner and name are non-empty.
func (r Repository) IsValid() bool {
	return r.Owner != "" && r.Name != ""
}
