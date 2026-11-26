package config

import (
	"fmt"
	"strings"
)

const (
	// TrustEnvTrusted marks a source whose tracker is restricted to trusted users
	// (e.g., corporate Jira or private single-team repos). External-author gating,
	// tamper guards, and prompt hardening default off.
	TrustEnvTrusted = "trusted"
	// TrustEnvUntrusted marks a source open to external/public contributors
	// (e.g., a public GitHub repository). External-author gating, tamper guards,
	// and conditional prompt hardening apply.
	TrustEnvUntrusted = "untrusted"

	// TrustGuardLocked requires untrusted-author issues to be locked against edits/comments.
	TrustGuardLocked = "locked"
	// TrustGuardApprovedHash requires a local approval hash (follow-up #1238).
	TrustGuardApprovedHash = "approved_hash"

	// TrustRulePlaceholder in start_prompt places the untrusted-input rule there
	// instead of at the top.
	TrustRulePlaceholder = "{{trust_rule}}"
)

// DefaultUntrustedAuthorAssociations returns the default author associations
// trusted when trust.environment is "untrusted".
func DefaultUntrustedAuthorAssociations() []string {
	return []string{"OWNER"}
}

// DefaultUntrustedReadyStatuses returns the default ready statuses allowed for
// untrusted authors when trust.environment is "untrusted".
func DefaultUntrustedReadyStatuses() []string {
	return []string{DefaultGitHubReleaseStatus}
}

// DefaultUntrustedGuards returns the default tamper guards enabled for
// untrusted authors when trust.environment is "untrusted".
func DefaultUntrustedGuards() []string {
	return []string{TrustGuardLocked}
}

// Trust is the optional [source.trust] table classifying whether a source's
// issue tracker is trusted or open to untrusted external authors.
type Trust struct {
	// Environment is "trusted", "untrusted", or empty (unset).
	Environment string `toml:"environment"`
	// TrustedAuthorAssociations lists backend author associations (e.g., "OWNER")
	// treated as trusted under environment = "untrusted". Authenticated me is
	// always trusted. Default ["OWNER"] when omitted under "untrusted".
	TrustedAuthorAssociations []string `toml:"trusted_author_associations"`
	// TrustedAuthors is an optional allowlist of explicit author logins.
	TrustedAuthors []string `toml:"trusted_authors"`
	// UntrustedReadyStatuses lists states eligible for untrusted authors under
	// environment = "untrusted". Default ["Todo"] when omitted; explicit [] blocks all.
	UntrustedReadyStatuses []string `toml:"untrusted_ready_statuses"`
	// Guards lists enabled tamper guards for untrusted authors under
	// environment = "untrusted". Supported: "locked", "approved_hash".
	// Default ["locked"] when omitted; explicit [] disables tamper guards.
	Guards []string `toml:"guards"`
}

// EffectiveTrustedAuthorAssociations returns the configured associations, or
// DefaultUntrustedAuthorAssociations when nil and Environment is "untrusted".
// An explicit non-nil empty slice stays empty.
func (t Trust) EffectiveTrustedAuthorAssociations() []string {
	if t.TrustedAuthorAssociations != nil {
		return t.TrustedAuthorAssociations
	}
	if t.Environment == TrustEnvUntrusted {
		return DefaultUntrustedAuthorAssociations()
	}
	return nil
}

// EffectiveUntrustedReadyStatuses returns the configured untrusted ready statuses,
// or DefaultUntrustedReadyStatuses when nil and Environment is "untrusted".
// An explicit non-nil empty slice stays empty.
func (t Trust) EffectiveUntrustedReadyStatuses() []string {
	if t.UntrustedReadyStatuses != nil {
		return t.UntrustedReadyStatuses
	}
	if t.Environment == TrustEnvUntrusted {
		return DefaultUntrustedReadyStatuses()
	}
	return nil
}

// EffectiveGuards returns the configured tamper guards, or DefaultUntrustedGuards
// when nil and Environment is "untrusted". An explicit non-nil empty slice stays empty.
func (t Trust) EffectiveGuards() []string {
	if t.Guards != nil {
		return t.Guards
	}
	if t.Environment == TrustEnvUntrusted {
		return DefaultUntrustedGuards()
	}
	return nil
}

type rawTrust struct {
	Environment               *string  `toml:"environment"`
	TrustedAuthorAssociations []string `toml:"trusted_author_associations"`
	TrustedAuthors            []string `toml:"trusted_authors"`
	UntrustedReadyStatuses    []string `toml:"untrusted_ready_statuses"`
	Guards                    []string `toml:"guards"`
}

func resolveSourceTrust(src *Source, raw *rawTrust) error {
	if raw == nil {
		return nil
	}
	if raw.Environment != nil {
		if err := ValidTrustEnvironment(*raw.Environment); err != nil {
			return fmt.Errorf("source %q trust.environment: %w", src.Name, err)
		}
		src.Trust.Environment = *raw.Environment
	}
	src.Trust.TrustedAuthorAssociations = copyStrings(raw.TrustedAuthorAssociations)
	src.Trust.TrustedAuthors = copyStrings(raw.TrustedAuthors)
	src.Trust.UntrustedReadyStatuses = copyStrings(raw.UntrustedReadyStatuses)
	src.Trust.Guards = copyStrings(raw.Guards)

	if src.Trust.Environment == TrustEnvUntrusted {
		if raw.TrustedAuthorAssociations == nil {
			src.Trust.TrustedAuthorAssociations = DefaultUntrustedAuthorAssociations()
		}
		if raw.UntrustedReadyStatuses == nil {
			src.Trust.UntrustedReadyStatuses = DefaultUntrustedReadyStatuses()
		}
		if raw.Guards == nil {
			src.Trust.Guards = DefaultUntrustedGuards()
		}
	}
	return nil
}

// ValidTrustEnvironment checks a [source.trust] environment value: trusted or untrusted.
func ValidTrustEnvironment(value string) error {
	switch value {
	case TrustEnvTrusted, TrustEnvUntrusted:
		return nil
	case "":
		return fmt.Errorf("expected %s or %s, found empty", TrustEnvTrusted, TrustEnvUntrusted)
	default:
		return fmt.Errorf("expected %s or %s, found %q", TrustEnvTrusted, TrustEnvUntrusted, value)
	}
}

// ValidTrustGuard checks a [source.trust] guards element: locked or approved_hash.
func ValidTrustGuard(value string) error {
	switch value {
	case TrustGuardLocked, TrustGuardApprovedHash:
		return nil
	case "":
		return fmt.Errorf("expected %s or %s, found empty", TrustGuardLocked, TrustGuardApprovedHash)
	default:
		return fmt.Errorf("expected %s or %s, found %q", TrustGuardLocked, TrustGuardApprovedHash, value)
	}
}

func validateSourceTrust(src *Source) error {
	if src.Trust.Environment != "" {
		if err := ValidTrustEnvironment(src.Trust.Environment); err != nil {
			return fmt.Errorf("source %q trust.environment: %w", src.Name, err)
		}
	}
	for _, assoc := range src.Trust.TrustedAuthorAssociations {
		if strings.TrimSpace(assoc) == "" {
			return fmt.Errorf("source %q trust.trusted_author_associations: expected a non-empty association, found %q", src.Name, assoc)
		}
	}
	for _, author := range src.Trust.TrustedAuthors {
		if strings.TrimSpace(author) == "" {
			return fmt.Errorf("source %q trust.trusted_authors: expected a non-empty login, found %q", src.Name, author)
		}
	}
	for _, status := range src.Trust.UntrustedReadyStatuses {
		if strings.TrimSpace(status) == "" {
			return fmt.Errorf("source %q trust.untrusted_ready_statuses: expected a non-empty status, found %q", src.Name, status)
		}
	}
	for _, guard := range src.Trust.Guards {
		if err := ValidTrustGuard(guard); err != nil {
			return fmt.Errorf("source %q trust.guards: %w", src.Name, err)
		}
	}
	return nil
}
