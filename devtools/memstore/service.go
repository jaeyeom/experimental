package memstore

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
)

// Sentinel errors returned by Service operations.
var (
	ErrNotFound  = errors.New("not found")
	ErrAmbiguous = errors.New("ambiguous match")
	ErrForbidden = errors.New("forbidden")
)

// AmbiguousError is returned when ResolveEntity matches multiple entities in
// the highest-priority matching tier.
type AmbiguousError struct {
	Query      string
	Candidates []EntityMatch
}

// Error implements the error interface.
func (e *AmbiguousError) Error() string {
	return fmt.Sprintf("%s: %q matched %d entities", ErrAmbiguous, e.Query, len(e.Candidates))
}

// Unwrap returns ErrAmbiguous so errors.Is(err, ErrAmbiguous) is true.
func (e *AmbiguousError) Unwrap() error {
	return ErrAmbiguous
}

// Actor identifies the caller and reason for an operation recorded in
// revisions and access_log.
type Actor struct {
	Caller string
	Reason string
}

// Service provides profile-enforced multi-vault CRUD and entity resolution.
type Service struct {
	policy Policy
	vaults map[string]*Vault
	order  []string
}

// OpenService resolves profileName against cfg using default-deny semantics and
// opens only the vaults permitted by that profile.
func OpenService(cfg Config, profileName string) (*Service, error) {
	policy, err := ResolveProfile(cfg, profileName)
	if err != nil {
		return nil, fmt.Errorf("%w: %w", ErrForbidden, err)
	}

	allowed := policy.AllowedVaults()
	vaults := make(map[string]*Vault, len(allowed))
	for _, name := range allowed {
		vc, ok := cfg.Vaults[name]
		if !ok {
			closeVaults(vaults)
			return nil, fmt.Errorf("vault %q not found in config", name)
		}
		v, err := OpenVault(vc)
		if err != nil {
			closeVaults(vaults)
			return nil, fmt.Errorf("open vault %q: %w", name, err)
		}
		vaults[name] = v
	}

	return &Service{
		policy: policy,
		vaults: vaults,
		order:  allowed,
	}, nil
}

// Close closes all vaults opened by the Service.
func (s *Service) Close() error {
	var errs []error
	for _, name := range s.order {
		if v := s.vaults[name]; v != nil {
			if err := v.Close(); err != nil {
				errs = append(errs, fmt.Errorf("close vault %q: %w", name, err))
			}
		}
	}
	return errors.Join(errs...)
}

// Policy returns the resolved access policy for this Service.
func (s *Service) Policy() Policy {
	return s.policy
}

func closeVaults(vaults map[string]*Vault) {
	for _, v := range vaults {
		_ = v.Close()
	}
}

func (s *Service) openVault(name string) (*Vault, error) {
	v, ok := s.vaults[name]
	if !ok {
		return nil, fmt.Errorf("%w: vault %q is not permitted by profile %q", ErrForbidden, name, s.policy.name)
	}
	return v, nil
}

func (s *Service) authorize(vaultName, compartment string, sens Sensitivity, cap Capability) (*Vault, error) {
	if err := s.policy.CanAccess(vaultName, compartment, sens, cap); err != nil {
		return nil, fmt.Errorf("%w: %w", ErrForbidden, err)
	}
	return s.openVault(vaultName)
}

func (s *Service) targetVaults(vaultName string) ([]string, error) {
	if vaultName == "" {
		return s.order, nil
	}
	if _, err := s.openVault(vaultName); err != nil {
		return nil, err
	}
	return []string{vaultName}, nil
}

func (s *Service) allowedSensitivities() []string {
	all := []Sensitivity{
		SensitivityPublic,
		SensitivityNormal,
		SensitivityPrivate,
		SensitivityRestricted,
	}
	out := make([]string, 0, len(all))
	for _, sens := range all {
		if r, ok := sens.rank(); ok && r <= s.policy.maxRank {
			out = append(out, string(sens))
		}
	}
	return out
}

func (s *Service) logAccess(ctx context.Context, v *Vault, actor Actor, command, entityID string) error {
	return v.LogAccess(ctx, actor.Caller, s.policy.name, command, entityID, actor.Reason)
}

func defaultCompartment(c string) string {
	if c == "" {
		return "general"
	}
	return c
}

func defaultSensitivity(s Sensitivity) Sensitivity {
	if s == "" {
		return SensitivityNormal
	}
	return s
}

func newID(prefix string) (string, error) {
	var b [12]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("generate id: %w", err)
	}
	return prefix + "_" + hex.EncodeToString(b[:]), nil
}

func (s *Service) allowedScopeJSON() (string, string, error) {
	comps := s.policy.AllowedCompartments()
	sens := s.allowedSensitivities()
	cb, err := json.Marshal(comps)
	if err != nil {
		return "", "", fmt.Errorf("marshal compartments: %w", err)
	}
	sb, err := json.Marshal(sens)
	if err != nil {
		return "", "", fmt.Errorf("marshal sensitivities: %w", err)
	}
	return string(cb), string(sb), nil
}
