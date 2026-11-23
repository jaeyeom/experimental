package memstore

import (
	"fmt"
	"slices"
)

// Sensitivity is an ordered classification level for memory items.
type Sensitivity string

// Sensitivity levels ordered from least to most sensitive.
const (
	SensitivityPublic     Sensitivity = "public"
	SensitivityNormal     Sensitivity = "normal"
	SensitivityPrivate    Sensitivity = "private"
	SensitivityRestricted Sensitivity = "restricted"
)

func (s Sensitivity) rank() (int, bool) {
	switch s {
	case SensitivityPublic:
		return 0, true
	case SensitivityNormal:
		return 1, true
	case SensitivityPrivate:
		return 2, true
	case SensitivityRestricted:
		return 3, true
	default:
		return 0, false
	}
}

// Capability is an action permitted to a caller profile.
type Capability string

// Supported profile capabilities.
const (
	CapabilityRead          Capability = "read"
	CapabilityWrite         Capability = "write"
	CapabilityWriteEpisodic Capability = "write-episodic"
	CapabilityProposeFact   Capability = "propose-fact"
	CapabilityExport        Capability = "export"
	CapabilityAdmin         Capability = "admin"
)

func (c Capability) valid() bool {
	switch c {
	case CapabilityRead,
		CapabilityWrite,
		CapabilityWriteEpisodic,
		CapabilityProposeFact,
		CapabilityExport,
		CapabilityAdmin:
		return true
	default:
		return false
	}
}

// Policy is a resolved, default-deny access boundary for a caller profile.
type Policy struct {
	name           string
	vaults         map[string]struct{}
	compartments   map[string]struct{}
	maxSensitivity Sensitivity
	maxRank        int
	capabilities   map[Capability]struct{}
}

// ResolveProfile resolves name against cfg.Profiles using default-deny
// semantics: an empty or unknown profile name always returns an error.
func ResolveProfile(cfg Config, name string) (Policy, error) {
	if name == "" {
		return Policy{}, fmt.Errorf("profile name is required (default-deny)")
	}
	prof, ok := cfg.Profiles[name]
	if !ok {
		return Policy{}, fmt.Errorf("unknown profile %q", name)
	}
	maxRank, ok := prof.MaxSensitivity.rank()
	if !ok {
		return Policy{}, fmt.Errorf("profile %q has invalid max_sensitivity %q", name, prof.MaxSensitivity)
	}

	p := Policy{
		name:           name,
		vaults:         toSet(prof.Vaults),
		compartments:   toSet(prof.Compartments),
		maxSensitivity: prof.MaxSensitivity,
		maxRank:        maxRank,
		capabilities:   make(map[Capability]struct{}, len(prof.Capabilities)),
	}
	for _, cap := range prof.Capabilities {
		if !cap.valid() {
			return Policy{}, fmt.Errorf("profile %q has invalid capability %q", name, cap)
		}
		p.capabilities[cap] = struct{}{}
	}
	return p, nil
}

// CanAccess checks whether the policy permits cap on (vault, compartment, sens).
func (p Policy) CanAccess(vault, compartment string, sens Sensitivity, cap Capability) error {
	if _, ok := p.vaults[vault]; !ok {
		return fmt.Errorf("profile %q does not allow vault %q", p.name, vault)
	}
	if _, ok := p.compartments[compartment]; !ok {
		return fmt.Errorf("profile %q does not allow compartment %q", p.name, compartment)
	}
	rank, ok := sens.rank()
	if !ok {
		return fmt.Errorf("unknown sensitivity %q", sens)
	}
	if rank > p.maxRank {
		return fmt.Errorf("profile %q caps sensitivity at %q (requested %q)", p.name, p.maxSensitivity, sens)
	}
	if !cap.valid() {
		return fmt.Errorf("unknown capability %q", cap)
	}
	if !p.HasCapability(cap) {
		return fmt.Errorf("profile %q lacks capability %q", p.name, cap)
	}
	return nil
}

// HasCapability reports whether the policy grants cap.
func (p Policy) HasCapability(cap Capability) bool {
	_, ok := p.capabilities[cap]
	return ok
}

// AllowedVaults returns the sorted list of vaults permitted by the policy.
func (p Policy) AllowedVaults() []string {
	return sortedKeys(p.vaults)
}

// AllowedCompartments returns the sorted list of compartments permitted by the policy.
func (p Policy) AllowedCompartments() []string {
	return sortedKeys(p.compartments)
}

// MaxSensitivity returns the maximum sensitivity permitted by the policy.
func (p Policy) MaxSensitivity() Sensitivity {
	return p.maxSensitivity
}

func toSet(items []string) map[string]struct{} {
	s := make(map[string]struct{}, len(items))
	for _, item := range items {
		s[item] = struct{}{}
	}
	return s
}

func sortedKeys(m map[string]struct{}) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	return keys
}
