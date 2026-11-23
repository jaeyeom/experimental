package memstore

import (
	"slices"
	"testing"
)

func TestResolveProfileDefaultDeny(t *testing.T) {
	cfg := Config{
		Vaults: map[string]VaultConfig{
			"shared": {Name: "shared", Path: "/tmp/shared.db", Residency: ResidencyLocalOnly},
		},
		Profiles: map[string]ProfileConfig{
			"work": {
				Name:           "work",
				Vaults:         []string{"shared"},
				Compartments:   []string{"general"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityRead},
			},
		},
	}

	tests := []struct {
		name    string
		profile string
		wantErr bool
	}{
		{name: "empty profile denied", profile: "", wantErr: true},
		{name: "unknown profile denied", profile: "nonexistent", wantErr: true},
		{name: "known profile allowed", profile: "work", wantErr: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ResolveProfile(cfg, tc.profile)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ResolveProfile(%q) error = %v, wantErr %v", tc.profile, err, tc.wantErr)
			}
		})
	}
}

func TestPolicyCanAccess(t *testing.T) {
	cfg := Config{
		Vaults: map[string]VaultConfig{
			"shared":   {Name: "shared", Path: "/tmp/shared.db", Residency: ResidencyLocalOnly},
			"work":     {Name: "work", Path: "/tmp/work.db", Residency: ResidencyLocalOnly},
			"personal": {Name: "personal", Path: "/tmp/personal.db", Residency: ResidencyLocalOnly},
		},
		Profiles: map[string]ProfileConfig{
			"work-agent": {
				Name:           "work-agent",
				Vaults:         []string{"work", "shared"},
				Compartments:   []string{"ops", "general", "engineering"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityRead, CapabilityWriteEpisodic, CapabilityProposeFact},
			},
		},
	}

	pol, err := ResolveProfile(cfg, "work-agent")
	if err != nil {
		t.Fatalf("ResolveProfile: %v", err)
	}

	tests := []struct {
		name        string
		vault       string
		compartment string
		sens        Sensitivity
		cap         Capability
		wantErr     bool
	}{
		{
			name:        "allowed exact sensitivity",
			vault:       "work",
			compartment: "engineering",
			sens:        SensitivityNormal,
			cap:         CapabilityRead,
		},
		{
			name:        "allowed lower sensitivity",
			vault:       "shared",
			compartment: "general",
			sens:        SensitivityPublic,
			cap:         CapabilityWriteEpisodic,
		},
		{
			name:        "denied vault",
			vault:       "personal",
			compartment: "general",
			sens:        SensitivityPublic,
			cap:         CapabilityRead,
			wantErr:     true,
		},
		{
			name:        "denied compartment",
			vault:       "work",
			compartment: "finance",
			sens:        SensitivityNormal,
			cap:         CapabilityRead,
			wantErr:     true,
		},
		{
			name:        "denied sensitivity above ceiling",
			vault:       "work",
			compartment: "general",
			sens:        SensitivityPrivate,
			cap:         CapabilityRead,
			wantErr:     true,
		},
		{
			name:        "denied restricted sensitivity",
			vault:       "work",
			compartment: "general",
			sens:        SensitivityRestricted,
			cap:         CapabilityRead,
			wantErr:     true,
		},
		{
			name:        "denied unknown sensitivity",
			vault:       "work",
			compartment: "general",
			sens:        Sensitivity("bogus"),
			cap:         CapabilityRead,
			wantErr:     true,
		},
		{
			name:        "denied capability",
			vault:       "work",
			compartment: "general",
			sens:        SensitivityNormal,
			cap:         CapabilityWrite,
			wantErr:     true,
		},
		{
			name:        "denied unknown capability",
			vault:       "work",
			compartment: "general",
			sens:        SensitivityNormal,
			cap:         Capability("bogus"),
			wantErr:     true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			err := pol.CanAccess(tc.vault, tc.compartment, tc.sens, tc.cap)
			if (err != nil) != tc.wantErr {
				t.Errorf("CanAccess(%q, %q, %q, %q) error = %v, wantErr %v",
					tc.vault, tc.compartment, tc.sens, tc.cap, err, tc.wantErr)
			}
		})
	}
}

func TestPolicyAccessors(t *testing.T) {
	cfg := Config{
		Vaults: map[string]VaultConfig{
			"work":   {Name: "work", Path: "/tmp/work.db", Residency: ResidencyLocalOnly},
			"shared": {Name: "shared", Path: "/tmp/shared.db", Residency: ResidencyLocalOnly},
		},
		Profiles: map[string]ProfileConfig{
			"p": {
				Name:           "p",
				Vaults:         []string{"work", "shared"},
				Compartments:   []string{"ops", "engineering", "general"},
				MaxSensitivity: SensitivityPrivate,
				Capabilities:   []Capability{CapabilityRead, CapabilityExport},
			},
		},
	}

	pol, err := ResolveProfile(cfg, "p")
	if err != nil {
		t.Fatalf("ResolveProfile: %v", err)
	}

	if got, want := pol.AllowedVaults(), []string{"shared", "work"}; !slices.Equal(got, want) {
		t.Errorf("AllowedVaults() = %v, want %v", got, want)
	}
	if got, want := pol.AllowedCompartments(), []string{"engineering", "general", "ops"}; !slices.Equal(got, want) {
		t.Errorf("AllowedCompartments() = %v, want %v", got, want)
	}
	if got := pol.MaxSensitivity(); got != SensitivityPrivate {
		t.Errorf("MaxSensitivity() = %q, want %q", got, SensitivityPrivate)
	}
	if !pol.HasCapability(CapabilityRead) || !pol.HasCapability(CapabilityExport) {
		t.Error("HasCapability returned false for granted capability")
	}
	if pol.HasCapability(CapabilityAdmin) {
		t.Error("HasCapability returned true for ungranted capability")
	}
}

func TestSensitivityOrdering(t *testing.T) {
	ordered := []Sensitivity{
		SensitivityPublic,
		SensitivityNormal,
		SensitivityPrivate,
		SensitivityRestricted,
	}
	for i := 0; i < len(ordered); i++ {
		ri, ok := ordered[i].rank()
		if !ok {
			t.Fatalf("rank(%q) not ok", ordered[i])
		}
		for j := i + 1; j < len(ordered); j++ {
			rj, ok := ordered[j].rank()
			if !ok {
				t.Fatalf("rank(%q) not ok", ordered[j])
			}
			if ri >= rj {
				t.Errorf("rank(%q)=%d >= rank(%q)=%d", ordered[i], ri, ordered[j], rj)
			}
		}
	}
}
