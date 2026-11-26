package config_test

import (
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadSourceTrustDefaults(t *testing.T) {
	t.Run("unset leaves trust zero", func(t *testing.T) {
		cfg, err := newFake(t).load(tomlDoc(githubLines()))
		if err != nil {
			t.Fatal(err)
		}
		got := cfg.Sources[0].Trust
		if !reflect.DeepEqual(got, config.Trust{}) {
			t.Fatalf("trust = %#v, want zero", got)
		}
		if got.EffectiveGuards() != nil || got.EffectiveUntrustedReadyStatuses() != nil || got.EffectiveTrustedAuthorAssociations() != nil {
			t.Fatalf("effective defaults should be nil when unset: %#v", got)
		}
	})

	t.Run("trusted keeps guards and gating off", func(t *testing.T) {
		lines := cat(githubLines(), []string{
			"[source.trust]",
			`environment = "trusted"`,
		})
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		got := cfg.Sources[0].Trust
		want := config.Trust{Environment: config.TrustEnvTrusted}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("trust = %#v, want %#v", got, want)
		}
		if got.EffectiveGuards() != nil || got.EffectiveUntrustedReadyStatuses() != nil || got.EffectiveTrustedAuthorAssociations() != nil {
			t.Fatalf("effective defaults should be nil when trusted: %#v", got)
		}
	})

	t.Run("untrusted applies defaults", func(t *testing.T) {
		lines := cat(githubLines(), []string{
			"[source.trust]",
			`environment = "untrusted"`,
		})
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		got := cfg.Sources[0].Trust
		want := config.Trust{
			Environment:               config.TrustEnvUntrusted,
			TrustedAuthorAssociations: []string{"OWNER"},
			UntrustedReadyStatuses:    []string{"Todo"},
			Guards:                    []string{"locked"},
		}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("trust = %#v, want %#v", got, want)
		}
	})
}

func TestLoadSourceTrustExplicitListsAndOverrides(t *testing.T) {
	t.Run("empty slices disable guards and block all untrusted statuses", func(t *testing.T) {
		lines := cat(githubLines(), []string{
			"[source.trust]",
			`environment = "untrusted"`,
			`trusted_author_associations = []`,
			`untrusted_ready_statuses = []`,
			`guards = []`,
		})
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		got := cfg.Sources[0].Trust
		if got.TrustedAuthorAssociations == nil || len(got.TrustedAuthorAssociations) != 0 {
			t.Fatalf("trusted_author_associations = %#v, want non-nil empty", got.TrustedAuthorAssociations)
		}
		if got.UntrustedReadyStatuses == nil || len(got.UntrustedReadyStatuses) != 0 {
			t.Fatalf("untrusted_ready_statuses = %#v, want non-nil empty", got.UntrustedReadyStatuses)
		}
		if got.Guards == nil || len(got.Guards) != 0 {
			t.Fatalf("guards = %#v, want non-nil empty", got.Guards)
		}
		if got.EffectiveGuards() == nil || len(got.EffectiveGuards()) != 0 {
			t.Fatalf("effective guards = %#v, want non-nil empty", got.EffectiveGuards())
		}
		if got.EffectiveUntrustedReadyStatuses() == nil || len(got.EffectiveUntrustedReadyStatuses()) != 0 {
			t.Fatalf("effective untrusted_ready_statuses = %#v, want non-nil empty", got.EffectiveUntrustedReadyStatuses())
		}
	})

	t.Run("custom lists and approved_hash accepted", func(t *testing.T) {
		lines := cat(githubLines(), []string{
			"[source.trust]",
			`environment = "untrusted"`,
			`trusted_author_associations = ["OWNER", "MEMBER"]`,
			`trusted_authors = ["alice", "bob"]`,
			`untrusted_ready_statuses = ["Todo", "Ready"]`,
			`guards = ["locked", "approved_hash"]`,
		})
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		want := config.Trust{
			Environment:               config.TrustEnvUntrusted,
			TrustedAuthorAssociations: []string{"OWNER", "MEMBER"},
			TrustedAuthors:            []string{"alice", "bob"},
			UntrustedReadyStatuses:    []string{"Todo", "Ready"},
			Guards:                    []string{"locked", "approved_hash"},
		}
		if !reflect.DeepEqual(cfg.Sources[0].Trust, want) {
			t.Fatalf("trust = %#v, want %#v", cfg.Sources[0].Trust, want)
		}
	})

	t.Run("trust_rule placeholder allowed in start_prompt", func(t *testing.T) {
		lines := replaceLine(githubLines(), "start_prompt", `start_prompt = "Work on {id}\n{{trust_rule}}\nGo."`)
		lines = cat(lines, []string{
			"[source.trust]",
			`environment = "untrusted"`,
		})
		if _, err := newFake(t).load(tomlDoc(lines)); err != nil {
			t.Fatal(err)
		}
	})
}

func TestLoadSourceTrustValidationErrors(t *testing.T) {
	tests := []struct {
		name    string
		trust   []string
		wantErr string
	}{
		{
			name:    "empty environment",
			trust:   []string{`environment = ""`},
			wantErr: `source "experimental" trust.environment: expected trusted or untrusted, found empty`,
		},
		{
			name:    "invalid environment",
			trust:   []string{`environment = "public"`},
			wantErr: `source "experimental" trust.environment: expected trusted or untrusted, found "public"`,
		},
		{
			name:    "unknown guard",
			trust:   []string{`environment = "untrusted"`, `guards = ["magic"]`},
			wantErr: `source "experimental" trust.guards: expected locked or approved_hash, found "magic"`,
		},
		{
			name:    "empty guard",
			trust:   []string{`environment = "untrusted"`, `guards = [""]`},
			wantErr: `source "experimental" trust.guards: expected locked or approved_hash, found empty`,
		},
		{
			name:    "empty trusted_author_associations entry",
			trust:   []string{`environment = "untrusted"`, `trusted_author_associations = [""]`},
			wantErr: `source "experimental" trust.trusted_author_associations: expected a non-empty association`,
		},
		{
			name:    "empty trusted_authors entry",
			trust:   []string{`environment = "untrusted"`, `trusted_authors = [" "]`},
			wantErr: `source "experimental" trust.trusted_authors: expected a non-empty login`,
		},
		{
			name:    "empty untrusted_ready_statuses entry",
			trust:   []string{`environment = "untrusted"`, `untrusted_ready_statuses = [""]`},
			wantErr: `source "experimental" trust.untrusted_ready_statuses: expected a non-empty status`,
		},
		{
			name:    "unknown trust key",
			trust:   []string{`mode = "strict"`},
			wantErr: `unknown key source.trust.mode`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			lines := cat(githubLines(), []string{"[source.trust]"}, tc.trust)
			_, err := newFake(t).load(tomlDoc(lines))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}
