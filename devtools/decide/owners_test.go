package decide

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestOwnersConfigParsesAndDefaultsToOff(t *testing.T) {
	cfg, err := parseConfig([]byte(`{}`), t.TempDir())
	if err != nil {
		t.Fatalf("parseConfig empty: %v", err)
	}
	if cfg.Owners.EffectiveMode() != OwnersOff {
		t.Errorf("EffectiveMode = %q, want %q", cfg.Owners.EffectiveMode(), OwnersOff)
	}

	raw := `{
  "owners": {
    "mode": "warn",
    "allowed": ["lead/*", "ops/inbox"],
    "managers": { "team/*": "lead/planner", "*": "ops/inbox" }
  }
}`
	cfg, err = parseConfig([]byte(raw), t.TempDir())
	if err != nil {
		t.Fatalf("parseConfig owners: %v", err)
	}
	if cfg.Owners.EffectiveMode() != OwnersWarn {
		t.Errorf("mode = %q, want warn", cfg.Owners.EffectiveMode())
	}
	if len(cfg.Owners.Allowed) != 2 || cfg.Owners.Managers["team/*"] != "lead/planner" {
		t.Errorf("owners = %+v", cfg.Owners)
	}
}

func TestOwnersConfigRejectsInvalidValues(t *testing.T) {
	cases := []struct {
		name string
		json string
		want string
	}{
		{
			name: "bad mode",
			json: `{"owners":{"mode":"block","allowed":["*"]}}`,
			want: "owners.mode",
		},
		{
			name: "empty allowed with warn",
			json: `{"owners":{"mode":"warn","allowed":[]}}`,
			want: "owners.allowed",
		},
		{
			name: "empty allowed with enforce",
			json: `{"owners":{"mode":"enforce"}}`,
			want: "owners.allowed",
		},
		{
			name: "bad allowed glob",
			json: `{"owners":{"mode":"warn","allowed":["lead/["]}}`,
			want: "owners.allowed[0]",
		},
		{
			name: "empty allowed pattern",
			json: `{"owners":{"mode":"warn","allowed":[""]}}`,
			want: "owners.allowed[0]",
		},
		{
			name: "bad manager glob",
			json: `{"owners":{"mode":"off","managers":{"team/[":"lead/planner"}}}`,
			want: "owners.managers",
		},
		{
			name: "empty manager name",
			json: `{"owners":{"mode":"off","managers":{"team/*":""}}}`,
			want: "owners.managers",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := parseConfig([]byte(tc.json), t.TempDir())
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v, want substring %q", err, tc.want)
			}
		})
	}
}

func TestMatchOwner(t *testing.T) {
	allowed := []string{"lead/*", "ops/inbox"}
	for _, yes := range []string{"lead/planner", "lead/a", "ops/inbox"} {
		if !MatchOwner(allowed, yes) {
			t.Errorf("MatchOwner(%q) = false, want true", yes)
		}
	}
	for _, no := range []string{"lead/sub/deep", "ops/other", "team/one", ""} {
		if MatchOwner(allowed, no) {
			t.Errorf("MatchOwner(%q) = true, want false", no)
		}
	}
}

func TestLookupManagerOrder(t *testing.T) {
	managers := map[string]string{
		"*":          "ops/inbox",
		"team/*":     "lead/planner",
		"team/a*":    "lead/alpha",
		"team/alice": "lead/alice",
		"team/ops/*": "lead/ops",
	}
	cases := []struct {
		owner string
		want  string
		ok    bool
	}{
		{"team/alice", "lead/alice", true}, // exact (0 wildcards) beats team/a* and team/*
		{"team/ada", "lead/alpha", true},   // longer prefix team/a* beats team/*
		{"team/bob", "lead/planner", true}, // 1 slash beats *
		{"team/ops/x", "lead/ops", true},   // 2 slashes beats team/* and *
		{"solo", "ops/inbox", true},        // falls back to *
		{"other/one", "", false},           // * does not cross /
	}
	for _, tc := range cases {
		got, ok := LookupManager(managers, tc.owner)
		if got != tc.want || ok != tc.ok {
			t.Errorf("LookupManager(%q) = (%q, %v), want (%q, %v)", tc.owner, got, ok, tc.want, tc.ok)
		}
	}
}

func TestMergeDisplayHubOwnersOverridesLocal(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", dir)
	writeConfig(t, `{"hub":"me@hub","owners":{"mode":"off"}}`)
	local, err := LoadConfigFile()
	if err != nil {
		t.Fatal(err)
	}
	hub := DisplaySettings{
		Keys: map[string]json.RawMessage{
			"owners": json.RawMessage(`{"mode":"enforce","allowed":["lead/*"]}`),
		},
	}
	eff, err := MergeDisplay(local, hub)
	if err != nil {
		t.Fatalf("MergeDisplay: %v", err)
	}
	if eff.Config.Owners.EffectiveMode() != OwnersEnforce || eff.Sources["owners"] != SourceHub {
		t.Fatalf("owners = %+v, source = %q, want hub enforce", eff.Config.Owners, eff.Sources["owners"])
	}
}

func TestCheckOwner(t *testing.T) {
	managers := map[string]string{"team/*": "lead/planner"}
	cases := []struct {
		name        string
		cfg         OwnersConfig
		owner       string
		wantMsg     string
		wantEnforce bool
	}{
		{
			name:  "off",
			cfg:   OwnersConfig{Mode: OwnersOff, Allowed: []string{"lead/*"}, Managers: managers},
			owner: "team/bob",
		},
		{
			name:  "allowed",
			cfg:   OwnersConfig{Mode: OwnersEnforce, Allowed: []string{"lead/*"}, Managers: managers},
			owner: "lead/planner",
		},
		{
			name:    "warn with manager",
			cfg:     OwnersConfig{Mode: OwnersWarn, Allowed: []string{"lead/*"}, Managers: managers},
			owner:   "team/bob",
			wantMsg: "owner team/bob is not a direct report; ask lead/planner instead",
		},
		{
			name:    "warn without manager",
			cfg:     OwnersConfig{Mode: OwnersWarn, Allowed: []string{"lead/*"}, Managers: managers},
			owner:   "other/bob",
			wantMsg: "owner other/bob is not a direct report; ask your manager instead",
		},
		{
			name:        "enforce with manager",
			cfg:         OwnersConfig{Mode: OwnersEnforce, Allowed: []string{"lead/*"}, Managers: managers},
			owner:       "team/bob",
			wantMsg:     "owner team/bob is not a direct report; ask lead/planner instead",
			wantEnforce: true,
		},
		{
			name:        "enforce without manager",
			cfg:         OwnersConfig{Mode: OwnersEnforce, Allowed: []string{"lead/*"}, Managers: managers},
			owner:       "other/bob",
			wantMsg:     "owner other/bob is not a direct report; ask your manager instead",
			wantEnforce: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			msg, enforce := CheckOwner(tc.cfg, tc.owner)
			if msg != tc.wantMsg || enforce != tc.wantEnforce {
				t.Errorf("CheckOwner(%q) = (%q, %v), want (%q, %v)", tc.owner, msg, enforce, tc.wantMsg, tc.wantEnforce)
			}
		})
	}
}
