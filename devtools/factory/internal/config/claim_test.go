package config_test

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadClaimDefaults(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(cat(jiraLines(), githubLines())))
	if err != nil {
		t.Fatal(err)
	}
	wantTop := config.Claim{
		RequireLeaf:       true,
		RequireUnblocked:  true,
		SourceContainment: true,
		Assign:            true,
		OwnershipRule:     config.OwnershipUnassignedOrMe,
		MarkInProgress:    true,
		InProgressRule:    config.InProgressBlock,
		CreateLabel:       false,
		Comment:           true,
	}
	if cfg.Claim != wantTop {
		t.Fatalf("top claim = %+v, want %+v", cfg.Claim, wantTop)
	}

	wantJira := wantTop
	wantJira.Status = "In Progress"
	if cfg.Sources[0].Claim != wantJira {
		t.Fatalf("jira claim = %+v, want %+v", cfg.Sources[0].Claim, wantJira)
	}
	if !cfg.Sources[0].ClaimAssign || cfg.Sources[0].ClaimStatus != "In Progress" || cfg.Sources[0].AssigneeOK != "me" {
		t.Fatalf("jira legacy fields = assign:%v status:%q assignee_ok:%q", cfg.Sources[0].ClaimAssign, cfg.Sources[0].ClaimStatus, cfg.Sources[0].AssigneeOK)
	}

	wantGH := wantTop
	wantGH.Status = config.DefaultGitHubClaimStatus
	if cfg.Sources[1].Claim != wantGH {
		t.Fatalf("github claim = %+v, want %+v", cfg.Sources[1].Claim, wantGH)
	}
	if !cfg.Sources[1].ClaimAssign || cfg.Sources[1].ClaimStatus != config.DefaultGitHubClaimStatus || cfg.Sources[1].ClaimLabel != "" || cfg.Sources[1].AssigneeOK != "me" {
		t.Fatalf("github legacy fields = assign:%v status:%q label:%q assignee_ok:%q", cfg.Sources[1].ClaimAssign, cfg.Sources[1].ClaimStatus, cfg.Sources[1].ClaimLabel, cfg.Sources[1].AssigneeOK)
	}
	wantReady := config.DefaultGitHubReadyStatuses()
	if strings.Join(cfg.Sources[1].ReadyStatuses, ",") != strings.Join(wantReady, ",") {
		t.Fatalf("github ready_statuses = %v, want %v", cfg.Sources[1].ReadyStatuses, wantReady)
	}
}

func TestLoadClaimInheritanceAndOverrides(t *testing.T) {
	t.Run("top level inherited by jira and github", func(t *testing.T) {
		lines := cat(
			[]string{
				"[claim]",
				"require_leaf = false",
				"require_unblocked = false",
				"source_containment = false",
				"assign = false",
				`ownership_rule = "unassigned"`,
				"mark_in_progress = true",
				`in_progress_rule = "allow"`,
				"create_label = true",
				"comment = false",
				`nickname = "mac-mini-1"`,
				`comment_template = "Claimed {id} ({source}) by {agent} on {nickname} at {timestamp}."`,
			},
			jiraLines(),
			githubLines(),
		)
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		wantTop := config.Claim{
			RequireLeaf:       false,
			RequireUnblocked:  false,
			SourceContainment: false,
			Assign:            false,
			OwnershipRule:     config.OwnershipUnassigned,
			MarkInProgress:    true,
			InProgressRule:    config.InProgressAllow,
			CreateLabel:       true,
			Comment:           false,
			Nickname:          "mac-mini-1",
			CommentTemplate:   "Claimed {id} ({source}) by {agent} on {nickname} at {timestamp}.",
		}
		if cfg.Claim != wantTop {
			t.Fatalf("top claim = %+v, want %+v", cfg.Claim, wantTop)
		}
		for i, src := range cfg.Sources {
			want := wantTop
			if src.Kind == "jira" {
				want.Status = "In Progress"
			} else {
				want.Status = config.DefaultGitHubClaimStatus
			}
			if src.Claim != want {
				t.Fatalf("source[%d] claim = %+v, want %+v", i, src.Claim, want)
			}
			if src.ClaimAssign != false || src.AssigneeOK != "unassigned" {
				t.Fatalf("source[%d] synced legacy = assign:%v assignee_ok:%q", i, src.ClaimAssign, src.AssigneeOK)
			}
		}
	})

	t.Run("source.claim overrides top level including status transition label", func(t *testing.T) {
		jira := without(jiraLines(), "claim_status")
		lines := cat(
			[]string{
				"[claim]",
				"assign = false",
				`ownership_rule = "unassigned"`,
				`in_progress_rule = "allow"`,
				"comment = false",
				`nickname = "global-box"`,
			},
			jira,
			[]string{
				"[source.claim]",
				"require_leaf = false",
				"assign = true",
				`ownership_rule = "any"`,
				`in_progress_rule = "block"`,
				"comment = true",
				`nickname = "jira-box"`,
				`comment_template = "{agent} started {id}"`,
				`status = "Doing"`,
				`transition = "start work"`,
				`label = "wip"`,
			},
			githubLines(),
			[]string{
				"[source.claim]",
				"create_label = true",
				`label = "claimed"`,
				`ownership_rule = "unassigned_or_me"`,
			},
		)
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		j := cfg.Sources[0]
		if j.Claim.RequireLeaf || !j.Claim.Assign || j.Claim.OwnershipRule != config.OwnershipAny ||
			j.Claim.InProgressRule != config.InProgressBlock || !j.Claim.Comment ||
			j.Claim.Nickname != "jira-box" || j.Claim.CommentTemplate != "{agent} started {id}" ||
			j.Claim.Status != "Doing" || j.Claim.Transition != "start work" || j.Claim.Label != "wip" {
			t.Fatalf("jira claim = %+v", j.Claim)
		}
		if !j.ClaimAssign || j.ClaimStatus != "Doing" || j.ClaimTransition != "start work" || j.ClaimLabel != "wip" || j.AssigneeOK != "any" {
			t.Fatalf("jira legacy fields out of sync: %+v", j)
		}

		gh := cfg.Sources[1]
		if gh.Claim.Assign || !gh.Claim.CreateLabel || gh.Claim.Label != "claimed" ||
			gh.Claim.OwnershipRule != config.OwnershipUnassignedOrMe || gh.Claim.Nickname != "global-box" {
			t.Fatalf("github claim = %+v", gh.Claim)
		}
		if gh.ClaimAssign || gh.ClaimLabel != "claimed" || gh.AssigneeOK != "me" {
			t.Fatalf("github legacy fields out of sync: %+v", gh)
		}
	})

	t.Run("legacy source keys override top level [claim] and stay in sync", func(t *testing.T) {
		lines := cat(
			[]string{
				"[claim]",
				"assign = false",
				`ownership_rule = "unassigned"`,
			},
			append(jiraLines(),
				"claim_assign = true",
				`claim_transition = "begin"`,
				`assignee_ok = "any"`,
			),
			append(githubLines(),
				"claim_assign = true",
				`claim_label = "active"`,
				`assignee_ok = "me"`,
			),
		)
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		j := cfg.Sources[0]
		if !j.Claim.Assign || !j.ClaimAssign || j.Claim.Transition != "begin" || j.ClaimTransition != "begin" ||
			j.Claim.OwnershipRule != config.OwnershipAny || j.AssigneeOK != "any" {
			t.Fatalf("jira = claim:%+v assign:%v trans:%q assignee_ok:%q", j.Claim, j.ClaimAssign, j.ClaimTransition, j.AssigneeOK)
		}
		gh := cfg.Sources[1]
		if !gh.Claim.Assign || !gh.ClaimAssign || gh.Claim.Label != "active" || gh.ClaimLabel != "active" ||
			gh.Claim.OwnershipRule != config.OwnershipUnassignedOrMe || gh.AssigneeOK != "me" {
			t.Fatalf("github = claim:%+v assign:%v label:%q assignee_ok:%q", gh.Claim, gh.ClaimAssign, gh.ClaimLabel, gh.AssigneeOK)
		}
	})
}

func TestLoadClaimMarkInProgressFalseAllowsEmptyStatusAndLabel(t *testing.T) {
	t.Run("top level mark_in_progress false", func(t *testing.T) {
		lines := cat(
			[]string{"[claim]", "mark_in_progress = false"},
			without(jiraLines(), "claim_status"),
			append(githubLines(), `claim_label = ""`),
		)
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		if cfg.Sources[0].Claim.MarkInProgress || cfg.Sources[0].ClaimStatus != "" || cfg.Sources[0].Claim.Status != "" {
			t.Fatalf("jira = %+v", cfg.Sources[0].Claim)
		}
		if cfg.Sources[1].Claim.MarkInProgress || cfg.Sources[1].ClaimLabel != "" || cfg.Sources[1].Claim.Label != "" {
			t.Fatalf("github = %+v", cfg.Sources[1].Claim)
		}
	})

	t.Run("per source mark_in_progress false", func(t *testing.T) {
		lines := cat(
			without(jiraLines(), "claim_status"),
			[]string{"[source.claim]", "mark_in_progress = false"},
			githubLines(),
			[]string{"[source.claim]", "mark_in_progress = false", `label = ""`},
		)
		cfg, err := newFake(t).load(tomlDoc(lines))
		if err != nil {
			t.Fatal(err)
		}
		if cfg.Sources[0].ClaimStatus != "" || cfg.Sources[1].ClaimLabel != "" {
			t.Fatalf("expected empty status/label, got jira=%q gh=%q", cfg.Sources[0].ClaimStatus, cfg.Sources[1].ClaimLabel)
		}
	})

	t.Run("mark_in_progress true rejects empty github claim_status and claim_label", func(t *testing.T) {
		for _, tc := range []struct {
			name  string
			lines []string
		}{
			{name: "legacy empty label", lines: append(githubLines(), `claim_label = ""`)},
			{name: "source.claim empty label", lines: cat(githubLines(), []string{"[source.claim]", `label = ""`})},
			{name: "legacy empty status", lines: append(githubLines(), `claim_status = ""`)},
			{name: "source.claim empty status", lines: cat(githubLines(), []string{"[source.claim]", `status = ""`})},
		} {
			t.Run(tc.name, func(t *testing.T) {
				_, err := newFake(t).load(tomlDoc(tc.lines))
				if err == nil || !strings.Contains(err.Error(), "expected claim_status or claim_label") {
					t.Fatalf("err = %v, want claim_status or claim_label error", err)
				}
			})
		}
	})

	t.Run("explicit github claim_label disables default claim_status", func(t *testing.T) {
		cfg, err := newFake(t).load(tomlDoc(append(githubLines(), `claim_label = "in-progress"`)))
		if err != nil {
			t.Fatal(err)
		}
		gh := cfg.Sources[0]
		if gh.ClaimStatus != "" || gh.Claim.Status != "" || gh.ClaimLabel != "in-progress" || gh.Claim.Label != "in-progress" {
			t.Fatalf("github = status:%q label:%q claim:%+v", gh.ClaimStatus, gh.ClaimLabel, gh.Claim)
		}
	})

	t.Run("explicit github ready_statuses overrides default including empty list", func(t *testing.T) {
		cfg, err := newFake(t).load(tomlDoc(append(githubLines(), `ready_statuses = ["Todo"]`)))
		if err != nil {
			t.Fatal(err)
		}
		if strings.Join(cfg.Sources[0].ReadyStatuses, ",") != "Todo" {
			t.Fatalf("ready_statuses = %v, want [Todo]", cfg.Sources[0].ReadyStatuses)
		}
		cfgEmpty, err := newFake(t).load(tomlDoc(append(githubLines(), `ready_statuses = []`)))
		if err != nil {
			t.Fatal(err)
		}
		if len(cfgEmpty.Sources[0].ReadyStatuses) != 0 {
			t.Fatalf("ready_statuses = %v, want empty", cfgEmpty.Sources[0].ReadyStatuses)
		}
	})
}

func TestLoadClaimConflicts(t *testing.T) {
	tests := []struct {
		name    string
		lines   []string
		wantErr string
	}{
		{
			name:    "claim_assign and claim.assign",
			lines:   cat(append(githubLines(), "claim_assign = false"), []string{"[source.claim]", "assign = true"}),
			wantErr: `source "experimental": expected one of claim.assign or claim_assign, found both`,
		},
		{
			name:    "claim_status and claim.status",
			lines:   cat(jiraLines(), []string{"[source.claim]", `status = "Doing"`}),
			wantErr: `source "acme": expected one of claim.status or claim_status, found both`,
		},
		{
			name:    "claim_transition and claim.transition",
			lines:   cat(append(jiraLines(), `claim_transition = "start"`), []string{"[source.claim]", `transition = "begin"`}),
			wantErr: `source "acme": expected one of claim.transition or claim_transition, found both`,
		},
		{
			name:    "claim_label and claim.label",
			lines:   cat(append(githubLines(), `claim_label = "wip"`), []string{"[source.claim]", `label = "active"`}),
			wantErr: `source "experimental": expected one of claim.label or claim_label, found both`,
		},
		{
			name:    "assignee_ok and claim.ownership_rule",
			lines:   cat(append(githubLines(), `assignee_ok = "me"`), []string{"[source.claim]", `ownership_rule = "any"`}),
			wantErr: `source "experimental": expected one of claim.ownership_rule or assignee_ok, found both`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tomlDoc(tc.lines))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestLoadClaimValidation(t *testing.T) {
	t.Run("valid nicknames and templates", func(t *testing.T) {
		validNicknames := []string{"", "a", "0", "mac-1", "a" + strings.Repeat("b", 31)}
		for _, nick := range validNicknames {
			lines := cat([]string{"[claim]", `nickname = "` + nick + `"`, `comment_template = "Claimed {id} in {source} by {agent} on {nickname} at {timestamp}"`}, githubLines())
			cfg, err := newFake(t).load(tomlDoc(lines))
			if err != nil {
				t.Fatalf("nickname %q: %v", nick, err)
			}
			if cfg.Claim.Nickname != nick {
				t.Fatalf("nickname = %q, want %q", cfg.Claim.Nickname, nick)
			}
		}
	})

	tests := []struct {
		name    string
		lines   []string
		wantErr string
	}{
		{
			name:    "invalid top ownership_rule",
			lines:   cat([]string{"[claim]", `ownership_rule = "me"`}, githubLines()),
			wantErr: `claim.ownership_rule: expected unassigned_or_me, unassigned, or any, found "me"`,
		},
		{
			name:    "empty top ownership_rule",
			lines:   cat([]string{"[claim]", `ownership_rule = ""`}, githubLines()),
			wantErr: "claim.ownership_rule: expected unassigned_or_me, unassigned, or any, found empty",
		},
		{
			name:    "invalid source ownership_rule",
			lines:   cat(githubLines(), []string{"[source.claim]", `ownership_rule = "nobody"`}),
			wantErr: `source "experimental" claim.ownership_rule: expected unassigned_or_me, unassigned, or any, found "nobody"`,
		},
		{
			name:    "invalid top in_progress_rule",
			lines:   cat([]string{"[claim]", `in_progress_rule = "warn"`}, githubLines()),
			wantErr: `claim.in_progress_rule: expected block or allow, found "warn"`,
		},
		{
			name:    "empty top in_progress_rule",
			lines:   cat([]string{"[claim]", `in_progress_rule = ""`}, githubLines()),
			wantErr: "claim.in_progress_rule: expected block or allow, found empty",
		},
		{
			name:    "invalid source in_progress_rule",
			lines:   cat(githubLines(), []string{"[source.claim]", `in_progress_rule = "skip"`}),
			wantErr: `source "experimental" claim.in_progress_rule: expected block or allow, found "skip"`,
		},
		{
			name:    "nickname uppercase",
			lines:   cat([]string{"[claim]", `nickname = "Mac1"`}, githubLines()),
			wantErr: `claim.nickname: expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$, found "Mac1"`,
		},
		{
			name:    "nickname leading dash",
			lines:   cat([]string{"[claim]", `nickname = "-box"`}, githubLines()),
			wantErr: `claim.nickname: expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$, found "-box"`,
		},
		{
			name:    "nickname underscore",
			lines:   cat([]string{"[claim]", `nickname = "my_box"`}, githubLines()),
			wantErr: `claim.nickname: expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$, found "my_box"`,
		},
		{
			name:    "nickname too long",
			lines:   cat([]string{"[claim]", `nickname = "a` + strings.Repeat("b", 32) + `"`}, githubLines()),
			wantErr: "claim.nickname: expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$",
		},
		{
			name:    "source nickname invalid",
			lines:   cat(githubLines(), []string{"[source.claim]", `nickname = "bad box"`}),
			wantErr: `source "experimental" claim.nickname: expected empty or a nickname matching ^[a-z0-9][a-z0-9-]{0,31}$, found "bad box"`,
		},
		{
			name:    "comment_template unknown placeholder",
			lines:   cat([]string{"[claim]", `comment_template = "Claimed by {user} at {timestamp}"`}, githubLines()),
			wantErr: "claim.comment_template: expected one of {agent}, {nickname}, {timestamp}, {id}, {source}, found {user}",
		},
		{
			name:    "comment_template unknown underscore placeholder",
			lines:   cat([]string{"[claim]", `comment_template = "Claimed by {agent_name}"`}, githubLines()),
			wantErr: "claim.comment_template: expected one of {agent}, {nickname}, {timestamp}, {id}, {source}, found {agent_name}",
		},
		{
			name:    "comment_template empty placeholder",
			lines:   cat([]string{"[claim]", `comment_template = "Claimed by {}"`}, githubLines()),
			wantErr: "claim.comment_template: expected one of {agent}, {nickname}, {timestamp}, {id}, {source}, found {}",
		},
		{
			name:    "comment_template unclosed brace",
			lines:   cat([]string{"[claim]", `comment_template = "Claimed by {agent"`}, githubLines()),
			wantErr: `claim.comment_template: literal braces aren't supported, found "{agent"`,
		},
		{
			name:    "comment_template stray close brace",
			lines:   cat([]string{"[claim]", `comment_template = "Claimed by agent}"`}, githubLines()),
			wantErr: `claim.comment_template: literal braces aren't supported, found "}"`,
		},
		{
			name:    "source comment_template unknown placeholder",
			lines:   cat(githubLines(), []string{"[source.claim]", `comment_template = "{url}"`}),
			wantErr: `source "experimental" claim.comment_template: expected one of {agent}, {nickname}, {timestamp}, {id}, {source}, found {url}`,
		},
		{
			name:    "top claim rejects status",
			lines:   cat([]string{"[claim]", `status = "In Progress"`}, githubLines()),
			wantErr: "unknown key claim.status",
		},
		{
			name:    "top claim rejects transition",
			lines:   cat([]string{"[claim]", `transition = "start"`}, githubLines()),
			wantErr: "unknown key claim.transition",
		},
		{
			name:    "top claim rejects label",
			lines:   cat([]string{"[claim]", `label = "in-progress"`}, githubLines()),
			wantErr: "unknown key claim.label",
		},
		{
			name:    "unknown source.claim key",
			lines:   cat(githubLines(), []string{"[source.claim]", "bogus = true"}),
			wantErr: "unknown key source.claim.bogus",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tomlDoc(tc.lines))
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}
