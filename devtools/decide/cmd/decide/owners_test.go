package main

import (
	"encoding/json"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestAskAndSendOwnerAllowListAllowedAndOff(t *testing.T) {
	for _, tc := range []struct {
		name string
		cfg  string
	}{
		{
			name: "off",
			cfg:  `{"owners":{"mode":"off","allowed":["lead/*"]},"targets":[{"name":"team/bob","pane":"pane-b"}]}`,
		},
		{
			name: "allowed",
			cfg:  `{"owners":{"mode":"enforce","allowed":["lead/*","team/bob"]},"targets":[{"name":"team/bob","pane":"pane-b"}]}`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			writeDecideConfig(t, tc.cfg)
			stdout, stderr, code := runCLI("ask", "--owner", "team/bob", "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Proceed?")
			if code != 0 {
				t.Fatalf("ask = %d, stderr=%q", code, stderr)
			}
			if stderr != "" {
				t.Errorf("ask stderr = %q, want empty", stderr)
			}
			id := strings.TrimSpace(stdout)
			got := showJSON(t, id)
			if len(got.Tags) != 0 {
				t.Errorf("ask tags = %v, want empty", got.Tags)
			}
		})
	}
}

func TestAskOwnerAllowListWarn(t *testing.T) {
	cases := []struct {
		name    string
		owner   string
		wantErr string
	}{
		{
			name:    "mapped manager",
			owner:   "team/bob",
			wantErr: "owner team/bob is not a direct report; ask lead/planner instead",
		},
		{
			name:    "generic manager",
			owner:   "other/bob",
			wantErr: "owner other/bob is not a direct report; ask your manager instead",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			writeDecideConfig(t, `{
				"owners": {
					"mode": "warn",
					"allowed": ["lead/*"],
					"managers": {"team/*": "lead/planner"}
				}
			}`)
			stdout, stderr, code := runCLI("ask", "--owner", tc.owner, "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Proceed?")
			if code != 0 {
				t.Fatalf("ask = %d, stderr=%q", code, stderr)
			}
			if !strings.Contains(stderr, tc.wantErr) {
				t.Errorf("stderr = %q, want substring %q", stderr, tc.wantErr)
			}
			id := strings.TrimSpace(stdout)

			got := showJSON(t, id)
			if !slices.Contains(got.Tags, decide.TagOffLine) {
				t.Errorf("showJSON tags = %v, want %q", got.Tags, decide.TagOffLine)
			}

			showOut, showErr, showCode := runCLI("show", id)
			if showCode != 0 {
				t.Fatalf("show = %d, stderr=%q", showCode, showErr)
			}
			if !strings.Contains(showOut, "tag: off-line\n") {
				t.Errorf("show output = %q, want tag: off-line", showOut)
			}

			showJSONOut, showJSONErr, showJSONCode := runCLI("show", "--json", id)
			if showJSONCode != 0 {
				t.Fatalf("show --json = %d, stderr=%q", showJSONCode, showJSONErr)
			}
			var decoded struct {
				Tags []string `json:"tags"`
			}
			if err := json.Unmarshal([]byte(showJSONOut), &decoded); err != nil {
				t.Fatalf("unmarshal show --json: %v", err)
			}
			if !slices.Contains(decoded.Tags, decide.TagOffLine) {
				t.Errorf("show --json tags = %v, want %q", decoded.Tags, decide.TagOffLine)
			}
		})
	}
}

func TestAskAndSendOwnerAllowListEnforce(t *testing.T) {
	writeDecideConfig(t, `{
		"owners": {
			"mode": "enforce",
			"allowed": ["lead/*"],
			"managers": {"team/*": "lead/planner"}
		},
		"targets": [{"name": "team/bob", "pane": "pane-b"}],
		"sendTemplate": "true {pane}"
	}`)

	stdout, stderr, code := runCLI("ask", "--owner", "team/bob", "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Proceed?")
	if code != 3 {
		t.Fatalf("ask exit = %d, want 3 (stdout=%q stderr=%q)", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "owner team/bob is not a direct report; ask lead/planner instead") {
		t.Errorf("ask stderr = %q", stderr)
	}

	stdout, stderr, code = runCLI("send", "--to", "team/bob", "hello")
	if code != 3 {
		t.Fatalf("send exit = %d, want 3 (stdout=%q stderr=%q)", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "owner team/bob is not a direct report; ask lead/planner instead") {
		t.Errorf("send stderr = %q", stderr)
	}

	if items := showThreads(t); len(items) != 0 {
		t.Fatalf("ledger has %d items, want 0", len(items))
	}
}

func TestSendWarnTagsAndUINeverBlocked(t *testing.T) {
	t.Run("send warn", func(t *testing.T) {
		writeDecideConfig(t, `{
			"owners": {
				"mode": "warn",
				"allowed": ["lead/*"],
				"managers": {"team/*": "lead/planner"}
			},
			"targets": [{"name": "team/bob", "pane": "pane-b"}],
			"sendTemplate": "true {pane}"
		}`)
		stdout, stderr, code := runCLI("send", "--to", "team/bob", "hello")
		if code != 0 {
			t.Fatalf("send = %d, stdout=%q stderr=%q", code, stdout, stderr)
		}
		if !strings.Contains(stderr, "owner team/bob is not a direct report; ask lead/planner instead") {
			t.Errorf("stderr = %q", stderr)
		}
		items := showThreads(t)
		if len(items) != 1 || !slices.Contains(items[0].Tags, decide.TagOffLine) {
			t.Fatalf("items = %+v, want 1 item tagged %q", items, decide.TagOffLine)
		}
	})

	t.Run("via ui in enforce", func(t *testing.T) {
		writeDecideConfig(t, `{
			"owners": {
				"mode": "enforce",
				"allowed": ["lead/*"]
			},
			"targets": [{"name": "team/bob", "pane": "pane-b"}],
			"sendTemplate": "true {pane}"
		}`)
		stdout, stderr, code := runCLI("send", "--via", "ui", "--to", "team/bob", "hello from ui")
		if code != 0 {
			t.Fatalf("send --via ui = %d, stdout=%q stderr=%q", code, stdout, stderr)
		}
		if stderr != "" {
			t.Errorf("stderr = %q, want empty", stderr)
		}
		items := showThreads(t)
		if len(items) != 1 || len(items[0].Tags) != 0 {
			t.Fatalf("items = %+v, want 1 untagged item", items)
		}
	})
}

func TestReplyToExistingItemByDisallowedOwnerAllowed(t *testing.T) {
	for _, mode := range []string{decide.OwnersWarn, decide.OwnersEnforce} {
		t.Run(mode, func(t *testing.T) {
			writeDecideConfig(t, `{
				"owners": {
					"mode": "`+mode+`",
					"allowed": ["lead/*"]
				},
				"targets": [{"name": "team/bob", "pane": "pane-b"}],
				"sendTemplate": "true {pane}"
			}`)
			// UI creates the item for disallowed owner team/bob.
			_, stderr, code := runCLI("send", "--via", "ui", "--to", "team/bob", "please check")
			if code != 0 {
				t.Fatalf("send --via ui = %d, stderr=%q", code, stderr)
			}
			items := showThreads(t)
			if len(items) != 1 {
				t.Fatalf("items = %d, want 1", len(items))
			}
			id := items[0].ID

			// Disallowed owner replies to the existing item.
			_, stderr, code = runCLI("reply", id, "on it")
			if code != 0 {
				t.Fatalf("reply = %d, stderr=%q", code, stderr)
			}
			if stderr != "" {
				t.Errorf("reply stderr = %q, want empty", stderr)
			}
		})
	}
}

func TestListOffLine(t *testing.T) {
	writeDecideConfig(t, `{
		"owners": {
			"mode": "warn",
			"allowed": ["lead/*"],
			"managers": {"team/*": "lead/planner"}
		}
	}`)

	// Create an untagged item (allowed owner).
	allowedOut, stderr, code := runCLI("ask", "--owner", "lead/planner", "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Allowed open?")
	if code != 0 {
		t.Fatalf("ask allowed = %d, stderr=%q", code, stderr)
	}
	allowedID := strings.TrimSpace(allowedOut)

	// With no off-line items, list --off-line is empty and exits 0.
	stdout, stderr, code := runCLI("list", "--off-line")
	if code != 0 {
		t.Fatalf("list --off-line (empty) = %d, stderr=%q", code, stderr)
	}
	if stdout != "" {
		t.Errorf("list --off-line (empty) stdout = %q, want empty", stdout)
	}
	stdout, stderr, code = runCLI("list", "--off-line", "--json")
	if code != 0 {
		t.Fatalf("list --off-line --json (empty) = %d, stderr=%q", code, stderr)
	}
	if strings.TrimSpace(stdout) != "[]" {
		t.Errorf("list --off-line --json (empty) = %q, want []", stdout)
	}

	// Create two off-line items (disallowed owners in warn mode).
	offOpenOut, _, code := runCLI("ask", "--owner", "team/bob", "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Off-line open?")
	if code != 0 {
		t.Fatalf("ask off-line 1 = %d", code)
	}
	offOpenID := strings.TrimSpace(offOpenOut)

	offAnsweredOut, _, code := runCLI("ask", "--owner", "team/alice", "--kind", "relay", "--option", "a:Yes", "--option", "b:No", "--rec", "a", "Off-line answered?")
	if code != 0 {
		t.Fatalf("ask off-line 2 = %d", code)
	}
	offAnsweredID := strings.TrimSpace(offAnsweredOut)

	// Answer one off-line item and the allowed item so both states have tagged + untagged items.
	if _, stderr, code := runCLI("answer", "--via", "ui", offAnsweredID, "a"); code != 0 {
		t.Fatalf("answer off-line = %d, stderr=%q", code, stderr)
	}
	if _, stderr, code := runCLI("answer", "--via", "ui", allowedID, "a"); code != 0 {
		t.Fatalf("answer allowed = %d, stderr=%q", code, stderr)
	}

	// Default state (open): only offOpenID is listed.
	stdout, stderr, code = runCLI("list", "--off-line")
	if code != 0 {
		t.Fatalf("list --off-line = %d, stderr=%q", code, stderr)
	}
	if !strings.Contains(stdout, offOpenID) || strings.Contains(stdout, offAnsweredID) || strings.Contains(stdout, allowedID) {
		t.Errorf("list --off-line = %q, want only %s", stdout, offOpenID)
	}

	// --json includes the off-line tag.
	stdout, stderr, code = runCLI("list", "--off-line", "--json")
	if code != 0 {
		t.Fatalf("list --off-line --json = %d, stderr=%q", code, stderr)
	}
	var openItems []decide.Item
	if err := json.Unmarshal([]byte(stdout), &openItems); err != nil {
		t.Fatalf("unmarshal list --off-line --json: %v", err)
	}
	if len(openItems) != 1 || openItems[0].ID != offOpenID || !slices.Contains(openItems[0].Tags, decide.TagOffLine) {
		t.Errorf("list --off-line --json = %+v, want 1 item %s with tag %q", openItems, offOpenID, decide.TagOffLine)
	}

	// Combines with --state: answered lists only offAnsweredID, not allowedID or offOpenID.
	stdout, stderr, code = runCLI("list", "--off-line", "--state", "answered", "--json")
	if code != 0 {
		t.Fatalf("list --off-line --state answered --json = %d, stderr=%q", code, stderr)
	}
	var answeredItems []decide.Item
	if err := json.Unmarshal([]byte(stdout), &answeredItems); err != nil {
		t.Fatalf("unmarshal list --off-line --state answered --json: %v", err)
	}
	if len(answeredItems) != 1 || answeredItems[0].ID != offAnsweredID || !slices.Contains(answeredItems[0].Tags, decide.TagOffLine) {
		t.Errorf("list --off-line --state answered --json = %+v, want 1 item %s with tag %q", answeredItems, offAnsweredID, decide.TagOffLine)
	}
}
