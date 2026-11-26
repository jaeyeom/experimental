package config_test

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestLoadSearchOrder(t *testing.T) {
	const cwd = "/work/repo"
	local := docNamed("from-local", cwd)
	repoRoot := docNamed("from-repo-root", cwd)
	xdgDoc := docNamed("from-xdg", cwd)
	explicit := docNamed("from-explicit", cwd)

	tests := []struct {
		name     string
		explicit string
		wd       string
		home     string
		xdg      string
		xdgSet   bool
		dirs     []string
		files    map[string]string
		wantName string
		wantPath string
		wantErr  string
	}{
		{
			name:     "explicit path",
			explicit: "/cfg/explicit.toml",
			wd:       "/work",
			home:     "/home/u",
			xdg:      "/xdg",
			xdgSet:   true,
			files: map[string]string{
				"/cfg/explicit.toml":        explicit,
				"/work/factory.toml":        local,
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-explicit",
			wantPath: "/cfg/explicit.toml",
		},
		{
			name:   "cwd factory.toml",
			wd:     "/work",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			files: map[string]string{
				"/work/factory.toml":        local,
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-local",
			wantPath: "/work/factory.toml",
		},
		{
			name:   "xdg config home",
			wd:     "/work",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			files: map[string]string{
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-xdg",
			wantPath: "/xdg/factory/factory.toml",
		},
		{
			name: "home config when XDG_CONFIG_HOME is unset",
			wd:   "/work",
			home: "/home/u",
			files: map[string]string{
				"/home/u/.config/factory/factory.toml": xdgDoc,
			},
			wantName: "from-xdg",
			wantPath: "/home/u/.config/factory/factory.toml",
		},
		{
			name:    "none found",
			wd:      "/work",
			home:    "/home/u",
			wantErr: "error: no config found; looked in: /work/factory.toml, /home/u/.config/factory/factory.toml",
		},
		{
			name:    "empty XDG_CONFIG_HOME",
			wd:      "/work",
			home:    "/home/u",
			xdgSet:  true,
			wantErr: "XDG_CONFIG_HOME: expected a non-empty path, found empty",
		},
		{
			name:     "missing explicit path",
			explicit: "/cfg/missing.toml",
			wd:       "/work",
			home:     "/home/u",
			files: map[string]string{
				"/work/factory.toml": local,
			},
			wantErr: "config file not found: /cfg/missing.toml",
		},
		{
			name:   "cwd factory.toml wins over git root and xdg",
			wd:     "/work/repo/sub/dir",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			dirs:   []string{"/work/repo/.git"},
			files: map[string]string{
				"/work/repo/sub/dir/factory.toml": local,
				"/work/repo/factory.toml":         repoRoot,
				"/xdg/factory/factory.toml":       xdgDoc,
			},
			wantName: "from-local",
			wantPath: "/work/repo/sub/dir/factory.toml",
		},
		{
			name:   "git root factory.toml in nested subdir with .git dir",
			wd:     "/work/repo/sub/dir",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			dirs:   []string{"/work/repo/.git"},
			files: map[string]string{
				"/work/repo/factory.toml":   repoRoot,
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-repo-root",
			wantPath: "/work/repo/factory.toml",
		},
		{
			name:   "git root factory.toml in nested subdir with worktree .git file",
			wd:     "/work/repo/sub/dir",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			files: map[string]string{
				"/work/repo/.git":           "gitdir: /work/.git/worktrees/repo\n",
				"/work/repo/factory.toml":   repoRoot,
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-repo-root",
			wantPath: "/work/repo/factory.toml",
		},
		{
			name:   "falls back to xdg when git repo has no factory.toml",
			wd:     "/work/repo/sub/dir",
			home:   "/home/u",
			xdg:    "/xdg",
			xdgSet: true,
			dirs:   []string{"/work/repo/.git"},
			files: map[string]string{
				"/xdg/factory/factory.toml": xdgDoc,
			},
			wantName: "from-xdg",
			wantPath: "/xdg/factory/factory.toml",
		},
		{
			name:    "none found inside nested git subdir lists all three paths",
			wd:      "/work/repo/sub/dir",
			home:    "/home/u",
			dirs:    []string{"/work/repo/.git"},
			wantErr: "error: no config found; looked in: /work/repo/sub/dir/factory.toml, /work/repo/factory.toml, /home/u/.config/factory/factory.toml",
		},
		{
			name:    "none found at git root omits duplicate repo root path",
			wd:      "/work/repo",
			home:    "/home/u",
			dirs:    []string{"/work/repo/.git"},
			wantErr: "error: no config found; looked in: /work/repo/factory.toml, /home/u/.config/factory/factory.toml",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fsys := newFake(t)
			fsys.wd = tc.wd
			fsys.home = tc.home
			if tc.xdgSet {
				fsys.env["XDG_CONFIG_HOME"] = tc.xdg
			}
			fsys.dir(cwd)
			for _, d := range tc.dirs {
				fsys.dir(d)
			}
			for path, body := range tc.files {
				fsys.file(path, body, 0o644)
			}
			cfg, err := config.Load(tc.explicit, fsys.environment())
			if tc.wantErr != "" {
				if err == nil {
					t.Fatalf("Load() error = nil, want %q", tc.wantErr)
				}
				if err.Error() != tc.wantErr {
					t.Fatalf("Load() error = %q, want %q", err.Error(), tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("Load() error = %v", err)
			}
			if cfg.Path != tc.wantPath {
				t.Fatalf("Path = %q, want %q", cfg.Path, tc.wantPath)
			}
			if len(cfg.Sources) != 1 || cfg.Sources[0].Name != tc.wantName {
				t.Fatalf("sources = %+v, want name %s", cfg.Sources, tc.wantName)
			}
		})
	}
}

func TestLoadGate(t *testing.T) {
	fs := newFake(t)
	fs.env["HERDR_PANE_ID"] = "w1:p1"
	doc := tomlDoc(append([]string{
		"[gate]",
		`ignore_panes = ["w1:p9", "w1:p8"]`,
		`ignore_tabs = ["dashboard", "coord"]`,
	}, githubLines()...))
	cfg, err := fs.load(doc)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.CallerPaneID != "w1:p1" {
		t.Fatalf("CallerPaneID = %q", cfg.CallerPaneID)
	}
	if !reflect.DeepEqual(cfg.Gate.IgnorePanes, []string{"w1:p9", "w1:p8"}) {
		t.Fatalf("IgnorePanes = %#v", cfg.Gate.IgnorePanes)
	}
	if !reflect.DeepEqual(cfg.Gate.IgnoreTabs, []string{"dashboard", "coord"}) {
		t.Fatalf("IgnoreTabs = %#v", cfg.Gate.IgnoreTabs)
	}
	if cfg.Gate.BaseBranch != "" || cfg.Gate.Require != "" {
		t.Fatalf("branch gate = %#v, want unset", cfg.Gate)
	}
}

func TestLoadGateBaseBranch(t *testing.T) {
	tests := []struct {
		name    string
		lines   []string
		branch  string
		require string
		wantErr string
	}{
		{
			name:    "on_base",
			lines:   []string{`base_branch = "main"`, `require = "on_base"`},
			branch:  "main",
			require: config.GateOnBase,
		},
		{
			name:    "clean_against_base",
			lines:   []string{`base_branch = "main"`, `require = "clean_against_base"`},
			branch:  "main",
			require: config.GateCleanAgainstBase,
		},
		{
			name:    "unknown_require",
			lines:   []string{`base_branch = "main"`, `require = "maybe"`},
			wantErr: `gate.require: expected on_base or clean_against_base, found "maybe"`,
		},
		{
			name:    "require_without_branch",
			lines:   []string{`require = "on_base"`},
			wantErr: "gate.base_branch: expected a non-empty branch, found empty",
		},
		{
			name:    "branch_without_require",
			lines:   []string{`base_branch = "main"`},
			wantErr: "gate.require: expected on_base or clean_against_base, found empty",
		},
		{
			name: "empty_require_with_ignores",
			lines: []string{
				`ignore_panes = ["w1:p1"]`,
				`base_branch = ""`,
				`require = ""`,
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			doc := tomlDoc(append(append([]string{"[gate]"}, tc.lines...), githubLines()...))
			cfg, err := newFake(t).load(doc)
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("Load() error = %v, want %s", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if cfg.Gate.BaseBranch != tc.branch || cfg.Gate.Require != tc.require {
				t.Fatalf("gate branch = %q require = %q", cfg.Gate.BaseBranch, cfg.Gate.Require)
			}
		})
	}
}

func TestLoadHold(t *testing.T) {
	home := "/home/u"
	tests := []struct {
		name    string
		lines   []string
		files   []string
		tabs    []string
		ttl     time.Duration
		ttlSet  bool
		def     string
		home    string
		wantErr string
	}{
		{
			name: "default path",
			home: home,
			def:  filepath.Join(home, ".local", "state", "factory", "hold"),
		},
		{
			name:  "explicit empty list uses default",
			lines: []string{"hold_files = []"},
			home:  home,
			def:   filepath.Join(home, ".local", "state", "factory", "hold"),
		},
		{
			name:   "tilde and ttl",
			lines:  []string{`hold_files = ["~/state/hold", "/tmp/other"]`, `hold_tabs = ["coord"]`, `hold_ttl = "12h"`},
			home:   home,
			files:  []string{filepath.Join(home, "state", "hold"), "/tmp/other"},
			tabs:   []string{"coord"},
			ttl:    12 * time.Hour,
			ttlSet: true,
		},
		{
			name:   "zero ttl is set",
			lines:  []string{`hold_files = ["/tmp/hold"]`, `hold_ttl = "0s"`},
			home:   home,
			files:  []string{"/tmp/hold"},
			ttlSet: true,
		},
		{
			name:    "negative ttl",
			lines:   []string{`hold_ttl = "-1s"`},
			home:    home,
			wantErr: "gate.hold_ttl: expected a non-negative duration, found -1s",
		},
		{
			name:    "bad ttl",
			lines:   []string{`hold_ttl = "soon"`},
			home:    home,
			wantErr: `gate.hold_ttl: expected a duration, found "soon": time: invalid duration "soon"`,
		},
		{
			name:    "empty path",
			lines:   []string{`hold_files = [""]`},
			home:    home,
			wantErr: "gate.hold_files: expected a non-empty path, found empty",
		},
		{
			name:    "empty tab",
			lines:   []string{`hold_tabs = ["coord", ""]`},
			home:    home,
			wantErr: "gate.hold_tabs: expected a non-empty tab label, found empty",
		},
		{
			name:    "tilde without home",
			lines:   []string{`hold_files = ["~/hold"]`},
			wantErr: "gate.hold_files: home directory: expected a non-empty path, found empty",
		},
		{
			name:    "default without home",
			wantErr: "gate.hold_files: home directory: expected a non-empty path, found empty",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fs := newFake(t)
			fs.home = tc.home
			doc := tomlDoc(append(append([]string{"[gate]"}, tc.lines...), githubLines()...))
			cfg, err := fs.load(doc)
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("Load() error = %v, want %s", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if len(cfg.Gate.HoldFiles) != len(tc.files) {
				t.Fatalf("HoldFiles = %#v, want %#v", cfg.Gate.HoldFiles, tc.files)
			}
			for i := range tc.files {
				if cfg.Gate.HoldFiles[i] != tc.files[i] {
					t.Fatalf("HoldFiles = %#v, want %#v", cfg.Gate.HoldFiles, tc.files)
				}
			}
			if len(cfg.Gate.HoldTabs) != len(tc.tabs) {
				t.Fatalf("HoldTabs = %#v, want %#v", cfg.Gate.HoldTabs, tc.tabs)
			}
			for i := range tc.tabs {
				if cfg.Gate.HoldTabs[i] != tc.tabs[i] {
					t.Fatalf("HoldTabs = %#v, want %#v", cfg.Gate.HoldTabs, tc.tabs)
				}
			}
			if cfg.Gate.DefaultHoldFile != tc.def {
				t.Fatalf("DefaultHoldFile = %q, want %q", cfg.Gate.DefaultHoldFile, tc.def)
			}
			switch {
			case tc.ttlSet && cfg.Gate.HoldTTL == nil:
				t.Fatal("HoldTTL = nil, want a duration")
			case !tc.ttlSet && cfg.Gate.HoldTTL != nil:
				t.Fatalf("HoldTTL = %s, want unset", *cfg.Gate.HoldTTL)
			case tc.ttlSet && *cfg.Gate.HoldTTL != tc.ttl:
				t.Fatalf("HoldTTL = %s, want %s", *cfg.Gate.HoldTTL, tc.ttl)
			}
			got := cfg.Gate.ActiveHoldFiles()
			wantActive := tc.files
			if len(wantActive) == 0 {
				wantActive = []string{tc.def}
			}
			if !reflect.DeepEqual(got, wantActive) {
				t.Fatalf("ActiveHoldFiles = %#v, want %#v", got, wantActive)
			}
		})
	}
}

func TestLoadCallerPaneUnset(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.CallerPaneID != "" {
		t.Fatalf("CallerPaneID = %q, want empty", cfg.CallerPaneID)
	}
	if cfg.Gate.IgnorePanes != nil || cfg.Gate.IgnoreTabs != nil || cfg.Gate.BaseBranch != "" || cfg.Gate.Require != "" {
		t.Fatalf("Gate = %+v, want empty", cfg.Gate)
	}
}

func TestLoadCallerPaneEmpty(t *testing.T) {
	fs := newFake(t)
	fs.env["HERDR_PANE_ID"] = ""
	_, err := fs.load(tomlDoc(githubLines()))
	if err == nil || err.Error() != "HERDR_PANE_ID: expected a non-empty pane id, found empty" {
		t.Fatalf("Load() error = %v", err)
	}
}

func TestLoadGateEmptyEntry(t *testing.T) {
	tests := []struct {
		name string
		line string
		want string
	}{
		{
			name: "pane",
			line: `ignore_panes = [""]`,
			want: "gate.ignore_panes: expected a non-empty pane id, found empty",
		},
		{
			name: "tab",
			line: `ignore_tabs = ["dashboard", ""]`,
			want: "gate.ignore_tabs: expected a non-empty tab label, found empty",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			doc := tomlDoc(append([]string{"[gate]", tc.line}, githubLines()...))
			_, err := newFake(t).load(doc)
			if err == nil || err.Error() != tc.want {
				t.Fatalf("Load() error = %v, want %s", err, tc.want)
			}
		})
	}
}

func TestLoadUnknownKey(t *testing.T) {
	tests := []struct {
		name string
		doc  string
		key  string
	}{
		{
			name: "root key",
			doc: tomlDoc(append(
				[]string{"max_in_flght = 1"},
				githubLines()...,
			)),
			key: "max_in_flght",
		},
		{
			name: "defaults key",
			doc: tomlDoc(append(
				[]string{"[defaults]", "dry_rnn = true"},
				githubLines()...,
			)),
			key: "dry_rnn",
		},
		{
			name: "source key",
			doc:  tomlDoc(append(githubLines(), `not_a_field = "x"`)),
			key:  "not_a_field",
		},
		{
			name: "gate key",
			doc: tomlDoc(append(
				[]string{"[gate]", `ignore_pane = ["w1:p1"]`},
				githubLines()...,
			)),
			key: "ignore_pane",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tc.doc)
			if err == nil {
				t.Fatal("Load() error = nil, want unknown key")
			}
			if !strings.Contains(err.Error(), tc.key) {
				t.Fatalf("Load() error = %q, want it to name %s", err.Error(), tc.key)
			}
		})
	}
}

func TestLoadRankOrder(t *testing.T) {
	unset, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := sourcesByName(t, unset)["experimental"].RankOrder; got != nil {
		t.Fatalf("rank_order = %#v, want nil", got)
	}

	doc := tomlDoc(append(githubLines(), `rank_order = ["parent_rank", "unblocks", "priority", "created"]`))
	cfg, err := newFake(t).load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	got := sourcesByName(t, cfg)["experimental"].RankOrder
	want := []string{"parent_rank", "unblocks", "priority", "created"}
	if !slices.Equal(got, want) {
		t.Fatalf("rank_order = %#v, want %#v", got, want)
	}
}

func TestLoadRankOrderRejected(t *testing.T) {
	tests := []struct {
		name string
		line string
		want []string
	}{
		{
			name: "unknown",
			line: `rank_order = ["parent_rank", "unblocks", "priority", "id"]`,
			want: []string{`source "experimental"`, `field "rank_order"`, "unknown key", `"id"`},
		},
		{
			name: "duplicate",
			line: `rank_order = ["unblocks", "unblocks", "priority", "created"]`,
			want: []string{`field "rank_order"`, "duplicate", `"unblocks"`},
		},
		{
			name: "missing",
			line: `rank_order = ["unblocks", "priority", "created"]`,
			want: []string{`field "rank_order"`, "missing", `"parent_rank"`},
		},
		{
			name: "empty",
			line: `rank_order = []`,
			want: []string{`field "rank_order"`, "missing", `"parent_rank"`, `"unblocks"`, `"priority"`, `"created"`},
		},
		{
			name: "empty key",
			line: `rank_order = ["unblocks", "", "priority", "created"]`,
			want: []string{`field "rank_order"`, "found empty"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			doc := tomlDoc(append(githubLines(), tc.line))
			_, err := newFake(t).load(doc)
			if err == nil {
				t.Fatal("Load() error = nil")
			}
			for _, part := range tc.want {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("Load() error = %q, want %q", err.Error(), part)
				}
			}
		})
	}
}

func TestLoadParentBlocksChild(t *testing.T) {
	unset, err := newFake(t).load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := sourcesByName(t, unset)["experimental"].ParentBlocksChild; got != "ignore" {
		t.Fatalf("parent_blocks_child = %q, want ignore", got)
	}

	doc := tomlDoc(append(githubLines(), `parent_blocks_child = "honor"`))
	cfg, err := newFake(t).load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := sourcesByName(t, cfg)["experimental"].ParentBlocksChild; got != "honor" {
		t.Fatalf("parent_blocks_child = %q, want honor", got)
	}

	tests := []struct {
		name string
		line string
		want []string
	}{
		{
			name: "unknown",
			line: `parent_blocks_child = "drop"`,
			want: []string{`source "experimental"`, `field "parent_blocks_child"`, "expected", "ignore", "honor", `found "drop"`},
		},
		{
			name: "empty",
			line: `parent_blocks_child = ""`,
			want: []string{`field "parent_blocks_child"`, "expected", "ignore", "honor", "found empty"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			bad := tomlDoc(append(githubLines(), tc.line))
			_, err := newFake(t).load(bad)
			if err == nil {
				t.Fatal("Load() error = nil")
			}
			for _, part := range tc.want {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("Load() error = %q, want %q", err.Error(), part)
				}
			}
		})
	}
}

func TestLoadValidation(t *testing.T) {
	tests := []struct {
		name    string
		doc     string
		mutate  func(*fakeFS)
		wantErr []string
	}{
		{
			name:    "kind missing",
			doc:     tomlDoc(without(githubLines(), "kind")),
			wantErr: []string{`field "kind"`, "jira", "github"},
		},
		{
			name:    "kind not jira or github",
			doc:     tomlDoc(replaceLine(githubLines(), "kind", `kind = "svn"`)),
			wantErr: []string{`field "kind"`, "svn"},
		},
		{
			name:    "kind is case sensitive",
			doc:     tomlDoc(replaceLine(githubLines(), "kind", `kind = "Jira"`)),
			wantErr: []string{`field "kind"`, "Jira"},
		},
		{
			name:    "name missing",
			doc:     tomlDoc(without(githubLines(), "name")),
			wantErr: []string{`field "name"`},
		},
		{
			name:    "name duplicate",
			doc:     tomlDoc(append(githubLines(), githubLines()...)),
			wantErr: []string{`source "experimental"`, `field "name"`, "unique"},
		},
		{
			name:    "jira site missing",
			doc:     tomlDoc(without(jiraLines(), "site")),
			wantErr: []string{`source "acme"`, `field "site"`},
		},
		{
			name:    "jira site must be https",
			doc:     tomlDoc(replaceLine(jiraLines(), "site", `site = "http://example.atlassian.net"`)),
			wantErr: []string{`field "site"`, "https"},
		},
		{
			name:    "jira email missing",
			doc:     tomlDoc(without(jiraLines(), "email")),
			wantErr: []string{`field "email"`},
		},
		{
			name:    "jira token file and token env",
			doc:     tomlDoc(append(jiraLines(), `token_file = "/secrets/jira-token"`)),
			wantErr: []string{`field "token_file"`, "exactly one", "both"},
		},
		{
			name:    "jira token missing",
			doc:     tomlDoc(without(jiraLines(), "token_env")),
			wantErr: []string{`field "token_file"`, "exactly one", "neither"},
		},
		{
			name:    "jira jql missing",
			doc:     tomlDoc(without(jiraLines(), "jql")),
			wantErr: []string{`field "jql"`},
		},
		{
			name:    "jira claim status missing",
			doc:     tomlDoc(without(jiraLines(), "claim_status")),
			wantErr: []string{`field "claim_status"`},
		},
		{
			name:    "jira cwd missing",
			doc:     tomlDoc(without(jiraLines(), "cwd")),
			wantErr: []string{`field "cwd"`},
		},
		{
			name:    "jira base missing",
			doc:     tomlDoc(without(jiraLines(), "base")),
			wantErr: []string{`field "base"`},
		},
		{
			name:    "jira start prompt missing",
			doc:     tomlDoc(without(jiraLines(), "start_prompt")),
			wantErr: []string{`field "start_prompt"`},
		},
		{
			name:    "github repo missing",
			doc:     tomlDoc(without(githubLines(), "repo")),
			wantErr: []string{`field "repo"`},
		},
		{
			name:    "github repo shape",
			doc:     tomlDoc(replaceLine(githubLines(), "repo", `repo = "you/yourrepo/extra"`)),
			wantErr: []string{`field "repo"`, "you/yourrepo/extra"},
		},
		{
			name:    "github cwd missing",
			doc:     tomlDoc(without(githubLines(), "cwd")),
			wantErr: []string{`field "cwd"`},
		},
		{
			name:    "github base missing",
			doc:     tomlDoc(without(githubLines(), "base")),
			wantErr: []string{`field "base"`},
		},
		{
			name:    "github start prompt missing",
			doc:     tomlDoc(without(githubLines(), "start_prompt")),
			wantErr: []string{`field "start_prompt"`},
		},
		{
			name:    "cwd missing on disk",
			doc:     tomlDoc(replaceLine(githubLines(), "cwd", `cwd = "/work/missing"`)),
			wantErr: []string{`field "cwd"`, "/work/missing", "directory"},
		},
		{
			name: "cwd is a file",
			doc:  tomlDoc(replaceLine(githubLines(), "cwd", `cwd = "/work/not-a-dir"`)),
			mutate: func(f *fakeFS) {
				f.file("/work/not-a-dir", "x", 0o644)
			},
			wantErr: []string{`field "cwd"`, "file"},
		},
		{
			name: "workspace rule has parent and label",
			doc: tomlDoc(append(githubLines(),
				"[[source.workspace_rule]]",
				`parent = "PRJ-1"`,
				`label = "clang-tidy"`,
				`workspace = "smoke"`,
			)),
			wantErr: []string{`field "workspace_rule"`, "exactly one", "both"},
		},
		{
			name: "workspace rule has neither parent nor label",
			doc: tomlDoc(append(githubLines(),
				"[[source.workspace_rule]]",
				`workspace = "smoke"`,
			)),
			wantErr: []string{`field "workspace_rule"`, "exactly one", "neither"},
		},
		{
			name: "workspace rule missing workspace",
			doc: tomlDoc(append(githubLines(),
				"[[source.workspace_rule]]",
				`parent = "PRJ-1"`,
			)),
			wantErr: []string{`field "workspace_rule"`, "workspace"},
		},
		{
			name: "agent rule has no matcher",
			doc: tomlDoc(append(githubLines(),
				"[[agent_rule]]",
				`agent_args = ["--model", "sonnet"]`,
			)),
			wantErr: []string{"agent_rule 1", "labels", "neither"},
		},
		{
			name: "agent rule label is empty",
			doc: tomlDoc(append(githubLines(),
				"[[agent_rule]]",
				`labels = [""]`,
			)),
			wantErr: []string{"agent_rule 1", `field "labels"`, "found empty"},
		},
		{
			name:    "agent arg is empty",
			doc:     tomlDoc(append([]string{"[defaults]", `agent_args = ["--model", ""]`}, githubLines()...)),
			wantErr: []string{`field "agent_args"`, "found empty"},
		},
		{
			name:    "unknown placeholder",
			doc:     tomlDoc(replaceLine(githubLines(), "start_prompt", `start_prompt = "work {nope}"`)),
			wantErr: []string{`field "start_prompt"`, "{nope}", "{caller}", "{caller_bus}", "{report_back}"},
		},
		{
			name:    "literal brace",
			doc:     tomlDoc(replaceLine(githubLines(), "start_prompt", `start_prompt = "brace } here"`)),
			wantErr: []string{`field "start_prompt"`, "literal braces"},
		},
		{
			name:    "assignee_ok",
			doc:     tomlDoc(append(githubLines(), `assignee_ok = "someone"`)),
			wantErr: []string{`field "assignee_ok"`, "someone"},
		},
		{
			name:    "create_assignee",
			doc:     tomlDoc(append(jiraLines(), `create_assignee = "anyone"`)),
			wantErr: []string{`field "create_assignee"`, "expected me or unassigned", "anyone"},
		},
		{
			name:    "pr title pattern needs id",
			doc:     tomlDoc(append(githubLines(), `pr_title_pattern = "prefix"`)),
			wantErr: []string{`field "pr_title_pattern"`, "{id}", "prefix"},
		},
		{
			name:    "pr title pattern must compile",
			doc:     tomlDoc(append(githubLines(), `pr_title_pattern = "[{id}"`)),
			wantErr: []string{`field "pr_title_pattern"`, "X-1"},
		},
		{
			name: "known placeholders",
			doc: tomlDoc(replaceLine(githubLines(), "start_prompt",
				`start_prompt = "{id} {title} {url} {source} {base} {parent} {caller} {caller_bus} {report_back}"`)),
		},
		{
			name:    "empty priority rank name",
			doc:     tomlDoc(append(jiraLines(), `priority_ranks = { "" = 1 }`)),
			wantErr: []string{`source "acme"`, `field "priority_ranks"`, "non-empty", "found empty"},
		},
		{
			name:    "empty known priority",
			doc:     tomlDoc(append(jiraLines(), `known_priorities = [""]`)),
			wantErr: []string{`source "acme"`, `field "known_priorities"`, "non-empty", "found empty"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fsys := newFake(t)
			if tc.mutate != nil {
				tc.mutate(fsys)
			}
			_, err := fsys.load(tc.doc)
			if len(tc.wantErr) == 0 {
				if err != nil {
					t.Fatalf("Load() error = %v", err)
				}
				return
			}
			if err == nil {
				t.Fatalf("Load() error = nil, want %q", tc.wantErr)
			}
			for _, part := range tc.wantErr {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("Load() error = %q, want substring %q", err.Error(), part)
				}
			}
		})
	}
}

func TestLoadPriorityRanks(t *testing.T) {
	fsys := newFake(t)
	doc := tomlDoc(append(jiraLines(),
		`priority_ranks = { Highest = 9, High = 1 }`,
		`known_priorities = ["Untriaged", "High"]`,
	))
	cfg, err := fsys.load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	src := cfg.Sources[0]
	if src.PriorityRanks["Highest"] != 9 || src.PriorityRanks["High"] != 1 || len(src.PriorityRanks) != 2 {
		t.Fatalf("priority_ranks = %#v", src.PriorityRanks)
	}
	if len(src.KnownPriorities) != 2 || src.KnownPriorities[0] != "Untriaged" || src.KnownPriorities[1] != "High" {
		t.Fatalf("known_priorities = %#v", src.KnownPriorities)
	}

	ghDoc := tomlDoc(append(githubLines(), `priority_ranks = { Highest = 4 }`, `known_priorities = ["Queued"]`))
	ghCfg, err := fsys.load(ghDoc)
	if err != nil {
		t.Fatalf("Load(github) error = %v", err)
	}
	got := ghCfg.Sources[0]
	if got.Kind != "github" || got.PriorityRanks["Highest"] != 4 || len(got.KnownPriorities) != 1 || got.KnownPriorities[0] != "Queued" {
		t.Fatalf("github source = kind %s ranks %#v known %#v", got.Kind, got.PriorityRanks, got.KnownPriorities)
	}
}

func TestLoadPRTemplate(t *testing.T) {
	fsys := newFake(t)
	cfg, err := fsys.load(tomlDoc(append(githubLines(), `pr_template = "docs/pull_request_template.md"`)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if cfg.Sources[0].PRTemplate != "docs/pull_request_template.md" {
		t.Fatalf("PRTemplate = %q", cfg.Sources[0].PRTemplate)
	}
	plain, err := fsys.load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if plain.Sources[0].PRTemplate != "" {
		t.Fatalf("default PRTemplate = %q, want empty", plain.Sources[0].PRTemplate)
	}
}

func TestLoadOrphanIgnore(t *testing.T) {
	fsys := newFake(t)
	cfg, err := fsys.load(tomlDoc(append(jiraLines(),
		`orphan_ignore = ["PRJ-1", "PRJ-2"]`,
		`orphan_ignore_labels = ["wont-fix"]`,
	)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if !reflect.DeepEqual(cfg.Sources[0].OrphanIgnore, []string{"PRJ-1", "PRJ-2"}) {
		t.Fatalf("OrphanIgnore = %#v", cfg.Sources[0].OrphanIgnore)
	}
	if !reflect.DeepEqual(cfg.Sources[0].OrphanIgnoreLabels, []string{"wont-fix"}) {
		t.Fatalf("OrphanIgnoreLabels = %#v", cfg.Sources[0].OrphanIgnoreLabels)
	}
	plain, err := fsys.load(tomlDoc(jiraLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if plain.Sources[0].OrphanIgnore != nil || plain.Sources[0].OrphanIgnoreLabels != nil {
		t.Fatalf("unset ignore = %#v labels %#v", plain.Sources[0].OrphanIgnore, plain.Sources[0].OrphanIgnoreLabels)
	}
	empty, err := fsys.load(tomlDoc(append(jiraLines(),
		`orphan_ignore = []`,
		`orphan_ignore_labels = []`,
	)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if empty.Sources[0].OrphanIgnore == nil || len(empty.Sources[0].OrphanIgnore) != 0 {
		t.Fatalf("empty orphan_ignore = %#v", empty.Sources[0].OrphanIgnore)
	}
	if empty.Sources[0].OrphanIgnoreLabels == nil || len(empty.Sources[0].OrphanIgnoreLabels) != 0 {
		t.Fatalf("empty orphan_ignore_labels = %#v", empty.Sources[0].OrphanIgnoreLabels)
	}
}

func TestLoadDoctorFields(t *testing.T) {
	fsys := newFake(t)
	plain, err := fsys.load(tomlDoc(jiraLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	src := plain.Sources[0]
	if !reflect.DeepEqual(src.ClaimParentTypes, []string{"Epic", "Story"}) ||
		!reflect.DeepEqual(src.OrphanEpicChildTypes, []string{"Story", "Task", "Bug"}) ||
		src.ExternalBlockerLookups != 100 {
		t.Fatalf("defaults = %#v %#v %d", src.ClaimParentTypes, src.OrphanEpicChildTypes, src.ExternalBlockerLookups)
	}
	set, err := fsys.load(tomlDoc(append(jiraLines(),
		`claim_parent_types = ["Initiative"]`,
		`orphan_epic_child_types = []`,
		`external_blocker_lookups = 0`,
	)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	src = set.Sources[0]
	if !reflect.DeepEqual(src.ClaimParentTypes, []string{"Initiative"}) ||
		src.OrphanEpicChildTypes == nil || len(src.OrphanEpicChildTypes) != 0 ||
		src.ExternalBlockerLookups != 0 {
		t.Fatalf("set = %#v %#v %d", src.ClaimParentTypes, src.OrphanEpicChildTypes, src.ExternalBlockerLookups)
	}
	for _, tc := range []struct {
		line string
		want string
	}{
		{line: `doctor_blocker_lookups = -1`, want: `field "doctor_blocker_lookups": expected a non-negative count, found -1`},
		{line: `external_blocker_lookups = -1`, want: `field "external_blocker_lookups": expected a non-negative count, found -1`},
		{line: "external_blocker_lookups = 1\ndoctor_blocker_lookups = 2", want: `expected one of external_blocker_lookups or doctor_blocker_lookups (the old name), found both`},
		{line: `claim_parent_types = [""]`, want: `field "claim_parent_types": expected a non-empty item type, found ""`},
		{line: `orphan_epic_child_types = [" "]`, want: `field "orphan_epic_child_types": expected a non-empty item type, found " "`},
	} {
		_, err := fsys.load(tomlDoc(append(jiraLines(), tc.line)))
		if err == nil || !strings.Contains(err.Error(), tc.want) {
			t.Fatalf("%s: error = %v, want %q", tc.line, err, tc.want)
		}
	}
}

func TestLoadClaimTransition(t *testing.T) {
	fsys := newFake(t)
	cfg, err := fsys.load(tomlDoc(append(jiraLines(), `claim_transition = "start work"`)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if cfg.Sources[0].ClaimTransition != "start work" {
		t.Fatalf("ClaimTransition = %q", cfg.Sources[0].ClaimTransition)
	}
	plain, err := fsys.load(tomlDoc(jiraLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if plain.Sources[0].ClaimTransition != "" {
		t.Fatalf("default ClaimTransition = %q, want empty", plain.Sources[0].ClaimTransition)
	}
}

func TestLoadExpandsHome(t *testing.T) {
	fsys := newFake(t)
	const home = "/home/u"
	fsys.home = home
	repo := home + "/src/yourrepo"
	fsys.dir(repo)
	token := home + "/.config/factory/jira-token"
	fsys.file(token, "  secret-token \n", 0o600)
	doc := `
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "you@example.com"
token_file = "~/.config/factory/jira-token"
jql = "project = PRJ"
claim_status = "In Progress"
cwd = "~/src/yourrepo"
base = "main"
start_prompt = "work {id}"
`
	cfg, err := fsys.load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	src := cfg.Sources[0]
	if src.Cwd != repo {
		t.Fatalf("Cwd = %q, want %q", src.Cwd, repo)
	}
	if src.TokenFile != token {
		t.Fatalf("TokenFile = %q, want %q", src.TokenFile, token)
	}
	if src.Token != "" {
		t.Fatalf("Load read the token, Token = %q", src.Token)
	}
	if err := cfg.Sources[0].ResolveToken(); err != nil {
		t.Fatalf("ResolveToken: %v", err)
	}
	if cfg.Sources[0].Token != "secret-token" {
		t.Fatalf("Token = %q, want secret-token", cfg.Sources[0].Token)
	}
}

func TestLoadStartPromptFile(t *testing.T) {
	fsys := newFake(t)
	fsys.home = "/home/u"
	fsys.dir("/home/u/prompts")
	fsys.file("/home/u/prompts/start.txt", "see {url}\n", 0o644)
	fsys.file("/prompts/plain.txt", "work {id} {title}\n", 0o644)
	tests := []struct {
		name   string
		prompt string
		want   string
	}{
		{name: "at file", prompt: "@/prompts/plain.txt", want: "work {id} {title}\n"},
		{name: "at file with tilde", prompt: "@~/prompts/start.txt", want: "see {url}\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			doc := tomlDoc(replaceLine(githubLines(), "start_prompt", fmt.Sprintf("start_prompt = %q", tc.prompt)))
			cfg, err := fsys.load(doc)
			if err != nil {
				t.Fatalf("Load() error = %v", err)
			}
			if cfg.Sources[0].StartPrompt != tc.want {
				t.Fatalf("StartPrompt = %q, want %q", cfg.Sources[0].StartPrompt, tc.want)
			}
		})
	}
}

func TestLoadDefaultsAndListOverride(t *testing.T) {
	doc := `
[defaults]
max_in_flight = 4
min_start_interval = "1m"
full_sync_every = "2h"
stall_after = "30m"
exclude_labels = ["custom"]
human_labels = ["review"]
skip_types = ["Bug"]
agent_kind = "codex"
dry_run = false

[[source]]
name = "inherits"
kind = "github"
repo = "you/inherits"
cwd = "/work/repo"
base = "main"
start_prompt = "work {id}"

[[source]]
name = "overrides"
kind = "github"
repo = "you/overrides"
cwd = "/work/repo"
base = "main"
start_prompt = "work {title}"
exclude_labels = ["only"]
human_labels = ["person"]
skip_types = ["Story"]

[[source]]
name = "clears"
kind = "github"
repo = "you/clears"
cwd = "/work/repo"
base = "main"
start_prompt = "work {url}"
exclude_labels = []
human_labels = []
skip_types = []
`
	cfg, err := newFake(t).load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if cfg.Defaults.MaxInFlight != 4 ||
		cfg.Defaults.MinStartInterval != time.Minute ||
		cfg.Defaults.FullSyncEvery != 2*time.Hour ||
		cfg.Defaults.StallAfter != 30*time.Minute ||
		cfg.Defaults.AgentKind != "codex" ||
		cfg.Defaults.DryRun {
		t.Fatalf("defaults = %+v", cfg.Defaults)
	}
	if !reflect.DeepEqual(cfg.Defaults.ExcludeLabels, []string{"custom"}) {
		t.Fatalf("default exclude labels = %#v", cfg.Defaults.ExcludeLabels)
	}
	if !reflect.DeepEqual(cfg.Defaults.HumanLabels, []string{"review"}) {
		t.Fatalf("default human labels = %#v", cfg.Defaults.HumanLabels)
	}
	byName := sourcesByName(t, cfg)
	if !reflect.DeepEqual(byName["inherits"].ExcludeLabels, []string{"custom"}) {
		t.Fatalf("inherits exclude = %#v", byName["inherits"].ExcludeLabels)
	}
	if !reflect.DeepEqual(byName["inherits"].HumanLabels, []string{"review"}) {
		t.Fatalf("inherits human = %#v", byName["inherits"].HumanLabels)
	}
	if !reflect.DeepEqual(byName["inherits"].SkipTypes, []string{"Bug"}) {
		t.Fatalf("inherits skip = %#v", byName["inherits"].SkipTypes)
	}
	if !reflect.DeepEqual(byName["overrides"].ExcludeLabels, []string{"only"}) {
		t.Fatalf("overrides exclude = %#v, want [only]", byName["overrides"].ExcludeLabels)
	}
	if !reflect.DeepEqual(byName["overrides"].HumanLabels, []string{"person"}) {
		t.Fatalf("overrides human = %#v, want [person]", byName["overrides"].HumanLabels)
	}
	if !reflect.DeepEqual(byName["overrides"].SkipTypes, []string{"Story"}) {
		t.Fatalf("overrides skip = %#v, want [Story]", byName["overrides"].SkipTypes)
	}
	if len(byName["clears"].ExcludeLabels) != 0 || len(byName["clears"].HumanLabels) != 0 || len(byName["clears"].SkipTypes) != 0 {
		t.Fatalf("clears = exclude %#v human %#v skip %#v", byName["clears"].ExcludeLabels, byName["clears"].HumanLabels, byName["clears"].SkipTypes)
	}
}

func TestLoadAgentRules(t *testing.T) {
	doc := tomlDoc(append([]string{
		"[defaults]",
		`agent_args = ["--model", "opus"]`,
		"",
		"[[agent_rule]]",
		`labels = ["mechanical"]`,
		`agent_args = ["--model", "sonnet"]`,
		"",
		"[[agent_rule]]",
		`labels = ["report-only", "chore"]`,
		`agent_args = []`,
	}, githubLines()...))
	cfg, err := newFake(t).load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if !reflect.DeepEqual(cfg.Defaults.AgentArgs, []string{"--model", "opus"}) {
		t.Fatalf("defaults = %#v", cfg.Defaults.AgentArgs)
	}
	if len(cfg.AgentRules) != 2 ||
		!reflect.DeepEqual(cfg.AgentRules[0].Labels, []string{"mechanical"}) ||
		!reflect.DeepEqual(cfg.AgentRules[0].AgentArgs, []string{"--model", "sonnet"}) ||
		!reflect.DeepEqual(cfg.AgentRules[1].Labels, []string{"report-only", "chore"}) ||
		len(cfg.AgentRules[1].AgentArgs) != 0 {
		t.Fatalf("rules = %#v", cfg.AgentRules)
	}
	cfg.AgentRules[0].Labels[0] = "mutated"
	cfg.AgentRules[0].AgentArgs[0] = "mutated"
	again, err := newFake(t).load(doc)
	if err != nil {
		t.Fatal(err)
	}
	if again.AgentRules[0].Labels[0] == "mutated" || again.AgentRules[0].AgentArgs[0] == "mutated" {
		t.Fatal("agent rule slices are shared")
	}
}

func TestLoadCaller(t *testing.T) {
	tests := []struct {
		name string
		line string
		want string
	}{
		{name: "set", line: `caller = "coord"`, want: "coord"},
		{name: "empty", line: `caller = ""`, want: ""},
		{name: "blank", line: `caller = "   "`, want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			doc := tomlDoc(append([]string{"[defaults]", tc.line}, githubLines()...))
			cfg, err := newFake(t).load(doc)
			if err != nil {
				t.Fatalf("Load() error = %v", err)
			}
			if cfg.Defaults.Caller != tc.want {
				t.Fatalf("Caller = %q, want %q", cfg.Defaults.Caller, tc.want)
			}
		})
	}
}

func TestLoadSyncMaxAge(t *testing.T) {
	top := newFake(t)
	cfg, err := top.load("[sync]\nmax_age = \"15m\"\n" + tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if cfg.Sync.MaxAge != 15*time.Minute {
		t.Fatalf("Sync.MaxAge = %s", cfg.Sync.MaxAge)
	}
	if cfg.Sources[0].MaxAge != nil {
		t.Fatalf("source MaxAge = %v, want nil", cfg.Sources[0].MaxAge)
	}
	if cfg.Sources[0].EffectiveMaxAge(cfg.Sync.MaxAge) != 15*time.Minute {
		t.Fatal("source did not inherit sync.max_age")
	}

	over := newFake(t)
	doc := "[sync]\nmax_age = \"15m\"\n" + tomlDoc(append(githubLines(), `max_age = "0s"`))
	cfg, err = over.load(doc)
	if err != nil {
		t.Fatalf("override Load() error = %v", err)
	}
	if cfg.Sources[0].MaxAge == nil || *cfg.Sources[0].MaxAge != 0 {
		t.Fatalf("override MaxAge = %v", cfg.Sources[0].MaxAge)
	}
	if cfg.Sources[0].EffectiveMaxAge(cfg.Sync.MaxAge) != 0 {
		t.Fatal("source max_age did not override the top-level value")
	}

	omitted := newFake(t)
	cfg, err = omitted.load(tomlDoc(githubLines()))
	if err != nil {
		t.Fatalf("omitted Load() error = %v", err)
	}
	if cfg.Sync.MaxAge != 0 || cfg.Sources[0].MaxAge != nil {
		t.Fatalf("omitted = sync %s source %v", cfg.Sync.MaxAge, cfg.Sources[0].MaxAge)
	}

	bad := []struct {
		name string
		doc  string
		want string
	}{
		{name: "top", doc: "[sync]\nmax_age = \"soon\"\n" + tomlDoc(githubLines()), want: `sync.max_age: expected a duration, found "soon"`},
		{name: "negative top", doc: "[sync]\nmax_age = \"-1s\"\n" + tomlDoc(githubLines()), want: "sync.max_age: expected a non-negative duration, found -1s"},
		{name: "source", doc: tomlDoc(append(githubLines(), `max_age = "soon"`)), want: `source "experimental" field "max_age": expected a duration, found "soon"`},
		{name: "negative source", doc: tomlDoc(append(githubLines(), `max_age = "-1m"`)), want: `source "experimental" field "max_age": expected a non-negative duration, found -1m0s`},
	}
	for _, tc := range bad {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newFake(t).load(tc.doc)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestLoadBuiltinDefaults(t *testing.T) {
	doc := tomlDoc(append(githubLines(), jiraLines()...))
	doc = strings.Replace(doc, `claim_status = "In Progress"`, "claim_status = \"In Progress\"\nclaim_assign = false", 1)
	cfg, err := newFake(t).load(doc)
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	wantLabels := []string{"needs-human", "blocked-external"}
	wantHuman := []string{"needs-human"}
	wantSkip := []string{"Epic"}
	if cfg.Defaults.MaxInFlight != 3 ||
		cfg.Defaults.MinStartInterval != 10*time.Minute ||
		cfg.Defaults.FullSyncEvery != 24*time.Hour ||
		cfg.Defaults.StallAfter != 2*time.Hour ||
		cfg.Defaults.AgentKind != "claude" ||
		!cfg.Defaults.DryRun ||
		cfg.Defaults.Caller != "" ||
		!reflect.DeepEqual(cfg.Defaults.ExcludeLabels, wantLabels) ||
		!reflect.DeepEqual(cfg.Defaults.HumanLabels, wantHuman) ||
		!reflect.DeepEqual(cfg.Defaults.SkipTypes, wantSkip) {
		t.Fatalf("defaults = %+v", cfg.Defaults)
	}
	byName := sourcesByName(t, cfg)
	gh := byName["experimental"]
	jira := byName["acme"]
	if !reflect.DeepEqual(gh.ExcludeLabels, wantLabels) || !reflect.DeepEqual(jira.ExcludeLabels, wantLabels) {
		t.Fatalf("exclude labels github=%#v jira=%#v", gh.ExcludeLabels, jira.ExcludeLabels)
	}
	if !reflect.DeepEqual(gh.HumanLabels, wantHuman) || !reflect.DeepEqual(jira.HumanLabels, wantHuman) {
		t.Fatalf("human labels github=%#v jira=%#v", gh.HumanLabels, jira.HumanLabels)
	}
	gh.ExcludeLabels[0] = "mutated"
	if jira.ExcludeLabels[0] == "mutated" {
		t.Fatal("exclude label slices are shared")
	}
	gh.HumanLabels[0] = "mutated"
	if jira.HumanLabels[0] == "mutated" {
		t.Fatal("human label slices are shared")
	}
	if gh.Workspace != "yourrepo" {
		t.Fatalf("github workspace = %q, want yourrepo", gh.Workspace)
	}
	if gh.ClaimStatus != config.DefaultGitHubClaimStatus || gh.ClaimLabel != "" {
		t.Fatalf("github claim status=%q label=%q, want %q and empty", gh.ClaimStatus, gh.ClaimLabel, config.DefaultGitHubClaimStatus)
	}
	if !reflect.DeepEqual(gh.ReadyStatuses, config.DefaultGitHubReadyStatuses()) {
		t.Fatalf("github ready_statuses = %#v, want %#v", gh.ReadyStatuses, config.DefaultGitHubReadyStatuses())
	}
	if gh.AssigneeOK != "me" || jira.AssigneeOK != "me" {
		t.Fatalf("assignee_ok github=%q jira=%q", gh.AssigneeOK, jira.AssigneeOK)
	}
	if jira.ClaimAssign {
		t.Fatal("explicit claim_assign = false was replaced with the default")
	}
	if jira.BlocksLinkType != "Blocks" || jira.CreateIssueType != "Task" || jira.PRTitlePattern != `\[{id}\]` {
		t.Fatalf("jira defaults = %+v", jira)
	}
	if err := resolveTokens(cfg); err != nil {
		t.Fatalf("ResolveToken: %v", err)
	}
	if token := jiraToken(cfg); token != "from-env" {
		t.Fatalf("Token = %q", token)
	}
}

func TestLoadTokenFileMode(t *testing.T) {
	tests := []struct {
		name    string
		mode    fs.FileMode
		wantErr bool
	}{
		{name: "owner only", mode: 0o600},
		{name: "group read", mode: 0o640, wantErr: true},
		{name: "other read", mode: 0o604, wantErr: true},
		{name: "world read", mode: 0o644, wantErr: true},
	}
	const tokenPath = "/secrets/jira-token"
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fsys := newFake(t)
			fsys.file(tokenPath, "  secret-token \n", tc.mode)
			doc := tomlDoc(append(without(jiraLines(), "token_env"), `token_file = "/secrets/jira-token"`))
			cfg, err := fsys.load(doc)
			if err != nil {
				t.Fatalf("Load() error = %v, want nil (mode is checked at use)", err)
			}
			if cfg.Sources[0].Token != "" {
				t.Fatalf("Load read the token, Token = %q", cfg.Sources[0].Token)
			}
			err = cfg.Sources[0].ResolveToken()
			if tc.wantErr {
				if err == nil {
					t.Fatal("ResolveToken() error = nil, want mode error")
				}
				if !strings.Contains(err.Error(), "chmod 600") || !strings.Contains(err.Error(), tokenPath) {
					t.Fatalf("ResolveToken() error = %q, want chmod 600 and %s", err.Error(), tokenPath)
				}
				if !strings.Contains(err.Error(), `field "token_file"`) || !strings.Contains(err.Error(), "acme") {
					t.Fatalf("ResolveToken() error = %q, want source and field", err.Error())
				}
				return
			}
			if err != nil {
				t.Fatalf("ResolveToken() error = %v", err)
			}
			if cfg.Sources[0].Token != "secret-token" {
				t.Fatalf("Token = %q", cfg.Sources[0].Token)
			}
		})
	}
}

func TestResolveTokenDeferred(t *testing.T) {
	t.Run("missing file", func(t *testing.T) {
		fsys := newFake(t)
		const tokenPath = "/missing/jira-token"
		doc := tomlDoc(append(without(jiraLines(), "token_env"), `token_file = "/missing/jira-token"`))
		cfg, err := fsys.load(doc)
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if cfg.Sources[0].Token != "" {
			t.Fatalf("Load read the token, Token = %q", cfg.Sources[0].Token)
		}
		err = cfg.Sources[0].ResolveToken()
		if err == nil {
			t.Fatal("ResolveToken() error = nil, want missing file")
		}
		for _, part := range []string{`source "acme"`, `field "token_file"`, tokenPath} {
			if !strings.Contains(err.Error(), part) {
				t.Fatalf("ResolveToken() error = %q, want substring %q", err.Error(), part)
			}
		}
		again := cfg.Sources[0].ResolveToken()
		if again == nil || again.Error() != err.Error() {
			t.Fatalf("second ResolveToken() = %v, want %v", again, err)
		}
	})

	t.Run("unset env", func(t *testing.T) {
		fsys := newFake(t)
		delete(fsys.env, "JIRA_TOKEN")
		cfg, err := fsys.load(tomlDoc(jiraLines()))
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if cfg.Sources[0].Token != "" {
			t.Fatalf("Load read the token, Token = %q", cfg.Sources[0].Token)
		}
		err = cfg.Sources[0].ResolveToken()
		if err == nil {
			t.Fatal("ResolveToken() error = nil, want unset")
		}
		for _, part := range []string{`source "acme"`, `field "token_env"`, "JIRA_TOKEN", "unset"} {
			if !strings.Contains(err.Error(), part) {
				t.Fatalf("ResolveToken() error = %q, want substring %q", err.Error(), part)
			}
		}
	})

	t.Run("reads env at use", func(t *testing.T) {
		fsys := newFake(t)
		cfg, err := fsys.load(tomlDoc(jiraLines()))
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if cfg.Sources[0].Token != "" {
			t.Fatalf("Load read the token, Token = %q", cfg.Sources[0].Token)
		}
		fsys.env["JIRA_TOKEN"] = "later"
		if err := cfg.Sources[0].ResolveToken(); err != nil {
			t.Fatalf("ResolveToken: %v", err)
		}
		if cfg.Sources[0].Token != "later" {
			t.Fatalf("Token = %q, want later", cfg.Sources[0].Token)
		}
		delete(fsys.env, "JIRA_TOKEN")
		if err := cfg.Sources[0].ResolveToken(); err != nil {
			t.Fatalf("second ResolveToken: %v", err)
		}
		if cfg.Sources[0].Token != "later" {
			t.Fatalf("Token after second call = %q, want later", cfg.Sources[0].Token)
		}
	})

	t.Run("nil source", func(t *testing.T) {
		var src *config.Source
		err := src.ResolveToken()
		if err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("ResolveToken() = %v, want nil source", err)
		}
	})

	t.Run("github is a no-op", func(t *testing.T) {
		cfg, err := newFake(t).load(tomlDoc(githubLines()))
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if err := cfg.Sources[0].ResolveToken(); err != nil {
			t.Fatalf("ResolveToken: %v", err)
		}
		if cfg.Sources[0].Token != "" {
			t.Fatalf("Token = %q", cfg.Sources[0].Token)
		}
	})
}

func TestLoadExampleFile(t *testing.T) {
	home := t.TempDir()
	repo := filepath.Join(home, "src", "yourrepo")
	if err := os.MkdirAll(repo, 0o750); err != nil {
		t.Fatal(err)
	}
	prompt := "Work {id} {title} {url} from {source} on {base}. Parent {parent}.\n"
	if err := os.WriteFile(filepath.Join(repo, "start.txt"), []byte(prompt), 0o600); err != nil {
		t.Fatal(err)
	}
	tokenDir := filepath.Join(home, ".config", "factory")
	if err := os.MkdirAll(tokenDir, 0o750); err != nil {
		t.Fatal(err)
	}
	tokenPath := filepath.Join(tokenDir, "jira-token")
	if err := os.WriteFile(tokenPath, []byte("  secret-token \n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(tokenPath, 0o600); err != nil {
		t.Fatal(err)
	}

	example := examplePath(t)
	cfg, err := config.Load(example, config.Env{
		LookupEnv: func(string) (string, bool) { return "", false },
		Getwd:     func() (string, error) { return t.TempDir(), nil },
		HomeDir:   func() (string, error) { return home, nil },
		ReadFile:  os.ReadFile,
		Stat:      os.Stat,
	})
	if err != nil {
		t.Fatalf("Load(example) error = %v", err)
	}
	if cfg.Path != example {
		t.Fatalf("Path = %q, want %q", cfg.Path, example)
	}
	if cfg.Defaults.DryRun != true || cfg.Defaults.MaxInFlight != 3 || cfg.Defaults.StallAfter != 2*time.Hour {
		t.Fatalf("defaults = %+v", cfg.Defaults)
	}
	if len(cfg.Sources) != 2 {
		t.Fatalf("sources = %d, want 2", len(cfg.Sources))
	}
	byName := sourcesByName(t, cfg)
	jira := byName["acme"]
	gh := byName["experimental"]
	if jira.Kind != "jira" || gh.Kind != "github" {
		t.Fatalf("kinds = %s %s", jira.Kind, gh.Kind)
	}
	if jira.Email != "you@example.com" || jira.Site != "https://example.atlassian.net" {
		t.Fatalf("jira identity = %+v", jira)
	}
	if jira.Cwd != repo || gh.Cwd != repo {
		t.Fatalf("cwd jira=%q github=%q, want %q", jira.Cwd, gh.Cwd, repo)
	}
	if jira.Token != "" {
		t.Fatalf("Load read the token, Token = %q", jira.Token)
	}
	if err := resolveTokens(cfg); err != nil {
		t.Fatalf("ResolveToken: %v", err)
	}
	if token := jiraToken(cfg); token != "secret-token" {
		t.Fatalf("Token = %q", token)
	}
	if jira.StartPrompt != prompt {
		t.Fatalf("jira prompt = %q", jira.StartPrompt)
	}
	if gh.StartPrompt != "/gh-issue-resolver {url} Do not run factory file, link, or release on a remote lane. When done, report back: {report_back}" {
		t.Fatalf("github prompt = %q", gh.StartPrompt)
	}
	if gh.Repo != "you/yourrepo" || gh.Workspace != "yourrepo" {
		t.Fatalf("github repo=%q workspace=%q", gh.Repo, gh.Workspace)
	}
	if len(jira.WorkspaceRules) != 2 ||
		jira.WorkspaceRules[0].Parent != "PRJ-2131" ||
		jira.WorkspaceRules[0].Workspace != "smoke" ||
		jira.WorkspaceRules[1].Label != "clang-tidy" ||
		jira.WorkspaceRules[1].Workspace != "clang-tidy" {
		t.Fatalf("workspace rules = %+v", jira.WorkspaceRules)
	}
}

func TestLoadExamples(t *testing.T) {
	home := t.TempDir()
	monorepo := filepath.Join(home, "src", "monorepo")
	repo := filepath.Join(home, "src", "yourrepo")
	if err := os.MkdirAll(monorepo, 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(repo, 0o750); err != nil {
		t.Fatal(err)
	}
	tokenDir := filepath.Join(home, ".config", "factory")
	if err := os.MkdirAll(tokenDir, 0o750); err != nil {
		t.Fatal(err)
	}
	tokenPath := filepath.Join(tokenDir, "jira-token")
	if err := os.WriteFile(tokenPath, []byte("  secret-token \n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(tokenPath, 0o600); err != nil {
		t.Fatal(err)
	}
	env := config.Env{
		LookupEnv: func(string) (string, bool) { return "", false },
		Getwd:     func() (string, error) { return t.TempDir(), nil },
		HomeDir:   func() (string, error) { return home, nil },
		ReadFile:  os.ReadFile,
		Stat:      os.Stat,
	}
	dir := examplesDir(t)

	t.Run("jira-monorepo", func(t *testing.T) {
		path := filepath.Join(dir, "jira-monorepo.toml")
		cfg, err := config.Load(path, env)
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if cfg.Path != path {
			t.Fatalf("Path = %q, want %q", cfg.Path, path)
		}
		if len(cfg.Sources) != 1 {
			t.Fatalf("sources = %d, want 1", len(cfg.Sources))
		}
		src := cfg.Sources[0]
		if src.Name != "monorepo" || src.Kind != "jira" {
			t.Fatalf("source = %s %s", src.Kind, src.Name)
		}
		if src.Cwd != monorepo {
			t.Fatalf("cwd = %q, want %q", src.Cwd, monorepo)
		}
		if src.Token != "" {
			t.Fatalf("Load read the token, Token = %q", src.Token)
		}
		if err := cfg.Sources[0].ResolveToken(); err != nil {
			t.Fatalf("ResolveToken: %v", err)
		}
		if cfg.Sources[0].Token != "secret-token" {
			t.Fatalf("Token = %q", cfg.Sources[0].Token)
		}
		if len(src.RequireLabels) != 1 || src.RequireLabels[0] != "agent-ready" {
			t.Fatalf("require_labels = %q", src.RequireLabels)
		}
		if len(src.WorkspaceRules) != 2 ||
			src.WorkspaceRules[0].Parent != "PRJ-2131" ||
			src.WorkspaceRules[0].Label != "" ||
			src.WorkspaceRules[0].Workspace != "smoke" ||
			src.WorkspaceRules[1].Parent != "" ||
			src.WorkspaceRules[1].Label != "clang-tidy" ||
			src.WorkspaceRules[1].Workspace != "clang-tidy" {
			t.Fatalf("workspace rules = %+v", src.WorkspaceRules)
		}
		branch := strings.Index(src.StartPrompt, "origin/{base}")
		ticket := strings.Index(src.StartPrompt, "/implement-jira-ticket {id}")
		if branch < 0 || ticket < 0 || branch > ticket {
			t.Fatalf("prompt = %q", src.StartPrompt)
		}
		if !strings.Contains(src.StartPrompt, "git fetch origin") {
			t.Fatalf("prompt = %q", src.StartPrompt)
		}
	})

	t.Run("github-repo", func(t *testing.T) {
		path := filepath.Join(dir, "github-repo.toml")
		cfg, err := config.Load(path, env)
		if err != nil {
			t.Fatalf("Load() error = %v", err)
		}
		if len(cfg.Sources) != 1 {
			t.Fatalf("sources = %d, want 1", len(cfg.Sources))
		}
		src := cfg.Sources[0]
		if src.Name != "issues" || src.Kind != "github" || src.Repo != "you/yourrepo" {
			t.Fatalf("source = %+v", src)
		}
		if src.Workspace != "yourrepo" {
			t.Fatalf("workspace = %q, want repo name yourrepo", src.Workspace)
		}
		if src.Cwd != repo {
			t.Fatalf("cwd = %q, want %q", src.Cwd, repo)
		}
		if src.StartPrompt != "/gh-issue-resolver {url}" {
			t.Fatalf("prompt = %q", src.StartPrompt)
		}
		if len(src.RequireLabels) != 1 || src.RequireLabels[0] != "agent-ready" {
			t.Fatalf("require_labels = %q", src.RequireLabels)
		}
	})
}

func examplePath(t *testing.T) string {
	t.Helper()
	return filepath.Join("..", "..", "factory.example.toml")
}

func examplesDir(t *testing.T) string {
	t.Helper()
	return filepath.Join(filepath.Dir(examplePath(t)), "examples")
}

func resolveTokens(cfg *config.Config) error {
	for i := range cfg.Sources {
		if err := cfg.Sources[i].ResolveToken(); err != nil {
			return err
		}
	}
	return nil
}

func jiraToken(cfg *config.Config) string {
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind == "jira" {
			return cfg.Sources[i].Token
		}
	}
	return ""
}

func sourcesByName(t *testing.T, cfg *config.Config) map[string]config.Source {
	t.Helper()
	out := make(map[string]config.Source, len(cfg.Sources))
	for _, src := range cfg.Sources {
		out[src.Name] = src
	}
	return out
}

func docNamed(name, cwd string) string {
	return fmt.Sprintf(`
[[source]]
name = %q
kind = "github"
repo = "you/yourrepo"
cwd = %q
base = "main"
start_prompt = "work {id}"
`, name, cwd)
}

func githubLines() []string {
	return []string{
		"[[source]]",
		`name = "experimental"`,
		`kind = "github"`,
		`repo = "you/yourrepo"`,
		`cwd = "/work/repo"`,
		`base = "main"`,
		`start_prompt = "work {id}"`,
	}
}

func jiraLines() []string {
	return []string{
		"[[source]]",
		`name = "acme"`,
		`kind = "jira"`,
		`site = "https://example.atlassian.net"`,
		`email = "you@example.com"`,
		`token_env = "JIRA_TOKEN"`,
		`jql = "project = PRJ"`,
		`claim_status = "In Progress"`,
		`cwd = "/work/repo"`,
		`base = "main"`,
		`start_prompt = "work {id}"`,
	}
}

func tomlDoc(lines []string) string {
	return strings.Join(lines, "\n") + "\n"
}

func without(lines []string, key string) []string {
	out := make([]string, 0, len(lines))
	prefix := key + " "
	exact := key + "="
	for _, line := range lines {
		if strings.HasPrefix(line, prefix) || strings.HasPrefix(line, exact) {
			continue
		}
		out = append(out, line)
	}
	return out
}

func replaceLine(lines []string, key, newline string) []string {
	out := append([]string(nil), lines...)
	prefix := key + " "
	exact := key + "="
	for i, line := range out {
		if strings.HasPrefix(line, prefix) || strings.HasPrefix(line, exact) {
			out[i] = newline
			return out
		}
	}
	return append(out, newline)
}

type fakeFS struct {
	wd    string
	home  string
	env   map[string]string
	files map[string]fakeFile
}

type fakeFile struct {
	data string
	mode fs.FileMode
	dir  bool
}

func newFake(t *testing.T) *fakeFS {
	t.Helper()
	f := &fakeFS{
		wd:    "/work",
		home:  "/home/u",
		env:   map[string]string{"JIRA_TOKEN": "from-env"},
		files: map[string]fakeFile{},
	}
	f.dir("/work/repo")
	f.file("/secrets/jira-token", "token\n", 0o600)
	return f
}

func (f *fakeFS) dir(path string) {
	f.files[path] = fakeFile{dir: true, mode: 0o755}
}

func (f *fakeFS) file(path, data string, mode fs.FileMode) {
	f.files[path] = fakeFile{data: data, mode: mode}
}

func (f *fakeFS) load(doc string) (*config.Config, error) {
	const path = "/cfg/factory.toml"
	f.file(path, doc, 0o644)
	return config.Load(path, f.environment())
}

func (f *fakeFS) environment() config.Env {
	return config.Env{
		LookupEnv: func(key string) (string, bool) {
			v, ok := f.env[key]
			return v, ok
		},
		Getwd:   func() (string, error) { return f.wd, nil },
		HomeDir: func() (string, error) { return f.home, nil },
		ReadFile: func(path string) ([]byte, error) {
			file, ok := f.files[path]
			if !ok || file.dir {
				return nil, fs.ErrNotExist
			}
			return []byte(file.data), nil
		},
		Stat: func(path string) (fs.FileInfo, error) {
			file, ok := f.files[path]
			if !ok {
				return nil, fs.ErrNotExist
			}
			return fakeInfo{name: filepath.Base(path), mode: file.mode, dir: file.dir}, nil
		},
	}
}

type fakeInfo struct {
	name string
	mode fs.FileMode
	dir  bool
}

func (f fakeInfo) Name() string       { return f.name }
func (f fakeInfo) Size() int64        { return 0 }
func (f fakeInfo) Mode() fs.FileMode  { return f.mode }
func (f fakeInfo) ModTime() time.Time { return time.Time{} }
func (f fakeInfo) IsDir() bool        { return f.dir }
func (f fakeInfo) Sys() any           { return nil }

func TestLoadCreateSubtaskIssueType(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(jiraLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := cfg.Sources[0].CreateSubtaskIssueType; got != "" {
		t.Fatalf("default create_subtask_issue_type = %q, want empty", got)
	}
	cfg, err = newFake(t).load(tomlDoc(append(jiraLines(), `create_subtask_issue_type = "Subtask"`)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := cfg.Sources[0].CreateSubtaskIssueType; got != "Subtask" {
		t.Fatalf("create_subtask_issue_type = %q, want Subtask", got)
	}
}

func TestLoadCreateAssignee(t *testing.T) {
	cfg, err := newFake(t).load(tomlDoc(jiraLines()))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := cfg.Sources[0].CreateAssignee; got != config.CreateAssigneeMe {
		t.Fatalf("default create_assignee = %q, want me", got)
	}
	cfg, err = newFake(t).load(tomlDoc(append(githubLines(), `create_assignee = "unassigned"`)))
	if err != nil {
		t.Fatalf("Load() error = %v", err)
	}
	if got := cfg.Sources[0].CreateAssignee; got != config.CreateAssigneeUnassigned {
		t.Fatalf("create_assignee = %q, want unassigned", got)
	}
}

func TestGitRoot(t *testing.T) {
	tests := []struct {
		name     string
		dir      string
		dirs     []string
		files    map[string]fs.FileMode
		statErr  error
		wantRoot string
		wantOK   bool
		wantErr  string
	}{
		{
			name:     "at git root with .git directory",
			dir:      "/work/repo",
			dirs:     []string{"/work/repo/.git"},
			wantRoot: "/work/repo",
			wantOK:   true,
		},
		{
			name:     "nested subdirectory with .git directory",
			dir:      "/work/repo/a/b",
			dirs:     []string{"/work/repo/.git"},
			wantRoot: "/work/repo",
			wantOK:   true,
		},
		{
			name:     "nested subdirectory with worktree .git regular file",
			dir:      "/work/wt/sub",
			files:    map[string]fs.FileMode{"/work/wt/.git": 0o644},
			wantRoot: "/work/wt",
			wantOK:   true,
		},
		{
			name:     "nearest ancestor .git wins",
			dir:      "/work/outer/inner/sub",
			dirs:     []string{"/work/outer/.git", "/work/outer/inner/.git"},
			wantRoot: "/work/outer/inner",
			wantOK:   true,
		},
		{
			name:   "outside any git repository",
			dir:    "/work/plain/sub",
			wantOK: false,
		},
		{
			name:    "empty directory",
			dir:     "",
			wantErr: "git root: expected a non-empty directory, found empty",
		},
		{
			name:    "non-directory non-regular .git marker",
			dir:     "/work/repo/sub",
			files:   map[string]fs.FileMode{"/work/repo/.git": fs.ModeNamedPipe},
			wantErr: "expected directory or regular file",
		},
		{
			name:    "stat permission error",
			dir:     "/work/repo",
			statErr: fs.ErrPermission,
			wantErr: "git marker /work/repo/.git:",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fsys := newFake(t)
			for _, d := range tc.dirs {
				fsys.dir(d)
			}
			for path, mode := range tc.files {
				fsys.file(path, "", mode)
			}
			env := fsys.environment()
			if tc.statErr != nil {
				env.Stat = func(string) (fs.FileInfo, error) {
					return nil, tc.statErr
				}
			}
			root, ok, err := config.GitRoot(tc.dir, env)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("GitRoot() error = %v, want substring %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("GitRoot() error = %v", err)
			}
			if ok != tc.wantOK || root != tc.wantRoot {
				t.Fatalf("GitRoot() = (%q, %v), want (%q, %v)", root, ok, tc.wantRoot, tc.wantOK)
			}
		})
	}
}
