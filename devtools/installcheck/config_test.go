package installcheck

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestParseConfigValid(t *testing.T) {
	tests := []struct {
		name     string
		toml     string
		wantMode string
		wantLbl  string
	}{
		{
			name: "agentbus backwards compat",
			toml: `notify_name = "a"
[[repo]]
url = "git@github.com:o/r.git"
issues = [1]
`,
			wantMode: NotifyModeAgentbus,
			wantLbl:  "a",
		},
		{
			name: "standalone command without agentbus",
			toml: `notify_command = ["/usr/local/bin/notify.sh", "--tag", "ic"]
gh_bin = "~/bin/gh-throttle"
[[repo]]
url = "https://github.com/o/r"
query = "is:issue author:@me"
`,
			wantMode: NotifyModeCommand,
			wantLbl:  "/usr/local/bin/notify.sh",
		},
		{
			name: "standalone stdout mode",
			toml: `notify_mode = "stdout"
[[repo]]
url = "https://github.com/o/r"
issues = [42]
`,
			wantMode: NotifyModeStdout,
			wantLbl:  "stdout",
		},
		{
			name: "command with optional recipient",
			toml: `notify_mode = "command"
notify_name = "ops"
notify_command = ["bridge", "{recipient}", "{message}"]
[[repo]]
url = "https://github.com/o/r"
issues = [1]
`,
			wantMode: NotifyModeCommand,
			wantLbl:  "bridge",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg, err := ParseConfig([]byte(tc.toml))
			if err != nil {
				t.Fatalf("ParseConfig() = %v", err)
			}
			if got := cfg.EffectiveNotifyMode(); got != tc.wantMode {
				t.Errorf("EffectiveNotifyMode() = %q, want %q", got, tc.wantMode)
			}
			if got := cfg.NotifyTargetLabel(); got != tc.wantLbl {
				t.Errorf("NotifyTargetLabel() = %q, want %q", got, tc.wantLbl)
			}
		})
	}
}

func TestParseConfigErrors(t *testing.T) {
	tests := []struct {
		name string
		toml string
	}{
		{
			name: "no notifier configured",
			toml: "[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "no repo",
			toml: "notify_name='a'\n",
		},
		{
			name: "repo without issues or query",
			toml: "notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\n",
		},
		{
			name: "repo with whitespace query only",
			toml: "notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\nquery='   '\n",
		},
		{
			name: "non-positive issue number",
			toml: "notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[0]\n",
		},
		{
			name: "negative issue number",
			toml: "notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[-3]\n",
		},
		{
			name: "duplicate slug across different url formats",
			toml: "notify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n[[repo]]\nurl='git@github.com:o/r.git'\nissues=[2]\n",
		},
		{
			name: "url starting with dash",
			toml: "notify_name='a'\n[[repo]]\nurl='--upload-pack=evil/o/r'\nissues=[1]\n",
		},
		{
			name: "unknown top-level key",
			toml: "notify_name='a'\nbogus=true\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "unknown notify_mode",
			toml: "notify_mode='slack'\nnotify_name='a'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "command mode without command",
			toml: "notify_mode='command'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "empty notify_command",
			toml: "notify_command=[]\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "empty arg in notify_command",
			toml: "notify_command=['echo', '']\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "stdout mode with notify_command",
			toml: "notify_mode='stdout'\nnotify_command=['echo']\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "agentbus mode without notify_name",
			toml: "notify_mode='agentbus'\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "agentbus mode with notify_command",
			toml: "notify_mode='agentbus'\nnotify_name='a'\nnotify_command=['echo']\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "whitespace notify_name",
			toml: "notify_mode='stdout'\nnotify_name='   '\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
		{
			name: "explicit empty gh_bin",
			toml: "notify_mode='stdout'\ngh_bin=''\n[[repo]]\nurl='https://github.com/o/r'\nissues=[1]\n",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if _, err := ParseConfig([]byte(tc.toml)); err == nil {
				t.Errorf("ParseConfig() accepted invalid config:\n%s", tc.toml)
			}
		})
	}
}

func TestResolveGHBin(t *testing.T) {
	tests := []struct {
		name    string
		cfgBin  string
		env     []string
		want    string
		wantErr string
	}{
		{
			name: "default gh",
			env:  []string{"HOME=/home/u"},
			want: "gh",
		},
		{
			name: "GH_BIN env",
			env:  []string{"HOME=/home/u", "GH_BIN=/opt/bin/gh-throttle"},
			want: "/opt/bin/gh-throttle",
		},
		{
			name: "GH_BIN home expansion",
			env:  []string{"HOME=/home/u", "GH_BIN=~/bin/gh-throttle"},
			want: "/home/u/bin/gh-throttle",
		},
		{
			name:   "gh_bin overrides GH_BIN",
			cfgBin: "/usr/local/bin/gh-custom",
			env:    []string{"HOME=/home/u", "GH_BIN=/opt/bin/gh-throttle"},
			want:   "/usr/local/bin/gh-custom",
		},
		{
			name:   "gh_bin home expansion",
			cfgBin: "~/bin/gh-throttle",
			env:    []string{"HOME=/home/u"},
			want:   "/home/u/bin/gh-throttle",
		},
		{
			name:    "empty GH_BIN env is error",
			env:     []string{"HOME=/home/u", "GH_BIN="},
			wantErr: "GH_BIN",
		},
		{
			name:    "gh_bin home expansion without HOME",
			cfgBin:  "~/bin/gh-throttle",
			wantErr: "HOME is not set",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg := Config{GHBin: tc.cfgBin}
			got, err := cfg.ResolveGHBin(tc.env)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ResolveGHBin() err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ResolveGHBin() = %v", err)
			}
			if got != tc.want {
				t.Fatalf("ResolveGHBin() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestStatePersistenceEdgeCases(t *testing.T) {
	t.Run("missing file returns empty state", func(t *testing.T) {
		st, err := LoadState(filepath.Join(t.TempDir(), "missing.json"))
		if err != nil {
			t.Fatalf("LoadState() = %v", err)
		}
		if len(st.Repos) != 0 || !st.LastRun.IsZero() {
			t.Fatalf("got %+v, want empty", st)
		}
	})

	t.Run("corrupt json fails", func(t *testing.T) {
		p := filepath.Join(t.TempDir(), "state.json")
		if err := os.WriteFile(p, []byte("{not json"), 0o600); err != nil {
			t.Fatal(err)
		}
		if _, err := LoadState(p); err == nil {
			t.Fatal("expected error for corrupt json")
		}
	})

	t.Run("empty path fails", func(t *testing.T) {
		if _, err := LoadState(""); err == nil {
			t.Fatal("expected error for empty LoadState path")
		}
		if err := SaveState("", State{}); err == nil {
			t.Fatal("expected error for empty SaveState path")
		}
	})

	t.Run("atomic overwrite leaves no temp files and keeps mode 0600", func(t *testing.T) {
		dir := t.TempDir()
		p := filepath.Join(dir, "state.json")
		if err := SaveState(p, State{Repos: map[string]RepoState{"o/r": {Head: "aaa", Reported: []int{1}}}}); err != nil {
			t.Fatal(err)
		}
		if err := SaveState(p, State{Repos: map[string]RepoState{"o/r": {Head: "bbb", Reported: []int{1, 2}}}}); err != nil {
			t.Fatal(err)
		}
		info, err := os.Stat(p)
		if err != nil || info.Mode().Perm() != 0o600 {
			t.Fatalf("mode = %v err = %v", info, err)
		}
		got, err := LoadState(p)
		if err != nil || got.Repos["o/r"].Head != "bbb" || !got.Repos["o/r"].has(2) {
			t.Fatalf("got %+v err = %v", got, err)
		}
		entries, err := os.ReadDir(dir)
		if err != nil {
			t.Fatal(err)
		}
		if len(entries) != 1 || entries[0].Name() != "state.json" {
			t.Fatalf("unexpected files in state dir: %v", entries)
		}
	})
}
