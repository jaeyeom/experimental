package approve

import (
	"bytes"
	"errors"
	"os/exec"
	"reflect"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func TestConfiguredRepos(t *testing.T) {
	cfg := &config.Config{
		GitHub: config.GitHubConfig{
			Owner: "acme",
			Repos: []string{"widgets", "other/gadgets", "widgets", ""},
		},
	}
	got := ConfiguredRepos(cfg)
	want := []string{"acme/widgets", "other/gadgets"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("ConfiguredRepos() = %v, want %v", got, want)
	}
}

func TestParseOpenRef(t *testing.T) {
	tests := []struct {
		name        string
		raw         string
		repos       []string
		want        github.PullRef
		wantErr     bool
		wantInvalid bool
		wantContain []string
	}{
		{
			name:  "number with single repo",
			raw:   "123",
			repos: []string{"acme/widgets"},
			want:  github.PullRef{Owner: "acme", Repo: "widgets", Number: 123},
		},
		{
			name:        "number with several repos",
			raw:         "123",
			repos:       []string{"acme/widgets", "acme/gadgets"},
			wantErr:     true,
			wantInvalid: false,
			wantContain: []string{"acme/widgets", "acme/gadgets"},
		},
		{
			name:        "number with no repos",
			raw:         "123",
			repos:       nil,
			wantErr:     true,
			wantInvalid: false,
		},
		{
			name: "owner/repo#N",
			raw:  "acme/widgets#42",
			want: github.PullRef{Owner: "acme", Repo: "widgets", Number: 42},
		},
		{
			name: "pull request URL",
			raw:  "https://github.com/acme/widgets/pull/42",
			want: github.PullRef{Owner: "acme", Repo: "widgets", Number: 42},
		},
		{
			name: "URL with trailing /files and query",
			raw:  "https://github.com/acme/widgets/pull/42/files?w=1#diff-1",
			want: github.PullRef{Owner: "acme", Repo: "widgets", Number: 42},
		},
		{
			name:        "garbage",
			raw:         "not-a-pr",
			repos:       []string{"acme/widgets"},
			wantErr:     true,
			wantInvalid: true,
		},
		{
			name:        "zero",
			raw:         "0",
			repos:       []string{"acme/widgets"},
			wantErr:     true,
			wantInvalid: true,
		},
		{
			name:        "negative",
			raw:         "-5",
			repos:       []string{"acme/widgets"},
			wantErr:     true,
			wantInvalid: true,
		},
		{
			name:        "empty",
			raw:         "   ",
			repos:       []string{"acme/widgets"},
			wantErr:     true,
			wantInvalid: true,
		},
		{
			name:        "wrong host",
			raw:         "https://example.com/acme/widgets/pull/42",
			wantErr:     true,
			wantInvalid: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := ParseOpenRef(tc.raw, tc.repos)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("ParseOpenRef(%q) = %+v, want error", tc.raw, got)
				}
				if errors.Is(err, ErrInvalidPullRef) != tc.wantInvalid {
					t.Errorf("errors.Is(err, ErrInvalidPullRef) = %v, want %v (err=%v)", errors.Is(err, ErrInvalidPullRef), tc.wantInvalid, err)
				}
				for _, sub := range tc.wantContain {
					if !strings.Contains(err.Error(), sub) {
						t.Errorf("error %q missing %q", err.Error(), sub)
					}
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseOpenRef(%q) error = %v", tc.raw, err)
			}
			if got.Owner != tc.want.Owner || got.Repo != tc.want.Repo || got.Number != tc.want.Number {
				t.Errorf("ParseOpenRef(%q) = %+v, want %+v", tc.raw, got, tc.want)
			}
		})
	}
}

func TestOpenPrint(t *testing.T) {
	var out bytes.Buffer
	ran := false
	loaded := false
	err := Open(OpenOptions{
		Ref:   "https://github.com/acme/widgets/pull/99/files",
		Print: true,
		Out:   &out,
		LoadRepos: func() ([]string, error) {
			loaded = true
			return nil, errors.New("should not load config")
		},
		Run: func(string, ...string) error {
			ran = true
			return nil
		},
	})
	if err != nil {
		t.Fatalf("Open(--print) error = %v", err)
	}
	const want = "https://github.com/acme/widgets/pull/99\n"
	if out.String() != want {
		t.Fatalf("out = %q, want %q", out.String(), want)
	}
	if ran {
		t.Fatal("opener was invoked with --print")
	}
	if loaded {
		t.Fatal("LoadRepos was called for a URL ref")
	}
}

func TestOpenOpenerSelection(t *testing.T) {
	tests := []struct {
		name     string
		ref      string
		repos    []string
		ghBin    string
		ghFound  bool
		goos     string
		wantName string
		wantArgs []string
	}{
		{
			name:     "gh available",
			ref:      "acme/widgets#12",
			ghBin:    "gh",
			ghFound:  true,
			goos:     "darwin",
			wantName: "gh",
			wantArgs: []string{"browse", "12", "-R", "acme/widgets"},
		},
		{
			name:     "custom GH_BIN",
			ref:      "12",
			repos:    []string{"acme/widgets"},
			ghBin:    "/custom/gh",
			ghFound:  true,
			goos:     "linux",
			wantName: "/custom/gh",
			wantArgs: []string{"browse", "12", "-R", "acme/widgets"},
		},
		{
			name:     "fallback darwin open",
			ref:      "acme/widgets#12",
			ghBin:    "gh",
			ghFound:  false,
			goos:     "darwin",
			wantName: "open",
			wantArgs: []string{"https://github.com/acme/widgets/pull/12"},
		},
		{
			name:     "fallback linux xdg-open",
			ref:      "https://github.com/acme/widgets/pull/12/files",
			ghBin:    "gh",
			ghFound:  false,
			goos:     "linux",
			wantName: "xdg-open",
			wantArgs: []string{"https://github.com/acme/widgets/pull/12"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var gotName string
			var gotArgs []string
			err := Open(OpenOptions{
				Ref:   tc.ref,
				GHBin: tc.ghBin,
				GOOS:  tc.goos,
				LoadRepos: func() ([]string, error) {
					return tc.repos, nil
				},
				LookPath: func(file string) (string, error) {
					if tc.ghFound && file == tc.ghBin {
						return file, nil
					}
					return "", exec.ErrNotFound
				},
				Run: func(name string, args ...string) error {
					gotName = name
					gotArgs = append([]string(nil), args...)
					return nil
				},
			})
			if err != nil {
				t.Fatalf("Open() error = %v", err)
			}
			if gotName != tc.wantName || !reflect.DeepEqual(gotArgs, tc.wantArgs) {
				t.Errorf("ran %q %v, want %q %v", gotName, gotArgs, tc.wantName, tc.wantArgs)
			}
		})
	}
}

func TestOpenOpenerFailure(t *testing.T) {
	boom := errors.New("exit status 1")
	err := Open(OpenOptions{
		Ref:   "acme/widgets#12",
		GHBin: "gh",
		LookPath: func(string) (string, error) {
			return "/usr/bin/gh", nil
		},
		Run: func(string, ...string) error {
			return boom
		},
	})
	if !errors.Is(err, boom) {
		t.Fatalf("Open() error = %v, want wrapping %v", err, boom)
	}
}
