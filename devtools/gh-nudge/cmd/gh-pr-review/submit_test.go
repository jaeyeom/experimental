package main

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/argparser"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/prreview"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

func TestMain(m *testing.M) {
	if pklExec := resolvePklExec(); pklExec != "" {
		_ = os.Setenv("PKL_EXEC", pklExec)
	}
	if pklCacheDir := resolvePklCacheDir(); pklCacheDir != "" {
		_ = os.Setenv("PKL_CACHE_DIR", pklCacheDir)
	}
	os.Exit(m.Run())
}

func resolvePklExec() string {
	if bin := os.Getenv("PKL_EXEC"); bin != "" {
		return bin
	}
	return resolveRunfile(filepath.Join("*pkl-cli-*", "file", "downloaded"), false)
}

func resolvePklCacheDir() string {
	if dir := os.Getenv("PKL_CACHE_DIR"); dir != "" {
		return dir
	}
	return resolveRunfile(filepath.Join("*pkl_project", "packages"), true)
}

func resolveRunfile(relGlob string, wantDir bool) string {
	for _, key := range []string{"RUNFILES_DIR", "TEST_SRCDIR"} {
		root := os.Getenv(key)
		if root == "" {
			continue
		}
		matches, err := filepath.Glob(filepath.Join(root, relGlob))
		if err != nil {
			continue
		}
		for _, candidate := range matches {
			info, err := os.Stat(candidate)
			if err != nil || info.IsDir() != wantDir {
				continue
			}
			if abs, err := filepath.Abs(candidate); err == nil {
				return abs
			}
			return candidate
		}
	}
	return ""
}

func schemaPath(t *testing.T) string {
	t.Helper()
	for _, candidate := range []string{
		"devtools/gh-nudge/pkl/Config.pkl",
		"../../pkl/Config.pkl",
	} {
		if _, err := os.Stat(candidate); err == nil {
			abs, err := filepath.Abs(candidate)
			if err != nil {
				t.Fatal(err)
			}
			return abs
		}
	}
	t.Fatal("Config.pkl not found")
	return ""
}

func TestParseSubmitFlagsViaOutboxBeforePositionals(t *testing.T) {
	args := []string{"--via-outbox", "acme/widgets", "123", "--owner", "agent-1", "--ref", "r-1"}
	detected, err := autoDetectArgsWithOptions(args, true, submitArgParserOptions())
	if err != nil {
		t.Fatalf("autoDetectArgsWithOptions() error = %v", err)
	}
	parser := argparser.NewArgParser(detected, submitArgParserOptions())
	if err := validateSubmitOptions(parser); err != nil {
		t.Fatalf("validateSubmitOptions() error = %v", err)
	}
	repo, prNum, err := parseSubmitArgs(parser)
	if err != nil {
		t.Fatalf("parseSubmitArgs() error = %v", err)
	}
	if repo.Owner != "acme" || repo.Name != "widgets" || prNum != 123 {
		t.Fatalf("parsed = %+v #%d, want acme/widgets #123", repo, prNum)
	}
	if !parser.GetBoolOption("via-outbox") {
		t.Fatal("via-outbox = false, want true")
	}
	if got := parser.GetOption("owner"); got != "agent-1" {
		t.Fatalf("owner = %q, want %q", got, "agent-1")
	}
	if got := parser.GetOption("ref"); got != "r-1" {
		t.Fatalf("ref = %q, want %q", got, "r-1")
	}
}

func TestBuildSubmitOptions_DefaultsAndPklConfig(t *testing.T) {
	t.Run("no flag and no config defaults off", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		parser := argparser.NewArgParser([]string{"acme/widgets", "42"}, submitArgParserOptions())
		opts, cmd, err := buildSubmitOptions(parser)
		if err != nil {
			t.Fatalf("buildSubmitOptions() error = %v", err)
		}
		if opts.ViaOutbox {
			t.Fatal("ViaOutbox = true, want false")
		}
		if opts.OutboxOwner != "" {
			t.Fatalf("OutboxOwner = %q, want empty", opts.OutboxOwner)
		}
		if strings.Join(cmd, " ") != "gh-outbox" {
			t.Fatalf("cmd = %v, want [gh-outbox]", cmd)
		}
	})

	t.Run("pkl config enables via_outbox and sets owner and outbox_command", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		schema := schemaPath(t)
		cfgDir := t.TempDir()
		cfgPath := filepath.Join(cfgDir, "config.pkl")

		fakeOutbox := filepath.Join(t.TempDir(), "fake-gh-outbox")
		script := "#!/usr/bin/env sh\necho 99\n"
		if err := os.WriteFile(fakeOutbox, []byte(script), 0o700); err != nil {
			t.Fatal(err)
		}

		content := fmt.Sprintf(`amends %q

github {
  repos { "widgets" }
}

review_post {
  via_outbox = true
  owner = "cfg-agent"
  outbox_command { %q }
}
`, schema, fakeOutbox)
		if err := os.WriteFile(cfgPath, []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}

		parser := argparser.NewArgParser([]string{"acme/widgets", "42", "--config", cfgPath}, submitArgParserOptions())
		opts, cmd, err := buildSubmitOptions(parser)
		if err != nil {
			t.Fatalf("buildSubmitOptions() error = %v", err)
		}
		if !opts.ViaOutbox || opts.OutboxOwner != "cfg-agent" {
			t.Fatalf("opts = %+v, want ViaOutbox=true OutboxOwner=cfg-agent", opts)
		}
		if len(cmd) != 1 || cmd[0] != fakeOutbox {
			t.Fatalf("cmd = %v, want [%s]", cmd, fakeOutbox)
		}

		// End-to-end submit using the Pkl-configured outbox command.
		storageHome := t.TempDir()
		ghStorage, err := storage.NewGitHubStorage(storageHome)
		if err != nil {
			t.Fatal(err)
		}
		repo := models.NewRepository("acme", "widgets")
		if err := ghStorage.AddComment(repo, models.NewPRTarget(42), models.Comment{
			Path: "main.go",
			Line: models.NewSingleLine(10),
			Body: "Use a helper here",
			Side: models.SideRight,
		}); err != nil {
			t.Fatal(err)
		}
		handler := prreview.NewCommandHandler(ghStorage, nil, nil, storageHome)
		handler.SetOutboxClient(prreview.NewCLIOutboxClient(cmd))
		var outBuf, errBuf bytes.Buffer
		handler.SetOutputWriters(&outBuf, &errBuf)
		if err := handler.SubmitCommandWithOptions(repo, 42, "", "", "", models.NewTextFormatter(), models.ClearAction{}, opts); err != nil {
			t.Fatalf("SubmitCommandWithOptions() error = %v", err)
		}
		if got := strings.TrimSpace(outBuf.String()); got != "99" {
			t.Fatalf("stdout = %q, want %q", got, "99")
		}
	})

	t.Run("explicit --via-outbox=false overrides config", func(t *testing.T) {
		t.Setenv("HOME", t.TempDir())
		schema := schemaPath(t)
		cfgPath := filepath.Join(t.TempDir(), "config.pkl")
		content := fmt.Sprintf(`amends %q

github {
  repos { "widgets" }
}

review_post {
  via_outbox = true
  owner = "cfg-agent"
}
`, schema)
		if err := os.WriteFile(cfgPath, []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
		parser := argparser.NewArgParser([]string{"acme/widgets", "42", "--config", cfgPath, "--via-outbox=false"}, submitArgParserOptions())
		opts, _, err := buildSubmitOptions(parser)
		if err != nil {
			t.Fatalf("buildSubmitOptions() error = %v", err)
		}
		if opts.ViaOutbox {
			t.Fatal("ViaOutbox = true, want false when --via-outbox=false is passed")
		}
	})
}
