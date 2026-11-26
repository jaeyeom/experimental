package main

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ghoutbox "github.com/jaeyeom/experimental/devtools/gh-outbox"
)

type testApp struct {
	app
	out, errOut *bytes.Buffer
	dir         string
}

func newTestApp(t *testing.T) *testApp {
	t.Helper()
	dir := t.TempDir()
	out, errOut := &bytes.Buffer{}, &bytes.Buffer{}
	return &testApp{
		app: app{
			stdout: out, stderr: errOut,
			env: []string{"HOME=" + dir},
			newOutbox: func(cfg ghoutbox.Config) *ghoutbox.Outbox {
				cfg.Lint.AgentbusNames = false
				return &ghoutbox.Outbox{Store: ghoutbox.NewStore(cfg.DataDir), Config: cfg}
			},
		},
		out: out, errOut: errOut, dir: dir,
	}
}

func (a *testApp) file(t *testing.T, name, text string) string {
	t.Helper()
	p := filepath.Join(a.dir, name)
	if err := os.WriteFile(p, []byte(text), 0o600); err != nil {
		t.Fatal(err)
	}
	return p
}

func TestSubmitBodyWithoutLineIsUsageError(t *testing.T) {
	a := newTestApp(t)
	body := a.file(t, "body.txt", "hello")
	code := a.run(context.Background(), []string{"submit", "--repo", "o/r", "--pr", "7", "--owner", "pm/x", "--body-file", body})
	if code != 2 || !strings.Contains(a.errOut.String(), "--line") || !strings.Contains(a.errOut.String(), "--kind") {
		t.Errorf("code = %d stderr = %q", code, a.errOut.String())
	}
}

func TestLintOnly(t *testing.T) {
	a := newTestApp(t)
	clean := a.file(t, "clean.txt", "Consider a test.")
	dirty := a.file(t, "dirty.txt", "ok\nsee D-12")
	if code := a.run(context.Background(), []string{"submit", "--lint-only", "--line", "a.go:3=" + clean}); code != 0 || a.out.String() != "clean\n" {
		t.Errorf("clean: code = %d out = %q err = %q", code, a.out.String(), a.errOut.String())
	}
	code := a.run(context.Background(), []string{"submit", "--lint-only", "--line", "a.go:3=" + clean, "--line", "b.go:9=" + dirty})
	if code != 1 || !strings.Contains(a.errOut.String(), `b.go:9: "D-12" (decide id)`) {
		t.Errorf("dirty: code = %d err = %q", code, a.errOut.String())
	}

	a.out.Reset()
	a.errOut.Reset()
	if code := a.run(context.Background(), []string{"submit", "--lint-only", "--kind", "reply", "--in-reply-to", "42", "--body-file", clean}); code != 0 || a.out.String() != "clean\n" {
		t.Errorf("reply clean: code = %d out = %q err = %q", code, a.out.String(), a.errOut.String())
	}
	if code := a.run(context.Background(), []string{"submit", "--lint-only", "--kind", "reply", "--in-reply-to", "42", "--body-file", dirty}); code != 1 || !strings.Contains(a.errOut.String(), `body line 2: "D-12"`) {
		t.Errorf("reply dirty: code = %d err = %q", code, a.errOut.String())
	}
}

func TestLsAndShowEmpty(t *testing.T) {
	a := newTestApp(t)
	if code := a.run(context.Background(), []string{"ls"}); code != 0 || !strings.HasPrefix(a.out.String(), "ID") {
		t.Errorf("ls: code = %d out = %q err = %q", code, a.out.String(), a.errOut.String())
	}
	if code := a.run(context.Background(), []string{"show", "3"}); code != 1 || !strings.Contains(a.errOut.String(), "no such draft") {
		t.Errorf("show: code = %d err = %q", code, a.errOut.String())
	}
}

func TestUnknownCommand(t *testing.T) {
	a := newTestApp(t)
	if code := a.run(context.Background(), []string{"post"}); code != 2 {
		t.Errorf("code = %d", code)
	}
}

func TestSubmitReplyFlags(t *testing.T) {
	a := newTestApp(t)
	body := a.file(t, "reply.txt", "Done.")
	code := a.run(context.Background(), []string{"submit", "--repo", "o/r", "--pr", "7", "--owner", "pm/x", "--kind", "reply", "--body-file", body})
	if code != 2 || !strings.Contains(a.errOut.String(), "--in-reply-to") {
		t.Errorf("missing --in-reply-to: code = %d stderr = %q", code, a.errOut.String())
	}

	a.errOut.Reset()
	code = a.run(context.Background(), []string{"submit", "--lint-only", "--kind", "reply", "--in-reply-to", "42", "--allow-resolved", "--body-file", body})
	if code != 0 || a.out.String() != "clean\n" {
		t.Errorf("allow-resolved reply lint: code = %d out = %q err = %q", code, a.out.String(), a.errOut.String())
	}
}
