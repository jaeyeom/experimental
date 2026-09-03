package dispatch

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

func TestRenderAllVariables(t *testing.T) {
	t.Parallel()

	line := 42
	id := "PROJ-123"
	pr := scan.PR{
		Repo:       "acme/widgets",
		Number:     123,
		Title:      "[PROJ-123] Fix the widget",
		URL:        "https://github.com/acme/widgets/pull/123",
		Identifier: &id,
		BlockingComments: []scan.Comment{{
			Author: "reviewer-login",
			Path:   "src/widget.go",
			Line:   &line,
			URL:    "https://github.com/acme/widgets/pull/123#discussion_r1",
			Body:   "This should handle the nil case.",
		}},
	}
	tmpl := "{repo} {number} {url} {identifier} {title}\n{comments}"
	got := Render(tmpl, pr, config.Defaults())
	want := "acme/widgets 123 https://github.com/acme/widgets/pull/123 PROJ-123 [PROJ-123] Fix the widget\n" +
		"- src/widget.go:42 — reviewer-login: This should handle the nil case. (https://github.com/acme/widgets/pull/123#discussion_r1)"
	if got != want {
		t.Fatalf("Render() = %q\nwant %q", got, want)
	}
}

func TestRenderNullLine(t *testing.T) {
	t.Parallel()

	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "r",
			Path:   "src/widget.go",
			URL:    "u",
			Body:   "nits",
		}},
	}
	got := Render("{comments}", pr, config.Defaults())
	want := "- src/widget.go — r: nits (u)"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderEmptyPath(t *testing.T) {
	t.Parallel()

	line := 7
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "r",
			Path:   "",
			Line:   &line,
			URL:    "u",
			Body:   "overall",
		}},
	}
	got := Render("{comments}", pr, config.Defaults())
	want := "- (no path) — r: overall (u)"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderDefaultTemplate(t *testing.T) {
	t.Parallel()

	line := 42
	pr := fixtureEligiblePR()
	pr.BlockingComments[0].Line = &line
	got := Render(config.Defaults().PromptTemplate, pr, config.Defaults())
	if !strings.Contains(got, "PR #123 (https://github.com/acme/widgets/pull/123)") {
		t.Fatalf("missing number/url: %q", got)
	}
	if !strings.Contains(got, "Check out fix-widget") {
		t.Fatalf("missing checkout of head: %q", got)
	}
	if !strings.Contains(got, "gh pr checkout 123") {
		t.Fatalf("missing gh pr checkout: %q", got)
	}
	if !strings.Contains(got, "origin/main") {
		t.Fatalf("missing origin/base: %q", got)
	}
	if !strings.Contains(got, "on fix-widget at its latest tip") {
		t.Fatalf("missing latest-tip guard: %q", got)
	}
	idxCheckout := strings.Index(got, "Check out fix-widget")
	idxAddress := strings.Index(got, "Address the unresolved review comments")
	if idxCheckout < 0 || idxAddress < 0 || idxCheckout > idxAddress {
		t.Fatalf("branch-switch preamble not before comment body: %q", got)
	}
	if !strings.Contains(got, "- src/widget.go:42 — reviewer-login: This should handle the nil case. (https://github.com/acme/widgets/pull/123#discussion_r1)") {
		t.Fatalf("missing comments: %q", got)
	}
	if strings.Contains(got, "{") {
		t.Fatalf("unreplaced placeholder: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "ask the user") {
		t.Fatalf("missing ask-the-user: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "recommend") {
		t.Fatalf("missing recommended option: %q", got)
	}
	if strings.Contains(got, "understand the reviewer's ask, make the change") {
		t.Fatalf("still autonomous resolve: %q", got)
	}
	if !strings.Contains(got, "gh pr edit 123 --add-reviewer") {
		t.Fatalf("missing re-request reviewer: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "skip bots") {
		t.Fatalf("missing skip-bots: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "do not dismiss") {
		t.Fatalf("missing do-not-dismiss: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "unanswered threads") {
		t.Fatalf("missing unanswered-threads guard: %q", got)
	}
	idxPush := strings.Index(got, "Push after the mechanical threads")
	idxRerequest := strings.Index(got, "gh pr edit 123 --add-reviewer")
	if idxPush < 0 || idxRerequest < 0 || idxPush > idxRerequest {
		t.Fatalf("re-request not after push: %q", got)
	}
}

func TestRenderLongestKeyFirst(t *testing.T) {
	t.Parallel()

	id := "PROJ-123"
	pr := scan.PR{Identifier: &id, Number: 9}
	got := Render("{identifier} {number}", pr, config.Defaults())
	if got != "PROJ-123 9" {
		t.Fatalf("Render() = %q, want %q", got, "PROJ-123 9")
	}
}

func TestRenderNilIdentifier(t *testing.T) {
	t.Parallel()

	got := Render("id={identifier}.", scan.PR{}, config.Defaults())
	if got != "id=." {
		t.Fatalf("Render() = %q, want %q", got, "id=.")
	}
}

func TestRenderDefaultRebaseTemplate(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	got := Render(config.Defaults().RebasePromptTemplate, pr, config.Defaults())
	if !strings.Contains(got, "PR #123 (https://github.com/acme/widgets/pull/123)") {
		t.Fatalf("missing number/url: %q", got)
	}
	if !strings.Contains(got, "Check out fix-widget") {
		t.Fatalf("missing checkout: %q", got)
	}
	if !strings.Contains(got, "origin/main") {
		t.Fatalf("missing origin/base: %q", got)
	}
	if strings.Contains(got, "{") {
		t.Fatalf("unreplaced placeholder: %q", got)
	}
	if strings.Contains(got, "add-reviewer") {
		t.Fatalf("rebase template re-requests reviewer: %q", got)
	}
}

func TestRenderHeadAndBase(t *testing.T) {
	t.Parallel()

	pr := scan.PR{Head: "fix-widget", Base: "main"}
	got := Render("git switch {head}; rebase onto {base}", pr, config.Defaults())
	want := "git switch fix-widget; rebase onto main"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderUnknownPlaceholder(t *testing.T) {
	t.Parallel()

	got := Render("keep {foo} literal", scan.PR{Head: "fix-widget"}, config.Defaults())
	want := "keep {foo} literal"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderStripsOverlongLinkTargets(t *testing.T) {
	t.Parallel()

	line := 10
	longTarget := "https://example.test/open-in-editor?text=" + strings.Repeat("A", 1500)
	body := "Handle the nil pointer. See [docs](https://pkg.go.dev/fmt). Solve it in [vscode](" + longTarget + ")."
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "review-bot",
			Path:   "src/widget.go",
			Line:   &line,
			URL:    "https://github.com/acme/widgets/pull/1#discussion_r9",
			Body:   body,
		}},
	}
	got := Render("{comments}", pr, config.Defaults())
	if !strings.Contains(got, "Handle the nil pointer.") {
		t.Fatalf("missing prose: %q", got)
	}
	if !strings.Contains(got, "[docs](https://pkg.go.dev/fmt)") {
		t.Fatalf("short link not kept: %q", got)
	}
	if strings.Contains(got, longTarget) {
		t.Fatalf("long link target still present: %q", got)
	}
	if !strings.Contains(got, "vscode") {
		t.Fatalf("long link label dropped: %q", got)
	}
	if strings.Contains(got, "[vscode](") {
		t.Fatalf("long link not reduced to its label: %q", got)
	}
	if len(got) >= 1024 {
		t.Fatalf("rendered comments still oversized: %d bytes", len(got))
	}
}

func TestRenderCapsLongCommentBody(t *testing.T) {
	t.Parallel()

	line := 3
	thread := "https://github.com/acme/widgets/pull/1#discussion_r2"
	body := strings.Repeat("x", 5000)
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "r",
			Path:   "src/widget.go",
			Line:   &line,
			URL:    thread,
			Body:   body,
		}},
	}
	got := Render("{comments}", pr, config.Defaults())
	if strings.Contains(got, body) {
		t.Fatal("uncapped body still present")
	}
	wantMark := "… [truncated, see " + thread + "]"
	if !strings.Contains(got, wantMark) {
		t.Fatalf("missing truncation marker: %q", got)
	}
	if !strings.Contains(got, strings.Repeat("x", 4000)) {
		t.Fatalf("truncated prefix missing: %q", got)
	}
}

func TestRenderStripsLinksBeforeBodyCap(t *testing.T) {
	t.Parallel()

	line := 1
	longTarget := "https://example.test/?t=" + strings.Repeat("A", 3000)
	prose := strings.Repeat("ask ", 500)
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "b",
			Path:   "f.go",
			Line:   &line,
			URL:    "u",
			Body:   prose + "[vscode](" + longTarget + ")",
		}},
	}
	got := Render("{comments}", pr, config.Defaults())
	if strings.Contains(got, "truncated") {
		t.Fatalf("stripped body was still truncated: %q", got)
	}
	if !strings.Contains(got, prose) {
		t.Fatalf("prose missing: %q", got)
	}
	if strings.Contains(got, longTarget) {
		t.Fatalf("long target kept: %q", got)
	}
}

func TestRenderZeroLinkMaxKeepsLongTarget(t *testing.T) {
	t.Parallel()

	line := 10
	longTarget := "https://example.test/open-in-editor?text=" + strings.Repeat("A", 1500)
	cfg := config.Defaults()
	cfg.CommentLinkMaxChars = 0
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "b",
			Path:   "f.go",
			Line:   &line,
			URL:    "u",
			Body:   "See [vscode](" + longTarget + ").",
		}},
	}
	got := Render("{comments}", pr, cfg)
	if !strings.Contains(got, longTarget) {
		t.Fatalf("0 link max dropped target: %q", got)
	}
}

func TestRenderZeroBodyMaxKeepsLongBody(t *testing.T) {
	t.Parallel()

	line := 3
	body := strings.Repeat("x", 5000)
	cfg := config.Defaults()
	cfg.CommentBodyMaxChars = 0
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "r",
			Path:   "f.go",
			Line:   &line,
			URL:    "u",
			Body:   body,
		}},
	}
	got := Render("{comments}", pr, cfg)
	if !strings.Contains(got, body) {
		t.Fatalf("0 body max truncated: %q", got)
	}
	if strings.Contains(got, "truncated") {
		t.Fatalf("0 body max still marked truncated: %q", got)
	}
}
