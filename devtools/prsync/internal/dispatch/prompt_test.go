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
	got := Render(tmpl, pr)
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
	got := Render("{comments}", pr)
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
	got := Render("{comments}", pr)
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
	got := Render(config.Defaults().PromptTemplate, pr)
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
}

func TestRenderLongestKeyFirst(t *testing.T) {
	t.Parallel()

	id := "PROJ-123"
	pr := scan.PR{Identifier: &id, Number: 9}
	got := Render("{identifier} {number}", pr)
	if got != "PROJ-123 9" {
		t.Fatalf("Render() = %q, want %q", got, "PROJ-123 9")
	}
}

func TestRenderNilIdentifier(t *testing.T) {
	t.Parallel()

	got := Render("id={identifier}.", scan.PR{})
	if got != "id=." {
		t.Fatalf("Render() = %q, want %q", got, "id=.")
	}
}

func TestRenderDefaultRebaseTemplate(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	got := Render(config.Defaults().RebasePromptTemplate, pr)
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
}

func TestRenderHeadAndBase(t *testing.T) {
	t.Parallel()

	pr := scan.PR{Head: "fix-widget", Base: "main"}
	got := Render("git switch {head}; rebase onto {base}", pr)
	want := "git switch fix-widget; rebase onto main"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderUnknownPlaceholder(t *testing.T) {
	t.Parallel()

	got := Render("keep {foo} literal", scan.PR{Head: "fix-widget"})
	want := "keep {foo} literal"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}
