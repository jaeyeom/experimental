package dispatch

import (
	"fmt"
	"regexp"
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
	if !strings.Contains(got, "State: open.") {
		t.Fatalf("missing pull request state: %q", got)
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
	if !strings.Contains(strings.ToLower(got), "without asking the user") {
		t.Fatalf("missing mechanical autonomy: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "reply on every listed thread") {
		t.Fatalf("missing reply-on-every-thread: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "unless the reply asks the reviewer a follow-up") {
		t.Fatalf("missing follow-up resolve exception: %q", got)
	}
	if !regexp.MustCompile(`(?is)do not edit, reply to the\s+reviewer, resolve, or push for that thread until an answer arrives\.`).MatchString(got) {
		t.Fatalf("missing hold-reply until the user answers: %q", got)
	}
	if strings.Count(got, config.Defaults().DispatchAskInstruction) != 1 {
		t.Fatalf("default ask instruction count = %d, want 1: %q", strings.Count(got, config.Defaults().DispatchAskInstruction), got)
	}
	if strings.Contains(strings.ToLower(got), "ask the user") {
		t.Fatalf("default prompt contains ask-the-user wording: %q", got)
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
	if !strings.Contains(got, "NEVER re-request anyone who has already approved") {
		t.Fatalf("missing skip-approvers guard: %q", got)
	}
	if !strings.Contains(got, "gh pr view 123 --json latestReviews") {
		t.Fatalf("missing approval-state check: %q", got)
	}
	idxPush := strings.Index(got, "Push after the mechanical threads")
	idxRerequest := strings.Index(got, "gh pr edit 123 --add-reviewer")
	if idxPush < 0 || idxRerequest < 0 || idxPush > idxRerequest {
		t.Fatalf("re-request not after push: %q", got)
	}
}

func TestRenderDispatchAskPlaceholders(t *testing.T) {
	cfg := config.Defaults()
	cfg.DispatchAskInstruction = "send the question to {ask_to} over the message bus"
	cfg.DispatchAskTo = "manager-7"
	if got := Render("{ask}", fixtureEligiblePR(), cfg); got != "send the question to manager-7 over the message bus" {
		t.Fatalf("render ask = %q", got)
	}
	if got := Render("custom {number} template", fixtureEligiblePR(), cfg); got != "custom 123 template" {
		t.Fatalf("template without ask changed unexpectedly: %q", got)
	}
	if got := Render("unknown {other}", fixtureEligiblePR(), cfg); got != "unknown {other}" {
		t.Fatalf("unknown placeholder changed: %q", got)
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

func TestRenderState(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		state string
		want  string
	}{
		{name: "open enum", state: "OPEN", want: "open"},
		{name: "closed", state: "closed", want: "closed"},
		{name: "merged", state: " MERGED ", want: "merged"},
		{name: "empty", state: "", want: ""},
		{name: "unrecognized", state: "DRAFT", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := Render("state={state}.", scan.PR{State: tc.state}, config.Defaults())
			want := "state=" + tc.want + "."
			if got != want {
				t.Fatalf("Render() = %q, want %q", got, want)
			}
		})
	}
}

func TestRenderDefaultRebaseTemplate(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	got := Render(config.Defaults().RebasePromptTemplate, pr, config.Defaults())
	if !strings.Contains(got, "PR #123 (https://github.com/acme/widgets/pull/123)") {
		t.Fatalf("missing number/url: %q", got)
	}
	if !strings.Contains(got, "State: open.") {
		t.Fatalf("missing pull request state: %q", got)
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

func TestRenderDefaultCIFixTemplate(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	got := Render(config.Defaults().CIFixPromptTemplate, pr, config.Defaults())
	if !strings.Contains(got, "PR #123 (https://github.com/acme/widgets/pull/123)") {
		t.Fatalf("missing number/url: %q", got)
	}
	if !strings.Contains(got, "State: open.") {
		t.Fatalf("missing pull request state: %q", got)
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
	idxFix := strings.Index(got, "Fix the failing CI")
	if idxCheckout < 0 || idxFix < 0 || idxCheckout > idxFix {
		t.Fatalf("branch-switch preamble not before CI-fix body: %q", got)
	}
	if strings.Contains(got, "{") {
		t.Fatalf("unreplaced placeholder: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "reproduce") {
		t.Fatalf("missing reproduce: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "flaky") {
		t.Fatalf("missing flake triage: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "ask the user") {
		t.Fatalf("missing ask-the-user: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "recommend") {
		t.Fatalf("missing recommended option: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "never push an unverified") {
		t.Fatalf("missing verify-before-push: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "do not touch") {
		t.Fatalf("missing stay-scoped: %q", got)
	}
	if !strings.Contains(got, "CI-log / flaky-test") {
		t.Fatalf("missing generic CI tooling: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "investigate from scratch") {
		t.Fatalf("empty hint should tell the agent to investigate from scratch: %q", got)
	}
	if strings.Contains(got, "add-reviewer") {
		t.Fatalf("CI-fix template re-requests reviewer: %q", got)
	}
	if strings.Contains(got, "unresolved review comments") {
		t.Fatalf("used comment template: %q", got)
	}
}

func TestRenderHintVerbatim(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	hint := "job X failed at target Y; key error: boom; looks real (not flake) {head}"
	got := RenderHint(config.Defaults().CIFixPromptTemplate, pr, config.Defaults(), hint)
	if !strings.Contains(got, hint) {
		t.Fatalf("hint not present verbatim: %q", got)
	}
	if !strings.Contains(strings.ToLower(got), "do not trust") {
		t.Fatalf("missing do-not-trust framing: %q", got)
	}
	if strings.Contains(got, "(none; investigate from scratch)") {
		t.Fatalf("used empty-hint fallback with a hint: %q", got)
	}
}

func TestRenderEmptyHintInvestigatesFromScratch(t *testing.T) {
	t.Parallel()

	got := RenderHint("hint={hint}.", scan.PR{}, config.Defaults(), "   ")
	want := "hint=(none; investigate from scratch)."
	if got != want {
		t.Fatalf("RenderHint() = %q, want %q", got, want)
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

func ExampleRender_stripsOverlongLinkTargets() {
	line := 10
	longTarget := "https://example.test/open-in-editor?text=" + strings.Repeat("A", 1500)
	pr := scan.PR{
		BlockingComments: []scan.Comment{{
			Author: "review-bot",
			Path:   "src/widget.go",
			Line:   &line,
			URL:    "https://github.com/acme/widgets/pull/1#discussion_r9",
			Body:   "Handle the nil pointer. See [docs](https://pkg.go.dev/fmt). Solve it in [vscode](" + longTarget + ").",
		}},
	}
	fmt.Println(Render("{comments}", pr, config.Defaults()))
	// Output:
	// - src/widget.go:10 — review-bot: Handle the nil pointer. See [docs](https://pkg.go.dev/fmt). Solve it in vscode. (https://github.com/acme/widgets/pull/1#discussion_r9)
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

func TestRenderNoStripPatternsLeavesBodyByteIdentical(t *testing.T) {
	t.Parallel()

	line := 10
	body := "Fix the nil check.\n\n\nReply with /apply.\n  "
	url := "https://github.com/acme/widgets/pull/1#discussion_r9"
	pr := commentPR(line, "review-bot", "src/widget.go", url, body)
	got := Render("{comments}", pr, config.Defaults())
	want := "- src/widget.go:10 — review-bot: " + body + " (" + url + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}

	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{}
	if gotEmpty := Render("{comments}", pr, cfg); gotEmpty != got {
		t.Fatalf("empty pattern list = %q, want %q", gotEmpty, got)
	}
}

func TestRenderUnmatchedStripPatternLeavesBodyByteIdentical(t *testing.T) {
	t.Parallel()

	line := 10
	body := "Fix the nil check.\n\n\nStill here.  "
	url := "https://github.com/acme/widgets/pull/1#discussion_r9"
	pr := commentPR(line, "review-bot", "src/widget.go", url, body)
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?m)^Powered by Bot$`)}
	got := Render("{comments}", pr, cfg)
	want := "- src/widget.go:10 — review-bot: " + body + " (" + url + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderStripsSingleLineBoilerplate(t *testing.T) {
	t.Parallel()

	line := 10
	url := "https://github.com/acme/widgets/pull/1#discussion_r9"
	pr := commentPR(line, "review-bot", "src/widget.go", url, "Fix the nil check.\n\nReply with /apply.")
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?m)^Reply with /apply\.$`)}
	got := Render("{comments}", pr, cfg)
	want := "- src/widget.go:10 — review-bot: Fix the nil check. (" + url + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderAppliesEveryStripPattern(t *testing.T) {
	t.Parallel()

	line := 1
	url := "u"
	body := "Fix it.\n\nReply with /apply.\n\nPowered by Bot"
	pr := commentPR(line, "b", "f.go", url, body)
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{
		regexp.MustCompile(`(?m)^Reply with /apply\.$`),
		regexp.MustCompile(`(?m)^Powered by Bot$`),
	}
	got := Render("{comments}", pr, cfg)
	want := "- f.go:1 — b: Fix it. (u)"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderStripsMultilineFooter(t *testing.T) {
	t.Parallel()

	line := 4
	url := "https://github.com/acme/widgets/pull/1#discussion_r3"
	body := "Use errors.Is.\n\n---\nPowered by Bot\nHow to apply: click the button."
	pr := commentPR(line, "review-bot", "src/widget.go", url, body)
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?s)\n---\n.*`)}
	got := Render("{comments}", pr, cfg)
	want := "- src/widget.go:4 — review-bot: Use errors.Is. (" + url + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
	if strings.Contains(got, "Powered by Bot") || strings.Contains(got, "How to apply") {
		t.Fatalf("footer still present: %q", got)
	}
}

func TestRenderEmptyStrippedBodyKeepsComment(t *testing.T) {
	t.Parallel()

	realURL := "https://github.com/acme/widgets/pull/1#discussion_r1"
	botURL := "https://github.com/acme/widgets/pull/1#discussion_r2"
	line := 8
	pr := scan.PR{
		BlockingComments: []scan.Comment{
			{
				Author: "reviewer",
				Path:   "src/widget.go",
				Line:   &line,
				URL:    realURL,
				Body:   "Fix the nil check.\n\nReply with /apply.",
			},
			{
				Author: "review-bot",
				Path:   "src/widget.go",
				URL:    botURL,
				Body:   "Reply with /apply.",
			},
		},
	}
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?m)^Reply with /apply\.$`)}
	got := Render("{comments}", pr, cfg)
	want := "- src/widget.go:8 — reviewer: Fix the nil check. (" + realURL + ")\n\n" +
		"- src/widget.go — review-bot: (boilerplate only; see " + botURL + ") (" + botURL + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderBlankLineBetweenUnaddressedThreads(t *testing.T) {
	t.Parallel()

	line1, line2, line3 := 1, 2, 3
	pr := scan.PR{
		Number: 7,
		URL:    "https://github.com/acme/widgets/pull/7",
		Head:   "fix",
		Base:   "main",
		BlockingComments: []scan.Comment{
			{Author: "alice", Path: "a.go", Line: &line1, URL: "https://example.test/1", Body: "first"},
			{Author: "bob", Path: "b.go", Line: &line2, URL: "https://example.test/2", Body: "second\nstill second"},
			{Author: "cara", Path: "c.go", Line: &line3, URL: "https://example.test/3", Body: "third"},
		},
	}
	got := Render(config.Defaults().PromptTemplate, pr, config.Defaults())
	const heading = "Unaddressed threads:\n"
	idx := strings.Index(got, heading)
	if idx < 0 {
		t.Fatalf("missing heading: %q", got)
	}
	want := strings.Join([]string{
		"- a.go:1 — alice: first (https://example.test/1)",
		"",
		"- b.go:2 — bob: second",
		"still second (https://example.test/2)",
		"",
		"- c.go:3 — cara: third (https://example.test/3)",
	}, "\n")
	rest := got[idx+len(heading):]
	if !strings.HasPrefix(rest, want+"\n") {
		t.Fatalf("threads after heading:\n%s\nwant prefix:\n%s", rest, want)
	}

	pr.BlockingComments = pr.BlockingComments[:1]
	one := Render("{comments}", pr, config.Defaults())
	if strings.Contains(one, "\n\n") {
		t.Fatalf("single thread has a blank line: %q", one)
	}
}

func TestRenderStripCollapsesNewlineRunsLeftBehind(t *testing.T) {
	t.Parallel()

	line := 1
	url := "u"
	body := "Keep.\n\n\n\nAlso.\n\n\nFOOTER"
	pr := commentPR(line, "b", "f.go", url, body)
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`(?m)^FOOTER$`)}
	got := Render("{comments}", pr, cfg)
	want := "- f.go:1 — b: Keep.\n\nAlso. (u)"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func TestRenderStripsBeforeLinkAndBodyCap(t *testing.T) {
	t.Parallel()

	line := 1
	url := "https://github.com/acme/widgets/pull/1#discussion_r4"
	footer := "Generated by ReviewBot. Apply with /apply."
	pr := commentPR(line, "review-bot", "f.go", url, "Please fix this bug.\n"+footer)
	cfg := config.Defaults()
	cfg.CommentBodyMaxChars = 30
	cfg.CommentStripPatterns = []*regexp.Regexp{
		regexp.MustCompile(`Generated by ReviewBot\. Apply with /apply\.`),
	}
	got := Render("{comments}", pr, cfg)
	if strings.Contains(got, "Generated") || strings.Contains(got, "truncated") {
		t.Fatalf("footer survived or cap ran first: %q", got)
	}
	if !strings.Contains(got, "Please fix this bug.") {
		t.Fatalf("prose missing: %q", got)
	}
}

func TestRenderStripLeavesAuthorPathAndURL(t *testing.T) {
	t.Parallel()

	line := 2
	url := "https://github.com/acme/widgets/pull/1#discussion_r5"
	pr := commentPR(line, "bot", "src/bot.go", url, "Fix the nil check.")
	cfg := config.Defaults()
	cfg.CommentStripPatterns = []*regexp.Regexp{regexp.MustCompile(`bot`)}
	got := Render("{comments}", pr, cfg)
	want := "- src/bot.go:2 — bot: Fix the nil check. (" + url + ")"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
}

func commentPR(line int, author, path, url, body string) scan.PR {
	return scan.PR{
		BlockingComments: []scan.Comment{{
			Author: author,
			Path:   path,
			Line:   &line,
			URL:    url,
			Body:   body,
		}},
	}
}

func TestRenderPromptLeaseInstructionOnlyWhenParallel(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	cfg := config.Defaults()
	serial := renderPrompt(cfg, Request{}, pr)
	if strings.Contains(serial, "agentbus lease claim") {
		t.Fatalf("serial prompt unexpectedly includes lease instruction: %q", serial)
	}

	cfg.DispatchMaxParallel = 3
	cfg.GateLease = "required"
	parallel := renderPrompt(cfg, Request{}, pr)
	if !strings.Contains(parallel, `agentbus lease claim --for 40m --why "PR #123"`) || !strings.Contains(parallel, "agentbus lease release") {
		t.Fatalf("parallel prompt missing default lease commands: %q", parallel)
	}
	if !strings.HasPrefix(parallel, "Shared checkout:") {
		t.Fatalf("lease instruction should lead the prompt, got: %q", parallel)
	}

	cfg.DispatchLeaseInstruction = "Run custom-lease take --pr {number} before write and custom-lease drop when finished."
	custom := renderPrompt(cfg, Request{}, pr)
	if !strings.HasPrefix(custom, "Run custom-lease take --pr 123 before write and custom-lease drop when finished.\n\n") {
		t.Fatalf("custom lease prompt = %q", custom)
	}
}

func TestRenderPromptCatchupAndRerequestMatrix(t *testing.T) {
	t.Parallel()

	pr := fixtureEligiblePR()
	pr.Actions = nil

	// Exact byte-for-byte baseline for today's defaults (rebase + on).
	wantDefaultComment := strings.Join([]string{
		"Get onto this PR's branch before editing:",
		"1. If the working tree has uncommitted changes, stash or commit them first.",
		"2. Check out fix-widget (if absent locally, run `gh pr checkout 123`).",
		"3. Fetch origin and bring fix-widget up to date with origin/main as needed.",
		"Do not start editing until you are on fix-widget at its latest tip.",
		"",
		"Address the unresolved review comments on PR #123 (https://github.com/acme/widgets/pull/123). State: open.",
		"",
		"Unaddressed threads:",
		"- src/widget.go:42 — reviewer-login: This should handle the nil case. (https://github.com/acme/widgets/pull/123#discussion_r1)",
		"",
		"Triage each thread before editing or pushing:",
		"- Mechanical / unambiguous (typo, nil check, rename, obvious bug): make the",
		"  change without asking the user, and continue.",
		"- Design decision, tradeoff, or disagreement: ask how to proceed through your configured channel, with a recommended option, and do not wait silently in your own screen; keep working on the other threads. Do not edit, reply to the",
		"  reviewer, resolve, or push for that thread until an answer arrives.",
		"",
		"Push after the mechanical threads are done and any design questions have an",
		"answer.",
		"",
		"Reply on every listed thread once its outcome is known: right away for a",
		"mechanical thread, and after the answer arrives for any other. Post the reply",
		"on that review thread, not as a new top-level PR comment. Then mark the",
		"thread resolved, unless the reply asks the reviewer a follow-up question.",
		"Leave a follow-up open so the reviewer can answer. A reply that closes the",
		"point (change made, already done, or declined) must be resolved.",
		"",
		"After those threads are handled and pushed, re-request review only from the",
		"human authors of the threads listed above, once all of their listed comments",
		"are addressed:",
		"  gh pr edit 123 --add-reviewer <login>",
		"NEVER re-request anyone who has already approved: check",
		"`gh pr view 123 --json latestReviews` first and skip every login whose",
		"state is APPROVED, even if you addressed their threads. Also skip bots, the",
		"PR author, anyone already pending, and anyone who still has",
		"unanswered threads. If nobody is left, request nobody. Do not dismiss reviews.",
		"",
		"Do not touch other PRs.",
	}, "\n")
	if got := renderPrompt(config.Defaults(), Request{}, pr); got != wantDefaultComment {
		t.Fatalf("default comment prompt mismatch:\ngot:\n%s\nwant:\n%s", got, wantDefaultComment)
	}

	wantDefaultCIFix := strings.Join([]string{
		"Get onto this PR's branch before editing:",
		"1. If the working tree has uncommitted changes, stash or commit them first.",
		"2. Check out fix-widget (if absent locally, run `gh pr checkout 123`).",
		"3. Fetch origin and bring fix-widget up to date with origin/main as needed.",
		"Do not start editing until you are on fix-widget at its latest tip.",
		"",
		"Fix the failing CI on PR #123 (https://github.com/acme/widgets/pull/123). State: open. Stay scoped to this PR.",
		"Do not touch unrelated failures or other PRs.",
		"",
		"Caller hint (non-authoritative starting point; verify against live CI",
		"state, do not trust — CI may have re-run, e.g. after a rebase, and the",
		"failure may have changed):",
		"check X failed",
		"",
		"Use the repo's available CI-log / flaky-test investigation tools if any.",
		"Do not assume a particular CI provider or build system.",
		"",
		"1. Reproduce before touching anything. Run the failing check locally on",
		"   this branch and confirm it actually fails before proposing a change.",
		"2. Triage before fixing. Not every red check is a code bug:",
		"   - a flaky or infrastructure failure → do not \"fix\" it; report back that",
		"     it is not a code problem and stop.",
		"   - a genuine failure with an unambiguous fix → fix it, verify locally,",
		"     push.",
		"   - a failure whose fix is a judgment call (a design tradeoff, or one",
		"     that needs new tests written to a scope that isn't obvious) → pause",
		"     and ask the user, with a recommended option, before editing or",
		"     pushing.",
		"3. Verify locally before pushing. Re-run the same check and confirm it",
		"   now passes; never push an unverified fix.",
	}, "\n")
	if got := renderPrompt(config.Defaults(), Request{CIFix: true, Hint: "check X failed"}, pr); got != wantDefaultCIFix {
		t.Fatalf("default CI-fix prompt mismatch:\ngot:\n%s\nwant:\n%s", got, wantDefaultCIFix)
	}

	mqPR := pr
	mqPR.Actions = []scan.Action{{Name: scan.ActionMergeQueueFix, CommentURL: "https://example.com/rca"}}
	wantDefaultMQ := strings.Join([]string{
		"The merge queue ejected PR #123 (https://github.com/acme/widgets/pull/123). State: open.",
		"Read the RCA comment and fix the cause:",
		"https://example.com/rca",
		"",
		"When the ejection is a merge conflict, rebase onto origin/main, regenerate lockfiles, and push.",
		"",
		"Do not touch other PRs.",
	}, "\n")
	if got := renderPrompt(config.Defaults(), Request{}, mqPR); got != wantDefaultMQ {
		t.Fatalf("default merge-queue-fix prompt mismatch:\ngot:\n%s\nwant:\n%s", got, wantDefaultMQ)
	}

	for _, catchup := range []string{"rebase", "merge"} {
		for _, rerequest := range []string{"on", "off"} {
			t.Run("catchup="+catchup+"/rerequest="+rerequest, func(t *testing.T) {
				t.Parallel()
				cfg := config.Defaults()
				cfg.DispatchCatchup = catchup
				cfg.DispatchRerequest = rerequest

				comment := renderPrompt(cfg, Request{}, pr)
				ciFix := renderPrompt(cfg, Request{CIFix: true, Hint: "check X failed"}, pr)
				mqFix := renderPrompt(cfg, Request{}, mqPR)

				if catchup == "merge" {
					for name, p := range map[string]string{"comment": comment, "ci-fix": ciFix, "merge-queue-fix": mqFix} {
						if !strings.Contains(p, "merge origin/main into fix-widget") {
							t.Errorf("%s prompt missing merge instruction:\n%s", name, p)
						}
						if !strings.Contains(p, "push normally") || !strings.Contains(strings.ToLower(p), "force-push") {
							t.Errorf("%s prompt missing push-normally / no-force-push:\n%s", name, p)
						}
						if strings.Contains(p, "bring fix-widget up to date with origin/main as needed") {
							t.Errorf("%s prompt kept generic catch-up preamble:\n%s", name, p)
						}
					}
				} else {
					if comment != renderPrompt(config.Defaults(), Request{}, pr) && rerequest == "on" {
						t.Errorf("rebase+on comment differs from default:\n%s", comment)
					}
					if ciFix != wantDefaultCIFix {
						t.Errorf("rebase CI-fix differs from default:\n%s", ciFix)
					}
					if mqFix != wantDefaultMQ {
						t.Errorf("rebase merge-queue-fix differs from default:\n%s", mqFix)
					}
				}

				if rerequest == "off" {
					if strings.Contains(comment, "add-reviewer") || strings.Contains(comment, "re-request") || strings.Contains(comment, "latestReviews") {
						t.Errorf("comment prompt still asks to re-request reviewers:\n%s", comment)
					}
					if !strings.Contains(comment, "another tool handles reviewer requests") {
						t.Errorf("comment prompt missing other-tool reviewer note:\n%s", comment)
					}
					for name, p := range map[string]string{"ci-fix": ciFix, "merge-queue-fix": mqFix} {
						if strings.Contains(p, "add-reviewer") || strings.Contains(p, "re-request") {
							t.Errorf("%s prompt mentions reviewer requests:\n%s", name, p)
						}
					}
				} else if !strings.Contains(comment, "gh pr edit 123 --add-reviewer") {
					t.Errorf("rerequest=on comment prompt missing add-reviewer:\n%s", comment)
				}
			})
		}
	}

	// Custom templates must not be rewritten by dispatch_catchup or dispatch_rerequest.
	cfg := config.Defaults()
	cfg.DispatchCatchup = "merge"
	cfg.DispatchRerequest = "off"
	cfg.PromptTemplate = "custom comment {head} {base}"
	cfg.CIFixPromptTemplate = "custom ci {head} {base}"
	if got := renderPrompt(cfg, Request{}, pr); got != "custom comment fix-widget main" {
		t.Fatalf("custom comment prompt = %q", got)
	}
	if got := renderPrompt(cfg, Request{CIFix: true}, pr); got != "custom ci fix-widget main" {
		t.Fatalf("custom ci-fix prompt = %q", got)
	}
}
