package approve

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestStatusCellPerAction(t *testing.T) {
	for _, tc := range []struct {
		action string
		want   string
	}{
		{ActionApproved, "✅ approved"},
		{ActionWouldApprove, "✅ approved (dry run)"},
		{ActionHandoff, "🤝 handed off"},
		{ActionWouldHandoff, "🤝 handed off (dry run)"},
		{ActionManual, "👀 left for you"},
		{ActionMarkViewed, "👀 left for you"},
		{ActionWouldMarkViewed, "👀 left for you (dry run)"},
		{ActionSkipped, "⏭️ skipped"},
		{ActionError, "❌ error"},
	} {
		got := statusCell(tc.action, false)
		if strings.TrimRight(got, " ") != tc.want {
			t.Errorf("statusCell(%q) = %q, want %q", tc.action, got, tc.want)
		}
		if _, word, _ := strings.Cut(got, " "); len(word) != len("left for you (dry run)") {
			t.Errorf("statusCell(%q) = %q is not padded to the column width", tc.action, got)
		}
	}
}

func TestStatusCellColorsOnlyMarkerAndWord(t *testing.T) {
	for _, tc := range []struct {
		action string
		want   string
	}{
		{ActionApproved, "\x1b[32m✅ approved\x1b[0m"},
		{ActionWouldApprove, "\x1b[32m✅ approved (dry run)\x1b[0m"},
		{ActionHandoff, "\x1b[36m🤝 handed off\x1b[0m"},
		{ActionManual, "\x1b[33m👀 left for you\x1b[0m"},
		{ActionSkipped, "\x1b[2m⏭️ skipped\x1b[0m"},
		{ActionError, "\x1b[31m❌ error\x1b[0m"},
	} {
		got := statusCell(tc.action, true)
		if strings.TrimRight(got, " ") != tc.want {
			t.Errorf("statusCell(%q, color) = %q, want %q", tc.action, got, tc.want)
		}
	}
}

func TestUseColor(t *testing.T) {
	file, err := os.Create(filepath.Join(t.TempDir(), "out"))
	if err != nil {
		t.Fatal(err)
	}
	defer file.Close()
	var buf bytes.Buffer
	for _, tc := range []struct {
		name    string
		mode    string
		noColor string
		want    bool
	}{
		{"never", ColorNever, "", false},
		{"always", ColorAlways, "", true},
		{"always with NO_COLOR", ColorAlways, "1", false},
		{"auto with NO_COLOR", ColorAuto, "1", false},
		{"auto to a buffer", ColorAuto, "", false},
	} {
		got, err := UseColor(tc.mode, &buf, tc.noColor)
		if err != nil || got != tc.want {
			t.Errorf("%s: UseColor() = %v, %v, want %v", tc.name, got, err, tc.want)
		}
	}
	if got, err := UseColor(ColorAuto, file, ""); err != nil || got {
		t.Errorf("auto to a regular file: UseColor() = %v, %v, want false", got, err)
	}
	if _, err := UseColor("sometimes", &buf, ""); err == nil {
		t.Error("UseColor(sometimes) error = nil, want an error")
	}
}

func TestWriteSummaryGroupsInOrderWithCounts(t *testing.T) {
	rule := "dashboard"
	entries := []LogEntry{
		{PR: "acme/widgets#1", Title: "One", Action: ActionSkipped, Reason: "pull request is closed"},
		{PR: "acme/widgets#2", Title: "Two", Action: ActionApproved, Rule: &rule},
		{PR: "acme/widgets#3", Title: "Three", Action: ActionManual, Rules: []RuleResult{{
			Rule: "dashboard", Conditions: []ConditionResult{{Condition: "paths_all_match", Pass: false, Saw: "infra/main.tf"}},
		}}},
		{PR: "acme/widgets#4", Action: ActionError, Reason: "gh: HTTP 502"},
		{PR: "acme/widgets#5", Title: "Five", Action: ActionWouldMarkViewed, Paths: []string{"a"}},
	}
	var out bytes.Buffer
	if err := writeSummary(&out, entries, false); err != nil {
		t.Fatal(err)
	}
	want := "\n" +
		"👀 Left for you (2)\n" +
		"  acme/widgets#3  Three - dashboard: paths_all_match failed (infra/main.tf)\n" +
		"  acme/widgets#5  Five (dry run) - would_mark_viewed: 1 files\n" +
		"  1 not evaluated (load failed); rerun to check them\n" +
		"\n" +
		"✅ Approved (1)\n" +
		"  acme/widgets#2  Two\n" +
		"\n" +
		"❌ Errors (1)\n" +
		"  acme/widgets#4 - gh: HTTP 502\n" +
		"\n" +
		"⏭️ Skipped (1)\n" +
		"  acme/widgets#1  One - pull request is closed\n"
	if out.String() != want {
		t.Fatalf("summary =\n%s\nwant\n%s", out.String(), want)
	}
	if strings.Contains(out.String(), "Handed off") {
		t.Fatal("summary lists the empty handed off group")
	}
}

func TestWriteSummaryColorsOnlyHeadingMarkerAndWord(t *testing.T) {
	var out bytes.Buffer
	entries := []LogEntry{{PR: "acme/widgets#1", Title: "One", Action: ActionHandoff, Reviewers: []string{"bob"}}}
	if err := writeSummary(&out, entries, true); err != nil {
		t.Fatal(err)
	}
	want := "\n\x1b[36m🤝 Handed off\x1b[0m (1)\n  acme/widgets#1  One - reviewers: bob\n"
	if out.String() != want {
		t.Fatalf("summary = %q, want %q", out.String(), want)
	}
}

func TestWriteSummaryEmpty(t *testing.T) {
	var out bytes.Buffer
	if err := writeSummary(&out, nil, true); err != nil {
		t.Fatal(err)
	}
	if out.Len() != 0 {
		t.Fatalf("summary = %q, want nothing", out.String())
	}
}

func TestWriteSummaryErrorsOnlyShowsZeroLeftForYou(t *testing.T) {
	entries := []LogEntry{
		{PR: "acme/widgets#1", Action: ActionError, Reason: "gh: HTTP 504"},
		{PR: "acme/widgets#2", Action: ActionError, Reason: "gh: HTTP 504"},
	}
	var out bytes.Buffer
	if err := writeSummary(&out, entries, false); err != nil {
		t.Fatal(err)
	}
	want := "\n" +
		"👀 Left for you (0)\n" +
		"  2 not evaluated (load failed); rerun to check them\n" +
		"\n" +
		"❌ Errors (2)\n" +
		"  acme/widgets#1 - gh: HTTP 504\n" +
		"  acme/widgets#2 - gh: HTTP 504\n"
	if out.String() != want {
		t.Fatalf("summary =\n%s\nwant\n%s", out.String(), want)
	}
}

// displayRun evaluates one approvable and one manual pull request.
func displayRun(t *testing.T, opts Options) runResult {
	t.Helper()
	manual := prNumber(2)
	manual.Files = append(manual.Files, models.File{Path: "infra/main.tf"})
	return run(t, newFake(prNumber(1), manual), []config.ApproveRule{dashboardRule}, opts)
}

func TestRunRowsAndSummary(t *testing.T) {
	res := displayRun(t, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Contains(res.out, "\x1b") {
		t.Fatalf("uncolored run has escape codes: %q", res.out)
	}
	lines := strings.Split(res.out, "\n")
	if !strings.HasPrefix(lines[1], "✅ approved (dry run)") || !strings.Contains(lines[1], "acme/widgets#1") {
		t.Fatalf("first row = %q", lines[1])
	}
	_, summary, ok := strings.Cut(res.out, "\n\n")
	if !ok {
		t.Fatalf("no summary:\n%s", res.out)
	}
	left, approved := strings.Index(summary, "👀 Left for you (1)"), strings.Index(summary, "✅ Approved (1)")
	if left < 0 || approved < 0 || left > approved {
		t.Fatalf("summary =\n%s", summary)
	}
	if !strings.Contains(summary, "acme/widgets#2  Tidy dashboard - dashboard: paths_all_match failed") ||
		!strings.Contains(summary, "acme/widgets#1  Tidy dashboard (dry run)\n") {
		t.Fatalf("summary =\n%s", summary)
	}
}

func TestRunApplyHasNoDryRunMarker(t *testing.T) {
	res := run(t, newFake(prNumber(1)), []config.ApproveRule{dashboardRule}, Options{Apply: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if strings.Contains(res.out, "(dry run)") || !strings.Contains(res.out, "✅ approved ") {
		t.Fatalf("out = %s", res.out)
	}
}

func TestRunColorOnlyMarkerAndWord(t *testing.T) {
	res := displayRun(t, Options{Color: true, Explain: true})
	if res.err != nil {
		t.Fatal(res.err)
	}
	plain := res.out
	for _, colored := range []string{
		"\x1b[32m✅ approved (dry run)\x1b[0m",
		"\x1b[33m👀 left for you\x1b[0m",
		"\x1b[32m✅ Approved\x1b[0m",
		"\x1b[33m👀 Left for you\x1b[0m",
	} {
		if !strings.Contains(plain, colored) {
			t.Fatalf("out missing %q:\n%q", colored, res.out)
		}
		plain = strings.ReplaceAll(plain, colored, "")
	}
	if strings.Contains(plain, "\x1b") {
		t.Fatalf("escape codes outside the marker and word:\n%q", plain)
	}
}

func TestRunAuditLogUnchangedByDisplay(t *testing.T) {
	logOf := func(opts Options) string {
		var out, logBuf bytes.Buffer
		manual := prNumber(2)
		manual.Files = append(manual.Files, models.File{Path: "infra/main.tf"})
		runner := &Runner{
			Client: newFake(prNumber(1), manual),
			Config: config.ApproveConfig{Body: "LGTM", Rules: []config.ApproveRule{dashboardRule}},
			Out:    &out,
			Log:    &logBuf,
			Now:    fixedNow,
		}
		if err := runner.Run(opts); err != nil {
			t.Fatal(err)
		}
		return logBuf.String()
	}
	// want is the log the runner wrote before rows had markers and a summary.
	const want = `{"time":"2026-11-11T09:00:00Z","pr":"acme/widgets#1","url":"https://github.com/acme/widgets/pull/1","title":"Tidy dashboard","author":"alice","headSha":"h1","rule":"dashboard","rules":[{"rule":"dashboard","matched":true,"conditions":[{"condition":"paths_all_match","pass":true,"saw":"all 1 files match"},{"condition":"not_draft","pass":true,"saw":"ready for review"},{"condition":"max_unresolved_threads","pass":true,"saw":"0 unresolved threads (max 0)"}]}],"action":"would_approve"}` + "\n" +
		`{"time":"2026-11-11T09:00:00Z","pr":"acme/widgets#2","url":"https://github.com/acme/widgets/pull/1","title":"Tidy dashboard","author":"alice","headSha":"h1","rule":null,"rules":[{"rule":"dashboard","matched":false,"conditions":[{"condition":"paths_all_match","pass":false,"saw":"outside globs: infra/main.tf"},{"condition":"not_draft","pass":true,"saw":"ready for review"},{"condition":"max_unresolved_threads","pass":true,"saw":"0 unresolved threads (max 0)"}]}],"action":"manual"}` + "\n"
	for _, opts := range []Options{{}, {Color: true}, {Color: true, Explain: true}} {
		if got := logOf(opts); got != want {
			t.Fatalf("audit log with %+v =\n%s\nwant\n%s", opts, got, want)
		}
	}
}
