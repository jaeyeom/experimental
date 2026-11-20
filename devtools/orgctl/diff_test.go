package orgctl

import (
	"strings"
	"testing"
)

func decodeSpec(t *testing.T, raw string) Spec {
	t.Helper()
	spec, err := Decode([]byte(raw))
	if err != nil {
		t.Fatal(err)
	}
	return spec
}

func diffText(t *testing.T, before, after string, full bool) string {
	t.Helper()
	var lines []string
	for _, c := range DiffSpecs(decodeSpec(t, before), decodeSpec(t, after)) {
		lines = append(lines, c.Lines(full)...)
	}
	return strings.Join(lines, "\n")
}

func TestDiffRolesAddedAndRemoved(t *testing.T) {
	got := diffText(t,
		`{"roles":{"review/lead":{},"review/legacy":{}}}`,
		`{"roles":{"review/lead":{},"review/triage":{}}}`, false)
	want := "- role review/legacy (removed)\n+ role review/triage (new)"
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestDiffScalarAndListFields(t *testing.T) {
	got := diffText(t,
		`{"roles":{"review/lead":{"reportsTo":"desk/head","model":"opus","escalates":["a","b"],"decides":["x","y"]}}}`,
		`{"roles":{"review/lead":{"reportsTo":"desk/chief","model":"opus","escalates":["b","new checks"],"decides":["y","x"]}}}`, false)
	want := strings.Join([]string{
		"~ review/lead: reportsTo desk/head -> desk/chief",
		`~ review/lead: + escalates "new checks"`,
		`~ review/lead: - escalates "a"`,
	}, "\n")
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestDiffReorderAloneIsNoChange(t *testing.T) {
	before := `{"roles":{"r":{"decides":["a","b"],"schedules":[{"id":"x","cron":"1"},{"id":"y","cron":"2"}]}},
		"rules":[{"id":"one","text":"1"},{"id":"two","text":"2"}]}`
	after := `{"roles":{"r":{"decides":["b","a"],"schedules":[{"id":"y","cron":"2"},{"id":"x","cron":"1"}]}},
		"rules":[{"id":"two","text":"2"},{"id":"one","text":"1"}]}`
	if changes := DiffSpecs(decodeSpec(t, before), decodeSpec(t, after)); len(changes) != 0 {
		t.Fatalf("reorder reported: %+v", changes)
	}
}

func TestDiffSchedulesByID(t *testing.T) {
	got := diffText(t,
		`{"roles":{"review/lead":{"schedules":[
			{"id":"nightly","cron":"0 2 * * *","prompt":"Check the queue and file a summary."},
			{"id":"before","cron":"5 * * * *"}]}}}`,
		`{"roles":{"review/lead":{"schedules":[
			{"id":"weekly","cron":"0 9 * * 1"},
			{"id":"nightly","cron":"0 3 * * *","prompt":"Check the review queue and file a short summary."}]}}}`, false)
	want := strings.Join([]string{
		`~ review/lead: + schedule "weekly"`,
		`~ review/lead: - schedule "before"`,
		`~ review/lead: schedule nightly cron "0 2 * * *" -> "0 3 * * *"`,
		"~ review/lead: schedule nightly prompt changed (+2 -0 words)",
		"    Check the {+review+} queue and file",
		"    and file a {+short+} summary.",
	}, "\n")
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestDiffOrgAndClassRules(t *testing.T) {
	got := diffText(t,
		`{"rules":[{"id":"keep","text":"Same."},{"id":"gone","text":"Old."},{"id":"edit","text":"Write plain words.","source":"handbook"}],
		  "classes":{"review":{"rules":[{"id":"cite","text":"Cite the line."},{"id":"drop","text":"x"}]}}}`,
		`{"rules":[{"id":"edit","text":"Write short plain words.","source":"guide"},{"id":"keep","text":"Same."},{"id":"new","text":"New."}],
		  "classes":{"review":{"rules":[{"id":"cite","text":"Cite the exact line."},{"id":"add","text":"y"}]}}}`, false)
	want := strings.Join([]string{
		"+ rule new (new)",
		"- rule gone (removed)",
		"~ rule edit: text changed (+1 -0 words)",
		"    Write {+short+} plain words.",
		"~ rule edit: source handbook -> guide",
		`~ class review: + rule "add"`,
		`~ class review: - rule "drop"`,
		"~ class review: rule cite text changed (+1 -0 words)",
		"    Cite the {+exact+} line.",
	}, "\n")
	if got != want {
		t.Fatalf("got\n%s\nwant\n%s", got, want)
	}
}

func TestDiffLongTextIsCappedUnlessFull(t *testing.T) {
	before := `{"roles":{"r":{"when":"a b c d e f g h i j k l m n o p q r s t u v w x y z"}}}`
	after := `{"roles":{"r":{"when":"a B c d e F g h i j K l m n o P q r s t U v w x y z"}}}`
	capped := diffText(t, before, after, false)
	lines := strings.Split(capped, "\n")
	if lines[0] != "~ r: when changed (+5 -5 words)" || len(lines) != 5 || !strings.Contains(lines[4], "2 more (--full shows all)") {
		t.Fatalf("capped:\n%s", capped)
	}
	full := strings.Split(diffText(t, before, after, true), "\n")
	if len(full) != 6 || !strings.Contains(full[5], "[-u-] {+U+}") {
		t.Fatalf("full:\n%s", strings.Join(full, "\n"))
	}
}
