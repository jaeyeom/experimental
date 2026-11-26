package doctor

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestPriorityInversionFinding(t *testing.T) {
	ten := jiraItem("PRJ-10", "Task", "To Do", "", false, nil)
	ten.Priority = 2
	eleven := jiraItem("PRJ-11", "Task", "To Do", "", false, nil, "PRJ-10")
	eleven.Priority = 2
	twenty := jiraItem("PRJ-20", "Task", "To Do", "", false, nil, "PRJ-11")
	parent := jiraItem("PRJ-100", "Task", "To Do", "", false, nil)
	parent.Priority = 2
	child := jiraItem("PRJ-101", "Task", "To Do", "PRJ-100", false, nil, "PRJ-100")
	done := jiraItem("PRJ-30", "Task", "Done", "", true, nil)
	done.Priority = 2
	urgentDone := jiraItem("PRJ-31", "Task", "Done", "", true, nil, "PRJ-30")
	in := inputWith(map[string]source.Item{
		ten.ID: ten, eleven.ID: eleven, twenty.ID: twenty,
		parent.ID: parent, child.ID: child, done.ID: done, urgentDone.ID: urgentDone,
	}, nil)
	got, err := Check(in)
	if err != nil {
		t.Fatal(err)
	}
	var lines []string
	for _, found := range got {
		if found.Check != CheckPriorityInversion {
			continue
		}
		if found.Severity != SeverityWarn || found.Suggestion != "factory priorities --apply" {
			t.Fatalf("finding = %+v", found)
		}
		lines = append(lines, found.Message)
	}
	want := []string{
		"PRJ-10  priority Medium (2)  blocks PRJ-20 (Highest, 0) via PRJ-10 → PRJ-11 → PRJ-20  suggest Highest",
		"PRJ-11  priority Medium (2)  blocks PRJ-20 (Highest, 0)  suggest Highest",
	}
	if strings.Join(lines, "\n") != strings.Join(want, "\n") {
		t.Fatalf("lines =\n%s", strings.Join(lines, "\n"))
	}
	text := FormatText(got)
	if !strings.Contains(text, "priority-inversion (2)\n") {
		t.Fatalf("text =\n%s", text)
	}
	for _, absent := range []string{"PRJ-100", "PRJ-101", "PRJ-30", "PRJ-31"} {
		if strings.Contains(strings.Join(lines, "\n"), absent) {
			t.Fatalf("listed %s\n%s", absent, strings.Join(lines, "\n"))
		}
	}
	in.Config.ParentBlocksChild = config.ParentBlocksHonor
	honored, err := Check(in)
	if err != nil {
		t.Fatal(err)
	}
	for _, found := range honored {
		if found.Check == CheckPriorityInversion && strings.Contains(found.Message, "PRJ-100") {
			t.Fatalf("honor listed the parent link: %s", found.Message)
		}
	}
}
