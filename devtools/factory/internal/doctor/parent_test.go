package doctor

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestDoctorParentAndChildBlocks(t *testing.T) {
	t.Parallel()

	for _, mode := range []string{config.ParentBlocksIgnore, config.ParentBlocksHonor} {
		t.Run(mode, func(t *testing.T) {
			t.Parallel()
			in := baseInput(parentChildItems())
			in.Config.ParentBlocksChild = mode
			verb := "ignored"
			if mode == config.ParentBlocksHonor {
				verb = "honored"
			}
			want := []Finding{
				parentFinding(verb, "PRJ-100", "PRJ-101", true),
				parentFinding(verb, "PRJ-100", "PRJ-102", true),
				parentFinding(verb, "PRJ-100", "PRJ-103", true),
				parentFinding(verb, "PRJ-100", "PRJ-105", true),
				parentFinding(verb, "PRJ-100", "PRJ-110", false),
				parentFinding(verb, "PRJ-DONE", "PRJ-104", true),
				childFinding("PRJ-120", "PRJ-100", true),
				childFinding("PRJ-130", "PRJ-100", false),
			}
			equalFindings(t, mustCheck(t, in), want)
			if parentFinding(verb, "PRJ-100", "PRJ-105", true).Message != verb+": PRJ-100 is the parent of PRJ-105; a parent blocking its own child is usually a linking mistake" {
				t.Fatalf("note = %q", parentFinding(verb, "PRJ-100", "PRJ-105", true).Message)
			}
		})
	}
}

func TestDoctorParentBlockCycle(t *testing.T) {
	t.Parallel()

	ready := []string{"agent-ready"}
	items := map[string]source.Item{
		"CYC-A": jiraItem("CYC-A", "Task", "To Do", "CYC-B", false, ready, "CYC-B", "CYC-C"),
		"CYC-B": jiraItem("CYC-B", "Task", "To Do", "CYC-C", false, ready),
		"CYC-C": jiraItem("CYC-C", "Task", "To Do", "CYC-B", false, ready),
	}
	want := []Finding{
		parentFinding("ignored", "CYC-B", "CYC-A", true),
		parentFinding("ignored", "CYC-C", "CYC-A", false),
	}
	equalFindings(t, mustCheck(t, baseInput(items)), want)
}

func TestDoctorParentBlockUnmirrored(t *testing.T) {
	t.Parallel()

	ready := []string{"agent-ready"}
	items := map[string]source.Item{
		"PRJ-EPIC": jiraItem("PRJ-EPIC", "Epic", "To Do", "", false, nil),
		"PRJ-200":  jiraItem("PRJ-200", "Task", "To Do", "STORY", false, ready, "STORY"),
		"PRJ-201":  jiraItem("PRJ-201", "Task", "To Do", "STORY", false, ready, "PRJ-EPIC"),
	}
	want := []Finding{
		finding(CheckUnknownBlocker, SeverityWarn, "PRJ-200 is blocked by STORY, which is not in the mirror", "widen jql, or unlink", "PRJ-200", "STORY"),
		parentFinding("ignored", "STORY", "PRJ-200", true),
	}
	equalFindings(t, mustCheck(t, baseInput(items)), want)
}

func TestDoctorGitHubSubIssueBlocks(t *testing.T) {
	t.Parallel()

	const (
		parent = "acme/widgets#10"
		child  = "acme/widgets#11"
		grand  = "acme/widgets#12"
	)
	labels := []string{"agent-ready"}
	items := map[string]source.Item{
		parent: {ID: parent, Type: "Issue", State: "OPEN", Labels: labels, BlockedBy: []source.BlockRef{{ID: child}}},
		child:  {ID: child, Type: "Issue", State: "OPEN", Parent: parent, Labels: labels, BlockedBy: []source.BlockRef{{ID: parent}}},
		grand:  {ID: grand, Type: "Issue", State: "OPEN", Parent: child, Labels: labels, BlockedBy: []source.BlockRef{{ID: parent}}},
	}
	want := []Finding{
		cycleWant(parent, child, parent),
		parentFinding("ignored", parent, child, true),
		parentFinding("ignored", parent, grand, false),
		childFinding(child, parent, true),
	}
	equalFindings(t, mustCheck(t, githubInput(items)), want)
}

func TestDoctorParentBlocksModeRejected(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"EPIC": jiraItem("EPIC", "Epic", "To Do", "", false, nil),
	}
	tests := []struct {
		name string
		mode string
		want string
	}{
		{name: "unknown", mode: "drop", want: `found "drop"`},
		{name: "empty", mode: "", want: "found empty"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			in := baseInput(items)
			in.Config.ParentBlocksChild = tc.mode
			_, err := Check(in)
			if err == nil {
				t.Fatal("Check error = nil")
			}
			for _, part := range []string{"parent_blocks_child", "expected", "ignore", "honor", tc.want} {
				if !strings.Contains(err.Error(), part) {
					t.Fatalf("Check error = %q, want %q", err.Error(), part)
				}
			}
		})
	}
}

func parentChildItems() map[string]source.Item {
	ready := []string{"agent-ready"}
	return map[string]source.Item{
		"PRJ-100":  jiraItem("PRJ-100", "Epic", "To Do", "", false, nil, "PRJ-120", "PRJ-130"),
		"PRJ-101":  jiraItem("PRJ-101", "Task", "To Do", "PRJ-100", false, ready, "PRJ-100"),
		"PRJ-102":  jiraItem("PRJ-102", "Task", "To Do", "PRJ-100", false, ready, "PRJ-100"),
		"PRJ-103":  jiraItem("PRJ-103", "Task", "To Do", "PRJ-100", false, ready, "PRJ-100"),
		"PRJ-105":  jiraItem("PRJ-105", "Task", "To Do", "PRJ-100", false, ready, "PRJ-100", "PRJ-100"),
		"PRJ-110":  jiraItem("PRJ-110", "Task", "To Do", "PRJ-101", false, ready, "PRJ-100"),
		"PRJ-120":  jiraItem("PRJ-120", "Task", "To Do", "PRJ-100", false, ready),
		"PRJ-130":  jiraItem("PRJ-130", "Task", "To Do", "PRJ-110", false, ready),
		"PRJ-DONE": jiraItem("PRJ-DONE", "Epic", "Done", "", true, nil),
		"PRJ-104":  jiraItem("PRJ-104", "Task", "To Do", "PRJ-DONE", false, ready, "PRJ-DONE"),
	}
}

func parentFinding(verb, ancestor, descendant string, direct bool) Finding {
	relation := "an ancestor"
	if direct {
		relation = "the parent"
	}
	return finding(
		CheckParentBlocksChild,
		SeverityWarn,
		verb+": "+ancestor+" is "+relation+" of "+descendant+"; a parent blocking its own child is usually a linking mistake",
		"factory unlink "+ancestor+" "+descendant,
		ancestor, descendant,
	)
}

func childFinding(descendant, ancestor string, direct bool) Finding {
	message := descendant + " blocks its ancestor " + ancestor + "; a child blocking its own ancestor is usually redundant"
	if direct {
		message = descendant + " blocks its parent " + ancestor + "; a child blocking its own parent is usually redundant"
	}
	return finding(
		CheckChildBlocksParent,
		SeverityInfo,
		message,
		"factory unlink "+descendant+" "+ancestor,
		descendant, ancestor,
	)
}
