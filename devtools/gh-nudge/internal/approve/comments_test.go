package approve

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func commentRule() config.ApproveRule {
	return config.ApproveRule{Name: "cleanup", DeletionsAndCommentsOnly: true}
}

func addedPatch(line string) string {
	return "@@ -1 +1 @@\n+" + line + "\n"
}

func commentPR(files ...github.PullFile) github.ApprovalPullRequest {
	pr := basePR()
	pr.Deletions = 1
	pr.Additions = 1
	pr.ChangedFiles = len(files)
	pr.Patches = files
	return pr
}

func oneFile(path, patch string, additions int) github.PullFile {
	return github.PullFile{Path: path, Status: "modified", Patch: patch, PatchSet: true, Additions: additions, Deletions: 1}
}

func evalComments(t *testing.T, pr github.ApprovalPullRequest) ConditionResult {
	t.Helper()
	return conditionResult(t, EvaluateRule(commentRule(), pr), "deletions_and_comments_only")
}

func TestCommentOnlyAdditionPerLanguage(t *testing.T) {
	langs := []struct {
		path     string
		comment  string
		code     string
		trailing string
	}{
		{"a.py", "# note", "x = 1", "x = 1  # note"},
		{"a.sh", "# note", "echo hi", "echo hi # note"},
		{"BUILD", "# note", "x = 1", "x = 1  # note"},
		{"Makefile", "# note", "all:", "all: # note"},
		{"Dockerfile", "# note", "FROM alpine", "FROM alpine # note"},
		{"a.go", "// note", "return nil", "return nil // x"},
		{"a.c", "// note", "return 1;", "return 1; // x"},
		{"a.rs", "// note", "return 1;", "return 1; // x"},
		{"a.sql", "-- note", "select 1", "select 1 -- note"},
		{"a.lua", "-- note", "return 1", "return 1 -- note"},
		{"a.md", "<!-- note -->", "hello", "hello <!-- note -->"},
		{"a.html", "<!-- note -->", "<p>hi</p>", "<p>hi</p> <!-- note -->"},
		{"a.el", "; note", "(print 1)", "(print 1) ; note"},
		{"a.lisp", "; note", "(print 1)", "(print 1) ; note"},
		{"a.ini", "; note", "a=1", "a=1 ; note"},
		{"a.cfg", "# note", "a=1", "a=1 # note"},
	}
	for _, lang := range langs {
		cases := []struct {
			name string
			line string
			want bool
		}{
			{"comment", lang.comment, true},
			{"code", lang.code, false},
			{"trailing", lang.trailing, false},
			{"blank", "", true},
		}
		for _, tc := range cases {
			t.Run(lang.path+"/"+tc.name, func(t *testing.T) {
				pr := commentPR(oneFile(lang.path, addedPatch(tc.line), 1))
				got := evalComments(t, pr)
				if got.Pass != tc.want {
					t.Fatalf("pass = %v, want %v (saw %q)", got.Pass, tc.want, got.Saw)
				}
				if tc.want && !strings.Contains(got.Saw, "1 deletions, 1 comment/blank additions") {
					t.Fatalf("saw = %q", got.Saw)
				}
				if !tc.want && !strings.Contains(got.Saw, "is code") {
					t.Fatalf("saw = %q, want a code reason", got.Saw)
				}
			})
		}
	}
}

func TestDeletionsAndCommentsOnlySpecialLines(t *testing.T) {
	tests := []struct {
		name string
		file github.PullFile
		want bool
		saw  string
	}{
		{
			name: "c include",
			file: oneFile("a.h", addedPatch("#include <stdio.h>"), 1),
			saw:  "is code",
		},
		{
			name: "c define",
			file: oneFile("a.c", addedPatch("#define X 1"), 1),
			saw:  "is code",
		},
		{
			name: "python hash is a comment",
			file: oneFile("a.py", addedPatch("#include is a comment"), 1),
			want: true,
			saw:  "1 deletions, 1 comment/blank additions",
		},
		{
			name: "python docstring",
			file: oneFile("a.py", addedPatch(`"""doc"""`), 1),
			saw:  "is code",
		},
		{
			name: "python string that looks like a comment",
			file: oneFile("a.py", addedPatch(`x = "# not a comment"`), 1),
			saw:  `is code: "x = \"# not a comment\""`,
		},
		{
			name: "go block",
			file: oneFile("a.go", "@@ -1 +1,3 @@\n+/*\n+ note\n+ */\n", 3),
			want: true,
			saw:  "1 deletions, 3 comment/blank additions",
		},
		{
			name: "rust block",
			file: oneFile("a.rs", "@@ -1 +1,3 @@\n+/*\n+ note\n+ */\n", 3),
			want: true,
			saw:  "3 comment/blank additions",
		},
		{
			name: "block then code",
			file: oneFile("a.go", addedPatch("/* note */ return nil"), 1),
			saw:  `is code: "/* note */ return nil"`,
		},
		{
			name: "block opened in context",
			file: oneFile("a.go", "@@ -1,2 +1,3 @@\n /* open\n+ inner\n */\n", 1),
			saw:  "a.go:+2 is code",
		},
		{
			name: "html block",
			file: oneFile("a.html", "@@ -1 +1,3 @@\n+<!--\n+ note\n+ -->\n", 3),
			want: true,
			saw:  "3 comment/blank additions",
		},
		{
			name: "markdown prose",
			file: oneFile("README.md", addedPatch("hello"), 1),
			saw:  "is code",
		},
		{
			name: "unknown extension",
			file: oneFile("notes.txt", addedPatch("# comment"), 1),
			saw:  "notes.txt:+1 is code",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := evalComments(t, commentPR(tt.file))
			if got.Pass != tt.want {
				t.Fatalf("pass = %v, want %v (saw %q)", got.Pass, tt.want, got.Saw)
			}
			if !strings.Contains(got.Saw, tt.saw) {
				t.Fatalf("saw = %q, want it to contain %q", got.Saw, tt.saw)
			}
		})
	}
}

func TestDeletionsAndCommentsOnlyEdges(t *testing.T) {
	t.Run("no deletions", func(t *testing.T) {
		pr := commentPR(oneFile("a.py", addedPatch("# note"), 1))
		pr.Deletions = 0
		got := evalComments(t, pr)
		if got.Pass || !strings.Contains(got.Saw, "0 deletions") {
			t.Fatalf("saw = %q", got.Saw)
		}
	})
	t.Run("missing patch", func(t *testing.T) {
		pr := commentPR(github.PullFile{Path: "blob.bin", Status: "modified", Additions: 1, PatchSet: false})
		got := evalComments(t, pr)
		if got.Pass || got.Saw != "patch unavailable for blob.bin" {
			t.Fatalf("saw = %q", got.Saw)
		}
	})
	t.Run("rename without a patch passes", func(t *testing.T) {
		pr := commentPR(
			github.PullFile{Path: "new.go", Previous: "old.go", Status: "renamed"},
			oneFile("a.py", "@@ -1,2 +1,1 @@\n-old\n+# note\n", 1),
		)
		got := evalComments(t, pr)
		if !got.Pass || !strings.Contains(got.Saw, "1 deletions, 1 comment/blank additions") {
			t.Fatalf("saw = %q", got.Saw)
		}
	})
	t.Run("truncated file list", func(t *testing.T) {
		pr := commentPR(oneFile("a.py", addedPatch("# note"), 1))
		pr.ChangedFiles = 2
		pr.Files = []models.File{{Path: "a.py"}, {Path: "b.py"}}
		got := evalComments(t, pr)
		if got.Pass || got.Saw != "patch unavailable for b.py" {
			t.Fatalf("saw = %q", got.Saw)
		}
	})
	t.Run("inactive by default and composes", func(t *testing.T) {
		pr := commentPR(oneFile("a.py", addedPatch("# note"), 1))
		bare := EvaluateRule(config.ApproveRule{Name: "bare"}, pr)
		if len(bare.Conditions) != 1 || bare.Conditions[0].Condition == "deletions_and_comments_only" {
			t.Fatalf("bare = %+v", bare.Conditions)
		}
		both := config.ApproveRule{Name: "both", DeletionsAndCommentsOnly: true, LabelsRequired: []string{"missing"}}
		got := EvaluateRule(both, pr)
		if got.Matched || len(got.Conditions) != 3 {
			t.Fatalf("composed = %+v", got)
		}
		if !conditionResult(t, got, "deletions_and_comments_only").Pass {
			t.Fatal("comment condition should pass")
		}
		if conditionResult(t, got, "labels_required").Pass {
			t.Fatal("label condition should fail")
		}
	})
}

func TestRunFetchesPatchesOnlyWhenActive(t *testing.T) {
	pr := prNumber(1)
	pr.Deletions = 2
	file := oneFile("a.py", "@@ -1,2 +1,2 @@\n-old\n+# note\n", 1)
	fake := newFake(pr)
	fake.patches = map[int][]github.PullFile{1: {file}}

	res := run(t, fake, []config.ApproveRule{dashboardRule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.patchReads != 0 {
		t.Fatalf("patch reads = %d, want 0 when the condition is inactive", fake.patchReads)
	}

	rule := config.ApproveRule{Name: "cleanup", DeletionsAndCommentsOnly: true, NotDraft: true}
	res = run(t, fake, []config.ApproveRule{rule}, Options{})
	if res.err != nil {
		t.Fatal(res.err)
	}
	if fake.patchReads != 1 {
		t.Fatalf("patch reads = %d, want 1", fake.patchReads)
	}
	if res.log[0].Rule == nil || *res.log[0].Rule != "cleanup" || res.log[0].Action != ActionWouldApprove {
		t.Fatalf("log = %+v", res.log[0])
	}
	saw := conditionResult(t, res.log[0].Rules[0], "deletions_and_comments_only").Saw
	if saw != "2 deletions, 1 comment/blank additions" {
		t.Fatalf("saw = %q", saw)
	}
}
