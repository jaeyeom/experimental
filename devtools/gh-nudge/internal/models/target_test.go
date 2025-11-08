package models

import (
	"testing"
)

func TestPRTarget(t *testing.T) {
	target := NewPRTarget(123)
	repo := NewRepository("owner", "repo")

	t.Run("String", func(t *testing.T) {
		expected := "#123"
		if got := target.String(); got != expected {
			t.Errorf("String() = %q, want %q", got, expected)
		}
	})

	t.Run("BuildPath", func(t *testing.T) {
		expected := "repos/owner/repo/pull/123"
		if got := target.BuildPath(repo); got != expected {
			t.Errorf("BuildPath() = %q, want %q", got, expected)
		}
	})

	t.Run("IsPR", func(t *testing.T) {
		if !target.IsPR() {
			t.Error("IsPR() = false, want true")
		}
	})
}

func TestBranchTarget(t *testing.T) {
	target := NewBranchTarget("feature/test")
	repo := NewRepository("owner", "repo")

	t.Run("String", func(t *testing.T) {
		expected := "branch:feature/test"
		if got := target.String(); got != expected {
			t.Errorf("String() = %q, want %q", got, expected)
		}
	})

	t.Run("BuildPath", func(t *testing.T) {
		expected := "repos/owner/repo/branch/feature_test"
		if got := target.BuildPath(repo); got != expected {
			t.Errorf("BuildPath() = %q, want %q", got, expected)
		}
	})

	t.Run("BuildPath_NoSlashes", func(t *testing.T) {
		target := NewBranchTarget("main")
		expected := "repos/owner/repo/branch/main"
		if got := target.BuildPath(repo); got != expected {
			t.Errorf("BuildPath() = %q, want %q", got, expected)
		}
	})

	t.Run("IsPR", func(t *testing.T) {
		if target.IsPR() {
			t.Error("IsPR() = true, want false")
		}
	})
}

func TestNewReviewTarget(t *testing.T) {
	t.Run("FromPR", func(t *testing.T) {
		parsed := &ParsedIdentifier{
			Type:     IdentifierPR,
			PRNumber: 456,
		}
		target := NewReviewTarget(parsed)

		if !target.IsPR() {
			t.Error("Expected PR target")
		}

		if got := target.String(); got != "#456" {
			t.Errorf("String() = %q, want %q", got, "#456")
		}
	})

	t.Run("FromBranch", func(t *testing.T) {
		parsed := &ParsedIdentifier{
			Type:       IdentifierBranch,
			BranchName: "develop",
		}
		target := NewReviewTarget(parsed)

		if target.IsPR() {
			t.Error("Expected branch target")
		}

		if got := target.String(); got != "branch:develop" {
			t.Errorf("String() = %q, want %q", got, "branch:develop")
		}
	})
}

func TestReviewTargetInterface(t *testing.T) {
	repo := NewRepository("test", "repo")

	tests := []struct {
		name       string
		target     ReviewTarget
		wantString string
		wantPath   string
		wantIsPR   bool
	}{
		{
			name:       "PR target",
			target:     PRTarget{Number: 100},
			wantString: "#100",
			wantPath:   "repos/test/repo/pull/100",
			wantIsPR:   true,
		},
		{
			name:       "Branch target simple",
			target:     BranchTarget{Name: "main"},
			wantString: "branch:main",
			wantPath:   "repos/test/repo/branch/main",
			wantIsPR:   false,
		},
		{
			name:       "Branch target with slashes",
			target:     BranchTarget{Name: "feature/new-feature"},
			wantString: "branch:feature/new-feature",
			wantPath:   "repos/test/repo/branch/feature_new-feature",
			wantIsPR:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.target.String(); got != tt.wantString {
				t.Errorf("String() = %q, want %q", got, tt.wantString)
			}
			if got := tt.target.BuildPath(repo); got != tt.wantPath {
				t.Errorf("BuildPath() = %q, want %q", got, tt.wantPath)
			}
			if got := tt.target.IsPR(); got != tt.wantIsPR {
				t.Errorf("IsPR() = %v, want %v", got, tt.wantIsPR)
			}
		})
	}
}
