package git

import (
	"strings"
	"testing"
)

func TestNewBranch(t *testing.T) {
	tests := []struct {
		name       string
		branchName string
		wantErr    bool
		errMsg     string
	}{
		// Valid branch names
		{
			name:       "valid simple branch",
			branchName: "main",
			wantErr:    false,
		},
		{
			name:       "valid branch with slash",
			branchName: "feature/new-feature",
			wantErr:    false,
		},
		{
			name:       "valid branch with underscore",
			branchName: "feature_branch",
			wantErr:    false,
		},
		{
			name:       "valid branch with hyphen",
			branchName: "feature-branch",
			wantErr:    false,
		},
		{
			name:       "valid branch with dot",
			branchName: "v1.0.0",
			wantErr:    false,
		},
		{
			name:       "valid single character",
			branchName: "a",
			wantErr:    false,
		},
		{
			name:       "valid complex branch name",
			branchName: "feature/ABC-123_test-branch",
			wantErr:    false,
		},

		// Invalid branch names
		{
			name:       "empty branch name",
			branchName: "",
			wantErr:    true,
			errMsg:     "branch name cannot be empty",
		},
		{
			name:       "branch with double dots",
			branchName: "feature..branch",
			wantErr:    true,
			errMsg:     "branch name cannot contain '..'",
		},
		{
			name:       "branch starting with hyphen",
			branchName: "-feature",
			wantErr:    true,
			errMsg:     "branch name cannot start with '-'",
		},
		{
			name:       "branch ending with dot",
			branchName: "feature.",
			wantErr:    true,
			errMsg:     "branch name cannot end with '.'",
		},
		{
			name:       "branch with space",
			branchName: "feature branch",
			wantErr:    true,
			errMsg:     "branch name cannot contain spaces",
		},
		{
			name:       "branch with tab",
			branchName: "feature\tbranch",
			wantErr:    true,
			errMsg:     "branch name cannot contain whitespace characters",
		},
		{
			name:       "branch with newline",
			branchName: "feature\nbranch",
			wantErr:    true,
			errMsg:     "branch name cannot contain whitespace characters",
		},
		{
			name:       "branch with carriage return",
			branchName: "feature\rbranch",
			wantErr:    true,
			errMsg:     "branch name cannot contain whitespace characters",
		},
		{
			name:       "branch with special characters",
			branchName: "feature@branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with semicolon",
			branchName: "feature;branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with colon",
			branchName: "feature:branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with question mark",
			branchName: "feature?branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with asterisk",
			branchName: "feature*branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with bracket",
			branchName: "feature[branch]",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with backslash",
			branchName: "feature\\branch",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch starting with dot",
			branchName: ".feature",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch ending with slash",
			branchName: "feature/",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with command injection attempt",
			branchName: "feature; rm -rf /",
			wantErr:    true,
			errMsg:     "branch name cannot contain spaces",
		},
		{
			name:       "branch with pipe",
			branchName: "feature|command",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with backtick",
			branchName: "feature`command`",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
		{
			name:       "branch with dollar sign",
			branchName: "feature$var",
			wantErr:    true,
			errMsg:     "invalid branch name",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			branch, err := NewBranch(tt.branchName)

			if tt.wantErr {
				if err == nil {
					t.Errorf("NewBranch(%q) expected error but got none", tt.branchName)
				} else if tt.errMsg != "" && !strings.Contains(err.Error(), tt.errMsg) {
					t.Errorf("NewBranch(%q) error = %v, want error containing %q",
						tt.branchName, err, tt.errMsg)
				}
			} else {
				if err != nil {
					t.Errorf("NewBranch(%q) unexpected error: %v", tt.branchName, err)
				}
				if branch.String() != tt.branchName {
					t.Errorf("NewBranch(%q) branch.String() = %q, want %q",
						tt.branchName, branch.String(), tt.branchName)
				}
			}
		})
	}
}

func TestBranch_String(t *testing.T) {
	tests := []struct {
		name       string
		branchName string
	}{
		{
			name:       "simple branch name",
			branchName: "main",
		},
		{
			name:       "complex branch name",
			branchName: "feature/ABC-123_test",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			branch, err := NewBranch(tt.branchName)
			if err != nil {
				t.Fatalf("NewBranch(%q) unexpected error: %v", tt.branchName, err)
			}

			if got := branch.String(); got != tt.branchName {
				t.Errorf("Branch.String() = %q, want %q", got, tt.branchName)
			}
		})
	}
}
