package models

import (
	"testing"
)

func TestParseIdentifier(t *testing.T) {
	tests := []struct {
		name       string
		identifier string
		wantIsPR   bool
		wantPRNum  int
		wantBranch string
		wantErr    bool
	}{
		{
			name:       "PR number - single digit",
			identifier: "1",
			wantIsPR:   true,
			wantPRNum:  1,
		},
		{
			name:       "PR number - multiple digits",
			identifier: "123",
			wantIsPR:   true,
			wantPRNum:  123,
		},
		{
			name:       "PR number - large number",
			identifier: "999999",
			wantIsPR:   true,
			wantPRNum:  999999,
		},
		{
			name:       "Branch name - simple",
			identifier: "main",
			wantIsPR:   false,
			wantBranch: "main",
		},
		{
			name:       "Branch name - with slash",
			identifier: "feature/auth-fix",
			wantIsPR:   false,
			wantBranch: "feature/auth-fix",
		},
		{
			name:       "Branch name - with dashes",
			identifier: "feature-branch-123",
			wantIsPR:   false,
			wantBranch: "feature-branch-123",
		},
		{
			name:       "Branch name - looks like number but has letters",
			identifier: "123-feature",
			wantIsPR:   false,
			wantBranch: "123-feature",
		},
		{
			name:       "Branch name - starts with number",
			identifier: "2.0-release",
			wantIsPR:   false,
			wantBranch: "2.0-release",
		},
		{
			name:       "Invalid - empty",
			identifier: "",
			wantErr:    true,
		},
		{
			name:       "Invalid - zero PR number",
			identifier: "0",
			wantErr:    true,
		},
		{
			name:       "Invalid - negative PR number",
			identifier: "-5",
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parsed, err := ParseIdentifier(tt.identifier)
			if (err != nil) != tt.wantErr {
				t.Errorf("ParseIdentifier() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if tt.wantErr {
				return
			}

			if parsed.IsPR() != tt.wantIsPR {
				t.Errorf("ParseIdentifier() IsPR = %v, want %v", parsed.IsPR(), tt.wantIsPR)
			}

			if tt.wantIsPR {
				if parsed.PRNumber != tt.wantPRNum {
					t.Errorf("ParseIdentifier() PRNumber = %v, want %v", parsed.PRNumber, tt.wantPRNum)
				}
			} else {
				if parsed.BranchName != tt.wantBranch {
					t.Errorf("ParseIdentifier() BranchName = %v, want %v", parsed.BranchName, tt.wantBranch)
				}
			}

			// Test String() method
			expectedStr := tt.identifier
			if tt.wantIsPR && parsed.String() != expectedStr {
				t.Errorf("ParseIdentifier() String() = %v, want %v", parsed.String(), expectedStr)
			}
		})
	}
}

func TestParsedIdentifier_Methods(t *testing.T) {
	// Test PR identifier
	prIdentifier := &ParsedIdentifier{
		Type:     IdentifierPR,
		PRNumber: 42,
	}

	if !prIdentifier.IsPR() {
		t.Error("IsPR() should return true for PR identifier")
	}
	if prIdentifier.IsBranch() {
		t.Error("IsBranch() should return false for PR identifier")
	}
	if prIdentifier.String() != "42" {
		t.Errorf("String() for PR = %v, want %v", prIdentifier.String(), "42")
	}

	// Test branch identifier
	branchIdentifier := &ParsedIdentifier{
		Type:       IdentifierBranch,
		BranchName: "feature/test",
	}

	if branchIdentifier.IsPR() {
		t.Error("IsPR() should return false for branch identifier")
	}
	if !branchIdentifier.IsBranch() {
		t.Error("IsBranch() should return true for branch identifier")
	}
	if branchIdentifier.String() != "feature/test" {
		t.Errorf("String() for branch = %v, want %v", branchIdentifier.String(), "feature/test")
	}
}
