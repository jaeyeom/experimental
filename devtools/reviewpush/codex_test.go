package reviewpush

import (
	"testing"
)

func TestBuildArgsTermuxSandbox(t *testing.T) {
	// A helpCache string that contains all supported flags so supportsFlag
	// returns true for --sandbox (and other flags we don't care about here).
	fakeHelp := "--ask-for-approval --sandbox --ephemeral --color --output-schema --output-last-message"

	input := &CodexPassInput{
		RepoDir:    "/tmp/test-repo",
		BaseBranch: "main",
		RemoteRef:  "origin/main",
	}

	tests := []struct {
		name          string
		termuxVersion string // empty means unset
		wantSandbox   string
	}{
		{
			name:          "non-Termux: uses workspace-write",
			termuxVersion: "",
			wantSandbox:   "workspace-write",
		},
		{
			name:          "Termux: uses danger-full-access",
			termuxVersion: "0.118.0",
			wantSandbox:   "danger-full-access",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.termuxVersion != "" {
				t.Setenv("TERMUX_VERSION", tt.termuxVersion)
			} else {
				// Ensure the variable is unset even if the host shell has it.
				t.Setenv("TERMUX_VERSION", "")
			}

			r := &RealCodexRunner{
				CodexBin:   "codex",
				helpCache:  fakeHelp,
				helpCached: true,
			}

			args := r.buildArgs(input, "/tmp/schema.json", "/tmp/output.json")

			// Find --sandbox and its value.
			sandboxValue := ""
			for i, a := range args {
				if a == "--sandbox" && i+1 < len(args) {
					sandboxValue = args[i+1]
					break
				}
			}

			if sandboxValue == "" {
				t.Fatalf("--sandbox flag not found in args: %v", args)
			}
			if sandboxValue != tt.wantSandbox {
				t.Errorf("--sandbox value: got %q, want %q (full args: %v)", sandboxValue, tt.wantSandbox, args)
			}
		})
	}
}
