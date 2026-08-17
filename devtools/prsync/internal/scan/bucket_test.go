package scan

import (
	"testing"
)

func TestBucket(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name  string
		facts PRFacts
		want  string
	}{
		{
			name:  "draft plus failing CI is needs_you",
			facts: PRFacts{IsDraft: true, CIState: "failing"},
			want:  "needs_you",
		},
		{
			name:  "draft plus unaddressed is needs_you",
			facts: PRFacts{IsDraft: true, Unaddressed: true, CIState: "green"},
			want:  "needs_you",
		},
		{
			name:  "draft otherwise is draft",
			facts: PRFacts{IsDraft: true, CIState: "green", ReviewDecision: "APPROVED"},
			want:  "draft",
		},
		{
			name: "approved with outstanding requests is needs_you",
			facts: PRFacts{
				ReviewDecision: "APPROVED",
				ReviewRequests: []string{"User:reviewer"},
				CIState:        "green",
				Mergeable:      "MERGEABLE",
			},
			want: "needs_you",
		},
		{
			name: "approved green mergeable is ready",
			facts: PRFacts{
				ReviewDecision: "APPROVED",
				CIState:        "green",
				Mergeable:      "MERGEABLE",
			},
			want: "ready",
		},
		{
			name: "approved green unknown mergeable is still ready",
			facts: PRFacts{
				ReviewDecision: "APPROVED",
				CIState:        "green",
				Mergeable:      "UNKNOWN",
			},
			want: "ready",
		},
		{
			name: "approved green conflicting is waiting",
			facts: PRFacts{
				ReviewDecision: "APPROVED",
				CIState:        "green",
				Mergeable:      "CONFLICTING",
			},
			want: "waiting",
		},
		{
			name: "changes requested is waiting",
			facts: PRFacts{
				ReviewDecision: "CHANGES_REQUESTED",
				CIState:        "green",
				Mergeable:      "MERGEABLE",
			},
			want: "waiting",
		},
		{
			name: "review required is waiting",
			facts: PRFacts{
				ReviewDecision: "REVIEW_REQUIRED",
				CIState:        "green",
				Mergeable:      "MERGEABLE",
			},
			want: "waiting",
		},
		{
			name:  "empty decision is waiting",
			facts: PRFacts{CIState: "green", Mergeable: "MERGEABLE"},
			want:  "waiting",
		},
		{
			name: "pending CI is waiting",
			facts: PRFacts{
				ReviewDecision: "APPROVED",
				CIState:        "pending",
				Mergeable:      "MERGEABLE",
			},
			want: "waiting",
		},
		{
			name:  "failing CI is needs_you",
			facts: PRFacts{CIState: "failing"},
			want:  "needs_you",
		},
		{
			name:  "unaddressed comments are needs_you",
			facts: PRFacts{Unaddressed: true, CIState: "green", ReviewDecision: "APPROVED"},
			want:  "needs_you",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got := Bucket(tc.facts)
			if got != tc.want {
				t.Fatalf("Bucket() = %q, want %q", got, tc.want)
			}
		})
	}
}
