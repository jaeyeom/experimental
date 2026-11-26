package gh

import (
	"context"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestParseVersion(t *testing.T) {
	tests := []struct {
		name    string
		output  string
		want    SemVer
		wantErr bool
	}{
		{
			name:   "standard gh output",
			output: "gh version 2.105.0 (2026-11-17)\nhttps://github.com/cli/cli/releases/tag/v2.105.0\n",
			want:   SemVer{Major: 2, Minor: 105, Patch: 0},
		},
		{
			name:   "prerelease suffix",
			output: "gh version 2.40.1-rc1 (2024-01-01)\n",
			want:   SemVer{Major: 2, Minor: 40, Patch: 1},
		},
		{
			name:    "empty output",
			output:  "",
			wantErr: true,
		},
		{
			name:    "unexpected format",
			output:  "not-gh 1.0",
			wantErr: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := ParseVersion(tc.output)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ParseVersion(%q) error = %v, wantErr %v", tc.output, err, tc.wantErr)
			}
			if !tc.wantErr && got != tc.want {
				t.Fatalf("ParseVersion(%q) = %+v, want %+v", tc.output, got, tc.want)
			}
		})
	}
}

func TestSemVerAtLeast(t *testing.T) {
	tests := []struct {
		v    SemVer
		min  SemVer
		want bool
	}{
		{SemVer{2, 0, 0}, SemVer{2, 0, 0}, true},
		{SemVer{2, 105, 0}, SemVer{2, 0, 0}, true},
		{SemVer{3, 0, 0}, SemVer{2, 105, 0}, true},
		{SemVer{1, 99, 9}, SemVer{2, 0, 0}, false},
		{SemVer{2, 0, 1}, SemVer{2, 0, 2}, false},
	}
	for _, tc := range tests {
		if got := tc.v.AtLeast(tc.min); got != tc.want {
			t.Errorf("%s.AtLeast(%s) = %v, want %v", tc.v, tc.min, got, tc.want)
		}
	}
}

func TestClientVersion(t *testing.T) {
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("gh", "--version").
		WillSucceed("gh version 2.105.0 (2026-11-17)\n", 0).Once().Build()
	got, err := NewClient(mock, "gh").Version(context.Background())
	if err != nil {
		t.Fatalf("Version() error = %v", err)
	}
	if got != (SemVer{Major: 2, Minor: 105, Patch: 0}) {
		t.Fatalf("Version() = %+v", got)
	}
}

func TestClientPlanningStatuses(t *testing.T) {
	t.Run("decodes statuses", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		payload := `{"data":{"repository":{"planningStatuses":{"nodes":[` +
			`{"id":"IS_kgAC","name":"Backlog","category":{"name":"Backlog","state":"OPEN"}},` +
			`{"id":"IS_kgAD","name":"Todo","category":{"name":"Planned","state":"OPEN"}},` +
			`{"id":"IS_kgAE","name":"In progress","category":{"name":"Started","state":"OPEN"}},` +
			`{"id":"IS_kgAF","name":"Done","category":{"name":"Completed","state":"CLOSED"}}` +
			`]}}}}`
		mock.ExpectCommandWithArgs("gh",
			"api", "graphql",
			"-f", "query="+planningStatusesQuery,
			"-f", "owner=acme",
			"-f", "name=widgets",
		).WillSucceed(payload, 0).Once().Build()

		got, err := NewClient(mock, "gh").PlanningStatuses(context.Background(), "acme/widgets")
		if err != nil {
			t.Fatalf("PlanningStatuses() error = %v", err)
		}
		if len(got) != 4 {
			t.Fatalf("len = %d, want 4", len(got))
		}
		if got[2] != (PlanningStatus{ID: "IS_kgAE", Name: "In progress", Category: "Started", State: "OPEN"}) {
			t.Fatalf("got[2] = %+v", got[2])
		}
	})

	t.Run("rejects graphql errors", func(t *testing.T) {
		mock := executor.NewMockExecutor()
		mock.ExpectCommandWithArgs("gh",
			"api", "graphql",
			"-f", "query="+planningStatusesQuery,
			"-f", "owner=acme",
			"-f", "name=widgets",
		).WillSucceed(`{"errors":[{"message":"Field 'planningStatuses' doesn't exist"}]}`, 0).Once().Build()

		_, err := NewClient(mock, "gh").PlanningStatuses(context.Background(), "acme/widgets")
		if err == nil || !strings.Contains(err.Error(), "planningStatuses") {
			t.Fatalf("error = %v", err)
		}
	})

	t.Run("rejects bad repo", func(t *testing.T) {
		_, err := NewClient(executor.NewMockExecutor(), "gh").PlanningStatuses(context.Background(), "bad-repo")
		if err == nil || !strings.Contains(err.Error(), "expected repository owner/repo") {
			t.Fatalf("error = %v", err)
		}
	})
}
