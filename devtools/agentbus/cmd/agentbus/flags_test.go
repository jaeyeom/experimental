package main

import (
	"flag"
	"io"
	"slices"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
)

func TestParseInterspersedAcceptsFlagsAfterPositionals(t *testing.T) {
	for _, tt := range []struct {
		args     []string
		wantPos  []string
		wantFlag bool
	}{
		{[]string{"a", "--x"}, []string{"a"}, true},
		{[]string{"--x", "a"}, []string{"a"}, true},
		{[]string{"a", "--x", "b"}, []string{"a", "b"}, true},
		{[]string{"a", "b"}, []string{"a", "b"}, false},
		{[]string{"--", "--x"}, []string{"--x"}, false},
		{[]string{"a", "--", "--x", "b"}, []string{"a", "--x", "b"}, false},
		{[]string{"a", "-", "--x"}, []string{"a", "-"}, true},
		{nil, nil, false},
	} {
		fs := flag.NewFlagSet("t", flag.ContinueOnError)
		x := fs.Bool("x", false, "")
		pos, err := parseInterspersed(fs, tt.args)
		if err != nil {
			t.Fatalf("%q: %v", tt.args, err)
		}
		if !slices.Equal(pos, tt.wantPos) || *x != tt.wantFlag {
			t.Errorf("%q = %q x=%v, want %q x=%v", tt.args, pos, *x, tt.wantPos, tt.wantFlag)
		}
	}
}

func TestParseInterspersedReportsBadFlags(t *testing.T) {
	fs := flag.NewFlagSet("t", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	if _, err := parseInterspersed(fs, []string{"a", "--bogus"}); err == nil {
		t.Fatal("want an error for an unknown flag after a positional")
	}
}

func TestHibernateFlagsAfterTheHandle(t *testing.T) {
	parse := func(args ...string) hibernate.Request {
		t.Helper()
		req, _, err := parseHibernate(args, io.Discard)
		if err != nil {
			t.Fatalf("%q: %v", args, err)
		}
		return req
	}
	after, before := parse("ops/x", "--dry-run"), parse("--dry-run", "ops/x")
	if !after.DryRun || !slices.Equal(after.Targets, before.Targets) || after.DryRun != before.DryRun {
		t.Fatalf("after = %+v, before = %+v", after, before)
	}
	if _, asJSON, err := parseHibernate([]string{"ops/x", "--json"}, io.Discard); err != nil || !asJSON {
		t.Fatalf("--json after the handle: json=%v err=%v", asJSON, err)
	}
	dashed := parse("--", "--dry-run")
	if dashed.DryRun || !slices.Equal(dashed.Targets, []string{"--dry-run"}) {
		t.Fatalf("-- --dry-run = %+v, want --dry-run as a name", dashed)
	}
}

func TestWakeTakesANameAfterDoubleDash(t *testing.T) {
	name, err := parseWake([]string{"--", "-odd"}, io.Discard)
	if err != nil || name != "-odd" {
		t.Fatalf("name = %q, err = %v", name, err)
	}
	if _, err := parseWake([]string{"ops/x", "--now"}, io.Discard); err == nil {
		t.Fatal("want an error for an unknown flag after the name")
	}
}

func TestLeaseUnshareFlagsAfterThePane(t *testing.T) {
	f, err := parseLeaseFlags("unshare", []string{"w1:p2", "--json"}, io.Discard)
	if err != nil || !f.jsonOut || !slices.Equal(f.with, []string{"w1:p2"}) {
		t.Fatalf("flags = %+v, err = %v", f, err)
	}
}
