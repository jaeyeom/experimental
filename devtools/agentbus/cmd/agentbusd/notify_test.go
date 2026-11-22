package main

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
)

func TestDecideArgvOffersAdoptOnlyForANeedsAdoptNote(t *testing.T) {
	adopt := decideArgv("decide", lease.NeedsAdoptNote(lease.Takeover{From: "a", BackupRef: "refs/lease-backup/x", Lease: lease.Status{Checkout: "local:/repo"}}))
	if !slices.Contains(adopt, "adopt:I will run agentbus lease adopt") {
		t.Fatalf("needs-adopt argv = %q", adopt)
	}
	if !slices.Contains(adopt, "decision") {
		t.Fatalf("needs-adopt argv missing decision class = %q", adopt)
	}
	note := lease.PausedNote("local:/repo")
	paused := decideArgv("decide", note)
	if !slices.Contains(paused, "note") {
		t.Fatalf("paused argv missing note class = %q", paused)
	}
	if slices.Contains(paused, "--option") || slices.Contains(paused, "--rec") || paused[len(paused)-1] != note {
		t.Fatalf("paused argv = %q", paused)
	}
}

func TestDecideArgvPassesLongLines(t *testing.T) {
	argv := decideArgv("decide", lease.PausedNote("local:/repo"))
	if len(argv) < 3 || argv[1] != "ask" || argv[2] != "--long-lines" {
		t.Fatalf("argv = %q", argv)
	}
}

func TestDecideNotifierRetriesWithoutLongLinesOnAnOlderDecide(t *testing.T) {
	var argvs [][]string
	var stderr strings.Builder
	notify := decideNotifier(func(string) (string, error) { return "decide", nil },
		func(_ context.Context, argv []string) error {
			argvs = append(argvs, argv)
			if slices.Contains(argv, "--long-lines") {
				return errors.New("decide: exit status 2: flag provided but not defined: -long-lines")
			}
			return nil
		}, &stderr)
	notify("note")
	if len(argvs) != 2 || slices.Contains(argvs[1], "--long-lines") || argvs[1][len(argvs[1])-1] != "note" {
		t.Fatalf("argvs = %q", argvs)
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestRunQuietErrorCarriesStderr(t *testing.T) {
	err := runQuiet(context.Background(), []string{"sh", "-c", "echo 'flag provided but not defined: -long-lines' >&2; exit 2"})
	if !olderDecide(err) {
		t.Fatalf("err = %v", err)
	}
}

func TestDecideNotifierIsNilWithoutDecide(t *testing.T) {
	ran := false
	notify := decideNotifier(func(string) (string, error) { return "", errors.New("not found") },
		func(context.Context, []string) error { ran = true; return nil }, &strings.Builder{})
	if notify != nil {
		t.Fatal("a notifier without decide on PATH")
	}
	if ran {
		t.Fatal("ran a command")
	}
}

func TestDecideNotifierAsksWithTheNoteAsOneArgument(t *testing.T) {
	var got []string
	var stderr strings.Builder
	notify := decideNotifier(func(name string) (string, error) { return "/fake/bin/" + name, nil },
		func(_ context.Context, argv []string) error { got = argv; return nil }, &stderr)
	note := "agentbus lease: local:/repo needs adopt; it's yours"
	notify(note)
	if len(got) == 0 || got[0] != "/fake/bin/decide" || got[1] != "ask" || got[len(got)-1] != note {
		t.Fatalf("argv = %q", got)
	}
	if !slices.Contains(got, "--owner") || !slices.Contains(got, "agentbus") || !slices.Contains(got, "relay") {
		t.Fatalf("argv = %q", got)
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestDecideNotifierReportsAFailure(t *testing.T) {
	var stderr strings.Builder
	notify := decideNotifier(func(string) (string, error) { return "decide", nil },
		func(context.Context, []string) error { return errors.New("boom") }, &stderr)
	notify("note")
	if !strings.Contains(stderr.String(), "decide note: boom") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
