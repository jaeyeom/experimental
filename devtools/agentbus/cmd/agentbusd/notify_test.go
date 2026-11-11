package main

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
)

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
