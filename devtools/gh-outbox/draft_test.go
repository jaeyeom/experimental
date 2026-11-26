package ghoutbox

import (
	"errors"
	"strings"
	"testing"
)

func TestParseLineSpec(t *testing.T) {
	tests := []struct {
		spec, path, file string
		line             int
		wantErr          bool
	}{
		{spec: "a/b.go:12=note.txt", path: "a/b.go", line: 12, file: "note.txt"},
		{spec: "c:d.go:3=x=y.txt", path: "c:d.go", line: 3, file: "x=y.txt"},
		{spec: "a.go=note.txt", wantErr: true},
		{spec: "a.go:0=note.txt", wantErr: true},
		{spec: "a.go:x=note.txt", wantErr: true},
		{spec: ":3=note.txt", wantErr: true},
		{spec: "a.go:3=", wantErr: true},
		{spec: "a.go:3", wantErr: true},
	}
	for _, tt := range tests {
		path, line, file, err := ParseLineSpec(tt.spec)
		if (err != nil) != tt.wantErr {
			t.Errorf("ParseLineSpec(%q) err = %v, wantErr %v", tt.spec, err, tt.wantErr)
			continue
		}
		if !tt.wantErr && (path != tt.path || line != tt.line || file != tt.file) {
			t.Errorf("ParseLineSpec(%q) = %q, %d, %q", tt.spec, path, line, file)
		}
	}
}

func validDraft() Draft {
	return Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Lines: []LineComment{{Path: "a.go", Line: 3, Text: "hi"}}}
}

func TestValidateDefaultsToThread(t *testing.T) {
	d := validDraft()
	if err := d.Validate(); err != nil {
		t.Fatal(err)
	}
	if d.Kind != KindThread {
		t.Errorf("Kind = %q, want %q", d.Kind, KindThread)
	}
}

func TestValidateBodyWithoutLineOrKindIsUsageError(t *testing.T) {
	d := Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Body: "looks good"}
	err := d.Validate()
	var ue *UsageError
	if !errors.As(err, &ue) {
		t.Fatalf("err = %v, want a UsageError", err)
	}
	if !strings.Contains(err.Error(), "--line") || !strings.Contains(err.Error(), "--kind") {
		t.Errorf("err = %v, want it to say to give a line or choose a review kind", err)
	}
	if d.Kind != "" {
		t.Errorf("Kind = %q, want no fallback to a review", d.Kind)
	}
}

func TestValidate(t *testing.T) {
	tests := []struct {
		name    string
		mod     func(*Draft)
		wantErr string
	}{
		{"bad repo", func(d *Draft) { d.Repo = "nope" }, "owner/name"},
		{"bad pr", func(d *Draft) { d.PR = 0 }, "--pr"},
		{"no owner", func(d *Draft) { d.Owner = "" }, "--owner"},
		{"thread with body", func(d *Draft) { d.Body = "x" }, "no body"},
		{"thread without lines", func(d *Draft) { d.Kind = KindThread; d.Lines = nil }, "--line"},
		{"unknown kind", func(d *Draft) { d.Kind = "merge" }, "--kind"},
		{"empty line text", func(d *Draft) { d.Lines[0].Text = " \n" }, "empty"},
		{"bad utf8", func(d *Draft) { d.Lines[0].Text = "\xff" }, "UTF-8"},
		{"review without text", func(d *Draft) { d.Kind = KindComment; d.Lines = nil }, "body"},
		{"approve empty ok", func(d *Draft) { d.Kind = KindApprove; d.Lines = nil }, ""},
		{"review with lines ok", func(d *Draft) { d.Kind = KindRequestChanges; d.Body = "b" }, ""},
		{"edit thread two lines", func(d *Draft) { d.Edit = "5"; d.Lines = append(d.Lines, d.Lines[0]) }, "exactly one"},
		{"edit review with lines", func(d *Draft) { d.Edit = "5"; d.Kind = KindComment; d.Body = "b" }, "no --line"},
		{"edit review ok", func(d *Draft) { d.Edit = "5"; d.Kind = KindComment; d.Body = "b"; d.Lines = nil }, ""},
		{"edit bad id", func(d *Draft) { d.Edit = "abc" }, "--edit"},
		{"reply ok", func(d *Draft) { d.Kind = KindReply; d.InReplyTo = "42"; d.Body = "thanks"; d.Lines = nil }, ""},
		{"reply missing in-reply-to", func(d *Draft) { d.Kind = KindReply; d.Body = "thanks"; d.Lines = nil }, "--in-reply-to"},
		{"reply bad in-reply-to", func(d *Draft) { d.Kind = KindReply; d.InReplyTo = "abc"; d.Body = "thanks"; d.Lines = nil }, "--in-reply-to"},
		{"reply empty body", func(d *Draft) { d.Kind = KindReply; d.InReplyTo = "42"; d.Body = " \n"; d.Lines = nil }, "body"},
		{"reply with lines", func(d *Draft) { d.Kind = KindReply; d.InReplyTo = "42"; d.Body = "thanks" }, "no --line"},
		{"reply edit ok", func(d *Draft) { d.Kind = KindReply; d.Edit = "42"; d.Body = "updated"; d.Lines = nil }, ""},
		{"reply edit with in-reply-to", func(d *Draft) {
			d.Kind = KindReply
			d.Edit = "42"
			d.InReplyTo = "10"
			d.Body = "updated"
			d.Lines = nil
		}, "--in-reply-to"},
		{"thread with in-reply-to", func(d *Draft) { d.InReplyTo = "42" }, "--in-reply-to"},
		{"comment with allow-resolved", func(d *Draft) { d.Kind = KindComment; d.Body = "b"; d.AllowResolved = true }, "--allow-resolved"},
	}
	for _, tt := range tests {
		d := validDraft()
		d.Lines = append([]LineComment(nil), d.Lines...)
		tt.mod(&d)
		err := d.Validate()
		switch {
		case tt.wantErr == "" && err != nil:
			t.Errorf("%s: err = %v", tt.name, err)
		case tt.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tt.wantErr)):
			t.Errorf("%s: err = %v, want it to contain %q", tt.name, err, tt.wantErr)
		}
	}
}

func TestDestinations(t *testing.T) {
	d := Draft{Kind: KindThread, Lines: []LineComment{{Path: "a.go", Line: 3}, {Path: "b/c.go", Line: 10}}}
	want := []string{"discussion thread on a.go line 3", "discussion thread on b/c.go line 10"}
	if got := d.Destinations(); strings.Join(got, "|") != strings.Join(want, "|") {
		t.Errorf("Destinations = %q, want %q", got, want)
	}
	for kind, want := range map[string]string{
		KindComment:        "review (main conversation): comment",
		KindRequestChanges: "review (main conversation): request changes",
		KindApprove:        "review (main conversation): approve",
	} {
		d := Draft{Kind: kind}
		if got := d.Destinations(); len(got) != 1 || got[0] != want {
			t.Errorf("%s: Destinations = %q, want %q", kind, got, want)
		}
	}
	d = Draft{Kind: KindThread, Edit: "9", Lines: []LineComment{{Path: "a.go", Line: 3}}}
	if got := d.Destinations(); len(got) != 1 || got[0] != "edit of comment 9 in the discussion thread on a.go line 3" {
		t.Errorf("edit Destinations = %q", got)
	}
	d = Draft{Kind: KindApprove, Edit: "9"}
	if got := d.Destinations(); len(got) != 1 || got[0] != "edit of review 9 (main conversation)" {
		t.Errorf("edit review Destinations = %q", got)
	}
	d = Draft{Kind: KindReply, InReplyTo: "42"}
	if got := d.Destinations(); len(got) != 1 || got[0] != "reply to comment 42" {
		t.Errorf("reply Destinations = %q", got)
	}
	d = Draft{Kind: KindReply, Edit: "9"}
	if got := d.Destinations(); len(got) != 1 || got[0] != "edit of reply comment 9" {
		t.Errorf("edit reply Destinations = %q", got)
	}
}
