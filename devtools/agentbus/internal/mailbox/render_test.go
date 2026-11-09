package mailbox

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestRenderMatchesThePasteShape(t *testing.T) {
	sent := time.Date(2026, 11, 9, 14, 2, 0, 0, time.UTC)
	msg := message{
		ID: "m-7f3a", From: "ops/inbox", To: "eng/reviewer", Re: "m-7e10",
		SentAt: sent, Body: "<body>", Replyable: true,
	}
	got := render(msg, false)
	want := fmt.Sprintf("[agentbus m-7f3a from ops/inbox → eng/reviewer · re m-7e10 · %s]\n<body>\n↩ reply: agentbus reply m-7f3a \"...\"\n", sent.Local().Format("15:04"))
	if got != want {
		t.Fatalf("render =\n%s\nwant\n%s", got, want)
	}
}

func TestRenderAnonymousHasNoReplyAddress(t *testing.T) {
	sent := time.Date(2026, 11, 9, 14, 2, 0, 0, time.UTC)
	msg := message{
		ID: "m-1", From: "anon:host-9", To: "ops/inbox",
		SentAt: sent, Body: "ping", Replyable: false,
	}
	got := render(msg, false)
	clock := sent.Local().Format("15:04")
	if !strings.Contains(got, "from anon:host-9 → ops/inbox · "+clock) || !strings.HasSuffix(got, "no reply address\n") {
		t.Fatalf("render = %q", got)
	}
	if strings.Contains(got, "↩ reply:") {
		t.Fatalf("render = %q", got)
	}
}

func TestLongAndUnsafeBodiesPreview(t *testing.T) {
	sent := time.Date(2026, 11, 9, 14, 2, 0, 0, time.UTC)
	body := strings.Repeat("x", previewLen) + "END-OF-BODY"
	msg := message{ID: "m-long", From: "ops/inbox", To: "eng/reviewer", SentAt: sent, Body: body, Replyable: true}
	got := render(msg, true)
	if strings.Contains(got, "END-OF-BODY") || !strings.Contains(got, "agentbus show m-long") {
		t.Fatalf("preview = %q", got)
	}
	if !unsafeBody("hello\x00world") || unsafeBody("hello\nworld") {
		t.Fatal("unsafe classification")
	}
}
