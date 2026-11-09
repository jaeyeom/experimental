package mailbox

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

// render is the paste form. A preview keeps the stored body off the
// transport and points at agentbus show. An anonymous sender has no
// reply line.
func render(msg message, preview bool) string {
	var b strings.Builder
	fmt.Fprintf(&b, "[agentbus %s from %s → %s", msg.ID, msg.From, msg.To)
	if msg.Re != "" {
		fmt.Fprintf(&b, " · re %s", msg.Re)
	}
	fmt.Fprintf(&b, " · %s]\n", msg.SentAt.Local().Format("15:04"))
	switch {
	case preview:
		fmt.Fprintf(&b, "%s\nagentbus show %s\n", previewText(msg.Body), msg.ID)
	case msg.Body == "" || strings.HasSuffix(msg.Body, "\n"):
		fmt.Fprintf(&b, "%s", msg.Body)
		if msg.Body == "" {
			b.WriteByte('\n')
		}
	default:
		fmt.Fprintf(&b, "%s\n", msg.Body)
	}
	if msg.Replyable {
		fmt.Fprintf(&b, "↩ reply: agentbus reply %s \"...\"\n", msg.ID)
	} else {
		b.WriteString("no reply address\n")
	}
	return b.String()
}

func previewText(body string) string {
	if len(body) <= previewLen {
		return strings.TrimRight(body, "\n")
	}
	cut := previewLen
	for cut > 0 && !utf8.RuneStart(body[cut]) {
		cut--
	}
	return body[:cut] + "…"
}

// unsafeBody reports bytes a paste into a terminal must not carry.
// Newlines and tabs are allowed. Other C0 controls and DEL are not.
// The exact set is the builder's choice for "unsafe for the transport".
func unsafeBody(body string) bool {
	for _, r := range body {
		if r == '\n' || r == '\t' {
			continue
		}
		if r < 32 || r == 0x7f {
			return true
		}
	}
	return false
}
