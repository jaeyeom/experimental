package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// promptHookTimeout bounds each re-queue call. The daemon may be finishing
// the delivery that caused the merge, so it is longer than hookTimeout.
const promptHookTimeout = 5 * time.Second

const envelopePrefix = "[agentbus m-"

var envelopeHeader = regexp.MustCompile(`\[agentbus (m-[^\s\]]*)`)

// cmdPromptHook is the Claude Code UserPromptSubmit hook. A prompt where
// human text comes before an agentbus envelope is a bus message pasted
// into a user draft. It prints a block decision and asks the bus to
// re-queue each merged message. Any other prompt passes with no output.
func cmdPromptHook(ctx context.Context, args, env []string, stdin io.Reader, stdout, _ io.Writer) int {
	if len(args) != 0 {
		return 0
	}
	raw, err := io.ReadAll(io.LimitReader(stdin, 1<<20))
	if err != nil || len(bytes.TrimSpace(raw)) == 0 {
		return 0
	}
	var input map[string]any
	if err := json.Unmarshal(raw, &input); err != nil {
		return 0
	}
	prompt, _ := input["prompt"].(string)
	session, _ := input["session_id"].(string)
	ids := mergedEnvelopeIDs(prompt)
	if len(ids) == 0 {
		return 0
	}
	failed := map[string]error{}
	for _, id := range ids {
		if err := requeue(ctx, env, id, session); err != nil {
			failed[id] = err
		}
	}
	out, err := json.Marshal(map[string]string{
		"decision": "block",
		"reason":   blockReason(ids, failed),
	})
	if err != nil {
		return 0
	}
	fmt.Fprintf(stdout, "%s\n", out)
	return 0
}

// mergedEnvelopeIDs returns the ids of envelope headers that follow other
// text. A prompt that starts with an envelope is a clean delivery.
func mergedEnvelopeIDs(prompt string) []string {
	text := strings.TrimLeft(prompt, " \t\r\n")
	if strings.HasPrefix(text, envelopePrefix) {
		return nil
	}
	var ids []string
	for _, match := range envelopeHeader.FindAllStringSubmatch(text, -1) {
		if !slices.Contains(ids, match[1]) {
			ids = append(ids, match[1])
		}
	}
	return ids
}

func requeue(ctx context.Context, env []string, id, session string) error {
	callCtx, cancel := context.WithTimeout(ctx, promptHookTimeout)
	defer cancel()
	resp, err := dialBus(callCtx, env, daemon.Request{Op: "requeue", Name: id, Session: session})
	if err != nil {
		return errors.New("the bus is not reachable")
	}
	if !resp.OK {
		return errors.New(resp.Error)
	}
	return nil
}

func blockReason(ids []string, failed map[string]error) string {
	var b strings.Builder
	b.WriteString("agentbus: your unsent draft was merged with a bus message, so this prompt was not sent. ")
	for _, id := range ids {
		fmt.Fprintf(&b, "Delete the [agentbus %s ...] part, through its reply line, from the input box, keep your own text, and submit again. ", id)
		if err, ok := failed[id]; ok {
			fmt.Fprintf(&b, "The bus could not re-queue %s (%s), so the copy in the input box is the only one: save it before you delete it. ", id, err)
			continue
		}
		fmt.Fprintf(&b, "The bus re-queued %s and delivers it again once the input box is empty. ", id)
	}
	return strings.TrimSpace(b.String())
}
