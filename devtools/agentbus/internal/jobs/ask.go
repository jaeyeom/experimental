package jobs

import (
	"context"
	"errors"
	"slices"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// BusReply is one agent bus reply to the message a park asked.
type BusReply struct {
	ID   string
	From string
	// Addressed is a reply from the name the message was sent to, or
	// from whoever holds that name now.
	Addressed bool
	Text      string
}

// BusReplies reads the agent bus replies to a message, oldest first.
type BusReplies interface {
	Replies(ctx context.Context, msg string) ([]BusReply, error)
}

// RunAsk reads the replies to every park's agent bus message. The first
// reply from the addressed name makes a job that still waits on the bus
// ready, with the reply's text as its note and answer. Any other reply,
// and every reply to a job regated onto a decide item, is ignored and
// logged once with its sender. A manager's reply resumes the work; it is
// never the user's approval. Replies are read with the store unlocked.
func RunAsk(ctx context.Context, s *Store, b BusReplies) {
	if b == nil {
		return
	}
	for _, j := range s.askParked() {
		if ctx.Err() != nil {
			return
		}
		replies, err := b.Replies(ctx, j.AskMsg)
		if err != nil {
			// Read again on the next pass.
			continue
		}
		for _, r := range replies {
			if slices.Contains(j.AskSeen, r.ID) {
				continue
			}
			resumedJob, err := s.askReply(j.ID, j.AskMsg, r)
			if err != nil || resumedJob {
				break
			}
		}
	}
}

// askParked returns the parked jobs that asked over the agent bus,
// regated or not.
func (s *Store) askParked() []Job {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []Job
	for _, j := range s.doc.Jobs {
		if j.AskMsg != "" && j.parked() {
			out = append(out, *clone(j))
		}
	}
	return out
}

var errNotAsking = errors.New("job no longer waits on this message")

// askReply applies reply r to the job parked on message msg, and reports
// whether it resumed the job.
func (s *Store) askReply(id, msg string, r BusReply) (bool, error) {
	resumedJob := false
	_, err := s.update(id, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if j.AskMsg != msg || !j.parked() {
			return nil, errNotAsking
		}
		if r.Addressed && j.waitsOnBus() {
			resumedJob = true
			return resumed(s, j, r.From, r.Text, r.Text, now)
		}
		j.AskSeen = append(j.AskSeen, r.ID)
		why := "not from " + j.AskTo
		if !j.waitsOnBus() {
			why = "the job waits on decide " + j.ParkedOn
		}
		ev := jobEvent(j, "", r.From, "ignored reply "+r.ID+" to "+msg+": "+why)
		return []eventlog.Event{ev}, nil
	})
	return resumedJob, err
}
