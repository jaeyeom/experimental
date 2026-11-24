package broker

import (
	"context"
	"sync"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const workerEventBufferSize = 16

// EventType identifies the kind of server-sent event delivered to a worker.
type EventType string

const (
	// EventJobAvailable notifies workers that an eligible job is queued.
	EventJobAvailable EventType = "job_available"
	// EventJobCanceled notifies a worker that one of its leased jobs was canceled.
	EventJobCanceled EventType = "job_canceled"
)

// WorkerEvent is a server-sent event emitted by EventHub to subscribed workers.
type WorkerEvent struct {
	Type EventType
	Data any
}

type workerSub struct {
	workerID string
	repos    map[string]struct{}
	ch       chan WorkerEvent
}

// EventHub is a thread-safe in-memory pub/sub hub for worker SSE wakeups and
// producer job completion long-polls.
type EventHub struct {
	mu         sync.Mutex
	nextSubID  uint64
	subs       map[uint64]*workerSub
	jobWaiters map[string]chan struct{}
}

// NewEventHub creates an initialized EventHub.
func NewEventHub() *EventHub {
	return &EventHub{
		subs:       make(map[uint64]*workerSub),
		jobWaiters: make(map[string]chan struct{}),
	}
}

// SubscribeWorker registers a worker SSE stream filtered by workerID and
// supportedRepos. An empty supportedRepos slice matches all repositories.
// The returned unsubscribe function must be called when the stream ends.
func (h *EventHub) SubscribeWorker(workerID string, supportedRepos []string) (<-chan WorkerEvent, func()) {
	repos := make(map[string]struct{}, len(supportedRepos))
	for _, r := range supportedRepos {
		if r != "" {
			repos[r] = struct{}{}
		}
	}
	sub := &workerSub{
		workerID: workerID,
		repos:    repos,
		ch:       make(chan WorkerEvent, workerEventBufferSize),
	}

	h.mu.Lock()
	h.nextSubID++
	id := h.nextSubID
	h.subs[id] = sub
	h.mu.Unlock()

	unsubscribe := func() {
		h.mu.Lock()
		defer h.mu.Unlock()
		if existing, ok := h.subs[id]; ok {
			delete(h.subs, id)
			close(existing.ch)
		}
	}
	return sub.ch, unsubscribe
}

// PublishJobAvailable emits a job_available event to all subscribed workers
// whose repo filter includes repo.
func (h *EventHub) PublishJobAvailable(repo, skill string) {
	ev := WorkerEvent{
		Type: EventJobAvailable,
		Data: protocol.SSEJobAvailable{
			Repo:  repo,
			Skill: skill,
		},
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	for _, sub := range h.subs {
		if len(sub.repos) > 0 {
			if _, ok := sub.repos[repo]; !ok {
				continue
			}
		}
		sendCoalescing(sub.ch, ev)
	}
}

// BroadcastJobAvailable wakes all subscribed workers so they poll for requeued
// jobs after an expired-lease sweep.
func (h *EventHub) BroadcastJobAvailable() {
	ev := WorkerEvent{
		Type: EventJobAvailable,
		Data: protocol.SSEJobAvailable{},
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	for _, sub := range h.subs {
		sendCoalescing(sub.ch, ev)
	}
}

// PublishJobCanceled emits a job_canceled event to subscriptions belonging to
// workerID.
func (h *EventHub) PublishJobCanceled(workerID, jobID string) {
	if workerID == "" {
		return
	}
	ev := WorkerEvent{
		Type: EventJobCanceled,
		Data: protocol.SSEJobCanceled{
			JobID: jobID,
		},
	}

	h.mu.Lock()
	defer h.mu.Unlock()
	for _, sub := range h.subs {
		if sub.workerID != workerID {
			continue
		}
		sendCoalescing(sub.ch, ev)
	}
}

// WatchJob returns a channel that is closed the next time NotifyJob or
// NotifyAllJobs is called for jobID.
func (h *EventHub) WatchJob(jobID string) <-chan struct{} {
	h.mu.Lock()
	defer h.mu.Unlock()
	ch, ok := h.jobWaiters[jobID]
	if !ok {
		ch = make(chan struct{})
		h.jobWaiters[jobID] = ch
	}
	return ch
}

// WaitJob blocks until NotifyJob(jobID) is called or ctx is done. It returns
// true if notified and false if ctx ended first.
func (h *EventHub) WaitJob(ctx context.Context, jobID string) bool {
	ch := h.WatchJob(jobID)
	select {
	case <-ch:
		return true
	case <-ctx.Done():
		return false
	}
}

// NotifyJob wakes all waiters currently blocked on jobID.
func (h *EventHub) NotifyJob(jobID string) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if ch, ok := h.jobWaiters[jobID]; ok {
		close(ch)
		delete(h.jobWaiters, jobID)
	}
}

// NotifyAllJobs wakes all waiters currently blocked on any job, e.g. after a
// lease sweep transitions expired jobs to TIMED_OUT.
func (h *EventHub) NotifyAllJobs() {
	h.mu.Lock()
	defer h.mu.Unlock()
	for jobID, ch := range h.jobWaiters {
		close(ch)
		delete(h.jobWaiters, jobID)
	}
}

// sendCoalescing delivers ev without blocking, dropping the oldest queued event
// if ch is full so slow workers never stall dispatch.
func sendCoalescing(ch chan WorkerEvent, ev WorkerEvent) {
	select {
	case ch <- ev:
		return
	default:
	}
	select {
	case <-ch:
	default:
	}
	select {
	case ch <- ev:
	default:
	}
}
