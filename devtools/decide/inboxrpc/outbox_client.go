package inboxrpc

import (
	"errors"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// OutboxEnabled reports whether this client queues writes in a local outbox.
func (c *Client) OutboxEnabled() bool {
	return c.UseOutbox
}

// Outbox returns a snapshot of all entries in the local outbox.
func (c *Client) Outbox() []tui.OutboxEntry {
	if !c.UseOutbox {
		return nil
	}
	ob, err := c.ensureOutbox()
	if err != nil {
		return nil
	}
	return ob.entries()
}

// EditOutbox updates a pending, held, or failed entry's text and resumes sending.
func (c *Client) EditOutbox(id, text string) error {
	ob, err := c.ensureOutbox()
	if err != nil {
		return err
	}
	var curPtr *decide.Item
	for _, e := range ob.entries() {
		if e.ID == id && e.ItemID != "" {
			if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
				if cur, ok := rep.get(e.ItemID); ok {
					curPtr = &cur
				}
			}
			break
		}
	}
	if err := ob.edit(id, text, c.now(), curPtr); err != nil {
		return err
	}
	c.startOutboxIfPending()
	return nil
}

// CancelOutbox removes a pending, held, or failed entry from the outbox.
func (c *Client) CancelOutbox(id string) error {
	ob, err := c.ensureOutbox()
	if err != nil {
		return err
	}
	ob.remove(id)
	return nil
}

// DismissFailedOutbox removes a failed entry from the outbox.
func (c *Client) DismissFailedOutbox(id string) error {
	return c.CancelOutbox(id)
}

// SendOutboxAsMessage turns a failed entry into a plain reply carrying its
// text and queues it for sending. A reply is accepted on a done thread.
func (c *Client) SendOutboxAsMessage(id string) error {
	ob, err := c.ensureOutbox()
	if err != nil {
		return err
	}
	var curPtr *decide.Item
	for _, e := range ob.entries() {
		if e.ID == id && e.ItemID != "" {
			if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
				if cur, ok := rep.get(e.ItemID); ok {
					curPtr = &cur
				}
			}
			break
		}
	}
	if err := ob.asMessage(id, c.now(), curPtr); err != nil {
		return err
	}
	c.startOutboxIfPending()
	return nil
}

// isDoneRefusal reports whether err is the ledger refusing a done or
// archive because the item is already done. That is a no-op success, as
// the hub now treats it; this covers a hub that still refuses.
func isDoneRefusal(op string, err error) bool {
	return (op == OpDone || op == OpArchive) && strings.HasSuffix(err.Error(), " is done")
}

// SendOutbox forces a held entry to be sent despite the detected conflict.
func (c *Client) SendOutbox(id string) error {
	ob, err := c.ensureOutbox()
	if err != nil {
		return err
	}
	var curPtr *decide.Item
	for _, e := range ob.entries() {
		if e.ID == id && e.ItemID != "" {
			if rep, rerr := c.ensureReplica(); rerr == nil && rep != nil {
				if cur, ok := rep.get(e.ItemID); ok {
					curPtr = &cur
				}
			}
			break
		}
	}
	if err := ob.forceSend(id, curPtr); err != nil {
		return err
	}
	c.startOutboxIfPending()
	return nil
}

func (c *Client) enqueueOutbox(entry tui.OutboxEntry) (decide.Item, error) {
	rep, err := c.ensureReplica()
	if err != nil {
		return decide.Item{}, err
	}
	ob, err := c.ensureOutbox()
	if err != nil {
		return decide.Item{}, err
	}
	entry.WrittenAt = c.now()
	var item decide.Item
	if entry.ItemID != "" {
		if cur, ok := rep.get(entry.ItemID); ok {
			item = cur
			setBaseline(&entry, cur)
		} else {
			item = decide.Item{ID: entry.ItemID}
		}
	}
	ob.append(entry)
	c.startOutboxIfPending()
	return item, nil
}

func (c *Client) startOutboxIfPending() {
	ob, err := c.ensureOutbox()
	if err != nil || !ob.hasPending() {
		return
	}
	c.outboxMu.Lock()
	startWorker := false
	if !c.outboxRunning {
		c.outboxRunning = true
		if c.outboxWake == nil {
			c.outboxWake = make(chan struct{}, 1)
		}
		startWorker = true
	}
	c.outboxMu.Unlock()
	if startWorker {
		c.wg.Add(1)
		go func() {
			defer c.wg.Done()
			c.outboxWorker()
		}()
	}
	c.kickOutbox()
}

func (c *Client) kickOutbox() {
	c.outboxMu.Lock()
	ch := c.outboxWake
	c.outboxMu.Unlock()
	if ch == nil {
		return
	}
	select {
	case ch <- struct{}{}:
	default:
	}
}

func (c *Client) outboxWorker() {
	const retryInterval = 250 * time.Millisecond
	for {
		c.mu.Lock()
		closed := c.closed
		c.mu.Unlock()
		if closed {
			return
		}
		ob, err := c.ensureOutbox()
		if err != nil {
			return
		}
		entry, ok := ob.nextSendable()
		if !ok {
			c.outboxMu.Lock()
			c.outboxRunning = false
			c.outboxMu.Unlock()
			return
		}
		if c.sendOutboxEntry(ob, entry) {
			continue
		}
		c.maybeReconnect()
		c.outboxMu.Lock()
		wake := c.outboxWake
		c.outboxMu.Unlock()
		timer := time.NewTimer(retryInterval)
		select {
		case <-wake:
			timer.Stop()
		case <-timer.C:
		}
	}
}

func (c *Client) checkLocalConflict(entry tui.OutboxEntry) string {
	if entry.Force || entry.ItemID == "" || entry.BaseState == "" {
		return ""
	}
	rep, rerr := c.ensureReplica()
	if rerr != nil || rep == nil {
		return ""
	}
	cur, ok := rep.get(entry.ItemID)
	if !ok {
		return ""
	}
	return DetectConflict(entry.Op, entry.BaseState, entry.BaseMessageCount, entry.BaseLastMessageAt, entry.BaseAnswered, cur)
}

func buildOutboxRequest(entry tui.OutboxEntry) Request {
	w := entry.WrittenAt
	req := Request{
		Op:        entry.Op,
		Item:      entry.ItemID,
		Text:      entry.Text,
		Route:     entry.Route,
		Until:     entry.Until,
		Seen:      entry.Seen,
		To:        entry.To,
		Bus:       entry.Bus,
		Forward:   entry.Forward,
		Name:      entry.Name,
		Choice:    entry.Choice,
		WrittenAt: &w,
	}
	if !entry.Force && entry.ItemID != "" && entry.BaseState != "" {
		req.ConflictCheck = true
		req.BaseState = entry.BaseState
		req.BaseMessageCount = entry.BaseMessageCount
		req.BaseAnswered = entry.BaseAnswered
		if !entry.BaseLastMessageAt.IsZero() {
			t := entry.BaseLastMessageAt
			req.BaseLastMessageAt = &t
		}
	}
	return req
}

// sendOutboxEntry attempts to send entry to the hub. It returns true if the
// worker should immediately move on to the next entry (either because entry
// completed, was held on conflict, or failed permanently), and false if the
// hub was unreachable and the worker should wait and retry.
func (c *Client) sendOutboxEntry(ob *outboxStore, entry tui.OutboxEntry) bool {
	if reason := c.checkLocalConflict(entry); reason != "" {
		ob.markHeld(entry.ID, reason)
		return true
	}
	resp, err := c.call(buildOutboxRequest(entry))
	if err != nil {
		if errors.Is(err, tui.ErrUnreachable) {
			return false
		}
		if isDoneRefusal(entry.Op, err) {
			ob.remove(entry.ID)
			return true
		}
		ob.markFailed(entry.ID, err.Error())
		return true
	}
	rep, rerr := c.ensureReplica()
	if resp.Conflict != "" {
		if rerr == nil && rep != nil && resp.Item != nil {
			rep.upsertLocal(*resp.Item)
		}
		ob.markHeld(entry.ID, resp.Conflict)
		return true
	}
	if rerr == nil && rep != nil && resp.Item != nil {
		rep.upsertLocal(*resp.Item)
		ob.rebaselineItem(entry.ItemID, *resp.Item)
	}
	if resp.Display != nil {
		c.noteDisplay(*resp.Display)
	}
	ob.remove(entry.ID)
	return true
}
