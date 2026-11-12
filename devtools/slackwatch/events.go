package slackwatch

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"log/slog"
	"strings"
	"time"
)

// Message is one Slack message, as delivered by an event or a fetch.
// ThreadTS is empty or equal to TS for a top-level message. ChannelType is
// Slack's channel_type: channel, group, im, or mpim. EditedTS is set only
// for an edit.
type Message struct {
	Channel     string
	ChannelType string
	TS          string
	ThreadTS    string
	User        string
	Text        string
	EditedTS    string
}

func (m Message) threaded() bool { return m.ThreadTS != "" && m.ThreadTS != m.TS }

func (m Message) direct() bool {
	switch m.ChannelType {
	case "im", "mpim":
		return true
	case "":
		return strings.HasPrefix(m.Channel, "D")
	}
	return false
}

// notifier delivers notifications. *Notifier is the real one.
type notifier interface {
	Notify(ctx context.Context, note Notification) error
}

// Processor stores and notifies Slack messages. Self is the token owner's
// user id from auth.test, and TeamURL its workspace URL for permalinks.
// AfterFunc schedules delayed work such as the unowned grace check; nil
// means time.AfterFunc. Logger reports errors from that work; nil means
// slog.Default.
type Processor struct {
	Store     *Store
	Notifier  notifier
	Config    Config
	Self      string
	TeamURL   string
	Now       func() time.Time
	AfterFunc func(d time.Duration, f func())
	Logger    *slog.Logger
}

// msgState is what the store knows about a message before it is handled.
type msgState struct {
	seen    bool           // stored already as a reply or inbound
	watched bool           // the message or its thread has a watch
	thread  *Watch         // the watch on the message's thread, if threaded
	dm      *Watch         // the latest active dm watch before a top-level DM
	subs    []Sub          // the subscriptions to the message's channel
	outbox  []OutboxRecord // awaiting-handoff records for the message's destination
}

// Inbound kinds as stored in the inbound table.
const (
	inboundDM      = "dm"
	inboundMention = "mention"
)

type action int

const (
	actDrop action = iota
	actReply
	actReopen
	actFooter
	actInbound
	actOutboxLink
)

type decision struct {
	act    action
	watch  Watch
	notify bool
	kind   string       // inbound kind
	subs   []Sub        // subscriptions to notify, besides act
	outbox OutboxRecord // the record an own post is linked to
}

// classify decides what to do with m. It does not touch the store.
func classify(m Message, st msgState, self string, cfg Config, now time.Time) decision {
	if st.seen {
		return decision{}
	}
	d := classifyAct(m, st, self, cfg, now)
	isSelf := m.User == self
	for _, s := range st.subs {
		if s.active(now) && (!isSelf || s.IncludeSelf) {
			d.subs = append(d.subs, s)
		}
	}
	return d
}

func classifyAct(m Message, st msgState, self string, cfg Config, now time.Time) decision {
	isSelf := m.User == self
	if d, ok := classifyWatched(m, st, isSelf, cfg, now); ok {
		return d
	}
	if isSelf {
		// An own post matching an approved handoff post is linked to it
		// rather than becoming an unowned footer watch.
		if r, ok := matchOutbox(m, st.outbox, cfg.FooterPattern.Regexp); ok && !st.watched {
			return decision{act: actOutboxLink, outbox: r}
		}
		if !st.watched && cfg.FooterPattern.Regexp != nil && cfg.FooterPattern.MatchString(m.Text) {
			return decision{act: actFooter}
		}
		return decision{}
	}
	if m.direct() {
		return decision{act: actInbound, kind: inboundDM}
	}
	if mentions(m.Text, self) {
		return decision{act: actInbound, kind: inboundMention}
	}
	return decision{}
}

// classifyWatched matches a reply to an active or recently closed thread
// watch, or a top-level DM after a dm watch.
func classifyWatched(m Message, st msgState, isSelf bool, cfg Config, now time.Time) (decision, bool) {
	notify := !isSelf || cfg.NotifySelf
	if w := st.thread; w != nil && m.threaded() && CompareTS(m.TS, w.SinceTS) > 0 {
		if w.State != StateClosed {
			return decision{act: actReply, watch: *w, notify: notify}, true
		}
		if now.Sub(w.ClosedAt) <= cfg.ReopenWindow.Duration {
			return decision{act: actReopen, watch: *w, notify: notify}, true
		}
	}
	if w := st.dm; w != nil && !m.threaded() && m.direct() && !isSelf && CompareTS(m.TS, w.SinceTS) > 0 {
		return decision{act: actReply, watch: *w, notify: true}, true
	}
	return decision{}, false
}

func mentions(text, user string) bool {
	return user != "" && (strings.Contains(text, "<@"+user+">") || strings.Contains(text, "<@"+user+"|"))
}

// HandleMessage classifies a new message and stores or notifies it. The
// same (channel, ts) handled again is a no-op.
func (p *Processor) HandleMessage(ctx context.Context, m Message) error {
	st, err := p.Store.messageState(m)
	if err != nil {
		return err
	}
	now := p.Now()
	d := classify(m, st, p.Self, p.Config, now)
	notified, err := p.apply(ctx, m, d, now)
	if err != nil {
		return err
	}
	return p.notifySubs(ctx, m, d.subs, notified)
}

// apply carries out d.act and returns the target it notified, if any.
func (p *Processor) apply(ctx context.Context, m Message, d decision, now time.Time) (string, error) {
	switch d.act {
	case actReply, actReopen:
		return p.storeReply(ctx, m, d, now)
	case actFooter:
		w, created, err := p.Store.AddWatch(PostRef{Channel: m.Channel, TS: m.TS, ThreadTS: m.ThreadTS},
			WatchUpdate{Source: ptr(SourceFooter)}, now)
		if err != nil || !created {
			return "", err
		}
		return "", p.notifyUnownedAfterGrace(ctx, w.ID, m)
	case actInbound:
		return "", p.Store.addInbound(m, d.kind)
	case actOutboxLink:
		return "", p.linkOutbox(m, d.outbox, now)
	}
	return "", nil
}

// notifyUnownedAfterGrace sends the unowned notification for footer
// watch id once the unownedGrace period ends, unless an owner claimed it
// meanwhile. A zero grace notifies at once.
func (p *Processor) notifyUnownedAfterGrace(ctx context.Context, id int64, m Message) error {
	check := func() error {
		w, err := getWatch(p.Store.db, id)
		if err != nil || w.State != StateUnowned {
			return err
		}
		return p.notify(ctx, p.note(EventUnowned, w, m))
	}
	grace := p.Config.UnownedGrace.Duration
	if grace <= 0 {
		return check()
	}
	after := p.AfterFunc
	if after == nil {
		after = func(d time.Duration, f func()) { time.AfterFunc(d, f) }
	}
	after(grace, func() {
		if ctx.Err() != nil {
			return
		}
		if err := check(); err != nil {
			p.logger().Error("slackwatch: unowned notification", "watch", id, "err", err)
		}
	})
	return nil
}

func (p *Processor) logger() *slog.Logger {
	if p.Logger != nil {
		return p.Logger
	}
	return slog.Default()
}

func (p *Processor) storeReply(ctx context.Context, m Message, d decision, now time.Time) (string, error) {
	added, err := p.Store.AddReply(p.reply(m, d.watch.ID))
	if err != nil || !added {
		return "", err
	}
	event := EventReply
	if d.act == actReopen {
		event = EventReopened
		if err := p.Store.reopenWatch(d.watch.ID); err != nil {
			return "", err
		}
	} else if d.watch.State == StateUnowned {
		event = EventUnowned
	}
	if _, err := p.Store.DB().Exec(`UPDATE watches SET last_activity = ? WHERE id = ?`, now.Unix(), d.watch.ID); err != nil {
		return "", fmt.Errorf("update last_activity of watch %d: %w", d.watch.ID, err)
	}
	if !d.notify {
		return "", nil
	}
	target := p.Config.Target(d.watch.Owner)
	if d.watch.Owner == "" {
		target = p.Config.Target(p.Config.FallbackTarget)
	}
	return target, p.notify(ctx, p.note(event, d.watch, m))
}

func (p *Processor) reply(m Message, watchID int64) Reply {
	return Reply{
		Channel: m.Channel, TS: m.TS, WatchID: watchID, Author: m.User, Text: m.Text,
		ThreadTS: m.ThreadTS, IsSelf: m.User == p.Self,
	}
}

// notifySubs stores m as a channel message and notifies each subscriber
// once per target. A target in notified already got m as a watch reply.
func (p *Processor) notifySubs(ctx context.Context, m Message, subs []Sub, notified string) error {
	if len(subs) == 0 {
		return nil
	}
	// A row already stored as a watch reply keeps its watch.
	if _, err := p.Store.AddReply(p.reply(m, 0)); err != nil {
		return err
	}
	done := map[string]bool{}
	if notified != "" {
		done[notified] = true
	}
	thread := m.ThreadTS
	if thread == "" {
		thread = m.TS
	}
	for _, s := range subs {
		target := p.Config.Target(s.Owner)
		if done[target] {
			continue
		}
		done[target] = true
		if err := p.notify(ctx, Notification{
			Event:     EventChannel,
			Owner:     s.Owner,
			Channel:   m.Channel,
			ReplyTS:   m.TS,
			PostLink:  permalink(p.TeamURL, m.Channel, thread, ""),
			ReplyLink: permalink(p.TeamURL, m.Channel, m.TS, m.ThreadTS),
			Purpose:   s.Purpose,
			Author:    m.User,
			Text:      m.Text,
		}); err != nil {
			return err
		}
	}
	return nil
}

func (p *Processor) notify(ctx context.Context, note Notification) error {
	if err := p.Notifier.Notify(ctx, note); err != nil {
		return fmt.Errorf("notify %s for watch %d: %w", note.Event, note.WatchID, err)
	}
	return nil
}

func (p *Processor) note(event string, w Watch, m Message) Notification {
	return Notification{
		Event:     event,
		WatchID:   w.ID,
		Owner:     w.Owner,
		Channel:   m.Channel,
		ReplyTS:   m.TS,
		PostLink:  permalink(p.TeamURL, w.Channel, w.ThreadTS, ""),
		ReplyLink: permalink(p.TeamURL, m.Channel, m.TS, m.ThreadTS),
		Purpose:   w.Purpose,
		Refs:      w.Refs,
		Author:    m.User,
		Text:      m.Text,
	}
}

// Permalink builds a Slack message link from the workspace URL. It is
// empty when the URL is unknown.
func Permalink(teamURL, channel, ts, threadTS string) string {
	return permalink(teamURL, channel, ts, threadTS)
}

// permalink builds a Slack message link from the workspace URL, in the
// form Slack itself uses. It is empty when the URL is unknown.
func permalink(teamURL, channel, ts, threadTS string) string {
	if teamURL == "" {
		return ""
	}
	link := strings.TrimSuffix(teamURL, "/") + "/archives/" + channel + "/p" + strings.Replace(ts, ".", "", 1)
	if threadTS != "" && threadTS != ts {
		link += "?thread_ts=" + threadTS + "&cid=" + channel
	}
	return link
}

// HandleEdit updates the stored text of an edited message. It never
// notifies.
func (p *Processor) HandleEdit(m Message) error {
	if _, err := p.Store.DB().Exec(`UPDATE replies SET text = ?, edited_at = ? WHERE channel = ? AND ts = ?`,
		m.Text, m.EditedTS, m.Channel, m.TS); err != nil {
		return fmt.Errorf("edit reply %s/%s: %w", m.Channel, m.TS, err)
	}
	if _, err := p.Store.DB().Exec(`UPDATE inbound SET text = ? WHERE channel = ? AND ts = ?`,
		m.Text, m.Channel, m.TS); err != nil {
		return fmt.Errorf("edit inbound %s/%s: %w", m.Channel, m.TS, err)
	}
	return nil
}

// HandleDelete marks a stored reply deleted. It never notifies.
func (p *Processor) HandleDelete(channel, ts string) error {
	if _, err := p.Store.DB().Exec(`UPDATE replies SET deleted = 1 WHERE channel = ? AND ts = ?`, channel, ts); err != nil {
		return fmt.Errorf("delete reply %s/%s: %w", channel, ts, err)
	}
	return nil
}

// HandleReaction stores a reaction on a watched post or a stored reply
// and drops any other. It never notifies.
func (p *Processor) HandleReaction(channel string, r Reaction) error {
	if _, err := p.Store.DB().Exec(`
INSERT INTO reactions (channel, ts, user, name)
SELECT ?1, ?2, ?3, ?4 WHERE EXISTS (
	SELECT 1 FROM watches WHERE channel = ?1 AND thread_ts = ?2
	UNION ALL SELECT 1 FROM watch_posts p JOIN watches w ON w.id = p.watch_id WHERE w.channel = ?1 AND p.ts = ?2
	UNION ALL SELECT 1 FROM replies WHERE channel = ?1 AND ts = ?2 AND watch_id IS NOT NULL)
ON CONFLICT DO NOTHING`, channel, r.TS, r.User, r.Name); err != nil {
		return fmt.Errorf("add reaction %s/%s: %w", channel, r.TS, err)
	}
	return nil
}

// messageState reads what classify needs about m.
func (s *Store) messageState(m Message) (msgState, error) {
	var st msgState
	if err := s.db.QueryRow(`SELECT EXISTS (SELECT 1 FROM replies WHERE channel = ?1 AND ts = ?2)
	OR EXISTS (SELECT 1 FROM inbound WHERE channel = ?1 AND ts = ?2)`, m.Channel, m.TS).Scan(&st.seen); err != nil {
		return msgState{}, fmt.Errorf("check message %s/%s: %w", m.Channel, m.TS, err)
	}
	if st.seen {
		return st, nil
	}
	subs, err := s.channelSubs(m.Channel)
	if err != nil {
		return msgState{}, err
	}
	st.subs = subs
	_, err = findWatchID(s.db, PostRef{Channel: m.Channel, TS: m.TS, ThreadTS: m.ThreadTS})
	switch {
	case err == nil:
		st.watched = true
	case !errors.Is(err, ErrNoWatch):
		return msgState{}, err
	}
	dest := ""
	if m.threaded() {
		dest = m.ThreadTS
	}
	if st.outbox, err = s.awaitingHandoff(m.Channel, dest); err != nil {
		return msgState{}, err
	}
	if m.threaded() {
		w, err := s.watchByThread(m.Channel, m.ThreadTS)
		if err != nil {
			return msgState{}, err
		}
		st.thread = w
	} else if m.direct() {
		w, err := s.latestDMWatch(m.Channel, m.TS)
		if err != nil {
			return msgState{}, err
		}
		st.dm = w
	}
	return st, nil
}

func (s *Store) watchByThread(channel, thread string) (*Watch, error) {
	var id int64
	err := s.db.QueryRow(`SELECT id FROM watches WHERE channel = ? AND thread_ts = ?`, channel, thread).Scan(&id)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("find watch on %s/%s: %w", channel, thread, err)
	}
	w, err := getWatch(s.db, id)
	if err != nil {
		return nil, err
	}
	return &w, nil
}

// latestDMWatch returns the active dm watch in channel with the latest
// since_ts before ts, or nil.
func (s *Store) latestDMWatch(channel, ts string) (*Watch, error) {
	var latest *Watch
	err := s.queryEach(func(rows *sql.Rows) error {
		var r watchRow
		if err := rows.Scan(r.dest()...); err != nil {
			return err //nolint:wrapcheck // Wrapped by queryEach.
		}
		if w := r.watch(); CompareTS(w.SinceTS, ts) < 0 && (latest == nil || CompareTS(w.SinceTS, latest.SinceTS) > 0) {
			latest = &w
		}
		return nil
	}, `SELECT `+watchCols+` FROM watches WHERE channel = ? AND kind = ? AND state <> ?`, channel, KindDM, StateClosed)
	if err != nil {
		return nil, fmt.Errorf("find dm watch in %s: %w", channel, err)
	}
	return latest, nil
}

// reopenWatch reopens a closed watch: open with an owner, else unowned.
func (s *Store) reopenWatch(id int64) error {
	if _, err := s.db.Exec(`UPDATE watches SET
state = CASE owner WHEN '' THEN ? ELSE ? END, closed_at = NULL, close_reason = ''
WHERE id = ?`, StateUnowned, StateOpen, id); err != nil {
		return fmt.Errorf("reopen watch %d: %w", id, err)
	}
	return nil
}

func (s *Store) addInbound(m Message, kind string) error {
	if _, err := s.db.Exec(`INSERT INTO inbound (channel, ts, author, text, kind) VALUES (?, ?, ?, ?, ?)
ON CONFLICT (channel, ts) DO NOTHING`, m.Channel, m.TS, m.User, m.Text, kind); err != nil {
		return fmt.Errorf("add inbound %s/%s: %w", m.Channel, m.TS, err)
	}
	return nil
}

func ptr[T any](v T) *T { return &v }
