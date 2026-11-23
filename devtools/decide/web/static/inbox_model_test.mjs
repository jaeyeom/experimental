import test from 'node:test';
import assert from 'node:assert/strict';
import {
  badgeOf, effectiveClass, hasUnread, unreadCount, sortItems, inboxEntries, selectFolder,
  matchesQuery, filterEntries, snippet, undelivered, draftMark, lastStamp, noteSeen, relativeAge,
  deadlineLabel, inboxView,
} from './inbox_model.js';

const T = (m) => new Date(Date.UTC(2026, 0, 1, 0, m)).toISOString();
const item = (id, o = {}) => ({
  id, question: 'q ' + id, owner: 'agent-a', state: 'open', unread: false, createdAt: T(0), ...o,
});
const ids = (xs) => xs.map((e) => (e.item ? e.item.id : e.id));

test('badge and effective class', () => {
  assert.equal(effectiveClass(item('D-001')), 'decision');
  const want = { decision: 'D', approval: 'A', request: 'R', fact: 'Q', inbound: 'I', task: 'T', note: 'N' };
  for (const [c, b] of Object.entries(want)) assert.equal(badgeOf(item('x', { class: c })), b);
  assert.equal(badgeOf(item('x')), 'D');
  assert.equal(badgeOf(item('x', { class: 'bogus' })), 'D');
});

test('groups come in order approvals, needs you, requests, notes', () => {
  const items = [
    item('N-001', { class: 'note', createdAt: T(9) }),
    item('R-001', { class: 'request', createdAt: T(8) }),
    item('I-001', { class: 'inbound', createdAt: T(7) }),
    item('T-001', { class: 'task', createdAt: T(6) }),
    item('Q-001', { class: 'fact', createdAt: T(5) }),
    item('D-001', { createdAt: T(4) }),
    item('A-001', { class: 'approval', createdAt: T(3) }),
  ];
  const out = inboxEntries(items);
  assert.deepEqual(out.map((e) => e.group), ['approvals', 'needs-you', 'needs-you', 'needs-you', 'requests', 'requests', 'notes']);
  assert.deepEqual(ids(out), ['A-001', 'T-001', 'Q-001', 'D-001', 'R-001', 'I-001', 'N-001']);
});

test('nested open approval rolls under its parent', () => {
  const items = [
    item('D-001', { createdAt: T(1) }),
    item('A-001', { class: 'approval', parent: 'D-001', createdAt: T(2) }),
    item('A-002', { class: 'approval', parent: 'D-001', createdAt: T(3) }),
    item('A-003', { class: 'approval', parent: 'D-001', state: 'answered', createdAt: T(4) }),
    item('A-004', { class: 'approval', parent: 'GONE', createdAt: T(5) }),
  ];
  const out = inboxEntries(items);
  // A-003 is not open, so it is not nested; A-004's parent is absent.
  assert.deepEqual(ids(out), ['A-004', 'A-003', 'D-001']);
  const parent = out.find((e) => e.item.id === 'D-001');
  assert.equal(parent.group, 'approvals');
  assert.equal(parent.approvalWaiting, true);
  assert.deepEqual(parent.approvals.map((a) => a.id), ['A-002', 'A-001']);
  assert.equal(out.some((e) => e.item.id === 'A-002'), false);
});

test('rows keep sort order: deadline soonest, then newest, then higher id', () => {
  const items = [
    item('D-001', { createdAt: T(1) }),
    item('D-002', { createdAt: T(5) }),
    item('D-003', { createdAt: T(2), deadline: T(90) }),
    item('D-004', { createdAt: T(2), deadline: T(30) }),
    item('D-005', { createdAt: T(5) }),
    item('D-006', { createdAt: T(0), messages: [{ author: 'agent-a', time: T(50), text: 'x' }] }),
  ];
  assert.deepEqual(ids(sortItems(items)), ['D-004', 'D-003', 'D-006', 'D-005', 'D-002', 'D-001']);
  assert.deepEqual(ids(inboxEntries(items)), ids(sortItems(items)));
  assert.equal(items[0].id, 'D-001', 'input not mutated');
});

test('deadline ties break toward newer', () => {
  const items = [
    item('D-001', { createdAt: T(1), deadline: T(10) }),
    item('D-002', { createdAt: T(2), deadline: T(10) }),
  ];
  assert.deepEqual(ids(sortItems(items)), ['D-002', 'D-001']);
});

test('class filter keeps those classes; parent with approval matches approval', () => {
  const items = [
    item('D-001', { createdAt: T(1) }),
    item('D-002', { createdAt: T(2) }),
    item('A-001', { class: 'approval', parent: 'D-001', createdAt: T(3) }),
    item('A-002', { class: 'approval', createdAt: T(4) }),
    item('N-001', { class: 'note', createdAt: T(5) }),
  ];
  assert.deepEqual(ids(inboxEntries(items, ['approval'])), ['A-002', 'D-001']);
  assert.deepEqual(ids(inboxEntries(items, ['decision'])), ['D-001', 'D-002']);
  assert.deepEqual(ids(inboxEntries(items, ['note'])), ['N-001']);
  assert.deepEqual(ids(inboxEntries(items, ['task'])), []);
  assert.deepEqual(ids(inboxEntries(items, [])), ['A-002', 'D-001', 'D-002', 'N-001']);
});

test('unread: flag, agent message after last read, excluding notes', () => {
  assert.equal(hasUnread(item('D-001', { unread: true })), true);
  // No messages: the ask itself is the owner message, unread until read.
  assert.equal(hasUnread(item('D-001')), true);
  assert.equal(hasUnread(item('D-001', { readAt: T(5) })), false);
  assert.equal(hasUnread(item('D-001', { readAt: T(5), messages: [{ author: 'agent-a', time: T(6), text: 'x' }] })), true);
  // A human message moves the seen mark; system messages are never unread.
  assert.equal(hasUnread(item('D-001', { messages: [
    { author: 'agent-a', time: T(1), text: 'x' }, { author: 'human', time: T(2), text: 'y' }] })), false);
  assert.equal(hasUnread(item('D-001', { readAt: T(1), messages: [{ author: 'system', time: T(6), text: 'x' }] })), false);
  const items = [item('D-001', { unread: true }), item('N-001', { class: 'note', unread: true }),
    item('D-002', { readAt: T(9) })];
  assert.equal(unreadCount(items), 1);
});

test('folders: inbox holds open and done-unread, hides snoozed; snoozed lists any state by wake time', () => {
  const now = Date.parse(T(60));
  const items = [
    item('D-001'), item('D-002', { state: 'done-unread' }), item('D-003', { state: 'answered' }),
    item('D-004', { state: 'done' }), item('D-005', { state: 'withdrawn' }),
    item('D-006', { snoozedUntil: T(200) }), item('D-007', { state: 'answered', snoozedUntil: T(100) }),
    item('D-008', { snoozedUntil: T(10) }), // wake time passed: back in the inbox
  ];
  assert.deepEqual(ids(selectFolder(items, 'inbox', now)).sort(), ['D-001', 'D-002', 'D-008']);
  assert.deepEqual(ids(selectFolder(items, 'snoozed', now)), ['D-007', 'D-006']);
  assert.deepEqual(ids(selectFolder(items, 'answered', now)), ['D-003']);
  assert.deepEqual(ids(selectFolder(items, 'done', now)), ['D-004']);
  assert.deepEqual(ids(selectFolder(items, 'withdrawn', now)), ['D-005']);
});

test('search matches id, owner, question, messages, delivery markers', () => {
  const it = item('D-042', {
    owner: 'Builder', question: 'Ship the Thing?', verbatim: 'verbatim body',
    messages: [{ author: 'Builder', time: T(1), text: 'see attached', verbatim: 'raw dump' }],
    deliveryNotice: 'late message',
    state: 'answered', answeredAt: T(5), delivery: [{ at: T(6), outcome: 'failed', error: 'bus down' }],
  });
  for (const q of ['d-042', 'builder', 'ship the', 'see attached', 'raw dump', 'verbatim body', 'late message', 'NOT DELIVERED']) {
    assert.equal(matchesQuery(it, q), true, q);
  }
  assert.equal(matchesQuery(it, 'nope'), false);
  assert.equal(matchesQuery(it, ''), true);
  assert.equal(matchesQuery(it, '  '), true);
  // Matches stay inside one field.
  assert.equal(matchesQuery(item('D-001', { owner: 'ab', question: 'cd' }), 'abcd'), false);
});

test('filterEntries searches nested approvals and filters by agent', () => {
  const items = [
    item('D-001', { owner: 'x', createdAt: T(1) }),
    item('A-001', { class: 'approval', parent: 'D-001', owner: 'x', question: 'deploy prod', createdAt: T(2) }),
    item('D-002', { owner: 'y', createdAt: T(3) }),
  ];
  const entries = inboxEntries(items);
  assert.deepEqual(ids(filterEntries(entries, { query: 'deploy' })), ['D-001']);
  assert.deepEqual(ids(filterEntries(entries, { agent: 'y' })), ['D-002']);
  assert.deepEqual(ids(filterEntries(entries, { agent: 'x', query: 'zzz' })), []);
  assert.equal(filterEntries(entries, {}), entries);
});

test('snippet, undelivered, draft marks', () => {
  assert.equal(snippet(item('D-001', { question: '  a\n  b ' })), 'a b');
  assert.equal(snippet(item('D-001', { question: 'q', messages: [{ author: 'a', time: T(1), text: 'latest one' }] })), 'latest one');
  assert.equal(snippet(item('D-001', { question: '', verbatim: 'vb' })), 'vb');
  assert.equal(snippet(item('D-001', { deliveryNotice: 'late' })), 'late');
  const failed = item('D-001', { state: 'answered', answeredAt: T(5), delivery: [{ at: T(6), outcome: 'failed', error: 'boom' }] });
  assert.deepEqual(undelivered(failed), { error: 'boom' });
  assert.match(snippet(failed), /^not delivered: boom /);
  assert.equal(undelivered(item('D-001', { state: 'answered', answeredAt: T(5), delivery: [
    { at: T(6), outcome: 'failed', error: 'x' }, { at: T(7), outcome: 'delivered' }] })), null);
  // A failure before the answer, or a compose attempt, does not count.
  assert.equal(undelivered(item('D-001', { state: 'answered', answeredAt: T(5), delivery: [{ at: T(1), outcome: 'failed' }] })), null);
  assert.equal(undelivered(item('D-001', { delivery: [{ at: T(1), outcome: 'failed', compose: true }] })), null);
  assert.deepEqual(undelivered(item('D-001', { delivery: [{ at: T(1), outcome: 'failed' }] })), { error: '' });
  assert.match(snippet(item('D-001', { delivery: [{ at: T(1), outcome: 'failed' }], question: 'q' })), /^not delivered q/);

  const drafts = [{ key: 'reply:D-001', itemId: 'D-001', text: 'hi' },
    { key: 'reply:D-002', itemId: 'D-002', text: 'hi', conflicts: [{}] },
    { key: 'reply:D-004', itemId: 'D-004', text: '' }];
  assert.equal(draftMark('D-001', drafts), 'draft');
  assert.equal(draftMark('D-002', drafts), 'draft!');
  assert.equal(draftMark('D-003', drafts), '');
  assert.equal(draftMark('D-004', drafts), '');
  assert.equal(draftMark('D-003', drafts, new Set(['D-003'])), 'draft');
  assert.equal(draftMark('D-003', null), '');
});

test('seen stamps for archive', () => {
  const a = item('D-001', { createdAt: T(1), messages: [{ time: T(5) }, { time: T(3) }] });
  assert.equal(lastStamp(a), T(5));
  assert.equal(lastStamp(item('D-001', { createdAt: T(1) })), T(1));
  assert.equal(lastStamp(item('D-001', { lastMessageAt: '2026-01-01T00:07:00.123456789Z' })), '2026-01-01T00:07:00.123456789Z');
  const entries = inboxEntries([item('N-001', { class: 'note', createdAt: T(2) }),
    item('N-002', { class: 'note', createdAt: T(9) }), item('D-001', { createdAt: T(20) })]);
  assert.equal(noteSeen(entries), T(9));
  assert.equal(noteSeen([]), null);
});

test('relative age and deadline labels', () => {
  const now = Date.parse(T(0)) + 3 * 3600e3;
  assert.equal(relativeAge(T(0), now), '3h');
  assert.equal(relativeAge(new Date(now - 30e3).toISOString(), now), 'now');
  assert.equal(relativeAge(new Date(now - 5 * 60e3).toISOString(), now), '5m');
  assert.equal(relativeAge(new Date(now - 50 * 3600e3).toISOString(), now), '2d');
  assert.equal(relativeAge('garbage', now), '');
  assert.equal(deadlineLabel(new Date(now + 2 * 3600e3).toISOString(), now), 'due in 2h');
  assert.equal(deadlineLabel(new Date(now - 2 * 3600e3).toISOString(), now), 'overdue 2h');
  assert.equal(deadlineLabel(undefined, now), '');
});

test('inboxView combines folder, grouping, filters and counts', () => {
  const now = Date.parse(T(60));
  const items = [
    item('D-001', { unread: true, createdAt: T(1) }),
    item('N-001', { class: 'note', unread: true, createdAt: T(2) }),
    item('D-002', { state: 'answered' }),
    item('D-003', { snoozedUntil: T(500) }),
  ];
  const v = inboxView(items, { folder: 'inbox', classes: [], query: '', agent: '' }, now);
  assert.equal(v.grouped, true);
  assert.equal(v.unread, 1);
  assert.deepEqual(v.groups.map((g) => [g.group, ids(g.entries)]), [['needs-you', ['D-001']], ['notes', ['N-001']]]);
  const s = inboxView(items, { folder: 'snoozed', classes: ['note'], query: '', agent: '' }, now);
  assert.equal(s.grouped, false);
  assert.deepEqual(ids(s.rows), []);
  const a = inboxView(items, { folder: 'answered', classes: [], query: 'D-002', agent: '' }, now);
  assert.deepEqual(ids(a.rows), ['D-002']);
});
