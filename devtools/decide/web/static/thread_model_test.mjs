import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  openAtIndex, newDividerLabel, deliveryMark, approvalCardFor, isOpenQuestion, canSendAndArchive,
  hasOtherWorker, routeWarning, replyOnBus, mergeMarkers, hhmm, answerPlan, replyPlan, threadText, messagesOf,
} from './thread_model.js';
import { createDraftSync } from './draft_sync.js';

const T = (m) => `2026-11-20T10:${String(m).padStart(2, '0')}:00Z`;

test('openAtIndex: first agent message newer than readAt', () => {
  const it = { readAt: T(5), messages: [
    { author: 'dragon', time: T(1) }, { author: 'human', time: T(2) },
    { author: 'dragon', time: T(6) }, { author: 'dragon', time: T(7) }] };
  assert.equal(openAtIndex(it), 2);
});
test('openAtIndex: nothing new, never read, and zero readAt', () => {
  assert.equal(openAtIndex({ readAt: T(9), messages: [{ author: 'a', time: T(1) }] }), -1);
  assert.equal(openAtIndex({ messages: [{ author: 'a', time: T(1) }, { author: 'a', time: T(2) }] }), 0);
  assert.equal(openAtIndex({ readAt: '0001-01-01T00:00:00Z', messages: [{ author: 'a', time: T(1) }] }), 0);
  assert.equal(openAtIndex({ messages: [] }), -1);
  assert.equal(openAtIndex({}), -1);
});
test('openAtIndex: own and system messages are not new', () => {
  const it = { readAt: T(5), messages: [{ author: 'human', time: T(6) }, { author: 'system', time: T(7) }] };
  assert.equal(openAtIndex(it), -1);
});
test('newDividerLabel', () => {
  const d = new Date(T(5));
  const hh = `${String(d.getHours()).padStart(2, '0')}:${String(d.getMinutes()).padStart(2, '0')}`;
  assert.equal(hhmm(T(5)), hh);
  assert.equal(newDividerLabel(T(5)), `── new since ${hh} ──`);
  assert.equal(newDividerLabel(undefined), '── new ──');
  assert.equal(newDividerLabel('0001-01-01T00:00:00Z'), '── new ──');
});

test('deliveryMark mirrors the Go marks', () => {
  const msg = { author: 'human', time: T(1) };
  const mk = (d) => deliveryMark({ delivery: [{ message: T(1), at: T(1), target: 'x', ...d }] }, msg);
  assert.equal(mk({ outcome: 'waiting', reason: 'transport unreachable' }), 'waiting: bus down');
  assert.equal(mk({ outcome: 'waiting', reason: 'composer busy' }), 'waiting: target composer busy');
  assert.equal(mk({ outcome: 'waiting', reason: 'other' }), 'waiting');
  assert.equal(mk({ outcome: 'waiting' }), 'waiting');
  assert.equal(mk({ outcome: 'queued' }), 'waiting');
  assert.equal(mk({ outcome: 'failed', error: 'boom' }), 'failed: boom');
  assert.equal(mk({ outcome: 'failed' }), 'failed');
  assert.equal(mk({ outcome: 'delivered', deliveredAt: T(1) }), `delivered ${hhmm(T(1))}`);
  assert.equal(mk({ outcome: 'delivered', deliveredAt: T(4) }), `delivered ${hhmm(T(4))}, 3 min late`);
  assert.equal(mk({ outcome: 'delivered' }), `delivered ${hhmm(T(1))}`);
});
test('deliveryMark: latest record, resend ignored, agent messages unmarked', () => {
  const msg = { author: 'human', time: T(1) };
  const it = { delivery: [
    { message: T(1), at: T(1), outcome: 'failed', error: 'e' },
    { message: T(1), at: T(2), outcome: 'delivered', deliveredAt: T(2) },
    { message: T(1), at: T(3), outcome: 'resend' }] };
  assert.equal(deliveryMark(it, msg), `delivered ${hhmm(T(2))}`);
  assert.equal(deliveryMark(it, { author: 'dragon', time: T(1) }), '');
  assert.equal(deliveryMark(it, { author: 'human', time: T(9) }), '');
  assert.equal(deliveryMark({}, msg), '');
});
test('deliveryMark: legacy record without message belongs to the answer', () => {
  const it = { answeredAt: T(3), delivery: [{ at: T(3), outcome: 'failed', error: 'x' }] };
  assert.equal(deliveryMark(it, { author: 'human', time: T(3) }), 'failed: x');
  assert.equal(deliveryMark(it, { author: 'human', time: T(4) }), '');
});

test('approvalCardFor: standalone open approval, nested newest open card, none', () => {
  const a = { id: 'A-1', class: 'approval', state: 'open', messages: [{ time: T(1) }] };
  assert.equal(approvalCardFor([a], a).id, 'A-1');
  const parent = { id: 'D-1', class: 'decision', state: 'open' };
  const c1 = { id: 'A-2', class: 'approval', state: 'open', parent: 'D-1', createdAt: T(1) };
  const c2 = { id: 'A-3', class: 'approval', state: 'open', parent: 'D-1', createdAt: T(2) };
  const c3 = { id: 'A-4', class: 'approval', state: 'answered', parent: 'D-1', createdAt: T(3) };
  assert.equal(approvalCardFor([parent, c1, c2, c3], parent).id, 'A-3');
  assert.equal(approvalCardFor([parent], parent), null);
  assert.equal(approvalCardFor([{ ...a, state: 'answered' }], { ...a, state: 'answered' }), null);
});

test('isOpenQuestion and canSendAndArchive follow the TUI', () => {
  const q = { kind: 'relay', state: 'open', options: [{ key: 'a' }] };
  assert.equal(isOpenQuestion(q), true);
  assert.equal(isOpenQuestion({ kind: 'relay', state: 'open', freeText: true }), true);
  assert.equal(isOpenQuestion({ kind: 'relay', state: 'open' }), false);
  assert.equal(isOpenQuestion({ ...q, state: 'answered' }), false);
  assert.equal(isOpenQuestion({ ...q, kind: 'type-in' }), false);
  assert.equal(canSendAndArchive(q, false), false);
  assert.equal(canSendAndArchive({ kind: 'relay', state: 'open' }, false), true); // note
  assert.equal(canSendAndArchive({ ...q, state: 'answered' }, false), true);
  assert.equal(canSendAndArchive({ kind: 'relay', state: 'done' }, false), false);
  assert.equal(canSendAndArchive({ kind: 'relay', state: 'withdrawn' }, false), false);
  assert.equal(canSendAndArchive({ kind: 'relay', state: 'open' }, true), false); // pending answer
  assert.equal(canSendAndArchive(null, false), false);
});

test('worker routing and bus-down conditions', () => {
  const it = { owner: 'alice', worker: 'bob', notifyPane: 'agentbus:alice', workerNotifyPane: 'agentbus:bob' };
  assert.equal(hasOtherWorker(it), true);
  assert.equal(hasOtherWorker({ owner: 'a', worker: 'a' }), false);
  assert.equal(hasOtherWorker({ owner: 'a' }), false);
  assert.equal(routeWarning(it), 'This reply goes to alice (owner); bob is working on it.');
  assert.equal(routeWarning({ owner: 'a' }), '');
  assert.equal(replyOnBus(it, ''), true);
  assert.equal(replyOnBus(it, 'worker'), true);
  assert.equal(replyOnBus({ ...it, notifyPane: '%3' }, 'owner'), false);
  assert.equal(replyOnBus({ ...it, notifyPane: '%3' }, 'both'), true);
  assert.equal(replyOnBus({ ...it, notifyPane: '%3', workerNotifyPane: '' }, 'worker'), true); // default agentbus:<worker>
  assert.equal(replyOnBus({ owner: 'a', notifyPane: '' }, ''), false);
  assert.equal(replyOnBus({ ...it, notifyClosed: true }, ''), false);
  assert.equal(replyOnBus({ ...it, notifyPane: '%3', worker: 'bob' }, 'worker'), true);
  assert.equal(replyOnBus({ owner: 'a', notifyPane: 'agentbus: ' }, ''), false);
});

test('mergeMarkers matches the TUI conflict format', () => {
  assert.equal(
    mergeMarkers('mine', 'theirs', { updatedBy: 'dragon', updatedAt: '2026-11-21T20:00:00Z' }),
    '<<<<<<< current\nmine\n=======\ntheirs\n>>>>>>> dragon 2026-11-21T20:00:00Z');
  assert.equal(mergeMarkers('a\n', 'b\n', {}),
    '<<<<<<< current\na\n=======\nb\n>>>>>>> other ');
  assert.equal(mergeMarkers('', 'b', { updatedBy: 'x', updatedAt: 't' }),
    '<<<<<<< current\n=======\nb\n>>>>>>> x t');
});

test('answerPlan: option taps answer in one request; +note defers', () => {
  const it = { id: 'D-1', state: 'open', messages: [{ time: T(1) }], lastMessageAt: T(1) };
  assert.deepEqual(answerPlan(it, 'a').body, {
    value: 'a',
    conflictCheck: { baseState: 'open', baseMessageCount: 1, baseLastMessageAt: T(1), baseAnswered: false } });
  assert.equal(answerPlan(it, 'a', ' hi ').body.note, ' hi ');
  assert.equal('note' in answerPlan(it, 'a', '').body, false);
});

test('replyPlan: pending answer, open free-text question, plain reply', () => {
  const note = { id: 'N-1', kind: 'relay', state: 'open', owner: 'o' };
  assert.deepEqual(replyPlan(note, 'hi', { route: '' }), { op: 'reply', id: 'N-1', body: { text: 'hi' } });
  assert.deepEqual(replyPlan(note, 'hi', { route: 'both' }), { op: 'reply', id: 'N-1', body: { text: 'hi', route: 'both' } });
  const q = { id: 'D-1', kind: 'relay', state: 'open', freeText: true, owner: 'o' };
  assert.equal(replyPlan(q, 'x', {}).op, 'answer');
  assert.equal(replyPlan(q, 'x', {}).body.value, 'x');
  const withPending = replyPlan(q, 'why', { pending: { item: { id: 'A-1', state: 'open' }, value: 'edit' } });
  assert.equal(withPending.op, 'answer');
  assert.equal(withPending.id, 'A-1');
  assert.deepEqual([withPending.body.value, withPending.body.note], ['edit', 'why']);
});

// ---- draft sync ----
function harness(overrides = {}) {
  const timers = []; const calls = []; const local = {};
  const h = {
    timers, calls, local, statuses: [],
    put: overrides.put ?? (async (key, draft, baseRev) => {
      calls.push(['put', key, draft.text, baseRev]);
      return { draft: { key, text: draft.text, rev: baseRev + 1, conflicts: [] }, draftConflict: false };
    }),
    remove: overrides.remove ?? (async (key, cid) => { calls.push(['remove', key, cid]); return { ok: true, draft: null }; }),
  };
  h.sync = createDraftSync({
    key: 'reply:D-1', itemId: 'D-1', delay: 1500,
    put: h.put, remove: h.remove,
    saveLocal: async (k, t) => { local[k] = t; },
    setTimer: (fn, ms) => { timers.push({ fn, ms }); return timers.length; },
    clearTimer: (id) => { timers[id - 1].cleared = true; },
    onStatus: (s) => h.statuses.push(s),
  });
  return h;
}
const fire = async (h) => { const t = [...h.timers].reverse().find((x) => !x.cleared && !x.done); t.done = true; await t.fn(); };

test('draft sync: local buffer per keystroke, server save after 1.5s of quiet', async () => {
  const h = harness();
  h.sync.edit('h'); h.sync.edit('he');
  assert.equal(h.local['reply:D-1'], 'he');
  assert.equal(h.timers.length, 2); assert.equal(h.timers[0].cleared, true); assert.equal(h.timers[1].ms, 1500);
  assert.deepEqual(h.calls, []);
  await fire(h);
  assert.deepEqual(h.calls, [['put', 'reply:D-1', 'he', 0]]);
  h.sync.edit('hey'); await fire(h);
  assert.deepEqual(h.calls[1], ['put', 'reply:D-1', 'hey', 1]); // baseRev follows the saved rev
});
test('draft sync: flush on leave saves immediately, nothing when clean', async () => {
  const h = harness();
  await h.sync.flush();
  assert.deepEqual(h.calls, []);
  h.sync.edit('x');
  await h.sync.flush();
  assert.equal(h.calls.length, 1);
  await h.sync.flush();
  assert.equal(h.calls.length, 1);
});
test('draft sync: clearing the text deletes the draft', async () => {
  const h = harness();
  h.sync.adopt({ key: 'reply:D-1', text: 'old', rev: 3, conflicts: [] });
  h.sync.edit('  \n');
  await h.sync.flush();
  assert.deepEqual(h.calls, [['remove', 'reply:D-1', undefined]]);
  assert.equal(h.sync.state().baseRev, 0);
});
test('draft sync: 409 keeps text, records conflicts, banner state', async () => {
  const conflicts = [{ id: 'c1', text: 'theirs', updatedBy: 'dragon', updatedAt: 't' }];
  const h = harness({ put: async () => { const e = new Error('409'); e.status = 409;
    e.data = { draftConflict: true, draft: { key: 'reply:D-1', text: 'server', rev: 7, conflicts } }; throw e; } });
  h.sync.edit('mine'); await h.sync.flush();
  const s = h.sync.state();
  assert.equal(s.status, 'conflict'); assert.equal(s.baseRev, 7); assert.deepEqual(s.conflicts, conflicts);
  assert.equal(s.serverText, 'server');
});
test('draft sync: transient failure keeps text and dirty; retry resends', async () => {
  let fail = true;
  const h = harness({ put: async (key, d, base) => { if (fail) throw new TypeError('offline');
    return { draft: { key, text: d.text, rev: base + 1, conflicts: [] }, draftConflict: false }; } });
  h.sync.edit('keep me'); await h.sync.flush();
  assert.equal(h.sync.state().status, 'error');
  assert.equal(h.sync.state().dirty, true);
  assert.equal(h.local['reply:D-1'], 'keep me');
  fail = false; await h.sync.retry();
  assert.equal(h.sync.state().status, 'saved'); assert.equal(h.sync.state().dirty, false);
});
test('draft sync: resolve keep-current, use-other and merge-both', async () => {
  const mk = () => {
    const h = harness({ remove: async (key, cid) => ({ ok: true, draft: { key, text: 'server', rev: 9, conflicts: [] } }) });
    h.sync.adopt({ key: 'reply:D-1', text: 'server', rev: 8,
      conflicts: [{ id: 'c1', text: 'theirs', updatedBy: 'dragon', updatedAt: '2026-11-21T20:00:00Z' }] });
    return h;
  };
  let h = mk();
  assert.equal(await h.sync.resolve('keep', 'mine'), 'mine');
  assert.deepEqual(h.calls, [['put', 'reply:D-1', 'mine', 9]]);
  assert.equal(h.sync.state().conflicts.length, 0);
  h = mk();
  assert.equal(await h.sync.resolve('other', 'mine'), 'theirs');
  assert.deepEqual(h.calls, [['put', 'reply:D-1', 'theirs', 9]]);
  h = mk();
  const merged = await h.sync.resolve('merge', 'mine');
  assert.equal(merged, '<<<<<<< current\nmine\n=======\ntheirs\n>>>>>>> dragon 2026-11-21T20:00:00Z');
  assert.equal(h.calls[0][2], merged);
});
test('draft sync: keep-current with identical text does not rewrite', async () => {
  const h = harness({ remove: async (key) => ({ ok: true, draft: { key, text: 'same', rev: 4, conflicts: [] } }) });
  h.sync.adopt({ key: 'reply:D-1', text: 'same', rev: 3, conflicts: [{ id: 'c1', text: 'x' }] });
  assert.equal(await h.sync.resolve('keep', 'same'), 'same');
  assert.deepEqual(h.calls, []);
  assert.equal(h.sync.state().baseRev, 4);
});
test('draft sync: discard deletes and cancels the timer', async () => {
  const h = harness();
  h.sync.edit('x');
  await h.sync.discard();
  assert.equal(h.timers[0].cleared, true);
  assert.deepEqual(h.calls, [['remove', 'reply:D-1', undefined]]);
  assert.equal(h.local['reply:D-1'], '');
});

test('threadText and messagesOf', () => {
  const it = { id: 'D-1', question: 'Ship?', owner: 'o', createdAt: T(1),
    messages: [{ author: 'o', time: T(1), text: 'Ship?', verbatim: 'v' }, { author: 'human', by: 'me', time: T(2), text: 'yes' }] };
  assert.equal(threadText(it), `D-1 Ship?\n\no ${hhmm(T(1))}:\nShip?\n---\nv\n---\n\nme ${hhmm(T(2))}:\nyes`);
  assert.equal(messagesOf({ owner: 'o', question: 'q', createdAt: T(1) })[0].text, 'q');
  assert.equal(messagesOf(it).length, 2);
});
