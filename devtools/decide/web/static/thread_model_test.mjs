import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  openAtIndex, newDividerLabel, deliveryMark, approvalCardFor, isOpenQuestion, canSendAndArchive,
  hasOtherWorker, routeWarning, replyOnBus, mergeMarkers, hhmm, answerPlan, replyPlan, threadText, messagesOf,
  closedCard,
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

test('closedCard builds state-specific card data for closed/archived/withdrawn without answer', () => {
  const opts = [{ key: 'yes', label: 'Yes' }, { key: 'no', label: 'No' }];
  const closed = {
    kind: 'relay', state: 'done-unread', owner: 'ada',
    doneAt: T(7), outcome: 'Not posted: cancelled.\nSee log.', options: opts,
  };
  assert.deepEqual(closedCard(closed), {
    kind: 'closed',
    icon: '✅',
    title: 'Closed by ada',
    time: hhmm(T(7)),
    body: 'Not posted: cancelled.\nSee log.',
  });
  assert.deepEqual(closedCard({ ...closed, state: 'done' }), {
    kind: 'closed',
    icon: '✅',
    title: 'Closed by ada',
    time: hhmm(T(7)),
    body: 'Not posted: cancelled.\nSee log.',
  });
  assert.deepEqual(closedCard({ ...closed, state: 'done', outcome: 'archived' }), {
    kind: 'archived',
    icon: '📦',
    title: 'Closed by ada',
    time: hhmm(T(7)),
    body: 'archived',
  });
  assert.deepEqual(closedCard({
    kind: 'relay', state: 'withdrawn', owner: 'ada',
    withdrawnAt: T(9), reason: 'superseded\nby PR #42', options: opts,
  }), {
    kind: 'withdrawn',
    icon: '↩️',
    title: 'Withdrawn by ada',
    time: hhmm(T(9)),
    body: 'superseded\nby PR #42',
  });
  assert.equal(closedCard({ ...closed, answer: { value: 'yes' } }), null);
  assert.equal(closedCard({ ...closed, state: 'open' }), null);
  assert.equal(closedCard({ kind: 'relay', state: 'done', owner: 'ada', outcome: 'archived' }), null);
  assert.equal(closedCard({ kind: 'type-in', state: 'done', owner: 'ada', outcome: 'done', freeText: true }), null);
  assert.equal(canSendAndArchive(closed, false), false);
});

test('web thread render: snapshots for closed, archived, withdrawn, and literal HTML', async () => {
  // Minimal DOM stub so createThreadView can render under node.
  class FakeEl {
    constructor(tag) {
      this.tagName = tag.toUpperCase();
      this.children = [];
      this.attributes = {};
      this.style = {};
      this.className = '';
      this.textContent = '';
      this.hidden = false;
      this.disabled = false;
      this.value = '';
      this.classList = {
        toggle: () => {},
        contains: (c) => this.className.split(/\s+/).includes(c),
      };
    }
    get childNodes() { return this.children; }
    get lastElementChild() { return this.children[this.children.length - 1] ?? null; }
    setAttribute(k, v) {
      this.attributes[k] = String(v);
      if (k === 'class') this.className = String(v);
    }
    addEventListener() {}
    focus() {}
    blur() {}
    contains() { return false; }
    append(...nodes) {
      for (const n of nodes) {
        if (n === '' || n == null) continue;
        this.children.push(typeof n === 'string' ? { textContent: n, children: [], className: '', tagName: '#TEXT' } : n);
      }
    }
    replaceChildren(...nodes) {
      this.children = [];
      this.append(...nodes);
    }
    walk(fn) {
      fn(this);
      for (const c of this.children) if (c.walk) c.walk(fn);
    }
    querySelectorAll(sel) {
      const out = [];
      this.walk((n) => {
        if (sel === '.options button.option' && n.tagName === 'BUTTON' && n.classList.contains('option')) out.push(n);
        if (sel === 'button.option' && n.tagName === 'BUTTON' && n.classList.contains('option')) out.push(n);
        if (sel === '.approval-card' && n.classList.contains('approval-card')) out.push(n);
        if (sel === '.closed-card' && n.classList.contains('closed-card')) out.push(n);
        if (sel === '.closed-card-body' && n.classList.contains('closed-card-body')) out.push(n);
        if (sel === '.closed-card-head' && n.classList.contains('closed-card-head')) out.push(n);
      });
      return out;
    }
    allText() {
      const parts = [];
      this.walk((n) => { if (n.textContent) parts.push(n.textContent); });
      return parts.join('\n');
    }
  }
  globalThis.document = {
    createElement: (tag) => new FakeEl(tag),
    createTextNode: (t) => ({ textContent: String(t), children: [], className: '', tagName: '#TEXT' }),
    addEventListener: () => {},
    activeElement: null,
    visibilityState: 'visible',
  };
  globalThis.window = { addEventListener: () => {}, innerHeight: 800 };
  globalThis.requestAnimationFrame = () => {};
  globalThis.location = { origin: 'http://localhost', hash: '#/item/A-1' };

  const { createThreadView } = await import('./thread_view.js');
  const opts = [{ key: 'yes', label: 'Yes' }, { key: 'no', label: 'No' }, { key: 'edit', label: 'Edit' }];
  const renderItem = (item) => {
    const root = new FakeEl('div');
    const tv = createThreadView({ root, toast: () => {} });
    tv.render({ items: [item], drafts: [], display: {}, busHealth: null }, { name: 'item', arg: item.id });
    return root;
  };

  // 1. Done-unread with HTML payload and multi-line outcome
  const htmlOutcome = 'Line 1: <img src=x onerror="alert(1)">\n<script>boom()</script> & "ok"';
  const doneRoot = renderItem({
    id: 'A-1',
    class: 'approval',
    kind: 'relay',
    state: 'done-unread',
    owner: 'ada',
    doneAt: T(7),
    outcome: htmlOutcome,
    question: 'Post this?',
    options: opts,
    createdAt: T(1),
    messages: [{ author: 'ada', time: T(1), text: 'Post this?' }],
  });
  const doneCards = doneRoot.querySelectorAll('.closed-card');
  assert.equal(doneCards.length, 1);
  assert.ok(doneCards[0].classList.contains('closed-card-closed'));
  assert.equal(doneRoot.querySelectorAll('button.option').length, 0);
  assert.equal(doneRoot.querySelectorAll('.approval-card').length, 0);
  assert.equal(doneRoot.querySelectorAll('.closed-card-head')[0].allText(), `✅\nClosed by ada\n·\n${hhmm(T(7))}`);
  const doneBody = doneRoot.querySelectorAll('.closed-card-body')[0];
  assert.equal(doneBody.textContent, htmlOutcome);
  assert.equal(doneBody.children.length, 0, 'HTML outcome must be literal text, never parsed DOM');

  // 2. Archived
  const archRoot = renderItem({
    id: 'A-2',
    kind: 'relay',
    state: 'done',
    owner: 'ada',
    doneAt: T(8),
    outcome: 'archived',
    question: 'Approve?',
    options: opts,
    createdAt: T(1),
    messages: [{ author: 'ada', time: T(1), text: 'Approve?' }],
  });
  const archCards = archRoot.querySelectorAll('.closed-card');
  assert.equal(archCards.length, 1);
  assert.ok(archCards[0].classList.contains('closed-card-archived'));
  assert.equal(archRoot.querySelectorAll('.closed-card-head')[0].allText(), `📦\nClosed by ada\n·\n${hhmm(T(8))}`);
  assert.equal(archRoot.querySelectorAll('.closed-card-body')[0].textContent, 'archived');

  // 3. Withdrawn
  const wdRoot = renderItem({
    id: 'A-3',
    kind: 'relay',
    state: 'withdrawn',
    owner: 'ada',
    withdrawnAt: T(9),
    reason: 'Cancelled before merge\nSee #123',
    question: 'Deploy?',
    options: opts,
    createdAt: T(1),
    messages: [{ author: 'ada', time: T(1), text: 'Deploy?' }],
  });
  const wdCards = wdRoot.querySelectorAll('.closed-card');
  assert.equal(wdCards.length, 1);
  assert.ok(wdCards[0].classList.contains('closed-card-withdrawn'));
  assert.equal(wdRoot.querySelectorAll('button.option').length, 0);
  assert.equal(wdRoot.querySelectorAll('.closed-card-head')[0].allText(), `↩️\nWithdrawn by ada\n·\n${hhmm(T(9))}`);
  assert.equal(wdRoot.querySelectorAll('.closed-card-body')[0].textContent, 'Cancelled before merge\nSee #123');
});
