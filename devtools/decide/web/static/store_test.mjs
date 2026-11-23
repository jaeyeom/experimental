import test from 'node:test';
import assert from 'node:assert/strict';
import { emptyState, applyResync, applyDelta, parseRoute, freshness, busDown } from './store.js';

const item = (id, o = {}) => ({ id, question: id, state: 'open', unread: true, createdAt: '2026-01-01T00:00:00Z', ...o });

test('resync replaces items and sets seq', () => {
  const s0 = { ...emptyState(), items: [item('D1')], seq: 3 };
  const s1 = applyResync(s0, { seq: 9, items: [item('D2')], status: { x: 1 } }, 5);
  assert.deepEqual(s1.items.map((i) => i.id), ['D2']);
  assert.equal(s1.seq, 9);
  assert.equal(s1.syncedAt, 5);
  assert.deepEqual(s1.status, { x: 1 });
  assert.equal(s0.items[0].id, 'D1', 'input is not mutated');
});

test('delta upserts and deletes in seq order', () => {
  const s0 = { ...emptyState(), items: [item('D1'), item('D2')], seq: 4 };
  const s1 = applyDelta(s0, { seq: 7, changes: [
    { seq: 7, id: 'D1', deleted: true },
    { seq: 5, id: 'D3', item: item('D3') },
    { seq: 6, id: 'D2', item: item('D2', { question: 'changed' }) },
  ] });
  assert.deepEqual(s1.items.map((i) => i.id).sort(), ['D2', 'D3']);
  assert.equal(s1.items.find((i) => i.id === 'D2').question, 'changed');
  assert.equal(s1.seq, 7);
  assert.equal(s0.items.length, 2);
});

test('stale delta is ignored', () => {
  const s0 = { ...emptyState(), items: [item('D1')], seq: 10 };
  assert.equal(applyDelta(s0, { seq: 9, changes: [{ seq: 9, id: 'D1', deleted: true }] }), s0);
});

test('delta carries status and bus health', () => {
  const s1 = applyDelta(emptyState(), { seq: 1, changes: [], status: { a: 1 }, busHealth: { daemon: 'down' } });
  assert.deepEqual(s1.status, { a: 1 });
  assert.ok(busDown(s1.busHealth));
  assert.ok(!busDown({ daemon: 'up', transport: 'up' }));
  assert.ok(!busDown(null));
});

test('parseRoute', () => {
  assert.deepEqual(parseRoute(''), { name: 'inbox', arg: '' });
  assert.deepEqual(parseRoute('#/item/D12'), { name: 'item', arg: 'D12' });
  assert.deepEqual(parseRoute('#/item/a%20b'), { name: 'item', arg: 'a b' });
  assert.deepEqual(parseRoute('#/folder/open'), { name: 'folder', arg: 'open' });
  assert.deepEqual(parseRoute('#/settings'), { name: 'settings', arg: '' });
  assert.deepEqual(parseRoute('#/item'), { name: 'inbox', arg: '' });
  assert.deepEqual(parseRoute('#/nope'), { name: 'inbox', arg: '' });
  assert.deepEqual(parseRoute('#/item/%E0'), { name: 'inbox', arg: '' });
});

test('freshness', () => {
  assert.equal(freshness('connected', 1), 'connected');
  assert.equal(freshness('reconnecting', 1), 'reconnecting');
  assert.equal(freshness('offline', null), 'offline');
  const t = new Date(2026, 0, 1, 9, 5).getTime();
  assert.equal(freshness('offline', t), 'offline · copy from 09:05');
});
