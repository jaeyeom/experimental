import test from 'node:test';
import assert from 'node:assert/strict';
import {
  recipientList, filterRecipients, sendBody, needsBusConfirm, composeDraftBody, composePlan, forwardPreview,
} from './compose_model.js';
import { isStale, ageLabel, agentRows, resourceRows, leaseLabel } from './status_model.js';
import {
  contactList, toggleFavorite, orgLayout, livenessLabel, detailRows,
} from './contacts_model.js';
import {
  urlBase64ToUint8Array, pushSupport, subscriptionBody, classOptions, toggleClass, validateQuiet,
  prefsBody, formFromSub, permissionMessage,
} from './push_model.js';
import { applyResync, emptyState, parseRoute } from './store.js';

const now = Date.parse('2026-01-01T12:00:00Z');
const ago = (s) => new Date(now - s * 1000).toISOString();

// --- compose ---
const targets = [{ name: 'builder', pane: 'w2:p3' }, { name: 'coord', pane: 'agentbus:coord/primary' }];
const bus = [
  { name: 'zed', bus: true, liveness: 'live', when: 'writes docs' },
  { name: 'coord/primary', bus: true, liveness: 'live', when: 'coordinates' },
  { name: 'old', bus: true, liveness: 'dead' },
  { name: 'abe', bus: true, liveness: 'live' },
  { name: 'ledger-only', bus: false },
];

test('recipients: starred first in starred order, never duplicated, dead last', () => {
  const list = recipientList({ targets, contacts: bus, favorites: ['zed', 'gone'] });
  const names = list.map((r) => r.name);
  assert.deepEqual(names.slice(0, 2), ['zed', 'gone']);
  assert.ok(list[0].fav && list[1].fav);
  assert.equal(names.filter((n) => n === 'zed').length, 1);
  assert.ok(list[1].dead, 'a starred name the bus does not list is dead');
  const rest = list.slice(2);
  assert.deepEqual(rest.map((r) => r.name), ['builder', 'coord/primary', 'abe', 'old']);
  assert.ok(!names.includes('ledger-only'));
  assert.equal(rest.at(-1).dead, true);
  assert.equal(rest[0].bus, false);
});

test('recipient filter matches name or when, ignoring case', () => {
  const list = recipientList({ targets, contacts: bus, favorites: [] });
  assert.deepEqual(filterRecipients(list, 'DOCS').map((r) => r.name), ['zed']);
  assert.deepEqual(filterRecipients(list, 'ab').map((r) => r.name), ['abe']);
  assert.equal(filterRecipients(list, '').length, list.length);
  assert.equal(filterRecipients(list, 'nomatch').length, 0);
});

test('send body and bus-down confirmation', () => {
  const [builder, coord] = recipientList({ targets, contacts: bus, favorites: [] });
  assert.deepEqual(sendBody(builder, 'hi', ''), { to: 'builder', bus: false, text: 'hi' });
  assert.deepEqual(sendBody(coord, 'hi', 'D-1'), { to: 'coord/primary', bus: true, forward: 'D-1', text: 'hi' });
  assert.equal(needsBusConfirm(builder, { daemon: 'down' }), false);
  assert.equal(needsBusConfirm(coord, { daemon: 'down' }), true);
  assert.equal(needsBusConfirm(coord, { transport: 'down' }), true);
  assert.equal(needsBusConfirm(coord, { daemon: 'up', transport: 'up' }), false);
  assert.equal(needsBusConfirm(coord, null), false);
});

test('compose draft body', () => {
  const [builder] = recipientList({ targets, contacts: bus, favorites: [] });
  assert.deepEqual(composeDraftBody(builder, 'x'), { key: 'compose', kind: 'compose', to: 'builder', bus: false, text: 'x' });
  assert.deepEqual(composeDraftBody(null, 'x'), { key: 'compose', kind: 'compose', text: 'x' });
});

test('compose plan: restore prompt, forward never touches the saved draft', () => {
  const saved = { key: 'compose', text: 'half', to: 'abe', bus: true };
  assert.deepEqual(composePlan({ draft: saved, forward: '' }), { prompt: true, saveDraft: true, forward: '' });
  assert.equal(composePlan({ draft: { text: '  ' }, forward: '' }).prompt, false);
  assert.equal(composePlan({ draft: null, forward: '' }).prompt, false);
  assert.deepEqual(composePlan({ draft: saved, forward: 'D-9' }), { prompt: false, saveDraft: false, forward: 'D-9' });
});

test('forward preview is the thread text', () => {
  const t = forwardPreview({ id: 'D-9', question: 'ship?', owner: 'ada', createdAt: ago(60) });
  assert.match(t, /^D-9 ship\?/);
});

test('routes: compose forward id and query', () => {
  assert.deepEqual(parseRoute('#/compose/D-9'), { name: 'compose', arg: 'D-9' });
  assert.deepEqual(parseRoute('#/compose?to=a%20b'), { name: 'compose', arg: '', query: { to: 'a b' } });
  assert.deepEqual(parseRoute('#/compose'), { name: 'compose', arg: '' });
});

test('resync keeps targets, agentbus and orgTreeEnabled', () => {
  const s = applyResync(emptyState(), { seq: 1, items: [], targets, agentbus: true, orgTreeEnabled: true });
  assert.deepEqual(s.targets, targets);
  assert.equal(s.agentbus, true);
  assert.equal(s.orgTreeEnabled, true);
});

// --- status ---
test('status staleness: more than five minutes dims', () => {
  assert.equal(isStale(ago(299), now), false);
  assert.equal(isStale(ago(300), now), false);
  assert.equal(isStale(ago(301), now), true);
  assert.equal(isStale('', now), true);
  assert.equal(isStale('0001-01-01T00:00:00Z', now), true);
  assert.equal(ageLabel(ago(30), now), 'now');
  assert.equal(ageLabel(ago(600), now), '10m');
});

test('agent and resource rows', () => {
  const rows = agentRows({ agents: [{ name: 'a', state: 'working', text: 't', updatedAt: ago(10) },
    { name: 'b', state: 'idle', text: '', updatedAt: ago(900) }] }, now);
  assert.deepEqual(rows.map((r) => [r.name, r.stale]), [['a', false], ['b', true]]);
  assert.deepEqual(agentRows(null, now), []);
  const res = resourceRows({ resources: [{ name: 'repo', availability: 'busy', note: 'held by ada until 14:30, 2 queued', updatedAt: ago(1) }] }, now);
  assert.equal(res[0].lease, 'held by ada until 14:30, 2 queued');
});

test('lease labels', () => {
  assert.equal(leaseLabel({ availability: 'free', note: '' }), 'free');
  assert.equal(leaseLabel({ availability: 'free', note: '1 queued' }), 'free, 1 queued');
  assert.equal(leaseLabel({ availability: 'busy', note: 'paused by user' }), 'paused by user');
  assert.equal(leaseLabel({ availability: 'busy', note: 'needs adopt (backup r1)' }), 'needs adopt (backup r1)');
  assert.equal(leaseLabel({ availability: 'busy', note: '' }), 'busy');
  assert.equal(leaseLabel({ availability: 'busy', note: 'x', guess: true }), 'x (guess)');
});

// --- contacts ---
const C = (name, o = {}) => ({ name, bus: true, liveness: 'live', hidden: false, favorite: false,
  counts: { open: 0, answered: 0, done: 0, withdrawn: 0 }, deliveries: { pending: 0, failed: 0 }, ...o });

test('contact list: starred first in starred order, hidden dropped, dead last', () => {
  const all = [C('a'), C('b', { counts: { open: 3 } }), C('c', { liveness: 'dead' }), C('d', { hidden: true }), C('e')];
  const out = contactList(all, ['e', 'c']);
  assert.deepEqual(out.map((c) => c.name), ['e', 'c', 'b', 'a']);
  assert.deepEqual(out.map((c) => c.fav), [true, true, false, false]);
  assert.deepEqual(contactList(all, [], { showHidden: true }).map((c) => c.name).includes('d'), true);
});

test('toggle favorite appends or removes', () => {
  assert.deepEqual(toggleFavorite(['a'], 'b'), ['a', 'b']);
  assert.deepEqual(toggleFavorite(['a', 'b'], 'a'), ['b']);
});

test('org layout matches the TUI: tree order, pools and unassigned dimmed, other last', () => {
  const tree = { label: 'you', kind: 'operator', children: [
    { label: 'lead', kind: 'role', children: [
      { label: 'dev', kind: 'role', children: [] },
      { label: 'qa-pool', kind: 'pool', max: 3, children: [] },
    ] },
    { label: 'vacant', kind: 'role', children: [] },
  ] };
  const { rows } = orgLayout(tree, [C('dev'), C('lead'), C('stray')]);
  assert.deepEqual(rows.map((r) => [r.depth, r.label, r.contact, r.dim]), [
    [0, 'you', null, false],
    [1, 'lead', 'lead', false],
    [2, 'dev', 'dev', false],
    [2, '[pool] qa-pool (max 3)', null, true],
    [1, 'vacant', null, true],
    [0, 'other', null, true],
    [1, 'stray', 'stray', false],
  ]);
  assert.equal(orgLayout(tree, [C('lead'), C('dev')]).rows.some((r) => r.label === 'other'), false);
});

test('liveness and detail rows', () => {
  assert.equal(livenessLabel(C('a', { livenessAge: 42 })), 'live, seen 42s ago');
  assert.equal(livenessLabel(C('a', { bus: false })), 'not on the bus');
  assert.equal(livenessLabel(C('a', { liveness: 'dead', livenessAge: 7200 })), 'dead, seen 2h ago');
  const d = Object.fromEntries(detailRows(C('a', {
    when: 'does things', last: ago(120), counts: { open: 2, answered: 1, done: 3, withdrawn: 4 },
    deliveries: { pending: 1, failed: 2 },
  }), now));
  assert.equal(d.When, 'does things');
  assert.equal(d.Items, '2 open, 1 answered, 3 done, 4 withdrawn');
  assert.equal(d['Last activity'], '2m ago');
  assert.equal(d.Deliveries, '1 waiting, 2 failed');
});

// --- push / settings ---
test('VAPID base64url converts to bytes', () => {
  assert.deepEqual([...urlBase64ToUint8Array('AQID')], [1, 2, 3]);
  assert.deepEqual([...urlBase64ToUint8Array('-_8')], [251, 255]); // url alphabet, no padding
  assert.deepEqual([...urlBase64ToUint8Array('')], []);
});

test('push support honest messaging', () => {
  const ok = { secure: true, serviceWorker: true, pushManager: true, notification: true };
  assert.equal(pushSupport(ok).ok, true);
  assert.match(pushSupport({ ...ok, secure: false }).message, /HTTPS/);
  assert.match(pushSupport({ ...ok, serviceWorker: false }).message, /service worker/i);
  const ios = pushSupport({ ...ok, pushManager: false, ios: true, standalone: false });
  assert.equal(ios.ok, false);
  assert.match(ios.message, /Home Screen/);
  assert.match(pushSupport({ ...ok, pushManager: false }).message, /does not support/i);
  assert.match(permissionMessage('denied'), /blocked/i);
  assert.equal(permissionMessage('granted'), '');
});

test('subscription body never needs more than endpoint and keys', () => {
  const body = subscriptionBody({ toJSON: () => ({ endpoint: 'https://p/x', expirationTime: null, keys: { p256dh: 'k', auth: 'a' } }) });
  assert.deepEqual(body, { endpoint: 'https://p/x', keys: { p256dh: 'k', auth: 'a' } });
});

test('class options exclude notes and toggle', () => {
  assert.deepEqual(classOptions.map(([id]) => id), ['approval', 'decision', 'fact', 'task', 'request', 'inbound']);
  assert.deepEqual(toggleClass(['approval'], 'task'), ['approval', 'task']);
  assert.deepEqual(toggleClass(['approval', 'task'], 'approval'), ['task']);
});

test('quiet hours validation and preferences body', () => {
  assert.equal(validateQuiet('', ''), '');
  assert.equal(validateQuiet('22:00', '07:00'), '');
  assert.match(validateQuiet('22:00', ''), /both/i);
  assert.match(validateQuiet('25:00', '07:00'), /HH:MM/);
  assert.deepEqual(prefsBody('https://p/x', { classes: ['task'], quietStart: '22:00', quietEnd: '07:00' }, 'Asia/Seoul'),
    { endpoint: 'https://p/x', classes: ['task'], quietStart: '22:00', quietEnd: '07:00', tz: 'Asia/Seoul' });
  assert.deepEqual(prefsBody('e', { classes: [], quietStart: '', quietEnd: '' }, ''),
    { endpoint: 'e', classes: [], quietStart: '', quietEnd: '' });
  assert.deepEqual(formFromSub({ classes: ['fact'], quietStart: '01:00', quietEnd: '02:00', tz: 'UTC' }),
    { classes: ['fact'], quietStart: '01:00', quietEnd: '02:00', tz: 'UTC' });
  assert.deepEqual(formFromSub(null).classes, ['approval', 'decision', 'fact', 'task', 'request', 'inbound']);
});
