import test from 'node:test';
import assert from 'node:assert/strict';
import {
  recipientList, filterRecipients, sendBody, needsBusConfirm, composeDraftBody, composePlan, forwardPreview,
} from './compose_model.js';
import {
  isStale, ageLabel, formatUpdatedAgo, agentRows, resourceRows, leaseLabel, panelRows,
  busHealthRows, resolveDashboardPanels, buildDashboardPanels, gaugeModel, chartModel,
  tableSlice, formatUnknownText,
} from './status_model.js';
import {
  contactList, toggleFavorite, orgLayout, livenessLabel, detailRows,
} from './contacts_model.js';
import {
  urlBase64ToUint8Array, pushSupport, subscriptionBody, classOptions, toggleClass, validateQuiet,
  prefsBody, formFromSub, permissionMessage,
} from './push_model.js';
import {
  groupReasons, allowsEarlierItem, validateFlagInput, resolveClientFlagTarget, openFlagSheet, openGoodSheet,
} from './flag_sheet.js';
import { openAskCount, sortAsks, asksPanel, safeHttpURL } from './asks_model.js';
import { applyResync, applyDelta, emptyState, parseRoute } from './store.js';

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

test('panel rows handle readable, stale, and unreadable panels', () => {
  const rows = panelRows([
    { id: 'ci', title: 'CI', kind: 'kv', updatedAt: ago(30), stale: false },
    { id: 'old', title: 'Old', kind: 'kv', updatedAt: ago(600), stale: true },
    { id: 'bad', file: 'bad.json', unreadable: true, error: 'bad json' },
  ], now);
  assert.equal(rows[0].stale, false);
  assert.equal(rows[0].unreadable, false);
  assert.equal(rows[1].stale, true);
  assert.equal(rows[2].unreadable, true);
  assert.equal(rows[2].text, 'panel unreadable: bad.json');
});

test('dashboard ordering, built-in panels, and panel kind models', () => {
  assert.equal(formatUpdatedAgo(ago(3 * 3600), now), 'updated 3h ago');
  assert.equal(formatUpdatedAgo(ago(20), now), 'updated now');

  const resolved = resolveDashboardPanels(
    [{ id: 'ci', enabled: true, order: 0 }, { id: 'resources', enabled: false, order: 1 }],
    ['agents', 'resources', 'bus-health', 'ci', 'extra'],
  );
  assert.deepEqual(resolved.map((r) => [r.id, r.enabled]), [
    ['ci', true],
    ['resources', false],
    ['agents', true],
    ['bus-health', true],
    ['extra', true],
  ]);

  const built = buildDashboardPanels({
    status: {
      agents: [{ name: 'ada', state: 'working', text: 'coding', updatedAt: ago(10) }],
      resources: [{ name: 'repo', availability: 'free', updatedAt: ago(10) }],
    },
    busHealth: { daemon: 'up', transport: 'down', problems: ['socket closed'] },
    panels: [
      { id: 'ci', title: 'CI', kind: 'Text', data: { text: 'green' }, updatedAt: ago(60), stale: false },
      { id: 'extra', title: 'Extra', kind: 'Text', data: { text: 'hi' }, updatedAt: ago(60), stale: false },
    ],
    dashboardPanels: [
      { id: 'ci', enabled: true, order: 0 },
      { id: 'resources', enabled: false, order: 1 },
    ],
    now,
  });
  assert.deepEqual(built.map((p) => p.id), ['ci', 'agents', 'bus-health', 'extra']);
  assert.deepEqual(busHealthRows({ daemon: 'up', transport: 'down', problems: ['x'] }), [
    { label: 'daemon', value: 'up', state: 'ok' },
    { label: 'transport', value: 'down', state: 'fail' },
    { label: 'problem', value: 'x', state: 'fail' },
  ]);
  assert.equal(gaugeModel({ value: 80, max: 100, warn: 75, fail: 95, unit: '%' }).state, 'warn');
  assert.equal(gaugeModel({ value: 96, max: 100, warn: 75, fail: 95, unit: '%' }).state, 'fail');
  assert.equal(tableSlice({ columns: ['A'], rows: [[1], [2], [3], [4], [5], [6], [7]] }).more, 2);
  assert.match(formatUnknownText({ k: 'v' }), /"k": "v"/);
  const cm = chartModel({
    series: [{ t: '2026-01-01T10:00:00Z', v: 10 }, { t: '2026-01-01T11:00:00Z', v: 50 }],
    unit: '%',
    limit: 100,
    projection: { to: 100, at: '2026-01-01T13:00:00Z' },
  });
  assert.equal(cm.latest, 50);
  assert.equal(cm.limit, 100);
  assert.equal(cm.projection.at, '2026-01-01T13:00:00Z');
});

test('status view renders all panel kinds, unknown as Text, stale dimming, and built-in order', async () => {
  class FakeEl {
    constructor(tag) {
      this.tagName = tag.toUpperCase();
      this.attrs = {};
      this.children = [];
      this.textContent = '';
    }
    setAttribute(k, v) { this.attrs[k] = String(v); }
    get className() { return this.attrs.class ?? ''; }
    append(...nodes) {
      for (const n of nodes) {
        if (typeof n === 'string') this.children.push({ tagName: '#TEXT', textContent: n, children: [], attrs: {} });
        else if (n != null) this.children.push(n);
      }
    }
    replaceChildren(...nodes) { this.children = []; this.append(...nodes); }
    walk(fn) { fn(this); for (const c of this.children) c.walk?.(fn); }
    querySelectorAll(sel) {
      const out = [];
      this.walk((n) => {
        if (sel === 'section.panel-card' && n.tagName === 'SECTION' && n.className.split(' ').includes('panel-card')) out.push(n);
        if (sel === 'svg' && n.tagName.toUpperCase() === 'SVG') out.push(n);
      });
      return out;
    }
    allText() {
      const parts = [];
      this.walk((n) => { if (n.textContent) parts.push(n.textContent); });
      return parts.join('\n');
    }
  }
  const prevDoc = globalThis.document;
  globalThis.document = {
    createElement: (tag) => new FakeEl(tag),
    createElementNS: (_ns, tag) => new FakeEl(tag),
  };
  try {
    const { createStatusView } = await import('./status_view.js');
    const root = new FakeEl('div');
    const view = createStatusView({ root });
    const threeHoursAgo = new Date(Date.now() - 3 * 3600 * 1000).toISOString();
    const recent = new Date(Date.now() - 30 * 1000).toISOString();
    view.render({
      status: {
        agents: [{ name: 'ada', state: 'working', text: 'fixing', updatedAt: recent }],
        resources: [{ name: 'repo', availability: 'free', updatedAt: recent }],
      },
      busHealth: { daemon: 'up', transport: 'up' },
      dashboardPanels: [
        { id: 'p-text', enabled: true, order: 0 },
        { id: 'resources', enabled: false, order: 1 },
      ],
      panels: [
        { id: 'p-text', title: 'Note', kind: 'Text', data: { text: 'hello world' }, updatedAt: recent, stale: false },
        { id: 'p-list', title: 'Checks', kind: 'List', data: { rows: [{ label: 'ci', value: 'pass', state: 'ok' }] }, updatedAt: recent, stale: false },
        { id: 'p-gauge', title: 'CPU', kind: 'Gauge', data: { value: 82, max: 100, warn: 75, fail: 95, unit: '%' }, updatedAt: recent, stale: false },
        { id: 'p-chart', title: 'Quota', kind: 'Chart', data: { series: [{ t: recent, v: 40 }], unit: '%', limit: 100, projection: { to: 100, at: '2026-11-26T20:00:00Z' } }, updatedAt: recent, stale: false },
        { id: 'p-table', title: 'Runs', kind: 'Table', data: { columns: ['Job', 'State'], rows: [['1', 'ok'], ['2', 'ok'], ['3', 'ok'], ['4', 'ok'], ['5', 'ok'], ['6', 'fail']] }, updatedAt: recent, stale: false },
        { id: 'p-unk', title: 'Widget', kind: 'Custom', data: { ok: true }, updatedAt: recent, stale: false },
        { id: 'p-stale', title: 'Old Feed', kind: 'Text', data: { text: 'old' }, updatedAt: threeHoursAgo, stale: true },
      ],
    });

    const cards = root.querySelectorAll('section.panel-card');
    const ids = cards.map((c) => c.attrs['data-panel-id']);
    assert.deepEqual(ids, ['p-text', 'agents', 'bus-health', 'p-list', 'p-gauge', 'p-chart', 'p-table', 'p-unk', 'p-stale']);
    const staleCard = cards.find((c) => c.attrs['data-panel-id'] === 'p-stale');
    assert.ok(staleCard.className.split(' ').includes('dim'));
    const text = root.allText();
    for (const want of [
      'hello world', 'ci', 'pass', 'ok', '82/100% (82%)',
      'latest 40%', 'limit 100%', 'projected 100% at 2026-11-26T20:00:00Z',
      '+1 more', '"ok": true', 'stale · updated 3h ago',
      'ada', 'fixing', 'Bus Health', 'daemon',
    ]) {
      assert.ok(text.includes(want), `missing ${want} in:\n${text}`);
    }
    assert.equal(text.includes('Resources and checkouts'), false);
    assert.equal(root.querySelectorAll('svg').length, 2);
  } finally {
    globalThis.document = prevDoc;
  }
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

// --- flag reasons and asks ---
test('flag reason grouping, escalation earlier item, validation, and message target resolution', () => {
  const reasons = [
    { id: 'verdict-not-first', category: 'format', label: 'verdict not first' },
    { id: 'vague-pronoun', category: 'main-text', label: 'vague pronoun' },
    {
      id: 'already-decided',
      category: 'escalation',
      label: 'should not have been asked: an earlier human decision covers it',
      allowEarlierItem: true,
    },
    { id: 'other', category: 'other', label: 'other (note required)', requireNote: true },
  ];
  const groups = groupReasons(reasons);
  assert.deepEqual(groups.map((g) => g.category), ['format', 'main-text', 'escalation', 'other']);
  assert.equal(allowsEarlierItem(reasons, ['vague-pronoun']), false);
  assert.equal(allowsEarlierItem(reasons, ['already-decided']), true);
  assert.equal(validateFlagInput(reasons, [], '').ok, false);
  assert.equal(validateFlagInput(reasons, ['other'], '  ').ok, false);
  assert.equal(validateFlagInput(reasons, ['already-decided'], '', 'D-1 D-2').ok, false);
  assert.deepEqual(validateFlagInput(reasons, ['other', 'vague-pronoun', 'other'], '  unclear it  ', 'D-001'), {
    ok: true,
    reasonIds: ['other', 'vague-pronoun'],
    note: 'unclear it',
    earlierItemId: '',
  });
  assert.deepEqual(validateFlagInput(reasons, ['already-decided'], '', '  D-001  '), {
    ok: true,
    reasonIds: ['already-decided'],
    note: '',
    earlierItemId: 'D-001',
  });

  const item = {
    id: 'D-1',
    owner: 'alice',
    question: 'Choose one?',
    messages: [
      { author: 'agent', by: 'worker/alice', text: 'First msg' },
      { author: 'human', text: 'Reply' },
      { author: 'system', text: 'System notice' },
    ],
  };
  assert.deepEqual(resolveClientFlagTarget(item, null), { messageIndex: 0, agent: 'worker/alice', preview: 'First msg' });
  assert.deepEqual(resolveClientFlagTarget({ id: 'D-2', owner: 'bob', question: 'Q' }, null), { messageIndex: null, agent: 'bob', preview: 'Q' });
});

test('flag sheet shows optional earlier item field for escalation and good sheet submits thumbs-up', async () => {
  class FakeSheetEl {
    constructor(tag) {
      this.tagName = tag.toUpperCase();
      this.attrs = {};
      this.children = [];
      this.listeners = {};
      this.textContent = '';
      this.value = '';
      this.checked = false;
      this.hidden = false;
      this.disabled = false;
    }
    setAttribute(k, v) { this.attrs[k] = String(v); if (k === 'hidden') this.hidden = true; }
    addEventListener(type, fn) { (this.listeners[type] ??= []).push(fn); }
    dispatch(type) { for (const fn of (this.listeners[type] ?? [])) fn({ target: this }); }
    append(...nodes) {
      for (const n of nodes) {
        if (typeof n === 'string') this.children.push({ tagName: '#TEXT', textContent: n, children: [], attrs: {} });
        else if (n != null) this.children.push(n);
      }
    }
    showModal() { this.open = true; }
    close() { this.open = false; this.dispatch('close'); }
    remove() {}
    focus() {}
    walk(fn) { fn(this); for (const c of this.children) c.walk?.(fn); }
    findAll(pred) {
      const out = [];
      this.walk((n) => { if (pred(n)) out.push(n); });
      return out;
    }
  }
  const prevDoc = globalThis.document;
  const bodyChildren = [];
  globalThis.document = {
    createElement: (tag) => new FakeSheetEl(tag),
    body: { append: (...nodes) => bodyChildren.push(...nodes) },
  };
  try {
    const reasons = [
      { id: 'vague-pronoun', category: 'main-text', label: 'vague pronoun' },
      { id: 'already-decided', category: 'escalation', label: 'should not have been asked: an earlier human decision covers it', allowEarlierItem: true },
    ];
    const item = { id: 'D-5', owner: 'worker/ada', question: 'Repeat?', messages: [{ author: 'agent', by: 'worker/ada', text: 'Repeat?' }] };
    let submittedFlag = null;
    const flagDlg = openFlagSheet({
      item,
      reasons,
      onSubmit: async (payload) => { submittedFlag = payload; },
    });
    const earlierWrap = flagDlg.findAll((n) => n.attrs?.class === 'flag-earlier-wrap')[0];
    const earlierInput = flagDlg.findAll((n) => n.attrs?.id === 'flag-earlier-item')[0];
    const escCheck = flagDlg.findAll((n) => n.attrs?.value === 'already-decided')[0];
    assert.equal(earlierWrap.hidden, true);
    escCheck.checked = true;
    escCheck.dispatch('change');
    assert.equal(earlierWrap.hidden, false);
    earlierInput.value = 'D-001';
    const saveFlag = flagDlg.findAll((n) => n.tagName === 'BUTTON' && n.textContent === 'Save flag')[0];
    await saveFlag.listeners.click[0]();
    assert.deepEqual(submittedFlag, {
      messageIndex: 0,
      reasons: ['already-decided'],
      note: '',
      earlierItemId: 'D-001',
    });

    let submittedGood = null;
    const goodDlg = openGoodSheet({
      item,
      onSubmit: async (payload) => { submittedGood = payload; },
    });
    const goodNote = goodDlg.findAll((n) => n.attrs?.id === 'good-note')[0];
    goodNote.value = 'concise options';
    const saveGood = goodDlg.findAll((n) => n.tagName === 'BUTTON' && n.textContent === 'Save 👍')[0];
    await saveGood.listeners.click[0]();
    assert.deepEqual(submittedGood, { messageIndex: 0, note: 'concise options' });
  } finally {
    globalThis.document = prevDoc;
  }
});

test('asks sorting, panel visibility, safe URLs, and SSE state updates', () => {
  const list = [
    { id: 'ASK-001', status: 'done', person: 'Bob', what: 'old done', createdAt: ago(300) },
    { id: 'ASK-003', status: 'open', person: 'Dana', what: 'newer open', createdAt: ago(60) },
    { id: 'ASK-002', status: 'open', person: 'Carol', what: 'older open', createdAt: ago(200) },
  ];
  assert.deepEqual(sortAsks(list).map((a) => a.id), ['ASK-002', 'ASK-003', 'ASK-001']);
  assert.equal(openAskCount(list), 2);
  assert.equal(asksPanel(list, false, now), null);
  const panel = asksPanel(list, true, now);
  assert.equal(panel.id, 'asks');
  assert.equal(panel.data.rows[0].value, '2');
  assert.equal(panel.data.rows[1].label, 'ASK-002');
  assert.equal(buildDashboardPanels({ status: null, asks: list, asksEnabled: true, now }).map((p) => p.id).includes('asks'), true);
  assert.equal(buildDashboardPanels({ status: null, asks: list, asksEnabled: false, now }).map((p) => p.id).includes('asks'), false);

  assert.equal(safeHttpURL('https://example.com/pr/1'), 'https://example.com/pr/1');
  assert.equal(safeHttpURL('javascript:alert(1)'), '');
  assert.deepEqual(parseRoute('#/asks'), { name: 'asks', arg: '' });

  const s1 = applyResync(emptyState(), {
    seq: 2,
    items: [],
    reasons: [{ id: 'other', category: 'other', label: 'other', requireNote: true }],
    asksEnabled: true,
    asks: [list[2]],
  });
  assert.equal(s1.asksEnabled, true);
  assert.equal(s1.asks.length, 1);
  const s2 = applyDelta(s1, { seq: 3, changes: [], asks: list });
  assert.equal(s2.asks.length, 3);
});
