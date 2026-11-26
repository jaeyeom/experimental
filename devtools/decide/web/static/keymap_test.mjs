import test from 'node:test';
import assert from 'node:assert/strict';
import { createKeymap, eventToStroke, isReservedStroke, isReservedSeq } from './keymap.js';

const browse = ['list', 'detail', 'status', 'contacts'];

// Representative registry matching decide.ActionCatalog() defaults.
const defaultPayload = () => ({
  chordTimeoutMs: 1000,
  asciiKeys: false,
  actions: [
    { name: 'quit', view: 'global', scopes: browse, description: 'Quit.', where: 'any', keys: ['q'], helpKeys: ['q'], buttonKeys: ['q'] },
    { name: 'help', view: 'global', scopes: browse, description: 'Show shortcuts.', where: 'any', keys: ['?'], helpKeys: ['?'], buttonKeys: ['?'] },
    { name: 'mouse', view: 'global', scopes: browse, description: 'Toggle mouse.', where: 'any', keys: ['M'], helpKeys: ['M'], buttonKeys: ['M'] },
    { name: 'search', view: 'global', scopes: browse, description: 'Filter list.', where: 'any', keys: ['/'], helpKeys: ['/'], buttonKeys: ['/'] },
    { name: 'goto.inbox', view: 'global', scopes: browse, description: 'Go to Inbox.', where: 'any', keys: ['g i'], helpKeys: ['g i'], buttonKeys: ['g i'] },
    { name: 'goto.snoozed', view: 'global', scopes: browse, description: 'Go to Snoozed.', where: 'any', keys: ['g z'], helpKeys: ['g z'], buttonKeys: ['g z'] },
    { name: 'goto.answered', view: 'global', scopes: browse, description: 'Go to Answered.', where: 'any', keys: ['g a'], helpKeys: ['g a'], buttonKeys: ['g a'] },
    { name: 'goto.done', view: 'global', scopes: browse, description: 'Go to Done.', where: 'any', keys: ['g d'], helpKeys: ['g d'], buttonKeys: ['g d'] },
    { name: 'goto.withdrawn', view: 'global', scopes: browse, description: 'Go to Withdrawn.', where: 'any', keys: ['g w'], helpKeys: ['g w'], buttonKeys: ['g w'] },
    { name: 'goto.status', view: 'global', scopes: browse, description: 'Go to Status.', where: 'any', keys: ['g s'], helpKeys: ['g s'], buttonKeys: ['g s'] },
    { name: 'goto.contacts', view: 'global', scopes: browse, description: 'Go to Contacts.', where: 'any', keys: ['g c'], helpKeys: ['g c'], buttonKeys: ['g c'] },
    { name: 'goto.outbox', view: 'global', scopes: browse, description: 'Go to Outbox.', where: 'any', keys: ['g o'], helpKeys: ['g o'], buttonKeys: ['g o'] },
    { name: 'goto.top', view: 'global', scopes: browse, description: 'Top of list or item.', where: 'any', keys: ['g g'], helpKeys: ['g g'], buttonKeys: ['g g'] },

    { name: 'list.next', view: 'list', scopes: ['list'], description: 'Next row.', where: 'list', keys: ['j'], helpKeys: ['j'], buttonKeys: ['j'] },
    { name: 'list.prev', view: 'list', scopes: ['list'], description: 'Previous row.', where: 'list', keys: ['k'], helpKeys: ['k'], buttonKeys: ['k'] },
    { name: 'list.top', view: 'list', scopes: ['list'], description: 'Top of list.', where: 'list', keys: ['home'], helpKeys: ['home'], buttonKeys: ['home'] },
    { name: 'list.bottom', view: 'list', scopes: ['list'], description: 'Bottom of list.', where: 'list', keys: ['G', 'end'], helpKeys: ['G', 'end'], buttonKeys: ['G', 'end'] },
    { name: 'list.open', view: 'list', scopes: ['list'], description: 'Open row.', where: 'list', keys: ['o', 'enter'], helpKeys: ['o', 'enter'], buttonKeys: ['o', '⏎'] },
    { name: 'list.compose', view: 'list', scopes: ['list'], description: 'Compose.', where: 'list', keys: ['c'], helpKeys: ['c'], buttonKeys: ['c'] },
    { name: 'list.archive', view: 'list', scopes: ['list'], description: 'Archive.', where: 'list', keys: ['e', 'y'], helpKeys: ['e', 'y'], buttonKeys: ['e', 'y'] },
    { name: 'list.snooze', view: 'list', scopes: ['list'], description: 'Snooze.', where: 'list', keys: ['b'], helpKeys: ['b'], buttonKeys: ['b'] },
    { name: 'list.clearFilter', view: 'list', scopes: ['list'], description: 'Clear filter.', where: 'list', keys: ['esc'], helpKeys: ['esc'], buttonKeys: ['Esc'] },

    { name: 'detail.back', view: 'detail', scopes: ['detail'], description: 'Back.', where: 'item', keys: ['u', 'esc'], helpKeys: ['u', 'esc'], buttonKeys: ['u', 'Esc'] },
    { name: 'detail.scrollDown', view: 'detail', scopes: ['detail'], description: 'Scroll down.', where: 'item', keys: ['j', 'down'], helpKeys: ['j', 'down'], buttonKeys: ['j', '↓'] },
    { name: 'detail.scrollUp', view: 'detail', scopes: ['detail'], description: 'Scroll up.', where: 'item', keys: ['k', 'up'], helpKeys: ['k', 'up'], buttonKeys: ['k', '↑'] },
    { name: 'detail.top', view: 'detail', scopes: ['detail'], description: 'Top.', where: 'item', keys: ['home'], helpKeys: ['home'], buttonKeys: ['home'] },
    { name: 'detail.bottom', view: 'detail', scopes: ['detail'], description: 'Bottom.', where: 'item', keys: ['G', 'end'], helpKeys: ['G', 'end'], buttonKeys: ['G', 'end'] },
    { name: 'detail.nextMessage', view: 'detail', scopes: ['detail'], description: 'Next message.', where: 'item', keys: ['n'], helpKeys: ['n'], buttonKeys: ['n'] },
    { name: 'detail.prevMessage', view: 'detail', scopes: ['detail'], description: 'Previous message.', where: 'item', keys: ['p'], helpKeys: ['p'], buttonKeys: ['p'] },
    { name: 'detail.archive', view: 'detail', scopes: ['detail'], description: 'Archive.', where: 'item', keys: ['e', 'y'], helpKeys: ['e', 'y'], buttonKeys: ['e', 'y'] },
    { name: 'detail.snooze', view: 'detail', scopes: ['detail'], description: 'Snooze.', where: 'item', keys: ['b'], helpKeys: ['b'], buttonKeys: ['b'] },
    { name: 'detail.compose', view: 'detail', scopes: ['detail'], description: 'Compose.', where: 'item', keys: ['c'], helpKeys: ['c'], buttonKeys: ['c'] },
    { name: 'detail.outcome', view: 'detail', scopes: ['detail'], description: 'Archive with note.', where: 'item', keys: ['E'], helpKeys: ['E'], buttonKeys: ['E'] },
    { name: 'detail.reply', view: 'detail', scopes: ['detail'], description: 'Focus reply box.', where: 'item', keys: ['0'], helpKeys: ['0'], buttonKeys: ['0'] },

    { name: 'compose.send', view: 'compose', scopes: ['editor', 'compose'], description: 'Send.', where: 'every composer', keys: ['alt+enter', 'ctrl+s'], helpKeys: ['Alt+Enter', 'C-s'], buttonKeys: ['^⏎', '^S'] },
    { name: 'compose.nextField', view: 'compose', scopes: ['editor', 'compose', 'picker'], description: 'Next field.', where: 'compose', keys: ['tab'], helpKeys: ['tab'], buttonKeys: ['Tab'] },
    { name: 'compose.prevField', view: 'compose', scopes: ['editor', 'compose', 'picker'], description: 'Prev field.', where: 'compose', keys: ['shift+tab'], helpKeys: ['shift+tab'], buttonKeys: ['S-Tab'] },
    { name: 'compose.cancel', view: 'compose', scopes: ['editor', 'compose', 'picker'], description: 'Cancel.', where: 'compose', keys: ['esc'], helpKeys: ['esc'], buttonKeys: ['Esc'] },
  ],
});

const ev = (key, mods = {}) => ({ key, ctrlKey: false, altKey: false, metaKey: false, shiftKey: false, ...mods });

test('each action fires exactly once for its key', () => {
  const km = createKeymap(defaultPayload());
  const fired = [];
  const press = (events, opts) => {
    fired.length = 0;
    for (const e of events) {
      const r = km.handleKey(e, opts);
      if (r?.type === 'action') fired.push(r.action);
      if (r?.type === 'answer') fired.push(`answer:${r.index}`);
    }
    return [...fired];
  };

  // Folders and jumps
  assert.deepEqual(press([ev('g'), ev('i')], { scope: 'list' }), ['goto.inbox']);
  assert.deepEqual(press([ev('g'), ev('z')], { scope: 'list' }), ['goto.snoozed']);
  assert.deepEqual(press([ev('g'), ev('a')], { scope: 'list' }), ['goto.answered']);
  assert.deepEqual(press([ev('g'), ev('d')], { scope: 'list' }), ['goto.done']);
  assert.deepEqual(press([ev('g'), ev('w')], { scope: 'list' }), ['goto.withdrawn']);
  assert.deepEqual(press([ev('g'), ev('s')], { scope: 'list' }), ['goto.status']);
  assert.deepEqual(press([ev('g'), ev('c')], { scope: 'list' }), ['goto.contacts']);
  assert.deepEqual(press([ev('g'), ev('o')], { scope: 'list' }), ['goto.outbox']);
  assert.deepEqual(press([ev('g'), ev('g')], { scope: 'list' }), ['goto.top']);
  assert.deepEqual(press([ev('Home')], { scope: 'list' }), ['list.top']);
  assert.deepEqual(press([ev('G', { shiftKey: true })], { scope: 'list' }), ['list.bottom']);
  assert.deepEqual(press([ev('End')], { scope: 'list' }), ['list.bottom']);

  // Navigation
  assert.deepEqual(press([ev('j')], { scope: 'list' }), ['list.next']);
  assert.deepEqual(press([ev('k')], { scope: 'list' }), ['list.prev']);
  assert.deepEqual(press([ev('o')], { scope: 'list' }), ['list.open']);
  assert.deepEqual(press([ev('Enter')], { scope: 'list' }), ['list.open']);
  assert.deepEqual(press([ev('u')], { scope: 'detail' }), ['detail.back']);
  assert.deepEqual(press([ev('Escape')], { scope: 'detail' }), ['detail.back']);
  assert.deepEqual(press([ev('n')], { scope: 'detail' }), ['detail.nextMessage']);
  assert.deepEqual(press([ev('p')], { scope: 'detail' }), ['detail.prevMessage']);

  // Answering
  for (let i = 1; i <= 9; i++) {
    assert.deepEqual(press([ev(String(i))], { scope: 'detail' }), [`answer:${i}`]);
  }
  assert.deepEqual(press([ev('0')], { scope: 'detail' }), ['detail.reply']);

  // Item actions
  assert.deepEqual(press([ev('e')], { scope: 'list' }), ['list.archive']);
  assert.deepEqual(press([ev('y')], { scope: 'list' }), ['list.archive']);
  assert.deepEqual(press([ev('e')], { scope: 'detail' }), ['detail.archive']);
  assert.deepEqual(press([ev('y')], { scope: 'detail' }), ['detail.archive']);
  assert.deepEqual(press([ev('b')], { scope: 'list' }), ['list.snooze']);
  assert.deepEqual(press([ev('b')], { scope: 'detail' }), ['detail.snooze']);
  assert.deepEqual(press([ev('E', { shiftKey: true })], { scope: 'detail' }), ['detail.outcome']);
  assert.equal(km.handleKey(ev('d'), { scope: 'detail' }), null, 'd is unbound by default');
  assert.equal(km.handleKey(ev('E', { shiftKey: true }), { scope: 'list' }), null, 'E does nothing in list');
  assert.deepEqual(press([ev('c')], { scope: 'list' }), ['list.compose']);
  assert.deepEqual(press([ev('c')], { scope: 'detail' }), ['detail.compose']);

  // Search and help
  assert.deepEqual(press([ev('/')], { scope: 'list' }), ['search']);
  assert.deepEqual(press([ev('?', { shiftKey: true })], { scope: 'list' }), ['help']);

  // Composer send
  assert.deepEqual(press([ev('Enter', { ctrlKey: true })], { scope: 'detail', typing: true }), ['compose.send']);
  assert.deepEqual(press([ev('Enter', { altKey: true })], { scope: 'detail', typing: true }), ['compose.send']);
});

test('keys are ignored while typing in a text field, except send and Esc', () => {
  const km = createKeymap(defaultPayload());
  for (const key of ['j', 'k', 'e', 'y', 'b', 'E', 'c', 'g', '/', '?', '0', '1', 'u', 'Enter']) {
    assert.equal(km.handleKey(ev(key), { scope: 'detail', typing: true }), null, `key ${key} must be ignored while typing`);
  }
  assert.deepEqual(km.handleKey(ev('Enter', { ctrlKey: true }), { scope: 'detail', typing: true }), { type: 'action', action: 'compose.send' });
  assert.deepEqual(km.handleKey(ev('Enter', { altKey: true }), { scope: 'detail', typing: true }), { type: 'action', action: 'compose.send' });
  assert.deepEqual(km.handleKey(ev('Escape'), { scope: 'detail', typing: true }), { type: 'action', action: 'compose.cancel' });
});

test('browser-reserved keys stay with the browser', () => {
  assert.equal(isReservedStroke('tab'), true);
  assert.equal(isReservedStroke('shift+tab'), true);
  assert.equal(isReservedStroke('ctrl+w'), true);
  assert.equal(isReservedStroke('ctrl+t'), true);
  assert.equal(isReservedStroke('ctrl+l'), true);
  assert.equal(isReservedStroke('ctrl+r'), true);
  assert.equal(isReservedSeq('ctrl+w'), true);
  assert.equal(isReservedSeq('g i'), false);

  for (const e of [
    ev('w', { ctrlKey: true }),
    ev('t', { ctrlKey: true }),
    ev('l', { ctrlKey: true }),
    ev('r', { ctrlKey: true }),
    ev('w', { metaKey: true }),
    ev('t', { metaKey: true }),
    ev('l', { metaKey: true }),
    ev('r', { metaKey: true }),
    ev('Tab'),
    ev('Tab', { shiftKey: true }),
  ]) {
    assert.equal(eventToStroke(e), null);
  }
});

test('custom binding from user config applies on the web', () => {
  const payload = defaultPayload();
  const archive = payload.actions.find((a) => a.name === 'list.archive');
  archive.keys = ['x'];
  archive.helpKeys = ['x'];
  archive.buttonKeys = ['x'];

  const send = payload.actions.find((a) => a.name === 'compose.send');
  send.keys = ['ctrl+g'];
  send.helpKeys = ['C-g'];
  send.buttonKeys = ['^G'];

  const km = createKeymap(payload);
  assert.equal(km.handleKey(ev('e'), { scope: 'list' }), null, 'old default no longer fires');
  assert.deepEqual(km.handleKey(ev('x'), { scope: 'list' }), { type: 'action', action: 'list.archive' });
  assert.equal(km.hint('list.archive'), 'x');

  assert.equal(km.handleKey(ev('Enter', { ctrlKey: true }), { scope: 'compose', typing: true }), null, 'old send key unbound');
  assert.deepEqual(km.handleKey(ev('g', { ctrlKey: true }), { scope: 'compose', typing: true }), { type: 'action', action: 'compose.send' });
  assert.equal(km.hint('compose.send'), '^G');
});

test('chord times out after chordTimeoutMs', () => {
  let scheduledFn = null;
  let scheduledMs = 0;
  let cleared = false;
  const states = [];
  const km = createKeymap(defaultPayload(), {
    setTimer: (fn, ms) => { scheduledFn = fn; scheduledMs = ms; return 42; },
    clearTimer: () => { cleared = true; },
    onChordChange: (st) => { states.push(st); },
  });

  const start = km.handleKey(ev('g'), { scope: 'list' });
  assert.equal(start?.type, 'chord-start');
  assert.equal(km.pendingChord, 'g');
  assert.equal(scheduledMs, 1000);
  assert.equal(states.length, 1);
  assert.equal(states[0].prefix, 'g');

  // Fire the timeout callback.
  scheduledFn();
  assert.equal(km.pendingChord, null);
  assert.equal(states[states.length - 1], null);

  // Subsequent 'i' does NOT trigger goto.inbox after timeout.
  assert.equal(km.handleKey(ev('i'), { scope: 'list' }), null);

  // Starting a chord and pressing Esc cancels it cleanly.
  km.handleKey(ev('g'), { scope: 'list' });
  const cancel = km.handleKey(ev('Escape'), { scope: 'list' });
  assert.deepEqual(cancel, { type: 'chord-cancel' });
  assert.equal(cleared, true);
  assert.equal(km.pendingChord, null);
});

test('help overlay sections match registry including not available on web entries', () => {
  const payload = defaultPayload();
  // Rebind list.snooze exclusively to a browser-reserved key.
  const snooze = payload.actions.find((a) => a.name === 'list.snooze');
  snooze.keys = ['ctrl+w'];
  snooze.helpKeys = ['C-w'];
  snooze.buttonKeys = ['^W'];

  const km = createKeymap(payload);
  // ctrl+w is never intercepted and produces no button hint.
  assert.equal(km.handleKey(ev('w', { ctrlKey: true }), { scope: 'list' }), null);
  assert.equal(km.hint('list.snooze'), '');

  const sections = km.helpSections();
  const rows = sections.flatMap((s) => s.rows);
  const byName = new Map(rows.map((r) => [r.name, r]));

  // Every registry action appears in the help overlay.
  for (const act of payload.actions) {
    assert.ok(byName.has(act.name), `missing ${act.name} from help overlay`);
  }
  // Browser-reserved-only actions are marked "not available on web".
  assert.equal(byName.get('compose.nextField').status, 'unavailable');
  assert.equal(byName.get('compose.nextField').displayKeys, 'not available on web');
  assert.equal(byName.get('compose.prevField').status, 'unavailable');
  assert.equal(byName.get('compose.prevField').displayKeys, 'not available on web');
  assert.equal(byName.get('list.snooze').status, 'unavailable');
  assert.equal(byName.get('list.snooze').displayKeys, 'not available on web');
  // TUI-only actions are also marked "not available on web".
  assert.equal(byName.get('quit').status, 'unavailable');
  assert.equal(byName.get('quit').displayKeys, 'not available on web');
  // Available actions show their live help keys.
  assert.equal(byName.get('goto.inbox').status, 'available');
  assert.equal(byName.get('goto.inbox').displayKeys, 'g i');
  assert.equal(byName.get('list.archive').displayKeys, 'e / y');
  assert.equal(byName.get('detail.answer').displayKeys, '1–9');
});
