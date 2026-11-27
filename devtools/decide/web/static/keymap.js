// Pure keymap engine for decide web. Driven entirely by the server's
// GET /api/v1/keymap response (the same action registry and user ui.keys
// bindings the terminal inbox uses). No DOM dependency so it runs under node.

// Actions implemented by the web UI. Actions outside this set (or bound only
// to browser-reserved keys) are listed as "not available on web" in the ?
// overlay and never intercepted.
export const WEB_ACTIONS = new Set([
  'help',
  'search',
  'goto.inbox',
  'goto.snoozed',
  'goto.answered',
  'goto.done',
  'goto.withdrawn',
  'goto.status',
  'goto.contacts',
  'goto.outbox',
  'goto.asks',
  'goto.top',
  'list.next',
  'list.prev',
  'list.pageDown',
  'list.pageUp',
  'list.top',
  'list.bottom',
  'list.open',
  'list.compose',
  'list.archive',
  'list.snooze',
  'list.flag',
  'list.good',
  'list.clearFilter',
  'detail.back',
  'detail.scrollDown',
  'detail.scrollUp',
  'detail.pageDown',
  'detail.pageUp',
  'detail.top',
  'detail.bottom',
  'detail.nextMessage',
  'detail.prevMessage',
  'detail.archive',
  'detail.snooze',
  'detail.flag',
  'detail.good',
  'detail.compose',
  'detail.forward',
  'detail.outcome',
  'detail.reply',
  'status.next',
  'status.prev',
  'status.pageDown',
  'status.pageUp',
  'status.top',
  'status.bottom',
  'status.back',
  'status.compose',
  'status.clearFilter',
  'contacts.next',
  'contacts.prev',
  'contacts.pageDown',
  'contacts.pageUp',
  'contacts.top',
  'contacts.bottom',
  'contacts.back',
  'contacts.filter',
  'contacts.clearFilter',
  'contacts.compose',
  'contacts.star',
  'contacts.orgTree',
  'compose.send',
  'compose.sendAndArchive',
  'compose.discard',
  'compose.cancel',
]);

const VIEW_LABELS = {
  global: 'Global',
  list: 'List',
  detail: 'Item',
  status: 'Status',
  contacts: 'Contacts',
  outbox: 'Outbox',
  compose: 'Compose',
  editor: 'Editor',
};

const RESERVED_STROKES = new Set([
  'tab',
  'shift+tab',
  'ctrl+w',
  'ctrl+t',
  'ctrl+l',
  'ctrl+r',
]);

// isReservedStroke reports whether a canonical stroke stays with the browser.
export function isReservedStroke(stroke) {
  if (!stroke) return false;
  const lower = String(stroke).toLowerCase();
  return RESERVED_STROKES.has(lower) || lower.startsWith('meta+');
}

// isReservedSeq reports whether any stroke in a key sequence is browser-reserved.
export function isReservedSeq(seq) {
  const s = String(seq ?? '').trim();
  if (!s) return false;
  return s.split(' ').some(isReservedStroke);
}

const NAMED_KEYS = {
  Enter: 'enter',
  Escape: 'esc',
  Tab: 'tab',
  ArrowUp: 'up',
  ArrowDown: 'down',
  ArrowLeft: 'left',
  ArrowRight: 'right',
  PageUp: 'pgup',
  PageDown: 'pgdn',
  Home: 'home',
  End: 'end',
  Backspace: 'backspace',
  Delete: 'delete',
  ' ': 'space',
};

const MODIFIER_KEYS = new Set(['Shift', 'Control', 'Alt', 'Meta', 'CapsLock', 'NumLock', 'ScrollLock']);

// eventToStroke converts a KeyboardEvent-like object into the canonical
// stroke notation used by decide.KeyStroke.String(), or null when the event
// should stay with the browser (modifiers, Cmd+..., Tab, Ctrl+W/T/L/R).
export function eventToStroke(e) {
  if (!e || e.isComposing || !e.key || MODIFIER_KEYS.has(e.key)) return null;
  if (e.metaKey) return null;
  if (e.key === 'Tab') return null;

  let key = NAMED_KEYS[e.key];
  if (!key) {
    if (e.altKey && !e.ctrlKey && typeof e.code === 'string' && /^Key[A-Z]$/.test(e.code)) {
      key = e.code.slice(3).toLowerCase();
    } else if ([...e.key].length === 1) {
      key = e.key;
    } else {
      return null;
    }
  }

  // Ctrl+Enter normalizes to alt+enter, matching decide.ParseKeySeq and TUI.
  if (e.ctrlKey && !e.altKey && key === 'enter') {
    return 'alt+enter';
  }

  if (e.ctrlKey && key.length === 1 && key >= 'A' && key <= 'Z') {
    key = key.toLowerCase();
  }

  const parts = [];
  if (e.ctrlKey) parts.push('ctrl');
  if (e.altKey) parts.push('alt');
  parts.push(key);
  const stroke = parts.join('+');
  return isReservedStroke(stroke) ? null : stroke;
}

// isTextField reports whether target is an editable text input.
export function isTextField(target) {
  return !!target && typeof target.closest === 'function'
    && target.closest('input, textarea, select, [contenteditable]') !== null;
}

// createKeymap builds a stateful keyboard dispatcher and hint lookup from the
// server's /api/v1/keymap payload.
export function createKeymap(data, {
  setTimer = (fn, ms) => setTimeout(fn, ms),
  clearTimer = (t) => clearTimeout(t),
  onChordChange = () => {},
} = {}) {
  const actions = Array.isArray(data?.actions) ? data.actions : [];
  const chordTimeoutMs = Number(data?.chordTimeoutMs) > 0 ? Number(data.chordTimeoutMs) : 1000;
  const byName = new Map();
  // scope -> Map<canonicalSeq, actionName>
  const scopeExact = new Map();
  // scope -> Map<prefixStroke, Array<{next, action, helpNext}>>
  const scopePrefixes = new Map();

  for (const act of actions) {
    byName.set(act.name, act);
    if (!WEB_ACTIONS.has(act.name)) continue;
    const keys = act.keys ?? [];
    const scopes = act.scopes ?? [];
    for (let i = 0; i < keys.length; i++) {
      const seq = keys[i];
      if (!seq || isReservedSeq(seq)) continue;
      const strokes = seq.split(' ');
      for (const scope of scopes) {
        if (!scopeExact.has(scope)) scopeExact.set(scope, new Map());
        if (!scopePrefixes.has(scope)) scopePrefixes.set(scope, new Map());
        scopeExact.get(scope).set(seq, act.name);
        if (strokes.length === 2) {
          const [prefix, next] = strokes;
          const prefMap = scopePrefixes.get(scope);
          if (!prefMap.has(prefix)) prefMap.set(prefix, []);
          const helpParts = (act.helpKeys?.[i] ?? seq).split(' ');
          prefMap.get(prefix).push({ next, action: act.name, helpNext: helpParts[1] ?? next });
        }
      }
    }
  }

  let pending = null;
  let timer = null;

  function clearChord() {
    if (timer !== null) {
      clearTimer(timer);
      timer = null;
    }
    if (pending !== null) {
      pending = null;
      onChordChange(null);
    }
  }

  function isSendStroke(stroke) {
    return scopeExact.get('compose')?.get(stroke) === 'compose.send'
      || scopeExact.get('editor')?.get(stroke) === 'compose.send';
  }

  // handleKey processes one keydown event in the given scope.
  // Returns null if the event should be ignored / left to the browser, or:
  //   { type: 'action', action: string }
  //   { type: 'answer', index: number } // 1..9 in detail scope
  //   { type: 'chord-start', prefix: string, entries: Array<{next, action, helpNext}> }
  //   { type: 'chord-cancel' }
  //   { type: 'chord-miss', seq: string }
  function handleKey(e, { scope = 'list', typing = false } = {}) {
    const stroke = eventToStroke(e);
    if (!stroke) return null;

    if (typing) {
      clearChord();
      if (stroke === 'esc') return { type: 'action', action: 'compose.cancel' };
      if (isSendStroke(stroke)) return { type: 'action', action: 'compose.send' };
      return null;
    }

    if (pending !== null) {
      const prefix = pending;
      clearChord();
      if (stroke === 'esc') return { type: 'chord-cancel' };
      const act = scopeExact.get(scope)?.get(prefix + ' ' + stroke);
      if (act) return { type: 'action', action: act };
      return { type: 'chord-miss', seq: prefix + ' ' + stroke };
    }

    const entries = scopePrefixes.get(scope)?.get(stroke);
    if (entries && entries.length > 0) {
      pending = stroke;
      onChordChange({ prefix: stroke, entries });
      timer = setTimer(() => {
        timer = null;
        if (pending !== null) {
          pending = null;
          onChordChange(null);
        }
      }, chordTimeoutMs);
      return { type: 'chord-start', prefix: stroke, entries };
    }

    const act = scopeExact.get(scope)?.get(stroke);
    if (act) return { type: 'action', action: act };

    if (scope === 'detail' && /^[1-9]$/.test(stroke)) {
      return { type: 'answer', index: Number(stroke) };
    }

    return null;
  }

  // hint returns the primary non-reserved button key hint for action, or ''.
  function hint(actionName) {
    const act = byName.get(actionName);
    if (!act || !WEB_ACTIONS.has(actionName)) return '';
    const keys = act.keys ?? [];
    for (let i = 0; i < keys.length; i++) {
      if (!isReservedSeq(keys[i])) {
        return act.buttonKeys?.[i] ?? act.helpKeys?.[i] ?? keys[i] ?? '';
      }
    }
    return '';
  }

  // helpSections builds the structured sections for the ? overlay directly
  // from the server registry. Actions unavailable on web (TUI-only or bound
  // only to browser-reserved keys) are marked status: 'unavailable' with
  // displayKeys: 'not available on web'.
  function helpSections() {
    const sections = [];
    const byView = new Map();
    const sectionFor = (view) => {
      let s = byView.get(view);
      if (!s) {
        s = { view, title: VIEW_LABELS[view] ?? view, rows: [] };
        byView.set(view, s);
        sections.push(s);
      }
      return s;
    };

    for (const act of actions) {
      const s = sectionFor(act.view);
      const keys = act.keys ?? [];
      const helpKeys = act.helpKeys ?? keys;
      if (keys.length === 0) {
        s.rows.push({
          name: act.name,
          description: act.description,
          where: act.where,
          status: 'unbound',
          keys: [],
          displayKeys: '(unbound)',
        });
        continue;
      }
      const webHelpKeys = [];
      for (let i = 0; i < keys.length; i++) {
        if (!isReservedSeq(keys[i])) webHelpKeys.push(helpKeys[i] ?? keys[i]);
      }
      if (!WEB_ACTIONS.has(act.name) || webHelpKeys.length === 0) {
        s.rows.push({
          name: act.name,
          description: act.description,
          where: act.where,
          status: 'unavailable',
          keys: helpKeys,
          displayKeys: 'not available on web',
        });
        continue;
      }
      s.rows.push({
        name: act.name,
        description: act.description,
        where: act.where,
        status: 'available',
        keys: webHelpKeys,
        displayKeys: webHelpKeys.join(' / '),
      });
    }

    const detail = byView.get('detail');
    if (detail) {
      detail.rows.push({
        name: 'detail.answer',
        description: 'Answer with that option.',
        where: 'item',
        status: 'available',
        keys: ['1–9'],
        displayKeys: '1–9',
      });
    }
    return sections;
  }

  return {
    handleKey,
    hint,
    helpSections,
    clearChord,
    get pendingChord() { return pending; },
    get chordTimeoutMs() { return chordTimeoutMs; },
  };
}
