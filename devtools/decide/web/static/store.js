// Client state: pure reducers (unit tested under node) plus the
// IndexedDB cache and EventSource wiring used only in the browser.

export const emptyState = () => ({
  seq: 0, items: [], status: null, drafts: null, contacts: null,
  orgTree: null, display: null, busHealth: null, syncedAt: null,
});

// applyResync replaces everything the event carries and keeps the rest.
export function applyResync(state, ev, now = Date.now()) {
  return {
    ...state,
    seq: ev.seq,
    items: [...(ev.items ?? [])],
    status: ev.status ?? state.status,
    drafts: ev.drafts ?? state.drafts,
    display: ev.display ?? state.display,
    busHealth: ev.busHealth ?? null,
    syncedAt: now,
  };
}

// applyDelta upserts or deletes changed items. A delta at or below the
// current seq is stale and ignored, so replays are harmless.
export function applyDelta(state, ev, now = Date.now()) {
  if (ev.seq < state.seq) return state;
  const byId = new Map(state.items.map((it) => [it.id, it]));
  const changes = [...(ev.changes ?? [])].sort((a, b) => a.seq - b.seq);
  for (const c of changes) {
    if (c.deleted) byId.delete(c.id);
    else if (c.item) byId.set(c.id, c.item);
  }
  const next = {
    ...state,
    seq: Math.max(state.seq, ev.seq),
    items: [...byId.values()],
    syncedAt: now,
  };
  for (const k of ['status', 'drafts', 'orgSpecModTime']) {
    if (ev[k] !== undefined) next[k] = ev[k];
  }
  if (ev.busHealth !== undefined) next.busHealth = ev.busHealth;
  return next;
}

export const classOf = (it) => it.class ? it.class[0].toUpperCase() : (it.id?.[0] ?? 'D');

const live = new Set(['open', 'done-unread']);

// unreadCount excludes notes, like the server's decide.UnreadCount.
export function unreadCount(items) {
  return items.filter((it) => it.unread && live.has(it.state) && classOf(it) !== 'N').length;
}

export const inboxItems = (items) => items
  .filter((it) => live.has(it.state))
  .sort((a, b) => String(b.createdAt).localeCompare(String(a.createdAt)));

const routeNames = new Set(['inbox', 'folder', 'item', 'compose', 'status', 'contacts', 'settings']);

// parseRoute turns a location hash into {name, arg}. Unknown routes fall
// back to the inbox.
export function parseRoute(hash) {
  const parts = String(hash ?? '').replace(/^#\/?/, '').split('/').filter(Boolean);
  const name = parts[0] ?? 'inbox';
  if (!routeNames.has(name)) return { name: 'inbox', arg: '' };
  let arg = '';
  try { arg = parts[1] ? decodeURIComponent(parts[1]) : ''; } catch { arg = ''; }
  if ((name === 'item' || name === 'folder') && !arg) return { name: 'inbox', arg: '' };
  return { name, arg };
}

// busDown mirrors the TUI: the bus cannot deliver while the daemon or
// transport is down.
export const busDown = (h) => !!h && (h.daemon === 'down' || h.transport === 'down');

const pad = (n) => String(n).padStart(2, '0');

// freshness is the top-bar indicator text.
export function freshness(conn, syncedAt) {
  if (conn === 'connected') return 'connected';
  if (conn === 'reconnecting') return 'reconnecting';
  if (!syncedAt) return 'offline';
  const d = new Date(syncedAt);
  return `offline · copy from ${pad(d.getHours())}:${pad(d.getMinutes())}`;
}

// --- browser only below ---

const DB = 'decide-web';

function openDB() {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open(DB, 1);
    req.onupgradeneeded = () => {
      req.result.createObjectStore('snapshot');
      req.result.createObjectStore('localDrafts');
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

async function idb(store, mode, fn) {
  const db = await openDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(store, mode);
    const r = fn(tx.objectStore(store));
    tx.oncomplete = () => resolve(r?.result);
    tx.onerror = () => reject(tx.error);
  });
}

export const loadSnapshot = () => idb('snapshot', 'readonly', (s) => s.get('state'));
export const saveSnapshot = (state) => idb('snapshot', 'readwrite', (s) => s.put(state, 'state'));
export const saveLocalDraft = (key, text) => idb('localDrafts', 'readwrite', (s) => s.put(text, key));
export const loadLocalDraft = (key) => idb('localDrafts', 'readonly', (s) => s.get(key));

// createStore hydrates from IndexedDB, then follows the event stream.
// subscribe(fn) calls fn(state, conn) on every change.
export function createStore() {
  let state = emptyState();
  let conn = 'offline';
  let es = null;
  const subs = new Set();
  const emit = () => subs.forEach((fn) => fn(state, conn));
  const persist = () => saveSnapshot(state).catch(() => {});
  const set = (s) => { state = s; persist(); emit(); };

  function connect() {
    es?.close();
    es = new EventSource('/api/v1/events?since=' + state.seq);
    es.addEventListener('open', () => { conn = 'connected'; emit(); });
    es.addEventListener('resync', (e) => set(applyResync(state, JSON.parse(e.data))));
    es.addEventListener('delta', (e) => set(applyDelta(state, JSON.parse(e.data))));
    es.addEventListener('error', () => {
      conn = es.readyState === EventSource.CLOSED ? 'offline' : 'reconnecting';
      emit();
    });
  }

  return {
    subscribe(fn) { subs.add(fn); fn(state, conn); return () => subs.delete(fn); },
    get state() { return state; },
    async start() {
      try {
        const cached = await loadSnapshot();
        if (cached) { state = { ...emptyState(), ...cached }; emit(); }
      } catch { /* no cache */ }
      connect();
    },
    stop() { es?.close(); es = null; conn = 'offline'; emit(); },
    reconnect: connect,
  };
}
