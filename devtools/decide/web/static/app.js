// App shell: auth gate, theme, router, and rendering. DOM is built with
// textContent only; item text is never interpreted as HTML.
import * as api from './api.js';
import { createStore, parseRoute, unreadCount, inboxItems, classOf, freshness, busDown } from './store.js';

const $ = (id) => document.getElementById(id);
const el = (tag, attrs = {}, ...kids) => {
  const n = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === 'text') n.textContent = v;
    else if (v === true) n.setAttribute(k, '');
    else if (v !== false && v != null) n.setAttribute(k, v);
  }
  n.append(...kids);
  return n;
};

// Theme: system | light | dark, persisted per browser.
const THEMES = ['system', 'light', 'dark'];
function getTheme() {
  try { const t = localStorage.getItem('theme'); if (THEMES.includes(t)) return t; } catch { /* private mode */ }
  return 'system';
}
function setTheme(t) {
  try { localStorage.setItem('theme', t); } catch { /* private mode */ }
  applyTheme(t);
}
function applyTheme(t) {
  if (t === 'system') document.documentElement.removeAttribute('data-theme');
  else document.documentElement.setAttribute('data-theme', t);
}
applyTheme(getTheme());

const store = createStore();
let started = false;
let route = parseRoute(location.hash);

function showLogin(msg) {
  store.stop();
  started = false;
  $('app').hidden = true;
  $('login').hidden = false;
  const err = $('login-error');
  err.hidden = !msg;
  err.textContent = msg ?? '';
  $('token').focus();
}

async function boot() {
  let st;
  try { st = await api.authStatus(); } catch { st = null; }
  if (st && st.authRequired && !st.authenticated) { showLogin(); return; }
  $('login').hidden = true;
  $('app').hidden = false;
  if (!started) { started = true; await store.start(); }
  render();
}

api.setUnauthorizedHandler(() => showLogin());

$('login-form').addEventListener('submit', async (e) => {
  e.preventDefault();
  try {
    await api.login($('token').value);
    $('token').value = '';
    await boot();
  } catch (err) {
    showLogin(api.unauthorized(err) ? 'Wrong token.' : 'Cannot reach the server.');
  }
});

const NAV = [['inbox', 'Inbox'], ['folder', 'Folders'], ['status', 'Status'], ['contacts', 'Contacts']];

function renderNav(state) {
  const nav = $('nav');
  nav.replaceChildren(...NAV.map(([name, label]) => {
    const a = el('a', { href: name === 'folder' ? '#/folder/open' : '#/' + name, text: label,
      'aria-current': route.name === name ? 'page' : false });
    const n = name === 'inbox' ? unreadCount(state.items) : 0;
    if (n > 0) a.append(el('span', { class: 'badge-count', text: String(n), 'aria-label': `${n} unread` }));
    return a;
  }));
}

function renderList(state) {
  const items = inboxItems(state.items);
  const list = $('list');
  if (!items.length) { list.replaceChildren(el('p', { class: 'empty', text: 'Nothing waiting.' })); return; }
  const ul = el('ul');
  for (const it of items) {
    const c = classOf(it);
    ul.append(el('li', {}, el('a', {
      class: 'row' + (it.unread ? ' unread' : ''), href: '#/item/' + encodeURIComponent(it.id),
      'aria-current': route.name === 'item' && route.arg === it.id ? 'true' : false,
    }, el('span', { class: 'cls cls-' + c, text: c }), el('span', { class: 'mono', text: it.id }),
    el('span', { class: 'q', text: it.question }))));
  }
  list.replaceChildren(ul);
}

function themePicker() {
  const cur = getTheme();
  const box = el('div', { class: 'seg', role: 'group', 'aria-label': 'Theme' });
  for (const t of THEMES) {
    const b = el('button', { type: 'button', text: t, 'aria-pressed': String(t === cur) });
    b.addEventListener('click', () => { setTheme(t); render(); });
    box.append(b);
  }
  return box;
}

function renderDetail(state) {
  const main = $('main');
  switch (route.name) {
  case 'item': {
    const it = state.items.find((x) => x.id === route.arg);
    main.replaceChildren(it
      ? el('article', {}, el('h2', { text: it.id }), el('p', { text: it.question }),
        ...(it.verbatim ? [el('pre', { text: it.verbatim })] : []))
      : el('p', { class: 'empty', text: 'Item not found.' }));
    break;
  }
  case 'settings': {
    const out = el('button', { type: 'button', text: 'Sign out' });
    out.addEventListener('click', async () => { try { await api.logout(); } catch { /* ignore */ } showLogin(); });
    main.replaceChildren(el('h2', { text: 'Settings' }), el('h3', { text: 'Theme' }), themePicker(), el('p', {}, out));
    break;
  }
  case 'inbox':
    main.replaceChildren(el('p', { class: 'muted', text: 'Select an item.' }));
    break;
  default:
    main.replaceChildren(el('p', { class: 'muted', text: 'Not available yet.' }));
  }
}

const TITLES = { inbox: 'Inbox', folder: 'Folders', item: 'Thread', compose: 'Compose',
  status: 'Status', contacts: 'Contacts', settings: 'Settings' };

function render() {
  if (!started) return;
  const state = store.state;
  const conn = connState;
  const app = $('app');
  app.dataset.view = route.name === 'inbox' ? 'list' : 'detail';
  $('title').textContent = TITLES[route.name];
  $('fresh').textContent = freshness(conn, state.syncedAt);
  const bus = state.busHealth;
  const bad = busDown(bus);
  $('banner').hidden = !bad;
  if (bad) $('banner').textContent = ('Bus down: messages to agents are waiting. ' + (bus.problems?.[0] ?? '')).trim();
  renderNav(state);
  renderList(state);
  renderDetail(state);
}

let connState = 'offline';
store.subscribe((state, conn) => { connState = conn; render(); });

window.addEventListener('hashchange', () => {
  route = parseRoute(location.hash);
  render();
  $('main').focus({ preventScroll: true });
});
const clientId = crypto.randomUUID();
const presence = () => {
  if (started) api.post('/presence', { clientId, active: document.visibilityState === 'visible' }).catch(() => {});
};
setInterval(() => { if (document.visibilityState === 'visible') presence(); }, 30000);
document.addEventListener('visibilitychange', presence);

if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/sw.js', { scope: '/' }).catch(() => {});
}
boot();
