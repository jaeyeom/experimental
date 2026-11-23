// App shell: auth gate, theme, router, and rendering. DOM is built with
// textContent only; item text is never interpreted as HTML.
import * as api from './api.js';
import { createStore, parseRoute, freshness, busDown } from './store.js';
import { el } from './dom.js';
import { createInboxView } from './inbox_view.js';
import { createThreadView } from './thread_view.js';
import { unreadCount, selectFolder } from './inbox_model.js';

const $ = (id) => document.getElementById(id);
// toast shows a short message in the live region.
let toastTimer = 0;
function toast(msg) {
  const t = $('toast');
  t.textContent = msg;
  t.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { t.hidden = true; }, 5000);
}

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
    const n = name === 'inbox' ? unreadCount(selectFolder(state.items, 'inbox')) : 0;
    if (n > 0) a.append(el('span', { class: 'badge-count', text: String(n), 'aria-label': `${n} unread` }));
    return a;
  }));
}

const inboxView = createInboxView({ root: $('list'), toast });
const threadView = createThreadView({ root: $('main'), toast });

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
  if (route.name === 'item') { threadView.render(state, route); return; }
  threadView.leave();
  switch (route.name) {
  case 'settings': {
    const out = el('button', { type: 'button', text: 'Sign out' });
    out.addEventListener('click', async () => { try { await api.logout(); } catch { /* ignore */ } showLogin(); });
    main.replaceChildren(el('h2', { text: 'Settings' }), el('h3', { text: 'Theme' }), themePicker(), el('p', {}, out));
    break;
  }
  case 'inbox': case 'folder':
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
  app.dataset.view = route.name === 'inbox' || route.name === 'folder' ? 'list' : 'detail';
  $('title').textContent = TITLES[route.name];
  $('fresh').textContent = freshness(conn, state.syncedAt);
  const bus = state.busHealth;
  const bad = busDown(bus);
  $('banner').hidden = !bad;
  if (bad) $('banner').textContent = ('Bus down: messages to agents are waiting. ' + (bus.problems?.[0] ?? '')).trim();
  renderNav(state);
  inboxView.render(state, route);
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
