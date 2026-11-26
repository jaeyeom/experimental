// App shell: auth gate, theme, router, and rendering. DOM is built with
// textContent only; item text is never interpreted as HTML.
import * as api from './api.js';
import { createStore, parseRoute, freshness, busDown } from './store.js';
import { el } from './dom.js';
import { createInboxView } from './inbox_view.js';
import { createThreadView } from './thread_view.js';
import { createComposeView } from './compose_view.js';
import { createStatusView } from './status_view.js';
import { createContactsView } from './contacts_view.js';
import { createSettingsView } from './settings_view.js';
import { unreadCount, selectFolder } from './inbox_model.js';
import { createKeymap, isTextField } from './keymap.js';
import { toggleHelp, isHelpOpen, closeHelp } from './help_sheet.js';

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

let connState = 'offline';
const store = createStore();
let started = false;
let route = parseRoute(location.hash);
let lastFolderHash = '#/inbox';
let keymap = null;
const hint = (name) => keymap?.hint(name) ?? '';

function renderChord(st) {
  const node = $('chord');
  if (!node) return;
  if (!st) { node.hidden = true; node.textContent = ''; return; }
  node.textContent = `${st.prefix} … (${st.entries.map((e) => e.helpNext).join(' ')})`;
  node.hidden = false;
}

async function loadKeymap() {
  try {
    const data = await api.get('/keymap');
    keymap = createKeymap(data, { onChordChange: renderChord });
    threadView.refreshHints();
    composeView.refreshHints();
    render();
  } catch { /* retry on reload */ }
}

function showLogin(msg) {
  store.stop();
  started = false;
  keymap?.clearChord();
  keymap = null;
  closeHelp();
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
  if (!started) {
    started = true;
    await store.start();
    loadKeymap();
  }
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

const inboxView = createInboxView({ root: $('list'), toast, hint });
const threadView = createThreadView({ root: $('main'), toast, hint });

const composeView = createComposeView({ root: $('main'), toast, hint });
const statusView = createStatusView({ root: $('main') });
const contactsView = createContactsView({
  root: $('main'), toast, hint,
  onFilter: (name) => { inboxView.setAgent(name); location.hash = '#/inbox'; },
});
const settingsView = createSettingsView({
  root: $('main'), toast, themes: THEMES, getTheme, setTheme,
  connection: () => connState,
  signOut: async () => { try { await api.logout(); } catch { /* ignore */ } showLogin(); },
});
const secondary = { compose: composeView, status: statusView, contacts: contactsView, settings: settingsView };

function renderDetail(state) {
  const main = $('main');
  for (const [name, view] of Object.entries(secondary)) {
    if (name !== route.name) view.leave();
  }
  if (route.name === 'item') { threadView.render(state, route); return; }
  threadView.leave();
  if (secondary[route.name]) { secondary[route.name].render(state, route); return; }
  main.replaceChildren(el('p', { class: 'muted', text: 'Select an item.' }));
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

store.subscribe((state, conn) => { connState = conn; render(); });

window.addEventListener('hashchange', () => {
  route = parseRoute(location.hash);
  if (route.name === 'inbox') lastFolderHash = '#/inbox';
  else if (route.name === 'folder') lastFolderHash = '#/folder/' + encodeURIComponent(route.arg);
  keymap?.clearChord();
  render();
  $('main').focus({ preventScroll: true });
});

function currentScope() {
  switch (route.name) {
  case 'item': return 'detail';
  case 'status': return 'status';
  case 'contacts': return 'contacts';
  case 'compose': return 'compose';
  default: return 'list';
  }
}

function dispatchAction(action, target) {
  switch (action) {
  case 'help': toggleHelp(keymap); break;
  case 'search':
    if ($('list').offsetParent === null) location.hash = lastFolderHash;
    inboxView.focusSearch();
    break;
  case 'goto.inbox': location.hash = '#/inbox'; break;
  case 'goto.snoozed': location.hash = '#/folder/snoozed'; break;
  case 'goto.answered': location.hash = '#/folder/answered'; break;
  case 'goto.done': location.hash = '#/folder/done'; break;
  case 'goto.withdrawn': location.hash = '#/folder/withdrawn'; break;
  case 'goto.status': location.hash = '#/status'; break;
  case 'goto.contacts': location.hash = '#/contacts'; break;
  case 'goto.outbox': toast('Outbox is not available on web.'); break;
  case 'goto.top':
    if (route.name === 'item') threadView.scrollTo('top');
    else if (route.name === 'status') statusView.jump('top');
    else if (route.name === 'contacts') contactsView.jump('top');
    else inboxView.jump('top');
    break;

  case 'list.next': inboxView.move(1); break;
  case 'list.prev': inboxView.move(-1); break;
  case 'list.pageDown': inboxView.move(10); break;
  case 'list.pageUp': inboxView.move(-10); break;
  case 'list.top': inboxView.jump('top'); break;
  case 'list.bottom': inboxView.jump('bottom'); break;
  case 'list.open': inboxView.openSelected(); break;
  case 'list.compose': case 'detail.compose': location.hash = '#/compose'; break;
  case 'list.archive': inboxView.archiveSelected(); break;
  case 'list.snooze': inboxView.snoozeSelected(); break;
  case 'list.clearFilter': inboxView.clearFilters(); break;

  case 'detail.back': case 'status.back': case 'contacts.back': location.hash = lastFolderHash; break;
  case 'detail.scrollDown': threadView.scrollBy(60); break;
  case 'detail.scrollUp': threadView.scrollBy(-60); break;
  case 'detail.pageDown': threadView.scrollBy(($('main').clientHeight || 400) * 0.85); break;
  case 'detail.pageUp': threadView.scrollBy(-(($('main').clientHeight || 400) * 0.85)); break;
  case 'detail.top': threadView.scrollTo('top'); break;
  case 'detail.bottom': threadView.scrollTo('bottom'); break;
  case 'detail.nextMessage': threadView.stepMessage(1); break;
  case 'detail.prevMessage': threadView.stepMessage(-1); break;
  case 'detail.archive': threadView.archive(); break;
  case 'detail.snooze': threadView.snooze(); break;
  case 'detail.outcome': threadView.done(); break;
  case 'detail.forward': threadView.forward(); break;
  case 'detail.reply': threadView.focusReply(); break;

  case 'status.next': statusView.move(1); break;
  case 'status.prev': statusView.move(-1); break;
  case 'status.pageDown': statusView.move(10); break;
  case 'status.pageUp': statusView.move(-10); break;
  case 'status.top': statusView.jump('top'); break;
  case 'status.bottom': statusView.jump('bottom'); break;
  case 'status.compose': statusView.composeSelected(); break;
  case 'status.clearFilter': inboxView.setAgent(''); break;

  case 'contacts.next': contactsView.move(1); break;
  case 'contacts.prev': contactsView.move(-1); break;
  case 'contacts.pageDown': contactsView.move(10); break;
  case 'contacts.pageUp': contactsView.move(-10); break;
  case 'contacts.top': contactsView.jump('top'); break;
  case 'contacts.bottom': contactsView.jump('bottom'); break;
  case 'contacts.filter': contactsView.filterSelected(); break;
  case 'contacts.clearFilter': inboxView.setAgent(''); break;
  case 'contacts.compose': contactsView.composeSelected(); break;
  case 'contacts.star': contactsView.starSelected(); break;
  case 'contacts.orgTree': contactsView.toggleOrgTree(); break;

  case 'compose.send':
    if (route.name === 'compose') composeView.send();
    else if (route.name === 'item' && threadView.isReplyTarget(target)) threadView.sendReply(false);
    break;
  case 'compose.sendAndArchive':
    if (route.name === 'item' && threadView.isReplyTarget(target)) threadView.sendReply(true);
    break;
  case 'compose.discard':
    if (route.name === 'item' && threadView.isReplyTarget(target)) threadView.discardReply();
    break;
  case 'compose.cancel':
    if (inboxView.isSearchTarget(target)) inboxView.clearSearch();
    else if (route.name === 'item' && threadView.isReplyTarget(target)) threadView.blurReply();
    else if (route.name === 'compose') composeView.cancel(lastFolderHash);
    else if (target instanceof HTMLElement) target.blur();
    break;
  default:
  }
}

document.addEventListener('keydown', (e) => {
  if (!started || !keymap) return;
  if (isHelpOpen()) {
    const r = keymap.handleKey(e, { scope: 'list', typing: false });
    if (r?.type === 'action' && r.action === 'help') { e.preventDefault(); closeHelp(); }
    return;
  }
  if (document.querySelector('dialog[open]')) return;

  const res = keymap.handleKey(e, { scope: currentScope(), typing: isTextField(e.target) });
  if (!res) return;
  e.preventDefault();
  if (res.type === 'action') dispatchAction(res.action, e.target);
  else if (res.type === 'answer') threadView.answerOption(res.index);
});
// Status ages tick between events.
setInterval(() => { if (route.name === 'status') render(); }, 30000);
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
