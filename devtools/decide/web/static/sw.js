// Service worker: offline app shell, Web Push, and the Snooze 1h action.
// BUILD is replaced by the server with a hash of the embedded assets, so
// a new decide build refreshes the cache. /api/ is never cached or
// intercepted: responses carry credentials, and the event stream is
// long-lived. Offline data comes from the page's IndexedDB copy.
const BUILD = '__BUILD__';
const CACHE = 'decide-shell-' + BUILD;
const SHELL = [
  '/', '/manifest.webmanifest',
  '/static/styles.css', '/static/app.js', '/static/api.js', '/static/store.js',
  '/static/dom.js', '/static/inbox_model.js', '/static/inbox_view.js', '/static/swipe.js', '/static/snooze_sheet.js',
  '/static/text_render.js', '/static/thread_model.js', '/static/draft_sync.js', '/static/sheet.js', '/static/thread_view.js',
  '/static/compose_model.js', '/static/compose_view.js', '/static/status_model.js', '/static/status_view.js', '/static/contacts_model.js', '/static/contacts_view.js', '/static/push_model.js', '/static/settings_view.js',
  '/static/icons/icon.svg', '/static/icons/icon-maskable.svg',
];

self.addEventListener('install', (event) => {
  event.waitUntil(caches.open(CACHE).then((c) => c.addAll(SHELL)).then(() => self.skipWaiting()));
});

self.addEventListener('activate', (event) => {
  event.waitUntil(
    caches.keys()
      .then((keys) => Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k))))
      .then(() => self.clients.claim()),
  );
});

self.addEventListener('fetch', (event) => {
  const req = event.request;
  const url = new URL(req.url);
  // Only same-origin GETs outside /api/ are ours; everything else goes
  // straight to the network untouched.
  if (req.method !== 'GET' || url.origin !== self.location.origin || url.pathname.startsWith('/api/')) return;
  if (req.mode === 'navigate') {
    event.respondWith(fetch(req).catch(() => caches.match('/')));
    return;
  }
  if (url.pathname === '/sw.js') return;
  event.respondWith(caches.match(req).then((hit) => hit || fetch(req)));
});

self.addEventListener('push', (event) => {
  let p = {};
  try { p = event.data ? event.data.json() : {}; } catch { /* not JSON */ }
  event.waitUntil(self.registration.showNotification(p.title || 'decide', {
    body: p.body || '',
    tag: p.tag,
    data: p.data || {},
    actions: p.actions || [],
  }));
});

self.addEventListener('notificationclick', (event) => {
  const n = event.notification;
  const data = n.data || {};
  n.close();
  if (event.action === 'snooze-1h' && data.itemId) {
    event.waitUntil(fetch('/api/v1/items/' + encodeURIComponent(data.itemId) + '/snooze', {
      method: 'POST',
      credentials: 'same-origin',
      headers: { 'Content-Type': 'application/json', 'X-Decide-Client': '1' },
      body: JSON.stringify({ preset: '1h' }),
    }));
    return;
  }
  const target = typeof data.url === 'string' && data.url.startsWith('/') && !data.url.startsWith('//') ? data.url : '/';
  event.waitUntil(self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then((wins) => {
    for (const w of wins) {
      if ('focus' in w) { w.navigate?.(target); return w.focus(); }
    }
    return self.clients.openWindow(target);
  }));
});
