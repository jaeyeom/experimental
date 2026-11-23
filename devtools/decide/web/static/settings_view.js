// Settings: theme, Web Push (subscribe, per-class filters, quiet hours,
// test), and session. Built once per entry so live updates never reset a
// form. Push endpoints and keys are passed to the API and never shown.
import * as api from './api.js';
import { el, btn } from './dom.js';
import {
  urlBase64ToUint8Array, pushSupport, permissionMessage, subscriptionBody, classOptions,
  toggleClass, validateQuiet, prefsBody, formFromSub,
} from './push_model.js';

const env = () => ({
  secure: window.isSecureContext,
  serviceWorker: 'serviceWorker' in navigator,
  pushManager: 'PushManager' in window,
  notification: 'Notification' in window,
  ios: /iP(hone|ad|od)/.test(navigator.userAgent) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1),
  standalone: window.matchMedia?.('(display-mode: standalone)').matches || navigator.standalone === true,
});

const withTimeout = (p, ms) => Promise.race([p, new Promise((_, rej) => setTimeout(() => rej(new Error('timeout')), ms))]);

export function createSettingsView({ root, toast, themes, getTheme, setTheme, connection, signOut }) {
  let active = false;
  let authRequired = false;
  const push = { support: null, reg: null, sub: null, view: null, hubOn: true, form: formFromSub(null), msg: '', busy: false, loaded: false };
  let connNode = null;
  let pushBox = null;

  const tz = () => { try { return Intl.DateTimeFormat().resolvedOptions().timeZone || ''; } catch { return ''; } };

  async function loadPush() {
    push.support = pushSupport(env());
    if (!push.support.ok) { push.loaded = true; drawPush(); return; }
    try {
      const k = await api.get('/push/vapid-key');
      push.hubOn = !!k.enabled;
      push.key = k.publicKey;
      push.reg = await withTimeout(navigator.serviceWorker.ready, 5000);
      push.sub = await push.reg.pushManager.getSubscription();
      if (push.sub && push.hubOn) {
        // Subscribing again is idempotent and returns the saved preferences.
        push.view = await api.post('/push/subscribe', subscriptionBody(push.sub));
        push.form = formFromSub(push.view);
      }
    } catch {
      push.msg = 'Could not check notification status. Reload to try again.';
    }
    push.loaded = true;
    drawPush();
  }

  async function enable() {
    if (push.busy) return;
    push.busy = true; push.msg = '';
    try {
      const perm = await Notification.requestPermission(); // must be the first await, inside the tap
      if (perm !== 'granted') { push.msg = permissionMessage(perm) || 'Notification permission was not granted.'; return; }
      const applicationServerKey = urlBase64ToUint8Array(push.key);
      const opts = { userVisibleOnly: true, applicationServerKey };
      let sub;
      try { sub = await push.reg.pushManager.subscribe(opts); } catch (e) {
        if (e?.name !== 'InvalidStateError') throw e;
        // An old subscription made with another key blocks subscribe().
        await (await push.reg.pushManager.getSubscription())?.unsubscribe();
        sub = await push.reg.pushManager.subscribe(opts);
      }
      try {
        push.view = await api.post('/push/subscribe', subscriptionBody(sub));
      } catch (e) {
        await sub.unsubscribe().catch(() => {});
        throw e;
      }
      push.sub = sub;
      push.form = formFromSub(push.view);
      push.msg = 'Notifications are on for this device.';
    } catch (e) {
      push.msg = e?.data?.error ? `Could not turn on notifications: ${e.data.error}` : 'Could not turn on notifications. Check the browser settings and try again.';
    } finally { push.busy = false; drawPush(); }
  }

  async function disable() {
    if (push.busy || !push.sub) return;
    push.busy = true; push.msg = '';
    try {
      await api.call('DELETE', '/push/subscribe', { endpoint: push.sub.endpoint });
      await push.sub.unsubscribe();
      push.sub = null; push.view = null;
      push.msg = 'Notifications are off for this device.';
    } catch { push.msg = 'Could not turn off notifications. Try again.'; } finally { push.busy = false; drawPush(); }
  }

  async function save() {
    const err = validateQuiet(push.form.quietStart, push.form.quietEnd);
    if (err) { push.msg = err; drawPush(); return; }
    try {
      push.view = await api.call('PUT', '/push/preferences', prefsBody(push.sub.endpoint, push.form, tz()));
      push.form = formFromSub(push.view);
      toast('Notification settings saved');
    } catch (e) { toast(e?.data?.error ? `Not saved: ${e.data.error}` : 'Could not save. Try again.'); }
    drawPush();
  }

  async function test() {
    try {
      await api.post('/push/test', { endpoint: push.sub.endpoint });
      toast('Test notification sent');
    } catch (e) {
      if (e?.status === 410) { push.sub = null; push.view = null; push.msg = 'The push service dropped this device. Turn notifications on again.'; drawPush(); return; }
      toast(e?.status === 502 ? 'The push service refused the test notification.' : 'Could not send the test. Try again.');
    }
  }

  function drawPush() {
    if (!active || !pushBox) return;
    const kids = [el('h3', { text: 'Notifications' })];
    if (!push.loaded) kids.push(el('p', { class: 'muted', text: 'Checking.' }));
    else if (!push.support.ok) kids.push(el('p', { class: 'muted', text: push.support.message }));
    else if (!push.hubOn) kids.push(el('p', { class: 'muted', text: 'Notifications are turned off on the hub (web.push.enabled).' }));
    else {
      const on = !!push.sub;
      if (Notification.permission === 'denied' && !on) kids.push(el('p', { class: 'muted', text: permissionMessage('denied') }));
      kids.push(el('p', {}, on ? btn('Turn off notifications', disable, '', { disabled: push.busy }) : btn('Turn on notifications', enable, 'primary', { disabled: push.busy })));
      if (on) kids.push(...prefsForm());
    }
    if (push.msg) kids.push(el('p', { class: 'notice', role: 'status', text: push.msg }));
    pushBox.replaceChildren(...kids);
  }

  function prefsForm() {
    const f = push.form;
    const fs = el('fieldset', { class: 'classes' }, el('legend', { text: 'Notify me about' }));
    for (const [id, label] of classOptions) {
      const cb = el('input', { type: 'checkbox', id: 'cls-' + id, class: 'check', checked: f.classes.includes(id) });
      cb.checked = f.classes.includes(id);
      cb.addEventListener('change', () => { f.classes = toggleClass(f.classes, id); });
      fs.append(el('label', { class: 'check-row', for: 'cls-' + id }, cb, ' ' + label));
    }
    fs.append(el('p', { class: 'muted', text: 'Notes are never sent as notifications.' }));
    const time = (id, label, key) => {
      const i = el('input', { type: 'time', id, value: f[key] });
      i.addEventListener('input', () => { f[key] = i.value; });
      return el('div', {}, el('label', { for: id, text: label }), i);
    };
    const quiet = el('fieldset', { class: 'quiet' }, el('legend', { text: 'Quiet hours' }),
      el('div', { class: 'time-row' }, time('qs', 'Start', 'quietStart'), time('qe', 'End', 'quietEnd')),
      el('p', { class: 'muted', text: `Time zone: ${tz() || f.tz || 'unknown'}. Leave both empty for no quiet hours.` }));
    return [fs, quiet, el('div', { class: 'reply-actions' }, btn('Save', save, 'primary'), btn('Send test notification', test))];
  }

  function build() {
    const seg = el('div', { class: 'seg', role: 'group', 'aria-label': 'Theme' });
    const draw = () => seg.replaceChildren(...themes.map((t) => btn(t[0].toUpperCase() + t.slice(1),
      () => { setTheme(t); draw(); }, '', { 'aria-pressed': String(t === getTheme()) })));
    draw();
    pushBox = el('section', { 'aria-label': 'Notifications' });
    connNode = el('p', { role: 'status' });
    const session = el('section', { 'aria-label': 'Session' }, el('h3', { text: 'Session' }), connNode);
    if (authRequired) session.append(el('p', {}, btn('Sign out', signOut)));
    root.replaceChildren(el('h2', { text: 'Settings' }), el('h3', { text: 'Appearance' }), seg, pushBox, session);
    drawPush();
  }

  return {
    render() {
      if (connNode) connNode.textContent = 'Connection: ' + connection();
      if (active) return;
      active = true;
      api.authStatus().catch(() => null).then((st) => {
        authRequired = !!st?.authRequired;
        if (!active) return;
        build();
        connNode.textContent = 'Connection: ' + connection();
        loadPush();
      });
    },
    leave() { active = false; pushBox = null; connNode = null; Object.assign(push, { loaded: false, msg: '', sub: null, view: null }); },
  };
}
