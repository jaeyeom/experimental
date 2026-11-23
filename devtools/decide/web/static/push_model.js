// Web Push settings rules, pure: key conversion, support detection with
// honest messages, and the preference form. Endpoints and keys are only
// ever passed to the API, never rendered.

// urlBase64ToUint8Array converts the VAPID public key (base64url, no
// padding) to the bytes PushManager.subscribe wants as applicationServerKey.
export function urlBase64ToUint8Array(b64) {
  const pad = '='.repeat((4 - (b64.length % 4)) % 4);
  const bin = atob((b64 + pad).replace(/-/g, '+').replace(/_/g, '/'));
  return Uint8Array.from(bin, (c) => c.charCodeAt(0));
}

// pushSupport: env is {secure, serviceWorker, pushManager, notification,
// ios, standalone}.
export function pushSupport(env) {
  const no = (message) => ({ ok: false, message });
  if (!env.secure) return no('Notifications need HTTPS. Open decide through its HTTPS address (not plain http) to turn them on.');
  if (!env.serviceWorker) return no('This browser has no service worker support, which notifications need.');
  if (!env.pushManager || !env.notification) {
    if (env.ios && !env.standalone) {
      return no('On iPhone and iPad, notifications work only for an installed app: tap Share, choose Add to Home Screen, then open decide from the Home Screen and come back here.');
    }
    return no('This browser does not support web push notifications.');
  }
  return { ok: true, message: '' };
}

export function permissionMessage(perm) {
  if (perm === 'denied') return 'Notifications are blocked for this site. Allow them in the browser or system site settings, then try again.';
  return '';
}

// subscriptionBody is the POST /push/subscribe body from a PushSubscription.
export function subscriptionBody(sub) {
  const j = sub.toJSON();
  return { endpoint: j.endpoint, keys: { p256dh: j.keys?.p256dh, auth: j.keys?.auth } };
}

// Notes are never pushed, so they are not an option.
export const classOptions = [
  ['approval', 'Approval'], ['decision', 'Decision'], ['fact', 'Fact'],
  ['task', 'Task'], ['request', 'Request'], ['inbound', 'Inbound'],
];

export const toggleClass = (classes, id) => (classes.includes(id) ? classes.filter((c) => c !== id) : [...classes, id]);

const clock = /^([01]\d|2[0-3]):[0-5]\d$/;

// validateQuiet returns an error message, or '' when start and end are
// both empty (quiet hours off) or both valid HH:MM.
export function validateQuiet(start, end) {
  if (!start && !end) return '';
  if (!start || !end) return 'Set both a start and an end time, or clear both.';
  if (!clock.test(start) || !clock.test(end)) return 'Times must be HH:MM.';
  return '';
}

// prefsBody is the PUT /push/preferences body. tz is sent only when known.
export function prefsBody(endpoint, form, tz) {
  const b = { endpoint, classes: form.classes, quietStart: form.quietStart, quietEnd: form.quietEnd };
  if (tz) b.tz = tz;
  return b;
}

export function formFromSub(sub) {
  return {
    classes: sub?.classes ? [...sub.classes] : classOptions.map(([id]) => id),
    quietStart: sub?.quietStart ?? '', quietEnd: sub?.quietEnd ?? '', tz: sub?.tz ?? '',
  };
}
