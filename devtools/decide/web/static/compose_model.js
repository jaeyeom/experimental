// Compose rules, pure: recipient ordering and filtering, the send body,
// bus-down confirmation, and which compose draft behavior applies.
import { busDown } from './store.js';
import { threadText } from './thread_model.js';

const BUS_PANE = 'agentbus:';

// samePane: a bus handle holds a configured pane when equal or ending in
// "/<pane>" (herdr handles).
const samePane = (handle, pane) => !!handle && !!pane && (handle === pane || handle.endsWith('/' + pane));

const busEntry = (c) => ({ name: c.name, bus: true, dead: c.liveness === 'dead', live: c.liveness ?? '',
  when: c.when ?? '', pane: c.handle ?? '' });

// recipientList orders the picker like the TUI: starred names first in
// their starred order (each once), then configured targets (an
// agentbus:<name> target is replaced in place by that live name), then
// live bus names, then dead ones. A starred name nobody lists is dead.
export function recipientList({ targets = [], contacts = [], favorites = [] }) {
  const bus = contacts.filter((c) => c.bus && c.name).map(busEntry);
  const merged = new Set();
  const out = [];
  for (const t of targets) {
    if (t.pane?.startsWith(BUS_PANE)) {
      const name = t.pane.slice(BUS_PANE.length).trim();
      const hit = bus.find((b) => b.name === name);
      if (hit) { if (!merged.has(name)) { merged.add(name); out.push(hit); } continue; }
      if (name) { out.push({ name, bus: true, dead: false, live: '', when: '', pane: t.pane }); continue; }
    }
    if (bus.some((b) => samePane(b.pane, t.pane))) continue;
    out.push({ name: t.name, bus: false, dead: false, live: '', when: '', pane: t.pane ?? '' });
  }
  const rest = bus.filter((b) => !merged.has(b.name));
  const byName = (a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0);
  out.push(...rest.filter((b) => !b.dead).sort(byName), ...rest.filter((b) => b.dead).sort(byName));
  const favs = [];
  for (const name of new Set(favorites)) {
    const hit = out.find((r) => r.name === name);
    favs.push({ ...(hit ?? { name, bus: true, dead: true, live: 'dead', when: '', pane: '' }), fav: true });
  }
  const starred = new Set(favs.map((f) => f.name));
  return [...favs, ...out.filter((r) => !starred.has(r.name)).map((r) => ({ ...r, fav: false }))];
}

export function filterRecipients(list, query) {
  const q = String(query ?? '').trim().toLowerCase();
  if (!q) return list;
  return list.filter((r) => r.name.toLowerCase().includes(q) || (r.when ?? '').toLowerCase().includes(q));
}

// sendBody is the POST /send body. A target whose pane is agentbus:<name>
// goes over the bus.
export function sendBody(rec, text, forward) {
  const pane = rec.pane?.startsWith(BUS_PANE) ? rec.pane.slice(BUS_PANE.length).trim() : '';
  const bus = rec.bus || !!pane;
  const body = { to: pane || rec.name, bus };
  if (forward) body.forward = forward;
  body.text = text;
  return body;
}

export const needsBusConfirm = (rec, health) => !!rec && (rec.bus || !!rec.pane?.startsWith(BUS_PANE)) && busDown(health);

export function composeDraftBody(rec, text) {
  const d = { key: 'compose', kind: 'compose', text };
  if (rec) { d.to = rec.name; d.bus = !!rec.bus; }
  return d;
}

// composePlan: a forward is a preview that never reads or writes the
// saved compose draft; a plain compose offers to restore a non-blank one.
export function composePlan({ draft, forward }) {
  if (forward) return { prompt: false, saveDraft: false, forward };
  return { prompt: String(draft?.text ?? '').trim() !== '', saveDraft: true, forward: '' };
}

export const forwardPreview = (it) => threadText(it);
