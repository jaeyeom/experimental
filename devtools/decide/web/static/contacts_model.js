// Contacts rules, pure: list order, starring, org-tree layout (a port of
// tui/orgtree.go orgLayout), and the detail panel rows.
import { relativeAge } from './inbox_model.js';

export function contactList(contacts, favorites = [], { showHidden = false } = {}) {
  const byName = new Map(contacts.map((c) => [c.name, c]));
  const favs = [...new Set(favorites)];
  const out = favs.map((n) => ({ ...(byName.get(n) ?? { name: n, bus: true, liveness: 'dead', counts: {}, deliveries: {} }), fav: true }));
  const starred = new Set(favs);
  const dead = (c) => c.bus && c.liveness === 'dead';
  const open = (c) => c.counts?.open ?? 0;
  const rest = contacts.filter((c) => !starred.has(c.name) && (showHidden || !c.hidden))
    .sort((a, b) => (dead(a) - dead(b)) || (open(b) - open(a)) || (a.name < b.name ? -1 : a.name > b.name ? 1 : 0))
    .map((c) => ({ ...c, fav: false }));
  return [...out, ...rest];
}

export const toggleFavorite = (names, name) =>
  (names.includes(name) ? names.filter((n) => n !== name) : [...names, name]);

// orgLayout lists tree rows with a depth: the operator, each role under its
// manager (a role is a contact row only when a contact has that name), pools
// and roles with no contact dimmed, then an "other" group at the bottom.
export function orgLayout(root, contacts) {
  const names = new Set(contacts.map((c) => c.name));
  const placed = new Set();
  const rows = [];
  const walk = (n, depth) => {
    let label = n.label;
    let contact = null;
    let dim = false;
    if (n.kind === 'pool') { label = `[pool] ${n.label} (max ${n.max})`; dim = true; }
    else if (n.kind === 'role') {
      if (names.has(n.label) && !placed.has(n.label)) { placed.add(n.label); contact = n.label; } else dim = true;
    }
    rows.push({ depth, label, contact, dim, kind: n.kind });
    for (const child of n.children ?? []) walk(child, depth + 1);
  };
  walk(root, 0);
  const others = contacts.filter((c) => !placed.has(c.name));
  if (others.length) {
    rows.push({ depth: 0, label: 'other', contact: null, dim: true, kind: 'other' });
    for (const c of others) rows.push({ depth: 1, label: c.name, contact: c.name, dim: false, kind: 'contact' });
  }
  return { rows };
}

const seen = (s) => (s < 60 ? `${s}s` : s < 3600 ? `${Math.floor(s / 60)}m` : s < 86400 ? `${Math.floor(s / 3600)}h` : `${Math.floor(s / 86400)}d`);

export function livenessLabel(c) {
  if (!c.bus) return 'not on the bus';
  const l = c.liveness || 'unknown';
  return typeof c.livenessAge === 'number' ? `${l}, seen ${seen(c.livenessAge)} ago` : l;
}

export function detailRows(c, now = Date.now()) {
  const n = c.counts ?? {};
  const d = c.deliveries ?? {};
  const age = c.last ? relativeAge(c.last, now) : '';
  return [
    ['State', livenessLabel(c)],
    ['When', c.when || ''],
    ['Items', `${n.open ?? 0} open, ${n.answered ?? 0} answered, ${n.done ?? 0} done, ${n.withdrawn ?? 0} withdrawn`],
    ['Last activity', age === 'now' ? 'just now' : age ? age + ' ago' : ''],
    ['Deliveries', `${d.pending ?? 0} waiting, ${d.failed ?? 0} failed`],
  ];
}
