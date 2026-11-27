// Pure inbox model. It mirrors devtools/decide/inbox.go (InboxEntries,
// UnreadCount, BadgeOf) and ledger.go (Sort, Select, hasUnread) so the
// web list groups and orders exactly like the TUI. No DOM here: it runs
// under node for tests.

export const GROUPS = [
  ['approvals', 'Approvals'],
  ['needs-you', 'Needs you'],
  ['requests', 'Requests & inbound'],
  ['notes', 'Notes'],
];

export const FOLDERS = [
  ['inbox', 'Inbox'], ['snoozed', 'Snoozed'], ['answered', 'Answered'],
  ['done', 'Done'], ['withdrawn', 'Withdrawn'],
];

// Class name by badge letter, in chip order.
export const CLASS_CHIPS = [
  ['A', 'approval'], ['D', 'decision'], ['Q', 'fact'], ['T', 'task'],
  ['R', 'request'], ['I', 'inbound'], ['N', 'note'],
];

const PREFIX = Object.fromEntries(CLASS_CHIPS.map(([p, c]) => [c, p]));

const ms = (t) => {
  const n = typeof t === 'number' ? t : Date.parse(t);
  return Number.isNaN(n) ? 0 : n;
};
// A zero Go time marshals as 0001-01-01; treat it like a missing one.
const hasTime = (t) => !!t && ms(t) > 0 && !String(t).startsWith('0001-');

export const effectiveClass = (it) => it.class || 'decision';
export const badgeOf = (it) => PREFIX[effectiveClass(it)] ?? 'D';

function groupOf(cls) {
  switch (cls) {
  case 'approval': return 'approvals';
  case 'request': case 'inbound': return 'requests';
  case 'note': return 'notes';
  }
  return 'needs-you';
}

// lastStamp is the item's last message time as the server wrote it. It is
// the sort key and the `seen` value for archive.
export function lastStamp(it) {
  if (it.lastMessageAt) return it.lastMessageAt;
  let best = it.createdAt;
  for (const m of it.messages ?? []) if (ms(m.time) > ms(best)) best = m.time;
  return best;
}

const idNum = (id) => {
  const n = Number.parseInt(String(id).split('-').pop(), 10);
  return Number.isNaN(n) ? 0 : n;
};

function newer(a, b) {
  const at = ms(lastStamp(a)); const bt = ms(lastStamp(b));
  return at !== bt ? at > bt : idNum(a.id) > idNum(b.id);
}

function before(a, b) {
  const ad = hasTime(a.deadline); const bd = hasTime(b.deadline);
  if (ad && bd) return ms(a.deadline) !== ms(b.deadline) ? ms(a.deadline) < ms(b.deadline) : newer(a, b);
  if (ad) return true;
  if (bd) return false;
  return newer(a, b);
}

// sortItems is ledger Sort: soonest deadline, then no deadline, newest
// last message first, higher id on ties. It returns a new array.
export const sortItems = (items) => [...items].sort((a, b) => (before(a, b) ? -1 : before(b, a) ? 1 : 0));

// inboxEntries arranges items (an already selected inbox) into groups, in
// the order approvals, needs you, requests, notes. An open approval whose
// parent is in items is nested under it and has no row. A non-empty
// classes keeps only those classes; a parent with a waiting approval also
// matches "approval".
export function inboxEntries(items, classes = []) {
  const sorted = sortItems(items);
  const present = new Set(sorted.map((it) => it.id));
  const cards = new Map();
  const nested = new Set();
  for (const it of sorted) {
    if (effectiveClass(it) === 'approval' && it.state === 'open' && it.parent && present.has(it.parent)) {
      cards.set(it.parent, [...(cards.get(it.parent) ?? []), it]);
      nested.add(it.id);
    }
  }
  const want = new Set(classes);
  const byGroup = new Map(GROUPS.map(([g]) => [g, []]));
  for (const it of sorted) {
    if (nested.has(it.id)) continue;
    const e = { group: groupOf(effectiveClass(it)), item: it, approvals: [], approvalWaiting: false };
    const c = cards.get(it.id);
    if (c) { e.group = 'approvals'; e.approvals = c; e.approvalWaiting = true; }
    if (want.size > 0 && !want.has(effectiveClass(it)) && !(e.approvalWaiting && want.has('approval'))) continue;
    byGroup.get(e.group).push(e);
  }
  return GROUPS.flatMap(([g]) => byGroup.get(g));
}

// hasUnread is ledger hasUnread: the unread flag, or an agent message
// newer than the last read or human message. An item with no messages
// has its ask as the owner's message.
export function hasUnread(it) {
  if (it.unread) return true;
  const msgs = it.messages?.length ? it.messages
    : [{ author: it.owner, time: it.createdAt }];
  let seen = hasTime(it.readAt) ? ms(it.readAt) : 0;
  for (const m of msgs) if (m.author === 'human' && ms(m.time) > seen) seen = ms(m.time);
  return msgs.some((m) => m.author !== 'human' && m.author !== 'system' && ms(m.time) > seen);
}

// unreadCount counts items with something unread. Notes are excluded.
export const unreadCount = (items) => items
  .filter((it) => effectiveClass(it) !== 'note' && hasUnread(it)).length;

const snoozed = (it, now) => !!it.snoozedUntil && ms(it.snoozedUntil) > now;

// selectFolder is ledger Select and SelectSnoozed. Inbox also holds
// done-unread items. Snoozed items show only in the Snoozed folder, any
// state, soonest wake first.
export function selectFolder(items, folder, now = Date.now()) {
  if (folder === 'snoozed') {
    return items.filter((it) => snoozed(it, now))
      .sort((a, b) => ms(a.snoozedUntil) - ms(b.snoozedUntil));
  }
  const states = { inbox: ['open', 'done-unread'], answered: ['answered'], done: ['done'], withdrawn: ['withdrawn'] }[folder] ?? ['open', 'done-unread'];
  return sortItems(items.filter((it) => states.includes(it.state) && !snoozed(it, now)));
}

// undelivered is the failed-answer marker: the latest delivery attempt
// since the answer failed. Compose attempts are ignored.
export function undelivered(it) {
  const answered = hasTime(it.answeredAt) ? ms(it.answeredAt) : 0;
  const attempts = (it.delivery ?? []).filter((d) => !d.compose && (!answered || ms(d.at) >= answered));
  const last = attempts[attempts.length - 1];
  return last && last.outcome === 'failed' ? { error: last.error ?? '' } : null;
}

const oneLine = (s) => String(s ?? '').split(/\s+/).filter(Boolean).join(' ');

// snippet is the row's second line: the delivery notice, else the first
// line of the question or latest message, prefixed when undelivered.
export function snippet(it) {
  if (it.deliveryNotice) return it.deliveryNotice;
  let text = it.question || it.verbatim || '';
  const last = it.messages?.[it.messages.length - 1];
  if (last) text = last.text || last.verbatim || '';
  text = oneLine(text);
  const u = undelivered(it);
  if (u) {
    const note = u.error ? 'not delivered: ' + u.error : 'not delivered';
    return text ? note + ' ' + text : note;
  }
  return text;
}

// draftMark is the row's draft indicator: "draft!" when the persistent
// draft has a conflict, "draft" for any non-empty draft. localIds are
// items with a draft only in this browser.
export function draftMark(id, drafts, localIds = new Set()) {
  const mine = (drafts ?? []).filter((d) => d.itemId === id && String(d.text ?? '').trim() !== '');
  if (mine.some((d) => d.conflicts?.length)) return 'draft!';
  return mine.length || localIds.has(id) ? 'draft' : '';
}

function searchText(it) {
  const fields = [it.id, it.owner, it.question, it.verbatim, it.deliveryNotice];
  for (const m of it.messages ?? []) fields.push(m.text, m.verbatim);
  const u = undelivered(it);
  if (u) fields.push('not delivered', u.error);
  return fields.map((f) => f ?? '').join('\0').toLowerCase();
}

// matchesQuery is a case-insensitive literal search over the TUI's
// search fields plus the delivery marker.
export function matchesQuery(it, q) {
  const needle = String(q ?? '').trim().toLowerCase();
  return needle === '' || searchText(it).includes(needle);
}

const ownedBy = (it, agent) => it.owner === agent || it.worker === agent;

// filterEntries applies search text and the agent filter to grouped
// entries. A parent also matches on its waiting approvals.
export function filterEntries(entries, { query = '', agent = '' } = {}) {
  if (!String(query).trim() && !agent) return entries;
  return entries.filter((e) => (!agent || ownedBy(e.item, agent))
    && [e.item, ...e.approvals].some((it) => matchesQuery(it, query)));
}

function span(delta) {
  const m = Math.max(1, Math.floor(delta / 60e3));
  if (m < 60) return m + 'm';
  const h = Math.floor(m / 60);
  return h < 24 ? h + 'h' : Math.floor(h / 24) + 'd';
}

export function relativeAge(t, now = Date.now()) {
  if (!hasTime(t)) return '';
  const d = now - ms(t);
  return d < 60e3 ? 'now' : span(d);
}

export function deadlineLabel(t, now = Date.now()) {
  if (!hasTime(t)) return '';
  const d = ms(t) - now;
  return d >= 0 ? 'due in ' + span(d) : 'overdue ' + span(-d);
}

// inboxView is everything the list renders for one folder. The Inbox is
// grouped; other folders are flat rows in Sort order. unread always
// counts the inbox, whatever folder is open.
export function inboxView(items, { folder = 'inbox', classes = [], query = '', agent = '' } = {}, now = Date.now()) {
  const unread = unreadCount(selectFolder(items, 'inbox', now));
  const base = selectFolder(items, folder, now);
  if (folder === 'inbox') {
    const entries = filterEntries(inboxEntries(base, classes), { query, agent });
    const groups = GROUPS.map(([group, label]) => ({ group, label, entries: entries.filter((e) => e.group === group) }))
      .filter((g) => g.entries.length > 0);
    return { grouped: true, groups, entries, unread };
  }
  const want = new Set(classes);
  const rows = base.filter((it) => (want.size === 0 || want.has(effectiveClass(it)))
    && (!agent || ownedBy(it, agent)) && matchesQuery(it, query));
  return { grouped: false, rows, unread };
}
