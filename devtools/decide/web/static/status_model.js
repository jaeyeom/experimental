// Status board rules, pure. Dimming matches the TUI (decide.StatusStale):
// a row older than five minutes, or with no time, is stale.
import { relativeAge } from './inbox_model.js';

export const STALE_MS = 5 * 60 * 1000;

const valid = (t) => !!t && !String(t).startsWith('0001-') && !Number.isNaN(Date.parse(t));

export const isStale = (t, now = Date.now()) => !valid(t) || now - Date.parse(t) > STALE_MS;
export const ageLabel = (t, now = Date.now()) => (valid(t) ? relativeAge(t, now) : '');

export function agentRows(board, now = Date.now()) {
  return (board?.agents ?? []).map((a) => ({
    name: a.name, state: a.state, text: a.text ?? '', age: ageLabel(a.updatedAt, now), stale: isStale(a.updatedAt, now),
  }));
}

// leaseLabel: the hub formats the lease detail into note ("held by X
// until HH:MM", "paused by user", "needs adopt", "N queued"); free rows add
// only a queue. A row read without a live lease is marked a guess.
export function leaseLabel(r) {
  const note = r.note ?? '';
  let s = r.availability === 'free' ? ['free', note].filter(Boolean).join(', ') : (note || r.availability || 'busy');
  if (r.guess) s += ' (guess)';
  return s;
}

export function resourceRows(board, now = Date.now()) {
  return (board?.resources ?? []).map((r) => ({
    name: r.name, availability: r.availability, lease: leaseLabel(r),
    age: ageLabel(r.updatedAt, now), stale: isStale(r.updatedAt, now),
  }));
}
