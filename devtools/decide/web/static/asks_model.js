// Pure helpers for the optional Asks view and Status summary panel.
import { relativeAge } from './inbox_model.js';

export function openAskCount(asks) {
  return (asks ?? []).filter((a) => a.status === 'open').length;
}

export function sortAsks(asks) {
  const copy = [...(asks ?? [])];
  copy.sort((a, b) => {
    const openA = a.status === 'open';
    const openB = b.status === 'open';
    if (openA !== openB) return openA ? -1 : 1;
    const ta = Date.parse(a.createdAt || '') || 0;
    const tb = Date.parse(b.createdAt || '') || 0;
    if (ta !== tb) return ta - tb;
    return String(a.id ?? '').localeCompare(String(b.id ?? ''));
  });
  return copy;
}

export function safeHttpURL(raw) {
  try {
    const u = new URL(String(raw ?? ''));
    return (u.protocol === 'http:' || u.protocol === 'https:') ? u.href : '';
  } catch {
    return '';
  }
}

export function asksPanel(asks, enabled, now = Date.now()) {
  if (!enabled) return null;
  const open = sortAsks(asks).filter((a) => a.status === 'open');
  const rows = [{
    label: 'open',
    value: String(open.length),
    state: open.length > 0 ? 'warn' : 'ok',
  }];
  for (const a of open.slice(0, 5)) {
    const age = a.createdAt ? relativeAge(a.createdAt, now) : '';
    rows.push({
      label: a.id,
      value: `${a.person}: ${a.what}${age ? ' · ' + age : ''}`,
    });
  }
  return {
    id: 'asks',
    title: 'Asks',
    kind: 'List',
    data: { rows },
    builtIn: true,
    stale: false,
  };
}
