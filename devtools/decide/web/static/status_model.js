// Status board rules, pure. Dimming matches the TUI (decide.StatusStale):
// a row older than five minutes, or with no time, is stale.
import { relativeAge } from './inbox_model.js';

export const STALE_MS = 5 * 60 * 1000;
export const BUILTIN_PANEL_IDS = ['agents', 'resources', 'bus-health'];

const valid = (t) => !!t && !String(t).startsWith('0001-') && !Number.isNaN(Date.parse(t));

export const isStale = (t, now = Date.now()) => !valid(t) || now - Date.parse(t) > STALE_MS;
export const ageLabel = (t, now = Date.now()) => (valid(t) ? relativeAge(t, now) : '');

export function formatUpdatedAgo(t, now = Date.now()) {
  const age = ageLabel(t, now);
  if (!age) return '';
  return age === 'now' ? 'updated now' : `updated ${age} ago`;
}

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

export function panelRows(panels, now = Date.now()) {
  return (panels ?? []).map((p) => {
    if (p.unreadable) {
      return {
        id: p.id ?? p.file ?? '',
        unreadable: true,
        text: `panel unreadable: ${p.file || p.id || ''}`,
        stale: true,
      };
    }
    return {
      id: p.id,
      title: p.title ?? '',
      kind: p.kind ?? '',
      data: p.data,
      updatedAt: p.updatedAt ?? null,
      age: ageLabel(p.updatedAt, now),
      updatedAgo: formatUpdatedAgo(p.updatedAt, now),
      stale: !!p.stale,
      unreadable: false,
      source: p.source ?? '',
    };
  });
}

function busFieldState(val) {
  const v = String(val ?? '').trim().toLowerCase();
  if (v === 'up' || v === 'ok' || v === 'running') return 'ok';
  if (v === 'down' || v === 'error' || v === 'failed') return 'fail';
  return 'warn';
}

export function busHealthRows(busHealth) {
  if (!busHealth) return [];
  const rows = [];
  if (busHealth.daemon) rows.push({ label: 'daemon', value: String(busHealth.daemon), state: busFieldState(busHealth.daemon) });
  if (busHealth.transport) rows.push({ label: 'transport', value: String(busHealth.transport), state: busFieldState(busHealth.transport) });
  for (const p of (busHealth.problems ?? [])) {
    if (String(p).trim()) rows.push({ label: 'problem', value: String(p), state: 'fail' });
  }
  if (!rows.length && typeof busHealth.ok === 'boolean') {
    rows.push({ label: 'status', value: busHealth.ok ? 'ok' : 'down', state: busHealth.ok ? 'ok' : 'fail' });
  }
  return rows;
}

export function resolveDashboardPanels(configured, availableIds) {
  const listed = (configured ?? []).map((c, idx) => ({
    id: c.id,
    enabled: c.enabled !== false,
    order: Number.isFinite(Number(c.order)) ? Number(c.order) : 0,
    idx,
  }));
  listed.sort((a, b) => (a.order !== b.order ? a.order - b.order : a.idx - b.idx));
  const seen = new Set();
  const out = [];
  for (const item of listed) {
    if (!item.id || seen.has(item.id)) continue;
    seen.add(item.id);
    out.push({ id: item.id, enabled: item.enabled, order: out.length });
  }
  for (const id of (availableIds ?? [])) {
    if (!id || seen.has(id)) continue;
    seen.add(id);
    out.push({ id, enabled: true, order: out.length });
  }
  return out;
}

export function buildDashboardPanels({ status, busHealth, panels, dashboardPanels, now = Date.now() } = {}) {
  const byId = new Map();
  const availableIds = [];
  const add = (p) => {
    byId.set(p.id, p);
    availableIds.push(p.id);
  };

  const agents = agentRows(status, now);
  if (agents.length) {
    add({ id: 'agents', title: 'Agents', kind: 'agents-table', rows: agents, builtIn: true, stale: false });
  }
  const resources = resourceRows(status, now);
  if (resources.length) {
    add({ id: 'resources', title: 'Resources and checkouts', kind: 'resources-table', rows: resources, builtIn: true, stale: false });
  }
  const bhRows = busHealthRows(busHealth);
  if (bhRows.length) {
    add({ id: 'bus-health', title: 'Bus Health', kind: 'List', data: { rows: bhRows }, builtIn: true, stale: false });
  }

  for (const p of panelRows(panels, now)) {
    if (byId.has(p.id)) continue;
    add({ ...p, title: p.title || p.id, builtIn: false });
  }

  const resolved = resolveDashboardPanels(dashboardPanels, availableIds);
  const out = [];
  for (const st of resolved) {
    if (!st.enabled) continue;
    const panel = byId.get(st.id);
    if (panel) out.push(panel);
  }
  return out;
}

export function gaugeState(data) {
  const value = Number(data?.value ?? 0);
  const hasWarn = data?.warn != null && Number.isFinite(Number(data.warn));
  const hasFail = data?.fail != null && Number.isFinite(Number(data.fail));
  const warn = hasWarn ? Number(data.warn) : 0;
  const fail = hasFail ? Number(data.fail) : 0;
  if (hasFail && hasWarn && fail < warn) {
    if (value <= fail) return 'fail';
    if (value <= warn) return 'warn';
    return 'ok';
  }
  if (hasFail && value >= fail) return 'fail';
  if (hasWarn && value >= warn) return 'warn';
  return 'ok';
}

export function gaugeModel(data) {
  const value = Number(data?.value ?? 0);
  const max = Number(data?.max ?? 0);
  const ratio = max > 0 ? Math.max(0, Math.min(1, value / max)) : 0;
  return {
    value,
    max,
    ratio,
    pct: Math.round(ratio * 100),
    state: gaugeState(data),
    unit: data?.unit ?? '',
  };
}

export function tableSlice(data, maxRows = 5) {
  const columns = Array.isArray(data?.columns) ? data.columns.map(String) : [];
  const allRows = Array.isArray(data?.rows) ? data.rows : [];
  const rows = allRows.slice(0, maxRows).map((r) => (Array.isArray(r) ? r.map((c) => (c == null ? '' : String(c))) : []));
  return {
    columns,
    rows,
    more: Math.max(0, allRows.length - maxRows),
  };
}

export function formatUnknownText(data, maxChars = 240) {
  let s = '';
  try {
    s = JSON.stringify(data ?? null, null, 2);
  } catch {
    s = String(data ?? '');
  }
  if (s.length <= maxChars) return s;
  return s.slice(0, maxChars - 1) + '…';
}

export function chartModel(data) {
  const series = Array.isArray(data?.series) ? data.series : [];
  const unit = data?.unit ?? '';
  const hasLimit = data?.limit != null && Number.isFinite(Number(data.limit));
  const limit = hasLimit ? Number(data.limit) : null;
  const projection = data?.projection && data.projection.to != null && data.projection.at
    ? { to: Number(data.projection.to), at: String(data.projection.at) }
    : null;
  if (!series.length) {
    return { empty: true, points: [], min: 0, max: 0, latest: 0, unit, limit, projection };
  }
  const vals = series.map((p) => Number(p.v));
  let min = Math.min(...vals);
  let max = Math.max(...vals);
  if (limit !== null) {
    min = Math.min(min, limit);
    max = Math.max(max, limit);
  }
  if (projection) {
    min = Math.min(min, projection.to);
    max = Math.max(max, projection.to);
  }
  const span = max - min;
  const hasProj = !!projection;
  const xSpan = hasProj ? 0.8 : 1;
  const points = series.map((p, i) => ({
    x: series.length === 1 ? 0 : (i / (series.length - 1)) * xSpan,
    y: span > 0 ? 1 - (Number(p.v) - min) / span : 0.5,
    t: String(p.t ?? ''),
    v: Number(p.v),
  }));
  const normY = (v) => (span > 0 ? 1 - (v - min) / span : 0.5);
  return {
    empty: false,
    points,
    min,
    max,
    latest: vals[vals.length - 1],
    unit,
    limit,
    limitY: limit !== null ? normY(limit) : null,
    projection: projection ? { ...projection, x: 1, y: normY(projection.to) } : null,
  };
}
