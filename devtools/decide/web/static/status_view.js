// Status board: built-in sections and dashboard panels rendered from the
// shared panel model, read-only, live over SSE. Text is set with textContent
// and SVG nodes use createElementNS + setAttribute only.
import { el } from './dom.js';
import {
  buildDashboardPanels, gaugeModel, tableSlice, formatUnknownText, chartModel,
} from './status_model.js';

const SVG_NS = 'http://www.w3.org/2000/svg';

function svgEl(tag, attrs = {}, ...kids) {
  const n = document.createElementNS(SVG_NS, tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (v !== false && v != null) n.setAttribute(k, String(v));
  }
  n.append(...kids);
  return n;
}

export function createStatusView({ root }) {
  const table = (headers, rows, cells, rowAttrs = () => ({})) => el('div', { class: 'table-wrap' }, el('table', { class: 'board' },
    el('thead', {}, el('tr', {}, ...headers.map((h) => el('th', { scope: 'col', text: h })))),
    el('tbody', {}, ...rows.map((r) => el('tr', { class: r.stale ? 'dim' : '', tabindex: '0', ...rowAttrs(r) },
      ...cells(r).map((c, i) => el(i === 0 ? 'th' : 'td', i === 0 ? { scope: 'row', text: c } : { text: c })))))));

  const boardRows = () => [...root.querySelectorAll('tbody tr')];
  function move(delta) {
    const rows = boardRows();
    if (!rows.length) return;
    const cur = rows.indexOf(document.activeElement);
    const next = cur < 0 ? (delta > 0 ? 0 : rows.length - 1) : Math.max(0, Math.min(rows.length - 1, cur + delta));
    rows[next].focus();
  }
  function jump(where) {
    const rows = boardRows();
    if (!rows.length) return;
    rows[where === 'bottom' ? rows.length - 1 : 0].focus();
  }
  function composeSelected() {
    const agent = document.activeElement?.dataset?.agent;
    location.hash = agent ? '#/compose?to=' + encodeURIComponent(agent) : '#/compose';
  }

  function renderPanelBody(p) {
    switch (p.kind) {
    case 'agents-table':
      return [table(['Agent', 'State', 'Updated', 'Text'], p.rows, (r) => [r.name, r.state, r.age, r.text], (r) => ({ 'data-agent': r.name }))];
    case 'resources-table':
      return [table(['Resource', 'Lease', 'Updated'], p.rows, (r) => [r.name, r.lease, r.age])];
    case 'Text':
      return [el('pre', { class: 'panel-text', text: String(p.data?.text ?? '') })];
    case 'List': {
      const rows = Array.isArray(p.data?.rows) ? p.data.rows : [];
      if (!rows.length) return [el('p', { class: 'muted', text: '(empty)' })];
      return [el('ul', { class: 'panel-list' }, ...rows.map((r) => {
        const kids = [
          el('span', { class: 'panel-label', text: String(r.label ?? '') }),
          el('span', { class: 'panel-value', text: String(r.value ?? '') }),
        ];
        if (r.state) kids.push(el('span', { class: `state-badge state-${r.state}`, text: String(r.state) }));
        return el('li', { class: 'panel-row' }, ...kids);
      }))];
    }
    case 'Gauge': {
      const g = gaugeModel(p.data);
      const fillW = Math.round(g.ratio * 100);
      const label = `${g.value}/${g.max}${g.unit} (${g.pct}%)`;
      const svg = svgEl('svg', { class: 'gauge-svg', viewBox: '0 0 100 10', role: 'img', 'aria-label': `${p.title}: ${label}` },
        svgEl('rect', { class: 'gauge-track', x: 0, y: 0, width: 100, height: 10, rx: 3 }),
        svgEl('rect', { class: `gauge-fill state-${g.state}`, x: 0, y: 0, width: fillW, height: 10, rx: 3 }),
      );
      return [el('div', { class: 'panel-gauge' }, svg, el('span', { class: 'gauge-val mono', text: label }))];
    }
    case 'Chart': {
      const cm = chartModel(p.data);
      if (cm.empty) return [el('p', { class: 'muted', text: '(no data)' })];
      const w = 240;
      const h = 60;
      const pad = 4;
      const px = (x) => (pad + x * (w - 2 * pad)).toFixed(1);
      const py = (y) => (pad + y * (h - 2 * pad)).toFixed(1);
      const pts = cm.points.map((pt) => `${px(pt.x)},${py(pt.y)}`).join(' ');
      const shapes = [];
      if (cm.limitY !== null) {
        const ly = py(cm.limitY);
        shapes.push(svgEl('line', { class: 'chart-limit', x1: pad, x2: w - pad, y1: ly, y2: ly }));
      }
      if (cm.projection) {
        const last = cm.points[cm.points.length - 1];
        shapes.push(svgEl('line', {
          class: 'chart-proj',
          x1: px(last.x), y1: py(last.y),
          x2: px(cm.projection.x), y2: py(cm.projection.y),
        }));
      }
      shapes.push(svgEl('polyline', { class: 'chart-line', fill: 'none', points: pts }));
      const svg = svgEl('svg', { class: 'chart-svg', viewBox: `0 0 ${w} ${h}`, role: 'img', 'aria-label': `${p.title} chart` }, ...shapes);
      const parts = [`latest ${cm.latest}${cm.unit} (min ${cm.min}, max ${cm.max})`];
      if (cm.limit !== null) parts.push(`limit ${cm.limit}${cm.unit}`);
      if (cm.projection) parts.push(`projected ${cm.projection.to}${cm.unit} at ${cm.projection.at}`);
      return [svg, el('p', { class: 'panel-meta', text: parts.join(' · ') })];
    }
    case 'Table': {
      const sl = tableSlice(p.data);
      if (!sl.columns.length) return [el('p', { class: 'muted', text: '(empty table)' })];
      const kids = [table(sl.columns, sl.rows.map((r) => ({ cells: r, stale: false })), (r) => r.cells)];
      if (sl.more > 0) kids.push(el('p', { class: 'muted panel-more', text: `+${sl.more} more` }));
      return kids;
    }
    default:
      return [el('pre', { class: 'panel-text', text: formatUnknownText(p.data) })];
    }
  }

  function renderPanel(p) {
    if (p.unreadable) {
      return el('p', { class: 'muted', text: p.text });
    }
    const metaParts = [];
    if (p.stale) metaParts.push('stale');
    if (p.updatedAgo) metaParts.push(p.updatedAgo);
    const head = el('div', { class: 'panel-head' },
      el('h3', { text: p.title }),
      ...(metaParts.length ? [el('span', { class: 'panel-meta', text: metaParts.join(' · ') })] : []),
    );
    return el('section', {
      class: p.stale ? 'panel-card dim' : 'panel-card',
      'data-panel-id': p.id,
    }, head, ...renderPanelBody(p));
  }

  return {
    render(state) {
      const now = Date.now();
      const panels = buildDashboardPanels({
        status: state.status,
        busHealth: state.busHealth,
        panels: state.panels,
        dashboardPanels: state.dashboardPanels,
        now,
      });
      const kids = [el('h2', { text: 'Status' })];
      if (!panels.length) kids.push(el('p', { class: 'empty', text: 'No status yet.' }));
      for (const p of panels) kids.push(renderPanel(p));
      kids.push(el('p', { class: 'muted', text: 'Rows not updated for over 5 minutes are dimmed.' }));
      root.replaceChildren(...kids);
    },
    leave() {},
    move,
    jump,
    composeSelected,
  };
}
