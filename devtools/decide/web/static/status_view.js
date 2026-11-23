// Status board: agents and resource leases, redrawn on every store update
// so it is live over SSE. Rows older than five minutes are dimmed.
// Text is set with textContent only.
import { el } from './dom.js';
import { agentRows, resourceRows } from './status_model.js';

export function createStatusView({ root }) {
  const table = (headers, rows, cells) => el('div', { class: 'table-wrap' }, el('table', { class: 'board' },
    el('thead', {}, el('tr', {}, ...headers.map((h) => el('th', { scope: 'col', text: h })))),
    el('tbody', {}, ...rows.map((r) => el('tr', { class: r.stale ? 'dim' : '' },
      ...cells(r).map((c, i) => el(i === 0 ? 'th' : 'td', i === 0 ? { scope: 'row', text: c } : { text: c })))))));

  return {
    render(state) {
      const now = Date.now();
      const agents = agentRows(state.status, now);
      const res = resourceRows(state.status, now);
      const kids = [el('h2', { text: 'Status' })];
      if (!agents.length && !res.length) kids.push(el('p', { class: 'empty', text: 'No status yet.' }));
      if (agents.length) {
        kids.push(el('h3', { text: 'Agents' }),
          table(['Agent', 'State', 'Updated', 'Text'], agents, (r) => [r.name, r.state, r.age, r.text]));
      }
      if (res.length) {
        kids.push(el('h3', { text: 'Resources and checkouts' }),
          table(['Resource', 'Lease', 'Updated'], res, (r) => [r.name, r.lease, r.age]));
      }
      kids.push(el('p', { class: 'muted', text: 'Rows not updated for over 5 minutes are dimmed.' }));
      root.replaceChildren(...kids);
    },
    leave() {},
  };
}
