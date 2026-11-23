// Contacts: flat list or org tree, starring, and a detail sheet. Data is
// fetched on entry and after changes. Text is set with textContent only.
import * as api from './api.js';
import { el, btn } from './dom.js';
import { openSheet } from './sheet.js';
import { contactList, toggleFavorite, orgLayout, livenessLabel, detailRows } from './contacts_model.js';

export function createContactsView({ root, toast, onFilter }) {
  let active = false;
  let data = null; // {contacts, favorites, busError}
  let tree = null; // org tree root, or null
  let mode = 'list';
  let note = '';
  let orgOn = false;
  let busy = false;

  async function load() {
    try {
      const r = await api.get('/contacts');
      data = { contacts: r.contacts ?? [], favorites: r.favoriteContacts ?? [], busError: r.busError ?? '' };
    } catch {
      note = 'Could not load contacts.';
    }
    if (mode === 'tree') await loadTree();
    draw();
  }
  async function loadTree() {
    try {
      tree = (await api.get('/org-tree')).tree;
      note = '';
    } catch (e) {
      tree = null; mode = 'list';
      note = 'Org tree unavailable: ' + (e?.data?.error ?? 'could not reach the hub') ;
    }
  }

  async function star(name) {
    if (busy) return;
    busy = true;
    const next = toggleFavorite(data.favorites, name);
    try {
      const r = await api.post('/contacts/favorites', { names: next });
      const saved = r?.display?.keys?.favoriteContacts;
      data.favorites = Array.isArray(saved) ? saved : next;
    } catch { toast('Could not save the star. Try again.'); }
    busy = false;
    draw();
  }

  function detail(c) {
    const dl = el('dl', { class: 'detail-list' });
    for (const [k, v] of detailRows(c)) if (v) dl.append(el('dt', { text: k }), el('dd', { text: v }));
    openSheet({
      title: c.name, body: [dl], cancelLabel: 'Close', className: 'side',
      actions: [
        { label: 'Filter inbox by contact', run: () => onFilter(c.name) },
        { label: 'Compose message', primary: true, run: () => { location.hash = '#/compose?to=' + encodeURIComponent(c.name); } },
      ],
    });
  }

  function row(c) {
    const open = c.counts?.open ?? 0;
    const info = el('button', { type: 'button', class: 'contact-main', 'aria-label': `Details for ${c.name}` },
      el('span', { class: 'cname', text: c.name }),
      el('span', { class: 'muted', text: ' ' + livenessLabel(c) + (open ? `, ${open} open` : '') }));
    info.addEventListener('click', () => detail(c));
    const s = btn(c.fav ? '★' : '☆', () => star(c.name), 'star', { 'aria-pressed': String(!!c.fav), 'aria-label': (c.fav ? 'Unstar ' : 'Star ') + c.name });
    return el('li', { class: 'contact' + (c.bus && c.liveness === 'dead' ? ' dim' : '') }, info, s);
  }

  function draw() {
    if (!active) return;
    const kids = [el('h2', { text: 'Contacts' })];
    if (orgOn) {
      const seg = el('div', { class: 'seg', role: 'group', 'aria-label': 'View' });
      for (const [id, label] of [['list', 'List'], ['tree', 'Org Tree']]) {
        const b = btn(label, async () => { mode = id; if (id === 'tree' && !tree) await loadTree(); draw(); }, '', { 'aria-pressed': String(mode === id) });
        seg.append(b);
      }
      kids.push(seg);
    }
    if (note) kids.push(el('p', { class: 'muted', text: note }));
    if (data?.busError) kids.push(el('p', { class: 'muted', text: 'agentbus is unavailable; liveness is not shown.' }));
    if (!data) kids.push(el('p', { class: 'muted', text: 'Loading.' }));
    else {
      const list = contactList(data.contacts, data.favorites);
      if (mode === 'tree' && tree) {
        const byName = new Map(list.map((c) => [c.name, c]));
        const { rows } = orgLayout(tree, list);
        kids.push(el('ul', { class: 'contacts tree' }, ...rows.map((r) => {
          const c = r.contact ? byName.get(r.contact) : null;
          const li = c ? row(c) : el('li', { class: 'org-label' + (r.dim ? ' dim' : ''), text: r.label });
          li.classList.add('depth-' + Math.min(r.depth, 6));
          return li;
        })));
      } else if (list.length) kids.push(el('ul', { class: 'contacts' }, ...list.map(row)));
      else kids.push(el('p', { class: 'empty', text: 'No contacts yet.' }));
    }
    root.replaceChildren(...kids);
  }

  return {
    render(state) {
      orgOn = !!state.orgTreeEnabled;
      if (!active) { active = true; mode = 'list'; load(); } else draw();
    },
    leave() { active = false; data = null; tree = null; note = ''; },
  };
}
