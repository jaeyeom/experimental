// Inbox list view: folders, class chips, search, grouped rows, and
// Archive/Snooze by swipe, button or key. All text is set with
// textContent; item text is never parsed as HTML.
import * as api from './api.js';
import { el, keyHint } from './dom.js';
import {
  FOLDERS, CLASS_CHIPS, inboxView, badgeOf, hasUnread, effectiveClass, snippet, undelivered,
  draftMark, relativeAge, deadlineLabel, lastStamp,
} from './inbox_model.js';
import { attachSwipe } from './swipe.js';
import { openSnoozeSheet } from './snooze_sheet.js';
import { openFlagSheet, openGoodSheet } from './flag_sheet.js';

const folderOf = (route) => {
  const f = route.name === 'folder' ? route.arg : 'inbox';
  return FOLDERS.some(([id]) => id === f) ? f : 'inbox';
};
const closed = (it) => it.state === 'done' || it.state === 'withdrawn';
const wakeText = (t) => new Date(t).toLocaleString([], { weekday: 'short', hour: '2-digit', minute: '2-digit' });

export function createInboxView({ root, toast, hint = () => '' }) {
  const filter = { classes: new Set(), query: '', agent: '' };
  let state = null;
  let route = { name: 'inbox', arg: '' };
  let items = new Map();

  // Header is built once so typing in search keeps focus across updates.
  const search = el('input', { type: 'search', id: 'search', placeholder: 'Search', 'aria-label': 'Search items', autocomplete: 'off' });
  const folderBar = el('nav', { class: 'folders', 'aria-label': 'Folders' });
  const chipBar = el('div', { class: 'chips', role: 'group', 'aria-label': 'Filter by class' });
  const agentBar = el('div', { class: 'chips' });
  const body = el('div', { class: 'rows' });
  root.replaceChildren(folderBar, el('div', { class: 'search-row' }, search), chipBar, agentBar, body);

  const chip = (label, onClick, text = label) => {
    const b = el('button', { type: 'button', class: 'chip', text, 'aria-pressed': 'false', 'data-chip': label });
    b.addEventListener('click', onClick);
    return b;
  };
  const allChip = chip('All', () => { filter.classes.clear(); render(); });
  chipBar.append(allChip, ...CLASS_CHIPS.map(([p, cls]) => chip(p, () => {
    if (filter.classes.has(cls)) filter.classes.delete(cls); else filter.classes.add(cls);
    render();
  }, p)));
  for (const b of chipBar.querySelectorAll('button')) {
    const cls = CLASS_CHIPS.find(([p]) => p === b.dataset.chip)?.[1];
    if (cls) b.setAttribute('aria-label', `Class ${cls}`);
  }

  search.addEventListener('input', () => { filter.query = search.value; render(); });

  // --- actions ---
  async function archive(it) {
    try {
      await api.archive(it.id, lastStamp(it));
      toast(`Archived ${it.id}`);
    } catch (e) {
      toast(api.newerMessage(e) ? 'New message arrived — open thread before archiving' : 'Could not archive. Try again.');
    }
  }

  async function snooze(it, body) {
    try {
      await api.snooze(it.id, body);
      toast(`Snoozed ${it.id}`);
    } catch (e) {
      toast(e?.data?.error ?? 'Could not snooze. Try again.');
    }
  }

  function snoozeSheet(it, focusKey) {
    openSnoozeSheet({
      title: `Snooze ${it.id}`,
      pick: (b) => snooze(it, b),
      unsnooze: it.snoozedUntil ? async () => {
        try { await api.unsnooze(it.id); toast(`Unsnoozed ${it.id}`); } catch { toast('Could not unsnooze. Try again.'); }
      } : null,
      restoreFocus: () => root.querySelector(`[data-k="${CSS.escape(focusKey)}"]`)?.focus(),
    });
  }

  async function flagSheet(it, focusKey) {
    let list = state?.reasons ?? [];
    if (!list.length) {
      try { list = (await api.reasons())?.reasons ?? []; } catch { list = []; }
    }
    openFlagSheet({
      item: it,
      reasons: list,
      onSubmit: async (body) => {
        await api.flag(it.id, body);
        toast(`Flagged ${it.id}`);
      },
      restoreFocus: () => root.querySelector(`[data-k="${CSS.escape(focusKey)}"]`)?.focus(),
    });
  }

  function goodSheet(it, focusKey) {
    openGoodSheet({
      item: it,
      onSubmit: async (body) => {
        await api.good(it.id, body);
        toast(`Recorded thumbs-up for ${it.id}`);
      },
      restoreFocus: () => root.querySelector(`[data-k="${CSS.escape(focusKey)}"]`)?.focus(),
    });
  }

  // --- rendering ---
  const pill = (text, cls) => el('span', { class: 'pill ' + cls, text });

  function rowFor(entry, folder, drafts) {
    const it = entry.item;
    const now = Date.now();
    const cls = badgeOf(it);
    const unread = effectiveClass(it) !== 'note' && hasUnread(it);
    let when = deadlineLabel(it.deadline, now) || relativeAge(lastStamp(it), now);
    if (folder === 'snoozed' && it.snoozedUntil) when = 'wakes ' + wakeText(it.snoozedUntil);
    const pills = [];
    if (entry.approvalWaiting) pills.push(pill('approval waiting', 'warn'));
    if (undelivered(it)) pills.push(pill('undelivered', 'bad'));
    const dm = draftMark(it.id, drafts);
    if (dm) pills.push(pill(dm, dm === 'draft!' ? 'bad' : ''));
    const link = el('a', {
      class: 'row' + (unread ? ' unread' : ''), href: '#/item/' + encodeURIComponent(it.id),
      'aria-current': route.name === 'item' && route.arg === it.id ? 'true' : false,
      'data-k': it.id + ':row',
    },
    el('span', { class: 'row-top' },
      el('span', { class: 'cls cls-' + cls, text: cls, title: effectiveClass(it) }),
      ...(unread ? [el('span', { class: 'dot', role: 'img', 'aria-label': 'unread' })] : []),
      el('span', { class: 'owner', text: it.owner ?? '' }),
      el('span', { class: 'mono id', text: it.id }),
      el('span', { class: 'when' + (it.deadline && when.startsWith('overdue') ? ' overdue' : ''), text: when })),
    el('span', { class: 'row-bottom' }, el('span', { class: 'q', text: snippet(it) }), ...pills));
    const li = el('li', { class: 'row-wrap', 'data-id': it.id });
    li.append(link);
    const act = (name, actionName, fn, ariaLabel = `${name} ${it.id}`, dataName = name) => {
      const b = el('button', { type: 'button', class: 'act', 'aria-label': ariaLabel, 'data-k': `${it.id}:${dataName}` },
        name, keyHint(hint(actionName)));
      b.addEventListener('click', fn);
      return b;
    };
    const rowBtns = [];
    if (!closed(it)) {
      rowBtns.push(act('Archive', 'list.archive', () => archive(it)), act('Snooze', 'list.snooze', () => snoozeSheet(it, `${it.id}:Snooze`)));
      attachSwipe(li, { onArchive: () => archive(it), onSnooze: () => snoozeSheet(it, `${it.id}:row`) });
    }
    rowBtns.push(
      act('👍', 'list.good', () => goodSheet(it, `${it.id}:Good`), `Thumbs up ${it.id}`, 'Good'),
      act('Flag', 'list.flag', () => flagSheet(it, `${it.id}:Flag`)),
    );
    li.append(el('span', { class: 'row-actions' }, ...rowBtns));
    return li;
  }

  function render() {
    if (!state) return;
    const folder = folderOf(route);
    const view = inboxView(state.items, { folder, classes: [...filter.classes], query: filter.query, agent: filter.agent });
    const drafts = state.drafts;
    items = new Map(state.items.map((it) => [it.id, it]));

    // Remember focus: the body is rebuilt on every change.
    const focused = document.activeElement?.closest?.('[data-k]');
    const focusKey = root.contains(focused) ? focused.dataset.k : null;

    folderBar.replaceChildren(...FOLDERS.map(([id, label]) => el('a', {
      href: id === 'inbox' ? '#/inbox' : '#/folder/' + id, text: label, 'aria-current': id === folder ? 'page' : false,
    })));
    for (const b of chipBar.querySelectorAll('button')) {
      const cls = CLASS_CHIPS.find(([p]) => p === b.dataset.chip)?.[1];
      b.setAttribute('aria-pressed', String(cls ? filter.classes.has(cls) : filter.classes.size === 0));
    }
    agentBar.replaceChildren();
    if (filter.agent) {
      const x = el('button', { type: 'button', class: 'chip', text: `Filtered: ${filter.agent} ×`, 'aria-label': `Clear filter ${filter.agent}` });
      x.addEventListener('click', () => { filter.agent = ''; render(); });
      agentBar.append(x);
    }

    const sections = [];
    if (view.grouped) {
      for (const g of view.groups) {
        const head = el('div', { class: 'group-head' }, el('h2', { text: `${g.label} (${g.entries.length})` }));
        sections.push(el('section', {}, head, el('ul', {}, ...g.entries.map((e) => rowFor(e, folder, drafts)))));
      }
    } else if (view.rows.length) {
      sections.push(el('ul', {}, ...view.rows.map((it) => rowFor({ item: it, approvals: [], approvalWaiting: false }, folder, drafts))));
    }
    if (!sections.length) {
      const active = filter.classes.size || filter.query.trim() || filter.agent;
      sections.push(el('p', { class: 'empty', text: active ? 'No matches.' : 'Nothing here.' }));
    }
    body.replaceChildren(...sections);
    if (focusKey) root.querySelector(`[data-k="${CSS.escape(focusKey)}"]`)?.focus({ preventScroll: true });
  }

  // --- keyboard actions (invoked by the central keymap dispatcher) ---
  const listRows = () => [...root.querySelectorAll('a.row')];
  const focusedItem = () => items.get(document.activeElement?.closest?.('li[data-id]')?.dataset.id);

  function move(delta) {
    const rows = listRows();
    if (!rows.length) return;
    const cur = rows.indexOf(document.activeElement);
    const next = cur < 0 ? (delta > 0 ? 0 : rows.length - 1) : Math.max(0, Math.min(rows.length - 1, cur + delta));
    rows[next].focus();
  }

  function jump(where) {
    const rows = listRows();
    if (!rows.length) return;
    rows[where === 'bottom' ? rows.length - 1 : 0].focus();
  }

  function openSelected() {
    const rows = listRows();
    if (!rows.length) return;
    const cur = rows.indexOf(document.activeElement);
    rows[cur < 0 ? 0 : cur].click();
  }

  function archiveSelected() {
    const it = focusedItem();
    if (it && !closed(it)) archive(it);
  }

  function snoozeSelected() {
    const it = focusedItem();
    if (it && !closed(it)) snoozeSheet(it, `${it.id}:row`);
  }

  function flagSelected() {
    const it = focusedItem() ?? items.get(listRows()[0]?.closest?.('li[data-id]')?.dataset.id);
    if (it) flagSheet(it, `${it.id}:row`);
  }

  function goodSelected() {
    const it = focusedItem() ?? items.get(listRows()[0]?.closest?.('li[data-id]')?.dataset.id);
    if (it) goodSheet(it, `${it.id}:row`);
  }

  function clearFilters() {
    filter.classes.clear();
    filter.agent = '';
    filter.query = '';
    search.value = '';
    render();
  }

  return {
    render(s, r) { state = s; route = r; render(); },
    // setAgent narrows the list to one contact, for navigation from Contacts.
    setAgent(name) { filter.agent = name ?? ''; render(); },
    focusSearch() { search.focus(); search.select?.(); },
    isSearchTarget(target) { return target === search; },
    clearSearch() { search.value = ''; filter.query = ''; search.blur(); render(); },
    move,
    jump,
    openSelected,
    archiveSelected,
    snoozeSelected,
    flagSelected,
    goodSelected,
    clearFilters,
  };
}
