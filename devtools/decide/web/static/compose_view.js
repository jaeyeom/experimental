// Compose and Forward view. Built once per entry so typing is never
// disturbed by live updates. #/compose starts a new thread (with a saved
// draft); #/compose/<id> forwards item <id> as a preview that never
// touches the saved compose draft. All text is set via textContent.
import * as api from './api.js';
import { el, btn } from './dom.js';
import { createDraftSync } from './draft_sync.js';
import { loadLocalDraft, saveLocalDraft } from './store.js';
import { openSheet } from './sheet.js';
import {
  recipientList, filterRecipients, sendBody, needsBusConfirm, composeDraftBody, composePlan, forwardPreview,
} from './compose_model.js';

export function createComposeView({ root, toast }) {
  let cur = null;
  let state = null;

  function enter(route) {
    const forward = route.arg;
    const it = forward ? state.items.find((x) => x.id === forward) : null;
    if (forward && !it) {
      root.replaceChildren(el('p', { class: 'empty', text: 'Item to forward not found.' }));
      cur = { key: route.arg + '|' + (route.query?.to ?? ''), sync: null };
      return;
    }
    const c = { key: route.arg + '|' + (route.query?.to ?? ''), forward, selected: null, list: [], favorites: [],
      filter: '', sync: null, plan: null, wantTo: '', sending: false };
    cur = c;

    const filter = el('input', { type: 'search', placeholder: 'Find a recipient', 'aria-label': 'Filter recipients', autocomplete: 'off' });
    const picker = el('div', { class: 'picker', role: 'group', 'aria-label': 'Recipients' });
    const chosen = el('p', { class: 'chosen', 'aria-live': 'polite' });
    const note = el('p', { class: 'muted' });
    const restore = el('div', { class: 'draft-banner', hidden: true, role: 'group', 'aria-label': 'Saved draft' });
    const ta = el('textarea', { class: 'reply-text compose-text', rows: '6', 'aria-label': 'Message', placeholder: 'Message' });
    const send = btn('Send', () => trySend(), 'primary');
    const status = el('span', { class: 'muted', role: 'status', 'aria-live': 'polite' });

    function drawPicker() {
      const rows = filterRecipients(c.list, c.filter);
      picker.replaceChildren(...(rows.length ? rows.map((r) => {
        const label = (r.fav ? '★ ' : '') + r.name + (r.bus ? (r.dead ? ' ✗ dead' : ' ●') : '');
        const b = el('button', { type: 'button', class: 'rec-row' + (r.dead ? ' dim' : ''), 'aria-pressed': String(c.selected?.name === r.name), text: label });
        if (r.when) b.append(el('span', { class: 'muted when', text: ' ' + r.when }));
        b.addEventListener('click', () => { c.selected = r; drawPicker(); drawChosen(); if (c.sync && ta.value.trim()) c.sync.edit(ta.value); });
        return b;
      }) : [el('p', { class: 'muted', text: c.list.length ? 'No match.' : 'No recipients.' })]));
    }
    function drawChosen() {
      chosen.textContent = c.selected ? 'To: ' + c.selected.name + (c.selected.dead ? ' (dead: the bus holds the note until the name is claimed)' : '') : 'Pick a recipient.';
    }
    filter.addEventListener('input', () => { c.filter = filter.value; drawPicker(); });
    ta.addEventListener('input', () => { c.sync?.edit(ta.value); });

    async function trySend() {
      if (c.sending) return;
      const text = ta.value;
      if (!c.selected) { toast('Pick a recipient first.'); return; }
      if (!text.trim()) { toast('Write a message first.'); return; }
      if (needsBusConfirm(c.selected, state.busHealth)) {
        openSheet({
          title: 'Bus is down',
          body: [el('p', { text: `The bus cannot deliver now. The message to ${c.selected.name} is saved and delivered when the bus is back.` })],
          actions: [{ label: 'Send anyway', primary: true, run: () => doSend(text) }],
        });
        return;
      }
      doSend(text);
    }
    async function doSend(text) {
      c.sending = true;
      send.disabled = true;
      try {
        const res = await api.post('/send', sendBody(c.selected, text, c.forward));
        if (c.sync) { await c.sync.discard(); }
        toast(res?.notifyError ? `Sent, but notifying failed: ${res.notifyError}` : `Sent to ${c.selected.name}`);
        location.hash = '#/inbox';
      } catch (e) {
        toast(e?.data?.error ? `Not sent: ${e.data.error}` : 'Could not send. Try again.');
        c.sending = false;
        send.disabled = false;
      }
    }

    root.replaceChildren(el('div', { class: 'compose' },
      el('h2', { text: forward ? `Forward ${forward}` : 'New message' }),
      restore,
      ...(it ? [el('section', { 'aria-label': 'Forwarded thread' }, el('h3', { text: 'Forwarding' }), el('pre', { class: 'code fwd', text: forwardPreview(it) }))] : []),
      el('h3', { text: 'To' }), chosen, filter, picker, note,
      el('h3', { text: forward ? 'Add a note' : 'Message' }), ta,
      el('div', { class: 'reply-actions' }, send, status)));
    drawChosen();
    drawPicker();

    // Recipients: configured targets (from the hub) plus live bus names.
    api.get('/contacts').then((r) => {
      if (cur !== c) return;
      c.favorites = r.favoriteContacts ?? [];
      c.list = recipientList({ targets: state.targets, contacts: r.contacts ?? [], favorites: c.favorites });
      if (r.busError) note.textContent = 'agentbus is unavailable; only configured targets and starred names are listed.';
      const want = c.wantTo || route.query?.to;
      if (want) c.selected = c.list.find((x) => x.name === want) ?? c.selected;
      drawPicker(); drawChosen();
    }).catch(() => {
      if (cur !== c) return;
      c.list = recipientList({ targets: state.targets, contacts: [], favorites: [] });
      note.textContent = 'Could not load contacts; only configured targets are listed.';
      drawPicker();
    });

    // Draft: a plain compose offers a saved draft; a forward ignores it.
    const server = (state.drafts ?? []).find((d) => d.key === 'compose') ?? null;
    c.plan = composePlan({ draft: server, forward });
    if (!c.plan.saveDraft) return;
    c.sync = createDraftSync({
      key: 'compose', delay: 1500,
      makeDraft: (t) => composeDraftBody(c.selected, t),
      put: (k, d, base) => api.putDraft(k, d, base),
      remove: (k, cid) => api.removeDraft(k, cid),
      saveLocal: (k, t) => saveLocalDraft(k, t).catch(() => {}),
      setTimer: (fn, ms) => setTimeout(fn, ms), clearTimer: (t) => clearTimeout(t),
      onStatus: (s) => { status.textContent = s === 'saving' ? 'Saving draft' : s === 'saved' ? 'Draft saved' : s === 'error' ? 'Draft not saved' : ''; },
    });
    if (server) c.sync.adopt(server);
    loadLocalDraft('compose').then((local) => {
      if (cur !== c) return;
      let draft = server;
      if (!composePlan({ draft: server, forward }).prompt && typeof local === 'string' && local.trim()) draft = { text: local };
      if (!draft || !draft.text?.trim()) return;
      ta.disabled = true;
      restore.hidden = false;
      restore.replaceChildren(
        el('p', { text: 'You have a saved draft.' }),
        el('div', { class: 'reply-actions' },
          btn('Restore', () => {
            ta.value = draft.text;
            c.wantTo = draft.to ?? '';
            if (c.wantTo) { c.selected = c.list.find((x) => x.name === c.wantTo) ?? c.selected; drawPicker(); drawChosen(); }
            if (!server) c.sync.edit(draft.text);
            ta.disabled = false; restore.hidden = true; ta.focus();
          }, 'primary'),
          btn('Discard', () => { c.sync.discard(); ta.disabled = false; restore.hidden = true; })));
    }).catch(() => {});
    window.addEventListener('pagehide', () => { if (cur === c) c.sync?.flush(); });
  }

  return {
    render(s, route) {
      state = s;
      if (route.name !== 'compose') { leave(); return; }
      const key = route.arg + '|' + (route.query?.to ?? '');
      if (cur?.key !== key) { leave(); enter(route); }
    },
    leave,
  };

  function leave() {
    if (!cur) return;
    const c = cur;
    cur = null;
    c.sync?.flush();
  }
}
