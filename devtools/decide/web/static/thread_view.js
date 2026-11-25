// Thread detail view: header, safe message rendering, approval cards,
// decision options, and the pinned reply box with draft sync. All text is
// set with textContent or text nodes; item text is never parsed as HTML.
import * as api from './api.js';
import { el, btn, keyHint } from './dom.js';
import { parseBlocks, linkify } from './text_render.js';
import { badgeOf, effectiveClass, hasUnread, deadlineLabel, lastStamp } from './inbox_model.js';
import {
  openAtIndex, newDividerLabel, deliveryMark, approvalCardFor, canSendAndArchive, hasOtherWorker,
  routeWarning, replyOnBus, answerPlan, replyPlan, messagesOf, threadText, hhmm, isOpenQuestion,
  closedLine,
} from './thread_model.js';
import { createDraftSync } from './draft_sync.js';
import { loadLocalDraft, saveLocalDraft, busDown } from './store.js';
import { openSheet } from './sheet.js';
import { openSnoozeSheet } from './snooze_sheet.js';

// renderText turns message text into DOM: text and code blocks, with
// http(s) links as anchors. Nothing here can create an element from text.
export function renderText(text) {
  const root = el('div', { class: 'text' });
  for (const b of parseBlocks(text)) {
    if (b.type === 'code') {
      root.append(el('pre', { class: 'code' }, el('code', { text: b.text })));
      continue;
    }
    const p = el('p', { class: 'para' });
    for (const s of linkify(b.text)) {
      p.append(s.type === 'link'
        ? el('a', { href: s.href, target: '_blank', rel: 'noopener noreferrer', text: s.text })
        : document.createTextNode(s.text));
    }
    root.append(p);
  }
  return root;
}

export const verbatimCard = (text) => el('figure', { class: 'verbatim' },
  el('figcaption', { text: 'Will be posted as written' }), el('pre', { text }));

const pill = (text, cls = '') => el('span', { class: 'pill ' + cls, text });
async function copy(text, toast, what) {
  try { await navigator.clipboard.writeText(text); toast(`Copied ${what}`); } catch { toast('Could not copy'); }
}

export function createThreadView({ root, toast, hint = () => '' }) {
  let cur = null; // the open thread
  let state = null;

  const draftOf = (id) => (state?.drafts ?? []).find((d) => d.key === 'reply:' + id) ?? null;
  const itemOf = (id) => state?.items.find((x) => x.id === id) ?? null;

  // ---- answering ----
  async function submitAnswer(target, value) {
    try {
      const res = await api.answer(target.id, answerPlan(target, value).body);
      cur.notice = res.notifyError ? `Answer saved, but notifying the agent failed: ${res.notifyError}` : '';
      toast(`Answered ${target.id}: ${value}`);
    } catch (e) {
      const msg = e?.data?.error;
      toast(api.conflict(e) ? `Not sent: ${msg}. Review and try again.` : (msg || 'Could not send. Try again.'));
    }
    if (cur) update();
  }

  function approveSheet(card) {
    openSheet({
      title: `Approve ${card.id}?`,
      body: [el('p', { text: 'This authorizes the exact text above.' }),
        verbatimCard(card.verbatim || card.question || '')],
      actions: [{ label: 'Confirm Approve', primary: true, armMs: 600, run: () => submitAnswer(card, 'yes') }],
    });
  }
  function declineSheet(card) {
    openSheet({
      title: `Decline ${card.id}?`,
      body: [verbatimCard(card.verbatim || card.question || '')],
      actions: [{ label: 'Confirm Decline', danger: true, run: () => submitAnswer(card, 'no') }],
    });
  }
  function startPending(target, value) {
    cur.pending = { item: target, value };
    cur.refs.ta.focus();
    updateReplyChrome();
  }

  function approvalCard(card) {
    return el('section', { class: 'approval-card', 'aria-label': `Approval ${card.id}` },
      el('h2', { text: `Approval ${card.id}` }),
      card.question ? renderText(card.question) : '',
      verbatimCard(card.verbatim || '(no exact text attached)'),
      el('div', { class: 'approval-actions' },
        btn('Approve', () => approveSheet(card), 'primary'),
        btn('Edit', () => startPending(card, 'edit')),
        btn('Decline', () => declineSheet(card), 'danger')));
  }

  function optionsPanel(it) {
    const box = el('section', { class: 'options', 'aria-label': 'Options' });
    for (const o of it.options) {
      const main = btn('', () => { for (const b of box.querySelectorAll('button')) b.disabled = true; submitAnswer(it, o.key); }, 'option');
      main.append(el('span', { class: 'key mono', text: o.key }), el('span', { class: 'label', text: o.label }));
      if (o.recommended) main.append(el('span', { class: 'pill rec', text: 'Recommended' }));
      main.setAttribute('aria-label', `${o.key}: ${o.label}${o.recommended ? ' (recommended)' : ''}`);
      const note = btn('+ note', () => startPending(it, o.key), 'note-btn', { 'aria-label': `Answer ${o.key} with a note` });
      box.append(el('div', { class: 'option-row' }, main, note));
    }
    return box;
  }

  function typeInPanel(it) {
    return el('section', { class: 'type-in', 'aria-label': 'Type in' },
      el('p', {}, 'Type this where: ', el('strong', { text: it.where || '(unspecified)' })),
      el('pre', { class: 'type-text', text: it.typeText }),
      btn('Copy exact text', () => copy(it.typeText, toast, 'text'), 'primary'));
  }

  // ---- header and overflow ----
  function overflow(it) {
    const actions = [];
    if (it.state !== 'done' && it.state !== 'withdrawn') {
      actions.push({ label: 'Done with outcome', run: () => doneSheet(it) });
      actions.push({ label: it.snoozedUntil ? 'Snooze / Unsnooze' : 'Snooze', run: () => snoozeSheet(it) });
    }
    actions.push({ label: 'Forward', run: () => { location.hash = '#/compose/' + encodeURIComponent(it.id); } },
      { label: 'Copy ID', run: () => copy(it.id, toast, 'ID') },
      { label: 'Copy Link', run: () => copy(`${location.origin}/#/item/${encodeURIComponent(it.id)}`, toast, 'link') },
      { label: 'Copy Thread', run: () => copy(threadText(it), toast, 'thread') });
    openSheet({ title: it.id, actions, cancelLabel: 'Close' });
  }
  function doneSheet(it) {
    const input = el('input', { type: 'text', id: 'outcome', 'aria-label': 'Outcome', placeholder: 'Outcome (optional)', maxlength: '2000' });
    openSheet({ title: `Done with ${it.id}`, body: [input], actions: [{
      label: 'Mark done', primary: true,
      run: async () => {
        try { await api.done(it.id, input.value.trim()); toast(`Done ${it.id}`); } catch { toast('Could not mark done. Try again.'); }
      } }] });
  }
  async function archiveItem(it) {
    if (!it || it.state === 'done' || it.state === 'withdrawn') return;
    try {
      await api.archive(it.id, lastStamp(it));
      toast(`Archived ${it.id}`);
    } catch (e) {
      toast(api.newerMessage(e) ? 'New message arrived — review before archiving' : 'Could not archive. Try again.');
    }
  }

  function snoozeSheet(it) {
    openSnoozeSheet({
      title: `Snooze ${it.id}`,
      pick: async (b) => { try { await api.snooze(it.id, b); toast(`Snoozed ${it.id}`); } catch (e) { toast(e?.data?.error ?? 'Could not snooze.'); } },
      unsnooze: it.snoozedUntil ? async () => { try { await api.unsnooze(it.id); toast(`Unsnoozed ${it.id}`); } catch { toast('Could not unsnooze.'); } } : null,
    });
  }

  function header(it) {
    const cls = badgeOf(it);
    const pills = [pill(it.state)];
    const dl = it.deadline ? deadlineLabel(it.deadline, Date.now()) : '';
    if (dl) pills.push(pill(dl, dl.startsWith('overdue') ? 'bad' : ''));
    if (it.snoozedUntil && Date.parse(it.snoozedUntil) > Date.now()) {
      pills.push(pill('snoozed until ' + new Date(it.snoozedUntil).toLocaleString([], { weekday: 'short', hour: '2-digit', minute: '2-digit' }), 'warn'));
    }
    return el('header', { class: 'thread-head' },
      el('a', { class: 'back', href: '#/inbox', 'aria-label': 'Back to inbox', text: '←' }),
      el('span', { class: 'cls cls-' + cls, text: cls, title: effectiveClass(it) }),
      el('span', { class: 'mono id', text: it.id }),
      el('span', { class: 'owner', text: it.owner ?? '' }),
      ...(it.worker ? [pill(`Working on it: ${it.worker}`, 'warn')] : []),
      ...pills,
      btn('⋯', () => overflow(it), 'menu-btn', { 'aria-label': 'More actions' }));
  }

  // ---- messages ----
  function messageItem(it, m) {
    const mark = deliveryMark(it, m);
    const head = el('div', { class: 'msg-head' },
      el('span', { class: 'who', text: m.by || m.author }),
      el('time', { text: hhmm(m.time) }),
      ...(mark ? [el('span', { class: 'mark' + (mark.startsWith('failed') ? ' bad' : ''), text: mark })] : []));
    const li = el('li', { class: 'msg' + (m.author === 'human' ? ' mine' : '') }, head);
    if (m.optionKey) li.append(el('p', { class: 'muted', text: `answered: ${m.optionKey}${m.optionLabel ? ' ' + m.optionLabel : ''}` }));
    if (m.text) li.append(renderText(m.text));
    if (m.verbatim) li.append(verbatimCard(m.verbatim));
    return li;
  }

  function messages(it) {
    const list = el('ol', { class: 'messages' });
    messagesOf(it).forEach((m, i) => {
      if (i === cur.dividerIdx) {
        const d = el('li', { class: 'divider', role: 'separator', text: cur.dividerLabel });
        cur.dividerEl = d;
        list.append(d);
      }
      list.append(messageItem(it, m));
    });
    return list;
  }

  // ---- updating ----
  function update() {
    const it = itemOf(cur.id);
    if (!it) return;
    cur.item = it;
    const card = approvalCardFor(state.items, it);
    cur.dividerEl = null;
    const parts = [];
    if (cur.notice) parts.push(el('p', { class: 'notice error', role: 'alert', text: cur.notice }));
    if (card) parts.push(approvalCard(card));
    const closed = closedLine(it);
    if (closed) parts.push(el('p', { class: 'closed-notice muted', text: closed }));
    if (it.kind === 'relay' && it.state === 'open' && it.options?.length && effectiveClass(it) !== 'approval') parts.push(optionsPanel(it));
    if (it.kind === 'type-in' && it.state === 'open' && it.typeText) parts.push(typeInPanel(it));
    const w = routeWarning(it);
    if (w) parts.push(el('p', { class: 'muted', text: w }));
    parts.push(messages(it));
    cur.refs.head.replaceChildren(...header(it).childNodes);
    cur.refs.body.replaceChildren(...parts);
    if (hasUnread(it) && (it.messages?.length ?? 0) > cur.markedCount) {
      cur.markedCount = it.messages?.length ?? 0;
      api.markRead(it.id).catch(() => {});
    }
    syncRemoteDraft();
    updateReplyChrome();
  }

  // A draft saved by another client shows up through the event stream.
  function syncRemoteDraft() {
    const d = draftOf(cur.id);
    const s = cur.sync.state();
    if (!d || d.rev <= s.baseRev || s.dirty) {
      if (d && s.dirty && d.rev > s.baseRev) cur.sync.flush(); // surfaces a conflict
      return;
    }
    cur.sync.adopt(d);
    if (document.activeElement !== cur.refs.ta || cur.refs.ta.value === '') {
      cur.refs.ta.value = d.text;
      autosize();
    }
  }

  // ---- reply box ----
  const autosize = () => {
    const ta = cur.refs.ta;
    ta.style.height = 'auto';
    ta.style.height = Math.min(ta.scrollHeight, window.innerHeight * 0.4) + 'px';
  };

  function updateReplyChrome() {
    const { ta, form, archiveBtn, chip, banner } = cur.refs;
    const empty = ta.value.trim() === '';
    const focused = form.contains(document.activeElement);
    form.classList.toggle('collapsed', empty && !focused && !cur.pending);
    archiveBtn.hidden = !canSendAndArchive(cur.item, cur.pending);
    chip.hidden = !cur.pending;
    chip.replaceChildren();
    if (cur.pending) {
      chip.append(el('span', { text: `Answering ${cur.pending.item.id} with: ${cur.pending.value}` }),
        btn('×', () => { cur.pending = null; updateReplyChrome(); }, 'x', { 'aria-label': 'Cancel pending answer' }));
    }
    const s = cur.sync.state();
    const notes = [];
    if (s.status === 'conflict' && s.conflicts.length) {
      const c = s.conflicts[0];
      notes.push(el('div', { class: 'draft-banner', role: 'alert' },
        el('p', { text: `Another client saved a different draft${c.updatedBy ? ' (' + c.updatedBy + ')' : ''}.` }),
        el('pre', { class: 'conflict-text', text: c.text }),
        el('div', { class: 'row-btns' },
          btn('Keep current', () => resolve('keep')), btn('Use other', () => resolve('other')),
          btn('Merge both', () => resolve('merge')))));
    }
    if (s.status === 'error') {
      notes.push(el('div', { class: 'draft-banner', role: 'alert' },
        el('p', { text: 'Could not save the draft. Your text is kept on this device.' }),
        btn('Retry', () => cur.sync.retry())));
    }
    if (cur.sendError) {
      notes.push(el('div', { class: 'draft-banner', role: 'alert' },
        el('p', { text: cur.sendError.text }), btn('Retry', () => send(cur.sendError.archive))));
    }
    banner.replaceChildren(...notes);
  }

  async function resolve(action) {
    try {
      const next = await cur.sync.resolve(action, cur.refs.ta.value);
      cur.refs.ta.value = next;
      saveLocalDraft('reply:' + cur.id, next).catch(() => {});
      autosize();
    } catch { toast('Could not resolve the draft conflict. Try again.'); }
    updateReplyChrome();
  }

  // send walks the checks the TUI makes, in order: owner/worker route, then
  // bus down, then the write.
  function send(andArchive) {
    const it = cur.item;
    const text = cur.refs.ta.value;
    if (text.trim() === '' && !cur.pending) return;
    cur.sendError = null;
    const plain = !cur.pending && !(isOpenQuestion(it) && it.freeText);
    const go = (route) => {
      const ask = () => deliver(andArchive, route);
      if (replyOnBus(it, route) && busDown(state.busHealth)) {
        openSheet({
          title: "The bus is down. This message will wait until it's fixed.",
          actions: [{ label: 'Send anyway', primary: true, run: ask }],
        });
      } else ask();
    };
    if (plain && hasOtherWorker(it) && !cur.route) {
      openSheet({
        title: routeWarning(it),
        actions: [
          { label: `Send to ${it.worker} (worker)`, primary: true, run: () => go('worker') },
          { label: 'Send to both', run: () => go('both') },
          { label: `Send to ${it.owner} (owner only)`, run: () => go('owner') },
        ],
      });
      return;
    }
    go(plain ? cur.route : '');
  }

  async function deliver(andArchive, route) {
    const it = cur.item;
    const plan = replyPlan(it, cur.refs.ta.value, { route, pending: cur.pending });
    cur.refs.send.disabled = true;
    try {
      const res = await (plan.op === 'answer' ? api.answer(plan.id, plan.body) : api.reply(plan.id, plan.body));
      cur.notice = res.notifyError ? `Sent, but notifying the agent failed: ${res.notifyError}` : '';
      cur.refs.ta.value = '';
      cur.pending = null;
      cur.route = '';
      autosize();
      await cur.sync.discard();
      if (andArchive) {
        try { await api.archive(it.id, lastStamp(res.item ?? it)); toast(`Sent and archived ${it.id}`); } catch { toast('Sent, but could not archive.'); }
      }
    } catch (e) {
      const msg = e?.data?.error;
      cur.sendError = { text: api.conflict(e) ? `Not sent: ${msg}. Your text is kept.` : (msg ? `Not sent: ${msg}. Your text is kept.` : 'Could not send. Your text is kept.'), archive: andArchive };
    } finally {
      if (cur) { cur.refs.send.disabled = false; update(); }
    }
  }

  function discard() {
    const run = async () => {
      cur.refs.ta.value = '';
      cur.pending = null;
      autosize();
      await cur.sync.discard();
      updateReplyChrome();
    };
    if (cur.refs.ta.value.split('\n').length > 3) {
      openSheet({ title: 'Discard this draft?', actions: [{ label: 'Discard', danger: true, run }] });
    } else run();
  }

  function buildReply() {
    const ta = el('textarea', { class: 'reply-text', rows: '1', placeholder: 'Reply…', 'aria-label': 'Reply', maxlength: '60000' });
    const send_ = el('button', { type: 'submit', class: 'primary' }, 'Send', keyHint(hint('compose.send')));
    const archiveBtn = el('button', { type: 'button', hidden: true }, 'Send & Archive', keyHint(hint('compose.sendAndArchive')));
    archiveBtn.addEventListener('click', () => send(true));
    const discardBtn = el('button', { type: 'button' }, 'Discard', keyHint(hint('compose.discard')));
    discardBtn.addEventListener('click', discard);
    const chip = el('div', { class: 'pending-chip', hidden: true });
    const banner = el('div', { class: 'reply-banners' });
    const actions = el('div', { class: 'reply-actions' }, send_, archiveBtn, discardBtn);
    const form = el('form', { class: 'reply collapsed', autocomplete: 'off' }, banner, chip, ta, actions);
    form.addEventListener('submit', (e) => { e.preventDefault(); send(false); });
    ta.addEventListener('input', () => {
      cur.sync.edit(ta.value);
      autosize();
      updateReplyChrome();
    });
    form.addEventListener('focusin', updateReplyChrome);
    form.addEventListener('focusout', () => setTimeout(() => { if (cur) updateReplyChrome(); }, 0));
    return { form, ta, send: send_, archiveBtn, discardBtn, chip, banner };
  }

  function refreshHints() {
    if (!cur) return;
    cur.refs.send.replaceChildren('Send', keyHint(hint('compose.send')));
    cur.refs.archiveBtn.replaceChildren('Send & Archive', keyHint(hint('compose.sendAndArchive')));
    cur.refs.discardBtn.replaceChildren('Discard', keyHint(hint('compose.discard')));
  }

  // ---- lifecycle ----
  function enter(it) {
    const id = it.id;
    const key = 'reply:' + id;
    const refs = { head: el('header', { class: 'thread-head' }), body: el('div', { class: 'thread-body' }) };
    cur = {
      id, item: it, refs, pending: null, route: '', notice: '', sendError: null, markedCount: 0,
      dividerIdx: -1, dividerLabel: '', dividerEl: null, sync: null, msgIdx: -1,
    };
    const idx = openAtIndex(it);
    if (idx > 0) { cur.dividerIdx = idx; cur.dividerLabel = newDividerLabel(it.readAt); }
    Object.assign(refs, buildReply());
    cur.sync = createDraftSync({
      key, itemId: id, delay: 1500,
      put: (k, d, base) => api.putDraft(k, d, base),
      remove: (k, cid) => api.removeDraft(k, cid),
      saveLocal: (k, t) => saveLocalDraft(k, t).catch(() => {}),
      setTimer: (fn, ms) => setTimeout(fn, ms), clearTimer: (t) => clearTimeout(t),
      onStatus: () => { if (cur?.id === id) updateReplyChrome(); },
    });
    root.replaceChildren(el('div', { class: 'thread' }, refs.head, refs.body, refs.form));
    const server = draftOf(id);
    if (server) cur.sync.adopt(server);
    refs.ta.value = server?.text ?? '';
    // A buffer on this device that never reached the server wins and is saved.
    loadLocalDraft(key).then((t) => {
      if (cur?.id !== id || typeof t !== 'string' || t.trim() === '' || t === refs.ta.value) return;
      refs.ta.value = t;
      cur.sync.edit(t);
      autosize();
      updateReplyChrome();
    }).catch(() => {});
    update();
    autosize();
    requestAnimationFrame(() => {
      const open = (state.display?.keys?.ui?.openAt ?? 'new') === 'new';
      const target = open ? cur?.dividerEl : null;
      if (target) target.scrollIntoView({ block: 'start' });
      else refs.body.lastElementChild?.lastElementChild?.scrollIntoView?.({ block: 'end' });
    });
  }

  function leave() {
    if (!cur) return;
    const c = cur;
    cur = null;
    c.sync.flush();
  }

  document.addEventListener('visibilitychange', () => { if (cur && document.visibilityState === 'hidden') cur.sync.flush(); });
  window.addEventListener('pagehide', () => { cur?.sync.flush(); });

  function stepMessage(delta) {
    if (!cur) return;
    const msgs = [...cur.refs.body.querySelectorAll('li.msg')];
    if (!msgs.length) return;
    const prev = cur.msgIdx;
    const next = prev < 0 ? (delta > 0 ? 0 : msgs.length - 1) : Math.max(0, Math.min(msgs.length - 1, prev + delta));
    cur.msgIdx = next;
    msgs.forEach((m, i) => m.classList.toggle('msg-active', i === next));
    msgs[next].scrollIntoView({ block: 'nearest' });
  }

  function answerOption(index) {
    if (!cur) return;
    const buttons = cur.refs.body.querySelectorAll('.options button.option');
    if (buttons.length) {
      const target = buttons[index - 1];
      if (target && !target.disabled) target.click();
      return;
    }
    const approvalBtns = cur.refs.body.querySelectorAll('.approval-card .approval-actions button');
    const target = approvalBtns[index - 1];
    if (target && !target.disabled) target.click();
  }

  return {
    render(s, route) {
      state = s;
      if (route.name !== 'item') { leave(); return; }
      const it = itemOf(route.arg);
      if (!it) {
        leave();
        root.replaceChildren(el('p', { class: 'empty', text: 'Item not found.' }));
        return;
      }
      if (cur?.id !== route.arg) { leave(); enter(it); } else update();
    },
    leave,
    refreshHints,
    scrollBy(px) { root.scrollBy?.({ top: px, behavior: 'auto' }); },
    scrollTo(where) { root.scrollTop = where === 'bottom' ? root.scrollHeight : 0; },
    stepMessage,
    answerOption,
    focusReply() { cur?.refs.ta.focus(); },
    archive() { if (cur) archiveItem(cur.item); },
    snooze() { if (cur && cur.item.state !== 'done' && cur.item.state !== 'withdrawn') snoozeSheet(cur.item); },
    done() { if (cur && cur.item.state !== 'done' && cur.item.state !== 'withdrawn') doneSheet(cur.item); },
    forward() { if (cur) location.hash = '#/compose/' + encodeURIComponent(cur.item.id); },
    sendReply(andArchive = false) { if (cur) send(andArchive); },
    discardReply() { if (cur) discard(); },
    blurReply() { cur?.refs.ta.blur(); },
    isReplyTarget(target) { return !!cur?.refs?.form?.contains(target); },
  };
}
