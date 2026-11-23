// Pure thread model. It mirrors devtools/decide: mark.go (delivery marks),
// inbox.go (ApprovalCard), worker.go (ReplyTargets, RouteWarning),
// tui/model.go (isOpenQuestion, canSendAndArchive) and tui/drafts.go
// (formatMergeConflict). No DOM here: it runs under node for tests.
import { lastStamp } from './inbox_model.js';

const ms = (t) => {
  const n = typeof t === 'number' ? t : Date.parse(t);
  return Number.isNaN(n) ? 0 : n;
};
// A zero Go time marshals as 0001-01-01; treat it like a missing one.
const hasTime = (t) => !!t && ms(t) > 0 && !String(t).startsWith('0001-');
const pad = (n) => String(n).padStart(2, '0');
export function hhmm(t) {
  const d = new Date(t);
  return `${pad(d.getHours())}:${pad(d.getMinutes())}`;
}

// openAtIndex is the index of the first message from someone else that is
// newer than the previous read, or -1 when nothing is new. Your own and
// system messages are never "new".
export function openAtIndex(it) {
  const msgs = it?.messages ?? [];
  const seen = hasTime(it?.readAt) ? ms(it.readAt) : 0;
  return msgs.findIndex((m) => m.author !== 'human' && m.author !== 'system' && ms(m.time) > seen);
}

export const newDividerLabel = (readAt) =>
  hasTime(readAt) ? `── new since ${hhmm(readAt)} ──` : '── new ──';

// --- delivery marks (mark.go) ---
const heldText = (reason) => {
  switch (reason) {
  case 'transport unreachable': return 'bus down';
  case 'composer busy': return 'target composer busy';
  case 'other': return '';
  }
  return reason ?? '';
};

const deliveryFor = (it, d, at) => {
  if (hasTime(d.message)) return ms(d.message) === ms(at);
  return hasTime(it.answeredAt) && ms(at) === ms(it.answeredAt) && ms(d.at) >= ms(it.answeredAt);
};

// deliveryMark is the mark text beside a human message, or '' for none.
export function deliveryMark(it, msg) {
  if (!it || msg?.author !== 'human' || !hasTime(msg.time)) return '';
  const list = it.delivery ?? [];
  for (let i = list.length - 1; i >= 0; i--) {
    const d = list[i];
    const state = d.outcome === 'queued' ? 'waiting' : d.outcome;
    if (state === 'resend' || !deliveryFor(it, d, msg.time)) continue;
    switch (state) {
    case 'waiting': {
      const r = heldText(d.reason);
      return r ? `waiting: ${r}` : 'waiting';
    }
    case 'delivered': {
      const at = hasTime(d.deliveredAt) ? d.deliveredAt : d.at;
      const mins = Math.round((ms(at) - ms(d.at)) / 60000);
      return `delivered ${hhmm(at)}` + (mins > 0 ? `, ${mins} min late` : '');
    }
    case 'failed': return d.error ? `failed: ${d.error}` : 'failed';
    }
    return state;
  }
  return '';
}

// --- approvals and questions ---
const cls = (it) => it.class || 'decision';

// approvalCardFor is the open approval to act on for it: it itself when
// it is an open approval, else the newest open approval filed under it.
export function approvalCardFor(items, it) {
  if (!it) return null;
  if (cls(it) === 'approval') return it.state === 'open' ? it : null;
  let best = null;
  for (const c of items ?? []) {
    if (c.parent !== it.id || cls(c) !== 'approval' || c.state !== 'open') continue;
    if (!best || ms(lastStamp(c)) >= ms(lastStamp(best))) best = c;
  }
  return best;
}

export const isOpenQuestion = (it) => !!it && it.kind === 'relay' && it.state === 'open'
  && ((it.options?.length ?? 0) > 0 || !!it.freeText);

// canSendAndArchive: never on an open question or a pending answer, nor
// on a closed item.
export function canSendAndArchive(it, pendingAnswer) {
  if (!it || pendingAnswer) return false;
  if (it.state === 'done' || it.state === 'withdrawn') return false;
  return !isOpenQuestion(it);
}

// --- routing (worker.go) ---
export const hasOtherWorker = (it) => !!it?.worker && it.worker !== it.owner;

export const routeWarning = (it) => hasOtherWorker(it)
  ? `This reply goes to ${it.owner} (owner); ${it.worker} is working on it.` : '';

const BUS = 'agentbus:';
const busName = (pane) => {
  const t = String(pane ?? '').trim();
  return t.startsWith(BUS) && t.slice(BUS.length).trim() !== '';
};

// replyOnBus: does a reply with this route reach an agentbus recipient?
export function replyOnBus(it, route) {
  const worker = hasOtherWorker(it) && (route === 'worker' || route === 'both');
  const panes = [];
  if ((!worker || route === 'both') && !it.notifyClosed) panes.push(it.notifyPane);
  if (worker) panes.push(it.workerNotifyPane || BUS + it.worker);
  return panes.some(busName);
}

// --- drafts ---
// mergeMarkers is the "Merge both" text, as the TUI writes it.
export function mergeMarkers(current, other, { updatedBy = '', updatedAt = '' } = {}) {
  const nl = (s) => (s !== '' && !s.endsWith('\n') ? s + '\n' : s);
  return `<<<<<<< current\n${nl(current)}=======\n${nl(other)}>>>>>>> ${updatedBy || 'other'} ${updatedAt}`;
}

// --- sends ---
// answerPlan builds the answer request for item it. The conflictCheck makes
// the server refuse it when the item changed since it was shown, so a stale
// approval can never be answered. A blank note is left out.
export function answerPlan(it, value, note = '') {
  const body = {
    value,
    conflictCheck: {
      baseState: it.state,
      baseMessageCount: it.messages?.length ?? 0,
      baseLastMessageAt: lastStamp(it) ?? null,
      baseAnswered: !!it.answer,
    },
  };
  if (note.trim() !== '') body.note = note;
  return { op: 'answer', id: it.id, body };
}

// replyPlan decides what Send does: a pending option/edit answer carries the
// text as its note; free text on an open question is the answer; anything
// else is a reply, routed when a route was picked.
export function replyPlan(it, text, { route = '', pending = null } = {}) {
  if (pending) return answerPlan(pending.item, pending.value, text);
  if (isOpenQuestion(it) && it.freeText) return answerPlan(it, text);
  const body = { text };
  if (route) body.route = route;
  return { op: 'reply', id: it.id, body };
}

// threadText is the plain-text copy of a thread, oldest first.
export function threadText(it) {
  const msgs = it.messages?.length ? it.messages : [{ author: it.owner, time: it.createdAt, text: it.question }];
  const lines = [`${it.id} ${it.question ?? ''}`.trim()];
  for (const m of msgs) {
    lines.push('', `${m.by || m.author} ${hhmm(m.time)}:`, m.text ?? '');
    if (m.verbatim) lines.push('---', m.verbatim, '---');
  }
  return lines.join('\n');
}

// messagesOf is the messages to show: the thread, or the ask alone for an
// item that has none yet.
export const messagesOf = (it) => it.messages?.length ? it.messages
  : [{ author: it.owner, time: it.createdAt, text: it.question ?? '', verbatim: it.verbatim }];
