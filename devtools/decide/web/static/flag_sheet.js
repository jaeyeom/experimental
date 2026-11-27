// Flag modal sheet: multi-reason checklist grouped by category + note.
// All DOM uses el()/textContent only (never innerHTML).
import { el } from './dom.js';

export function groupReasons(reasons) {
  const groups = [];
  const byCat = new Map();
  for (const r of (reasons ?? [])) {
    const cat = r.category || 'general';
    if (!byCat.has(cat)) {
      const g = { category: cat, items: [] };
      byCat.set(cat, g);
      groups.push(g);
    }
    byCat.get(cat).items.push(r);
  }
  return groups;
}

export function allowsEarlierItem(reasons, selectedIds) {
  const byId = new Map((reasons ?? []).map((r) => [String(r.id).toLowerCase(), r]));
  for (const raw of (selectedIds ?? [])) {
    const key = String(raw ?? '').trim().toLowerCase();
    if (!key) continue;
    const r = byId.get(key);
    if (r?.allowEarlierItem || key === 'already-decided' || String(r?.category ?? '').toLowerCase() === 'escalation') {
      return true;
    }
  }
  return false;
}

export function validateFlagInput(reasons, selectedIds, note, earlierItemId = '') {
  const ids = [...new Set((selectedIds ?? []).map((s) => String(s ?? '').trim()).filter(Boolean))];
  if (!ids.length) return { ok: false, error: 'Pick at least one reason.' };
  const byId = new Map((reasons ?? []).map((r) => [String(r.id).toLowerCase(), r]));
  const trimmedNote = String(note ?? '').trim();
  for (const id of ids) {
    const r = byId.get(id.toLowerCase());
    if ((r?.requireNote || id.toLowerCase() === 'other') && !trimmedNote) {
      return { ok: false, error: `A note is required when selecting "${id}".` };
    }
  }
  const trimmedEarlier = String(earlierItemId ?? '').trim();
  if (/\s/.test(trimmedEarlier)) {
    return { ok: false, error: 'Earlier item ID must not contain whitespace.' };
  }
  const earlier = allowsEarlierItem(reasons, ids) ? trimmedEarlier : '';
  return { ok: true, reasonIds: ids, note: trimmedNote, earlierItemId: earlier };
}

export function resolveClientFlagTarget(item, messageIndex = null) {
  const msgs = Array.isArray(item?.messages) ? item.messages : [];
  if (messageIndex != null && messageIndex >= 0 && messageIndex < msgs.length) {
    const m = msgs[messageIndex];
    if (m && m.author !== 'human' && m.author !== 'system') {
      return { messageIndex, agent: m.by || m.author || item?.owner || '', preview: m.text || m.verbatim || '' };
    }
  }
  for (let i = msgs.length - 1; i >= 0; i--) {
    const m = msgs[i];
    if (m && m.author !== 'human' && m.author !== 'system') {
      return { messageIndex: i, agent: m.by || m.author || item?.owner || '', preview: m.text || m.verbatim || '' };
    }
  }
  return { messageIndex: null, agent: item?.owner || '', preview: item?.question || item?.verbatim || '' };
}

function previewLine(text) {
  const first = String(text ?? '').trim().split('\n', 1)[0] ?? '';
  return first.length > 96 ? first.slice(0, 93) + '…' : first;
}

// openFlagSheet displays the reason checklist dialog for flagging an item or message.
export function openFlagSheet({ item, messageIndex = null, reasons, onSubmit, restoreFocus }) {
  const target = resolveClientFlagTarget(item, messageIndex);
  const dlg = el('dialog', { class: 'sheet flag-sheet', 'aria-labelledby': 'flag-title' });
  const err = el('p', { class: 'error', role: 'alert', hidden: true });
  const close = () => dlg.close();

  const subtitle = target.agent ? `${item.id} · agent: ${target.agent}` : item.id;
  const header = el('h2', { id: 'flag-title', text: `Flag ${subtitle}` });
  const previewText = previewLine(target.preview);
  const body = [];
  if (previewText) body.push(el('p', { class: 'muted flag-preview', text: previewText }));

  const checkboxes = new Map();
  const earlierInput = el('input', {
    type: 'text',
    id: 'flag-earlier-item',
    class: 'flag-earlier-input',
    maxlength: '128',
    placeholder: 'Earlier item ID (optional, e.g. D-001)',
  });
  const earlierWrap = el('div', { class: 'flag-earlier-wrap', hidden: true },
    el('label', { for: 'flag-earlier-item', text: 'Earlier item ID (optional)' }),
    earlierInput);

  const selectedIds = () => {
    const ids = [];
    for (const [id, cb] of checkboxes) {
      if (cb.checked) ids.push(id);
    }
    return ids;
  };
  const syncEarlierVisibility = () => {
    earlierWrap.hidden = !allowsEarlierItem(reasons, selectedIds());
  };

  for (const g of groupReasons(reasons)) {
    const fs = el('fieldset', { class: 'flag-group' }, el('legend', { text: g.category }));
    for (const r of g.items) {
      const cb = el('input', { type: 'checkbox', class: 'check', value: r.id });
      cb.addEventListener('change', syncEarlierVisibility);
      checkboxes.set(r.id, cb);
      const labelText = r.label ? `${r.id} — ${r.label}` : r.id;
      fs.append(el('label', { class: 'check-row' }, cb, el('span', { text: labelText })));
    }
    body.push(fs);
  }

  const note = el('textarea', {
    id: 'flag-note',
    class: 'reply-text flag-note',
    rows: '2',
    maxlength: '4000',
    placeholder: 'Optional details (required when "other" is selected)',
  });
  body.push(earlierWrap, el('label', { for: 'flag-note', text: 'Note' }), note, err);

  const save = el('button', { type: 'button', class: 'primary', text: 'Save flag' });
  const cancel = el('button', { type: 'button', text: 'Cancel' });
  cancel.addEventListener('click', close);

  save.addEventListener('click', async () => {
    const selected = selectedIds();
    const v = validateFlagInput(reasons, selected, note.value, earlierInput.value);
    if (!v.ok) {
      err.textContent = v.error;
      err.hidden = false;
      if (selected.length && !note.value.trim()) note.focus();
      return;
    }
    err.hidden = true;
    save.disabled = true;
    try {
      const payload = { messageIndex: target.messageIndex, reasons: v.reasonIds, note: v.note };
      if (v.earlierItemId) payload.earlierItemId = v.earlierItemId;
      await onSubmit(payload);
      close();
    } catch (e) {
      err.textContent = e?.data?.error ?? 'Could not save flag.';
      err.hidden = false;
      save.disabled = false;
    }
  });

  dlg.append(header, ...body, el('div', { class: 'sheet-actions' }, save, cancel));
  dlg.addEventListener('close', () => { dlg.remove(); restoreFocus?.(); });
  document.body.append(dlg);
  dlg.showModal();
  return dlg;
}

// openGoodSheet displays the thumbs-up dialog with an optional note.
export function openGoodSheet({ item, messageIndex = null, onSubmit, restoreFocus }) {
  const target = resolveClientFlagTarget(item, messageIndex);
  const dlg = el('dialog', { class: 'sheet flag-sheet', 'aria-labelledby': 'good-title' });
  const err = el('p', { class: 'error', role: 'alert', hidden: true });
  const close = () => dlg.close();

  const subtitle = target.agent ? `${item.id} · agent: ${target.agent}` : item.id;
  const header = el('h2', { id: 'good-title', text: `Thumbs up ${subtitle}` });
  const previewText = previewLine(target.preview);
  const body = [];
  if (previewText) body.push(el('p', { class: 'muted flag-preview', text: previewText }));

  const note = el('textarea', {
    id: 'good-note',
    class: 'reply-text flag-note',
    rows: '2',
    maxlength: '4000',
    placeholder: 'Optional note (what worked well)',
  });
  body.push(el('label', { for: 'good-note', text: 'Note (optional)' }), note, err);

  const save = el('button', { type: 'button', class: 'primary', text: 'Save 👍' });
  const cancel = el('button', { type: 'button', text: 'Cancel' });
  cancel.addEventListener('click', close);

  save.addEventListener('click', async () => {
    err.hidden = true;
    save.disabled = true;
    try {
      await onSubmit({ messageIndex: target.messageIndex, note: note.value.trim() });
      close();
    } catch (e) {
      err.textContent = e?.data?.error ?? 'Could not save thumbs-up.';
      err.hidden = false;
      save.disabled = false;
    }
  });

  dlg.append(header, ...body, el('div', { class: 'sheet-actions' }, save, cancel));
  dlg.addEventListener('close', () => { dlg.remove(); restoreFocus?.(); });
  document.body.append(dlg);
  dlg.showModal();
  return dlg;
}
