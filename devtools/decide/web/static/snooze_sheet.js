// Snooze bottom sheet: presets, a custom time, and Unsnooze. A native
// <dialog> gives focus trapping, Esc to close and a labelled modal.
import { el } from './dom.js';

// preset names are resolved by the server in its local time, as the TUI does.
export const PRESETS = [
  { preset: '1h', label: '1 hour' },
  { preset: '3h', label: '3 hours' },
  { preset: 'tomorrow9', label: 'Tomorrow 9:00' },
  { preset: 'nextmon9', label: 'Next Monday 9:00' },
];

// customUntil turns a datetime-local value (local time) into an ISO
// instant, or an error message when it is empty or not in the future.
export function customUntil(value, now = Date.now()) {
  const t = value ? new Date(value).getTime() : NaN;
  if (Number.isNaN(t)) return { error: 'Pick a date and time.' };
  if (t <= now) return { error: 'Pick a time in the future.' };
  return { until: new Date(t).toISOString() };
}

// openSnoozeSheet shows the sheet for one item. pick receives
// {preset} or {until}; unsnooze is offered only when given.
export function openSnoozeSheet({ title, pick, unsnooze, restoreFocus }) {
  const dlg = el('dialog', { class: 'sheet', 'aria-labelledby': 'sheet-title' });
  const err = el('p', { class: 'error', role: 'alert', hidden: true });
  const close = () => { dlg.close(); };
  const choose = (body) => { close(); pick(body); };

  const presets = el('div', { class: 'sheet-presets' });
  for (const p of PRESETS) {
    const b = el('button', { type: 'button', text: p.label });
    b.addEventListener('click', () => choose({ preset: p.preset }));
    presets.append(b);
  }
  const input = el('input', { id: 'sheet-custom', type: 'datetime-local' });
  const go = el('button', { type: 'button', class: 'primary', text: 'Snooze until' });
  go.addEventListener('click', () => {
    const r = customUntil(input.value);
    if (r.error) { err.textContent = r.error; err.hidden = false; input.focus(); return; }
    choose({ until: r.until });
  });
  const cancel = el('button', { type: 'button', text: 'Cancel' });
  cancel.addEventListener('click', close);

  dlg.append(el('h2', { id: 'sheet-title', text: title }), presets,
    el('label', { for: 'sheet-custom', text: 'Custom time' }), input, err,
    el('div', { class: 'sheet-actions' }, go, cancel));
  if (unsnooze) {
    const u = el('button', { type: 'button', text: 'Unsnooze' });
    u.addEventListener('click', () => { close(); unsnooze(); });
    dlg.append(u);
  }
  dlg.addEventListener('close', () => { dlg.remove(); restoreFocus?.(); });
  document.body.append(dlg);
  dlg.showModal();
  return dlg;
}
