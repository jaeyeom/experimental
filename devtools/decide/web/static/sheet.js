// Generic confirmation bottom sheet on a native <dialog>: focus is trapped,
// Esc and the backdrop-less Cancel button close it, and focus starts on
// Cancel so a stray Enter or tap never confirms. Actions that matter can
// be armed after a short delay (armMs) to absorb accidental double taps.
import { el } from './dom.js';

// openSheet shows title, body nodes and actions [{label, run, primary?,
// danger?, armMs?}], then Cancel. className 'side' makes it a side panel on desktop. run is called after the sheet closes.
export function openSheet({ title, body = [], actions, cancelLabel = 'Cancel', restoreFocus, className = '' }) {
  const dlg = el('dialog', { class: ('sheet ' + className).trim(), 'aria-labelledby': 'sheet-title' });
  const close = () => dlg.close();
  const cancel = el('button', { type: 'button', text: cancelLabel });
  cancel.addEventListener('click', close);
  const buttons = actions.map((a) => {
    const b = el('button', { type: 'button', text: a.label, class: a.primary ? 'primary' : (a.danger ? 'danger' : '') });
    if (a.armMs) {
      b.disabled = true;
      setTimeout(() => { b.disabled = false; }, a.armMs);
    }
    b.addEventListener('click', () => {
      if (b.disabled) return;
      for (const x of buttons) x.disabled = true; // one tap, one action
      close();
      a.run();
    });
    return b;
  });
  dlg.append(el('h2', { id: 'sheet-title', text: title }), ...body,
    el('div', { class: 'sheet-actions sheet-stack' }, ...buttons, cancel));
  dlg.addEventListener('close', () => { dlg.remove(); restoreFocus?.(); });
  document.body.append(dlg);
  dlg.showModal();
  cancel.focus();
  return dlg;
}
