// Keyboard shortcuts overlay (? key). Built directly from the server's
// keymap registry via keymap.helpSections(), so there is no second list.
import { el } from './dom.js';

let openDlg = null;

export function isHelpOpen() {
  return !!openDlg && openDlg.open;
}

export function closeHelp() {
  if (openDlg?.open) openDlg.close();
}

export function toggleHelp(keymap) {
  if (isHelpOpen()) {
    closeHelp();
    return;
  }
  const sections = keymap.helpSections();
  const dlg = el('dialog', { class: 'sheet help-sheet', 'aria-labelledby': 'help-title' });
  const close = () => dlg.close();
  const closeBtn = el('button', { type: 'button', text: 'Close' });
  closeBtn.addEventListener('click', close);

  const body = el('div', { class: 'help-body' });
  for (const sec of sections) {
    const tbody = el('tbody', {}, ...sec.rows.map((r) => el('tr', { class: r.status === 'unavailable' ? 'dim' : '' },
      el('th', { scope: 'row', class: 'help-key' },
        r.status === 'available'
          ? el('kbd', { class: 'mono', text: r.displayKeys })
          : el('span', { class: 'muted', text: r.displayKeys })),
      el('td', { text: r.description }))));
    body.append(el('section', { class: 'help-sec' },
      el('h3', { text: sec.title }),
      el('table', { class: 'help-table' }, tbody)));
  }

  dlg.append(
    el('div', { class: 'help-head' }, el('h2', { id: 'help-title', text: 'Keyboard shortcuts' }), closeBtn),
    body,
  );
  dlg.addEventListener('close', () => {
    if (openDlg === dlg) openDlg = null;
    dlg.remove();
  });
  openDlg = dlg;
  document.body.append(dlg);
  dlg.showModal();
  closeBtn.focus();
}
