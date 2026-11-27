// Optional Asks view (open first, oldest first). Safe DOM only (never innerHTML).
import * as api from './api.js';
import { el, btn } from './dom.js';
import { openSheet } from './sheet.js';
import { relativeAge } from './inbox_model.js';
import { sortAsks, openAskCount, safeHttpURL } from './asks_model.js';

export function createAsksView({ root, toast }) {
  function closeAskDialog(ask, status) {
    const verb = status === 'dropped' ? 'Drop' : 'Mark done';
    const input = el('textarea', {
      id: 'ask-close-reason',
      class: 'reply-text',
      rows: '2',
      maxlength: '4000',
      placeholder: 'Reason (required)',
    });
    const err = el('p', { class: 'error', role: 'alert', hidden: true });
    openSheet({
      title: `${verb} ${ask.id}`,
      body: [
        el('p', { class: 'muted', text: `${ask.person}: ${ask.what}` }),
        el('label', { for: 'ask-close-reason', text: 'Reason' }),
        input,
        err,
      ],
      actions: [{
        label: verb,
        primary: status === 'done',
        danger: status === 'dropped',
        run: async () => {
          const reason = input.value.trim();
          if (!reason) { toast('Reason is required.'); return; }
          try {
            if (status === 'dropped') await api.askDrop(ask.id, reason);
            else await api.askDone(ask.id, reason);
            toast(`${ask.id} ${status}`);
          } catch (e) {
            toast(e?.data?.error ?? `Could not update ${ask.id}.`);
          }
        },
      }],
    });
    input.focus();
  }

  function askRow(a, now) {
    const statusCls = a.status === 'open' ? 'warn' : (a.status === 'done' ? 'ok' : '');
    const metaParts = [];
    if (a.owner) metaParts.push(a.owner);
    if (a.createdAt) metaParts.push(relativeAge(a.createdAt, now));
    const head = el('div', { class: 'ask-head' },
      el('span', { class: 'mono id', text: a.id }),
      el('span', { class: `pill ${statusCls}`.trim(), text: a.status }),
      a.itemId ? el('a', { class: 'mono', href: '#/item/' + encodeURIComponent(a.itemId), text: a.itemId }) : '',
      ...(metaParts.length ? [el('span', { class: 'muted', text: metaParts.join(' · ') })] : []),
    );
    const what = el('p', { class: 'ask-what', text: `${a.person}: ${a.what}` });
    const kids = [head, what];

    if (a.links?.length) {
      const linkBar = el('div', { class: 'ask-links' });
      for (const raw of a.links) {
        const href = safeHttpURL(raw);
        linkBar.append(href
          ? el('a', { href, target: '_blank', rel: 'noopener noreferrer', text: raw })
          : el('span', { class: 'mono muted', text: raw }));
      }
      kids.push(linkBar);
    }
    if (a.closeReason) {
      kids.push(el('p', { class: 'muted', text: `${a.status} by ${a.closedBy || 'human'}: ${a.closeReason}` }));
    }
    if (a.status === 'open') {
      kids.push(el('div', { class: 'row-btns' },
        btn('Done', () => closeAskDialog(a, 'done'), 'primary'),
        btn('Drop', () => closeAskDialog(a, 'dropped'), 'danger'),
      ));
    }
    return el('li', { class: 'ask-row', 'data-ask-id': a.id }, ...kids);
  }

  return {
    render(state) {
      if (!state.asksEnabled) {
        root.replaceChildren(
          el('h2', { text: 'Asks' }),
          el('p', { class: 'muted', text: 'Asks view is disabled. Set asks.enabled = true in config.json to enable it.' }),
        );
        return;
      }
      const list = sortAsks(state.asks);
      const openN = openAskCount(list);
      const kids = [
        el('div', { class: 'group-head' },
          el('h2', { text: `Asks (${openN} open)` })),
      ];
      if (!list.length) {
        kids.push(el('p', { class: 'empty', text: 'No asks recorded yet.' }));
      } else {
        const now = Date.now();
        kids.push(el('ul', { class: 'asks-list' }, ...list.map((a) => askRow(a, now))));
      }
      root.replaceChildren(...kids);
    },
    leave() {},
  };
}
