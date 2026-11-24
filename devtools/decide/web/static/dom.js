// Tiny DOM builder. Text always goes through textContent, never HTML.
export const el = (tag, attrs = {}, ...kids) => {
  const n = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === 'text') n.textContent = v;
    else if (v === true) n.setAttribute(k, '');
    else if (v !== false && v != null) n.setAttribute(k, v);
  }
  n.append(...kids);
  return n;
};

// btn builds a type=button button with a click handler.
export const btn = (text, fn, cls = '', attrs = {}) => {
  const b = el('button', { type: 'button', text, class: cls, ...attrs });
  b.addEventListener('click', fn);
  return b;
};

// keyHint builds a desktop-only key hint badge, or '' when unbound.
export const keyHint = (text) => (text ? el('kbd', { class: 'key-hint', text, 'aria-hidden': 'true' }) : '');
