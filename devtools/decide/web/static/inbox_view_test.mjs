import test from 'node:test';
import assert from 'node:assert/strict';
import * as api from './api.js';
import * as inboxModel from './inbox_model.js';

class FakeEl {
  constructor(tag) {
    this.tagName = tag.toUpperCase();
    this.children = [];
    this.attrs = {};
    this.dataset = {};
    this.className = '';
    this.textContent = '';
    this.value = '';
  }
  setAttribute(k, v) {
    this.attrs[k] = String(v);
    if (k === 'class') this.className = String(v);
    if (k.startsWith('data-')) {
      const key = k.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase());
      this.dataset[key] = String(v);
    }
  }
  addEventListener() {}
  contains(node) {
    if (node === this) return true;
    return this.children.some((c) => c.contains?.(node));
  }
  append(...nodes) {
    for (const n of nodes) {
      if (n === '' || n == null) continue;
      this.children.push(typeof n === 'string' ? { textContent: n, children: [], className: '', tagName: '#TEXT' } : n);
    }
  }
  replaceChildren(...nodes) {
    this.children = [];
    this.append(...nodes);
  }
  walk(fn) {
    fn(this);
    for (const c of this.children) if (c.walk) c.walk(fn);
  }
  querySelectorAll(sel) {
    const out = [];
    this.walk((n) => {
      if (sel === 'button' && n.tagName === 'BUTTON') out.push(n);
      if (sel === '.group-head' && n.className?.split(/\s+/).includes('group-head')) out.push(n);
    });
    return out;
  }
  querySelector() { return null; }
  allText() {
    const parts = [];
    this.walk((n) => { if (n.textContent) parts.push(n.textContent); });
    return parts.join(' ');
  }
}

test('web inbox renders no archive-all control and removes bulk archive helpers', async () => {
  assert.equal('archiveAllNotes' in api, false);
  assert.equal('noteSeen' in inboxModel, false);

  const prevDoc = globalThis.document;
  globalThis.document = {
    createElement: (tag) => new FakeEl(tag),
    activeElement: null,
  };
  try {
    const { createInboxView } = await import('./inbox_view.js');
    const root = new FakeEl('div');
    const view = createInboxView({ root, toast: () => {} });
    const now = new Date().toISOString();
    view.render({
      items: [
        { id: 'N-001', class: 'note', question: 'Note one', owner: 'ada', state: 'open', createdAt: now },
        { id: 'N-002', class: 'note', question: 'Note two', owner: 'ada', state: 'open', createdAt: now },
        { id: 'D-001', class: 'decision', question: 'Pick one?', owner: 'ada', state: 'open', createdAt: now },
      ],
      drafts: [],
    }, { name: 'inbox', arg: '' });

    const heads = root.querySelectorAll('.group-head');
    assert.equal(heads.length, 2);
    for (const head of heads) {
      assert.equal(head.querySelectorAll('button').length, 0, 'group header must not contain buttons');
    }
    const buttons = root.querySelectorAll('button');
    assert.equal(buttons.some((b) => /archive all/i.test(b.allText())), false);
    assert.equal(/archive all notes/i.test(root.allText()), false);
  } finally {
    globalThis.document = prevDoc;
  }
});
