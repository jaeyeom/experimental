// Reply draft sync: every keystroke is buffered locally, the server copy
// is saved 1.5s after typing stops (or on flush), and conflicts reported by
// the server are resolved like the TUI does. Pure: the network, storage and
// timers are injected, so it runs under node for tests.
import { mergeMarkers } from './thread_model.js';

const blank = (t) => String(t ?? '').trim() === '';

// put(key, draft, baseRev) resolves to {draft, draftConflict} and rejects
// with an error carrying {status:409, data} on a conflict.
// remove(key, conflictId?) resolves to {draft?}, the draft left behind.
export function createDraftSync({ key, itemId, put, remove, saveLocal, setTimer, clearTimer, onStatus = () => {}, delay = 1500 }) {
  let baseRev = 0;
  let text = '';
  let dirty = false;
  let status = 'idle'; // idle | saved | saving | error | conflict
  let conflicts = [];
  let serverText = '';
  let timer = 0;
  let chain = Promise.resolve();

  const set = (s) => { status = s; onStatus(s); };
  const cancel = () => { if (timer) { clearTimer(timer); timer = 0; } };
  const take = (d) => {
    if (!d) { baseRev = 0; conflicts = []; serverText = ''; return; }
    baseRev = d.rev ?? 0;
    conflicts = d.conflicts ?? [];
    serverText = d.text ?? '';
  };
  // Saves are serialised so baseRev is always the revision of the last one.
  const queue = (fn) => { chain = chain.then(fn, fn); return chain; };

  async function save() {
    cancel();
    if (!dirty) return;
    const snapshot = text;
    if (blank(snapshot)) {
      try {
        await remove(key);
        take(null);
        if (text === snapshot) dirty = false;
        set('idle');
      } catch { set('error'); }
      return;
    }
    set('saving');
    try {
      const r = await put(key, { key, kind: 'reply', itemId, text: snapshot }, baseRev);
      take(r.draft);
      if (text === snapshot) dirty = false;
      set(conflicts.length ? 'conflict' : 'saved');
    } catch (e) {
      if (e?.status === 409 && e.data?.draftConflict) {
        take(e.data.draft);
        if (text === snapshot) dirty = false;
        set('conflict');
      } else {
        set('error'); // text stays buffered and dirty; retry() resends
      }
    }
  }

  return {
    state: () => ({ baseRev, status, dirty, conflicts, serverText, text }),
    // adopt takes the draft the server already holds for this key.
    adopt(d) { take(d); text = d?.text ?? ''; dirty = false; set(conflicts.length ? 'conflict' : 'idle'); },
    edit(t) {
      text = t;
      dirty = true;
      saveLocal(key, t);
      cancel();
      timer = setTimer(() => { timer = 0; queue(save); }, delay);
    },
    flush: () => queue(save),
    retry: () => queue(save),
    async discard() {
      cancel();
      text = '';
      dirty = false;
      await saveLocal(key, '');
      await queue(async () => {
        try { await remove(key); take(null); set('idle'); } catch { set('error'); }
      });
    },
    // resolve applies keep | other | merge to the oldest conflict and
    // returns the text the reply box should now hold.
    resolve: (action, current) => queue(async () => {
      cancel();
      const c = conflicts[0];
      if (!c) return current;
      const was = serverText;
      const left = await remove(key, c.id);
      take(left?.draft ?? { rev: baseRev, text: was, conflicts: conflicts.slice(1) });
      let next = current;
      if (action === 'other') next = c.text;
      else if (action === 'merge') {
        next = mergeMarkers(blank(current) ? was : current, c.text, c);
      }
      if (next !== serverText && !blank(next)) {
        const r = await put(key, { key, kind: 'reply', itemId, text: next }, baseRev);
        take(r.draft);
      }
      text = next;
      dirty = false;
      set(conflicts.length ? 'conflict' : 'saved');
      return next;
    }),
  };
}
