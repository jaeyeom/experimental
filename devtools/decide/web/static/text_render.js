// Pure text rendering model: splits message text into text and code blocks
// and text into plain and link segments. No DOM here; the view turns the
// segments into text nodes and anchors. Only http and https links exist.

// safeHref returns the normalised URL when raw is an absolute http(s) URL,
// else null. Everything else (javascript:, data:, relative, ...) is refused.
export function safeHref(raw) {
  if (typeof raw !== 'string' || raw === '' || raw !== raw.trim()) return null;
  let u;
  try { u = new URL(raw); } catch { return null; }
  if (u.protocol !== 'http:' && u.protocol !== 'https:') return null;
  if (!u.hostname) return null;
  return u.href;
}

const URL_RE = /https?:\/\/[^\s<>"'`]+/gi;

// trimURL drops trailing punctuation and unbalanced closing brackets.
function trimURL(s) {
  const pairs = { ')': '(', ']': '[', '}': '{' };
  for (;;) {
    const last = s[s.length - 1];
    if (/[.,;:!?]/.test(last)) { s = s.slice(0, -1); continue; }
    const open = pairs[last];
    if (open) {
      const opens = s.split(open).length - 1;
      const closes = s.split(last).length - 1;
      if (closes > opens) { s = s.slice(0, -1); continue; }
    }
    return s;
  }
}

// linkify splits one text into [{type:'text'|'link', text, href?}].
// Concatenating every segment's text gives back the input exactly.
export function linkify(s) {
  const out = [];
  let pos = 0;
  const pushText = (t) => {
    if (!t) return;
    const prev = out[out.length - 1];
    if (prev?.type === 'text') prev.text += t; else out.push({ type: 'text', text: t });
  };
  for (const m of s.matchAll(URL_RE)) {
    const url = trimURL(m[0]);
    const href = safeHref(url);
    pushText(s.slice(pos, m.index));
    if (href) out.push({ type: 'link', text: url, href }); else pushText(url);
    pos = m.index + url.length;
  }
  pushText(s.slice(pos));
  return out;
}

const FENCE = /^ {0,3}```\s*([^\s`]*)\s*$/;
const indented = (l) => /^(?: {4}|\t)\s*\S/.test(l);

const dedent = (l) => l.replace(/^(?: {4}|\t)/, '');

// parseBlocks splits text into {type:'text', text} and
// {type:'code', text, lang} blocks. Fenced blocks end at the closing fence
// or the end of the text. An indented block needs a blank line (or the
// start of the text) before it, so an indented continuation line inside a
// paragraph stays text.
export function parseBlocks(text) {
  const lines = String(text ?? '').replace(/\r\n?/g, '\n').split('\n');
  const blocks = [];
  let para = [];
  const flush = () => {
    const t = para.join('\n').replace(/^\n+|\n+$/g, '');
    if (t) blocks.push({ type: 'text', text: t });
    para = [];
  };
  let i = 0;
  while (i < lines.length) {
    const fence = FENCE.exec(lines[i]);
    if (fence) {
      flush();
      const body = [];
      i++;
      while (i < lines.length && !/^ {0,3}```\s*$/.test(lines[i])) body.push(lines[i++]);
      i++; // closing fence, if any
      blocks.push({ type: 'code', text: body.join('\n'), lang: fence[1] });
      continue;
    }
    const startOfBlock = para.length === 0 || para[para.length - 1] === '';
    if (startOfBlock && indented(lines[i])) {
      flush();
      const body = [];
      let j = i;
      while (j < lines.length) {
        if (indented(lines[j])) { body.push(dedent(lines[j])); j++; continue; }
        // blank lines belong to the block only when another indented line follows
        let k = j;
        while (k < lines.length && lines[k].trim() === '') k++;
        if (k > j && k < lines.length && indented(lines[k])) {
          for (; j < k; j++) body.push('');
          continue;
        }
        break;
      }
      blocks.push({ type: 'code', text: body.join('\n'), lang: '' });
      i = j;
      continue;
    }
    para.push(lines[i]);
    i++;
  }
  flush();
  return blocks;
}
