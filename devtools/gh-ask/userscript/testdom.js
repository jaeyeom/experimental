// A tiny HTML parser for the test fixtures. It builds just enough of a
// DOM for gh-ask.user.js: tagName, getAttribute, parentElement,
// children, childNodes and textContent. It handles the well-formed
// markup in testdata/ and nothing more; it is not a general parser.
const VOID = new Set(["br", "col", "hr", "img", "input", "meta", "link"]);

const ENTITIES = { amp: "&", lt: "<", gt: ">", quot: '"', nbsp: " " };

function decode(s) {
  return s.replace(/&(#\d+|amp|lt|gt|quot|nbsp);/g, (_, e) =>
    e.startsWith("#") ? String.fromCodePoint(Number(e.slice(1))) : ENTITIES[e],
  );
}

class Text {
  constructor(data, parent) {
    this.nodeType = 3;
    this.data = data;
    this.parentElement = parent;
  }
  get textContent() {
    return this.data;
  }
}

class Element {
  constructor(tag, attrs, parent) {
    this.nodeType = 1;
    this.tagName = tag.toUpperCase();
    this.attrs = attrs;
    this.parentElement = parent;
    this.childNodes = [];
  }
  get children() {
    return this.childNodes.filter((n) => n.nodeType === 1);
  }
  getAttribute(name) {
    return Object.hasOwn(this.attrs, name) ? this.attrs[name] : null;
  }
  get textContent() {
    return this.childNodes.map((n) => n.textContent).join("");
  }
}

const TOKEN =
  /<!--[\s\S]*?-->|<\/([a-zA-Z0-9]+)\s*>|<([a-zA-Z0-9]+)((?:\s+[^\s=/>]+(?:\s*=\s*(?:"[^"]*"|'[^']*'|[^\s>]+))?)*)\s*(\/?)>|[^<]+/g;
const ATTR = /([^\s=/>]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+)))?/g;

function parse(html) {
  const root = new Element("#root", {}, null);
  let cur = root;
  for (const m of html.matchAll(TOKEN)) {
    const [tok, close, open, attrText, selfClose] = m;
    if (tok.startsWith("<!--")) {
      continue;
    }
    if (close) {
      if (cur.tagName !== close.toUpperCase()) {
        throw new Error(`testdom: </${close}> closes <${cur.tagName}>`);
      }
      cur = cur.parentElement;
      continue;
    }
    if (open) {
      const attrs = {};
      for (const a of attrText.matchAll(ATTR)) {
        attrs[a[1]] = decode(a[2] ?? a[3] ?? a[4] ?? "");
      }
      const el = new Element(open, attrs, cur);
      cur.childNodes.push(el);
      if (!selfClose && !VOID.has(open.toLowerCase())) {
        cur = el;
      }
      continue;
    }
    cur.childNodes.push(new Text(decode(tok), cur));
  }
  if (cur !== root) {
    throw new Error(`testdom: <${cur.tagName}> is not closed`);
  }
  return root;
}

// byAttr returns the first element, in document order, whose attribute
// name is value.
function byAttr(root, name, value) {
  const queue = [root];
  while (queue.length > 0) {
    const el = queue.shift();
    if (el.getAttribute(name) === value) {
      return el;
    }
    queue.unshift(...el.children);
  }
  throw new Error(`testdom: no element with ${name}=${value}`);
}

module.exports = { parse, byAttr };
