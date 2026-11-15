// ==UserScript==
// @name         gh-ask
// @namespace    https://github.com/jaeyeom/experimental/tree/main/devtools/gh-ask
// @version      0.1.10
// @description  Ask a local agent about lines in a pull request diff, through gh-ask serve on 127.0.0.1.
// @match        https://github.com/*
// @grant        GM_xmlhttpRequest
// @grant        GM_getValue
// @grant        GM_setValue
// @grant        GM_registerMenuCommand
// @grant        GM_addStyle
// @connect      127.0.0.1
// @connect      localhost
// @run-at       document-idle
// @noframes
// ==/UserScript==

// The first half of this file is pure: it reads a DOM-like tree through
// tagName, getAttribute, parentElement, children and textContent only,
// so gh-ask.test.js runs it in node against testdata/ fixtures. The
// browser glue (registerMenu, activatePanel, main, and boot) is the only code that uses
// document, window, or GM_* functions. bindPage takes those as arguments
// so tests can drive navigation without a browser. Agent requests go
// only to http://127.0.0.1:<port>. A same-origin fetch of the pull
// request's files page is the one exception, and it sends no token.

// Limits from devtools/gh-ask/message.go.
const MAX_SELECTION_BYTES = 4 << 10;
const MAX_QUESTION_BYTES = 2 << 10;
const MAX_PATH_BYTES = 1 << 10;
const MAX_PAGE_URL_BYTES = 2 << 10;

const POLL_INTERVAL_MS = 20_000;
const POLL_MAX_MS = 30 * 60_000;
const DEFAULT_PORT = 8765;

// The question popover's CSS width, and the gap kept inside the viewport.
const POPOVER_WIDTH = 360;
const VIEWPORT_MARGIN = 8;

const SHA_RE = /^[0-9a-fA-F]{7,64}$/;
const PR_URL_RE =
  /^https:\/\/github\.com\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)\/pull\/(\d+)\/(files|changes)(\/[0-9a-fA-F.]+)?\/?(?:[?#]|$)/;
const URL_HEAD_RE =
  /\/pull\/\d+\/(?:files|changes)\/(?:[0-9a-fA-F]{7,64}\.\.\.?)?([0-9a-fA-F]{7,64})(?:[/?#]|$)/;
// A line anchor: diff-<hash of path><L|R><line>.
const LINE_ANCHOR_RE = /^(diff-[0-9a-f]+)([LR])(\d+)$/;
const FILE_ANCHOR_RE = /^diff-[0-9a-f]+$/;
const HEAD_OID_RE = /"headOid":"([0-9a-fA-F]{40,64})"/;
// Class tokens of line-number cells: blob-num, blob-num-addition,
// new-diff-line-number, ...
const NUM_CLASS_RE = /(^|-)(num|number)(-|$)/;
const CONTROL_RE = /\p{Cc}/u;

function classes(el) {
  return (el.getAttribute("class") || "").split(/\s+/).filter(Boolean);
}

function closestTag(el, tag) {
  for (let e = el; e; e = e.parentElement) {
    if (e.tagName === tag) {
      return e;
    }
  }
  return null;
}

// findAll returns the descendants of root, in document order, that
// match pred.
function findAll(root, pred) {
  const out = [];
  const stack = [root];
  while (stack.length > 0) {
    const el = stack.pop();
    if (pred(el)) {
      out.push(el);
    }
    const kids = el.children;
    for (let i = kids.length - 1; i >= 0; i--) {
      stack.push(kids[i]);
    }
  }
  return out;
}

function utf8Len(s) {
  let n = 0;
  for (const ch of s) {
    const cp = ch.codePointAt(0);
    n += cp < 0x80 ? 1 : cp < 0x800 ? 2 : cp < 0x10000 ? 3 : 4;
  }
  return n;
}

// truncateUtf8 cuts s to at most max UTF-8 bytes without splitting a
// character.
function truncateUtf8(s, max) {
  let n = 0;
  let out = "";
  for (const ch of s) {
    const cp = ch.codePointAt(0);
    n += cp < 0x80 ? 1 : cp < 0x800 ? 2 : cp < 0x10000 ? 3 : 4;
    if (n > max) {
      break;
    }
    out += ch;
  }
  return out;
}

// parsePrUrl returns {repo, pr, page} for a pull request files or
// changes URL, where page is the URL without query or fragment.
function parsePrUrl(href) {
  const m = PR_URL_RE.exec(href);
  if (!m) {
    return null;
  }
  const page = `https://github.com/${m[1]}/${m[2]}/pull/${m[3]}/${m[4]}${m[5] || ""}`;
  return { repo: `${m[1]}/${m[2]}`, pr: Number(m[3]), page };
}

// findHeadSha returns the commit the diff shows, or "". A commit or
// range in the URL wins; then the PR layout JSON, which stays on the
// page across a soft navigation; then the hidden form inputs of the
// classic view; then headOid in embedded JSON.
function findHeadSha(root, href) {
  const m = URL_HEAD_RE.exec(href);
  if (m) {
    return m[1];
  }
  const layout = layoutHeadSha(root);
  if (layout) {
    return layout;
  }
  for (const name of ["head_sha", "comparison_end_oid"]) {
    for (const el of findAll(
      root,
      (e) => e.tagName === "INPUT" && e.getAttribute("name") === name,
    )) {
      const v = el.getAttribute("value") || "";
      if (SHA_RE.test(v)) {
        return v;
      }
    }
  }
  for (const el of findAll(
    root,
    (e) => e.tagName === "SCRIPT" && e.getAttribute("type") === "application/json",
  )) {
    const h = HEAD_OID_RE.exec(el.textContent);
    if (h) {
      return h[1];
    }
  }
  return "";
}

// layoutHeadSha reads pullRequestsLayoutRoute.pullRequest.headSha from
// the React app's embedded JSON. A parse error, a missing path, or a
// value that is not a commit is skipped so a later source can match.
function layoutHeadSha(root) {
  for (const el of findAll(
    root,
    (e) =>
      e.tagName === "SCRIPT" &&
      e.getAttribute("type") === "application/json" &&
      e.getAttribute("data-target") === "react-app.embeddedData",
  )) {
    const sha = layoutSha(el.textContent);
    if (sha) {
      return sha;
    }
  }
  return "";
}

function layoutSha(text) {
  let data;
  try {
    data = JSON.parse(text);
  } catch {
    return "";
  }
  const sha = data?.payload?.pullRequestsLayoutRoute?.pullRequest?.headSha;
  if (typeof sha === "string" && SHA_RE.test(sha)) {
    return sha;
  }
  return "";
}

function explicitSide(cell) {
  const v = cell.getAttribute("data-split-side") || cell.getAttribute("data-diff-side") || "";
  return v === "left" ? "LEFT" : v === "right" ? "RIGHT" : "";
}

// numCell parses a line-number cell, or returns null.
function numCell(cell) {
  const raw = cell.getAttribute("data-line-number");
  if (raw === null || !/^\d+$/.test(raw)) {
    return null;
  }
  for (const attr of ["id", "data-line-anchor"]) {
    const m = LINE_ANCHOR_RE.exec(cell.getAttribute(attr) || "");
    if (m) {
      return { cell, side: m[2] === "L" ? "LEFT" : "RIGHT", line: Number(m[3]), anchor: m[1] };
    }
  }
  const side = explicitSide(cell);
  return side ? { cell, side, line: Number(raw), anchor: "" } : null;
}

function isNumberish(cell) {
  return (
    cell.getAttribute("data-line-number") !== null ||
    classes(cell).some((c) => NUM_CLASS_RE.test(c))
  );
}

// codeText is the code in a cell, without the React view's marker span.
function codeText(el) {
  if (el.nodeType === 3) {
    return el.textContent;
  }
  if (classes(el).includes("diff-text-marker")) {
    return "";
  }
  let s = "";
  for (const n of el.childNodes) {
    s += codeText(n);
  }
  return s;
}

function cellKind(cell) {
  for (const el of findAll(cell, () => true)) {
    for (const c of classes(el)) {
      if (/(^|-)deletion$/.test(c)) {
        return "-";
      }
      if (/(^|-)addition$/.test(c)) {
        return "+";
      }
    }
  }
  return " ";
}

// parseRow splits a diff row into its number cells and code cells.
function parseRow(row) {
  const nums = [];
  const codes = [];
  for (const cell of row.children) {
    if (cell.tagName !== "TD") {
      continue;
    }
    if (isNumberish(cell)) {
      const n = numCell(cell);
      if (n) {
        nums.push(n);
      }
    } else {
      codes.push(cell);
    }
  }
  return { nums, codes };
}

function isHunkRow(row) {
  const { nums } = parseRow(row);
  return nums.length === 0 && row.textContent.trim().startsWith("@@");
}

// rowText renders a row as unified diff lines.
function rowText(row) {
  const { nums, codes } = parseRow(row);
  if (nums.length === 0) {
    return [row.textContent.trim()];
  }
  const has = (side) => nums.some((n) => n.side === side);
  if (codes.length === 1) {
    const mark = has("LEFT") && has("RIGHT") ? " " : has("LEFT") ? "-" : "+";
    return [mark + codeText(codes[0])];
  }
  // Split view: one code cell per side; context shows once, on the right.
  const out = [];
  for (const cell of codes) {
    const side = explicitSide(cell);
    const kind = cellKind(cell);
    const empty = !nums.some((n) => n.side === side);
    if (empty || (kind === " " && side === "LEFT")) {
      continue;
    }
    out.push(kind + codeText(cell));
  }
  return out;
}

function tableRows(table) {
  return findAll(table, (e) => e.tagName === "TR");
}

// hunkText is the hunk around row as unified diff text.
function hunkText(row, table) {
  const rows = tableRows(table);
  let lo = rows.indexOf(row);
  while (lo > 0 && !isHunkRow(rows[lo])) {
    lo--;
  }
  let hi = rows.indexOf(row) + 1;
  while (hi < rows.length && !isHunkRow(rows[hi])) {
    hi++;
  }
  return rows.slice(lo, hi).flatMap(rowText).join("\n");
}

function filePath(el) {
  for (let e = el; e; e = e.parentElement) {
    for (const attr of ["data-tagsearch-path", "data-file-path", "data-path"]) {
      const v = e.getAttribute(attr);
      if (v) {
        return v;
      }
    }
    const id = e.getAttribute("id") || "";
    if (FILE_ANCHOR_RE.test(id)) {
      const link = findAll(e, (a) => a.tagName === "A" && a.getAttribute("href") === `#${id}`)[0];
      if (link) {
        return link.textContent.replace(/[\u200e\u200f]/g, "").trim();
      }
    }
  }
  return "";
}

function fileAnchor(el) {
  for (let e = el; e; e = e.parentElement) {
    for (const attr of ["data-diff-anchor", "data-anchor", "id"]) {
      const v = e.getAttribute(attr) || "";
      if (FILE_ANCHOR_RE.test(v)) {
        return v;
      }
    }
  }
  return "";
}

// lineInfo describes the diff line el is in: {path, side, line, anchor,
// row, table}. It returns null outside a numbered diff line.
function lineInfo(el) {
  const cell = closestTag(el, "TD");
  const row = cell && closestTag(cell, "TR");
  const table = row && closestTag(row, "TABLE");
  if (!table) {
    return null;
  }
  const { nums } = parseRow(row);
  let num = nums.find((n) => n.cell === cell);
  if (!num) {
    if (isNumberish(cell)) {
      return null;
    }
    const want = explicitSide(cell) || (nums.some((n) => n.side === "RIGHT") ? "RIGHT" : "LEFT");
    num = nums.find((n) => n.side === want);
  }
  if (!num) {
    return null;
  }
  const path = filePath(table);
  if (!path) {
    return null;
  }
  return {
    path,
    side: num.side,
    line: num.line,
    anchor: num.anchor || fileAnchor(table),
    row,
    table,
  };
}

// buildPayload turns a hovered line (start === end) or a selection into
// the POST /ask body. It returns {payload} or {error}.
function buildPayload({ href, headSha, start, end, selection, question, deep }) {
  const url = parsePrUrl(href);
  if (!url) {
    return { error: "not a pull request files page" };
  }
  if (!SHA_RE.test(headSha || "")) {
    return { error: "could not find the head commit on this page" };
  }
  const a = lineInfo(start);
  const b = lineInfo(end);
  if (!a || !b) {
    return { error: "select a numbered diff line" };
  }
  if (a.table !== b.table) {
    return { error: "select lines in one file" };
  }
  if (utf8Len(b.path) > MAX_PATH_BYTES || CONTROL_RE.test(b.path)) {
    return { error: "file path is too long or has control characters" };
  }
  if (!question || question.trim() === "") {
    return { error: "the question is empty" };
  }
  if (utf8Len(question) > MAX_QUESTION_BYTES) {
    return { error: "the question is over 2 KB" };
  }
  const rows = tableRows(b.table);
  const ia = rows.indexOf(a.row);
  const ib = rows.indexOf(b.row);
  const side = b.side;
  const lines = rows
    .slice(Math.min(ia, ib), Math.max(ia, ib) + 1)
    .flatMap((r) => parseRow(r).nums)
    .filter((n) => n.side === side)
    .map((n) => n.line);
  const startLine = Math.min(...lines);
  const endLine = Math.max(...lines);
  const text = selection && selection.trim() !== "" ? selection : hunkText(b.row, b.table);
  let pageUrl = url.page;
  if (b.anchor) {
    const s = side[0];
    pageUrl += `#${b.anchor}${s}${startLine}${endLine === startLine ? "" : `-${s}${endLine}`}`;
  }
  if (utf8Len(pageUrl) > MAX_PAGE_URL_BYTES) {
    pageUrl = url.page;
  }
  return {
    payload: {
      repo: url.repo,
      pr: url.pr,
      head_sha: headSha,
      path: b.path,
      side,
      start_line: startLine,
      end_line: endLine,
      selection: truncateUtf8(text, MAX_SELECTION_BYTES),
      question,
      deep: Boolean(deep),
      page_url: pageUrl,
    },
  };
}

// prHeader is the pull request title, where the whole-PR button sits.
// The classic title comes first. Otherwise it is an h1 whose parent or
// grandparent has data-component="TitleArea".
function prHeader(root) {
  const found = findAll(
    root,
    (e) => classes(e).includes("js-issue-title") || e.getAttribute("data-testid") === "issue-title",
  );
  if (found[0]) {
    return found[0];
  }
  const titles = findAll(root, (e) => e.tagName === "H1" && inTitleArea(e));
  return titles[0] || null;
}

function classIncludes(el, part) {
  const value = el.getAttribute ? el.getAttribute("class") : "";
  return typeof value === "string" && value.includes(part);
}

function indexOfNode(list, node) {
  return Array.prototype.indexOf.call(list || [], node);
}

// panelToggleSlot is where the panel toggle goes. The sticky header
// wins: after the title copy control, or at the start of the actions
// when that control is missing. Otherwise it sits next to the title.
function panelToggleSlot(root) {
  const bar = findAll(root, (e) => classIncludes(e, "miniHeaderInner"))[0];
  if (bar) {
    const meta = findAll(bar, (e) => classIncludes(e, "metadata"))[0];
    const title = meta && findAll(meta, (e) => classIncludes(e, "titleWithCopy"))[0];
    if (title && title.parentElement) {
      return { parent: title.parentElement, after: title, first: false };
    }
    const actions = findAll(bar, (e) => classIncludes(e, "actions"))[0];
    if (actions) {
      return { parent: actions, after: null, first: true };
    }
  }
  const heading = prHeader(root);
  const host = heading?.parentElement;
  if (!host) {
    return null;
  }
  return { parent: host, after: null, first: false };
}

function toggleInSlot(slot, toggle) {
  const kids = slot.parent.children;
  const at = indexOfNode(kids, toggle);
  if (at < 0) {
    return false;
  }
  if (slot.after) {
    return kids[at - 1] === slot.after;
  }
  if (slot.first) {
    return at === 0;
  }
  return at === kids.length - 1;
}

function insertChild(parent, node, before) {
  if (typeof parent.insertBefore === "function") {
    parent.insertBefore(node, before || null);
    return;
  }
  const kids = parent.childNodes;
  const old = node.parentElement;
  if (old && old.childNodes) {
    const i = indexOfNode(old.childNodes, node);
    if (i >= 0) {
      old.childNodes.splice(i, 1);
    }
  }
  node.parentElement = parent;
  if (!before) {
    kids.push(node);
    return;
  }
  const at = indexOfNode(kids, before);
  kids.splice(at < 0 ? kids.length : at, 0, node);
}

// mountPanelToggle puts toggle in panelToggleSlot. The same node is
// moved when a better slot appears, so the page never has two.
function mountPanelToggle(root, toggle) {
  const slot = panelToggleSlot(root);
  if (!slot) {
    return false;
  }
  if (toggleInSlot(slot, toggle)) {
    return true;
  }
  const before = slot.first ? slot.parent.children[0] || null : null;
  insertChild(slot.parent, toggle, slot.after ? nextElement(slot.after) : before);
  return toggleInSlot(slot, toggle);
}

function nextElement(el) {
  const kids = el.parentElement ? el.parentElement.children : [];
  const at = indexOfNode(kids, el);
  return at >= 0 ? kids[at + 1] || null : null;
}

// inTitleArea reports whether el's parent or grandparent is a TitleArea.
function inTitleArea(el) {
  let e = el.parentElement;
  for (let i = 0; i < 2 && e; i++) {
    if (e.getAttribute("data-component") === "TitleArea") {
      return true;
    }
    e = e.parentElement;
  }
  return false;
}

// serverUrl returns http://127.0.0.1:<port><path>, or null for a bad
// port. It is the only place a request URL is made.
function serverUrl(port, path) {
  const s = String(port ?? "");
  if (!/^\d{1,5}$/.test(s)) {
    return null;
  }
  const n = Number(s);
  if (n < 1 || n > 65535) {
    return null;
  }
  return `http://127.0.0.1:${n}${path}`;
}

function statusPath(id) {
  return `/status/${encodeURIComponent(id)}`;
}

// askError explains a failed request. status 0 means no response;
// -1 means the token or port is not set.
function askError(status, serverMessage) {
  const base =
    {
      [-1]: "set the gh-ask token and port from the userscript menu",
      0: "server down: is gh-ask serve running?",
      400: "bad request",
      401: "auth failed: check the gh-ask token",
      403: "forbidden: repo not in repos, or bad Host/Origin",
      404: "not found: unknown id or no status_command",
      413: "request too large",
      429: "rate limit reached",
      502: "agent command failed",
    }[status] || `HTTP ${status}`;
  return typeof serverMessage === "string" && serverMessage !== ""
    ? `${base} (${truncateUtf8(serverMessage, 200)})`
    : base;
}

function statusLabel(st) {
  if (st.closed) {
    return "closed";
  }
  return st.answered ? "answered" : "waiting";
}

function chipText(id, label) {
  return `Asked in ${id} · ${label}`;
}

function validStatus(body) {
  return (
    body !== null &&
    typeof body === "object" &&
    typeof body.state === "string" &&
    typeof body.answered === "boolean" &&
    typeof body.closed === "boolean"
  );
}

// startPolling calls fetchStatus every intervalMs while isVisible(),
// until the thread is answered or closed, an auth error, or maxMs. It
// reports {label, error?} to onUpdate and returns a stop function.
// fetchStatus(done) calls done({status, body}).
function startPolling(opts) {
  const intervalMs = opts.intervalMs ?? POLL_INTERVAL_MS;
  const maxMs = opts.maxMs ?? POLL_MAX_MS;
  const begin = opts.now();
  let timer = null;
  let stopped = false;
  let label = "waiting";

  function schedule() {
    if (opts.now() + intervalMs - begin > maxMs) {
      stopped = true;
      opts.onUpdate({ label, error: `stopped polling after ${Math.round(maxMs / 60_000)} min` });
      return;
    }
    timer = opts.setTimeout(tick, intervalMs);
  }

  function tick() {
    timer = null;
    if (stopped) {
      return;
    }
    if (!opts.isVisible()) {
      schedule();
      return;
    }
    opts.fetchStatus((res) => {
      if (stopped) {
        return;
      }
      if (res.status === 200 && validStatus(res.body)) {
        label = statusLabel(res.body);
        opts.onUpdate({ label });
        if (label !== "waiting") {
          stopped = true;
          return;
        }
      } else {
        const msg = res.status === 200 ? "bad status reply" : askError(res.status, res.body?.error);
        opts.onUpdate({ label, error: msg });
        if ([-1, 401, 403, 404].includes(res.status)) {
          stopped = true;
          return;
        }
      }
      schedule();
    });
  }

  schedule();
  return () => {
    stopped = true;
    if (timer !== null) {
      opts.clearTimeout(timer);
      timer = null;
    }
  };
}

// onPopoverKey handles Cmd/Ctrl+Enter and Escape on the open popover.
// Plain Enter is left alone so the question box inserts a newline.
// The handled combination stops at the popover, so the page does not
// also submit a review comment.
function onPopoverKey(event, ctx) {
  if (event.key === "Escape") {
    event.preventDefault();
    event.stopPropagation();
    ctx.close();
    return;
  }
  if (event.key !== "Enter" || (!event.ctrlKey && !event.metaKey)) {
    return;
  }
  event.preventDefault();
  event.stopPropagation();
  if (ctx.sending || !ctx.question || String(ctx.question).trim() === "") {
    return;
  }
  ctx.send();
}

function clamp(value, min, max) {
  if (max < min) {
    return min;
  }
  return Math.min(Math.max(value, min), max);
}

// placeInView returns the viewport-relative box for a popover or the Ask
// button. anchor is the desired top-left in viewport coordinates. size is
// the measured box; width is capped at the viewport minus two margins.
// The box opens at anchor.top and, when that would pass the bottom margin,
// flips above the anchor. A point that already fits is left as it is.
function placeInView(anchor, size, viewport) {
  const margin = VIEWPORT_MARGIN;
  const room = Math.max(0, viewport.width - 2 * margin);
  const width = Math.min(Math.max(0, size.width), room);
  const height = Math.max(0, size.height);
  const left = clamp(anchor.left, margin, viewport.width - width - margin);
  let top = anchor.top;
  if (top + height > viewport.height - margin) {
    top = anchor.top - height;
  }
  top = clamp(top, margin, viewport.height - height - margin);
  return { left, top, width };
}

// A pull request page, including Conversation, Commits, Files and Checks.
const PR_PATH_RE = /^\/([^/]+)\/([^/]+)\/pull\/(\d+)(\/|$)/;
const PR_HREF_RE =
  /^https:\/\/github\.com\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)\/pull\/(\d+)(?:[/?#]|$)/;

const PANEL_POLL_YOU_MS = 20_000;
const PANEL_POLL_IDLE_MS = 60_000;
const PANEL_EMPTY = "No question yet";
const PANEL_FOOTER = "Read-only. Nothing here is posted to GitHub.";

// isDiffPath reports a pull request files or changes view. GitHub's
// soft navigation keeps one document, so the script matches every
// github.com page and turns on the diff buttons only for this path.
const DIFF_PATH_RE = /^\/[^/]+\/[^/]+\/pull\/\d+\/(files|changes)(\/|$)/;

function prKey(pathname) {
  const m = PR_PATH_RE.exec(pathname || "");
  if (!m) {
    return null;
  }
  return `${m[1]}/${m[2]}#${m[3]}`;
}

function isPrPath(pathname) {
  return prKey(pathname) !== null;
}

// parsePrPage reads repo and pr from any pull request tab. page has no
// query or fragment.
function parsePrPage(href) {
  const m = PR_HREF_RE.exec(String(href || ""));
  if (!m) {
    return null;
  }
  return {
    repo: `${m[1]}/${m[2]}`,
    pr: Number(m[3]),
    page: String(href).split(/[?#]/)[0],
  };
}

function threadPath(repo, pr) {
  const q = new URLSearchParams();
  q.set("repo", String(repo));
  q.set("pr", String(pr));
  return `/thread?${q}`;
}

function roleLabel(role) {
  return { you: "You", agent: "Agent", note: "Note" }[role] || role || "";
}

function formatMessageTime(iso) {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) {
    return "";
  }
  return d.toLocaleString();
}

function panelMessages(body) {
  if (!body || !Array.isArray(body.messages)) {
    return [];
  }
  return body.messages.filter(
    (m) =>
      m &&
      (m.role === "you" || m.role === "agent" || m.role === "note") &&
      typeof m.time === "string" &&
      typeof m.text === "string",
  );
}

// panelView is what the side panel shows for a GET /thread response and
// the thread's latest status. A miss is the empty state.
function panelView(threadRes, statusBody) {
  const id =
    threadRes && threadRes.status === 200 && threadRes.body && typeof threadRes.body.id === "string"
      ? threadRes.body.id
      : "";
  if (!id) {
    return { empty: true, id: null, closed: false, messages: [] };
  }
  return {
    empty: false,
    id,
    closed: Boolean(statusBody && statusBody.closed),
    messages: panelMessages(statusBody),
  };
}

// renderMessage builds one message from textContent only. The text is
// untrusted and is never parsed as HTML.
function renderMessage(doc, message) {
  const row = doc.createElement("div");
  row.className = "gh-ask-msg";
  const meta = doc.createElement("div");
  meta.className = "gh-ask-msg-meta";
  meta.textContent = `${roleLabel(message.role)} · ${formatMessageTime(message.time)}`;
  const text = doc.createElement("div");
  text.className = "gh-ask-msg-text";
  text.style.whiteSpace = "pre-wrap";
  text.textContent = message.text;
  row.append(meta, text);
  return row;
}

function renderPanel(doc, view) {
  const root = doc.createElement("div");
  if (view.empty) {
    const empty = doc.createElement("p");
    empty.textContent = PANEL_EMPTY;
    root.append(empty);
  } else {
    if (view.closed) {
      const closed = doc.createElement("div");
      closed.textContent = "closed";
      root.append(closed);
    }
    for (const message of view.messages) {
      root.append(renderMessage(doc, message));
    }
  }
  const footer = doc.createElement("p");
  footer.textContent = PANEL_FOOTER;
  root.append(footer);
  return root;
}

// panelPollInterval is 20s while the newest message is from you, and
// 60s when the agent has answered or there is nothing to wait on.
function panelPollInterval(messages) {
  const last = messages && messages.length ? messages[messages.length - 1] : null;
  return last && last.role === "you" ? PANEL_POLL_YOU_MS : PANEL_POLL_IDLE_MS;
}

function messageKey(messages) {
  if (!messages || messages.length === 0) {
    return "";
  }
  const last = messages[messages.length - 1];
  return `${messages.length}\n${last.role}\n${last.time}\n${last.text}`;
}

// startPanelPolling fetches status on the panel cadence until 30 minutes
// pass with no new message. refresh() starts that window over.
// fetchStatus(done) calls done({status, body}).
function startPanelPolling(opts) {
  const maxMs = opts.maxMs ?? POLL_MAX_MS;
  let messages = opts.messages ? opts.messages.slice() : [];
  let closed = Boolean(opts.closed);
  let lastChange = opts.now();
  let timer = null;
  let stopped = false;

  function halt(error) {
    stopped = true;
    if (timer !== null) {
      opts.clearTimeout(timer);
      timer = null;
    }
    opts.onUpdate({ messages, closed, stopped: true, error });
  }

  function schedule() {
    if (stopped) {
      return;
    }
    const elapsed = opts.now() - lastChange;
    const wait = panelPollInterval(messages);
    if (elapsed >= maxMs || elapsed + wait > maxMs) {
      halt(`stopped polling after ${Math.round(maxMs / 60_000)} min`);
      return;
    }
    timer = opts.setTimeout(tick, wait);
  }

  function tick() {
    timer = null;
    if (stopped) {
      return;
    }
    if (!opts.isVisible()) {
      schedule();
      return;
    }
    opts.fetchStatus((res) => {
      if (stopped) {
        return;
      }
      if (res.status === 200 && validStatus(res.body)) {
        const next = panelMessages(res.body);
        if (messageKey(next) !== messageKey(messages)) {
          lastChange = opts.now();
        }
        messages = next;
        closed = Boolean(res.body.closed);
        opts.onUpdate({ messages, closed, stopped: false });
      } else {
        const msg = res.status === 200 ? "bad status reply" : askError(res.status, res.body?.error);
        opts.onUpdate({ messages, closed, stopped: false, error: msg });
        if ([-1, 401, 403, 404].includes(res.status)) {
          stopped = true;
          return;
        }
      }
      schedule();
    });
  }

  schedule();
  return {
    stop() {
      stopped = true;
      if (timer !== null) {
        opts.clearTimeout(timer);
        timer = null;
      }
    },
    refresh() {
      stopped = false;
      lastChange = opts.now();
      if (timer !== null) {
        opts.clearTimeout(timer);
        timer = null;
      }
      schedule();
    },
  };
}

const HEAD_FINDING = "finding the head commit…";
const HEAD_MISSING = "couldn't find the head commit; open Files changed and ask there";

// headCache is the head commit for each repo#pr while this page is up.
// A URL change or a turbo load drops it, so a push is read again.
const headCache = new Map();

function forgetHeadSha() {
  headCache.clear();
}

// headNavDropsCache reports whether this navigation makes a cached head
// commit stale. The first observation has an empty previous URL. A
// turbo:load refreshes the same URL, which is how a push shows up.
function headNavDropsCache(prevHref, nextHref, eventType) {
  if (eventType === "turbo:load") {
    return true;
  }
  return Boolean(prevHref) && Boolean(nextHref) && prevHref !== nextHref;
}

function filesPagePath(page) {
  return `/${page.repo}/pull/${page.pr}/files`;
}

// resolveHeadSha uses the commit on this page. When the page has none,
// it fetches the files tab once and keeps the commit for this pull
// request. fetchText(path) returns HTML or rejects. parseHtml turns
// that HTML into a tree findHeadSha can read.
async function resolveHeadSha(opts) {
  const cache = opts.cache || headCache;
  const page = parsePrPage(opts.href);
  if (!page) {
    return { error: "not a pull request page" };
  }
  const key = `${page.repo}#${page.pr}`;
  const onPage = findHeadSha(opts.root, opts.href);
  if (onPage) {
    cache.set(key, onPage);
    return { headSha: onPage };
  }
  const cached = cache.get(key);
  if (cached) {
    return { headSha: cached };
  }
  if (typeof opts.fetchText !== "function") {
    return { error: HEAD_MISSING };
  }
  if (opts.onStatus) {
    opts.onStatus(HEAD_FINDING);
  }
  let html = "";
  try {
    html = await opts.fetchText(filesPagePath(page));
  } catch {
    return { error: HEAD_MISSING };
  }
  const doc = opts.parseHtml ? opts.parseHtml(String(html ?? "")) : null;
  const sha = doc ? findHeadSha(doc, `https://github.com${filesPagePath(page)}`) : "";
  if (!sha) {
    return { error: HEAD_MISSING };
  }
  cache.set(key, sha);
  return { headSha: sha };
}

// askLine resolves the head commit, then builds a line or selection
// POST /ask. request is the local server. It is not called when the
// head is missing. The question is left for the caller to keep.
async function askLine(opts) {
  const head = await resolveHeadSha(opts);
  if (head.error) {
    return head;
  }
  const built = buildPayload({
    href: opts.href,
    headSha: head.headSha,
    start: opts.start,
    end: opts.end,
    selection: opts.selection || "",
    question: opts.question,
    deep: opts.deep,
  });
  if (built.error || !built.payload) {
    return built;
  }
  if (opts.request) {
    opts.request(built.payload);
  }
  return built;
}

// popoverAskResult is the popover after an ask resolves. The question
// stays. An error is shown and nothing is sent.
function popoverAskResult(question, built) {
  if (!built || built.error || !built.payload) {
    const error = built?.error ? built.error : "could not find the head commit on this page";
    return { question, error, payload: null };
  }
  return { question, error: "", payload: built.payload };
}

// askPanel resolves the head commit, then builds the panel's POST /ask.
// request is the local server. It is not called when the head is missing.
async function askPanel(opts) {
  const head = await resolveHeadSha(opts);
  if (head.error) {
    return head;
  }
  const built = panelAsk({
    href: opts.href,
    headSha: head.headSha,
    question: opts.question,
    deep: opts.deep,
    line: opts.line,
  });
  if (built.error || !built.payload) {
    return built;
  }
  if (opts.request) {
    opts.request(built.payload);
  }
  return built;
}

// panelAsk is the follow-up POST /ask. With a line from a chip it sends
// that line; otherwise it sends the whole pull request.
function panelAsk({ href, headSha, question, deep, line }) {
  const url = parsePrPage(href);
  if (!url) {
    return { error: "not a pull request page" };
  }
  if (!SHA_RE.test(headSha || "")) {
    return { error: "could not find the head commit on this page" };
  }
  if (!question || question.trim() === "") {
    return { error: "the question is empty" };
  }
  if (utf8Len(question) > MAX_QUESTION_BYTES) {
    return { error: "the question is over 2 KB" };
  }
  let pageUrl = line && line.page_url ? line.page_url : url.page;
  if (utf8Len(pageUrl) > MAX_PAGE_URL_BYTES) {
    pageUrl = url.page;
  }
  const payload = {
    repo: url.repo,
    pr: url.pr,
    head_sha: headSha,
    question,
    deep: Boolean(deep),
    page_url: pageUrl,
  };
  if (line) {
    payload.path = line.path;
    payload.side = line.side;
    payload.start_line = line.start_line;
    payload.end_line = line.end_line;
    payload.selection = line.selection || "";
  }
  return { payload };
}

function isDiffPath(pathname) {
  return DIFF_PATH_RE.test(pathname || "");
}

// navDebounceMs batches turbo and history events into one activation check.
const navDebounceMs = 100;
const NAV_EVENT = "gh-ask:navigate";

// bindPage watches pathname. activate() runs on a diff view and
// activatePanel(), when given, runs for the whole pull request so the
// side panel stays up across its tabs. Each returns a stop function.
function bindPage(page) {
  let activePath = null;
  let stopSession = null;
  let panelKey = null;
  let stopPanel = null;
  let timer = null;
  let seenHref = "";
  let dropHead = false;

  function locationHref() {
    return page.location.href || page.location.pathname || "";
  }

  function syncPanel(path) {
    if (!page.activatePanel) {
      return;
    }
    const key = prKey(path);
    if (key) {
      if (panelKey === key) {
        return;
      }
      if (stopPanel) {
        stopPanel();
      }
      stopPanel = page.activatePanel();
      panelKey = key;
      return;
    }
    if (stopPanel) {
      stopPanel();
      stopPanel = null;
    }
    panelKey = null;
  }

  function syncDiff(path) {
    if (isDiffPath(path)) {
      if (activePath === path) {
        return;
      }
      if (stopSession) {
        stopSession();
        stopSession = null;
      }
      stopSession = page.activate();
      activePath = path;
      return;
    }
    if (stopSession) {
      stopSession();
      stopSession = null;
    }
    activePath = null;
  }

  function sync() {
    timer = null;
    const href = locationHref();
    if (dropHead || headNavDropsCache(seenHref, href, "")) {
      forgetHeadSha();
    }
    dropHead = false;
    seenHref = href;
    const path = page.location.pathname || "";
    syncPanel(path);
    syncDiff(path);
  }

  function schedule() {
    if (timer !== null) {
      page.clearTimeout(timer);
    }
    timer = page.setTimeout(sync, navDebounceMs);
  }

  const history = page.history;
  const original = {
    pushState: history.pushState,
    replaceState: history.replaceState,
  };
  for (const name of ["pushState", "replaceState"]) {
    const orig = original[name];
    history[name] = function patchedHistory(...args) {
      const result = orig.apply(this, args);
      page.window.dispatchEvent(new page.window.Event(NAV_EVENT));
      return result;
    };
  }

  function onNav(event) {
    const type = event && event.type;
    if (headNavDropsCache(seenHref, locationHref(), type)) {
      dropHead = true;
    }
    schedule();
  }
  for (const type of ["turbo:load", "turbo:render", "popstate", NAV_EVENT]) {
    page.window.addEventListener(type, onNav);
  }
  schedule();

  return {
    stop() {
      if (timer !== null) {
        page.clearTimeout(timer);
        timer = null;
      }
      for (const type of ["turbo:load", "turbo:render", "popstate", NAV_EVENT]) {
        page.window.removeEventListener(type, onNav);
      }
      history.pushState = original.pushState;
      history.replaceState = original.replaceState;
      if (stopSession) {
        stopSession();
        stopSession = null;
      }
      activePath = null;
      if (stopPanel) {
        stopPanel();
        stopPanel = null;
      }
      panelKey = null;
    },
  };
}

const GhAsk = {
  askError,
  askLine,
  askPanel,
  bindPage,
  buildPayload,
  chipText,
  findHeadSha,
  formatMessageTime,
  headNavDropsCache,
  isDiffPath,
  isPrPath,
  lineInfo,
  navDebounceMs,
  onPopoverKey,
  mountPanelToggle,
  panelAsk,
  panelPollInterval,
  panelView,
  parsePrUrl,
  placeInView,
  popoverAskResult,
  prHeader,
  renderMessage,
  renderPanel,
  serverUrl,
  startPanelPolling,
  startPolling,
  statusLabel,
  statusPath,
  threadPath,
  truncateUtf8,
};

// ---- Browser glue ----

const CSS = `
.gh-ask-btn { position: absolute; z-index: 100; font: 12px sans-serif; padding: 2px 6px;
  border: 1px solid #8c959f; border-radius: 6px; background: #f6f8fa; color: #1f2328; cursor: pointer; }
.gh-ask-btn.gh-ask-header { position: static; z-index: auto; margin-left: 8px;
  vertical-align: middle; line-height: 20px; padding: 0 8px; }
.gh-ask-pop { position: absolute; z-index: 101; width: 360px; padding: 8px; font: 13px sans-serif;
  border: 1px solid #8c959f; border-radius: 6px; background: #fff; color: #1f2328;
  box-shadow: 0 4px 12px rgba(0,0,0,.2); }
.gh-ask-pop textarea { width: 100%; height: 80px; box-sizing: border-box; }
.gh-ask-pop .gh-ask-row { display: flex; gap: 8px; align-items: center; margin-top: 6px; }
.gh-ask-pop .gh-ask-err { color: #cf222e; margin-top: 6px; white-space: pre-wrap; }
.gh-ask-chip { display: inline-block; margin-left: 8px; padding: 0 6px; font: 11px sans-serif;
  border-radius: 10px; background: #ddf4ff; color: #0969da; white-space: nowrap; cursor: pointer; }
.gh-ask-chip.gh-ask-bad { background: #ffebe9; color: #cf222e; }
`;

const PANEL_CSS = `
.gh-ask-panel { position: fixed; top: 0; right: 0; z-index: 200; width: 360px; max-width: 100%;
  height: 100vh; box-sizing: border-box; display: flex; flex-direction: column;
  background: #fff; color: #1f2328; border-left: 1px solid #d0d7de; font: 13px sans-serif; }
.gh-ask-panel-log { flex: 1; overflow: auto; padding: 12px; }
.gh-ask-msg { margin: 0 0 12px; }
.gh-ask-msg-meta { font: 11px sans-serif; color: #59636e; }
.gh-ask-msg-text { white-space: pre-wrap; }
.gh-ask-panel-foot { padding: 8px 12px; border-top: 1px solid #d0d7de; }
.gh-ask-panel-foot textarea { width: 100%; height: 72px; box-sizing: border-box; }
.gh-ask-panel .gh-ask-row { display: flex; gap: 8px; align-items: center; margin-top: 6px; }
.gh-ask-panel .gh-ask-err { color: #cf222e; margin-top: 6px; white-space: pre-wrap; }
.gh-ask-note { color: #59636e; margin: 8px 0 0; }
`;

function el(tag, props, children) {
  const e = document.createElement(tag);
  Object.assign(e, props);
  for (const c of children || []) {
    e.append(c);
  }
  return e;
}

// fetchSameOrigin reads a path on this page's origin. The panel uses it
// to load the files tab when the current tab has no head commit. A full
// URL is refused so this cannot be pointed at another host.
async function fetchSameOrigin(url) {
  if (typeof url !== "string" || !url.startsWith("/") || url.startsWith("//")) {
    throw new Error("refusing a non-relative fetch");
  }
  const res = await fetch(url, { credentials: "same-origin" });
  if (!res.ok) {
    throw new Error(String(res.status));
  }
  return res.text();
}

// loopbackRequest calls gh-ask serve. status 0 is no response, -1 missing settings.
function loopbackRequest(method, path, body, done) {
  const token = GM_getValue("token", "");
  const port = GM_getValue("port", DEFAULT_PORT);
  const url = serverUrl(port, path);
  if (!url || !token) {
    done({ status: -1 });
    return;
  }
  const headers = { "X-Gh-Ask-Token": token };
  if (body !== null) {
    headers["Content-Type"] = "application/json";
  }
  GM_xmlhttpRequest({
    method,
    url,
    headers,
    data: body === null ? undefined : JSON.stringify(body),
    anonymous: true,
    timeout: 150_000,
    onload: (r) => {
      let parsed = null;
      try {
        parsed = JSON.parse(r.responseText);
      } catch {
        parsed = null;
      }
      done({ status: r.status, body: parsed });
    },
    onerror: () => done({ status: 0 }),
    ontimeout: () => done({ status: 0 }),
  });
}

let panelApi = null;
let panelStyleAdded = false;

// activatePanel mounts the read-only thread panel for one pull request.
// It returns a function that removes the panel.
function activatePanel() {
  if (!panelStyleAdded) {
    GM_addStyle(PANEL_CSS);
    panelStyleAdded = true;
  }
  const doc = { createElement: (tag) => document.createElement(tag) };
  const toggle = el("button", {
    type: "button",
    className: "gh-ask-btn gh-ask-header",
    textContent: "gh-ask",
  });
  const log = el("div", { className: "gh-ask-panel-log" });
  const question = el("textarea", { placeholder: "Question for the agent" });
  const deep = el("input", { type: "checkbox" });
  const send = el("button", {
    type: "button",
    className: "btn btn-sm btn-primary",
    textContent: "Send",
    title: "Send (⌘↵ / Ctrl+↵)",
  });
  const refresh = el("button", { type: "button", className: "btn btn-sm", textContent: "Refresh" });
  const err = el("div", { className: "gh-ask-err" });
  const note = el("div", { className: "gh-ask-note", textContent: PANEL_FOOTER });
  const foot = el("div", { className: "gh-ask-panel-foot" }, [
    question,
    el("div", { className: "gh-ask-row" }, [el("label", {}, [deep, " deep"]), send, refresh]),
    err,
    note,
  ]);
  const panel = el("aside", { className: "gh-ask-panel", hidden: true }, [log, foot]);
  document.body.append(panel);

  let watch = null;
  let line = null;
  let poll = null;
  let sending = false;
  let threadId = null;

  function clearLog() {
    while (log.firstChild) {
      log.removeChild(log.firstChild);
    }
  }

  function paint(view) {
    clearLog();
    if (view.empty) {
      log.append(el("p", { textContent: PANEL_EMPTY }));
    } else {
      if (view.closed) {
        log.append(el("div", { textContent: "closed" }));
      }
      for (const message of view.messages) {
        log.append(renderMessage(doc, message));
      }
    }
    log.scrollTop = log.scrollHeight;
  }

  function stopPoll() {
    if (poll) {
      poll.stop();
      poll = null;
    }
  }

  function beginPoll(messages, closed) {
    stopPoll();
    poll = startPanelPolling({
      now: () => Date.now(),
      setTimeout: (fn, ms) => window.setTimeout(fn, ms),
      clearTimeout: (t) => window.clearTimeout(t),
      isVisible: () => document.visibilityState === "visible" && !panel.hidden,
      messages,
      closed,
      fetchStatus: (done) => loopbackRequest("GET", statusPath(threadId), null, done),
      onUpdate: ({ messages: msgs, closed: isClosed, error }) => {
        if (!threadId) {
          return;
        }
        paint(
          panelView(
            { status: 200, body: { id: threadId } },
            {
              state: "",
              answered: false,
              closed: Boolean(isClosed),
              messages: msgs,
            },
          ),
        );
        err.textContent = error || "";
      },
    });
  }

  function loadThread() {
    const page = parsePrPage(window.location.href);
    if (!page) {
      threadId = null;
      stopPoll();
      paint(panelView({ status: 404 }, null));
      return;
    }
    loopbackRequest("GET", threadPath(page.repo, page.pr), null, (res) => {
      if (panel.hidden) {
        return;
      }
      const view = panelView(res, null);
      if (view.empty) {
        threadId = null;
        stopPoll();
        paint(view);
        if (res.status !== 404 && res.status !== 200) {
          err.textContent = askError(res.status, res.body && res.body.error);
        }
        return;
      }
      threadId = view.id;
      loopbackRequest("GET", statusPath(threadId), null, (st) => {
        if (panel.hidden) {
          return;
        }
        const body =
          st.status === 200 && validStatus(st.body)
            ? st.body
            : { state: "", answered: false, closed: false, messages: [] };
        paint(panelView({ status: 200, body: { id: threadId } }, body));
        beginPoll(panelMessages(body), Boolean(body.closed));
        if (st.status !== 200) {
          err.textContent = askError(st.status, st.body && st.body.error);
        }
      });
    });
  }

  function open(nextLine) {
    line = nextLine || null;
    panel.hidden = false;
    err.textContent = "";
    loadThread();
  }

  function close() {
    panel.hidden = true;
    stopPoll();
  }

  async function submit() {
    if (sending) {
      return;
    }
    sending = true;
    send.disabled = true;
    const built = await askPanel({
      href: window.location.href,
      root: document.documentElement,
      question: question.value,
      deep: deep.checked,
      line,
      cache: headCache,
      onStatus: (msg) => {
        err.textContent = msg;
      },
      fetchText: fetchSameOrigin,
      parseHtml: (html) => new DOMParser().parseFromString(html, "text/html"),
      request: (payload) => {
        err.textContent = "";
        loopbackRequest("POST", "/ask", payload, (res) => {
          sending = false;
          send.disabled = false;
          if (panel.hidden) {
            return;
          }
          if (
            res.status === 200 &&
            res.body &&
            typeof res.body.id === "string" &&
            res.body.id !== ""
          ) {
            question.value = "";
            threadId = res.body.id;
            loadThread();
            return;
          }
          err.textContent =
            res.status === 200
              ? "the server returned no id"
              : askError(res.status, res.body && res.body.error);
        });
      },
    });
    if (built.error) {
      sending = false;
      send.disabled = false;
      err.textContent = built.error;
    }
  }

  toggle.addEventListener("click", () => {
    if (panel.hidden) {
      open(null);
    } else {
      close();
    }
  });
  refresh.addEventListener("click", () => {
    err.textContent = "";
    if (poll) {
      poll.refresh();
      return;
    }
    loadThread();
  });
  send.addEventListener("click", submit);
  foot.addEventListener("keydown", (event) => {
    onPopoverKey(event, { question: question.value, sending, send: submit, close });
  });

  let mountTimer = null;
  function mountToggle() {
    return mountPanelToggle(document.documentElement, toggle);
  }
  function scheduleMount() {
    if (mountTimer !== null) {
      window.clearTimeout(mountTimer);
    }
    mountTimer = window.setTimeout(() => {
      mountTimer = null;
      mountToggle();
    }, navDebounceMs);
  }
  watch = new MutationObserver(() => {
    scheduleMount();
  });
  watch.observe(document.documentElement, { childList: true, subtree: true });
  mountToggle();

  panelApi = { open, close };
  return function stop() {
    if (panelApi && panelApi.open === open) {
      panelApi = null;
    }
    stopPoll();
    if (mountTimer !== null) {
      window.clearTimeout(mountTimer);
      mountTimer = null;
    }
    if (watch) {
      watch.disconnect();
      watch = null;
    }
    toggle.remove();
    panel.remove();
  };
}

function registerMenu() {
  const TOKEN_RE = /^[\x21-\x7e]{1,256}$/;

  function settings() {
    return { token: GM_getValue("token", ""), port: GM_getValue("port", DEFAULT_PORT) };
  }

  GM_registerMenuCommand("Set gh-ask token / port", () => {
    // The current token is never shown; an empty answer keeps it.
    const token = window.prompt(
      "gh-ask token (the output of `gh-ask token`). Leave empty to keep the current one:",
      "",
    );
    if (token === null) {
      return;
    }
    const port = window.prompt("gh-ask port:", String(settings().port));
    if (port === null) {
      return;
    }
    if (!serverUrl(port.trim(), "/ask")) {
      window.alert("gh-ask: the port must be 1-65535.");
      return;
    }
    if (token.trim() !== "") {
      if (!TOKEN_RE.test(token.trim())) {
        window.alert("gh-ask: the token has spaces or control characters.");
        return;
      }
      GM_setValue("token", token.trim());
    }
    GM_setValue("port", Number(port.trim()));
  });
}

// main installs the diff-view controls and returns a function that
// removes them, their listeners, and any status polling.
function main() {
  function request(method, path, body, done) {
    loopbackRequest(method, path, body, done);
  }

  const style = el("style", { textContent: CSS });
  document.head.append(style);

  const button = el("button", {
    type: "button",
    className: "gh-ask-btn",
    textContent: "Ask agent",
    hidden: true,
  });
  document.body.append(button);
  let target = null;
  let popover = null;
  let popoverPoint = null;
  const pollStops = [];

  function elementOf(node) {
    return node && node.nodeType === 3 ? node.parentElement : node;
  }

  // Measure hidden and offscreen, then clamp. Inline width is cleared
  // first so a later, wider window can grow back to the natural size.
  function place(node, rect) {
    const previousVisibility = node.style.visibility;
    node.style.visibility = "hidden";
    node.style.left = "-10000px";
    node.style.top = "0px";
    node.style.width = "";
    const preferred = node.classList.contains("gh-ask-pop") ? POPOVER_WIDTH : node.offsetWidth;
    const room = Math.max(0, window.innerWidth - 2 * VIEWPORT_MARGIN);
    node.style.width = `${Math.min(preferred, room)}px`;
    const placed = placeInView(
      { left: rect.left, top: rect.top },
      { width: preferred, height: node.offsetHeight },
      { width: window.innerWidth, height: window.innerHeight },
    );
    node.style.visibility = previousVisibility;
    node.style.width = `${placed.width}px`;
    node.style.top = `${placed.top + window.scrollY}px`;
    node.style.left = `${placed.left + window.scrollX}px`;
  }

  function popoverAnchor() {
    return {
      top: Number.parseFloat(button.style.top) - window.scrollY + 24,
      left: Number.parseFloat(button.style.left) - window.scrollX,
    };
  }

  function onResize() {
    if (popover && popoverPoint) {
      place(popover, popoverPoint());
    }
  }
  window.addEventListener("resize", onResize);

  function onMouseOver(e) {
    if (popover || target?.selection || button.contains(e.target)) {
      return;
    }
    const info = lineInfo(e.target);
    if (!info) {
      button.hidden = true;
      target = null;
      return;
    }
    target = { start: e.target, end: e.target, selection: "" };
    const r = info.row.getBoundingClientRect();
    button.hidden = false;
    place(button, { top: r.top, left: r.right - button.offsetWidth - 4 });
  }
  document.addEventListener("mouseover", onMouseOver);

  function onMouseUp(e) {
    if (popover || button.contains(e.target)) {
      return;
    }
    window.setTimeout(() => {
      const sel = window.getSelection();
      if (!sel || sel.isCollapsed || sel.rangeCount === 0) {
        if (target?.selection) {
          target = null;
          button.hidden = true;
        }
        return;
      }
      const start = elementOf(sel.anchorNode);
      const end = elementOf(sel.focusNode);
      if (!lineInfo(start) || !lineInfo(end)) {
        return;
      }
      target = { start, end, selection: sel.toString() };
      const r = sel.getRangeAt(0).getBoundingClientRect();
      button.hidden = false;
      place(button, { top: r.bottom + 4, left: r.right });
    }, 0);
  }
  document.addEventListener("mouseup", onMouseUp);

  function closePopover() {
    if (popover) {
      popover.remove();
      popover = null;
    }
    popoverPoint = null;
    target = null;
    button.hidden = true;
  }

  function onKeyDown(e) {
    if (e.key === "Escape") {
      closePopover();
    }
  }
  document.addEventListener("keydown", onKeyDown);

  function setChip(chip, text, bad) {
    chip.textContent = text;
    chip.classList.toggle("gh-ask-bad", bad);
  }

  function track(chip, id) {
    const shownId = truncateUtf8(id, 100);
    setChip(chip, chipText(shownId, "waiting"), false);
    pollStops.push(
      startPolling({
        now: () => Date.now(),
        setTimeout: (fn, ms) => window.setTimeout(fn, ms),
        clearTimeout: (t) => window.clearTimeout(t),
        isVisible: () => document.visibilityState === "visible",
        fetchStatus: (done) => request("GET", statusPath(id), null, done),
        onUpdate: ({ label, error }) => {
          setChip(chip, chipText(shownId, label) + (error ? ` · ${error}` : ""), Boolean(error));
        },
      }),
    );
  }

  // openAsk shows the question popover. point() is the viewport anchor.
  // build(question, deep, onStatus) returns {payload} or {error}, and
  // may be async while the head commit is fetched. placeChip inserts
  // the status chip next to the control that opened it. A failed ask
  // leaves the question text in the box.
  function openAsk(point, build, placeChip) {
    if (popover) {
      return;
    }
    popoverPoint = point;
    const question = el("textarea", { placeholder: "Question for the agent" });
    const deep = el("input", { type: "checkbox" });
    const send = el("button", {
      type: "button",
      className: "btn btn-sm btn-primary",
      textContent: "Send",
      title: "Send (⌘↵ / Ctrl+↵)",
    });
    const cancel = el("button", { type: "button", className: "btn btn-sm", textContent: "Cancel" });
    const err = el("div", { className: "gh-ask-err" });
    popover = el("div", { className: "gh-ask-pop" }, [
      question,
      el("div", { className: "gh-ask-row" }, [el("label", {}, [deep, " deep"]), send, cancel]),
      err,
    ]);
    document.body.append(popover);
    place(popover, popoverPoint());
    button.hidden = true;
    question.focus();
    cancel.addEventListener("click", closePopover);

    let chip = null;
    let sending = false;
    async function submit() {
      if (sending) {
        return;
      }
      sending = true;
      send.disabled = true;
      const built = await build(question.value, deep.checked, (msg) => {
        err.textContent = msg;
      });
      if (!popover) {
        return;
      }
      const shown = popoverAskResult(question.value, built);
      if (!shown.payload) {
        sending = false;
        send.disabled = false;
        err.textContent = shown.error;
        return;
      }
      err.textContent = "";
      if (!chip) {
        chip = el("span", { className: "gh-ask-chip" });
        const asked = shown.payload;
        const chipLine =
          asked && asked.path
            ? {
                path: asked.path,
                side: asked.side,
                start_line: asked.start_line,
                end_line: asked.end_line,
                selection: asked.selection,
                page_url: asked.page_url,
              }
            : null;
        chip.addEventListener("click", () => {
          if (panelApi) {
            panelApi.open(chipLine);
          }
        });
        placeChip(chip);
      }
      setChip(chip, "Asking agent…", false);
      request("POST", "/ask", shown.payload, (res) => {
        sending = false;
        send.disabled = false;
        if (
          res.status === 200 &&
          res.body &&
          typeof res.body.id === "string" &&
          res.body.id !== ""
        ) {
          closePopover();
          track(chip, res.body.id);
          return;
        }
        const msg =
          res.status === 200 ? "the server returned no id" : askError(res.status, res.body?.error);
        setChip(chip, `gh-ask: ${msg}`, true);
        err.textContent = msg;
      });
    }
    send.addEventListener("click", submit);
    popover.addEventListener("keydown", (event) => {
      onPopoverKey(event, {
        question: question.value,
        sending,
        send: submit,
        close: closePopover,
      });
    });
  }

  function headOpts(onStatus) {
    return {
      href: window.location.href,
      root: document.documentElement,
      cache: headCache,
      onStatus,
      fetchText: fetchSameOrigin,
      parseHtml: (html) => new DOMParser().parseFromString(html, "text/html"),
    };
  }

  button.addEventListener("click", () => {
    if (!target) {
      return;
    }
    const ask = target;
    openAsk(
      () => popoverAnchor(),
      (question, deep, onStatus) =>
        askLine({
          ...headOpts(onStatus),
          start: ask.start,
          end: ask.end,
          selection: ask.selection,
          question,
          deep,
        }),
      (chip) => {
        const info = lineInfo(ask.end);
        if (info) {
          info.row.lastElementChild.append(chip);
        }
      },
    );
  });

  return function deactivate() {
    for (const stop of pollStops) {
      stop();
    }
    pollStops.length = 0;
    window.removeEventListener("resize", onResize);
    document.removeEventListener("mouseover", onMouseOver);
    document.removeEventListener("mouseup", onMouseUp);
    document.removeEventListener("keydown", onKeyDown);
    if (popover) {
      popover.remove();
      popover = null;
    }
    button.remove();
    style.remove();
    for (const chip of document.querySelectorAll(".gh-ask-chip")) {
      chip.remove();
    }
  };
}

function boot() {
  registerMenu();
  bindPage({
    location: window.location,
    history: window.history,
    window,
    setTimeout: (fn, ms) => window.setTimeout(fn, ms),
    clearTimeout: (t) => window.clearTimeout(t),
    activate: () => main(),
    activatePanel: () => activatePanel(),
  });
}

if (typeof module === "object" && module !== null && module.exports) {
  module.exports = GhAsk;
} else {
  boot();
}
