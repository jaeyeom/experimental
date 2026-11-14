// ==UserScript==
// @name         gh-ask
// @namespace    https://github.com/jaeyeom/experimental/tree/main/devtools/gh-ask
// @version      0.1.0
// @description  Ask a local agent about lines in a pull request diff, through gh-ask serve on 127.0.0.1.
// @match        https://github.com/*/*/pull/*/files*
// @match        https://github.com/*/*/pull/*/changes*
// @grant        GM_xmlhttpRequest
// @grant        GM_getValue
// @grant        GM_setValue
// @grant        GM_registerMenuCommand
// @connect      127.0.0.1
// @connect      localhost
// @run-at       document-idle
// @noframes
// ==/UserScript==

// The first half of this file is pure: it reads a DOM-like tree through
// tagName, getAttribute, parentElement, children and textContent only,
// so gh-ask.test.js runs it in node against testdata/ fixtures. The
// browser glue, main(), is the only code that uses document, window or
// GM_* functions. It sends requests only to http://127.0.0.1:<port>.

// Limits from devtools/gh-ask/message.go.
const MAX_SELECTION_BYTES = 4 << 10;
const MAX_QUESTION_BYTES = 2 << 10;
const MAX_PATH_BYTES = 1 << 10;
const MAX_PAGE_URL_BYTES = 2 << 10;

const POLL_INTERVAL_MS = 20_000;
const POLL_MAX_MS = 30 * 60_000;
const DEFAULT_PORT = 8765;

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
// range in the URL wins; then the hidden form inputs of the classic view;
// then the embedded JSON of the React view.
function findHeadSha(root, href) {
  const m = URL_HEAD_RE.exec(href);
  if (m) {
    return m[1];
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

const GhAsk = {
  askError,
  buildPayload,
  chipText,
  findHeadSha,
  lineInfo,
  parsePrUrl,
  serverUrl,
  startPolling,
  statusLabel,
  statusPath,
  truncateUtf8,
};

// ---- Browser glue ----

const CSS = `
.gh-ask-btn { position: absolute; z-index: 100; font: 12px sans-serif; padding: 2px 6px;
  border: 1px solid #8c959f; border-radius: 6px; background: #f6f8fa; color: #1f2328; cursor: pointer; }
.gh-ask-pop { position: absolute; z-index: 101; width: 360px; padding: 8px; font: 13px sans-serif;
  border: 1px solid #8c959f; border-radius: 6px; background: #fff; color: #1f2328;
  box-shadow: 0 4px 12px rgba(0,0,0,.2); }
.gh-ask-pop textarea { width: 100%; height: 80px; box-sizing: border-box; }
.gh-ask-pop .gh-ask-row { display: flex; gap: 8px; align-items: center; margin-top: 6px; }
.gh-ask-pop .gh-ask-err { color: #cf222e; margin-top: 6px; white-space: pre-wrap; }
.gh-ask-chip { display: inline-block; margin-left: 8px; padding: 0 6px; font: 11px sans-serif;
  border-radius: 10px; background: #ddf4ff; color: #0969da; white-space: nowrap; }
.gh-ask-chip.gh-ask-bad { background: #ffebe9; color: #cf222e; }
`;

function main() {
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

  // request sends one request to the loopback server and calls done
  // with {status, body}. status 0 is no response, -1 missing settings.
  function request(method, path, body, done) {
    const { token, port } = settings();
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

  function el(tag, props, children) {
    const e = document.createElement(tag);
    Object.assign(e, props);
    for (const c of children || []) {
      e.append(c);
    }
    return e;
  }

  document.head.append(el("style", { textContent: CSS }));

  const button = el("button", {
    type: "button",
    className: "gh-ask-btn",
    textContent: "Ask agent",
    hidden: true,
  });
  document.body.append(button);
  let target = null;
  let popover = null;

  function elementOf(node) {
    return node && node.nodeType === 3 ? node.parentElement : node;
  }

  function place(node, rect) {
    node.style.top = `${rect.top + window.scrollY}px`;
    node.style.left = `${Math.max(0, rect.left + window.scrollX)}px`;
  }

  document.addEventListener("mouseover", (e) => {
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
  });

  document.addEventListener("mouseup", (e) => {
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
  });

  function closePopover() {
    if (popover) {
      popover.remove();
      popover = null;
    }
    target = null;
    button.hidden = true;
  }

  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      closePopover();
    }
  });

  function setChip(chip, text, bad) {
    chip.textContent = text;
    chip.classList.toggle("gh-ask-bad", bad);
  }

  function track(chip, id) {
    const shownId = truncateUtf8(id, 100);
    setChip(chip, chipText(shownId, "waiting"), false);
    startPolling({
      now: () => Date.now(),
      setTimeout: (fn, ms) => window.setTimeout(fn, ms),
      clearTimeout: (t) => window.clearTimeout(t),
      isVisible: () => document.visibilityState === "visible",
      fetchStatus: (done) => request("GET", statusPath(id), null, done),
      onUpdate: ({ label, error }) => {
        setChip(chip, chipText(shownId, label) + (error ? ` · ${error}` : ""), Boolean(error));
      },
    });
  }

  button.addEventListener("click", () => {
    if (!target) {
      return;
    }
    const ask = target;
    const question = el("textarea", { placeholder: "Question for the agent" });
    const deep = el("input", { type: "checkbox" });
    const send = el("button", {
      type: "button",
      className: "btn btn-sm btn-primary",
      textContent: "Send",
    });
    const cancel = el("button", { type: "button", className: "btn btn-sm", textContent: "Cancel" });
    const err = el("div", { className: "gh-ask-err" });
    popover = el("div", { className: "gh-ask-pop" }, [
      question,
      el("div", { className: "gh-ask-row" }, [el("label", {}, [deep, " deep"]), send, cancel]),
      err,
    ]);
    document.body.append(popover);
    place(popover, {
      top: Number.parseFloat(button.style.top) - window.scrollY + 24,
      left: Number.parseFloat(button.style.left) - window.scrollX,
    });
    button.hidden = true;
    question.focus();
    cancel.addEventListener("click", closePopover);

    let chip = null;
    send.addEventListener("click", () => {
      const built = buildPayload({
        href: window.location.href,
        headSha: findHeadSha(document.documentElement, window.location.href),
        start: ask.start,
        end: ask.end,
        selection: ask.selection,
        question: question.value,
        deep: deep.checked,
      });
      if (built.error) {
        err.textContent = built.error;
        return;
      }
      if (!chip) {
        chip = el("span", { className: "gh-ask-chip" });
        const info = lineInfo(ask.end);
        info.row.lastElementChild.append(chip);
      }
      setChip(chip, "Asking agent…", false);
      send.disabled = true;
      err.textContent = "";
      request("POST", "/ask", built.payload, (res) => {
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
    });
  });
}

if (typeof module === "object" && module !== null && module.exports) {
  module.exports = GhAsk;
} else {
  main();
}
